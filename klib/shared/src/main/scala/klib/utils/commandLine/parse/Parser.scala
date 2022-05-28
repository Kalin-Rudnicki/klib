package klib.utils.commandLine.parse

import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import scala.annotation.tailrec
import scala.reflect.ClassTag
import zio.Zippable

import klib.fp.typeclass.*
import klib.utils.{given, *}

final case class BuiltParser[+T](
    parseF: IndexedArgs => EitherNel[Error, T],
    helpString: HelpConfig => String,
) {

  def parse(argStrings: String*): BuiltParser.Result[T] =
    parse(argStrings.toList)

  def parse(argStrings: List[String]): BuiltParser.Result[T] = {
    import Arg.find.*

    val args: IndexedArgs = Arg.parse(argStrings)

    val helpExtra: Arg.find.FindFunction[Boolean] = {
      def lp(name: String, value: => Boolean): FindFunction[Boolean] =
        find(name).constValue(value)
      def sp(name: Char, value: => Boolean): FindFunction[Boolean] =
        find(name).constValue(value)

      FindFunction.first(
        lp("help-extra", true),
        sp('H', true),
        lp("help", false),
        sp('h', false),
      )
    }

    helpExtra.attemptToFind(args).map(_.arg) match {
      case Some(helpExtra) => BuiltParser.Result.Help(helpExtra, helpString(HelpConfig.default(helpExtra)))
      case None =>
        parseF(args) match {
          case Left(errors) => BuiltParser.Result.Failure(errors)
          case Right(value) => BuiltParser.Result.Success(value)
        }
    }
  }

}
object BuiltParser {

  sealed trait Result[+T]
  object Result {
    final case class Success[+T](result: T) extends Result[T]
    final case class Failure(errors: NonEmptyList[Error]) extends Result[Nothing]
    final case class Help(isHelpExtra: Boolean, helpString: String) extends Result[Nothing] {
      override def toString: String = helpString
    }
  }

}

final case class Parser[+T](
    parseF: IndexedArgs => Parser.Result[T],
    elements: List[Element],
) {

  // =====| Building |=====

  def >&>[T2](other: Parser[T2])(implicit zip: Zippable[T, T2]): Parser[zip.Out] =
    Parser(
      parseF = { args =>
        val res1 = this.parseF(args)
        val res2 = other.parseF(res1.remainingArgs)
        (res1.res, res2.res) match {
          case (Right(r1), Right(r2))   => Parser.Result.success(zip.zip(r1, r2), res2.remainingArgs)
          case (Left(err1), Right(_))   => Parser.Result.failure(err1, res2.remainingArgs)
          case (Right(_), Left(err2))   => Parser.Result.failure(err2, res2.remainingArgs)
          case (Left(err1), Left(err2)) => Parser.Result.failure(err1 ::: err2, res2.remainingArgs)
        }
      },
      elements = this.elements ::: other.elements,
    )

  def map[T2](f: T => T2): Parser[T2] =
    Parser(
      parseF = parseF(_).mapResult(f),
      elements = elements,
    )

  def as[T2](t2: => T2): Parser[T2] =
    map(_ => t2)

  def section(label: ColorString, indentBy: Int = 1): Parser[T] =
    Parser(
      parseF = parseF,
      elements = Element.Section(label, elements, indentBy) :: Nil,
    )

  // =====| Build |=====

  private def build[Extras](extrasF: IndexedArgs => EitherNel[Error, Extras])(implicit
      zip: Zippable[T, Extras],
  ): BuiltParser[zip.Out] =
    BuiltParser(
      parseF = { args =>
        val result = parseF(args)
        val extras = extrasF(result.remainingArgs)
        (result.res, extras).parMapN(zip.zip)
      },
      helpString = { helpConfig =>
        def makeHelpElement(
            longName: String,
            shortName: Char,
            description: String,
        ): Element = {
          val primaryParams: NonEmptyList[Param] =
            NonEmptyList.of(
              Param.Long(longName),
              Param.Short(shortName),
            )

          Element.ParamElement(
            baseName = longName,
            typeName = "flag",
            primaryParams = primaryParams,
            aliasParams = Nil,
            requirementLevel = None,
            description = description :: Nil,
          )
        }

        val allElements =
          List(
            Element.Break,
            makeHelpElement("help", 'h', "Display help message"),
            makeHelpElement("help-extra", 'H', "Display help message, with extra details"),
            Element.Break,
          ) ::: elements

        val allParamsNames: List[String] = allElements.flatMap(_.allParams.flatMap(_.allParamStrings))
        val paramNamesCount: Map[String, Int] = allParamsNames.groupBy(identity).map { (k, v) => (k, v.size) }
        val duplicateNames: List[String] = paramNamesCount.filter(_._2 > 1).toList.map(_._1)
        val duplicateNamesLines: Pair[List[LeftLine], List[String]] =
          if (helpConfig.helpExtra && duplicateNames.nonEmpty)
            Pair(
              List(
                LeftLine(""),
                LeftLine("[Duplicate Param Names]".yellow),
              ) ::: duplicateNames.map(n => LeftLine(n).indentBy(1)),
              Nil,
            )
          else
            Pair(Nil, Nil)

        val linePairs: List[Pair[ColorString, String]] =
          Pair.zipPairs(helpConfig)(
            allElements.flatMap(_.helpStringLinesPair(helpConfig)),
            duplicateNamesLines :: Nil,
          )

        val lines: List[String] =
          Pair.makeLines(helpConfig, linePairs)

        ("Usage : [ARGS]" :: lines).mkString("\n")
      },
    )

  def discardExtras: BuiltParser[T] =
    build(_ => ().asRight)

  def disallowExtras: BuiltParser[T] =
    build {
      _.toNel match {
        case Some(nel) => nel.map { arg => Error(None, Error.Reason.UnexpectedArg(arg)) }.asLeft
        case None      => ().asRight
      }
    }

  def extrasAsArgs(implicit zip: Zippable[T, List[Arg]]): BuiltParser[zip.Out] =
    build(_.map(_.value).asRight)

  def extrasAsIndexedArgs(implicit zip: Zippable[T, List[Indexed[Arg]]]): BuiltParser[zip.Out] =
    build(_.asRight)

}
object Parser {

  final case class Result[+T] private (
      res: EitherNel[Error, T],
      remainingArgs: IndexedArgs,
  ) {

    def mapResult[T2](f: T => T2): Result[T2] =
      Result(res.map(f), remainingArgs)

    def flatMapResult[T2](f: T => EitherNel[Error, T2]): Result[T2] =
      Result(res.flatMap(f), remainingArgs)

  }
  object Result {

    // =====| Basic Builders |=====

    inline def success[T](t: T, remainingArgs: IndexedArgs): Result[T] =
      Result(t.asRight, remainingArgs)

    inline def failure(errors: NonEmptyList[Error], remainingArgs: IndexedArgs): Result[Nothing] =
      Result(errors.asLeft, remainingArgs)
    inline def failure(error0: Error, errorN: Error*)(remainingArgs: IndexedArgs): Result[Nothing] =
      failure(NonEmptyList(error0, errorN.toList), remainingArgs)

    inline def fromEither[T](res: EitherNel[Error, T], remainingArgs: IndexedArgs): Result[T] =
      Result(res, remainingArgs)

  }

  sealed trait GenBuilder[T] {

    protected def primaryParamName: ColorString
    protected def makeElement(requirementLevel: RequirementLevel): Element
    protected def optionalParseFunction(args: IndexedArgs, element: Element): Result[Option[T]]

    private final def build[T2](
        requirementLevel: RequirementLevel,
    )(
        mapRes: (Option[T], Element) => EitherNel[Error, T2],
    ): Parser[T2] = {
      val element: Element = makeElement(requirementLevel)
      val parseFunction: IndexedArgs => Result[T2] = optionalParseFunction(_, element).flatMapResult(mapRes(_, element))

      Parser(
        parseF = parseFunction,
        elements = element :: Nil,
      )
    }

    final def required: Parser[T] =
      build[T](RequirementLevel.Required) { (res, element) =>
        res match {
          case Some(value) => value.asRight
          case None        => NonEmptyList.one(Error(element.some, Error.Reason.MissingRequired(primaryParamName.toString))).asLeft
        }
      }
    final def default(default: T): Parser[T] =
      build[T](RequirementLevel.Default) { (res, _) => res.getOrElse(default).asRight }
    final def optional: Parser[Option[T]] =
      build[Option[T]](RequirementLevel.Optional) { (res, _) => res.asRight }

  }

  // =====| Builders |=====

  def unit: Parser[Unit] =
    Parser(
      args => Result.success((), args),
      Nil,
    )

  private def ofParser[T](parsers: NonEmptyList[(String, Parser[T])])(
      resF: NonEmptyList[(String, Parser[T])] => IndexedArgs => Parser.Result[T],
  ): Parser[T] =
    Parser(
      parseF = resF(parsers),
      elements = parsers.toList.flatMap { (n, p) => p.section(s"$n:").elements },
    )

  def firstOf[T](p0: (String, Parser[T]), p1: (String, Parser[T]), pN: (String, Parser[T])*): Parser[T] =
    ofParser(NonEmptyList(p0, p1 :: pN.toList)) { case NonEmptyList((hName, hParser), tail) =>
      args =>
        val hRes = hParser.parseF(args)
        tail.foldLeft(hRes) { case (acc, (tName, tParser)) =>
          val r = tParser.parseF(args)
          Parser.Result.fromEither(
            acc.res match {
              case Right(accRes) => accRes.asRight
              case Left(accErrors) =>
                r.res match {
                  case Right(rRes)   => rRes.asRight
                  case Left(rErrors) => (accErrors ::: rErrors).asLeft
                }
            },
            Arg.remainingInBoth(acc.remainingArgs, r.remainingArgs),
          )
        }
    }

  def eitherFirst[L, R](leftParser: (String, Parser[L]), rightParser: (String, Parser[R])): Parser[Either[L, R]] =
    firstOf(
      leftParser._1 -> leftParser._2.map(_.asLeft),
      rightParser._1 -> rightParser._2.map(_.asRight),
    )

  def exclusiveOf[T](p0: (String, Parser[T]), p1: (String, Parser[T]), pN: (String, Parser[T])*): Parser[T] =
    ofParser(NonEmptyList(p0, p1 :: pN.toList)) { case NonEmptyList((hName, hParser), tail) =>
      args =>
        val hRes = hParser.parseF(args)
        val (accRes, accNames) =
          tail.foldLeft((hRes, Option.when(hRes.res.isRight)(hName).toSet)) { case ((acc, names), (tName, tParser)) =>
            val r = tParser.parseF(args)
            (
              Parser.Result.fromEither(
                acc.res match {
                  case Right(accRes) => accRes.asRight
                  case Left(accErrors) =>
                    r.res match {
                      case Right(rRes)   => rRes.asRight
                      case Left(rErrors) => (accErrors ::: rErrors).asLeft
                    }
                },
                Arg.remainingInBoth(acc.remainingArgs, r.remainingArgs),
              ),
              names | Option.when(r.res.isRight)(tName).toSet,
            )
          }

        accRes.flatMapResult { res =>
          Either.cond(
            accNames.size == 1,
            res,
            NonEmptyList.one(Error(None, Error.Reason.ViolatedExclusiveOr(accNames))),
          )
        }
    }

  def eitherExclusive[L, R](leftParser: (String, Parser[L]), rightParser: (String, Parser[R])): Parser[Either[L, R]] =
    exclusiveOf(
      leftParser._1 -> leftParser._2.map(_.asLeft),
      rightParser._1 -> rightParser._2.map(_.asRight),
    )

  object singleValue {

    final case class Builder[T](
        private val _baseName: String,
        private val _typeName: String,
        private val _decodeFromString: DecodeFromString[T],
        private val _primaryLongParam: Param.LongWithValue,
        private val _longParamAliases: List[Param.LongWithValue],
        private val _primaryShortParam: Option[Param.ShortWithValue],
        private val _shortParamAliases: List[Param.ShortWithValue],
        private val _description: List[String],
    ) extends GenBuilder[T] {

      // =====| Modify |=====

      def withPrimaryLongParam(name: String): Builder[T] =
        this.copy(_primaryLongParam = Param.LongWithValue(name))

      def withLongParamAliases(names: String*): Builder[T] =
        this.copy(_longParamAliases = names.toList.map(Param.LongWithValue.apply))

      def addLongParamAliases(names: String*): Builder[T] =
        this.copy(_longParamAliases = _longParamAliases ::: names.toList.map(Param.LongWithValue.apply))

      def withPrimaryShortParam(name: Char): Builder[T] =
        this.copy(_primaryShortParam = Param.ShortWithValue(name).some)

      def withoutPrimaryShortParam: Builder[T] =
        this.copy(_primaryShortParam = None)

      def withShortParamAliases(names: Char*): Builder[T] =
        this.copy(_shortParamAliases = names.toList.map(Param.ShortWithValue.apply))

      def addShortParamAliases(names: Char*): Builder[T] =
        this.copy(_shortParamAliases = _shortParamAliases ::: names.toList.map(Param.ShortWithValue.apply))

      def withDescription(description: String*): Builder[T] =
        this.copy(_description = description.toList)

      def addDescription(description: String*): Builder[T] =
        this.copy(_description = _description ::: description.toList)

      def many: Builder[List[T]] =
        this.copy(_decodeFromString = _decodeFromString.commaSeparatedList)

      // =====| Build |=====

      override protected def primaryParamName: ColorString = _primaryLongParam.formattedName

      override protected def makeElement(requirementLevel: RequirementLevel): Element =
        Element.ParamElement(
          baseName = _baseName,
          typeName = _typeName,
          primaryParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList),
          aliasParams = _longParamAliases ::: _shortParamAliases,
          requirementLevel = requirementLevel.some,
          description = _description,
        )

      override protected def optionalParseFunction(args: IndexedArgs, element: Element): Result[Option[T]] = {
        import Arg.find.*

        val findFunction: FindFunction[String] =
          FindFunction.first {
            FindFunction.fromParam(_primaryLongParam) ::
              _primaryShortParam.map(FindFunction.fromParam).toList :::
              _longParamAliases.map(FindFunction.fromParam) :::
              _shortParamAliases.map(FindFunction.fromParam)
          }

        findFunction.attemptToFind(args) match {
          case Some(found) =>
            Result.fromEither(
              res = _decodeFromString.decodeError(found.arg) match {
                case Left(error)  => NonEmptyList.one(Error(element.some, Error.Reason.MalformattedValue(found.arg, error))).asLeft
                case Right(value) => value.some.asRight
              },
              remainingArgs = found.remaining,
            )
          case None =>
            Result.success(None, args)
        }
      }
    }

    def apply[T](
        baseName: String,
        primaryLongParamName: Defaultable[String] = Defaultable.Auto,
        primaryShortParamName: DefaultableOption[Char] = Defaultable.Auto,
    )(implicit
        dfs: DecodeFromString[T],
        ct: ClassTag[T],
    ): Builder[T] =
      Builder(
        _baseName = baseName,
        _typeName = ct.runtimeClass.getSimpleName,
        _decodeFromString = dfs,
        _primaryLongParam = Param.LongWithValue(primaryLongParamName.toValue(baseName)),
        _longParamAliases = Nil,
        _primaryShortParam = primaryShortParamName.toOptionO(baseName.headOption).map(Param.ShortWithValue.apply),
        _shortParamAliases = Nil,
        _description = Nil,
      )

    def enumPairs[T](
        baseName: String,
        values: Seq[(String, T)],
        primaryLongParamName: Defaultable[String] = Defaultable.Auto,
        primaryShortParamName: DefaultableOption[Char] = Defaultable.Auto,
    )(implicit
        ct: ClassTag[T],
    ): Builder[T] = {
      implicit val dfs: DecodeFromString[T] = { str =>
        val upcaseValues = values.map { (s, t) => (s.toUpperCase, t) }
        upcaseValues.toMap.get(str.toUpperCase) match {
          case Some(value) => value.asRight
          case None        => NonEmptyList.one(s"Invalid value ${str.unesc}, valid: ${upcaseValues.map(_._1).mkString(", ")}").asLeft
        }
      }

      // TODO (KR) : Description
      singleValue[T](
        baseName = baseName,
        primaryLongParamName = primaryLongParamName,
        primaryShortParamName = primaryShortParamName,
      )
    }

    def enumValues[T](
        baseName: String,
        values: Seq[T],
        nameF: T => List[String] = (t: T) => t.toString :: Nil,
        primaryLongParamName: Defaultable[String] = Defaultable.Auto,
        primaryShortParamName: DefaultableOption[Char] = Defaultable.Auto,
    )(implicit
        ct: ClassTag[T],
    ): Builder[T] =
      enumPairs(
        baseName = baseName,
        values = values.flatMap(v => nameF(v).map((_, v))),
        primaryLongParamName = primaryLongParamName,
        primaryShortParamName = primaryShortParamName,
      )

  }

  object toggle {

    final case class Builder(
        private val _baseName: String,
        private val _primaryLongParam: Param.LongToggle,
        private val _longParamAliases: List[Param.LongToggle],
        private val _primaryShortParam: Option[Param.ShortToggle],
        private val _shortParamAliases: List[Param.ShortToggle],
        private val _description: List[String],
    ) extends GenBuilder[Boolean] {

      // =====| Modify |=====

      def withPrimaryLongParam(truePrefix: Option[String], falsePrefix: Option[String]): Builder =
        this.copy(_primaryLongParam = Param.LongToggle(_baseName, truePrefix, falsePrefix))

      def withLongParamAliases(params: Param.LongToggle*): Builder =
        this.copy(_longParamAliases = params.toList)

      def addLongParamAliases(params: Param.LongToggle*): Builder =
        this.copy(_longParamAliases = _longParamAliases ::: params.toList)

      def withPrimaryShortParam(trueName: Char, falseName: Char): Builder =
        this.copy(_primaryShortParam = Param.ShortToggle(trueName, falseName).some)

      def withoutPrimaryShortParam: Builder =
        this.copy(_primaryShortParam = None)

      def withShortParamAliases(params: Param.ShortToggle*): Builder =
        this.copy(_shortParamAliases = params.toList)

      def addShortParamAliases(params: Param.ShortToggle*): Builder =
        this.copy(_shortParamAliases = _shortParamAliases ::: params.toList)

      def withDescription(description: String*): Builder =
        this.copy(_description = description.toList)

      def addDescription(description: String*): Builder =
        this.copy(_description = _description ::: description.toList)

      // =====| Build |=====

      override protected def primaryParamName: ColorString = _primaryLongParam.formattedName

      override protected def makeElement(requirementLevel: RequirementLevel): Element =
        Element.ParamElement(
          baseName = _baseName,
          typeName = "Boolean",
          primaryParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList),
          aliasParams = _longParamAliases ::: _shortParamAliases,
          requirementLevel = requirementLevel.some,
          description = _description,
        )

      override protected def optionalParseFunction(args: IndexedArgs, element: Element): Result[Option[Boolean]] = {
        import Arg.find.*

        val findFunction: FindFunction[Boolean] =
          FindFunction.first {
            FindFunction.fromParam(_primaryLongParam) ::
              _primaryShortParam.map(FindFunction.fromParam(_)).toList :::
              _longParamAliases.map(FindFunction.fromParam(_)) :::
              _shortParamAliases.map(FindFunction.fromParam(_))
          }

        findFunction.attemptToFind(args) match {
          case Some(found) => Result.success(found.arg.some, found.remaining)
          case None        => Result.success(None, args)
        }
      }

    }

    def apply[T](
        baseName: String,
        primaryLongParamTruePrefix: DefaultableOption[String] = Defaultable.Auto,
        primaryLongParamFalsePrefix: DefaultableOption[String] = Defaultable.Auto,
        primaryShortParamName: DefaultableOption[Char] = Defaultable.Auto,
    ): Builder =
      Builder(
        _baseName = baseName,
        _primaryLongParam = (primaryLongParamTruePrefix, primaryLongParamFalsePrefix) match {
          case (Defaultable.Auto, Defaultable.Auto) => Param.LongToggle(baseName, None, "no".some)

          case (Defaultable.Auto, Defaultable.Some(falsePrefix)) => Param.LongToggle(baseName, None, falsePrefix.some)
          case (Defaultable.Auto, Defaultable.None)              => Param.LongToggle(baseName, "yes".some, None)

          case (Defaultable.Some(truePrefix), Defaultable.Auto) => Param.LongToggle(baseName, truePrefix.some, None)
          case (Defaultable.None, Defaultable.Auto)             => Param.LongToggle(baseName, None, "no".some)

          case (truePrefix, falsePrefix) => Param.LongToggle(baseName, truePrefix.toOptionO(None), falsePrefix.toOptionV("no"))
        },
        _longParamAliases = Nil,
        _primaryShortParam = primaryShortParamName
          .toOptionO(baseName.headOption)
          .map { c => Param.ShortToggle(c.toUpper, c.toLower) },
        _shortParamAliases = Nil,
        _description = Nil,
      )

  }

  object flag {

    final case class Builder(
        private val _baseName: String,
        private val _primaryLongParam: Param.Long,
        private val _longParamAliases: List[Param.Long],
        private val _primaryShortParam: Option[Param.Short],
        private val _shortParamAliases: List[Param.Short],
        private val _description: List[String],
    ) extends GenBuilder[Boolean] {

      // =====| Modify |=====

      def withPrimaryLongParam(name: String): Builder =
        this.copy(_primaryLongParam = Param.Long(name))

      def withLongParamAliases(names: String*): Builder =
        this.copy(_longParamAliases = names.toList.map(Param.Long.apply))

      def addLongParamAliases(names: String*): Builder =
        this.copy(_longParamAliases = _longParamAliases ::: names.toList.map(Param.Long.apply))

      def withPrimaryShortParam(name: Char): Builder =
        this.copy(_primaryShortParam = Param.Short(name).some)

      def withoutPrimaryShortParam: Builder =
        this.copy(_primaryShortParam = None)

      def withShortParamAliases(names: Char*): Builder =
        this.copy(_shortParamAliases = names.toList.map(Param.Short.apply))

      def addShortParamAliases(names: Char*): Builder =
        this.copy(_shortParamAliases = _shortParamAliases ::: names.toList.map(Param.Short.apply))

      def withDescription(description: String*): Builder =
        this.copy(_description = description.toList)

      def addDescription(description: String*): Builder =
        this.copy(_description = _description ::: description.toList)

      // =====| Build |=====

      override protected def primaryParamName: ColorString = _primaryLongParam.formattedName

      override protected def makeElement(requirementLevel: RequirementLevel): Element =
        Element.ParamElement(
          baseName = _baseName,
          typeName = "Boolean",
          primaryParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList),
          aliasParams = _longParamAliases ::: _shortParamAliases,
          requirementLevel = requirementLevel.some,
          description = _description,
        )

      override protected def optionalParseFunction(args: IndexedArgs, element: Element): Result[Option[Boolean]] = {
        import Arg.find.*

        val findFunction: FindFunction[Unit] =
          FindFunction.first {
            FindFunction.fromParam(_primaryLongParam) ::
              _primaryShortParam.map(FindFunction.fromParam(_)).toList :::
              _longParamAliases.map(FindFunction.fromParam(_)) :::
              _shortParamAliases.map(FindFunction.fromParam(_))
          }

        findFunction.attemptToFind(args) match {
          case Some(found) => Result.success(true.some, found.remaining)
          case None        => Result.success(false.some, args)
        }
      }

    }

    def apply(
        baseName: String,
        primaryLongParamName: Defaultable[String] = Defaultable.Auto,
        primaryShortParamName: DefaultableOption[Char] = Defaultable.Auto,
    ): Builder =
      Builder(
        _baseName = baseName,
        _primaryLongParam = Param.Long(primaryLongParamName.toValue(baseName)),
        _longParamAliases = Nil,
        _primaryShortParam = primaryShortParamName.toOptionO(baseName.headOption).map(Param.Short.apply),
        _shortParamAliases = Nil,
        _description = Nil,
      )

  }

  object present {

    final case class Builder(
        private val _baseName: String,
        private val _primaryLongParam: Param.Long,
        private val _longParamAliases: List[Param.Long],
        private val _primaryShortParam: Option[Param.Short],
        private val _shortParamAliases: List[Param.Short],
        private val _description: List[String],
    ) extends GenBuilder[Unit] {

      // =====| Modify |=====

      def withPrimaryLongParam(name: String): Builder =
        this.copy(_primaryLongParam = Param.Long(name))

      def withLongParamAliases(names: String*): Builder =
        this.copy(_longParamAliases = names.toList.map(Param.Long.apply))

      def addLongParamAliases(names: String*): Builder =
        this.copy(_longParamAliases = _longParamAliases ::: names.toList.map(Param.Long.apply))

      def withPrimaryShortParam(name: Char): Builder =
        this.copy(_primaryShortParam = Param.Short(name).some)

      def withoutPrimaryShortParam: Builder =
        this.copy(_primaryShortParam = None)

      def withShortParamAliases(names: Char*): Builder =
        this.copy(_shortParamAliases = names.toList.map(Param.Short.apply))

      def addShortParamAliases(names: Char*): Builder =
        this.copy(_shortParamAliases = _shortParamAliases ::: names.toList.map(Param.Short.apply))

      def withDescription(description: String*): Builder =
        this.copy(_description = description.toList)

      def addDescription(description: String*): Builder =
        this.copy(_description = _description ::: description.toList)

      // =====| Build |=====

      override protected def primaryParamName: ColorString = _primaryLongParam.formattedName

      override protected def makeElement(requirementLevel: RequirementLevel): Element =
        Element.ParamElement(
          baseName = _baseName,
          typeName = "Boolean",
          primaryParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList),
          aliasParams = _longParamAliases ::: _shortParamAliases,
          requirementLevel = requirementLevel.some,
          description = _description,
        )

      override protected def optionalParseFunction(args: IndexedArgs, element: Element): Result[Option[Unit]] = {
        import Arg.find.*

        val findFunction: FindFunction[Unit] =
          FindFunction.first {
            FindFunction.fromParam(_primaryLongParam) ::
              _primaryShortParam.map(FindFunction.fromParam(_)).toList :::
              _longParamAliases.map(FindFunction.fromParam(_)) :::
              _shortParamAliases.map(FindFunction.fromParam(_))
          }

        findFunction.attemptToFind(args) match {
          case Some(found) => Result.success(().some, found.remaining)
          case None        => Result.failure(Error(element.some, Error.Reason.MissingRequired(_baseName)))(args)
        }
      }

    }

    def apply(
        baseName: String,
        primaryLongParamName: Defaultable[String] = Defaultable.Auto,
        primaryShortParamName: DefaultableOption[Char] = Defaultable.Auto,
    ): Builder =
      Builder(
        _baseName = baseName,
        _primaryLongParam = Param.Long(primaryLongParamName.toValue(baseName)),
        _longParamAliases = Nil,
        _primaryShortParam = primaryShortParamName.toOptionO(baseName.headOption).map(Param.Short.apply),
        _shortParamAliases = Nil,
        _description = Nil,
      )

  }

}
