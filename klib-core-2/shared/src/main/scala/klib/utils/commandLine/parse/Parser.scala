package klib.utils.commandLine.parse

import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import zio.Zippable

import klib.fp.typeclass.*

final case class BuiltParser[+T](
    parseF: List[Arg] => EitherNel[Error, T],
    helpString: HelpConfig => String,
) {

  def parse(argStrings: String*): BuiltParser.Result[T] =
    parse(argStrings.toList)

  def parse(argStrings: List[String]): BuiltParser.Result[T] = {
    val args: List[Arg] = Arg.parse(argStrings)

    val helpExtra: Option[Boolean] = {
      import Arg.find.basic.*

      def lp(name: String, value: Boolean): Option[Boolean] =
        longParam(name)(args).map(_ => value)

      def sp(name: Char, value: Boolean): Option[Boolean] =
        shortParamSingle(name)(args).map(_ => value) orElse
          shortParamMulti(name)(args).map(_ => value)

      lp("help-extra", true) orElse
        sp('H', true) orElse
        lp("help", false) orElse
        sp('h', false)
    }

    helpExtra match {
      case Some(helpExtra) => BuiltParser.Result.Help(helpString(HelpConfig.default(helpExtra)))
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
    final case class Help(helpString: String) extends Result[Nothing] {
      override def toString: String = helpString
    }
  }

}

final case class Parser[+T](
    parseF: List[Arg] => Parser.Result[T],
    elements: List[Element],
) {

  // =====| Building |=====

  def >&>[T2](other: Parser[T2])(implicit zip: Zippable[T, T2]): Parser[zip.Out] =
    Parser(
      parseF = { args =>
        val res1 = this.parseF(args)
        val res2 = other.parseF(res1.remainingArgs)
        Parser.Result(
          (res1.res, res2.res) match {
            case (Right(r1), Right(r2))   => zip.zip(r1, r2).asRight
            case (Left(err1), Right(_))   => err1.asLeft
            case (Right(_), Left(err2))   => err2.asLeft
            case (Left(err1), Left(err2)) => (err1 ::: err2).asLeft
          },
          res2.remainingArgs,
        )
      },
      elements = this.elements ::: other.elements,
    )

  def map[T2](f: T => T2): Parser[T2] =
    Parser(
      parseF = parseF(_).mapResult(f),
      elements = elements,
    )

  // =====| Build |=====

  private def build[Extras](extrasF: List[Arg] => EitherNel[Error, Extras])(implicit
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
            allParams = primaryParams,
            requirementLevel = None,
            description = description :: Nil,
          )
        }

        val allElements =
          List(
            makeHelpElement("help", 'h', "Display help message"),
            makeHelpElement("help-extra", 'H', "Display help message, with extra details"),
          ) ::: elements

        val allParamsNames: List[String] = allElements.flatMap(_.allParams.toList.flatMap(_.allParamStrings))
        val paramNamesCount: Map[String, Int] = allParamsNames.groupBy(identity).map { (k, v) => (k, v.size) }
        val duplicateNames: List[String] = paramNamesCount.filter(_._2 > 1).toList.map(_._1)

        val linePairs: List[Pair[String]] =
          Pair.zipPairs(helpConfig)(
            allElements.map(_.helpStringLinesPair(helpConfig, 0)),
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
    build(_.asRight)

}
object Parser {

  final case class Result[+T](
      res: EitherNel[Error, T],
      remainingArgs: List[Arg],
  ) {

    def mapResult[T2](f: T => T2): Result[T2] =
      Result(res.map(f), remainingArgs)

    def flatMapResult[T2](f: T => EitherNel[Error, T2]): Result[T2] =
      Result(res.flatMap(f), remainingArgs)

  }

  sealed trait GenBuilder[T] {

    protected def makeElement(requirementLevel: RequirementLevel): Element
    protected def optionalParseFunction(args: List[Arg], element: Element): Result[Option[T]]

    private final def build[T2](
        requirementLevel: RequirementLevel,
    )(
        mapRes: (Option[T], Element) => EitherNel[Error, T2],
    ): Parser[T2] = {
      val element: Element = makeElement(requirementLevel)
      val parseFunction: List[Arg] => Result[T2] = optionalParseFunction(_, element).flatMapResult(mapRes(_, element))

      Parser(
        parseF = parseFunction,
        elements = element :: Nil,
      )
    }

    final def required: Parser[T] =
      build[T](RequirementLevel.Required) { (res, element) =>
        res match {
          case Some(value) => value.asRight
          case None        => NonEmptyList.one(Error(element.some, Error.Reason.MissingRequired)).asLeft
        }
      }
    final def default(default: T): Parser[T] =
      build[T](RequirementLevel.Default) { (res, _) => res.getOrElse(default).asRight }
    final def optional: Parser[Option[T]] =
      build[Option[T]](RequirementLevel.Optional) { (res, _) => res.asRight }

  }

  def unit: Parser[Unit] =
    Parser(
      args => Result(().asRight, args),
      Nil,
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

      override protected def makeElement(requirementLevel: RequirementLevel): Element =
        Element.ParamElement(
          baseName = _baseName,
          typeName = _typeName,
          primaryParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList),
          aliasParams = _longParamAliases ::: _shortParamAliases,
          allParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList ::: _longParamAliases ::: _shortParamAliases),
          requirementLevel = requirementLevel.some,
          description = _description,
        )

      override protected def optionalParseFunction(args: List[Arg], element: Element): Result[Option[T]] = {
        import Arg.find.*

        @tailrec
        def findLoop[T](queue: List[T])(find: T => Option[Found[String]]): Option[Found[String]] =
          queue match {
            case head :: tail =>
              find(head) match {
                case some @ Some(_) => some
                case None           => findLoop(tail)(find)
              }
            case Nil =>
              None
          }

        def foundFromLongParams: Option[Found[String]] =
          findLoop(_primaryLongParam :: _longParamAliases)(p => combined.longParamWithValue(p.name)(args))
        def foundFromShortParams: Option[Found[String]] =
          findLoop(_primaryShortParam.toList ::: _shortParamAliases)(p => combined.shortParamWithValue(p.name)(args))
        def foundFromAny: Option[Found[String]] = foundFromLongParams orElse foundFromShortParams

        foundFromAny match {
          case Some(found) =>
            Result(
              res = _decodeFromString.decode(found.arg) match {
                case Left(error)  => NonEmptyList.one(Error(element.some, Error.Reason.MalformattedValue(found.arg))).asLeft
                case Right(value) => value.some.asRight
              },
              remainingArgs = found.remaining,
            )
          case None =>
            Result(
              res = None.asRight,
              remainingArgs = args,
            )
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

      override protected def makeElement(requirementLevel: RequirementLevel): Element =
        Element.ParamElement(
          baseName = _baseName,
          typeName = "Boolean",
          primaryParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList),
          aliasParams = _longParamAliases ::: _shortParamAliases,
          allParams = NonEmptyList(_primaryLongParam, _primaryShortParam.toList ::: _longParamAliases ::: _shortParamAliases),
          requirementLevel = requirementLevel.some,
          description = _description,
        )

      override protected def optionalParseFunction(args: List[Arg], element: Element): Result[Option[Boolean]] = {
        import Arg.find.*

        val allLongParams: List[Param.LongToggle] = _primaryLongParam :: _longParamAliases
        val allShortParams: List[Param.ShortToggle] = _primaryShortParam.toList ::: _shortParamAliases

        @tailrec
        def findLoop[T](queue: List[T])(find: T => Option[Found[Boolean]]): Option[Found[Boolean]] =
          queue match {
            case head :: tail =>
              find(head) match {
                case some @ Some(_) => some
                case None           => findLoop(tail)(find)
              }
            case Nil =>
              None
          }

        def foundFromLongParams: Option[Found[Boolean]] =
          findLoop(_primaryLongParam :: _longParamAliases) { p =>
            basic.longParam(p.trueName)(args).map(_.map(_ => true)) orElse
              basic.longParam(p.falseName)(args).map(_.map(_ => false))
          }
        def foundFromShortParams: Option[Found[Boolean]] =
          findLoop(_primaryShortParam.toList ::: _shortParamAliases) { p =>
            basic.shortParamSingle(p.trueName)(args).map(_.map(_ => true)) orElse
              basic.shortParamMulti(p.trueName)(args).map(_.map(_ => true)) orElse
              basic.shortParamSingle(p.falseName)(args).map(_.map(_ => false)) orElse
              basic.shortParamMulti(p.falseName)(args).map(_.map(_ => false))
          }
        def foundFromAny: Option[Found[Boolean]] = foundFromLongParams orElse foundFromShortParams

        foundFromAny match {
          case Some(found) =>
            Result(
              res = found.arg.some.asRight,
              remainingArgs = found.remaining,
            )
          case None =>
            Result(
              res = None.asRight,
              remainingArgs = args,
            )
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

}
