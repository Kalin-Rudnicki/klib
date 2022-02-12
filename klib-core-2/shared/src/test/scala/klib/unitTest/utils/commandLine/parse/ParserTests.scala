package klib.unitTest.utils.commandLine.parse

import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import zio.test.Assertion.*
import zio.test.{Result as _, *}

import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.commandLine.parse.*

object ParserTests extends DefaultKSpec {

  private def makeSuite[P](name: String)(parser: BuiltParser[P])(children: (BuiltParser[P] ?=> TestSpec)*): TestSpec =
    suite(name)(children.map(_(using parser))*)

  private def makeTest[P](
      name: String,
      args: List[String],
      assertion: => Assertion[BuiltParser.Result[P]],
  )(implicit parser: BuiltParser[P]): TestSpec =
    test(name)(assert(parser.parse(args))(assertion))

  private def makePassingTest[P: BuiltParser](name: String)(args: String*)(exp: => P): TestSpec =
    makeTest(
      name,
      args.toList,
      isSubtype[BuiltParser.Result.Success[P]](equalTo(exp).imap("result", _.result)),
    )

  private def makeFailingTest[P: BuiltParser](name: String)(args: String*)(assertions: Assertion[Error.Reason]*): TestSpec =
    makeTest(
      name,
      args.toList,
      isSubtype[BuiltParser.Result.Failure](
        assertNel(assertSeq(assertions.map(_.imap[Error]("reason", _.reason))*))
          .imap("errors", _.errors),
      ),
    )

  private def makeHelpTest[P: BuiltParser](name: String)(args: String*)(isHelpExtra: Boolean): TestSpec =
    makeTest(
      name,
      args.toList,
      isSubtype[BuiltParser.Result.Help](equalTo(isHelpExtra).imap("isHelpExtra", _.isHelpExtra)),
    )

  // =====|  |=====

  private val helpSpec: TestSpec =
    makeSuite("help") {
      Parser.unit.disallowExtras
    }(
      suite("single")(
        makeHelpTest("--help-extra")("--help-extra")(true),
        makeHelpTest("-H")("-H")(true),
        makeHelpTest("--help")("--help")(false),
        makeHelpTest("-h")("-h")(false),
      ),
      suite("multi")(
        makeHelpTest("--help-extra > --help")("--help-extra", "--help")(true),
        makeHelpTest("--help-extra > -h")("--help-extra", "-h")(true),
        makeHelpTest("-H > --help")("-H", "--help")(true),
        makeHelpTest("-H > -h")("-H", "-h")(true),
      ),
    )

  private val requirementSpec: TestSpec = {
    val requiredSpec: TestSpec =
      makeSuite("required") {
        Parser.singleValue[String]("string").required.disallowExtras
      }(
        makePassingTest("present")("--string", "VALUE")("VALUE"),
        makeFailingTest("missing")()(
          isSubtype[Error.Reason.MissingRequired.type](anything),
        ),
        makeFailingTest("missing value")("--string")(
          isSubtype[Error.Reason.MissingRequired.type](anything),
          isSubtype[Error.Reason.UnexpectedArg](anything),
        ),
      )

    val defaultSpec: TestSpec =
      makeSuite("default") {
        Parser.singleValue[String]("string").default("DEFAULT").disallowExtras
      }(
        makePassingTest("present")("--string", "VALUE")("VALUE"),
        makePassingTest("missing")()("DEFAULT"),
        makeFailingTest("missing value")("--string")(
          isSubtype[Error.Reason.UnexpectedArg](anything),
        ),
      )

    val optionalSpec: TestSpec =
      makeSuite("optional") {
        Parser.singleValue[String]("string").optional.disallowExtras
      }(
        makePassingTest("present")("--string", "VALUE")("VALUE".some),
        makePassingTest("missing")()(Option.empty[String]),
        makeFailingTest("missing value")("--string")(
          isSubtype[Error.Reason.UnexpectedArg](anything),
        ),
      )

    suite("requirement")(
      requiredSpec,
      defaultSpec,
      optionalSpec,
    )
  }

  private val andSpec: TestSpec =
    makeSuite("and") {
      (
        Parser.singleValue[String]("value-1", primaryShortParamName = Defaultable.None).required >&>
          Parser.singleValue[String]("value-2", primaryShortParamName = Defaultable.None).required
      ).disallowExtras
    }(
      makePassingTest("both present")("--value-1=VALUE-1", "--value-2", "VALUE-2")(("VALUE-1", "VALUE-2")),
      makeFailingTest("1 missing (1)")("--value-2", "VALUE-2")(
        isSubtype[Error.Reason.MissingRequired.type](anything),
      ),
      makeFailingTest("1 missing (2)")("--value-1=VALUE-1")(
        isSubtype[Error.Reason.MissingRequired.type](anything),
      ),
      makeFailingTest("2 missing")()(
        isSubtype[Error.Reason.MissingRequired.type](anything),
        isSubtype[Error.Reason.MissingRequired.type](anything),
      ),
    )

  private val orSpec: TestSpec =
    suite("or")(
    )

  // =====|  |=====

  override def spec: TestSpec =
    suite("ParserTests")(
      helpSpec,
      requirementSpec,
      andSpec,
      orSpec,
    )

}
