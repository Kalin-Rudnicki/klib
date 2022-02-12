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
      )

    val defaultSpec: TestSpec =
      makeSuite("required") {
        Parser.singleValue[String]("string").default("DEFAULT").disallowExtras
      }()

    val optionalSpec: TestSpec =
      makeSuite("required") {
        Parser.singleValue[String]("string").optional.disallowExtras
      }()

    suite("requirement")(
      requiredSpec,
      defaultSpec,
      optionalSpec,
    )
  }

  // =====|  |=====

  override def spec: TestSpec =
    suite("ParserTests")(
      helpSpec,
      requirementSpec,
    )

}
