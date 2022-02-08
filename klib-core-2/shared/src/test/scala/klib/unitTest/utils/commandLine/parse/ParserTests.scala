package klib.unitTest.utils.commandLine.parse

import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import zio.test.Assertion.*
import zio.test.AssertionM.Render.*
import zio.test.{Result as _, *}

import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.commandLine.parse.*

object ParserTests extends DefaultKSpec {

  private final case class TestCase[P](
      name: String,
      args: List[String],
      assertion: Assertion[EitherNel[Error, (P, List[Arg])]],
      shouldPass: Boolean,
  ) {

    def toTest(parser: BuiltParser[(P, List[Arg])]): TestSpec =
      test(name)(assert(parser.parse(Arg.parse(args)))(assertion))

  }
  private object TestCase {

    private def passingAssertion[P](exp: P): Assertion[EitherNel[Error, (P, List[Arg])]] =
      isRight(equalTo(exp).imap[(P, List[Arg])]("res", _._1))

    private def failingAssertion[P](errorAssertion: Assertion[NonEmptyList[Error]]): Assertion[EitherNel[Error, (P, List[Arg])]] =
      isLeft(errorAssertion)

    private def remainingAssertion[P](outArgs: List[Arg]): Assertion[EitherNel[Error, (P, List[Arg])]] =
      isRight(equalTo(outArgs).imap[(P, List[Arg])]("remainingArgs", _._2))

    def passing[P](name: String)(inArgs: String*)(outArgs: Arg*)(exp: P): TestCase[P] =
      TestCase(
        name = name,
        args = inArgs.toList,
        assertion = passingAssertion[P](exp) && remainingAssertion[P](outArgs.toList),
        shouldPass = true,
      )

    def failing[P](name: String)(inArgs: String*)(errors: Assertion[Error]*): TestCase[P] =
      TestCase(
        name = name,
        args = inArgs.toList,
        assertion = failingAssertion[P](assertNel(assertSeq(errors*))),
        shouldPass = false,
      )

  }

  private final case class TestCaseSuite[P](
      name: String,
      parser: Parser[P],
      testCases: List[TestCase[P]],
  ) {
    private val builtParser: BuiltParser[(P, List[Arg])] = parser.extrasAsArgs

    def toSuite: TestSpec = {
      val (passes, fails) = testCases.partition(_.shouldPass)

      suite(name)(
        suite("Passes")(passes.map(_.toTest(builtParser))*),
        suite("Fails")(fails.map(_.toTest(builtParser))*),
      )
    }

  }
  private object TestCaseSuite {

    def build[P](name: String)(parser: Parser[P])(testCases: TestCase[P]*): TestCaseSuite[P] =
      TestCaseSuite(name, parser, testCases.toList)

  }

  private def reasonAssertion(assertion: Assertion[Error.Reason]): Assertion[Error] =
    assertion.imap("reason", _.reason)

  // =====| ... |=====

  private val personSpec1: TestSpec = {
    final case class Person(
        firstName: String,
        lastName: String,
        age: Int,
    )

    TestCaseSuite
      .build("person1")(
        (
          Parser.singleValue[String]("first-name").required >&>
            Parser.singleValue[String]("last-name").required >&>
            Parser.singleValue[Int]("age").required
        ).map(Person.apply),
      )(
        TestCase.failing("empty")()(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
        ),
        TestCase.failing("missing 2")(
          "--first-name",
          "First",
        )(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
        ),
        TestCase.failing("missing 1")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
        )(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
        ),
        TestCase.passing("success")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
          "--age",
          "100",
        )()(Person("First", "Last", 100)),
        TestCase.passing("success + extra")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
          "--age",
          "100",
          "unrelated-value",
        )(
          Arg.Value("unrelated-value"),
        )(Person("First", "Last", 100)),
        TestCase.failing("malformatted")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
          "--age",
          "not-an-int",
        )(
          reasonAssertion(equalTo(Error.Reason.MalformattedValue("not-an-int"))),
        ),
        TestCase.failing("malformatted + missing 1")(
          "--first-name",
          "First",
          "--age",
          "not-an-int",
        )(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
          reasonAssertion(equalTo(Error.Reason.MalformattedValue("not-an-int"))),
        ),
      )
      .toSuite
  }

  private val personSpec2: TestSpec = {
    final case class Person(
        firstName: String,
        lastName: String,
        age: Option[Int],
    )

    TestCaseSuite
      .build("person2")(
        (
          Parser.singleValue[String]("first-name").required >&>
            Parser.singleValue[String]("last-name").required >&>
            Parser.singleValue[Int]("age").optional
        ).map(Person.apply),
      )(
        TestCase.failing("empty")()(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
        ),
        TestCase.failing("missing 2")(
          "--first-name",
          "First",
        )(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
        ),
        TestCase.passing("success 1")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
        )()(Person("First", "Last", None)),
        TestCase.passing("success 2")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
          "--age",
          "100",
        )()(Person("First", "Last", 100.some)),
        TestCase.passing("success + extra")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
          "--age",
          "100",
          "unrelated-value",
        )(
          Arg.Value("unrelated-value"),
        )(Person("First", "Last", 100.some)),
        TestCase.failing("malformatted")(
          "--first-name",
          "First",
          "--last-name",
          "Last",
          "--age",
          "not-an-int",
        )(
          reasonAssertion(equalTo(Error.Reason.MalformattedValue("not-an-int"))),
        ),
        TestCase.failing("malformatted + missing 1")(
          "--first-name",
          "First",
          "--age",
          "not-an-int",
        )(
          reasonAssertion(equalTo(Error.Reason.MissingRequired)),
          reasonAssertion(equalTo(Error.Reason.MalformattedValue("not-an-int"))),
        ),
      )
      .toSuite
  }

  override def spec: TestSpec =
    suite("ParserTests")(
      personSpec1,
      personSpec2,
    )

}
