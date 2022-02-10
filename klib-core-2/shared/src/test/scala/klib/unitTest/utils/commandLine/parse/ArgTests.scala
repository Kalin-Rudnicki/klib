package klib.unitTest.utils.commandLine.parse

import cats.syntax.option.*
import zio.test.Assertion.*
import zio.test.{Result as _, *}

import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.commandLine.parse.*

object ArgTests extends DefaultKSpec {

  private val parseSpec: TestSpec = {
    def makeTest(name: String)(strArgs: String*)(expArgs: Arg*): TestSpec =
      test(name)(assert(Arg.parse(strArgs.toList))(equalTo(expArgs.toList)))

    suite("parse")(
      suite("simple")(
        makeTest("empty")()(),
        makeTest("short-param-multi")(
          "-ab",
        )(
          Arg.ShortParamMulti('a'),
          Arg.ShortParamMulti('b'),
        ),
        makeTest("short-param-single")(
          "-a",
        )(
          Arg.ShortParamSingle('a'),
        ),
        makeTest("short-param-single-with-value")(
          "-a=value",
        )(
          Arg.ShortParamSingleWithValue('a', "value"),
        ),
        makeTest("long-param")(
          "--long-param",
        )(
          Arg.LongParam("long-param"),
        ),
        makeTest("long-param-with-value")(
          "--long-param=value",
        )(
          Arg.LongParamWithValue("long-param", "value"),
        ),
        makeTest("value")(
          "value",
        )(
          Arg.Value("value"),
        ),
        makeTest("escaped-value")(
          "[-]--escaped-value",
        )(
          Arg.Value("--escaped-value"),
        ),
      ),
      suite("complex")(
        // TODO (KR) :
      ),
    )
  }

  private val findSpec: TestSpec = {
    import Arg.find.*
    import Arg.find.basic.*

    val foundSpec: TestSpec = {
      def assertFoundArg[A](arg: A): Assertion[Found[A]] =
        equalTo(arg).imap("foundArg", _.arg)
      def assertFoundBefore(before: Arg*): Assertion[Found[Any]] =
        equalTo(before.toList).imap("foundBefore", _.before)
      def assertFoundAfter(after: Arg*): Assertion[Found[Any]] =
        equalTo(after.toList).imap("foundAfter", _.after)

      def makeTest[A](name: String)(args: Arg*)(findF: List[Arg] => Option[Found[A]])(assertion: Assertion[Found[A]]): TestSpec =
        test(name)(assert(findF(args.toList))(isSome(assertion)))

      suite("found")(
        makeTest("short-param-multi")(
          Arg.Value("a"),
          Arg.ShortParamMulti('b'),
          Arg.Value("c"),
        )(shortParamMulti('b'))(
          assertFoundArg(Arg.ShortParamMulti('b')) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("short-param-single")(
          Arg.Value("a"),
          Arg.ShortParamSingle('b'),
          Arg.Value("c"),
        )(shortParamSingle('b'))(
          assertFoundArg(Arg.ShortParamSingle('b')) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("short-param-single-with-value")(
          Arg.Value("a"),
          Arg.ShortParamSingleWithValue('b', "value"),
          Arg.Value("c"),
        )(shortParamSingleWithValue('b'))(
          assertFoundArg(Arg.ShortParamSingleWithValue('b', "value")) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("long-param")(
          Arg.Value("a"),
          Arg.LongParam("b"),
          Arg.Value("c"),
        )(longParam("b"))(
          assertFoundArg(Arg.LongParam("b")) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("long-param-with-value")(
          Arg.Value("a"),
          Arg.LongParamWithValue("b", "value"),
          Arg.Value("c"),
        )(longParamWithValue("b"))(
          assertFoundArg(Arg.LongParamWithValue("b", "value")) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
      )
    }

    val notFoundSpec: TestSpec =
      suite("found")(
      )

    suite("find")(
      foundSpec,
      notFoundSpec,
    )
  }

  override def spec: TestSpec =
    suite("ArgTests")(
      parseSpec,
      findSpec,
    )

}
