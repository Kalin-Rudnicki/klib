package klib.unitTest.utils.commandLine.parse

import cats.syntax.option.*
import zio.test.Assertion.*
import zio.test.{Result as _, *}

import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.commandLine.parse.*

object ArgTests extends DefaultKSpec {

  private val parseSpec: TestSpec = {
    def makeTest(name: String)(strArgs: String*)(expArgs: Indexed[Arg]*): TestSpec =
      test(name)(assert(Arg.parse(strArgs.toList))(equalTo(expArgs.toList)))

    suite("parse")(
      suite("simple")(
        makeTest("empty")()(),
        makeTest("short-param-multi")(
          "-ab",
        )(
          Arg.ShortParamMulti('a', 0).atIndex(0),
          Arg.ShortParamMulti('b', 1).atIndex(0),
        ),
        makeTest("short-param-single")(
          "-a",
        )(
          Arg.ShortParamSingle('a').atIndex(0),
        ),
        makeTest("short-param-single-with-value")(
          "-a=value",
        )(
          Arg.ShortParamSingleWithValue('a', "value").atIndex(0),
        ),
        makeTest("long-param")(
          "--long-param",
        )(
          Arg.LongParam("long-param").atIndex(0),
        ),
        makeTest("long-param-with-value")(
          "--long-param=value",
        )(
          Arg.LongParamWithValue("long-param", "value").atIndex(0),
        ),
        makeTest("value")(
          "value",
        )(
          Arg.Value("value").atIndex(0),
        ),
        makeTest("escaped-value")(
          "[-]--escaped-value",
        )(
          Arg.Value("--escaped-value").atIndex(0),
        ),
      ),
      suite("complex")(
        // TODO (KR) :
      ),
    )
  }

  private val findSpec: TestSpec = {
    import Arg.find.*

    val foundSpec: TestSpec = {
      def assertFoundArg[A](arg: A): Assertion[Found[A]] =
        equalTo(arg).imap("foundArg", _.arg)
      def assertFoundBefore(before: Arg*): Assertion[Found[Any]] =
        equalTo(before.toList).imap("foundBefore", _.before.map(_.value))
      def assertFoundAfter(after: Arg*): Assertion[Found[Any]] =
        equalTo(after.toList).imap("foundAfter", _.after.map(_.value))

      def makeTest[A](name: String)(args: Arg*)(findF: FindFunction[A])(assertion: Assertion[Found[A]]): TestSpec =
        test(name)(assert(findF.attemptToFind(Indexed.list(args.toList)))(isSome(assertion)))

      suite("found")(
        makeTest("short-param-multi")(
          Arg.Value("a"),
          Arg.ShortParamMulti('b', 0),
          Arg.Value("c"),
        )(find('b').noValues)(
          assertFoundArg('b') &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("short-param-single")(
          Arg.Value("a"),
          Arg.ShortParamSingle('b'),
          Arg.Value("c"),
        )(find('b').noValues)(
          assertFoundArg('b') &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("short-param-single-with-value")(
          Arg.Value("a"),
          Arg.ShortParamSingleWithValue('b', "value"),
          Arg.Value("c"),
        )(find('b').singleValueWithName)(
          assertFoundArg(('b', "value")) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("long-param")(
          Arg.Value("a"),
          Arg.LongParam("b"),
          Arg.Value("c"),
        )(find("b").noValues)(
          assertFoundArg("b") &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
        makeTest("long-param-with-value")(
          Arg.Value("a"),
          Arg.LongParamWithValue("b", "value"),
          Arg.Value("c"),
        )(find("b").singleValueWithName)(
          assertFoundArg(("b", "value")) &&
            assertFoundBefore(Arg.Value("a")) &&
            assertFoundAfter(Arg.Value("c")),
        ),
      )
    }

    val notFoundSpec: TestSpec =
      suite("not-found")(
      )

    suite("find")(
      foundSpec,
      notFoundSpec,
    )
  }

  private val remainingArgsSpec: TestSpec = {
    def makeTest(name: String)(r1: Indexed[Arg]*)(r2: Indexed[Arg]*)(exp: Indexed[Arg]*): TestSpec =
      test(name)(assert(Arg.remainingInBoth(r1.toList, r2.toList))(equalTo(exp.toList)))

    suite("remaining-args")(
      makeTest("empty")()()(),
      makeTest("only in 1")(
        Arg.Value("A").atIndex(0),
      )()(),
      makeTest("only in 2")()(
        Arg.Value("B").atIndex(0),
      )(),
      makeTest("all in both")(
        Arg.Value("A").atIndex(0),
      )(
        Arg.Value("A").atIndex(0),
      )(
        Arg.Value("A").atIndex(0),
      ),
      makeTest("some in both")(
        Arg.Value("A").atIndex(0),
        Arg.Value("B").atIndex(1),
      )(
        Arg.Value("B").atIndex(1),
        Arg.Value("C").atIndex(2),
      )(
        Arg.Value("B").atIndex(1),
      ),
      makeTest("ordering is correct")(
        Arg.Value("A").atIndex(0),
        Arg.Value("B").atIndex(1),
        Arg.Value("D").atIndex(3),
        Arg.Value("E").atIndex(4),
      )(
        Arg.Value("B").atIndex(1),
        Arg.Value("C").atIndex(2),
        Arg.Value("D").atIndex(3),
      )(
        Arg.Value("B").atIndex(1),
        Arg.Value("D").atIndex(3),
      ),
      makeTest("short-arg-multi ordering is correct")(
        Arg.ShortParamMulti('a', 0).atIndex(0),
        Arg.ShortParamMulti('b', 1).atIndex(0),
        Arg.ShortParamMulti('d', 3).atIndex(0),
        Arg.ShortParamMulti('e', 4).atIndex(0),
      )(
        Arg.ShortParamMulti('b', 1).atIndex(0),
        Arg.ShortParamMulti('c', 2).atIndex(0),
        Arg.ShortParamMulti('d', 3).atIndex(0),
      )(
        Arg.ShortParamMulti('b', 1).atIndex(0),
        Arg.ShortParamMulti('d', 3).atIndex(0),
      ),
    )
  }

  override def spec: TestSpec =
    suite("ArgTests")(
      parseSpec,
      findSpec,
      remainingArgsSpec,
    )

}
