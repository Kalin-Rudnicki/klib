package klib.unitTest.utils

import zio.test.*
import zio.test.Assertion.*

import klib.unitTest.SpecUtils.*
import klib.utils.*

object ExecutableTests extends DefaultKSpec {

  private val splitArgsSpec: TestSpec = {
    def createTest(name: String, args: List[String], exp: => Executable.SplitArgs): TestSpec =
      test(name)(assert(Executable.SplitArgs.fromArgs(args))(equalTo(exp)))

    val basicSpec: TestSpec =
      suite("basic")(
        createTest(
          "empty-args",
          List(),
          Executable.SplitArgs(
            Nil,
            Nil,
            Nil,
          ),
        ),
        createTest(
          "sub-command-args",
          List("sc1", "sc2"),
          Executable.SplitArgs(
            List("sc1", "sc2"),
            Nil,
            Nil,
          ),
        ),
        createTest(
          "program-args-1",
          List("-p1", "p2"),
          Executable.SplitArgs(
            Nil,
            Nil,
            List("-p1", "p2"),
          ),
        ),
        createTest(
          "program-args-2",
          List("--", "p1", "p2"),
          Executable.SplitArgs(
            Nil,
            Nil,
            List("p1", "p2"),
          ),
        ),
        createTest(
          "logger-args",
          List("-l1", "l2", "--"),
          Executable.SplitArgs(
            Nil,
            List("-l1", "l2"),
            Nil,
          ),
        ),
      )

    val complexSpec: TestSpec =
      suite("complex")(
        createTest(
          "1",
          List("sc1", "sc2", "-p1", "p2"),
          Executable.SplitArgs(
            List("sc1", "sc2"),
            Nil,
            List("-p1", "p2"),
          ),
        ),
        createTest(
          "2",
          List("sc1", "sc2", "--", "p1", "p2"),
          Executable.SplitArgs(
            List("sc1", "sc2"),
            Nil,
            List("p1", "p2"),
          ),
        ),
        createTest(
          "3",
          List("sc1", "sc2", "-l1", "l2", "--", "p1", "p2"),
          Executable.SplitArgs(
            List("sc1", "sc2"),
            List("-l1", "l2"),
            List("p1", "p2"),
          ),
        ),
      )

    suite("split-args")(
      basicSpec,
      complexSpec,
    )
  }

  override def spec: TestSpec =
    suite("ExecutableTests")(
      splitArgsSpec,
    )

}
