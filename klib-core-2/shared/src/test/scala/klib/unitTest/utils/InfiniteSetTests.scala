package klib.unitTest.utils

import zio.test.*
import zio.test.Assertion.*

import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.InfiniteSet.*

object InfiniteSetTests extends DefaultKSpec {

  private final case class TestData(
      name: String,
      data: InfiniteSet[Int],
  )
  private object TestData {

    val Inclusive1: TestData = TestData("inclusive-1", Inclusive(1, 2))
    val Inclusive2: TestData = TestData("inclusive-2", Inclusive(2, 3))

    val Exclusive1: TestData = TestData("exclusive-1", Exclusive(1, 2))
    val Exclusive2: TestData = TestData("exclusive-2", Exclusive(2, 3))

  }

  private val operationsSpec: TestSpec = {
    final class Contains(
        contains1: => Boolean,
        contains2: => Boolean,
        contains3: => Boolean,
        contains4: => Boolean,
    ) {

      def _contains1: Boolean = contains1
      def _contains2: Boolean = contains2
      def _contains3: Boolean = contains3
      def _contains4: Boolean = contains4

    }
    final class Ops(
        explicit: => Set[Int],
        op_| : => InfiniteSet[Int],
        op_& : => InfiniteSet[Int],
        op_&~ : => InfiniteSet[Int],
    ) {

      def _explicit: Set[Int] = explicit
      def _op_| : InfiniteSet[Int] = op_|
      def _op_& : InfiniteSet[Int] = op_&
      def _op_&~ : InfiniteSet[Int] = op_&~

    }

    def makeSuite(
        testData: TestData,
    )(
        op_~ : => InfiniteSet[Int],
        contains: => Contains,
        inclusive1Ops: => Ops,
        inclusive2Ops: => Ops,
        exclusive1Ops: => Ops,
        exclusive2Ops: => Ops,
    ): TestSpec = {
      def suite_~ : TestSpec =
        test("~")(
          assert(testData.data.~)(equalTo(op_~)),
        )

      def suiteContains: TestSpec = {
        def makeTest(i: Int, c: => Boolean): TestSpec =
          test(i.toString)(assert(testData.data.contains(i))(equalTo(c)))

        suite("contains")(
          makeTest(1, contains._contains1),
          makeTest(2, contains._contains2),
          makeTest(3, contains._contains3),
          makeTest(4, contains._contains4),
        )
      }

      def suiteOps(
          otherTestData: TestData,
      )(
          ops: Ops,
      ): TestSpec =
        suite(otherTestData.name)(
          test("explicit")(assert(InfiniteSet.explicit(testData.data, otherTestData.data))(equalTo(ops._explicit))),
          test("|")(assert(testData.data | otherTestData.data)(equalTo(ops._op_|))),
          test("&")(assert(testData.data & otherTestData.data)(equalTo(ops._op_&))),
          test("&~")(assert(testData.data &~ otherTestData.data)(equalTo(ops._op_&~))),
        )

      suite(testData.name)(
        suite_~,
        suiteContains,
        suiteOps(TestData.Inclusive1)(inclusive1Ops),
        suiteOps(TestData.Inclusive2)(inclusive2Ops),
        suiteOps(TestData.Exclusive1)(exclusive1Ops),
        suiteOps(TestData.Exclusive2)(exclusive2Ops),
      )
    }

    val inclusive1Spec: TestSpec =
      makeSuite(TestData.Inclusive1)(
        op_~ = Exclusive(1, 2),
        contains = Contains(
          contains1 = true,
          contains2 = true,
          contains3 = false,
          contains4 = false,
        ),
        inclusive1Ops = Ops(
          explicit = Set(1, 2),
          op_| = Inclusive(1, 2),
          op_& = Inclusive(1, 2),
          op_&~ = Inclusive(),
        ),
        inclusive2Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Inclusive(1, 2, 3),
          op_& = Inclusive(2),
          op_&~ = Inclusive(1),
        ),
        exclusive1Ops = Ops(
          explicit = Set(1, 2),
          op_| = Exclusive(),
          op_& = Inclusive(),
          op_&~ = Inclusive(1, 2),
        ),
        exclusive2Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Exclusive(3),
          op_& = Inclusive(1),
          op_&~ = Inclusive(2),
        ),
      )

    val inclusive2Spec: TestSpec =
      makeSuite(TestData.Inclusive2)(
        op_~ = Exclusive(2, 3),
        contains = Contains(
          contains1 = false,
          contains2 = true,
          contains3 = true,
          contains4 = false,
        ),
        inclusive1Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Inclusive(1, 2, 3),
          op_& = Inclusive(2),
          op_&~ = Inclusive(3),
        ),
        inclusive2Ops = Ops(
          explicit = Set(2, 3),
          op_| = Inclusive(2, 3),
          op_& = Inclusive(2, 3),
          op_&~ = Inclusive(),
        ),
        exclusive1Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Exclusive(1),
          op_& = Inclusive(3),
          op_&~ = Inclusive(2),
        ),
        exclusive2Ops = Ops(
          explicit = Set(2, 3),
          op_| = Exclusive(),
          op_& = Inclusive(),
          op_&~ = Inclusive(2, 3),
        ),
      )

    val exclusive1Spec: TestSpec =
      makeSuite(TestData.Exclusive1)(
        op_~ = Inclusive(1, 2),
        contains = Contains(
          contains1 = false,
          contains2 = false,
          contains3 = true,
          contains4 = true,
        ),
        inclusive1Ops = Ops(
          explicit = Set(1, 2),
          op_| = Exclusive(),
          op_& = Inclusive(),
          op_&~ = Exclusive(1, 2),
        ),
        inclusive2Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Exclusive(1),
          op_& = Inclusive(3),
          op_&~ = Exclusive(1, 2, 3),
        ),
        exclusive1Ops = Ops(
          explicit = Set(1, 2),
          op_| = Exclusive(1, 2),
          op_& = Exclusive(1, 2),
          op_&~ = Inclusive(),
        ),
        exclusive2Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Exclusive(2),
          op_& = Exclusive(1, 2, 3),
          op_&~ = Inclusive(3),
        ),
      )

    val exclusive2Spec: TestSpec =
      makeSuite(TestData.Exclusive2)(
        op_~ = Inclusive(2, 3),
        contains = Contains(
          contains1 = true,
          contains2 = false,
          contains3 = false,
          contains4 = true,
        ),
        inclusive1Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Exclusive(3),
          op_& = Inclusive(1),
          op_&~ = Exclusive(1, 2, 3),
        ),
        inclusive2Ops = Ops(
          explicit = Set(2, 3),
          op_| = Exclusive(),
          op_& = Inclusive(),
          op_&~ = Exclusive(2, 3),
        ),
        exclusive1Ops = Ops(
          explicit = Set(1, 2, 3),
          op_| = Exclusive(2),
          op_& = Exclusive(1, 2, 3),
          op_&~ = Inclusive(1),
        ),
        exclusive2Ops = Ops(
          explicit = Set(2, 3),
          op_| = Exclusive(2, 3),
          op_& = Exclusive(2, 3),
          op_&~ = Inclusive(),
        ),
      )

    suite("Operations")(
      inclusive1Spec,
      inclusive2Spec,
      exclusive1Spec,
      exclusive2Spec,
    )
  }

  private val setTheorySpec: TestSpec = {
    def makeSuite(testData: TestData, otherTestData: TestData): TestSpec = {
      def basicSuite: TestSpec =
        suite("basic")(
          test("set == set")(assert(testData.data)(equalTo(testData.data))),
          test("test.~.~ == set")(assert(testData.data.~.~)(equalTo(testData.data))),
          test("set.~.explicit == set.explicit")(assert(testData.data.~.explicit)(equalTo(testData.data.explicit))),
        )
      def associativitySuite: TestSpec = {
        def makeSuite: TestSpec =
          suite("associativity")(
            test("(set | otherSet) == (otherSet | set)")(
              assert(testData.data | otherTestData.data)(equalTo(otherTestData.data | testData.data)),
            ),
            test("(set & otherSet) == (otherSet & set)")(
              assert(testData.data & otherTestData.data)(equalTo(otherTestData.data & testData.data)),
            ),
            test("(set &~ otherSet) != (otherSet &~ set)")(
              assert(testData.data &~ otherTestData.data)(not(equalTo(otherTestData.data &~ testData.data))),
            ),
          )

        if (testData.data == otherTestData.data) makeSuite @@ zio.test.TestAspect.ignore
        else makeSuite
      }
      def selfOperationsSuite: TestSpec =
        suite("self-operations")(
          test("(set | set) == set")(assert(testData.data | testData.data)(equalTo(testData.data))),
          test("(set & set) == set")(assert(testData.data & testData.data)(equalTo(testData.data))),
          test("(set &~ set) == InfiniteSet.empty")(assert(testData.data &~ testData.data)(equalTo(InfiniteSet.empty))),
        )
      def emptyOperationsSuite: TestSpec =
        suite("empty-operations")(
          test("(set | InfiniteSet.empty) == set")(
            assert(testData.data | InfiniteSet.empty)(equalTo(testData.data)),
          ),
          test("(set & InfiniteSet.empty) == InfiniteSet.empty")(
            assert(testData.data & InfiniteSet.empty)(equalTo(InfiniteSet.empty)),
          ),
          test("(set &~ InfiniteSet.empty) == set")(
            assert(testData.data &~ InfiniteSet.empty)(equalTo(testData.data)),
          ),
          test("(InfiniteSet.empty &~ set) == InfiniteSet.empty")(
            assert(InfiniteSet.empty &~ testData.data)(equalTo(InfiniteSet.empty)),
          ),
        )
      def fullOperationsSuite: TestSpec =
        suite("full-operations")(
          test("(set | InfiniteSet.full) == InfiniteSet.full")(
            assert(testData.data | InfiniteSet.full)(equalTo(InfiniteSet.full)),
          ),
          test("(set & InfiniteSet.full) == set")(
            assert(testData.data & InfiniteSet.full)(equalTo(testData.data)),
          ),
          test("(set &~ InfiniteSet.full) == InfiniteSet.empty")(
            assert(testData.data &~ InfiniteSet.full)(equalTo(InfiniteSet.empty)),
          ),
          test("(InfiniteSet.full &~ set) == set.~")(
            assert(InfiniteSet.full &~ testData.data)(equalTo(testData.data.~)),
          ),
        )

      suite(testData.name)(
        basicSuite,
        associativitySuite,
        selfOperationsSuite,
        emptyOperationsSuite,
        fullOperationsSuite,
      )
    }

    suite("set-theory")(
      makeSuite(TestData.Inclusive1, TestData.Exclusive2),
      makeSuite(TestData.Inclusive2, TestData.Exclusive1),
      makeSuite(TestData.Exclusive1, TestData.Inclusive2),
      makeSuite(TestData.Exclusive2, TestData.Inclusive1),
    )
  }

  override def spec: TestSpec =
    suite("InfiniteSetTests")(
      operationsSpec,
      setTheorySpec,
    )

}
