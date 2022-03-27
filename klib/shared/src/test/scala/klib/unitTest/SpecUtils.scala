package klib.unitTest

import cats.data.NonEmptyList
import cats.syntax.option.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.AssertionM.Render.*

import klib.utils.*

object SpecUtils {

  type TestEnv = Logger with FileSystem with RunMode

  val testLayers: TaskLayerM[TestEnv] =
    FileSystem.live ++
      Logger.live(Logger.LogLevel.Debug) ++
      ZLayer.succeed(RunMode.Dev)

  abstract class KSpec[Env](implicit
      val tag: Tag[Env],
  ) extends RunnableSpec[TestEnvironment with Env, Any] {

    protected final type TestSpec = ZSpec[Environment, Any]

    protected def layers: RLayerM[TestEnvironment, Env]

    override def aspects: List[TestAspectAtLeastR[Environment]] =
      List()

    override def runner: TestRunner[TestEnvironment with Env, Any] =
      TestRunner(TestExecutor.default(testEnvironment >+> layers.orDieKlib))

  }

  abstract class DefaultKSpec extends KSpec[TestEnv] {

    override protected final def layers: RLayerM[TestEnvironment, TestEnv] =
      testLayers

  }

  // =====| Assertions |=====

  def assertNel[T](assertion: Assertion[List[T]]): Assertion[NonEmptyList[T]] =
    assertion.imap("NonEmptyList", _.toList)

  def assertSeq[T](assertions: Assertion[T]*): Assertion[Seq[T]] =
    assertions.toList.zipWithIndex.foldLeft[Assertion[Seq[T]]](hasSize(equalTo(assertions.size))) { case (j, (a, i)) => j && hasAt(i)(a) }

  extension [A](assertion: Assertion[A]) {

    def imap[B](name: String, f: B => A): Assertion[B] =
      assertionRec(name)(param(assertion))(assertion)(f(_).some)

  }

}
