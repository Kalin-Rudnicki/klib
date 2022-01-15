package klib.unitTest

import zio._
import zio.test._

import klib.utils._

object SpecUtils {

  type TestEnv = Logger with FileSystem with RunMode

  val testLayers: Layer[Throwable, TestEnv] =
    FileSystem.live ++
      Logger.live(Logger.LogLevel.Debug) ++
      ZLayer.succeed(RunMode.Dev)

  abstract class KSpec[Env](implicit
      val tag: Tag[Env],
  ) extends RunnableSpec[TestEnvironment with Env, Any] {

    protected final type TestSpec = ZSpec[Environment, Any]

    protected def layers: ZLayer[TestEnvironment, Nothing, Env]

    override def aspects: List[TestAspectAtLeastR[Environment]] =
      List()

    override def runner: TestRunner[TestEnvironment with Env, Any] =
      TestRunner(TestExecutor.default(testEnvironment >+> layers))

  }

  abstract class DefaultKSpec extends KSpec[TestEnv] {

    override protected final def layers: ZLayer[TestEnvironment, Nothing, TestEnv] = testLayers.orDie

  }

}
