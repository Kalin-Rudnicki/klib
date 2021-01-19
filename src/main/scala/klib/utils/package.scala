package klib

import klib.fp.types.{Alive, Dead, ErrorAccumulator}

package object utils {

  trait Implicits extends ColorString.Implicits
  object Implicits extends Implicits

  def execErrorAccumulator[E, W, R](
      logger: Logger,
  )(
      ea: ErrorAccumulator[E, W, R],
  )(
      logError: Logger.Source => E => Unit,
  )(
      logWarning: Logger.Source => W => Unit,
  )(
      logResult: Logger.Source => R => Unit,
  ): Nothing = {
    logger() { src =>
      def _logErrors(errors: List[E]): Unit = {
        src.error(s"Errors (${errors.size}):")
        src.indented() { src =>
          errors.foreach(logError(src))
        }
      }
      def _logWarnings(warnings: List[W]): Unit = {
        src.error(s"Warnings (${warnings.size}):")
        src.indented() { src =>
          warnings.foreach(logWarning(src))
        }
      }
      def _logResult(result: R): Unit = {
        src.print("Result:")
        src.indented() { src =>
          logResult(src)(result)
        }
      }

      ea match {
        case Alive(r, warnings) =>
          if (warnings.nonEmpty)
            _logWarnings(warnings)
          _logResult(r)

          System.exit(0)
        case Dead(errors, warnings) =>
          if (errors.nonEmpty)
            _logErrors(errors)
          if (warnings.nonEmpty)
            _logWarnings(warnings)

          System.exit(1)
      }
    }

    ???
  }

}
