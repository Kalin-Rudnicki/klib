package klib

import klib.fp.types._

package object utils {

  trait Implicits extends ColorString.Implicits
  object Implicits extends Implicits

  type HasLogger = { val logger: Logger }

  final case class Command[T <: HasLogger](name: String, build: Seq[String] => T, run: T => ??[Unit]) {
    import klib.Implicits._

    private[utils] def runWithArgs(args: Seq[String]): IO[Unit] =
      for {
        conf <- build(args).pure[IO]
        (_, warnings, errors) = run(conf).run.toTuple
        _ <- conf.logger() { src =>
          def show(label: String, level: Logger.LogLevel, items: List[Throwable]): Unit =
            if (items.nonEmpty) {
              src.log(level, s"=====| $label (${items.size}) |=====")
              src.indented() { src =>
                items.foreach(src.debug(_))
                src.break
              }
            }

          show("Warnings", Logger.LogLevel.Warning, warnings)
          show("Errors", Logger.LogLevel.Fatal, errors)
        }
      } yield ()

  }

  def makeMain(commands: Command[_ <: HasLogger]*)(args: Array[String]): IO[Unit] = {
    val commandMap: Map[String, Command[_ <: HasLogger]] = commands.toList.map(c => c.name -> c).toMap

    def logFatal(msg: String): IO[Unit] =
      Logger(Logger.LogLevel.Fatal)() { src =>
        src.fatal(s"$msg. Options: ${commandMap.toList.map(_._1).sorted.mkString(", ")}")
      }

    args.toList match {
      case cmd :: args =>
        commandMap.get(cmd) match {
          case scala.Some(command) =>
            command.runWithArgs(args)
          case scala.None =>
            logFatal(s"Invalid command ($cmd)")
        }
      case Nil =>
        logFatal("Missing command")
    }
  }

}
