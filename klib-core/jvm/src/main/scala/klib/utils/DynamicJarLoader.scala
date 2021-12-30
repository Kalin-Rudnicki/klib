package klib.utils

import java.io._
import java.net.URLClassLoader
import java.util.jar._

import scala.reflect.ClassTag

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object DynamicJarLoader {

  // Get all instances of a given trait
  // Must have 0 arg constructor
  def getClassPathsOfTypeWithExtras[T: ClassTag, E](jarFile: File)(eF: T => E): IO[List[(String, E)]] = {
    val tClass = implicitly[ClassTag[T]].runtimeClass

    for {
      // --- Find all `.class` files ---
      classPaths <- for {
        jarBytes <- FileUtils.readFileBytes(jarFile)
        jarInputStream <- new JarInputStream(new ByteArrayInputStream(jarBytes), true).pure[IO]
        jarEntries <- {
          def getJarEntries(stack: List[JarEntry]): IO[List[JarEntry]] =
            for {
              mEntry <- Maybe(jarInputStream.getNextJarEntry).pure[IO]
              entries <- mEntry match {
                case Some(entry) =>
                  getJarEntries(entry :: stack)
                case None =>
                  stack.pure[IO]
              }
            } yield entries

          getJarEntries(Nil)
        }
        classPaths =
          jarEntries
            .map(_.getRealName)
            .filter(_.endsWith(".class"))
            .map { cp =>
              cp.substring(0, cp.length - 6).replaceAllLiterally("/", ".")
            }
      } yield classPaths
      // --- Find valid instances of `T` with zero-arg-constructor ---
      validClassPaths <- IO(new URLClassLoader(Array(jarFile.toURI.toURL), getClass.getClassLoader)).bracket { classLoader =>
        classPaths
          .map { cp =>
            for {
              klass <- classLoader.loadClass(cp).pure[IO]
              constructors <- klass.getDeclaredConstructors.toList.pure[IO]
              res <-
                constructors
                  .find(_.getParameterCount == 0)
                  .toMaybe
                  .map { constructor =>
                    if (constructor.trySetAccessible())
                      for {
                        inst <- constructor.newInstance().pure[IO]
                        isInst = tClass.isInstance(inst)
                      } yield isInst.maybe((cp, eF(inst.asInstanceOf[T])))
                    else
                      None.pure[IO]
                  }
                  .traverse
                  .map(_.flatten)
            } yield res
          }
          .traverse
          .map(_.flatMap(_.toOption))
      }(_.close().pure[IO])
    } yield validClassPaths
  }

  def getClassPathsOfType[T: ClassTag](jarFile: File): IO[List[String]] =
    getClassPathsOfTypeWithExtras[T, Unit](jarFile)(_ => ())
      .map(_.map(_._1))

  // Load the given 0-arg-constructor class
  // Anything in O must already exist in the class-path
  // TODO (KR) : Possibly de-dupe this
  def loadClassesFromJar[T, O](jarFile: File, classPaths: List[String])(withInstance: (String, T) => IO[O]): IO[List[O]] =
    IO(new URLClassLoader(Array(jarFile.toURI.toURL), getClass.getClassLoader)).bracket { classLoader =>
      def runForClassPath(classPath: String): IO[O] =
        for {
          klass <- classLoader.loadClass(classPath).pure[IO]
          zeroArgConstructor <- klass.getDeclaredConstructor().pure[IO]
          _ <- zeroArgConstructor.setAccessible(true).pure[IO]
          inst <- zeroArgConstructor.newInstance().asInstanceOf[T].pure[IO]
          res <- withInstance(classPath, inst)
        } yield res

      classPaths.map(runForClassPath).traverse
    }(_.close().pure[IO])

  def loadClassFromJar[T, O](jarFile: File, classPath: String)(withInstance: T => IO[O]): IO[O] =
    IO(new URLClassLoader(Array(jarFile.toURI.toURL), getClass.getClassLoader)).bracket { classLoader =>
      for {
        klass <- classLoader.loadClass(classPath).pure[IO]
        zeroArgConstructor <- klass.getDeclaredConstructor().pure[IO]
        _ <- zeroArgConstructor.setAccessible(true).pure[IO]
        inst <- zeroArgConstructor.newInstance().asInstanceOf[T].pure[IO]
        res <- withInstance(inst)
      } yield res
    }(_.close().pure[IO])

}
