package klib.utils

import cats.syntax.option.*
import java.io.ByteArrayInputStream
import java.net.URLClassLoader
import java.util.jar.*
import scala.reflect.ClassTag
import zio.*

import klib.utils.*

object DynamicJarLoader {

  final class Jar(file: File) {

    private[DynamicJarLoader] def managed: KRIO[Scope, ClassLoader] =
      ZIO.acquireReleaseClosable {
        for {
          url <- ZIO.kAttempt("Unable to convert file to URL")(file.toPath.toUri.toURL)
          classLoader <- ZIO.kAttempt("Unable to create URLClassLoader")(URLClassLoader(Array(url), getClass.getClassLoader))
        } yield classLoader
      }

    def use[R, A](useF: ClassLoader => KRIO[R, A]): KRIO[R, A] =
      ZIO.scoped { managed.flatMap(useF) }

  }

  final class JarClass[T](jar: Jar, classPath: String, klass: Class[T]) {

    def managed: KRIO[Scope, T] =
      jar.managed.flatMap { classLoader =>
        for {
          c <- ZIO.kAttempt(s"Unable to load class : $classPath")(classLoader.loadClass(classPath))
          constructors <- ZIO.kAttempt(s"Unable to get declared constructors : $classPath")(c.getDeclaredConstructors)
          zeroArgConstructor = constructors.find(_.getParameterCount == 0)
          constructor <- ZIO.fromOptionKError(zeroArgConstructor)(KError.UserError(s"No zero arg constructor found : $classPath"))
          _ <- ZIO.kAttempt(s"Unable to set constructor as accessible : $classPath")(constructor.setAccessible(true))
          inst <- ZIO.kAttempt(s"Unable to instantiate new instance : $classPath")(constructor.newInstance())
          isInstance <- ZIO.kAttempt(s"Unable to check if instance : $classPath")(klass.isInstance(inst))
          casted <-
            if (isInstance) ZIO.succeed(inst.asInstanceOf[T])
            else ZIO.failNEL(KError.Unexpected(s"'$classPath' is not an instance of $klass"))
        } yield casted
      }

    def use[R, A](useF: T => KRIO[R, A]): KRIO[R, A] =
      ZIO.scoped { managed.flatMap(useF) }

  }

  final class Builder1[T] private[DynamicJarLoader] (klass: Class[T]) {

    def inFile(file: File): KTask[List[JarClass[T]]] = {
      def getJarEntries(jarInputStream: JarInputStream, stack: List[JarEntry]): KTask[List[JarEntry]] =
        ZIO.kAttempt(s"Unable to get next jar entry : $file")(Option(jarInputStream.getNextJarEntry)).flatMap {
          case Some(next) => getJarEntries(jarInputStream, next :: stack)
          case None       => ZIO.succeed(stack.reverse)
        }

      def getClassPaths(jarEntries: List[JarEntry]): List[String] =
        jarEntries
          .map(_.getRealName)
          .filter(_.endsWith(".class"))
          .map { cp =>
            cp.substring(0, cp.length - 6).replaceAllLiterally("/", ".")
          }

      def getJarClass(jar: Jar, classLoader: ClassLoader, classPath: String): KTask[Option[JarClass[T]]] =
        (for {
          c <- ZIO.kAttempt(s"Unable to load class : $classPath")(classLoader.loadClass(classPath)).asSomeError
          _ <-
            ZIO
              .kAttempt(s"Unable to check if class is assignable from : $classPath")(klass.isAssignableFrom(c) && klass.toString != c.toString)
              .map(Option.when(_)(()))
              .some
          constructors <- ZIO.kAttempt(s"Unable to get declared constructors : $classPath")(c.getDeclaredConstructors).asSomeError
          zeroArgConstructor <- ZIO.fromOptionKError(constructors.find(_.getParameterCount == 0))(KError.UserError(s"No zero arg constructor found : $classPath")).asSomeError
          // TODO (KR) : Should this use 'setAccessible' or 'trySetAccessible'?
          _ <- ZIO.kAttempt(s"Unable to set constructor as accessible : $classPath")(zeroArgConstructor.setAccessible(true)).asSomeError
          inst <- ZIO.kAttempt(s"Unable to instantiate new instance : $classPath")(zeroArgConstructor.newInstance()).asSomeError
          isInstance <- ZIO.kAttempt(s"Unable to check if instance : $classPath")(klass.isInstance(inst)).asSomeError
          _ <- ZIO.failUnlessNEL(isInstance, KError.Unexpected(s"Class is not actually assignable... : $classPath")).asSomeError
        } yield JarClass(jar, classPath, klass)).optional

      for {
        jarBytes <- file.readBytes
        jarInputStream = ZIO.acquireReleaseClosable { ZIO.kAttempt(s"Unable to create JarInputStream : $file")(JarInputStream(ByteArrayInputStream(jarBytes), true)) }
        jarEntries <- ZIO.scoped { jarInputStream.flatMap(getJarEntries(_, Nil)) }
        classPaths = getClassPaths(jarEntries)
        jar = Jar(file)
        jarClasses <- jar.use { classLoader => ZIO.foreach(classPaths)(getJarClass(jar, classLoader, _)) }
      } yield jarClasses.flatten
    }

    def inDir(file: File): KTask[List[JarClass[T]]] =
      for {
        children <- file.children.map(_.toList)
        jarChildren = children.toList.filter { f => f.fileName.ext == "jar".some }
        results <- ZIO.foreach(jarChildren)(inFile)
      } yield results.flatten

  }

  def forType[T: ClassTag]: Builder1[T] =
    Builder1(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])

}
