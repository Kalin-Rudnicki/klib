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

    private[DynamicJarLoader] def managed: RIOM[Scope, ClassLoader] =
      ZIO.acquireRelease {
        for {
          url <- ZIOM.attempt(file.toPath.toUri.toURL)
          classLoader <- ZIOM.attempt(URLClassLoader(Array(url), getClass.getClassLoader))
        } yield classLoader
      } { classLoader =>
        ZIOM.attempt(classLoader.close).orDieKlib
      }

    def use[R, A](useF: ClassLoader => RIOM[R, A]): RIOM[R, A] =
      ZIO.scoped { managed.flatMap(useF) }

  }

  final class JarClass[T](jar: Jar, classPath: String, klass: Class[T]) {

    def managed: RIOM[Scope, T] =
      jar.managed.flatMap { classLoader =>
        for {
          c <- ZIOM.attempt(classLoader.loadClass(classPath))
          constructors <- ZIOM.attempt(c.getDeclaredConstructors)
          zeroArgConstructor = constructors.find(_.getParameterCount == 0)
          constructor <- ZIO.fromOption(zeroArgConstructor).orElseFail(KError.message.same("No zero arg constructor found"))
          _ <- ZIOM.attempt(constructor.setAccessible(true))
          inst <- ZIOM.attempt(constructor.newInstance())
          isInstance <- ZIOM.attempt(klass.isInstance(inst))
          casted <- ZIO.cond(isInstance, inst.asInstanceOf[T], KError.message.same(s"'$classPath' is not an instance of $klass"))
        } yield casted
      }

    def use[R, A](useF: T => RIOM[R, A]): RIOM[R, A] =
      ZIO.scoped { managed.flatMap(useF) }

  }

  final class Builder1[T] private[DynamicJarLoader] (klass: Class[T]) {

    def inFile(file: File): TaskM[List[JarClass[T]]] = {
      def getJarEntries(jarInputStream: JarInputStream, stack: List[JarEntry]): TaskM[List[JarEntry]] =
        ZIOM.attempt(Option(jarInputStream.getNextJarEntry)).flatMap {
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

      def getJarClass(jar: Jar, classLoader: ClassLoader, classPath: String): TaskM[Option[JarClass[T]]] =
        (for {
          c <- ZIOM.attempt(classLoader.loadClass(classPath)).asSomeError
          _ <- ZIO.whenZIO(ZIOM.attempt(klass.isAssignableFrom(c) && klass.toString != c.toString))(ZIO.unit).some
          constructors <- ZIOM.attempt(c.getDeclaredConstructors).asSomeError
          zeroArgConstructor <-
            ZIO
              .fromOption(constructors.find(_.getParameterCount == 0))
              .orElseFail(KError.message.same(s"No zero arg constructor for '$classPath'").some)
          setAccessible <- ZIOM.attempt(zeroArgConstructor.trySetAccessible()).asSomeError
          _ <- ZIO.cond(setAccessible, (), KError.message.same("Zero arg constructor is not accessible").some)
          inst <- ZIOM.attempt(zeroArgConstructor.newInstance()).asSomeError
          isIsntance <- ZIOM.attempt(klass.isInstance(inst)).asSomeError
          _ <- ZIO.cond(isIsntance, (), KError.message.unexpected("Class is not actually assignable...?").some)
        } yield JarClass(jar, classPath, klass)).optional

      for {
        jarBytes <- file.readBytes
        jarInputStream = ZIO.acquireRelease { ZIOM.attempt(JarInputStream(ByteArrayInputStream(jarBytes), true)) } { bais => ZIOM.attempt(bais.close).orDieKlib }
        jarEntries <- ZIO.scoped { jarInputStream.flatMap(getJarEntries(_, Nil)) }
        classPaths = getClassPaths(jarEntries)
        jar = Jar(file)
        jarClasses <- jar.use { classLoader => ZIO.foreach(classPaths)(getJarClass(jar, classLoader, _)) }
      } yield jarClasses.flatten
    }

    def inDir(file: File): TaskM[List[JarClass[T]]] =
      for {
        children <- file.children.map(_.toList)
        jarChildren = children.toList.filter { f => f.fileName.ext == "jar".some }
        results <- ZIO.foreach(jarChildren)(inFile)
      } yield results.flatten

  }

  def forType[T: ClassTag]: Builder1[T] =
    Builder1(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])

}
