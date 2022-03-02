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

    def use[R, A](useF: ClassLoader => RIOM[R, A]): RIOM[R, A] =
      ZManaged
        .fromAutoCloseable {
          for {
            url <- ZIO.attemptM(file.toPath.toUri.toURL)
            classLoader <- ZIO.attemptM(URLClassLoader(Array(url), getClass.getClassLoader))
          } yield classLoader
        }
        .use(useF)

  }

  final class JarClass[T](jar: Jar, classPath: String, klass: Class[T]) {

    def use[R, A](useF: T => RIOM[R, A]): RIOM[R, A] =
      jar.use { classLoader =>
        for {
          c <- ZIO.attemptM(classLoader.loadClass(classPath))
          constructors <- ZIO.attemptM(c.getDeclaredConstructors)
          zeroArgConstructor = constructors.find(_.getParameterCount == 0)
          constructor <- ZIO.fromOption(zeroArgConstructor).orElseFail(Message.same("No zero arg constructor found"))
          _ <- ZIO.attemptM(constructor.setAccessible(true))
          inst <- ZIO.attemptM(constructor.newInstance())
          isInstance <- ZIO.attemptM(klass.isInstance(inst))
          casted <- ZIO.cond(isInstance, inst.asInstanceOf[T], Message.same(s"'$classPath' is not an instance of $klass"))
          res <- useF(casted)
        } yield res
      }

  }

  final class Builder1[T] private[DynamicJarLoader] (klass: Class[T]) {

    def inFile(file: File): TaskM[List[JarClass[T]]] = {
      def getJarEntries(jarInputStream: JarInputStream, stack: List[JarEntry]): TaskM[List[JarEntry]] =
        ZIO.attemptM(Option(jarInputStream.getNextJarEntry)).flatMap {
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
          c <- ZIO.attemptM(classLoader.loadClass(classPath)).asSomeError
          _ <- ZIO.whenZIO(ZIO.attemptM(klass.isAssignableFrom(c) && klass.toString != c.toString))(ZIO.unit).some
          constructors <- ZIO.attemptM(c.getDeclaredConstructors).asSomeError
          zeroArgConstructor <-
            ZIO
              .fromOption(constructors.find(_.getParameterCount == 0))
              .orElseFail(Message.same(s"No zero arg constructor for '$classPath'").some)
          setAccessible <- ZIO.attemptM(zeroArgConstructor.trySetAccessible()).asSomeError
          _ <- ZIO.cond(setAccessible, (), Message.same("Zero arg constructor is not accessible").some)
          inst <- ZIO.attemptM(zeroArgConstructor.newInstance()).asSomeError
          isIsntance <- ZIO.attemptM(klass.isInstance(inst)).asSomeError
          _ <- ZIO.cond(isIsntance, (), Message.unexpected("Class is not actually assignable...?").some)
        } yield JarClass(jar, classPath, klass)).optional

      for {
        jarBytes <- file.readBytes
        jarInputStream = ZManaged.fromAutoCloseable(ZIO.attemptM(JarInputStream(ByteArrayInputStream(jarBytes), true)))
        jarEntries <- jarInputStream.use(getJarEntries(_, Nil))
        classPaths = getClassPaths(jarEntries)
        jar = Jar(file)
        jarClasses <- jar.use { classLoader => ZIO.foreach(classPaths)(getJarClass(jar, classLoader, _)) }
      } yield jarClasses.flatten
    }

    def inDir(file: File): RIOM[Logger, List[JarClass[T]]] =
      for {
        children <- file.children.map(_.toList)
        jarChildren = children.toList.filter { f => f.fileName.ext == "jar".some }
        results <- ZIO.foreach(jarChildren)(inFile)
      } yield results.flatten

  }

  def forType[T: ClassTag]: Builder1[T] =
    Builder1(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])

}
