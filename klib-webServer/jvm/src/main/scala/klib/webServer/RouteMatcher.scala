package klib.webServer

import io.circe._, generic.auto._, parser._

import klib.Implicits._
import klib.fp.types._

sealed trait RouteMatcher {

  def /:(const: String): RouteMatcher =
    RouteMatcher.Const(const)(this)

}

object RouteMatcher {

  final class Const private (private val const: String, private val child: RouteMatcher) extends RouteMatcher
  object Const {

    def apply(const: String)(child: RouteMatcher): RouteMatcher =
      new Const(const, child)

    def unapply(arg: Const): Option[(String, RouteMatcher)] =
      (arg.const, arg.child).someOpt

  }

  final class OneOf private (private val children: List[RouteMatcher]) extends RouteMatcher
  object OneOf {

    def apply(children: RouteMatcher*): RouteMatcher =
      new OneOf(children.toList)

    def unapply(arg: OneOf): Option[List[RouteMatcher]] =
      arg.children.someOpt

  }

  // TODO (KR) : (method: Method)
  final class Complete private (private val method: String, private val toResult: ??[MatchResult]) extends RouteMatcher
  object Complete {

    def apply(method: String)(toResult: ??[MatchResult]): RouteMatcher =
      new Complete(method, toResult)

    def unapply(arg: Complete): Option[(String, ??[MatchResult])] =
      (arg.method, arg.toResult).someOpt

  }

  final class WithBody[B] private (private val decoder: Decoder[B], private val child: B => RouteMatcher)
      extends RouteMatcher
  object WithBody {

    def apply[B: Decoder](child: B => RouteMatcher): RouteMatcher =
      new WithBody(implicitly[Decoder[B]], child)

    def unapply[B](arg: WithBody[B]): Option[(Decoder[B], B => RouteMatcher)] =
      (arg.decoder, arg.child).someOpt

  }

}
