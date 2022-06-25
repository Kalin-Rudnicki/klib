package klib.web.endpoint

final class EndpointType[PathT, ParamMapT, InputB <: Body, OutputB <: Body]
object EndpointType {

  trait Aggregate[ETF[_ <: EndpointType[_, _, _, _]]]
  object Aggregate {

    trait IsAggregate[T[_[_ <: EndpointType[_, _, _, _]]] <: Aggregate[_]] {
      def prependPath(path: String, agg: T[SharedTypes.QueryCodecT]): T[SharedTypes.QueryCodecT]
      def prependPaths(paths: List[String], agg: T[SharedTypes.QueryCodecT]): T[SharedTypes.QueryCodecT]
    }

  }

}

implicit class AggregateOps[T[_[_ <: EndpointType[_, _, _, _]]] <: EndpointType.Aggregate[_]](
    agg: T[SharedTypes.QueryCodecT],
)(implicit
    isAggregate: EndpointType.Aggregate.IsAggregate[T],
) {

  def /:(path: String): T[SharedTypes.QueryCodecT] =
    isAggregate.prependPath(path, agg)

  def /:(paths: List[String]): T[SharedTypes.QueryCodecT] =
    isAggregate.prependPaths(paths, agg)

}
