package klib.web.endpoint

final case class ParamMaps(
    headers: ParamMap,
    queryParams: ParamMap,
    cookies: ParamMap,
)
object ParamMaps {

  val Empty: ParamMaps =
    ParamMaps(
      ParamMap.empty("header"),
      ParamMap.empty("query-param"),
      ParamMap.empty("cookie"),
    )

}
