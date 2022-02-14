package klib.utils.commandLine.parse

final case class HelpConfig(
    helpExtra: Boolean,
    leftPadding: Int,
    maxParamsWidth: Int,
    centerPadding: Int,
)
object HelpConfig {

  def default(helpExtra: Boolean): HelpConfig =
    HelpConfig(
      helpExtra = helpExtra,
      leftPadding = 2,
      maxParamsWidth = 50,
      centerPadding = 4,
    )

}
