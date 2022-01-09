package klib.utils

val AnsiEscapeString: String = "\u001b["

extension (event: => Logger.Event) {

  def requireFlags(flags: String*): Logger.Event =
    Logger.Event.RequireFlags(flags.toSet, () => event)

}
