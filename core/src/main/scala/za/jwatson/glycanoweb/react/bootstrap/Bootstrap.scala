package za.jwatson.glycanoweb.react.bootstrap

object Bootstrap {
  case class Style(name: String)
  object Default extends Style("default")
  object Primary extends Style("primary")
  object Success extends Style("success")
  object Info extends Style("info")
  object Warning extends Style("warning")
  object Danger extends Style("danger")
  object LinkStyle extends Style("link")

  case class Size(name: String)
  object Lg extends Size("lg")
  object Md extends Size("md")
  object Sm extends Size("sm")
  object Xs extends Size("xs")
}
