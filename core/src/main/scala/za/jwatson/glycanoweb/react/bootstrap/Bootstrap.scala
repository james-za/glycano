package za.jwatson.glycanoweb.react.bootstrap

import japgolly.scalajs.react.extra.Reusability

object Bootstrap {
  case class Style(name: String) { override def toString = name }
  object Default extends Style("default")
  object Primary extends Style("primary")
  object Success extends Style("success")
  object Info extends Style("info")
  object Warning extends Style("warning")
  object Danger extends Style("danger")
  object LinkStyle extends Style("link")
  object Style {
    implicit val reusability: Reusability[Style] = Reusability.by_==
  }

  case class Size(name: String) { override def toString = name }
  object Lg extends Size("lg")
  object Md extends Size("md")
  object Sm extends Size("sm")
  object Xs extends Size("xs")
  object Size {
    implicit val reusability: Reusability[Size] = Reusability.by_==
  }
}
