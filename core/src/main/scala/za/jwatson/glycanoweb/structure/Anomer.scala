package za.jwatson.glycanoweb.structure

import japgolly.scalajs.react.extra.Reusability

sealed trait Anomer {
  val symbol: String
  val desc: String
}

object Anomer {
  case object Alpha extends Anomer { val symbol = "a"; val desc = "\u03B1" }
  case object Beta extends Anomer { val symbol = "b"; val desc = "\u03B2" }

  val Anomers: Seq[Anomer] = Seq(Alpha, Beta)
  val AnomerMap = Anomers.map(ano => ano.symbol -> ano).toMap

  def unapply(str: String): Option[Anomer] = AnomerMap.get(str)

  implicit val reusability: Reusability[Anomer] = Reusability.by_==
}