package za.jwatson.glycanoweb.structure

import japgolly.scalajs.react.extra.Reusability

sealed trait Absolute {
  val symbol: String
  val desc: String
}

object Absolute {
  case object D extends Absolute { val symbol = "D"; val desc = "\u1D05" }
  case object L extends Absolute { val symbol = "L"; val desc = "\u029F" }

  val Absolutes: Seq[Absolute] = Seq(D, L)
  val AbsoluteMap = Absolutes.map(abs => abs.symbol -> abs).toMap

  def unapply(str: String): Option[Absolute] = AbsoluteMap.get(str)

  implicit val reusability: Reusability[Absolute] = Reusability.by_==
}