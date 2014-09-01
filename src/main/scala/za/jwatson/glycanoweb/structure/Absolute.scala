package za.jwatson.glycanoweb.structure

sealed trait Absolute {
  val symbol: String
  val desc: String
}

object Absolute {
  case object D extends Absolute { val symbol = "D"; val desc = "\u1D05" }
  case object L extends Absolute { val symbol = "L"; val desc = "\u029F" }

  val Absolutes = Seq(D, L)
  val AbsoluteMap = Absolutes.map(abs => abs.symbol -> abs).toMap

  def unapply(str: String): Option[Absolute] = AbsoluteMap.get(str)
}