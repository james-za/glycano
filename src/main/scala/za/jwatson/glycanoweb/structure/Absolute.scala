package za.jwatson.glycanoweb.structure

/**
 * Created by James on 2014/06/21.
 */
sealed trait Absolute {
  val symbol: String
  val desc: String
}

object Absolute {
  case object D extends Absolute { val symbol = "D"; val desc = "\u1D05" }
  case object L extends Absolute { val symbol = "L"; val desc = "\u029F" }
}