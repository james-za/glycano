package za.jwatson.glycanoweb.structure

/**
 * Created by James on 2014/06/21.
 */
sealed trait Anomer {
  val symbol: String
  val desc: String
}

object Anomer {
  case object Alpha extends Anomer { val symbol = "a"; val desc = "\u03B1" }
  case object Beta extends Anomer { val symbol = "b"; val desc = "\u03B2" }
}