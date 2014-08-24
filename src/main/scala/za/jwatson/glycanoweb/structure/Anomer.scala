package za.jwatson.glycanoweb.structure

sealed trait Anomer {
  val symbol: String
  val desc: String
}
object Anomer {
  case object Alpha extends Anomer {
    override val symbol = "a"
    override val desc = "\u03B1"
  }
  case object Beta extends Anomer {
    override val symbol = "b"
    override val desc = "\u03B2"
  }
}

object Anomers {
  import upickle._
  sealed trait Anomer1
  case object Alpha1 extends Anomer1
  case object Beta1 extends Anomer1
  write(Alpha1)
}