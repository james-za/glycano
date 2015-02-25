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

  val Anomers: Seq[Anomer] = Seq(Alpha, Beta)
  val AnomerMap = Anomers.map(ano => ano.symbol -> ano).toMap

  def unapply(str: String): Option[Anomer] = AnomerMap.get(str)
}