package za.jwatson.glycanoweb.structure

case class Residue(id: Int, rt: ResidueType, anomer: Anomer, absolute: Absolute) {
  def symbol: String = rt match {
    case ResidueType.Begin => anomer.symbol + rt.symbol
    case ResidueType.End => rt.symbol
    case _ => anomer.symbol + absolute.symbol + rt.symbol
  }
  def desc: String = rt match {
    case ResidueType.Begin => s"${anomer.desc}-${rt.desc}"
    case ResidueType.End => rt.desc
    case _ => s"${anomer.desc}-${absolute.desc}-${rt.desc}"
  }
  
  override def toString: String = desc
}

object Residue {
  var nextId: Int = 0
  def next(rt: ResidueType,
            anomer: Anomer,
            absolute: Absolute): Residue = {
    nextId += 1
    Residue(nextId, rt, anomer, absolute)
  }

}

case class Link(residue: Residue, position: Int)











