package za.jwatson.glycanoweb.structure

import scala.collection.mutable.ListBuffer

case class Residue private[Residue](
  id: Int,
  rt: ResidueType,
  anomer: Anomer,
  absolute: Absolute
) {
  val substituents: Map[Int, ListBuffer[Substituent]] =
    (1 to rt.linkage).map(_ -> ListBuffer[Substituent]())(collection.breakOut)

  def symbol: String = anomer.symbol + absolute.symbol + rt.symbol
  def desc: String = s"${anomer.desc}-${absolute.desc}-${rt.desc}"
  
  override def toString: String = desc
}

object Residue {
  var nextId: Int = 0
  def apply(rt: ResidueType,
            anomer: Anomer,
            absolute: Absolute) = {
    nextId += 1
    new Residue(nextId, rt, anomer, absolute)
  }

  case class Link(residue: Residue, position: Int)
}













