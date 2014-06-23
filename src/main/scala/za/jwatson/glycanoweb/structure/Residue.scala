package za.jwatson.glycanoweb.structure

import scala.collection.mutable.ListBuffer

class Residue private[Residue](
  val id: Int,
  val rt: ResidueType,
  val anomeric: Anomer,
  val absolute: Absolute
) {
  val bonds = collection.mutable.Map[Int, Bond]()

  val substituents: Map[Int, ListBuffer[Substituent]] =
    (1 to rt.linkage).map(_ -> ListBuffer[Substituent]())(collection.breakOut)

  def symbol: String = anomeric.symbol + absolute.symbol + rt.symbol
  def desc: String = s"${anomeric.desc}-${absolute.desc}-${rt.desc}"

  def bond(other: Residue, to: Int, from: Int = 1): Bond = {
    val b = Bond(Link(this, from), Link(other, to))
    bonds += from -> b
    other.bonds += to -> b

    b
  }

  def removeBond(position: Int): Option[Bond] = {
    for(b <- bonds.get(position)) yield {
      b.from.residue.bonds -= b.from.position
      b.to.residue.bonds -= b.to.position
      b
    }
  }

  def targets: Seq[Link]

  def children: Seq[Residue] =

  override def hashCode(): Int = id
  override def equals(other: scala.Any): Boolean = other match {
    case r: Residue => r.id == this.id
    case _ => false
  }
}

object Residue {
  var nextId: Int = 0
  def apply(rt: ResidueType,
            anomeric: Anomer,
            absolute: Absolute) = {
    nextId += 1
    new Residue(nextId, rt, anomeric, absolute)
  }
}













