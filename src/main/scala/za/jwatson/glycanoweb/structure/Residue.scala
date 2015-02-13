package za.jwatson.glycanoweb.structure

import monocle.macros.Lenses

@Lenses case class Residue(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]] = Map.empty) {
  def substSymbol = (for {
    (i, sts) <- subs
    st <- sts
  } yield s"$i${st.name}").mkString
  
  def symbol: String = rt match {
    case ResidueType.Begin => ano.symbol + rt.symbol
    case ResidueType.End => rt.symbol
    case _ => ano.symbol + abs.symbol + rt.symbol + substSymbol
  }
  
  def desc: String = rt match {
    case ResidueType.Begin => s"${ano.desc}-${rt.desc}"
    case ResidueType.End => rt.desc
    case _ => s"${ano.desc}-${abs.desc}-${rt.desc}"
  }
  
  override def toString: String = desc
}

case class ResidueId(id: Int) extends AnyVal

object ResidueId {
  var id: Int = 0
  def nextId(): Int = { id += 1; id }
  def next(): ResidueId = ResidueId(nextId())
}

@Lenses case class Link(r: ResidueId, position: Int)











