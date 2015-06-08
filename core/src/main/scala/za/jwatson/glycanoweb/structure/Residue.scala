package za.jwatson.glycanoweb.structure

import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses

@Lenses case class Residue(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]] = Map.empty) {

  def substSymbol = substSymbolParts.mkString
  def substSymbolParts = for {
    (i, sts) <- subs
    stsString = sts.map(_.symbol).mkString
  } yield s"$i$stsString"
  
  def symbol: String = rt.category match {
    case ResidueCategory.Repeat => rt.symbol
    case _ => ano.symbol + abs.symbol + rt.symbol + substSymbol
  }
  
  def desc: String = rt match {
    case ResidueType.Begin => s"${ano.desc}-${rt.desc}"
    case ResidueType.End => rt.desc
    case _ => s"${ano.desc}-${abs.desc}-${rt.desc}"
  }
  
  override def toString: String = desc
}

object Residue {
  implicit val reusability: Reusability[Residue] = Reusability.by_==
}

case class ResidueId(id: Int) extends AnyVal

object ResidueId {
  var id: Int = 0
  def nextId(): Int = { id += 1; id }
  def next(): ResidueId = ResidueId(nextId())
}

@Lenses case class Link(r: ResidueId, position: Int)











