package za.jwatson.glycanoweb.structure

import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses
import Residue._

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
    case ResidueType.Glc if subs == subs6DeoxySugar => s"${ano.desc}-${abs.desc}-Qui"
    case ResidueType.Gal if subs == subs6DeoxySugar => s"${ano.desc}-${abs.desc}-Fuc"
    case ResidueType.Man if subs == subs6DeoxySugar => s"${ano.desc}-${abs.desc}-Rha"
    case ResidueType.ManOct if subs == subsKdo => s"${ano.desc}-${abs.desc}-Kdo"
    case ResidueType.Non if subs == subsSialicAcid => s"${ano.desc}-${abs.desc}-SialicAcid"
    case _ => s"${ano.desc}-${abs.desc}-${rt.desc}$substSymbol"
  }
  
  override def toString: String = desc
}

object Residue {
  implicit val reusability: Reusability[Residue] = Reusability.by_==

  val subs6DeoxySugar = Map(6 -> Vector(SubstituentType.deoxy))
  val subsKdo = Map(1 -> Vector(SubstituentType.cooh), 3 -> Vector(SubstituentType.deoxy))
  val subsSialicAcid = Map(1 -> Vector(SubstituentType.cooh), 5 -> Vector(SubstituentType.n, SubstituentType.ac))
}

case class ResidueId(id: Int) extends AnyVal

object ResidueId {
  var id: Int = 0
  def nextId(): Int = { id += 1; id }
  def next(): ResidueId = ResidueId(nextId())

  implicit val reusability: Reusability[ResidueId] = Reusability.by_==
}

@Lenses case class Link(r: ResidueId, position: Int)

object Link {
  implicit val reusability: Reusability[Link] = Reusability.by_==
}