package za.jwatson.glycanoweb

import upickle._
import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Beta, Alpha}
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.structure.RGraph._

case class Gly(residues: Map[Residue, GlyRes])
case class GlyRes(x: Double, y: Double, targetRes: Int, targetPos: Int, subs: Map[Int, Seq[SubstituentType]])

object Gly {
  def from(glycanoCanvas: GlycanoCanvas): Gly = {
    import glycanoCanvas.RichResidue

    implicit val graph = glycanoCanvas.graph()
    val rs = glycanoCanvas.graph().residues.toList
    val rp = rs.zipWithIndex.toMap
    val residues = for {
      r <- rs
      item <- r.getItem
      p = item.position
      tr = r.parent.fold(-1)(l => rp(l.residue))
      tp = r.parent.fold(-1)(_.position)
      subs = r.substituents.mapValues(_.map(_.st))
    } yield r -> GlyRes(p.x, p.y, tr, tp, subs)

    Gly(residues.toMap)
  }

  implicit val rwSubstituentType = ReadWriter[SubstituentType](st => Js.Str(st.symbol), {
    case Js.Str(sym) => SubstituentType.SubstituentsMap(sym)
  })
  implicit val rwResidueType = ReadWriter[ResidueType](rt => Js.Obj(
    "symbol" -> Js.Str(rt.symbol),
    "name" -> Js.Str(rt.desc),
    "linkage" -> Js.Num(rt.linkage),
    "category" -> Js.Str(rt.category.name)
  ), {
    case Js.Obj(
      ("symbol", Js.Str(sym)),
      ("name", Js.Str(name)),
      ("linkage", Js.Num(link)),
      ("category", Js.Str(cat))) =>
      ResidueType(sym, name, link.toInt, ResidueCategory.ResidueCategoryMap(cat))
  })
  implicit val rwAnomer = ReadWriter[Anomer](ano => Js.Str(ano.symbol), {
    case Js.Str(Alpha.symbol) => Alpha
    case Js.Str(Beta.symbol) => Beta
  })
  implicit val rwAbsolute = ReadWriter[Absolute](abs => Js.Str(abs.symbol), {
    case Js.Str(D.symbol) => D
    case Js.Str(L.symbol) => L
  })
  implicit val rwResidue = ReadWriter[Residue](r => Js.Obj(
    "type" -> Js.Str(r.rt.symbol),
    "anomer" -> writeJs(r.anomer),
    "absolute" -> writeJs(r.absolute)
  ), {
    case Js.Obj(("type", Js.Str(rt)), ("anomer", ano), ("absolute", abs)) =>
      Residue.next(ResidueType.ResidueTypeMap(rt), readJs[Anomer](ano), readJs[Absolute](abs))
  })
  //implicit val rwGly = ReadWriter()
}
