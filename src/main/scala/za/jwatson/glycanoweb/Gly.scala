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
    case Js.Str(SubstituentType(st)) => st
  })
  implicit val rwResidue = ReadWriter[Residue](r => Js.Arr(
    Js.Str(r.anomer.symbol),
    Js.Str(r.absolute.symbol),
    Js.Str(r.rt.symbol)
  ), {
    case Js.Arr(
      Js.Str(Anomer(ano)),
      Js.Str(Absolute(abs)),
      Js.Str(ResidueType(rt))
    ) => Residue.next(rt, ano, abs)
  })
  //implicit val rwGly = ReadWriter()
}
