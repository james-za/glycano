package za.jwatson.glycanoweb

import upickle._
import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

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

//  implicit val rSubsMap = upickle.MapR[Int, Seq[SubstituentType]]
//  implicit val wSubsMap = upickle.MapW[Int, Seq[SubstituentType]]

  implicit val rwGlyRes = ReadWriter[GlyRes](gr => Js.Arr(
    Js.Num(gr.x),
    Js.Num(gr.y),
    Js.Num(gr.targetRes),
    Js.Num(gr.targetPos),
    writeJs(gr.subs)
  ), {
    case Js.Arr(Js.Num(x), Js.Num(y), Js.Num(tr), Js.Num(tp), subs) =>
      GlyRes(x, y, tr.toInt, tp.toInt, readJs[Map[Int, Seq[SubstituentType]]](subs))
  })

//  implicit val rGlyMap = upickle.MapR[Residue, GlyRes]
//  implicit val wGlyMap = upickle.MapW[Residue, GlyRes]

  implicit val rwGly = ReadWriter[Gly](gly => writeJs(gly.residues), {
    case js => Gly(readJs[Map[Residue, GlyRes]](js))
  })
}
