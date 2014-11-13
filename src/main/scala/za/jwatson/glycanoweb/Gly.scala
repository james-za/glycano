package za.jwatson.glycanoweb

import upickle._
import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Beta, Alpha}
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.structure.RGraph._

import scalajs.js

case class Gly(residues: Map[Residue, GlyRes], annotations: Seq[GlyAnnot])
case class GlyRes(x: Double, y: Double, rot: Double, targetRes: Int, targetPos: Int, subs: Map[Int, Seq[SubstituentType]])
case class GlyAnnot(id: Int, x: Double, y: Double, rot: Double, text: String, size: Double)

object GlyAnnot {
  var nextId: Int = 0
  def next(x: Double, y: Double, rot: Double, text: String, size: Double): GlyAnnot = {
    nextId += 1
    GlyAnnot(nextId, x, y, rot, text, size)
  }
}

object Gly {
  def from(glycanoCanvas: GlycanoCanvas): Gly = {
    implicit val graph = glycanoCanvas.graph()
    val rs = glycanoCanvas.graph().residues.toList
    val rp = rs.zipWithIndex.toMap
    val residues = for {
      r <- rs
      x <- r.x
      y <- r.y
      rot <- r.rotation
      tr = r.parent.fold(-1)(l => rp(l.residue))
      tp = r.parent.fold(-1)(_.position)
      subs = r.substituents.mapValues(_.map(_.st))
    } yield r -> GlyRes(x, y, rot, tr, tp, subs)

    val annotations = glycanoCanvas.annotations().values.toList

    Gly(residues.toMap, annotations)
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
  implicit val rwGly = ReadWriter[Gly](gly => Js.Arr(
    Js.Arr((for ((r, gr) <- gly.residues.toSeq) yield Js.Arr(
      writeJs(r), Js.Num(gr.x), Js.Num(gr.y), Js.Num(gr.rot), Js.Num(gr.targetRes), Js.Num(gr.targetPos), Js.Obj(
        (for ((i, sts) <- gr.subs.toSeq) yield i.toString -> Js.Arr(sts.map(writeJs[SubstituentType]): _*)): _*
      )
    )): _*),
    Js.Arr((for (GlyAnnot(_, x, y, rot, text, size) <- gly.annotations) yield
      Js.Arr(Js.Num(x), Js.Num(y), Js.Num(rot), Js.Str(text), Js.Num(size))
    ): _*)
  ), {
    case Js.Arr(Js.Arr(rs @ _*), Js.Arr(as @ _*)) =>
      val residues = rs.collect {
        case Js.Arr(r, Js.Num(x), Js.Num(y), Js.Num(rot), Js.Num(tr), Js.Num(tp), Js.Obj(subs @ _*)) =>
          readJs[Residue](r) -> GlyRes(x, y, rot, tr.toInt, tp.toInt, subs.collect {
              case (i, Js.Arr(sts @ _*)) => i.toInt -> sts.map(readJs[SubstituentType])
          }.toMap): (Residue, GlyRes)
      }.toMap
      val annotations = as.collect {
        case Js.Arr(Js.Num(x), Js.Num(y), Js.Num(rot), Js.Str(text), Js.Num(size)) => GlyAnnot.next(x, y, rot, text, size)
      }
      Gly(residues, annotations)
  })
}
