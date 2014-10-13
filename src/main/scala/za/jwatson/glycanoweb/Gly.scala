package za.jwatson.glycanoweb

import upickle._
import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Beta, Alpha}
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.structure.RGraph._

import scalajs.js

case class Gly(residues: Map[Residue, GlyRes])
case class GlyRes(x: Double, y: Double, rot: Double, targetRes: Int, targetPos: Int, subs: Map[Int, Seq[SubstituentType]])

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
  implicit val rwGly = ReadWriter[Gly](gly => Js.Arr((for ((r, gr) <- gly.residues.toSeq) yield {
    Js.Arr(writeJs(r), writeJs(gr.x), writeJs(gr.y), writeJs(gr.rot), writeJs(gr.targetRes), writeJs(gr.targetPos), Js.Obj(
      (for ((i, sts) <- gr.subs.toSeq) yield i.toString -> Js.Arr(sts.map(writeJs[SubstituentType]): _*)): _*
    ))
  }): _*), {
    case Js.Arr(rs @ _*) => Gly(rs.collect {
      case Js.Arr(r, x, y, rot, tr, tp, Js.Obj(subs @ _*)) => readJs[Residue](r) -> GlyRes(
        readJs[Double](x), readJs[Double](y), readJs[Double](rot), readJs[Int](tr), readJs[Int](tp), subs.collect {
          case (i, Js.Arr(sts @ _*)) => i.toInt -> sts.map(readJs[SubstituentType])
        }.toMap
      )
    }.toMap)
  })
}
