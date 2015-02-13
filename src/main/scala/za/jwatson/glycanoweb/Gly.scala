package za.jwatson.glycanoweb

import upickle._
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Beta, Alpha}
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.structure.RGraph._

case class Gly(residues: Seq[GlyRes], annotations: Seq[GlyAnnot])
case class GlyRes(ano: Anomer, abs: Absolute, rt: ResidueType,
                  x: Double, y: Double, rot: Double,
                  targetRes: Int, targetPos: Int,
                  subs: Map[Int, Seq[SubstituentType]])
case class GlyAnnot(id: Int, x: Double, y: Double, rot: Double, text: String, size: Double)

object GlyAnnot {
  var nextId: Int = 0
  def next(x: Double, y: Double, rot: Double, text: String, size: Double): GlyAnnot = {
    nextId += 1
    GlyAnnot(nextId, x, y, rot, text, size)
  }
}

object Gly {
  def from(g: RGraph): Gly = {
    implicit val graph = g
    val entries = g.residues.values.toList
    val lookup = g.residues.keys.zipWithIndex.toMap
    val residues = for {
      GraphEntry(Residue(ano, abs, rt, subs), x, y, rot, _, parent) <- entries
      tr = parent.fold(-1)(l => lookup(l.r))
      tp = parent.fold(-1)(_.position)
    } yield GlyRes(ano, abs, rt, x, y, rot, tr, tp, subs)

    val annotations = for {
      (a, Annot(text, size, x, y, rot)) <- g.annotations.toList
    } yield GlyAnnot(a.id, x, y, rot, text, size)

    Gly(residues, annotations)
  }

  implicit val rwSubstituentType: ReadWriter[SubstituentType] = ReadWriter[SubstituentType](st => Js.Str(st.symbol), {
    case Js.Str(SubstituentType(st)) => st
  })
  implicit val rwGly: ReadWriter[Gly] = ReadWriter[Gly](gly => Js.Arr(
    Js.Arr((for (gr <- gly.residues) yield Js.Arr(
      Js.Str(gr.ano.symbol), Js.Str(gr.abs.symbol), Js.Str(gr.rt.symbol),
      Js.Num(gr.x), Js.Num(gr.y), Js.Num(gr.rot),
      Js.Num(gr.targetRes), Js.Num(gr.targetPos), Js.Obj(
        (for ((i, sts) <- gr.subs.toSeq) yield i.toString -> Js.Arr(sts.map(writeJs[SubstituentType]): _*)): _*
      )
    )): _*),
    Js.Arr((for (GlyAnnot(_, x, y, rot, text, size) <- gly.annotations) yield
      Js.Arr(Js.Num(x), Js.Num(y), Js.Num(rot), Js.Str(text), Js.Num(size))
    ): _*)
  ), {
    case Js.Arr(Js.Arr(rs @ _*), Js.Arr(as @ _*)) =>
      val residues = rs.collect {
        case Js.Arr(
          Js.Str(Anomer(ano)), Js.Str(Absolute(abs)), Js.Str(ResidueType(rt)),
          Js.Num(x), Js.Num(y), Js.Num(rot), Js.Num(tr), Js.Num(tp), Js.Obj(subs @ _*)) =>
          GlyRes(ano, abs, rt, x, y, rot, tr.toInt, tp.toInt, subs.collect {
              case (i, Js.Arr(sts @ _*)) => i.toInt -> sts.map(readJs[SubstituentType])
          }.toMap)
      }
      val annotations = as.collect {
        case Js.Arr(Js.Num(x), Js.Num(y), Js.Num(rot), Js.Str(text), Js.Num(size)) =>
          GlyAnnot(-1, x, y, rot, text, size)
      }
      Gly(residues, annotations)
  })
}
