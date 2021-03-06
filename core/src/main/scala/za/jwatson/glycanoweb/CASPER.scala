package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.structure._

import scala.annotation.tailrec
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
import RGraph._

object CASPER {
  def findRoot(r: ResidueId)(implicit graph: RGraph): Option[ResidueId] = findRootRec(r, Set.empty)
  @tailrec def findRootRec(r: ResidueId, rs: Set[ResidueId])(implicit graph: RGraph): Option[ResidueId] = {
    r.parent match {
      case None =>
        r.some
      case Some(Link(parent, i)) =>
        if(i == 1) none else findRootRec(parent, rs + r)
    }
  }

  @tailrec def findRoots(rs: Set[ResidueId], visited: Set[ResidueId] = Set.empty)(implicit graph: RGraph): Set[ResidueId] = {
    val next = rs.flatMap(r => r.parent.fold(r.some){
      case Link(p, 1) => none
      case Link(p, _) if rs.contains(p) || visited.contains(p) => none
      case Link(p, _) => p.some
    })
    if (rs == next) rs else findRoots(next, visited ++ rs)
  }

  def getStrings(residues: Set[ResidueId])(implicit graph: RGraph): Map[ResidueId, String] = {
    val roots = findRoots(residues).toSeq
    val strings = roots map (getString(graph, _))
    Map(roots zip strings: _*)
  }

  def getString(graph: RGraph, root: ResidueId): String = {
    implicit val rg = graph
    val strs = rec(root)
    strs.mkString
  }

  def containsBeginRepeat(r: ResidueId)(implicit graph: RGraph): Boolean = {
    if (r.rt.contains(ResidueType.Begin)) {
      true
    } else {
      r.children.getOrElse(Map.empty[Int, ResidueId]).values.exists(containsBeginRepeat)
    }
  }

  def resCompare(a: (Int, ResidueId), b: (Int, ResidueId))(implicit g: RGraph): Boolean = {
    (containsBeginRepeat(a._2), containsBeginRepeat(b._2)) match {
      case (true, false) => true
      case (false, true) => false
      case _ => a._1 < b._1
    }
  }

  def arrowString(b: Bond)(implicit g: RGraph): String = {
    val first = if (b.from.rt.contains(ResidueType.Begin)) "" else "(1"
    val last = if (b.to.r.rt.contains(ResidueType.End)) "" else b.to.position + ")"
    first + "->" + last
  }

  def rec(r: ResidueId, tail: Boolean = false)(implicit g: RGraph): Vector[String] = {
//    val substs = for {
//      (i, stack) <- r.substituents
//      st <- stack
//    } yield i + st.symbol
    val str = r.residue.map(_.symbol).getOrElse("") + r.bond.map(arrowString).getOrElse("")
    val children = for {
      children <- r.children.to[Vector]
      (_, c) <- children.toSeq.sortWith((a, b) => resCompare(a, b))
    } yield c
    val first = children.headOption.to[Vector].flatMap(rec(_))
    val rest = if (children.isEmpty) Vector.empty else children.tail.flatMap(rec(_, tail = true))

    (tail ? "[" | "") +: (first ++ rest) :+ str :+ (tail ? "]" | "")
  }
}
