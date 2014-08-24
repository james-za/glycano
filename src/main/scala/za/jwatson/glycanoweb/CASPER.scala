package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.structure.{Residue, RGraph}
import za.jwatson.glycanoweb.structure.Link

import scala.annotation.tailrec
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
import RGraph._

object CASPER {
  def findRoot(r: Residue)(implicit graph: RGraph): Option[Residue] = findRootRec(r, Set.empty)
  @tailrec def findRootRec(r: Residue, rs: Set[Residue])(implicit graph: RGraph): Option[Residue] = {
    r.parent match {
      case None =>
        r.some
      case Some(Link(parent, i)) =>
        if(i == 1) none else findRootRec(parent, rs + r)
    }
  }

  def getStrings(residues: Set[Residue])(implicit graph: RGraph): Map[Residue, String] = {
    val roots = residues.flatMap(findRoot(_)).toSeq
    val strings = roots map (getString(graph, _))
    Map(roots zip strings: _*)
  }

  def getString(graph: RGraph, root: Residue): String = {
    implicit val rg = graph
    val strs = rec(root)
    strs.mkString
  }

  def rec(r: Residue, tail: Boolean = false)(implicit g: RGraph): Vector[String] = {
    val substs = for {
      (i, stack) <- r.substituents
      subst <- stack
    } yield i + subst.st.symbol
    val link = r.bond.map("(1->" + _.to.position + ")").getOrElse("")
    val str = r.symbol + substs.mkString + link
    val children = for {
      children <- r.children.to[Vector]
      (_, c) <- children.toSeq.sortBy(_._1)
    } yield c
    val first = children.headOption.to[Vector].flatMap(rec(_))
    val rest = if (children.isEmpty) Vector.empty else children.tail.flatMap(rec(_, tail = true))

    (tail ? "[" | "") +: (first ++ rest) :+ str :+ (tail ? "]" | "")
  }
}
