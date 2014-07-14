package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.structure.{RGraph, Graph, Residue}
import za.jwatson.glycanoweb.structure.Residue.Link

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz._
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
    val tree = residueTree(root)
    val strTree = tree.map({ r =>
      val mod = for {
        Link(t, p) <- r.parent
        children <- r.children
      } yield {
        val left = children.exists(_._1 < p) ? "]" | ""
        val right = children.exists(_._1 > p) ? "[" | ""
        s"(1->$p)$left$right"
      }
      r.symbol + (mod | "")
    })
    strTree.flatten.reverseIterator.mkString
  }

  def residueTree(root: Residue)(implicit graph: RGraph): Tree[Residue] = {
    val leaves = root.children.toSeq.flatten.sortBy(-_._1).map(c => residueTree(c._2))
    root.node(leaves: _*)
  }
}
