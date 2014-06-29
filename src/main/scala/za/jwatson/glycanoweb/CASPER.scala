package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.structure.{Graph, Residue}
import za.jwatson.glycanoweb.structure.Residue.Link

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz._

object CASPER {
  def findRoot(graph: Graph, r: Residue): Option[Residue] = findRootRec(graph, r, Set.empty)
  @tailrec def findRootRec(graph: Graph, r: Residue, rs: Set[Residue]): Option[Residue] = {
    graph.parent(r) match {
      case None =>
        r.some
      case Some(Link(parent, i)) =>
        if(i == 1) none else findRootRec(graph, parent, rs + r)
    }
  }

  def getStrings(residues: Set[Residue])(implicit graph: Graph): Map[Residue, String] = {
    val roots = residues.flatMap(findRoot(graph, _)).toSeq
    val strings = roots map (getString(graph, _))
    Map(roots zip strings: _*)
  }

  def getString(graph: Graph, root: Residue): String = {
    val tree = residueTree(graph, root)
    val strTree = tree.map({ r =>
      val mod = for (Link(t, p) <- graph.parent(r)) yield {
        val left = graph.children(t).exists(_._1 < p) ? "]" | ""
        val right = graph.children(t).exists(_._1 > p) ? "[" | ""
        s"(1->$p)$left$right"
      }
      r.symbol + (mod | "")
    })
    strTree.flatten.reverseIterator.mkString
  }

  def residueTree(graph: Graph, root: Residue): Tree[Residue] = {
    val leaves = graph.children(root).toSeq.sortBy(-_._1).map(c => residueTree(graph, c._2))
    root.node(leaves: _*)
  }
}
