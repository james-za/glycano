package za.jwatson.glycanoweb.structure

import za.jwatson.glycanoweb.structure.Graph.GraphEntry
import za.jwatson.glycanoweb.structure.Residue.Link

import scala.collection.GenTraversableOnce
import scala.collection.{mutable => m}

class Graph() {
  val entries: m.Map[Residue, GraphEntry] = m.Map.empty
  
  def residues: Iterable[Residue] = entries.keys

  def roots: Iterable[Residue] = entries.filter({ case (r, e) => e.parent == None}).keySet

  def children(r: Residue): m.Map[Int, Residue] = entries(r).children

  def child(r: Residue, i: Int): Option[Residue] = entries(r).children.get(i)

  def parent(r: Residue): Option[Link] = entries(r).parent

  def +=(r: Residue): Unit = { entries(r) = GraphEntry.empty }
  def -=(r: Residue): Unit = {
    val er = entries(r)
    for(c <- er.children.values) removeBond(c)
    if(er.parent.isDefined) removeBond(r)
  }

  def ++=(rs: GenTraversableOnce[Residue]): Unit = { for(r <- rs) this += r }
  def --=(rs: GenTraversableOnce[Residue]): Unit = { for(r <- rs) this -= r }

  /**
   * create a bond between two residues
   * @param from source residue
   * @param to target residue
   * @param i target position
   */
  def addBond(from: Residue, to: Residue, i: Int): Unit = {

    entries(from).parent = Some(Link(to, i))
    if(i == 1) {
      entries(to).parent = Some(Link(from, i))
    } else {
      entries(to).children(i) = from
    }
  }

  /** remove bond between a source residue and its parent
    * @param from source residue
    * @return target position removed
    */
  def removeBond(from: Residue): Option[Link] = {
    for(link @ Link(p, i) <- parent(from)) yield {
      if(i == 1) {
        entries(p).parent = None
      } else {
        entries(p).children -= i
      }
      entries(from).parent = None
      link
    }
  }
}

object Graph {
  def apply(): Graph = new Graph()
  case class GraphEntry(children: m.Map[Int,  Residue], var parent: Option[Link])
  object GraphEntry {
    def empty = GraphEntry(m.Map.empty, None)
  }
}