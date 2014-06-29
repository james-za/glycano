package za.jwatson.glycanoweb.structure

import za.jwatson.glycanoweb.structure.RGraph.GraphEntry
import za.jwatson.glycanoweb.structure.Residue.Link

import scalaz._, Scalaz._
import PLens._

case class RGraph(entries: Map[Residue, GraphEntry] = Map.empty)

object RGraph {
  def apply(xs: Seq[Residue]): RGraph = RGraph(xs.map(_ -> GraphEntry()).toMap)

  case class GraphEntry(children: Map[Int, Residue] = Map.empty, parent: Option[Link] = None) {
    def +(ce: (Int, Residue)) = this.copy(children = children + ce)
    def -(ci: Int) = this.copy(children = children - ci)
  }

  private val entriesL: RGraph @> Map[Residue, GraphEntry] = Lens.lensg(g => es2 => g.copy(entries = es2), _.entries)
  private def entryL(r: Residue): RGraph @?> GraphEntry = ~entriesL >=> PLens.mapVPLens(r)

  private val childrenMapL: GraphEntry @> Map[Int, Residue] = Lens.lensg(ge => c2 => ge.copy(children = c2), ge => ge.children)
  private def childL(i: Int): GraphEntry @?> Residue = ~childrenMapL >=> PLens.mapVPLens(i)

  private val parentL: GraphEntry @> Option[Link] = Lens.lensg(ge => p2 => ge.copy(parent = p2), ge => ge.parent)

  private def getParentL(r: Residue): RGraph @?> Link = entryL(r) >=> ~parentL >=> somePLens
  private def getChildL(r: Residue, i: Int): RGraph @?> Residue = entryL(r) >=> childL(i)

  case class Bond(from: Residue, to: Link)
  
  def addBond(bond: Bond): State[RGraph, Unit] = addBond(bond.from)(bond.to)
  def addBond(from: Residue)(to: Link): State[RGraph, Unit] = for {
    _ <- entryL(to.residue) >=> ~childrenMapL %== { _ + (to.position -> from) }
    _ <- entryL(from) >=> ~parentL := to.some
  } yield ()

  def removeBond(bond: Bond): State[RGraph, Unit] = removeBond(bond.from)(bond.to)
  def removeBond(from: Residue)(to: Link): State[RGraph, Unit] = for {
    _ <- entryL(from) >=> ~parentL := none[Link]
    _ <- entryL(to.residue) >=> ~childrenMapL %== { _ - to.position }
  } yield ()

  def removeLink(link: Link): PState[RGraph, Unit] = link match {
    case Link(from, 1) => getParentL(from) >>- (removeBond(from)(_))
    case to @ Link(r, i) => getChildL(r, i) >>- (removeBond(_)(to))
  }
  
  implicit class RGraphOps(g: RGraph) {
    def -(residue: Residue): RGraph = entriesL.mod(_ - residue, g)
    def -(bond: Bond): RGraph = removeBond(bond) exec g
    def -(link: Link): RGraph = removeLink(link) exec g

    def +(residue: Residue): RGraph = entriesL.mod(_ + (residue -> GraphEntry()), g)
    def +(bond: Bond): RGraph = addBond(bond) exec g
    def +(bond: (Residue, Link)): RGraph = addBond(bond._1)(bond._2) exec g

    def --(residues: Seq[Residue]): RGraph = entriesL.mod(_ -- residues, g)
    def ++(residues: Seq[Residue]): RGraph = entriesL.mod(_ ++ residues.map(_ -> GraphEntry()), g)

    def roots: Set[Residue] = g.entries.filter(_._2.parent.isEmpty).keySet
  }

  implicit class ResidueOps(r: Residue) {
    def parent(implicit g: RGraph): Option[Link] = getParentL(r) get g
    def children(implicit g: RGraph): Option[Map[Int, Residue]] = entryL(r) >=> ~childrenMapL get g
    def child(i: Int)(implicit g: RGraph): Option[Residue] = getChildL(r, i) get g

    def hasParent(implicit g: RGraph): Boolean = g.entries.get(r).fold(false)(_.parent.nonEmpty)
    def hasChildren(implicit g: RGraph): Boolean = g.entries.get(r).fold(false)(_.children.nonEmpty)

    def numChildren(implicit g: RGraph): Int = g.entries.get(r).fold(0)(_.children.size)

    def isRoot(implicit g: RGraph): Boolean = !hasParent
    def isLeaf(implicit g: RGraph): Boolean = !hasChildren
  }
}
