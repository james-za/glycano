package za.jwatson.glycanoweb.structure

import RGraph._
import za.jwatson.glycanoweb.GlyAnnot

import scalaz.Maybe.Just
import scalaz.{Maybe, IList, State}
import scalaz.syntax.foldable._
import scalaz.std.map._
import scalaz.std.vector._

import monocle.Monocle._
import monocle.macros.{Lenses, Lenser}

case class RGraph(
  entries: Map[Residue, GraphEntry] = Map.empty,
  annots: Map[Int, GlyAnnot] = Map.empty)

@Lenses case class Placement(x: Double, y: Double, rotation: Double)

object RGraph {
  val lenser = Lenser[RGraph]
  val entries = lenser(_.entries)
  val annots = lenser(_.annots)

  def of(xs: Seq[Residue]): RGraph = RGraph(xs.map(_ -> GraphEntry()).toMap)

  @Lenses case class GraphEntry(
    x: Double = 0, y: Double = 0, rotation: Double = 0,
    children: Map[Int, Residue] = Map.empty,
    parent: Option[Link] = None,
    subs: Map[Int, Vector[SubstituentType]] = Map.empty
  ) {
    def +(ce: (Int, Residue)) = this.copy(children = children + ce)
    def -(ci: Int) = this.copy(children = children - ci)
  }

  def entryL(r: Residue) = RGraph.entries ^|-? index(r)
  def getParentL(r: Residue) = entryL(r) ^|-> GraphEntry.parent ^<-? some
  def getChildL(r: Residue, i: Int) = entryL(r) ^|-> GraphEntry.children ^|-? index(i)
  def linkSubstsL(link: Link) = entryL(link.residue) ^|-> GraphEntry.subs ^|-? index(link.position)

  case class Bond(from: Residue, to: Link)
//
//  def paste(buffer: RGraph): State[RGraph, RGraph] = for {
//    _ <- State.get[RGraph]
//    _ <- State.modify[RGraph](buffer.entries)
//  } yield buffer
  
  def addBond(bond: Bond): State[RGraph, Unit] = addBond(bond.from)(bond.to)
  def addBond(from: Residue)(to: Link): State[RGraph, Unit] = for {
    _ <- State.modify(entryL(to.residue) ^|-> GraphEntry.children modify (_.updated(to.position, from)))
    //_ <- State.modify(getParentL(from) set to)
    _ <- State.modify(entryL(from) ^|-> GraphEntry.parent set Some(to))
  } yield ()

  def removeBond(bond: Bond): State[RGraph, Unit] = removeBond(bond.from)(bond.to)
  def removeBond(from: Residue)(to: Link): State[RGraph, Unit] = for {
    _ <- State.modify(entryL(from) ^|-> GraphEntry.parent set None)
    _ <- State.modify(entryL(to.residue) ^|-> GraphEntry.children modify (_ - to.position))
  } yield ()

  def removeLink(link: Link): State[RGraph, Unit] = link match {
    case Link(from, 1) => for {
      to <- State.gets(getParentL(from).getMaybe)
      _ <- to.cata(removeBond(from)(_), State.modify[RGraph](g => g))
    } yield ()
    case to @ Link(r, i) => for {
      from <- State.gets(getChildL(r, i).getMaybe)
      _ <- from.cata(removeBond(_)(to), State.modify[RGraph](g => g))
    } yield ()
  }

  def addSubst(link: Link)(subst: SubstituentType) = linkSubstsL(link) modify (_ :+ subst)

  def removeSubst(link: Link)(position: Int) = linkSubstsL(link) modify (_.take(position).drop(1))

  def addResidue(r: Residue) = entryL(r) set GraphEntry()

  def removeResidue(r: Residue): State[RGraph, Unit] = for {
    parent <- State.gets(getParentL(r).getMaybe)
    children <- State.gets[RGraph, IList[Residue]]((entryL(r) ^|-> GraphEntry.children ^|->> each).getAll)
    _ <- State.modify(children.foldLeft(_: RGraph)((g, c) => g &|-? entryL(c) ^|-> GraphEntry.parent set None))
    _ <- State.modify(parent.foldLeft(_: RGraph)((g, link) => g &|-? entryL(link.residue) ^|-> GraphEntry.children ^|-> at(link.position) set Maybe.empty))
  } yield ()

  def setPlacement(r: Residue, placement: Placement): State[RGraph, Unit] = for {
    _ <- State.modify(entryL(r) ^|-> GraphEntry.x set placement.x)
    _ <- State.modify(entryL(r) ^|-> GraphEntry.y set placement.y)
    _ <- State.modify(entryL(r) ^|-> GraphEntry.rotation set placement.rotation)
  } yield ()

  
  implicit class RGraphOps(g: RGraph) {
    def -(residue: Residue): RGraph = g &|-> entries modify (_ - residue)
    def -(bond: Bond): RGraph = removeBond(bond) exec g
    def -(link: Link): RGraph = removeLink(link) exec g
    def -(subst: (Link, Int)): RGraph = removeSubst(subst._1)(subst._2)(g)
    def -(annot: GlyAnnot): RGraph = g &|-> annots modify (_ - annot.id)
    def --(links: Iterable[Link]): RGraph = links.foldLeft(g)(_ - _)

    def +(residue: Residue): RGraph = g &|-> entries modify (_.updated(residue, GraphEntry()))
    def +(bond: Bond): RGraph = addBond(bond) exec g
    //def +(bond: (Residue, Link)): RGraph = addBond(bond._1)(bond._2) exec g
    def +(subst: (Link, SubstituentType)): RGraph = g &|-? linkSubstsL(subst._1) modify (_ :+ subst._2)
    def +(annot: GlyAnnot): RGraph = g &|-> annots ^|-? index(annot.id) set annot

    def updated(r: Residue, placement: Placement): RGraph = setPlacement(r, placement) exec g
    
//    def --(residues: Seq[Residue]): RGraph = entriesL.mod(_ -- residues, g)
//    def ++(residues: Seq[Residue]): RGraph = entriesL.mod(_ ++ residues.map(_ -> GraphEntry()), g)

    def roots: Set[Residue] = g.entries.filter(_._2.parent.isEmpty).keySet

    def residues: Set[Residue] = g.entries.keySet
  }

  implicit class ResidueOps(r: Residue) {
    lazy val entry = RGraph.entries ^|-? index(r)

    def graphEntry(implicit g: RGraph) = (entry getMaybe g).toOption

    def x(implicit g: RGraph) = (entry ^|-> GraphEntry.x getMaybe g).toOption
    def y(implicit g: RGraph) = (entry ^|-> GraphEntry.y getMaybe g).toOption
    def rotation(implicit g: RGraph) = (entry ^|-> GraphEntry.rotation getMaybe g).toOption

    def parent(implicit g: RGraph) = (entry ^|-> GraphEntry.parent ^<-? some getMaybe g).toOption
    def children(implicit g: RGraph) = (entry ^|-> GraphEntry.children getMaybe g).toOption
    def child(i: Int)(implicit g: RGraph) = (entry ^|-> GraphEntry.children ^|-? index(i) getMaybe g).toOption
    def bond(implicit g: RGraph) = (entry ^|-> GraphEntry.parent ^<-? some getMaybe g map (Bond(r, _))).toOption

    def hasParent(implicit g: RGraph): Boolean = g.entries.get(r).fold(false)(_.parent.nonEmpty)
    def hasChildren(implicit g: RGraph): Boolean = g.entries.get(r).fold(false)(_.children.nonEmpty)

    def numChildren(implicit g: RGraph): Int = g.entries.get(r).fold(0)(_.children.size)

    def isRoot(implicit g: RGraph): Boolean = !hasParent
    def isLeaf(implicit g: RGraph): Boolean = !hasChildren

    def substituents(implicit g: RGraph) = (entryL(r) ^|-> GraphEntry.subs getMaybe g).orZero
  }

  implicit class LinkOps(l: Link) {
    def substituents(implicit g: RGraph) = (entryL(l.residue) ^|-> GraphEntry.subs ^|-? index(l.position) getMaybe g).orZero
  }
}
