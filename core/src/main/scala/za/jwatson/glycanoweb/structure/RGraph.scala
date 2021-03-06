package za.jwatson.glycanoweb.structure

import RGraph._
import japgolly.scalajs.react.extra.Reusability
import za.jwatson.glycanoweb.GlyAnnot

import scala.annotation.tailrec
import scalaz.State
import scalaz.syntax.foldable._
import scalaz.std.map._
import scalaz.std.vector._
import scalaz.syntax.std.option._

import monocle.Monocle._
import monocle.macros.{GenLens, Lenses}

case class RGraph(
  residues: Map[ResidueId, GraphEntry] = Map.empty,
  annotations: Map[AnnotId, Annot] = Map.empty
) {
  def size: Int = residues.size + annotations.size
  def isEmpty: Boolean = residues.isEmpty && annotations.isEmpty
  def nonEmpty: Boolean = !isEmpty
}

@Lenses case class Placement(x: Double, y: Double, rotation: Double)

object RGraph {
  implicit val reusability: Reusability[RGraph] = Reusability.by_==

  val lenser = GenLens[RGraph]
  val residues = lenser(_.residues)
  val annotations = lenser(_.annotations)

  def of(xs: Seq[GraphEntry]): RGraph = RGraph(xs.map(ge => ResidueId.next() -> ge).toMap)

  @Lenses case class GraphEntry(
    residue: Residue,
    x: Double = 0, y: Double = 0, rotation: Double = 0,
    children: Map[Int, ResidueId] = Map.empty,
    parent: Option[Link] = None
  ) {
    def +(ce: (Int, ResidueId)) = this.copy(children = children + ce)
    def -(ci: Int) = this.copy(children = children - ci)
  }

  object GraphEntry {
    implicit val reusability: Reusability[GraphEntry] = Reusability.by_==
  }

  @tailrec def rootAnomer(r: ResidueId, visited: Set[ResidueId] = Set.empty)(implicit g: RGraph): Anomer =
    r.parent match {
      case Some(Link(to, _)) if !visited.contains(to) && to.graphEntry.exists(_.residue.rt.category != ResidueCategory.Repeat) => rootAnomer(to, visited + to)
      case _ => r.anomer.getOrElse(Anomer.Alpha)
    }

  def entryL(r: ResidueId) = RGraph.residues ^|-? index(r)
  def getParentL(r: ResidueId) = entryL(r) ^|-> GraphEntry.parent ^<-? some
  def getChildL(r: ResidueId, i: Int) = entryL(r) ^|-> GraphEntry.children ^|-? index(i)
  def linkSubstsL(link: Link) = entryL(link.r) ^|-> GraphEntry.residue ^|-> Residue.subs ^|-? index(link.position)

  case class Bond(from: ResidueId, to: Link)
  object Bond {
    implicit val reusability: Reusability[Bond] = Reusability.by_==
  }
//
//  def paste(buffer: RGraph): State[RGraph, RGraph] = for {
//    _ <- State.get[RGraph]
//    _ <- State.modify[RGraph](buffer.entries)
//  } yield buffer
  
  def addBond(bond: Bond): State[RGraph, Unit] = addBond(bond.from)(bond.to)
  def addBond(from: ResidueId)(to: Link): State[RGraph, Unit] = for {
    _ <- State.modify(entryL(to.r) ^|-> GraphEntry.children modify (_.updated(to.position, from)))
    _ <- State.modify(entryL(from) ^|-> GraphEntry.parent set Some(to))
  } yield ()

  def removeBond(bond: Bond): State[RGraph, Unit] = removeBond(bond.from)(bond.to)
  def removeBond(from: ResidueId)(to: Link): State[RGraph, Unit] = for {
    _ <- State.modify(entryL(from) ^|-> GraphEntry.parent set None)
    _ <- State.modify(entryL(to.r) ^|-> GraphEntry.children modify (_ - to.position))
  } yield ()

  def removeLink(link: Link): State[RGraph, Unit] = link match {
    case Link(from, 1) => for {
      to <- State.gets(getParentL(from).getOption)
      _ <- to.cata(removeBond(from)(_), State.modify[RGraph](identity))
    } yield ()
    case to @ Link(r, i) => for {
      from <- State.gets(getChildL(r, i).getOption)
      _ <- from.cata(removeBond(_)(to), State.modify[RGraph](identity))
    } yield ()
  }

  def addSubst(link: Link)(subst: SubstituentType) = linkSubstsL(link) modify (_ :+ subst)

  def removeSubst(link: Link)(position: Int) = linkSubstsL(link) modify { v => v.take(position) ++ v.drop(position + 1) }

  def addResidue(r: Residue) = residues modify { _ + (ResidueId.next() -> GraphEntry(r)) }

  def removeResidue(r: ResidueId): State[RGraph, Unit] = for {
    parent <- State.gets(getParentL(r).getOption)
    children <- State.gets[RGraph, List[ResidueId]]((entryL(r) ^|-> GraphEntry.children ^|->> each).getAll)
    _ <- State.modify(children.foldLeft(_: RGraph)((g, c) => g &|-? entryL(c) ^|-> GraphEntry.parent set None))
    _ <- State.modify(parent.foldLeft(_: RGraph)((g, link) => g &|-? entryL(link.r) ^|-> GraphEntry.children ^|-> at(link.position) set None))
    _ <- State.modify(residues.modify(_ - r))
  } yield ()

  def setPlacement(r: ResidueId, placement: Placement): State[RGraph, Unit] = for {
    _ <- State.modify(entryL(r) ^|-> GraphEntry.x set placement.x)
    _ <- State.modify(entryL(r) ^|-> GraphEntry.y set placement.y)
    _ <- State.modify(entryL(r) ^|-> GraphEntry.rotation set placement.rotation)
  } yield ()

  def addBondRemovingOld(from: ResidueId)(to: Link): State[RGraph, Unit] = for {
    _ <- RGraph.removeLink(to)
    _ <- RGraph.addBond(from)(to)
  } yield ()
  
  implicit class RGraphOps(g: RGraph) {
    def -(r: ResidueId): RGraph = removeResidue(r) exec g
    def -(bond: Bond): RGraph = removeBond(bond) exec g
    def -(link: Link): RGraph = removeLink(link) exec g
    def -(subst: (Link, Int)): RGraph = removeSubst(subst._1)(subst._2)(g)
    def -(a: AnnotId): RGraph = g &|-> annotations modify { _ - a }
    def --(links: Iterable[Link]): RGraph = links.foldLeft(g)(_ - _)

    def +(ge: GraphEntry): RGraph = {
      val id = ResidueId.next()
      val g2 = g &|-> residues modify { _ + (id -> ge) }
      val g3 = ge.parent.fold(g2)(link => g2 + Bond(id, link))
      val g4 = ge.children.foldLeft(g3) {
        case (g0, (i, cid)) =>
          g0 + Bond(cid, Link(id, i))
      }
      g4
    }
    def +(residue: Residue): RGraph = addResidue(residue)(g)
    def +(bond: Bond): RGraph = addBond(bond) exec g
    //def +(bond: (Residue, Link)): RGraph = addBond(bond._1)(bond._2) exec g
    def +(subst: (Link, SubstituentType)): RGraph = g &|-? linkSubstsL(subst._1) modify (_ :+ subst._2)
    def +(annot: Annot): RGraph = g &|-> annotations modify { _ + (AnnotId.next() -> annot) }

    def updated(r: ResidueId, placement: Placement): RGraph = setPlacement(r, placement) exec g
    
//    def --(residues: Seq[Residue]): RGraph = entriesL.mod(_ -- residues, g)
//    def ++(residues: Seq[Residue]): RGraph = entriesL.mod(_ ++ residues.map(_ -> GraphEntry()), g)

    def roots: Set[ResidueId] = g.residues.filter(_._2.parent.isEmpty).keySet
  }

  implicit class ResidueOps(r: ResidueId) {
    lazy val entry = RGraph.residues ^|-? index(r)

    def graphEntry(implicit g: RGraph) = entry getOption g

    def x(implicit g: RGraph) = entry ^|-> GraphEntry.x getOption g
    def y(implicit g: RGraph) = entry ^|-> GraphEntry.y getOption g
    def rotation(implicit g: RGraph) = entry ^|-> GraphEntry.rotation getOption g

    def parent(implicit g: RGraph) = entry ^|-> GraphEntry.parent ^<-? some getOption g
    def children(implicit g: RGraph) = entry ^|-> GraphEntry.children getOption g
    def child(i: Int)(implicit g: RGraph) = entry ^|-> GraphEntry.children ^|-? index(i) getOption g
    def bond(implicit g: RGraph) = entry ^|-> GraphEntry.parent ^<-? some getOption g map (Bond(r, _))

    def residue(implicit g: RGraph) = entry ^|-> GraphEntry.residue getOption g

    def anomer(implicit g: RGraph) = entry ^|-> GraphEntry.residue ^|-> Residue.ano getOption g
    def absolute(implicit g: RGraph) = entry ^|-> GraphEntry.residue ^|-> Residue.abs getOption g
    def rt(implicit g: RGraph) = entry ^|-> GraphEntry.residue ^|-> Residue.rt getOption g
    def substituents(implicit g: RGraph) = (entryL(r) ^|-> GraphEntry.residue ^|-> Residue.subs getOption g).orZero

    def hasParent(implicit g: RGraph): Boolean = g.residues.get(r).fold(false)(_.parent.nonEmpty)
    def hasChildren(implicit g: RGraph): Boolean = g.residues.get(r).fold(false)(_.children.nonEmpty)

    def numChildren(implicit g: RGraph): Int = g.residues.get(r).fold(0)(_.children.size)

    def isRoot(implicit g: RGraph): Boolean = !hasParent
    def isLeaf(implicit g: RGraph): Boolean = !hasChildren

  }

  implicit class LinkOps(l: Link) {
    def substituents(implicit g: RGraph) = (entryL(l.r) ^|-> GraphEntry.residue ^|-> Residue.subs ^|-? index(l.position) getOption g).orZero
  }
}
