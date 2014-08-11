package za.jwatson.glycanoweb.render

import importedjs.paper.{PointText, Point}
import importedjs.{paper => p}
import org.scalajs.dom.{HTMLCanvasElement, MouseEvent}
import org.scalajs.jquery.{jQuery => jQ}
import rx._
import za.jwatson.glycanoweb.GlycanoWeb
import za.jwatson.glycanoweb.render.GlycanoCanvas.TempBond
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure.Residue.Link
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js

import scalaz.syntax.std.option._
import scalaz.std.option._

class GlycanoCanvas(canvas: HTMLCanvasElement) {
  val graph = Var(RGraph())
  implicit def _graph = graph()
  val residues = Rx { graph().entries.keySet }
  val bonds = Rx { residues().filter(_.hasParent) }


  val scope = new p.PaperScope()
  scope.setup(canvas)
  val convention = rx.Var[Convention](new Convention(scope))
  implicit def _convention = convention()
  val o = rx.Obs(convention, skipInitial = true) {
    //todo: conversion between uct/cfg/oxford
  }


  def addResidue(ano: Anomer, abs: Absolute, rt: ResidueType, pos: p.Point): Residue = {
    val residue = Residue(rt, ano, abs)
    graph() += residue
    convention().addResidue(residue, pos)
    residue
  }

  def removeResidue(residue: Residue): Unit = {
    graph() -= residue
    convention().removeResidue(residue)
  }

  def addSubstituent(link: Link, st: SubstituentType): Substituent = {
    val substituent = Substituent(st)
    val (pos, mid) = nextSubstPos(link)
    graph() += link -> substituent
    convention().addSubstituent(substituent, pos, mid)
    substituent
  }

  def removeSubstituent(subst: Substituent): Unit = {
    graph() -= subst
    convention().removeSubstituent(subst)
  }

  implicit class RichResidue(residue: Residue) {
    def getItem: Option[p.Item] = convention().getItem(residue)
  }
  implicit class RichSubstituent(substituent: Substituent) {
    def getItem: Option[p.Item] = convention().getItem(substituent)
  }
  implicit class RichItem(item: p.Item) {
    def getResidue: Option[Residue] = convention().getResidue(item)
  }

  scope.project.layers.push(new p.Layer())
  val bondLayer = scope.project.layers(0)
  val residueLayer = scope.project.layers(1)
  scope.project.activeLayer = residueLayer

  scope.tool = new p.Tool()
  scope.tool.onMouseDown = (e: p.ToolEvent) => {
  }


  canvas.onmousedown = mouseDown _
  canvas.onmouseup = mouseUp _
  canvas.onmousemove = mouseMove _

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case object PlaceResidue extends InputState
    case object AddSubstituent extends InputState
    case class BoxSelect(down: p.Point) extends InputState
    case class Drag(last: p.Point) extends InputState
    case object CreateBond extends InputState
    case object PostCreateBond extends InputState
    case class Hit(down: p.Point, item: p.Item) extends InputState
  }
  import InputState._

  val state = Var[InputState](Default)

  val selection = Var(Set.empty[Residue])
/*
  Obs(state) {
    state() match {
      case Default =>
      case PlaceResidue =>
      case BoxSelect =>
        clearSelection()
      case Drag =>
      case CreateBond =>
    }
  }*/


  val boxSelect = Var[Option[p.Rectangle]](None)

  Obs(boxSelect) {
    convention().drawBoxSelection(boxSelect())
  }

  def updateBoxSelect(begin: p.Point, end: p.Point): Unit = {
    boxSelect() = Some(new p.Rectangle(begin, end))
    for(rect <- boxSelect()) {
      val newSelection = residues().filter(convention().testSelection(rect, _)).toSet
      updateSelection(newSelection)
    }
  }

  def endBoxSelect(): Unit = {
    boxSelect() = None
  }

  def updateSelection(newSelection: Set[Residue]): Unit = {
    val oldSelection = selection()
    if(newSelection != oldSelection) {
      for (res <- oldSelection diff newSelection) convention().unhighlightResidue(res)
      for (res <- newSelection diff oldSelection) convention().highlightResidue(res)
      selection() = newSelection
    }
  }

  var hitHandle: Boolean = false
  
  val tempBond: ItemSeqMod[p.Path, TempBond] = new ItemSeqMod[p.Path, TempBond] {
    override val updateWhenCreating = false
    override def create(s: TempBond): p.Path = {
      val line = p.Path.Line(s.p1, s.p2)
      line.strokeWidth = 10
      line.strokeColor = "#AAAAAA"
      line.dashArray = js.Array(1, 15)
      line.strokeCap = "round"
      line
    }
    override def update(t: p.Path, s: TempBond): Unit = {
      t.segments(0).point = s.p1
      t.segments(1).point = s.p2
    }
    override def remove(t: p.Path, s: TempBond): Unit = t.remove()
  }

  val tempLink: ItemSeqMod[p.PointText, Link] = new ItemSeqMod[p.PointText, Link] {
    override val updateWhenCreating: Boolean = true
    override def create(source: Link): PointText = {
      val num = new p.PointText(convention().linkPosition(source))
      num.content = s"${source.position}"
      num.fillColor = new p.Color("white")
      num.strokeColor = new p.Color("black")
      num.strokeWidth = 1
      num.asInstanceOf[js.Dynamic].fontSize = 30
      num
    }
    override def update(item: PointText, source: Link): Unit = {
      item.position = convention().linkPosition(source)
    }
    override def remove(item: PointText, source: Link): Unit = item.remove()
  }

  def beginBond(residue: Residue, point: p.Point): Unit = {
    for(link @ Link(p, i) <- residue.parent) {
      graph() -= Link(residue, 1)
      convention().removeBond(residue)
      if(i == 1) {
        graph() -= link
        convention().removeBond(p)
      }
    }
    tempLink(Link(residue, 1))
    tempBond(TempBond.LinkToPoint(Link(residue, 1), point))
  }

  def updateBondTarget(point: p.Point, angle: Double): Unit = {
    for(from <- tempBond.items.keys.headOption collect {
      case TempBond.LinkToPoint(link, _) => link
      case TempBond.LinkToLink(link, _) => link
    }) {
      val closestLink = convention().getClosestLinkAnyFilter(linkFilter = _.residue != from.residue, from = point, angle = angle)
      closestLink.fold {
        tempLink(from)
        tempBond(TempBond.LinkToPoint(from, point))
      } { to =>
        tempLink(Set(from, to))
        tempBond(TempBond.LinkToLink(from, to))
      }
    }
  }

  def endBond(): Unit = {
    tempBond.items.keys foreach {
      case TempBond.LinkToLink(Link(from, 1), toLink @ Link(to, i)) if from != to =>
        if(i == 1) {
          for (parent <- to.parent) {
            graph() -= toLink
            convention().removeBond(to)
          }
        }
        for (child <- to.child(i)) {
          graph() -= Link(child, 1)
          convention().removeBond(child)
        }

        graph() += Bond(from, toLink)
        convention().addBond(from, to, i)
      case _ =>
    }
  }

  val residueTemplate = Rx {
    GlycanoWeb.residueType().map { rt =>
      convention().showResidueTemplate(none, null)
      Residue(rt, GlycanoWeb.anomeric(), GlycanoWeb.absolute())
    }
  }

  val clearOldTemplate = Obs(residueTemplate) {
    for (rTemp <- residueTemplate()) {
      canvas.style.cursor = "none"
      state() = PlaceResidue
    }
    scope.view.draw()
  }

  Obs(GlycanoWeb.substituentType) {
    convention().showSubstituentTemplate(None, null)
    for (st <- GlycanoWeb.substituentType()) {
      canvas.style.cursor = "default"
      state() = AddSubstituent
    }
    scope.view.draw()
  }

  Obs(GlycanoWeb.showModeSelect) {
    if(!GlycanoWeb.showModeSelect()) {
      canvas.style.cursor = "default"
      state() = Default
    }
    convention().showResidueTemplate(None, null)
    convention().showSubstituentTemplate(None, null)
    scope.view.draw()
  }

  def updateSelectionBonds(): Unit = {
    val affectedBonds = for {
      r <- selection()
      b <- r +: (r.children.fold(Seq[Residue]())(_.values.toSeq)
        ++ r.parent.toSeq.map(_.residue))
    } yield b

    for {
      from <- affectedBonds
      Link(to, i) <- from.parent
    } convention().updateBond(from, to, i)
  }

  def moveSelectionBy(delta: p.Point) {
    for {
      residue <- selection()
      item <- residue.getItem
      _ = item.position = item.position add delta
      (i, substs) <- residue.substituents
      subst <- substs
      substItem <- subst.getItem
    } {
      substItem.position = substItem.position add delta
    }
  }

  def canvasTopLeft = {
    val left = jQ(canvas).offset().asInstanceOf[js.Dynamic].left.asInstanceOf[Double]
    val top = jQ(canvas).offset().asInstanceOf[js.Dynamic].top.asInstanceOf[Double]
    val scrollLeft = jQ(org.scalajs.dom.window).scrollLeft()
    val scrollTop = jQ(org.scalajs.dom.window).scrollTop()
    p.Point(left - scrollLeft, top - scrollTop)
  }

  def clientToProject(e: MouseEvent): p.Point = {
    val pointClient = p.Point(e.clientX, e.clientY)
    val pointOffset = pointClient subtract canvasTopLeft
    scope.view.viewToProject(pointOffset)
  }

  def cancelPlace(): Unit = {
    tempBond(Set.empty[TempBond])
    tempLink(Set.empty[Link])
    GlycanoWeb.setResidueType(None)
  }

  def cancelSubst(): Unit = {
    GlycanoWeb.setSubstituentType(None)
  }

  def linkFilter(link: Link): Boolean = {
    val bondOk = if (link.position == 1) !link.residue.hasParent else true
    link.residue.child(link.position).isEmpty && bondOk
  }

  def showPlaceTempBonds(point: p.Point, angle: Double): Unit = {
    for {
      t <- residueTemplate()
      ti <- convention().residueTemplateItem()
    } {
      val ris = for {
        r <- residues()
        item <- r.getItem
      } yield r -> item
      val (right, left) = ris.partition {
        case (r, i) => convention().onRightOf(point, i.position, angle)
      }
      val rightTB = for {
        point <- convention().residueTemplateLinkPosition(1)
        link <- convention().getClosestLinkAnyFilterFrom(right.map(_._1), linkFilter, point, Some(true), angle, 200 * 200)
      } yield TempBond.LinkToTemplate(link, 1)
      
      val validLefts = for {
        (r, i) <- left.toSeq if !r.hasParent
        link = Link(r, 1)
        linkPt = convention().linkPosition(link)
        dist = linkPt.getDistance(point, squared = true)
        if dist < 200 * 200
      } yield (link, i, dist)
      val leftSorted = validLefts.sortBy(_._3).take(t.rt.linkage - 1)
      val slots = for {
        i: Int <- (1 to t.rt.linkage).toSet
        pt <- convention().residueTemplateLinkPosition(i)
      } yield i -> pt

      def slotAll(lefts: List[Link], slots: Set[(Int, p.Point)]): List[TempBond.LinkToTemplate] =
        lefts match {
          case Nil => Nil
          case x :: xs =>
            val lp = convention().linkPosition(x)
            val min = slots.minBy(_._2.getDistance(lp, squared = true))
            TempBond.LinkToTemplate(x, min._1) :: slotAll(xs, slots - min)
        }
      val leftTBs = slotAll(leftSorted.map(_._1).toList, slots)
      tempLink(leftTBs.map(_.link) ++ rightTB.map(_.link))
      tempBond(leftTBs ++ rightTB)
    }
  }

  def mouseDown(e: MouseEvent): Unit = {
    canvas.focus()
    canvas.blur()
    val point = clientToProject(e)
    state() match {
      case Default =>
        if(e.button == 0) {
          val hitTest = scope.project.hitTest(point)
          Option(hitTest) match {
            case Some(hit) =>
              if (hit.item matches js.Dynamic.literal(name = "delete": js.Any)) {
                val deleted = selection()
                updateSelection(Set.empty)
                for (r <- deleted) {
                  for {
                    (i, ss) <- r.substituents
                    s <- ss
                  } {
                    convention().removeSubstituent(s)
                  }
                  for (b <- r.bond) {
                    graph() -= b
                    convention().removeBond(r)
                  }
                  for {
                    m <- r.children
                    (i, c) <- m
                  } {
                    graph() -= Link(r, i)
                    convention().removeBond(c)
                  }
                  graph() -= r
                  convention().removeResidue(r)
                }
              } else for (r <- hit.item.getResidue) {
                if (convention().hitHandle(r, point)) {
                  convention().handlePress(r.some)
                  beginBond(r, point)
                  convention().handleHL(None)
                  state() = CreateBond
                } else {
                  state() = Hit(point, hit.item)
                  if(!selection().contains(r)) {
                    updateSelection(Set(r))
                  }
                }
              }
            case None =>
              updateBoxSelect(point, point)
              state() = BoxSelect(point)
          }
        }
      case PlaceResidue =>
        e.button match {
          case 0 =>
            for(t <- residueTemplate()) {
              val added = addResidue(t.anomer, t.absolute, t.rt, point)
              tempBond.items.keys foreach {
                case TempBond.LinkToTemplate(Link(from, 1), i) =>
                  graph() += Bond(from, Link(added, i))
                  convention().addBond(from, added, i)
                case TempBond.LinkToTemplate(toLink @ Link(to, i), 1) =>
                  graph() += Bond(added, toLink)
                  convention().addBond(added, to, i)
                case _ =>
              }
            }
          case 2 =>
            cancelPlace()
        }
      case AddSubstituent =>
        e.button match {
          case 0 =>
            for {
              st <- GlycanoWeb.substituentType()
              link <- convention().getClosestLinkAny(point, None)
            } {
              val added = addSubstituent(link, st)
            }
          case 2 =>
            cancelPlace()
        }
      case BoxSelect(_) =>
      case Drag(_) =>
      case CreateBond =>
        if(e.button == 0) {
          endBond()
        }
        tempBond(Set.empty[TempBond])
        tempLink(Set.empty[Link])
        state() = PostCreateBond
      case PostCreateBond =>
      case Hit(_, _) =>
    }
  }

  def mouseUp(e: MouseEvent): Unit = {
    state() match {
      case Default =>
      case PlaceResidue =>
      case AddSubstituent =>
      case BoxSelect(_) =>
        endBoxSelect()
        state() = Default
      case Drag(_) =>
        state() = Default
      case CreateBond =>
        convention().handlePress(None)
      case PostCreateBond =>
        state() = Default
      case Hit(_, item) =>
        for(r <- item.getResidue) {
          updateSelection(Set(r))
        }
        state() = Default
    }
  }

  def stackOffset(stack: Vector[Substituent]): Double = {
    stack match {
      case x +: xs =>
        val first = x.getItem.map(_.bounds.height / 2).getOrElse(0.0)
        val rest = xs.flatMap(_.getItem).map(_.bounds.height).sum
        first + rest
      case _ => 0
    }
  }

  def nextSubstPos(link: Link): (p.Point, Boolean) = {
    val stack = link.substituents
    val top = convention().linkPosition(link)
    val offset = stackOffset(stack)
    (top.add(0, offset), stack.isEmpty)
  }

  def mouseMove(e: MouseEvent): Unit = {
    val point = clientToProject(e)
    state() match {
      case Default =>
        val hitTest = scope.project.hitTest(point)
        val hitHandle = for {
          hit <- Option(hitTest)
          r <- hit.item.getResidue
          if convention().hitHandle(r, point)
        } yield r
        convention().handleHL(hitHandle)
      case PlaceResidue =>
        convention().showResidueTemplate(residueTemplate(), point)
        showPlaceTempBonds(point, angle = 0.0)
      case AddSubstituent =>
        convention().getClosestLinkAny(point, None).fold {
          convention().showSubstituentTemplate(GlycanoWeb.substituentType(), point, mid = true)
        } { link =>
          val (pos, mid) = nextSubstPos(link)
          convention().showSubstituentTemplate(GlycanoWeb.substituentType(), pos, mid)
        }
      case BoxSelect(down) =>
        updateBoxSelect(down, point)
      case Drag(last) =>
        val delta = point subtract last
        moveSelectionBy(delta)
        updateSelectionBonds()
        state() = Drag(point)
      case CreateBond =>
        updateBondTarget(point, angle = 0.0)
      case PostCreateBond =>
      case Hit(down, _) =>
        state() = Drag(down)
    }
  }
}

object GlycanoCanvas {
  sealed trait TempBond {
    def p1: p.Point
    def p2: p.Point
  }
  object TempBond {
    case class LinkToPoint(link: Link, point: p.Point)(implicit c: Convention) extends TempBond {
      override def p1: Point = c.linkPosition(link)
      override def p2: Point = point
    }
    case class LinkToLink(link1: Link, link2: Link)(implicit c: Convention) extends TempBond {
      override def p1: Point = c.linkPosition(link1)
      override def p2: Point = c.linkPosition(link2)
    }
    case class PointToPoint(p1: p.Point, p2: p.Point) extends TempBond
    case class LinkToTemplate(link: Link, i: Int)(implicit c: Convention) extends TempBond  {
      override def p1: Point = c.linkPosition(link)
      override def p2: Point = c.residueTemplateLinkPosition(i).getOrElse(p.Point(0, 0))
    }
  }
//  case class TempBond(from: Link, to: p.Point, toPos: Option[Int], toLink: Option[Link])
  case class Selected(residue: Residue, offset: p.Point)
}