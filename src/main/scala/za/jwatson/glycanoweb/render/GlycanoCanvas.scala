package za.jwatson.glycanoweb.render

import importedjs.{paper => p}
import org.scalajs.dom.{HTMLCanvasElement, MouseEvent}
import org.scalajs.jquery.{jQuery => jQ}
import rx._
import za.jwatson.glycanoweb.GlycanoWeb
import za.jwatson.glycanoweb.render.Convention.UCT
import za.jwatson.glycanoweb.render.GlycanoCanvas.TempBond
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure.Residue.Link
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js

class GlycanoCanvas(canvas: HTMLCanvasElement) {
  val graph = Var(RGraph())
  implicit def _graph = graph()
  val residues = Rx { graph().entries.keySet }
  val bonds = Rx { residues().filter(_.hasParent) }


  val scope = new p.PaperScope()
  scope.setup(canvas)
  val convention = rx.Var[Convention](new UCT(scope))
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

  implicit class RichResidue(residue: Residue) {
    def getItem: Option[p.Item] = convention().getItem(residue)
  }
  implicit class RichItem(item: p.Item) {
    def getResidue: Option[Residue] = convention().getResidue(item)
  }

  scope.project.layers.push(new p.Layer())
  val bondLayer = scope.project.layers(0)
  val residueLayer = scope.project.layers(1)
  scope.project.activeLayer = residueLayer

  canvas.onmousedown = mouseDown _
  canvas.onmouseup = mouseUp _
  canvas.onmousemove = mouseMove _

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case object PlaceResidue extends InputState
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
  val tempBonds = Var[Seq[TempBond]](Seq.empty)

  Obs(tempBonds) {
    val tbs = tempBonds()
    convention() tempBond (if(tbs.isEmpty) None else Some(tbs))
  }

  def beginBond(residue: Residue, point: p.Point): Unit = {
    for(link @ Link(p, i) <- residue.parent) {
      graph() -= link
      convention().removeBond(residue)
      if(i == 1) convention().removeBond(p)
    }
    tempBonds() = Seq(TempBond(Link(residue, 1), point, None, None))
  }
  
  def updateBondSource(point: p.Point): Unit = {
    for(TempBond(Link(from, i), to, _, _) <- tempBonds()) {
      if(i == 1) {

      }
      convention().getClosestLinkAny(point, parent = true, angle = 0)
    }
    tempBonds() = tempBonds()
  }

  def updateBondTarget(point: p.Point): Unit = {
    tempBonds() = tempBonds().map(_.copy(to = point))
  }

  def endBond(): Unit = {
    for {
      TempBond(fromLink @ Link(from, fromP), toPoint, _, _) <- tempBonds()
      toLink @ Link(to, p) <- convention().finishBond(fromLink, toPoint)
      if from != to
    } {
      for(parent <- to.parent) convention().removeBond(to)
      for(c <- to.child(p)) convention().removeBond(c)
      graph() -= toLink
      for(old <- to.parent if old.position == 1) {
        graph() -= old
        convention().removeBond(old.residue)
      }

      graph() += Bond(from, toLink)
      convention().addBond(from, to, p)
    }

    tempBonds() = Seq.empty
  }

  val residueTemplate = Rx {
    GlycanoWeb.residueType().map(
      Residue(_, GlycanoWeb.anomeric(), GlycanoWeb.absolute())
    )
  }

  val clearOldTemplate = Obs(residueTemplate) {
    state() = residueTemplate() match {
      case Some(r) => canvas.style.cursor = "none"; PlaceResidue
      case None => canvas.style.cursor = "default"; Default
    }
    convention().showResidueTemplate(None, null)
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
    } {
      item.position = item.position add delta
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
    tempBonds() = Seq.empty
    GlycanoWeb.residueType() = None
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
      val rrss = for {
        rightPt <- convention().residueTemplateLinkPosition(1).toSeq
        (rr, rri) <- right
        rrlink <- convention().getClosestLink(rightPt, rr, parent = true, angle)
        rrpos = convention().linkPosition(rrlink)
        if convention().linkValid(rightPt, rrpos, parent = true, angle)
        rrdist = rightPt.getDistance(rrpos, squared = true)
        if rrdist < 500 * 500
      } yield (rrlink, rrdist, rrpos)
      val rightTB = if(rrss.isEmpty) List() else {
        val rrmin = rrss.minBy(_._2)
        List(TempBond(null, rrmin._3, None, Some(rrmin._1)))
      }
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

      def slotAll(lefts: List[Link], slots: Set[(Int, p.Point)]): List[TempBond] =
        lefts match {
          case Nil => Nil
          case x :: xs =>
            val lp = convention().linkPosition(x)
            val min = slots.minBy(_._2.getDistance(lp, squared = true))
            TempBond(x, min._2, Some(min._1), None) :: slotAll(xs, slots - min)
        }
      tempBonds() = slotAll(leftSorted.map(_._1).toList, slots) ++ rightTB
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
              for (r <- hit.item.getResidue) {
                if (convention().hitHandle(r, point)) {
                  convention().handlePress(Some(r))
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
              for {
                tb <- tempBonds().filter(_.toPos.isDefined)
                leftPos <- tb.toPos
              } {
                graph() += Bond(tb.from.residue, Link(added, leftPos))
                convention().addBond(tb.from.residue, added, leftPos)
              }
              for {
                tb <- tempBonds()
                rightLink <- tb.toLink
              } {
                graph() += Bond(added, rightLink)
                convention().addBond(added, rightLink.residue, rightLink.position)
              }
            }
          case 2 =>
            cancelPlace()
        }
      case BoxSelect(_) =>
      case Drag(_) =>
      case CreateBond =>
        if(e.button != 0) {
          convention() tempBond None
        }
        endBond()
        state() = PostCreateBond
      case PostCreateBond =>
      case Hit(_, _) =>
    }
  }

  def mouseUp(e: MouseEvent): Unit = {
    state() match {
      case Default =>
      case PlaceResidue =>
      case BoxSelect(_) =>
        endBoxSelect()
        state() = Default
      case Drag(_) =>
        state() = Default
      case CreateBond =>
        convention().handlePress(None)
      case PostCreateBond =>
        convention().highlightLink(Seq.empty)
        state() = Default
      case Hit(_, item) =>
        for(r <- item.getResidue) {
          updateSelection(Set(r))
        }
        state() = Default
    }
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
        updateBondTarget(point)
      case BoxSelect(down) =>
        updateBoxSelect(down, point)
      case Drag(last) =>
        val delta = point subtract last
        moveSelectionBy(delta)
        updateSelectionBonds()
        state() = Drag(point)
      case CreateBond =>
        updateBondTarget(point)
        convention().highlightLink(for {
          tb <- tempBonds()
          link <- convention().getClosestLinkAny(tb.to, parent = true)
        } yield link)
      case PostCreateBond =>
      case Hit(down, _) =>
        state() = Drag(down)
    }
  }
}

object GlycanoCanvas {
  case class TempBond(from: Link, to: p.Point, toPos: Option[Int], toLink: Option[Link])
  case class Selected(residue: Residue, offset: p.Point)
}