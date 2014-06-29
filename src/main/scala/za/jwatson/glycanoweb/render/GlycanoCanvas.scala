package za.jwatson.glycanoweb.render

import importedjs.{paper => p}
import org.scalajs.dom.{HTMLCanvasElement, MouseEvent}
import org.scalajs.jquery.{jQuery => jQ}
import rx._
import za.jwatson.glycanoweb.GlycanoWeb
import za.jwatson.glycanoweb.render.Convention.UCT
import za.jwatson.glycanoweb.render.GlycanoCanvas.TempBond
import za.jwatson.glycanoweb.structure.Residue.Link
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js

class GlycanoCanvas(canvas: HTMLCanvasElement) {
  val graph = new Graph
  val residues = Var[Set[Residue]](Set.empty)
  val bonds = Var[Set[Residue]](Set.empty)

  val scope = new p.PaperScope()
  scope.setup(canvas)
  val convention = rx.Var[Convention](new UCT(scope))
  val o = rx.Obs(convention, skipInitial = true) {
    //todo: conversion between uct/cfg/oxford
  }


  def addResidue(ano: Anomer, abs: Absolute, rt: ResidueType, pos: p.Point): Unit = {
    val residue = Residue(rt, ano, abs)
    graph += residue
    convention().addResidue(residue, pos)
    residues() += residue
  }

  def removeResidue(residue: Residue): Unit = {
    graph -= residue
    convention().removeResidue(residue)
    residues() -= residue
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
      val newSelection = graph.residues.filter(convention().testSelection(rect, _)).toSet
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

  Obs(tempBonds)(convention() tempBond tempBonds())

  def beginBond(residue: Residue, point: p.Point): Unit = {
    for(Link(p, i) <- graph.parent(residue)) {
      graph.removeBond(residue)
      convention().removeBond(residue)
      bonds() -= residue
      if(i == 1) convention().removeBond(p)
    }
    tempBonds() = Seq(TempBond(Link(residue, 1), point))
  }
  
  def updateBondSource(point: p.Point): Unit = {
    for(TempBond(Link(from, i), to) <- tempBonds()) {
      if(i == 1) {

      }
      convention().getClosestLink(point, parent = true, angle = 0)
    }
    tempBonds() = tempBonds()
  }

  def updateBondTarget(point: p.Point): Unit = {
    tempBonds() = tempBonds().map(_.copy(to = point))
  }

  def endBond(): Unit = {
    for {
      TempBond(fromLink @ Link(from, fromP), toPoint) <- tempBonds()
      Link(to, p) <- convention().finishBond(fromLink, toPoint)
      if from != to
    } {
      if(p == 1) {
        for(Link(toParent, toTarget) <- graph.removeBond(to)) {
          convention().removeBond(to)
          bonds() -= to
          if(toTarget == 1) {
            convention().removeBond(toParent)
            bonds() -= toParent
          }
        }
      } else {
        for(oldFrom <- graph.child(to, p)) {
          graph.removeBond(oldFrom)
          convention().removeBond(oldFrom)
          bonds() -= oldFrom
        }
      }

      graph.addBond(from, to, p)
      convention().addBond(from, to, p)
      bonds() += from
    }
    tempBonds() = None
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
      b <- r +: (graph.children(r).values.toSeq
        ++ graph.parent(r).filter(_.position == 1).map(_.residue).toSeq)
    } yield b

    for {
      from <- affectedBonds
      Link(to, i) <- graph.parent(from)
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
    GlycanoWeb.residueType() = None
  }

  def mouseDown(e: MouseEvent): Unit = {
//    {
//      import scalaz._, Scalaz._
//
//      val mouseUp = State[InputState, Unit] {
//        case BoxSelect(down) => (Default, ())
//        case s => (s, ())
//      }
//      mouseUp.exec(state())
//      val up = for {
//        a <- gets[InputState, List[Int]](s => List(1, 2, 3))
//        b <- mouseUp
//      } yield b
//      up
//    }
    
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
            for(t <- residueTemplate())
              addResidue(t.anomer, t.absolute, t.rt, point)
          case 2 =>
            cancelPlace()
        }
      case BoxSelect(_) =>
      case Drag(_) =>
      case CreateBond =>
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
        convention().highlightLink(None)
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
          link <- convention().getClosestLink(tb.to)
        } yield link)
      case PostCreateBond =>
      case Hit(down, _) =>
        state() = Drag(down)
    }
  }
}

object GlycanoCanvas {
  case class TempBond(from: Link, to: p.Point)
  case class Selected(residue: Residue, offset: p.Point)
}