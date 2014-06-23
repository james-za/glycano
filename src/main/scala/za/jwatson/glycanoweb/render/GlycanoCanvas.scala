package za.jwatson.glycanoweb.render

import importedjs.{paper => p}
import org.scalajs.dom.{HTMLCanvasElement, MouseEvent}
import org.scalajs.jquery.{jQuery => jQ}
import scalajs.js
import rx._
import za.jwatson.glycanoweb.GlycanoWeb
import za.jwatson.glycanoweb.render.Convention.UCT
import za.jwatson.glycanoweb.render.GlycanoCanvas.TempBond
import za.jwatson.glycanoweb.structure._

class GlycanoCanvas(canvas: HTMLCanvasElement) {
  val residues = Var(Set[Residue]())
  val bonds = collection.mutable.Set[Bond]()
  val scope = new p.PaperScope()
  scope.setup(canvas)
  val convention = rx.Var[Convention](new UCT(scope))
  val o = rx.Obs(convention, skipInitial = true) {
    //todo: conversion between uct/cfg/oxford
  }

  def addResidue(ano: Anomer, abs: Absolute, rt: ResidueType, pos: p.Point): Unit = {
    val residue = Residue(rt, ano, abs)
    convention().addResidue(residue, pos)
    residues() += residue
  }

  def removeResidue(residue: Residue): Unit = {
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

  sealed trait State
  object State {
    case object Default extends State
    case object PlaceResidue extends State
    case class BoxSelect(down: p.Point) extends State
    case class Drag(last: p.Point) extends State
    case object CreateBond extends State
    case object PostCreateBond extends State
    case class Hit(down: p.Point, item: p.Item) extends State
  }
  import State._

  val state = Var[State](Default)

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



  Obs(selection) {
    for(r <- selection()) convention().highlightResidue(r)
  }

  val boxSelect = Var[Option[p.Rectangle]](None)

  Obs(boxSelect) {
    convention().drawBoxSelection(boxSelect())
  }

  def updateBoxSelect(begin: p.Point, end: p.Point): Unit = {
    boxSelect() = Some(new p.Rectangle(begin, end))
    for(rect <- boxSelect()) {
      clearSelection()
      selection() = residues().filter(convention().testSelection(rect, _))
    }
  }

  def endBoxSelect(): Unit = {

    boxSelect() = None
  }

  def clearSelection(): Unit = {
    for (res <- selection()) convention().unhighlightResidue(res)
    selection() = Set.empty[Residue]
  }

  var hitHandle: Boolean = false
  val tempBond = Var[Option[TempBond]](None)

  Obs(tempBond)(convention() tempBond tempBond())

  def beginBond(residue: Residue, point: p.Point): Unit = {
    for(bond <- residue.bonds.get(1)) {
      residue.removeBond(1)
      convention().removeBond(bond)
    }
    tempBond() = Some(TempBond(Link(residue, 1), point))
  }

  def updateBond(point: p.Point): Unit = {
    tempBond() = tempBond().map(tb => TempBond(tb.from, point))
  }

  def endBond(): Unit = {
    for {
      tb <- tempBond()
      linkTo <- convention().finishBond(tb.from, tb.to)
      if tb.from.residue != linkTo.residue
    } {
      val removed = linkTo.residue.removeBond(linkTo.position)
      for(b <- removed) {
        convention().removeBond(b)
      }
      val bond = tb.from.residue.bond(linkTo.residue, linkTo.position, tb.from.position)
      bonds += bond
      convention().addBond(bond)
    }
    tempBond() = None
  }

  val residueTemplate = Rx {
    GlycanoWeb.residueType().map(
      Residue(_, GlycanoWeb.anomeric(), GlycanoWeb.absolute())
    )
  }

  val clearOldTemplate = Obs(residueTemplate) {
    state() = residueTemplate() match {
      case Some(r) => PlaceResidue
      case None => Default
    }
    convention().showResidueTemplate(None, null)
    scope.view.draw()
  }

  def updateSelectionBonds(): Unit = {
    val affectedBonds = (for {
      r <- selection()
      b <- r.bonds.values
    } yield b)(collection.breakOut): Set[Bond]

    affectedBonds foreach convention().updateBond
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

  def mouseDown(e: MouseEvent): Unit = {
    canvas.focus()
    canvas.blur()
    val point = clientToProject(e)
    state() match {
      case Default if e.button == 0 =>
        val hitTest = scope.project.hitTest(point)
        Option(hitTest) match {
          case Some(hit) =>
            for(r <- hit.item.getResidue) {
              if (convention().hitHandle(r, point)) {
                convention().handlePress(Some(r))
                beginBond(r, point)
                convention().handleHL(None)
                state() = CreateBond
              } else {
                state() = Hit(point, hit.item)
                if (!selection().contains(r)) {
                  clearSelection()
                  selection() += r
                }
              }
            }
          case None =>
            updateBoxSelect(point, point)
            state() = BoxSelect(point)
        }
      case PlaceResidue =>
        e.button match {
          case 0 =>
            for(t <- residueTemplate())
              addResidue(t.anomeric, t.absolute, t.rt, point)
          case 2 =>
            GlycanoWeb.residueType() = None
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
        clearSelection()
        for(r <- item.getResidue) {
          selection() += r
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
      case BoxSelect(down) =>
        updateBoxSelect(down, point)
      case Drag(last) =>
        val delta = point subtract last
        moveSelectionBy(delta)
        updateSelectionBonds()
        state() = Drag(point)
      case CreateBond =>
        updateBond(point)
        convention().highlightLink(for {
          tb <- tempBond()
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