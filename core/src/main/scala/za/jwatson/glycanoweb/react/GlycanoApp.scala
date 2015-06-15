package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.Reusability._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Lens
import monocle.Monocle._
import monocle.macros.Lenses
import org.scalajs.dom
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.SVGRect
import za.jwatson.glycanoweb.Gly
import za.jwatson.glycanoweb.react.GlycanoCanvas.{Bounds, View}
import za.jwatson.glycanoweb.react.semantic._
import za.jwatson.glycanoweb.render.{DisplayConv, SubstituentShape}
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

import scala.reflect.macros.whitebox
import scala.scalajs.js
import scala.util.{Failure, Success, Try}
import scalaz.State
import scalaz.effect.IO

object GlycanoApp {
  case class Props(conventions: Map[String, DisplayConv])

  object Props {
    implicit val reusability: Reusability[Props] = Reusability.by_==
  }

  @Lenses case class AppState(
    undoPosition: Int = 0,
    history: Vector[RGraph] = Vector(RGraph()),
    selection: (Set[ResidueId], Set[AnnotId]) = (Set.empty, Set.empty),
    highlightBond: Option[ResidueId] = None,
    placeAnomer: Anomer = Anomer.Alpha, placeAbsolute: Absolute = Absolute.D,
    bondLabels: Boolean = false,
    view: View = View(),
    buffer: RGraph = RGraph(),
    mode: Mode = Mode.Selection,
    displayConv: DisplayConv = DisplayConv.convUCT,
    scaleSubstituents: Double = 1.0,
    limitUpdateRate: Boolean = false,
    annotationFontSize: Double = 24,
    bounds: Option[Bounds] = None,
    showGrid: Boolean = false,
    gridWidth: Double = 10.0,
    snapToGrid: Boolean = false,
    snapRotation: Boolean = false,
    snapRotationDegrees: Double = 15.0
  ) {
    def graph: RGraph = history(undoPosition)
  }

  sealed trait Mode
  object Mode {
    case object Selection extends Mode
    @Lenses case class PlaceResidue(residue: Residue) extends Mode
    case class PlaceSubstituent(st: SubstituentType) extends Mode
    case object PlaceAnnotation extends Mode

    implicit val reusability: Reusability[Mode] = Reusability.by_==
  }

  def saveGraph(graph: RGraph): Unit = {
    import Gly._
    import upickle._
    LocalStorage("glycano.temp") = write[Gly](Gly.from(graph))
  }

  def loadGraph(): RGraph = {
    import Gly._
    import upickle._
    val graph = for {
      str <- LocalStorage("glycano.temp")
      gly <- Try(read[Gly](str)).toOption
    } yield gly.toRGraph
    graph.getOrElse(RGraph())
  }

  object AppStateL {
    def setGraph(g: RGraph)(s: AppState): AppState = {
      val s2 = AppState.history.modify(g +: _.drop(s.undoPosition).take(50))(s)
      saveGraph(g)
      AppState.undoPosition.set(0)(s2)
    }
    def modGraph(f: RGraph => RGraph)(s: AppState): AppState = setGraph(f(s.graph))(s)
    val graphL = Lens[AppState, RGraph](_.graph)(setGraph)
  }

  def removeSelection(sel: (Set[ResidueId], Set[AnnotId])) = (sel._1.foldLeft(_: RGraph)(_ - _)) andThen (sel._2.foldLeft(_: RGraph)(_ - _))

  val copyS = for {
    sel <- State.gets((_: AppState).selection)
    g <- State.gets((_: AppState).graph)
    dr = g.residues.keySet diff sel._1
    da = g.annotations.keySet diff sel._2
    _ <- State.modify(AppState.buffer set removeSelection(dr, da)(g))
  } yield ()

  val deleteS = for {
    sel <- State.gets((_: AppState).selection)
    _ <- State.modify(AppStateL.modGraph(removeSelection(sel)))
  } yield ()

  val cutS = for {
    _ <- copyS
    _ <- deleteS
  } yield ()

  val offsetAmount = 20
  val offsetBufferResidues = RGraph.residues ^|->> each modify {
    (GraphEntry.x modify (_ + offsetAmount)) andThen (GraphEntry.y modify (_ + offsetAmount))
  }
  val offsetBufferAnnotations = RGraph.annotations ^|->> each modify {
    (Annot.x modify (_ + offsetAmount)) andThen (Annot.y modify (_ + offsetAmount))
  }
  val offsetBuffer = offsetBufferResidues andThen offsetBufferAnnotations
  val pasteS = for {
    buf <- State.gets((_: AppState).buffer)
    gSel <- State.gets { s: AppState =>
      val old = s.graph
      var g = old

      val lookupR = buf.residues.map { case (id, _) => id -> ResidueId.next() }
      val lookupA = buf.annotations.map { case (id, _) => id -> AnnotId.next() }

      for ((r, ge) <- buf.residues) {
        val modChildren = GraphEntry.children ^|->> each modify lookupR
        val modParent = GraphEntry.parent ^<-? some ^|-> Link.r modify lookupR
        g = g &|-> RGraph.residues modify { _ + (lookupR(r) -> (modChildren andThen modParent)(ge)) }
      }

      for ((id, annot) <- buf.annotations) {
        g = g &|-> RGraph.annotations modify { _ + (lookupA(id) -> annot) }
      }

      (g, (lookupR.values.toSet, lookupA.values.toSet))
    }
    _ <- State.modify(AppStateL setGraph gSel._1)
    _ <- State.modify(AppState.selection set gSel._2)
    _ <- State.modify(AppState.buffer modify offsetBuffer)
  } yield ()

  def undo(as: AppState): AppState =
    if (as.undoPosition < as.history.size - 1)
      AppState.undoPosition.modify(_ + 1)(as)
    else as
  def redo(as: AppState): AppState =
    if (as.undoPosition > 0)
      AppState.undoPosition.modify(_ - 1)(as)
    else as

  class Backend($: BackendScope[Props, AppState]) extends OnUnmount {
    def modState(mod: AppState => AppState): Unit = {
      $.modState(mod)
    }

    def toggleBondLabels(): Unit = {
      $.modState(AppState.bondLabels.modify(bl => !bl))
    }

    def zoomIn(): Unit = $.modState(AppState.view ^|-> View.scale modify (_ * 1.1))
    def zoomOut(): Unit = $.modState(AppState.view ^|-> View.scale modify (_ * (1.0 / 1.1)))
    def zoomReset(): Unit = $.modState(AppState.view ^|-> View.scale set 1.0)
    //def zoomWheel(e: ReactWheelEvent): Unit = t.modState(State.view ^|-> View.scale modify (_ + e.deltaY(e.nativeEvent)))

    def cut(): Unit = $.modState(cutS.exec)
    def copy(): Unit = $.modState(copyS.exec)
    def paste(): Unit = $.modState(pasteS.exec)
    def delete(): Unit = $.modState(deleteS.exec)

    def clearAll(): Unit = $.modState(AppStateL setGraph RGraph())

    def scaleSubstituentsSlider(): Unit = scaleSubstituents("ssSlider")
    def scaleSubstituentsNumber(): Unit = scaleSubstituents("ssNumber")
    def scaleSubstituents(ref: String): Unit = {
      for (input <- $.refs[dom.html.Input](ref)) {
        val scale = Try(input.getDOMNode().value.toDouble).getOrElse(1.0)
        $.modState(AppState.scaleSubstituents set scale)
      }
    }

    def toggleLimitUpdateRate(): Unit = {
      $.modState(AppState.limitUpdateRate modify { v => !v })
    }
    val refCanvas = Ref[dom.html.Div]("canvas")
    val refPage = Ref[dom.html.Div]("page")
    val resizeIO = IO(for (canvas <- refCanvas($); page <- refPage($)) {
      val nodeCanvas = canvas.getDOMNode()
      val nodePage = page.getDOMNode()
      val surrounding = nodePage.offsetHeight - nodeCanvas.clientHeight
      val height = dom.window.innerHeight - surrounding
      val setWidth = AppState.view ^|-> View.width set (nodeCanvas.clientWidth - 1).toInt
      val setHeight = AppState.view ^|-> View.height set height.toInt
      $.modState(setWidth andThen setHeight)
    })

    val keydownFunc: js.Function1[dom.KeyboardEvent, Unit] = keyDown _

    def keyDown(e: dom.KeyboardEvent): Unit = {

      val shift = e.asInstanceOf[js.Dynamic].shiftKey.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
      val ctrl = e.asInstanceOf[js.Dynamic].ctrlKey.asInstanceOf[js.UndefOr[Boolean]]
      val meta = e.asInstanceOf[js.Dynamic].metaKey.asInstanceOf[Boolean]
      val mod = meta || ctrl.getOrElse(false)
      e.keyCode match {
        case 46 =>
          delete()
        case 27 | 32 =>
          $.modState(AppState.mode set Mode.Selection)
        case 88 /*X*/ if mod =>
          cut()
        case 67 /*C*/ if mod =>
          copy()
        case 86 /*V*/ if mod =>
          paste()
        case 90 /*Z*/ if mod =>
          $.modState(if (shift) redo else undo)
        case 89 /*Y*/ if mod =>
          $.modState(redo)
        case _ =>
      }
    }

    val setAppStateFn: AppState ~=> IO[Unit] = ReusableFn($.setStateIO(_))
    val setModeFn: Mode ~=> IO[Unit] = ReusableFn($._setStateL(AppState.mode))
    val setGraphFn: RGraph ~=> IO[Unit] = ReusableFn($._setStateL(AppStateL.graphL))
    val setHighlightBondFn: Option[ResidueId] ~=> IO[Unit] = ReusableFn($._setStateL(AppState.highlightBond))
    val setDisplayConvFn: Option[DisplayConv] ~=> IO[Unit] = ReusableFn(_.fold(IO.ioUnit)($._setStateL(AppState.displayConv)))
    val setAnomerFn: Anomer ~=> IO[Unit] = ReusableFn($._setStateL(AppState.placeAnomer))
    val setAbsoluteFn: Absolute ~=> IO[Unit] = ReusableFn($._setStateL(AppState.placeAbsolute))
  }


  val C = ReactComponentB[Props]("GlycanoApp")
    .initialStateP(props => AppState(
      history = Vector(loadGraph()),
      displayConv = props.conventions.getOrElse("UCT", DisplayConv.convDefault)
    ))
    .backend(new Backend(_))
    .render { $ =>
      implicit val g: RGraph = $.state.graph
      implicit val dc: DisplayConv = $.state.displayConv

      val rvAppStateNavbar = $.backend.setAppStateFn.asVarR($.state, Navbar.reuseAppState)
      val rvAppStateCanvas = $.backend.setAppStateFn.asVarR($.state, GlycanoCanvas.reuseAppState)

      val rtTemplate = $.state.mode match {
        case Mode.PlaceResidue(res) => Some(res.rt)
        case _ => None
      }

      val rvGraph = $.backend.setGraphFn.asVar($.state.graph)
      val rvHighlightBond = $.backend.setHighlightBondFn.asVar($.state.highlightBond)
      val rvDisplayConv = $.backend.setDisplayConvFn.asVar(Some($.state.displayConv))
      val rvAnomer = $.backend.setAnomerFn.asVar($.state.placeAnomer)
      val rvAbsolute = $.backend.setAbsoluteFn.asVar($.state.placeAbsolute)
      val rvMode = $.backend.setModeFn.asVar($.state.mode)

      <.div(^.ref := "page")(
        MainMenu.C(rvGraph),
        div"ui padded grid"(^.padding := 0.5.rem)(
          div"sixteen wide column"(
            ToolBar.C(rvGraph)
          ),
          div"row"(
            div"four wide column"(
              ConventionPanel.C(rvDisplayConv),
              div"ui segment"(),
              div"ui segment"()
            ),
            div"eight wide column"(
              div"ui top attached segment"(
                div"ui fluid input"(<.input(^.tpe := "text", ^.ref := "casper"))
              ),
              div"ui attached segment"(^.padding := 0, ^.ref := "canvas", ^.fontSize := 0)(
                GlycanoCanvas.C(rvAppStateCanvas)
              ),
              div"ui bottom attached segment"(
                div"ui right floated input"(
                  <.input(^.tpe := "text", ^.ref := "zoom")
                )
              )
            ),
            div"four wide column"()
          )
        )
      )

//      div"container-fluid"(
//        div"row"(Navbar.C(rvAppStateNavbar)),
//
//        div"row"(
//          div"col-xs-3"(
//            div"row"(div"col-xs-12 text-center"(
//              RadioDisplayConv(RadioGroupMap.Props[DisplayConv](
//                rvDisplayConv,
//                DisplayConv.conventions.values.toSeq,
//                $.backend.getNameDisplayConvFn,
//                toggle = false
//              ))
//            )),
//            div"row"(div"col-xs-12"(
//              ResiduePanel.C(ResiduePanel.Props(rvAnomer, rvAbsolute, rvMode, dc, $.state.scaleSubstituents, $.props.conventions))
//            )),
//            div"row"(
//              div"col-xs-8"(
//                <.input(
//                  c"form-control",
//                  ^.ref := "ssSlider",
//                  ^.`type` := "range",
//                  "min".reactAttr := 0.1,
//                  "max".reactAttr := 2.0,
//                  ^.step := 0.01,
//                  ^.value := $.state.scaleSubstituents,
//                  ^.onChange --> $.backend.scaleSubstituentsSlider
//                )
//              ),
//              div"col-xs-4"(
//                <.input(
//                  c"form-control",
//                  ^.ref := "ssNumber",
//                  ^.`type` := "number",
//                  ^.value := $.state.scaleSubstituents,
//                  ^.onChange --> $.backend.scaleSubstituentsNumber
//                )
//              )
//            ),
//            div"row"(div"col-xs-12"(
//              SubstituentPanel((
//                rvMode,
//                $.state.scaleSubstituents
//              ))
//            )),
//            div"row"(
//              div"checkbox"(
//                <.label(
//                  <.input(
//                    ^.`type` := "checkbox",
//                    ^.checked := $.state.limitUpdateRate,
//                    ^.onChange --> $.backend.toggleLimitUpdateRate
//                  ),
//                  "Limit Update Rate"
//                )
//              )
//            )
//          ),
//          div"col-xs-6"(
//            div"row"(div"col-xs-12"(
//              CASPERDisplay(CASPERDisplay.Props($.state.history($.state.undoPosition), $.state.selection._1))
//            )),
//            div"row"(div"col-xs-12"(
//              div"panel panel-default"(
//                <.div(
//                  c"panel-body",
//                  ^.ref := "canvaspanel",
//                  ^.padding := 0.px
//                )(
//                  GlycanoCanvas(rvAppStateCanvas)
//                )
//              )
//            ))
//          ),
//          div"col-xs-3"(
//            OverviewPanel.C((rvGraph, $.state.selection, $.state.displayConv, rvHighlightBond))
//          )
//        )
//      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate(implicitly, Reusability.by_==))
    .configure(EventListener.installIO("resize", _.backend.resizeIO, _ => dom.window))
//    .configure(EventListener[dom.KeyboardEvent].installIO("keydown", $ => e => IO($.backend.keyDown(e))))
    .componentDidMountIO(_.backend.resizeIO)
    .build
}
