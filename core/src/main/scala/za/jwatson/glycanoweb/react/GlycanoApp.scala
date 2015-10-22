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
import org.scalajs.dom.EventTarget
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.raw.SVGRect
import za.jwatson.glycanoweb.Gly
import za.jwatson.glycanoweb.react.GlycanoCanvas.{Bounds, View}
import za.jwatson.glycanoweb.react.bootstrap._
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

    def doCopy = {
      val copiedResidues = graph.residues.filterKeys(selection._1.contains)
      val copiedAnnotations = graph.annotations.filterKeys(selection._2.contains)
      this &|-> AppState.buffer set RGraph(copiedResidues, copiedAnnotations)
    }

    def doDelete = {
      val remainingResidues = graph.residues.filterKeys(r => !selection._1.contains(r))
      val remainingAnnotations = graph.annotations.filterKeys(a => !selection._2.contains(a))
      this &|-> AppStateL.graphL set RGraph(remainingResidues, remainingAnnotations)
    }

    def doCut = this.doCopy.doDelete

    private val offsetAmount = 20
    private val offsetBufferResidues = RGraph.residues ^|->> each modify {
      (GraphEntry.x modify (_ + offsetAmount)) andThen (GraphEntry.y modify (_ + offsetAmount))
    }
    private val offsetBufferAnnotations = RGraph.annotations ^|->> each modify {
      (Annot.x modify (_ + offsetAmount)) andThen (Annot.y modify (_ + offsetAmount))
    }
    private val offsetBuffer = offsetBufferResidues andThen offsetBufferAnnotations

    def doPaste = {
      val buf = offsetBuffer(buffer)
      val lookupR = buf.residues.map { case (id, _) => id -> ResidueId.next() }
      val lookupA = buf.annotations.map { case (id, _) => id -> AnnotId.next() }

      val modChildren = GraphEntry.children ^|->> each modify lookupR
      val modParent = GraphEntry.parent ^<-? some ^|-> Link.r modify lookupR
      val updateLinks = modChildren andThen modParent

      val addedResidues = for ((r, ge) <- buf.residues) yield lookupR(r) -> updateLinks(ge)
      val addedAnnotations = for ((id, a) <- buf.annotations) yield lookupA(id) -> a

      val updateBuffer = AppState.buffer set buf
      val old = graph
      val pasteBuffer = AppStateL.graphL set RGraph(old.residues ++ addedResidues, old.annotations ++ addedAnnotations)
      val updateSelection = AppState.selection set (addedResidues.keySet, addedAnnotations.keySet)

      (updateBuffer andThen pasteBuffer andThen updateSelection)(this)
    }
  }

  sealed trait Mode
  object Mode {
    case object Selection extends Mode
    @Lenses case class PlaceResidue(residue: Residue) extends Mode
    case class PlaceSubstituent(st: SubstituentType) extends Mode
    case object PlaceAnnotation extends Mode
    case object CreateBond extends Mode
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

  def undo(as: AppState): AppState =
    if (as.undoPosition < as.history.size - 1)
      AppState.undoPosition.modify(_ + 1)(as)
    else as
  def redo(as: AppState): AppState =
    if (as.undoPosition > 0)
      AppState.undoPosition.modify(_ - 1)(as)
    else as

  class Backend($: BackendScope[Props, AppState]) extends OnUnmount {
    def toggleBondLabels(): Unit = {
      $.modState(AppState.bondLabels.modify(bl => !bl))
    }

    def zoomIn(): Unit = $.modState(AppState.view ^|-> View.scale modify (_ * 1.1))
    def zoomOut(): Unit = $.modState(AppState.view ^|-> View.scale modify (_ * (1.0 / 1.1)))
    def zoomReset(): Unit = $.modState(AppState.view ^|-> View.scale set 1.0)
    //def zoomWheel(e: ReactWheelEvent): Unit = t.modState(State.view ^|-> View.scale modify (_ + e.deltaY(e.nativeEvent)))

//    def cut(): Unit = $.modState(cutS.exec)
//    def copy(): Unit = $.modState(copyS.exec)
//    def paste(): Unit = $.modState(pasteS.exec)
//    def delete(): Unit = $.modState(deleteS.exec)

    def clearAll = $.modState(AppStateL setGraph RGraph())

    def scaleSubstituentsSlider = scaleSubstituents("ssSlider")
    def scaleSubstituentsNumber = scaleSubstituents("ssNumber")
    def scaleSubstituents(ref: String) = {
      for {
        input: dom.html.Input <- CallbackOption.liftOptionLike(Ref[dom.html.Input](ref)($))
        scale = Try(input.value.toDouble).getOrElse(1.0)
        _ <- $.modState(AppState.scaleSubstituents set scale).toCBO
      } yield ()
    }

    val toggleLimitUpdateRate = $.modState(AppState.limitUpdateRate modify { v => !v })

    val refCanvasPanel = Ref[dom.html.Div]("canvaspanel")
    val refCanvasColumn = Ref[dom.html.Div]("canvascolumn")

    val resizeCB = for {
      canvas <- CallbackOption.liftOptionLike(refCanvasPanel($))
      canvasOuter <- CallbackOption.liftOptionLike(refCanvasColumn($))
      _ <- $.modState {
        val bottom = canvasOuter.getBoundingClientRect().bottom
        val surrounding = bottom - canvas.clientHeight
        val height = (dom.window.innerHeight - surrounding).max(0)
        val setWidth = AppState.view ^|-> View.width set (canvas.clientWidth - 1).toInt
        val setHeight = AppState.view ^|-> View.height set height.toInt
        setWidth andThen setHeight
      }
    } yield ()

    def keydownCB(e: dom.KeyboardEvent) = CallbackOption.matchPF(e.keyCode) {
      case 46 => $.modState(_.doDelete)
      case 27 | 32 => $.modState(AppState.mode set Mode.Selection)
      case 88 /*X*/ if e.ctrlKey || e.metaKey => $.modState(_.doCut)
      case 67 /*C*/ if e.ctrlKey || e.metaKey => $.modState(_.doCopy)
      case 86 /*V*/ if e.ctrlKey || e.metaKey => $.modState(_.doPaste)
      case 90 /*Z*/ if e.ctrlKey || e.metaKey => $.modState(if (e.shiftKey) redo else undo)
      case 89 /*Y*/ if e.ctrlKey || e.metaKey => $.modState(redo)
    }.flatMap(_.toCBO)

    val setAppStateFn = ReusableFn($).setState

    val setDisplayConvFn: Option[DisplayConv] ~=> Callback = ReusableFn(_.fold(Callback.empty)($._setStateL(AppState.displayConv)))
    val getNameDisplayConvFn: DisplayConv ~=> String = ReusableFn(_.name)

    def render(p: Props, s: AppState) = {
      def rvAppState(r: Reusability[AppState]): ReusableVar[AppState] = setAppStateFn.asVarR(s, r)

      implicit val g: RGraph = s.graph
      implicit val dc: DisplayConv = s.displayConv

      val rvAppStateToolBar = rvAppState(ToolBar.reuseAppState)
      val rvAppStateCanvas = rvAppState(GlycanoCanvas.reuseAppState)

      val rtTemplate = s.mode match {
        case Mode.PlaceResidue(res) => Some(res.rt)
        case _ => None
      }

      val rvDisplayConv = setDisplayConvFn.asVar(Some(dc))

      div"container-fluid"(
        div"row"(Navbar.C(rvAppState(Navbar.reuseAppState))),
        ToolBar.C(rvAppStateToolBar),
        div"row"(
          div"col-xs-3"(
            div"row"(div"col-xs-12 text-center"(^.marginBottom := 20.px)(
              RadioDisplayConv(RadioGroupMap.Props[DisplayConv](
                rvDisplayConv,
                DisplayConv.conventions.values.toSeq,
                getNameDisplayConvFn,
                toggle = false
              ))
            )),
            div"row"(div"col-xs-12"(
              ResiduePanel.C(ResiduePanel.Props(
                rvAppState(ResiduePanel.reuseAppState),
                p.conventions
              ))
            )),
            div"row"(^.marginBottom := 5.px)(
              div"col-xs-8"(
                <.input(
                  c"form-control",
                  ^.ref := "ssSlider",
                  ^.`type` := "range",
                  "min".reactAttr := 0.1,
                  "max".reactAttr := 2.0,
                  ^.step := 0.01,
                  ^.value := s.scaleSubstituents,
                  ^.onChange --> scaleSubstituentsSlider
                )
              ),
              div"col-xs-4"(
                <.input(
                  c"form-control",
                  ^.ref := "ssNumber",
                  ^.`type` := "number",
                  ^.value := s.scaleSubstituents,
                  ^.onChange --> scaleSubstituentsNumber
                )
              )
            ),
            div"row"(div"col-xs-12"(
              SubstituentPanel.C(rvAppState(SubstituentPanel.reuseAppState))
            )),
            div"row"(
              div"checkbox"(
                <.label(
                  <.input(
                    ^.`type` := "checkbox",
                    ^.checked := s.limitUpdateRate,
                    ^.onChange --> toggleLimitUpdateRate
                  ),
                  "Limit Update Rate"
                )
              )
            )
          ),
          div"col-xs-6"(
            div"row"(div"col-xs-12"(^.ref := "canvascolumn")(
              div"panel panel-default"(
                div"panel panel-header"(^.marginBottom := 0)(
                  CASPERDisplay(CASPERDisplay.Props(s.history(s.undoPosition), s.selection._1))
                ),
                div"panel-body"(
                  ^.ref := "canvaspanel",
                  ^.padding := 0.px, ^.fontSize := 0,
                  ^.borderTop := "1px solid #ddd"
                )(
                  GlycanoCanvas(rvAppStateCanvas)
                ),
                div"panel-footer"(
                  ZoomToolbar.C(rvAppState(ZoomToolbar.reuseAppState))
                )
              )
            ))
          ),
          div"col-xs-3"(
            OverviewPanel.C(rvAppState(OverviewPanel.reuseAppState))
          )
        )
      )
    }
  }

  val RadioDisplayConv = RadioGroupMap[DisplayConv]

  val C = ReactComponentB[Props]("GlycanoApp")
    .initialState_P(props => AppState(
      history = Vector(loadGraph()),
      displayConv = props.conventions.getOrElse("UCT", DisplayConv.convDefault)
    ))
    .renderBackend[Backend]
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate(implicitly, Reusability.by_==))
    .configure(EventListener.install("resize", _.backend.resizeCB, _ => dom.window))
    .configure(EventListener[dom.KeyboardEvent].install("keydown", $ => e => $.backend.keydownCB(e), _ => dom.window))
    .componentDidMount(_.backend.resizeCB)
    .build
}
