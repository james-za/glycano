package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.{Getter, Lens}
import monocle.macros.{Lenser, Lenses}
import monocle.Monocle._
import org.scalajs.dom
import za.jwatson.glycanoweb.GlyAnnot
import za.jwatson.glycanoweb.react.GlycanoCanvas.View
import za.jwatson.glycanoweb.react.bootstrap.{GlyphIcon, Button, FormInput, NavbarHeader}
import za.jwatson.glycanoweb.render.{SubstituentShape, DisplayConv}
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scala.util.{Success, Failure, Try}
import scalaz.State
import scalaz.effect.IO

object GlycanoApp {
  case class Props(conventions: Map[String, DisplayConv])

  @Lenses case class AppState(
    undoPosition: Int = 0,
    history: Vector[RGraph] = Vector(RGraph()),
    selection: (Set[ResidueId], Set[AnnotId]) = (Set.empty, Set.empty),
    placeAnomer: Anomer = Anomer.Alpha, placeAbsolute: Absolute = Absolute.D,
    bondLabels: Boolean = false,
    view: View = View(),
    buffer: RGraph = RGraph(),
    mode: Mode = Mode.Selection,
    displayConv: DisplayConv = DisplayConv.convUCT,
    scaleSubstituents: Double = 1.0,
    limitUpdateRate: Boolean = true,
    annotationFontSize: Double = 24
  )

  sealed trait Mode
  object Mode {
    case object Selection extends Mode
    case class PlaceResidue(residue: Residue) extends Mode
    case class PlaceSubstituent(st: SubstituentType) extends Mode
    case object PlaceAnnotation extends Mode
  }

  object AppStateL {
    def graph(s: AppState): RGraph = s.history(s.undoPosition)
    def setGraph(g: RGraph)(s: AppState): AppState = {
      val s2 = AppState.history.modify(g +: _.drop(s.undoPosition).take(50))(s)
      AppState.undoPosition.set(0)(s2)
    }
    def modGraph(f: RGraph => RGraph)(s: AppState): AppState = setGraph(f(graph(s)))(s)
    val graphL = Lens[AppState, RGraph](graph)(setGraph)
  }

  def removeSelection(sel: (Set[ResidueId], Set[AnnotId])) = (sel._1.foldLeft(_: RGraph)(_ - _)) andThen (sel._2.foldLeft(_: RGraph)(_ - _))

  val copyS = for {
    sel <- State.gets((_: AppState).selection)
    g <- State.gets(AppStateL.graph)
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
      val old = AppStateL.graph(s)
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

  class Backend(t: BackendScope[Props, AppState]) {
    def modState(mod: AppState => AppState): Unit = {
      t.modState(mod)
    }

    def toggleBondLabels(): Unit = {
      t.modState(AppState.bondLabels.modify(bl => !bl))
    }

    def zoomIn(): Unit = t.modState(AppState.view ^|-> View.scale modify (_ * 1.1))
    def zoomOut(): Unit = t.modState(AppState.view ^|-> View.scale modify (_ * (1.0 / 1.1)))
    def zoomReset(): Unit = t.modState(AppState.view ^|-> View.scale set 1.0)
    //def zoomWheel(e: ReactWheelEvent): Unit = t.modState(State.view ^|-> View.scale modify (_ + e.deltaY(e.nativeEvent)))

    def cut(): Unit = t.modState(cutS.exec)
    def copy(): Unit = t.modState(copyS.exec)
    def paste(): Unit = t.modState(pasteS.exec)
    def delete(): Unit = t.modState(deleteS.exec)

    def clearAll(): Unit = t.modState(AppStateL setGraph RGraph())

    def scaleSubstituentsSlider(): Unit = scaleSubstituents("ssSlider")
    def scaleSubstituentsNumber(): Unit = scaleSubstituents("ssNumber")
    def scaleSubstituents(ref: String): Unit = {
      for (input <- t.refs[dom.html.Input](ref)) {
        val scale = Try(input.getDOMNode().value.toDouble).getOrElse(1.0)
        t.modState(AppState.scaleSubstituents set scale)
      }
    }

    def toggleLimitUpdateRate(): Unit = {
      t.modState(AppState.limitUpdateRate modify { v => !v })
    }

    val resizeFunc: js.Function1[dom.Event, Unit] = (e: dom.Event) => resize()

    def resize(): Unit = for (p <- Ref[dom.html.Div]("canvaspanel")(t)) {
      val rect = p.getDOMNode().getBoundingClientRect()
      val setw = AppState.view ^|-> View.width set rect.width.toInt + 1
      val seth = AppState.view ^|-> View.height set (dom.window.innerHeight - rect.top.toInt - 25 - 1)
      t.modState(setw andThen seth)
    }
  }

  val testGraph = {
    val r1 @ (rId1, _) = ResidueId.next() -> GraphEntry(Residue(Anomer.Alpha, Absolute.D, ResidueType.Glc), x = 50, y = 100)
    val r2 @ (rId2, _) = ResidueId.next() -> GraphEntry(Residue(Anomer.Beta, Absolute.D, ResidueType.Man), x = 350, y = 50)
    val r3 = ResidueId.next() -> GraphEntry(Residue(Anomer.Alpha, Absolute.L, ResidueType.Ido, Map(3 -> Vector(SubstituentType.cooh, SubstituentType.n))), x = 200, y = 300, rotation = 45)
    RGraph(residues = Map(r1, r2, r3)) + Bond(rId1, Link(rId2, 2))
  }

  def bondStatus(bond: Bond, csf: ComponentStateFocus[RGraph])(implicit g: RGraph, dc: DisplayConv): TagMod = {
    for {
      ge1 <- g.residues.get(bond.from)
      ge2 <- g.residues.get(bond.to.r)
    } yield <.div(^.cls := "row")(
      <.div(^.cls := "col-xs-6")(
        residueStatus(ge1.residue),
        <.svg.svg(
          ^.display.`inline-block`,
          ^.svg.width := 40.px,
          ^.svg.height := 30.px
        )(SVGBond(SVGBond.Props(ge1.residue.ano, None, (0, 15), (40, 15)))),
        residueStatus(ge2.residue)
      ),
      <.div(^.cls := "col-xs-2")(
        <.p(s"${ge1.residue.ano.desc}-${bond.to.position}")
      ),
      <.div(^.cls := "col-xs-4")(
        <.button(^.cls := "btn btn-link", ^.onClick ~~> csf.modStateIO(_ - bond))("remove bond")
      )
    ): TagMod
  }

  def residueStatus(r: Residue)(implicit dc: DisplayConv) = {
    val (residue, handle) = dc.shapes(r)
    val (_, w, h) = dc.boundsMemo(r)
    <.svg.svg(
      ^.display.`inline-block`,
      ^.svg.width := 40.px,
      ^.svg.height := 30.px,
      ^.svg.viewBox := s"0 0 $w $h"
    )(<.svg.g(residue, handle))
  }

  def subStatus(id: ResidueId, i: Int, j: Int, st: SubstituentType, csf: ComponentStateFocus[RGraph]) = {
    val (sub, (w, h)) = SubstituentShape(st)
    <.div(^.cls := "row")(
      <.div(^.cls := "col-xs-3")(
        <.svg.svg(
          ^.display.`inline-block`,
          ^.svg.height := 30.px,
          ^.svg.viewBox := s"0 0 $w, $h"
        )(sub)
      ),
      <.div(^.cls := "col-xs-5")(s"$i-${st.symbol}"),
      <.div(^.cls := "col-xs-4")(
        <.button(^.cls := "btn btn-link", ^.onClick ~~> csf.modStateIO(_ - (Link(id, i), j)))("remove")
      )
    )
  }

  def apply(props: Props, children: ReactNode*) = component(props, children)
  val component = ReactComponentB[Props]("GlycanoApp")
    .initialStateP(P => AppState(
      history = Vector(testGraph),
      displayConv = P.conventions.getOrElse("UCT", DisplayConv.convDefault)))
    .backend(new Backend(_))
    .render($ => {
      implicit val g: RGraph = AppStateL.graph($.state)
      implicit val dc: DisplayConv = $.state.displayConv

      val rtTemplate = $.state.mode match {
        case Mode.PlaceResidue(res) => Some(res.rt)
        case _ => None
      }

      val (rs, as) = $.state.selection
      val graph = AppStateL.graphL.get($.state)
      val rsel = graph.residues.filterKeys(rs.contains)
      val asel = graph.annotations.filterKeys(as.contains)

      <.div(^.cls := "container-fluid")(
        <.div(^.cls := "row")(Navbar(ExternalVar.state($.focusStateId))),

        <.div(^.cls := "row")(
          <.div(^.cls := "col-xs-3")(
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(
              ResiduePanel(ResiduePanel.Props(
                $.state.displayConv,
                $.state.placeAnomer,
                $.state.placeAbsolute,
                rtTemplate,
                $.backend.modState
              ))
            )),
            <.div(^.cls := "row")(
              <.div(^.cls := "col-xs-8")(
                <.input(
                  ^.cls := "form-control",
                  ^.ref := "ssSlider",
                  ^.`type` := "range",
                  "min".reactAttr := 0.1,
                  "max".reactAttr := 2.0,
                  ^.step := 0.01,
                  ^.value := $.state.scaleSubstituents,
                  ^.onChange --> $.backend.scaleSubstituentsSlider
                )
              ),
              <.div(^.cls := "col-xs-4")(
                <.input(
                  ^.cls := "form-control",
                  ^.ref := "ssNumber",
                  ^.`type` := "number",
                  ^.value := $.state.scaleSubstituents,
                  ^.onChange --> $.backend.scaleSubstituentsNumber
                )
              )
            ),
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(
              SubstituentPanel(SubstituentPanel.Props(
                ExternalVar.state($.focusStateL(AppState.mode)),
                $.state.scaleSubstituents
              ))
            )),
            <.div(^.cls := "row")(
              <.div(^.cls := "checkbox")(
                <.label(
                  <.input(
                    ^.`type` := "checkbox",
                    ^.checked := $.state.limitUpdateRate,
                    ^.onChange --> $.backend.toggleLimitUpdateRate
                  ),
                  "Limit Update Rate"
                )
              )
            )
          ),
          <.div(^.cls := "col-xs-6")(
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(
              CASPERDisplay(CASPERDisplay.Props($.state.history($.state.undoPosition), $.state.selection._1))
            )),
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(
              <.div(^.cls := "panel panel-default")(
                <.div(^.cls := "panel-body", ^.ref := "canvaspanel", ^.padding := 0.px)(
                  GlycanoCanvas(GlycanoCanvas.Props(
                    mode = ExternalVar.state($.focusStateL(AppState.mode)),
                    dc = $.state.displayConv,
                    graph = ExternalVar.state($.focusStateL(AppStateL.graphL)),
                    selection = ExternalVar.state($.focusStateL(AppState.selection)),
                    view = ExternalVar.state($.focusStateL(AppState.view)),
                    bondLabels = $.state.bondLabels,
                    scaleSubstituents = $.state.scaleSubstituents,
                    limitUpdateRate = $.state.limitUpdateRate,
                    annotationFontSize = $.state.annotationFontSize
                  ))
                )
              )
            ))
          ),
          <.div(^.cls := "col-xs-3")(
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(
              <.div(^.cls := "panel panel-default")(
                <.div(^.cls := "panel-body")(
                  rsel.toList match {
                    case Nil => ""
                    case (id, ge) :: Nil =>
                      val csf = $.focusStateL(AppStateL.graphL)
                      val first = for (link <- ge.parent.toSeq) yield bondStatus(Bond(id, link), csf)
                      val rest = for ((i, from) <- ge.children.toSeq) yield bondStatus(Bond(from, Link(id, i)), csf)
                      val subs = for {
                        (i, stack) <- ge.residue.subs.toSeq
                        (st, j) <- stack.zipWithIndex
                      } yield subStatus(id, i, j, st, csf)
                      first ++ rest ++ subs
                    case resList =>
                      for ((id, ge) <- resList) yield {
                        <.div(^.cls := "row")(
                          <.div(^.cls := "col-xs-12")(s"${ge.residue.desc}")
                        )
                      }
                  }
                )
              ),
              <.div(^.cls := "panel panel-default")(
                <.div(^.cls := "panel-body")(
                  for (_ <- asel.headOption) yield {
                    val annotationText = asel.map(_._2.text).reduce[String] {
                      case (text1, text2) =>
                        if (text1 == text2) text1 else ""
                    }
                    val fontSize = asel.map(_._2.size.toString).reduce[String] {
                      case (size1, size2) =>
                        if (size1 == size2) size1 else ""
                    }

                    <.div(
                      <.div(^.cls := "form-group input-group")(
                        "Text", <.input(
                          ^.cls := "form-control",
                          ^.`type` := "text",
                          ^.value := annotationText,
                          ^.onChange ~~> ((e: ReactEventI) => for {
                            _ <- e.preventDefaultIO
                            _ <- $.modStateIO(AppStateL.graphL ^|-> RGraph.annotations ^|->> filterIndex(asel.contains) ^|-> Annot.text set e.target.value)
                          } yield ())
                        )
                      ),
                      <.div(^.cls := "form-group input-group")(
                        "Font Size", <.input(
                          ^.cls := "form-control",
                          ^.`type` := "number",
                          ^.value := fontSize,
                          ^.onChange ~~> ((e: ReactEventI) => for {
                            _ <- e.preventDefaultIO
                            _ <- Try(e.target.value.toDouble) match {
                              case Failure(exception) => IO.ioUnit
                              case Success(value) => $.modStateIO(AppStateL.graphL ^|-> RGraph.annotations ^|->> filterIndex(asel.contains) ^|-> Annot.size set value)
                            }
                          } yield ())
                        )
                      )
                    )
                  }
                )
              )
            ))
          )
        )
      )
    })
    .componentDidMount { $ =>
      dom.window.addEventListener("resize", $.backend.resizeFunc)
      $.backend.resize()
    }
    .componentWillUnmount { $ =>
      dom.window.removeEventListener("resize", $.backend.resizeFunc)
    }
    .build
}
