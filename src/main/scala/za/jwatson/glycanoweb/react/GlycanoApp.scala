package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.{Getter, Lens}
import monocle.macros.{Lenser, Lenses}
import monocle.Monocle._
import za.jwatson.glycanoweb.GlyAnnot
import za.jwatson.glycanoweb.react.GlycanoCanvas.View
import za.jwatson.glycanoweb.react.bootstrap.{GlyphIcon, Button, FormInput, NavbarHeader}
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

object GlycanoApp {
  case class Props(conventions: Map[String, DisplayConv]/*, historyLimit: Int = 50*/)

  @Lenses case class State(
    undoPosition: Int = 0,
    history: Vector[RGraph] = Vector(RGraph()),
    selection: (Set[Residue], Set[GlyAnnot]) = (Set.empty, Set.empty),
    bondLabels: Boolean = false,
    view: View = View(),
    buffer: RGraph = RGraph(),
    mode: Mode = Mode.Selection,
    displayConv: DisplayConv = DisplayConv.convUCT
  )

  sealed trait Mode
  object Mode {
    case object Selection extends Mode
    case class PlaceResidue(ano: Anomer, abs: Absolute, rt: ResidueType) extends Mode
    case class PlaceSubstituent(st: SubstituentType) extends Mode
    case class PlaceAnnotation(size: Double) extends Mode
  }

  object StateL {
    def graph(s: State): RGraph = s.history(s.undoPosition)
    def setGraph(g: RGraph)(s: State): State = {
      val s2 = State.history.modify(g +: _.drop(s.undoPosition).take(50))(s)
      State.undoPosition.set(0)(s2)
    }
    def modGraph(f: RGraph => RGraph)(s: State): State = setGraph(f(graph(s)))(s)
    //val graph = Lens[State, RGraph](s => s.history(s.undoPosition))(g => s => State.history.modify(g +: _.drop(s.undoPosition).take(50/*t.props.historyLimit*/))(s))
  }

  val ST = ReactS.Fix[State]

  def removeSelection(sel: (Set[Residue], Set[GlyAnnot])) = (sel._1.foldLeft(_: RGraph)(_ - _)) andThen (sel._2.foldLeft(_: RGraph)(_ - _))

  val copyS = for {
    sel <- ST.gets(_.selection)
    g <- ST.gets(StateL.graph)
    dr = g.entries.keySet diff sel._1
    da = g.annots.values.toSet diff sel._2
    _ <- ST.mod(State.buffer set removeSelection(dr, da)(g))
  } yield ()

  val deleteS = for {
    sel <- ST.gets(_.selection)
    _ <- ST.mod(StateL.modGraph(removeSelection(sel)))
  } yield ()

  val cutS = for {
    _ <- copyS
    _ <- deleteS
  } yield ()
  //  val cutS = for {
  //    (rs, as) <- ST.gets(_.selection)
  //    g <- ST.gets(StateL.graph.get)
  //    dr = g.entries.keySet diff rs
  //    da = g.annots.values.toSet diff as
  //    _ <- ST.mod(State.buffer set removeSelection(dr, da)(g))
  //    _ <- ST.mod(StateL.graph modify removeSelection(rs, as))
  //  } yield ()

  val pasteS = for {
    buf <- ST.gets(_.buffer)
    gSel <- ST.gets { s =>
      var g = StateL.graph(s)
      def addResidue(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]], x: Double, y: Double, rot: Double): Residue = {
        val added = Residue.next(rt, ano, abs)
        g += added
        g = g.updated(added, Placement(x, y, rot))
        g = g &|-? RGraph.entryL(added) ^|-> GraphEntry.subs set subs
        added
      }
      val addedResidues = for ((src, ge) <- buf.entries) yield {
        addResidue(src.anomer, src.absolute, src.rt, ge.subs, ge.x, ge.y, ge.rotation)
      }
      val lookup = (buf.entries.keys zip addedResidues).toMap
      for {
        (src, ge) <- buf.entries
        Link(srcParent, position) <- ge.parent
        added <- lookup.get(src)
        addedParent <- lookup.get(srcParent)
      } g = addBond(Bond(added, Link(addedParent, position))).exec(g)

      val addedAnnotations = for (a <- buf.annots.values) yield {
        val added = GlyAnnot.next(a.x, a.y, a.rot, a.text, a.size)
        g += added
        added
      }

      (g, (addedResidues.toSet, addedAnnotations.toSet))
    }
    _ <- ST.mod(StateL setGraph gSel._1)
    _ <- ST.mod(State.selection set gSel._2)
  } yield ()

  class Backend(t: BackendScope[Props, State]) {
    def toggleBondLabels(): Unit = {
      t.modState(State.bondLabels.modify(bl => !bl))
    }
    def undo(): Unit =
      if(t.state.undoPosition < t.state.history.size - 1)
        t.modState(State.undoPosition.modify(_ + 1))
    def redo(): Unit =
      if(t.state.undoPosition > 0)
        t.modState(State.undoPosition.modify(_ - 1))

    def zoomIn(): Unit = t.modState(State.view ^|-> View.scale modify (_ * 1.1))
    def zoomOut(): Unit = t.modState(State.view ^|-> View.scale modify (_ * (1.0 / 1.1)))
    def zoomReset(): Unit = t.modState(State.view ^|-> View.scale set 1.0)
    //def zoomWheel(e: ReactWheelEvent): Unit = t.modState(State.view ^|-> View.scale modify (_ + e.deltaY(e.nativeEvent)))

    def cut(): Unit = t.runState(cutS).unsafePerformIO()
    def copy(): Unit = t.runState(copyS).unsafePerformIO()
    def paste(): Unit = t.runState(pasteS).unsafePerformIO()
    def delete(): Unit = t.runState(deleteS).unsafePerformIO()

    def clearAll(): Unit = t.modState(StateL setGraph RGraph())

    def residuePanelClick(template: ResiduePanel.State): Unit =
      t.modState(State.mode set template.rt.fold[Mode](Mode.Selection)(Mode.PlaceResidue(template.ano, template.abs, _)))

    def substPanelClick(template: SubstituentPanel.State): Unit =
      t.modState(State.mode set template.st.fold[Mode](Mode.Selection)(Mode.PlaceSubstituent.apply))

    def addAnnotation(): Unit = {
      t.modState(State.mode set Mode.PlaceAnnotation(30))
    }

    def modGraph(mod: RGraph => RGraph): Unit =
      t.modState(StateL modGraph mod)

    def setSelection(selection: (Set[Residue], Set[GlyAnnot])): Unit =
      t.modState(State.selection set selection)
  }

  val testGraph = {
    val r1 = Residue.next(ResidueType.Glc, Anomer.Alpha, Absolute.D)
    val r2 = Residue.next(ResidueType.Man, Anomer.Beta, Absolute.D)
    val r3 = Residue.next(ResidueType.Ido, Anomer.Alpha, Absolute.L)
    val b1 = Bond(r1, Link(r2, 2))
    (Seq(r1, r2, r3).foldLeft(RGraph())((g, r) => g + r) + b1)
      .updated(r1, Placement(50, 100, 0))
      .updated(r2, Placement(350, 50, 0))
      .updated(r3, Placement(200, 300, 45))
  }

  def apply(props: Props, children: ReactNode*) = component(props, children)
  val component = ReactComponentB[Props]("GlycanoApp")
    .initialStateP(P => State(
      history = Vector(testGraph),
      displayConv = P.conventions.getOrElse("UCT", DisplayConv.convDefault)))
    .backend(new Backend(_))
    .render((P, S, B) => {
      <.div(^.cls := "container-fluid")(
        <.div(^.cls := "row")(

          <.nav(^.cls := "navbar navbar-default", ^.role := "navigation")(<.div(^.cls := "container-fluid")(
            NavbarHeader("glycano-navbar-collapse", "Glycano"),
            <.div(^.cls := "collapse navbar-collapse", ^.id := "glycano-navbar-collapse")(
              <.p(^.cls := "navbar-text", "Load:"),
              <.form(^.cls := "navbar-form navbar-left")(
                <.div(^.cls := "form-group")(
                  FormInput(FormInput.Props("file", e => println(e.target.files(0).name)))
                )
              ),
              <.p(^.cls := "navbar-text")("Filename:"),
              <.form(^.cls := "navbar-form navbar-left")(
                <.div(^.cls := "form-group")(
                  <.input(
                    ^.ref := "filename",
                    ^.`type` := "text",
                    ^.cls := "form-control",
                    ^.placeholder := "Filename",
                    ^.value := "glycano",
                    ^.readOnly := "true")
                )
              ),
              <.ul(^.cls := "nav navbar-nav")(
                //saveDropdown
              ),
              <.form(^.cls := "navbar-form navbar-left")(
                <.div(^.cls := "form-group")(
                  <.label(^.cls := "checkbox-inline")(
                    <.input(
                      ^.checked := S.bondLabels,
                      ^.`type` := "checkbox",
                      ^.onClick --> B.toggleBondLabels()
                    ),
                    "Bond Labels"
                  )
                )
              ),
              " ",
              Button.withKey("b00")(Button.Props(() => B.clearAll(), nav = true), "Clear All"), " ",
              Button.withKey("b01")(Button.Props(() => B.delete(), nav = true), "Delete"), " ",
              Button.withKey("b02")(Button.Props(() => B.cut(), nav = true), "Cut"), " ",
              Button.withKey("b03")(Button.Props(() => B.copy(), nav = true), "Copy"), " ",
              Button.withKey("b04")(Button.Props(() => B.paste(), nav = true), "Paste"), " ",
              Button.withKey("b05")(Button.Props(() => B.undo(), nav = true), GlyphIcon("chevron-left"), " Undo"), " ",
              Button.withKey("b06")(Button.Props(() => B.redo(), nav = true), GlyphIcon("chevron-right"), " Redo"), " ",
              Button.withKey("b07")(Button.Props(() => B.addAnnotation(), nav = true), GlyphIcon("font"), " Add Annotation"), " ",
              Button.withKey("b08")(Button.Props(() => B.zoomOut(), nav = true), GlyphIcon("zoom-out")), " ",
              Button.withKey("b09")(Button.Props(() => B.zoomReset(), nav = true), "Reset Zoom"), " ",
              Button.withKey("b10")(Button.Props(() => B.zoomIn(), nav = true), GlyphIcon("zoom-in")), " "
            )
          ))
        ),

        <.div(^.cls := "row")(
          <.div(^.cls := "col-xs-3")(
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(ResiduePanel(ResiduePanel.Props(S.displayConv, B.residuePanelClick)))),
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(SubstituentPanel(SubstituentPanel.Props(B.substPanelClick))))
          ),
          <.div(^.cls := "col-xs-9")(
            GlycanoCanvas(GlycanoCanvas.Props(
              B.modGraph,
              B.setSelection,
              S.mode,
              dc = S.displayConv,
              graph = S.history(S.undoPosition),
              selection = S.selection,
              view = S.view,
              bondLabels = S.bondLabels
            ))
          )
        )
      )
    })
    .build
}
