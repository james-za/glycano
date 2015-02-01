package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.{Getter, Lens}
import monocle.macros.{Lenser, Lenses}
import monocle.Monocle._
import za.jwatson.glycanoweb.GlyAnnot
import za.jwatson.glycanoweb.react.GlycanoCanvas.View
import za.jwatson.glycanoweb.react.bootstrap.{FormInput, NavbarHeader}
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

object GlycanoApp {
  case class Props(/*historyLimit: Int = 50*/)

  @Lenses case class State(
    undoPosition: Int = 0,
    history: Vector[RGraph] = Vector(RGraph()),
    selection: (Set[Residue], Set[GlyAnnot]) = (Set.empty, Set.empty),
    bondLabels: Boolean = false,
    view: View = View(),
    buffer: RGraph = RGraph(),
    mode: Mode = Selection
  )

  sealed trait Mode
  case object Selection extends Mode
  case class PlaceResidue(ano: Anomer, abs: Absolute, rt: ResidueType) extends Mode
  case class PlaceAnnotation(size: Double) extends Mode

  object StateL {
    val graph = Lens[State, RGraph](s => s.history(s.undoPosition))(g => s => State.history.modify(g +: _.drop(s.undoPosition) take 50/*t.props.historyLimit*/)(s))
  }

  val ST = ReactS.Fix[State]

  def removeSelection(sel: (Set[Residue], Set[GlyAnnot])) = (sel._1.foldLeft(_: RGraph)(_ - _)) andThen (sel._2.foldLeft(_: RGraph)(_ - _))

  val copyS = for {
    sel <- ST.gets(_.selection)
    g <- ST.gets(StateL.graph.get)
    dr = g.entries.keySet diff sel._1
    da = g.annots.values.toSet diff sel._2
    _ <- ST.mod(State.buffer set removeSelection(dr, da)(g))
  } yield ()

  val deleteS = for {
    sel <- ST.gets(_.selection)
    _ <- ST.mod(StateL.graph modify removeSelection(sel))
  } yield ()

  val cutS = copyS >> deleteS
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
      var g = StateL.graph.get(s)
      def addResidue(ano: Anomer, abs: Absolute, rt: ResidueType, x: Double, y: Double, rot: Double): Residue = {
        val added = Residue.next(rt, ano, abs)
        g += added
        g = g.updated(added, Placement(x, y, rot))
        added
      }
      def addSubstituent(link: Link, st: SubstituentType): Unit = {
        val added = Substituent.next(st)
        g += link -> added
      }
      val addedResidues = for ((src, ge) <- buf.entries) yield {
        val added = addResidue(src.anomer, src.absolute, src.rt, ge.x, ge.y, ge.rotation)
        for {
          (pos, subs) <- ge.subs
          sub <- subs
        } addSubstituent(Link(added, pos), sub.st)
        added
      }
      val lookup = (buf.entries.keys zip addedResidues).toMap
      for {
        (src, ge) <- buf.entries
        Link(srcParent, position) <- ge.parent
        added <- lookup.get(src)
        addedParent <- lookup.get(srcParent)
      } addBond(Bond(added, Link(addedParent, position)))

      val addedSubstituents = for (a <- buf.annots.values) yield {
        val added = GlyAnnot.next(a.x, a.y, a.rot, a.text, a.size)
        g += added
        added
      }

      (g, (addedResidues.toSet, addedSubstituents.toSet))
    }
    _ <- ST.mod(StateL.graph set gSel._1)
    _ <- ST.mod(State.selection set gSel._2)
  } yield ()

  class Backend(t: BackendScope[Props, State]) {
    def toggleBondLabels(): Unit = t.modState(State.bondLabels.modify(!_))
    def undo(): Unit =
      if(t.state.undoPosition < t.state.history.size - 1)
        t.modState(State.undoPosition.modify(_ + 1))
    def redo(): Unit =
      if(t.state.undoPosition > 0)
        t.modState(State.undoPosition.modify(_ - 1))

    def zoomIn(): Unit = t.modState(State.view ^|-> View.scale modify (_ * 1.1))
    def zoomOut(): Unit = t.modState(State.view ^|-> View.scale modify (_ * 0.9))
    def zoomReset(): Unit = t.modState(State.view ^|-> View.scale set 1.0)
    //def zoomWheel(e: ReactWheelEvent): Unit = t.modState(State.view ^|-> View.scale modify (_ + e.deltaY(e.nativeEvent)))

    def cut(): Unit = t.runState(deleteS).unsafePerformIO()
    def copy(): Unit = t.runState(copyS).unsafePerformIO()
    def paste(): Unit = t.runState(pasteS).unsafePerformIO()
    def delete(): Unit = t.runState(deleteS).unsafePerformIO()

    def clearAll(): Unit = t.modState(StateL.graph set RGraph())

    def residuePanelClick(template: ResiduePanel.State): Unit =
      t.modState(State.mode set template.rt.fold[Mode](Selection)(PlaceResidue(template.ano, template.abs, _)))

    def addAnnotation(): Unit = {
      t.modState(State.mode set PlaceAnnotation(30))
    }
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

  println(RGraph() + Residue.next(ResidueType.Glc, Anomer.Alpha, Absolute.D))

  def apply(props: Props, children: ReactNode*) = component(props, children)
  val component = ReactComponentB[Props]("GlycanoApp")
    .initialState(State(history = Vector(testGraph)))
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
                    ^.id := "filename",
                    ^.`type` := "text",
                    ^.cls := "form-control",
                    ^.placeholder := "Filename",
                    ^.value := "glycano")
                )
              ),
              <.ul(^.cls := "nav navbar-nav")(
                //saveDropdown
              ),
              <.form(^.cls := "navbar-form navbar-left")(
                <.div(^.cls := "form-group")(
                  <.label(^.cls := "checkbox-inline")(
                    <.input(
                      S.bondLabels ?= (^.checked := "true"),
                      ^.`type` := "checkbox",
                      ^.value := "bondlabel",
                      ^.onClick --> B.toggleBondLabels()),
                    "Bond Labels" + (if (S.bondLabels) " checked" else "")
                  )
                )
              ),
              " ",
              Button(Button.Props(() => B.clearAll(), nav = true), "Clear All"), " ",
              Button(Button.Props(() => B.delete(), nav = true), "Delete"), " ",
              Button(Button.Props(() => B.cut(), nav = true), "Cut"), " ",
              Button(Button.Props(() => B.copy(), nav = true), "Copy"), " ",
              Button(Button.Props(() => B.paste(), nav = true), "Paste"), " ",
              Button(Button.Props(() => B.undo(), nav = true), GlyphIcon("chevron-left"), " Undo"), " ",
              Button(Button.Props(() => B.redo(), nav = true), GlyphIcon("chevron-right"), " Redo"), " ",
              Button(Button.Props(() => B.addAnnotation(), nav = true), GlyphIcon("font"), " Add Annotation"), " ",
              Button(Button.Props(() => B.zoomOut(), nav = true), GlyphIcon("zoom-out")), " ",
              Button(Button.Props(() => B.zoomReset(), nav = true), "100%"), " ",
              Button(Button.Props(() => B.zoomIn(), nav = true), GlyphIcon("zoom-in")), " "
            )
          ))
        ),

        <.div(^.cls := "row")(
          <.div(^.cls := "col-xs-3")(
            <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(ResiduePanel(ResiduePanel.Props(B.residuePanelClick))))
          ),
          <.div(^.cls := "col-xs-9")(
            GlycanoCanvas(GlycanoCanvas.Props(B, graph = S.history(S.undoPosition), selection = S.selection, view = S.view))
          )
        )
      )
    }).build
}
