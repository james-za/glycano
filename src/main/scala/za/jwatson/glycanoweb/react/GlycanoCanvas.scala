package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monocle.macros.Lenses
import za.jwatson.glycanoweb.GlyAnnot
import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.react.GlycanoApp.Mode.{PlaceAnnotation, PlaceSubstituent, PlaceResidue, Selection}
import za.jwatson.glycanoweb.react.GlycanoCanvas.InputState.BoxSelect
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph._

import za.jwatson.glycanoweb.structure._

import scalajs.js
import org.scalajs.dom

object GlycanoCanvas {
  case class Props(modGraph: (RGraph => RGraph) => Unit, setSelection: ((Set[Residue], Set[GlyAnnot])) => Unit,
                   mode: GlycanoApp.Mode, dc: DisplayConv, width: Int = 800, height: Int = 600, graph: RGraph,
                   selection: (Set[Residue], Set[GlyAnnot]), view: View = View())

  @Lenses case class State(hoverHandle: Option[Residue] = None, inputState: InputState = InputState.Default)

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1)

  class Backend(t: BackendScope[Props, State]) {
    def clientToView(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.HTMLElement]("canvas")(t).map(_.getDOMNode().asInstanceOf[dom.SVGSVGElement])
      view <- Ref[dom.HTMLElement]("view")(t).map(_.getDOMNode().asInstanceOf[dom.SVGGElement])
    } yield {
      val p = svg.createSVGPoint()
      p.x = x
      p.y = y
      val p2 = p.matrixTransform(view.getScreenCTM().inverse())
      (p2.x, p2.y)
    }
    def clientToCanvas(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.HTMLElement]("canvas")(t).map(_.getDOMNode().asInstanceOf[dom.SVGSVGElement])
    } yield {
      val p = svg.createSVGPoint()
      p.x = x
      p.y = y
      val p2 = p.matrixTransform(svg.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def mouseClick(e: ReactMouseEvent): Unit = (t.props.mode, t.state.inputState) match {
      case (Mode.PlaceResidue(ano, abs, rt), InputState.AddResidue(x, y)) =>
        val r = Residue.next(rt, ano, abs)
        t.props.modGraph(g => (g + r).updated(r, Placement(x, y, 0)))
      case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(Some(link))) =>

      case _ =>
    }

    def mouseMove(e: ReactMouseEvent): Unit = (t.props.mode, t.state.inputState) match {
      case (Mode.Selection, BoxSelect(down, _)) =>
        for (pos <- clientToView(e.clientX, e.clientY)) {
          t.modState(State.inputState set InputState.BoxSelect(down, pos))
        }
      case (Mode.PlaceResidue(ano, abs, rt), _) =>
        for ((x, y) <- clientToView(e.clientX, e.clientY)) {
          t.modState(State.inputState set InputState.AddResidue(x, y))
        }
      case (Mode.Selection, InputState.Drag((x0, y0))) =>
        for ((x, y) <- clientToView(e.clientX, e.clientY)) {
          val (dx, dy) = (x - x0, y - y0)
          val (rs, as) = t.props.selection
          t.modState(State.inputState set InputState.Drag((x, y)))
          t.props.modGraph(graph => graph.entries.filterKeys(rs.contains).foldLeft(graph) {
            case (g, (r, ge)) =>
              g.updated(r, Placement(ge.x + dx, ge.y + dy, ge.rotation))
          })
        }
      case _ =>
    }

    def inBounds(x: Double, y: Double) =
      0 <= x && x < t.props.width &&
      0 <= y && y < t.props.height

    def mouseOut(e: ReactMouseEvent): Unit = for ((x, y) <- clientToCanvas(e.clientX, e.clientY)) {
      if (!inBounds(x, y))
        t.modState(State.inputState set InputState.Out)
    }

    def boxSelectDown(e: ReactMouseEvent): Unit = t.props.mode match {
      case Selection =>
        for (down <- clientToView(e.clientX, e.clientY)) {
          t.modState(State.inputState set InputState.BoxSelect(down, down))
        }
      case _ =>
    }

    def mouseUp(e: ReactMouseEvent): Unit = (t.props.mode, t.state.inputState) match {
      case (Selection, BoxSelect((x1, y1), (x2, y2))) =>
        t.modState(State.inputState set InputState.Default)
        val (xMin, xMax) = if (x1 < x2) (x1, x2) else (x2, x1)
        val (yMin, yMax) = if (y1 < y2) (y1, y2) else (y2, y1)
        def inSelection(x: Double, y: Double) = xMin <= x && x < xMax && yMin <= y && y < yMax
        val residues = t.props.graph.entries.filter(e => inSelection(e._2.x, e._2.y)).keySet
        val annotations = t.props.graph.annots.filter(e => inSelection(e._2.x, e._2.y)).values.toSet
        t.props.setSelection(residues, annotations)
        println(residues)
      case (Mode.Selection, InputState.Drag(_)) =>
        t.modState(State.inputState set InputState.Default)
      case _ =>
    }

    def residueMouseDown(r: Residue)(e: ReactMouseEvent): Unit = {
      t.props.mode match {
        case Mode.Selection =>
          for (down <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.Drag(down))
            if (!t.props.selection._1.contains(r))
              t.props.setSelection(Set(r), Set.empty)
          }
        case _ =>
      }
    }

    def handleMouseDown(r: Residue)(e: ReactMouseEvent): Unit = {

    }
    
    def modInputState(inputState: InputState): Unit = t.modState(State.inputState set inputState)
  }

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case class AddResidue(x: Double, y: Double) extends InputState
    case class AddSubstituent(target: Option[Link]) extends InputState
    case class BoxSelect(from: (Double, Double), to: (Double, Double)) extends InputState
    case class Drag(last: (Double, Double)) extends InputState
    case object CreateBond extends InputState
    case object PostCreateBond extends InputState
    case class Hit(down: (Double, Double), item: Residue) extends InputState
    case class Rotate(item: Residue) extends InputState
    case class AddAnnotation(x: Double, y: Double) extends InputState
    case object Out extends InputState
  }

  def outlinePos(outline: IndexedSeq[(Double, Double)], r: Residue, ge: GraphEntry, i: Int): (Double, Double) = {
    val (x, y) = rotatePoint(outline(i), ge.rotation)
    (ge.x + x, ge.y + y)
  }

  def rotatePoint(p: (Double, Double), a: Double, c: (Double, Double) = (0, 0)): (Double, Double) = {
    val sin = math.sin(a)
    val cos = math.cos(a)

    val tx = p._1 - c._1
    val ty = p._2 - c._2

    val nx = tx * cos - ty * sin
    val ny = tx * sin + ty * cos

    (nx + c._1, ny + c._2)
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def apply(props: Props, children: ReactNode*) = component.apply(props, children)
  val component = ReactComponentB[Props]("GlycanoCanvas")
    .initialState(State())
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      val View(vx, vy, vs) = P.view

      val outlines = for ((r, ge) <- P.graph.entries) yield {
        r -> P.dc.outline(r.anomer, r.absolute, r.rt, ge.subs)
      }

      val bonds = for {
        (r @ Residue(_, rt, ano, abs), ge) <- P.graph.entries
        toLink @ Link(to, i) <- ge.parent
      } yield {
        val (x1, y1) = outlinePos(outlines(r), r, ge, 0)
        val (x2, y2) = outlinePos(outlines(r), to, P.graph.entries(to), i)
        <.svg.line(
          ^.key := Bond(r, toLink).##,
          ^.svg.x1 := x1, ^.svg.y1 := y1,
          ^.svg.x2 := x2, ^.svg.y2 := y2,
          ^.svg.stroke := "black", "strokeWidth".reactAttr := "7"
        )
      }

      val residues = for ((r, ge) <- P.graph.entries) yield {
        val selected = P.selection._1.contains(r)
        SVGResidue.withKey(r.id)(SVGResidue.Props(B.residueMouseDown(r), B.handleMouseDown(r), r, ge, P.dc, selected))
      }

      val selectionBox = S.inputState match {
        case InputState.BoxSelect((x1, y1), (x2, y2)) =>
          Some(<.svg.rect(
            ^.svg.x := math.min(x1, x2),
            ^.svg.y := math.min(y1, y2),
            ^.svg.width := math.abs(x2 - x1),
            ^.svg.height := math.abs(y2 - y1),
            ^.svg.fill := "#8080FF", "fillOpacity".reactAttr := "50%",
            ^.svg.stroke := "#8080FF", "strokeWidth".reactAttr := 1
          ))
        case _ => None
      }

      val tempResidue = (P.mode, S.inputState) match {
        case (Mode.PlaceResidue(ano, abs, rt), InputState.AddResidue(x, y)) =>
          val r = Residue(0, rt, ano, abs)
          val ge = GraphEntry(x, y, 0)
          Some(SVGResidue.withKey(0)(SVGResidue.Props(_ => (), _ => (), r, ge, P.dc, selected = false)))
        case _ => None
      }

      <.svg.svg(
        ^.svg.width := P.width,
        ^.svg.height := P.height,
        ^.ref := "canvas",
        ^.onMouseMove ==> B.mouseMove,
        ^.onClick ==> B.mouseClick,
        ^.onMouseOut ==> B.mouseOut,
        ^.onMouseUp ==> B.mouseUp
      )(
        <.svg.g(^.svg.transform := s"translate($vx $vy) scale($vs)", ^.ref := "view")(
          bonds,
          <.svg.rect(
            ^.svg.fill := "transparent",
            ^.svg.width := P.width,
            ^.svg.height := P.height,
            ^.onMouseDown ==> B.boxSelectDown
          ),
          residues,
          selectionBox,
          tempResidue
        )
      )
    })
    //.domType[dom.SVGSVGElement]
    .build

}
