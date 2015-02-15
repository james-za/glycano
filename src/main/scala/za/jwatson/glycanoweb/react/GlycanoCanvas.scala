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

import scala.scalajs.js.Dynamic
import scalajs.js
import org.scalajs.dom

object GlycanoCanvas {
  private val updateInterval = 16
  private var lastUpdated: Double = js.Date.now()
  def updateReady(): Boolean = {
    val time = js.Date.now()
    if (time - lastUpdated > updateInterval) {
      lastUpdated = time
      true
    } else false
  }

  case class Props(modGraph: (RGraph => RGraph) => Unit, setSelection: ((Set[ResidueId], Set[AnnotId])) => Unit,
                   mode: GlycanoApp.Mode, dc: DisplayConv, width: Int = 800, height: Int = 600, graph: RGraph,
                   selection: (Set[ResidueId], Set[AnnotId]), view: View = View(), bondLabels: Boolean = false)

  def cmp(p: Props) = (p.mode, p.dc.conv, p.graph, p.view, p.selection, p.width, p.height, p.bondLabels)

  @Lenses case class State(inputState: InputState = InputState.Default)

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1)

  class Backend(t: BackendScope[Props, State]) {
    implicit def graph = t.props.graph

    def clientToView(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.html.Element]("canvas")(t).map(_.getDOMNode().asInstanceOf[dom.svg.SVG])
      view <- Ref[dom.html.Element]("view")(t).map(_.getDOMNode().asInstanceOf[dom.svg.G])
    } yield {
      val p = svg.createSVGPoint()
      p.x = x
      p.y = y
      val p2 = p.matrixTransform(view.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def clientToCanvas(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.html.Element]("canvas")(t).map(_.getDOMNode().asInstanceOf[dom.svg.SVG])
    } yield {
      val p = svg.createSVGPoint()
      p.x = x
      p.y = y
      val p2 = p.matrixTransform(svg.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def mouseClick(e: ReactMouseEvent): Unit = (t.props.mode, t.state.inputState) match {
      case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y)) =>
        t.props.modGraph {
          _ + GraphEntry(residue, x, y)
        }
      case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(Some(link))) =>

      case _ =>
    }

    def mouseDown(e: ReactMouseEvent): Unit = {
      val button: Int = e.asInstanceOf[Dynamic].button.asInstanceOf[Int]
      if (button == 2) {
        e.preventDefault()
        (t.props.mode, t.state.inputState) match {
          case (Mode.Selection, InputState.CreateBond(_, _, _)) =>
            t.modState(State.inputState set InputState.Default)
          case _ =>
        }
      }
    }

    def closestLinkDsq(r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
      import scalaz.syntax.std.boolean._
      val links = for {
        ge <- r.graphEntry.toIterable
        (offset, i) <- t.props.dc.outline(ge.residue).zipWithIndex
        (ox, oy) = rotatePoint(offset, ge.rotation)
        (dx, dy) = (ge.x + ox - x, ge.y + oy - y)
        dsq = dx * dx + dy * dy if dsq < tsq
      } yield Link(r, i) -> dsq
      links.nonEmpty option links.minBy(_._2)
    }

    def mouseMove(e: ReactMouseEvent): Unit = if (updateReady()) (t.props.mode, t.state.inputState) match {
      case (Mode.Selection, BoxSelect(down, _)) =>
        for (pos <- clientToView(e.clientX, e.clientY)) {
          t.modState(State.inputState set InputState.BoxSelect(down, pos))
        }
      case (Mode.PlaceResidue(_), _) =>
        for ((x, y) <- clientToView(e.clientX, e.clientY)) {
          t.modState(State.inputState set InputState.AddResidue(x, y))
        }
      case (Mode.Selection, InputState.Drag(down@(x0, y0), _)) =>
        for ((x, y) <- clientToView(e.clientX, e.clientY)) {
          val offset = (x - x0, y - y0)
          t.modState(State.inputState set InputState.Drag(down, offset))
        }
      case (Mode.Selection, InputState.CreateBond(r, last, _)) =>
        import scalaz.syntax.std.boolean._
        for ((x, y) <- clientToView(e.clientX, e.clientY)) {
          val links = for {
            r <- t.props.graph.residues.keys
            linkDsq <- closestLinkDsq(r, x, y, 400)
          } yield linkDsq
          val target = links.nonEmpty option links.minBy(_._2)._1
          t.modState(State.inputState set InputState.CreateBond(r, (x, y), target))
        }
      case _ =>
    }

    def inBounds(x: Double, y: Double) =
      0 <= x && x < t.props.width &&
        0 <= y && y < t.props.height

    def mouseOut(e: ReactMouseEvent): Unit = t.props.mode match {
      case Mode.PlaceResidue(_) | Mode.PlaceSubstituent(_) | Mode.PlaceAnnotation(_) =>
        for ((x, y) <- clientToCanvas(e.clientX, e.clientY)) {
          if (!inBounds(x, y))
            t.modState(State.inputState set InputState.Out)
        }
      case _ =>
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
        val residues = t.props.graph.residues.filter(e => inSelection(e._2.x, e._2.y)).keySet
        val annotations = t.props.graph.annotations.filter(e => inSelection(e._2.x, e._2.y)).keySet
        t.props.setSelection(residues, annotations)
      case (Mode.Selection, InputState.Drag(_, (dx, dy))) =>
        if (dx != 0 || dy != 0) {
          val (rs, as) = t.props.selection
          t.props.modGraph(graph => graph.residues.filterKeys(rs.contains).foldLeft(graph) {
            case (g, (r, ge)) =>
              g.updated(r, Placement(ge.x + dx, ge.y + dy, ge.rotation))
          })
        }
        t.modState(State.inputState set InputState.Default)
      case (Mode.Selection, InputState.PreCreateBond(r)) =>
        for (to <- clientToView(e.clientX, e.clientY)) {
          t.modState(State.inputState set InputState.CreateBond(r, to, None))
        }
      case _ =>
    }

    def residueMouseDown(r: ResidueId)(e: ReactMouseEvent): Unit = {
      t.props.mode match {
        case Mode.Selection =>
          for (down <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.Drag(down, (0, 0)))
            if (!t.props.selection._1.contains(r))
              t.props.setSelection(Set(r), Set.empty)
          }
        case _ =>
      }
    }

    def handleMouseDown(r: ResidueId)(e: ReactMouseEvent): Unit = {
      t.props.mode match {
        case Mode.Selection =>
          t.modState(State.inputState set InputState.PreCreateBond(r))
        case _ =>
      }
    }
    
    def modInputState(inputState: InputState): Unit = t.modState(State.inputState set inputState)
  }

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case class AddResidue(x: Double, y: Double) extends InputState
    case class AddSubstituent(target: Option[Link]) extends InputState
    case class BoxSelect(from: (Double, Double), to: (Double, Double)) extends InputState
    case class Drag(down: (Double, Double), offset: (Double, Double)) extends InputState
    case class PreCreateBond(r: ResidueId) extends InputState
    case class CreateBond(r: ResidueId, to: (Double, Double), target: Option[Link]) extends InputState
    case object PostCreateBond extends InputState
    case class Hit(down: (Double, Double), item: Residue) extends InputState
    case class Rotate(item: Residue) extends InputState
    case class AddAnnotation(x: Double, y: Double) extends InputState
    case object Out extends InputState
  }

  def outlinePos(outline: IndexedSeq[(Double, Double)], ge: GraphEntry, i: Int): (Double, Double) = {
    val (x, y) = rotatePoint(outline(i), math.toRadians(ge.rotation))
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
      implicit val graph = P.graph

      val viewX = P.view.x
      val viewY = P.view.y
      val viewScale = P.view.scale

      val outlines = for ((r, ge) <- P.graph.residues) yield {
        r -> P.dc.outline(ge.residue)
      }

      val (drag, dx, dy) = S.inputState match {
        case InputState.Drag(_, (ox, oy)) => (true, ox, oy)
        case _ => (false, 0.0, 0.0)
      }

      val entriesOffset = for ((r, ge) <- P.graph.residues) yield {
        val selected = P.selection._1.contains(r)
        val geOffset = if (drag && selected) ge.copy(x = ge.x + dx, y = ge.y + dy) else ge
        r -> geOffset
      }

      val bonds = for {
        (r, ge) <- entriesOffset.toSeq
        toLink @ Link(toRes, i) <- ge.parent
      } yield {
        val from = outlinePos(outlines(r), ge, 0)
        val to = outlinePos(outlines(r), entriesOffset(toRes), i)
        SVGBond.withKey(r.id)(SVGBond.Props(ge.residue.ano, Some(i), from, to, P.bondLabels))
      }

      val tempBond = (P.mode, S.inputState) match {
        case (Mode.Selection, InputState.CreateBond(r, mouse, target)) =>
          for (ge <- r.graphEntry) yield {
            val from = outlinePos(outlines(r), ge, 0)
            val targetLink = for {
              Link(rLink, pos) <- target
              geLink <- rLink.graphEntry
            } yield outlinePos(outlines(rLink), geLink, pos)
            val to = targetLink getOrElse mouse
            SVGBond.withKey("tempBond")(SVGBond.Props(ge.residue.ano, None, from, to, P.bondLabels))
          }
        case _ =>
          None
      }

      val residues = for ((r, ge) <- entriesOffset) yield {
        val selected = P.selection._1.contains(r)
        SVGResidue.withKey(r.id)(SVGResidue.Props(B.residueMouseDown(r), B.handleMouseDown(r), ge, P.dc, selected))
      }

      val selectionBox = S.inputState match {
        case InputState.BoxSelect((x1, y1), (x2, y2)) =>
          Some(<.svg.rect(
            ^.svg.x := math.min(x1, x2),
            ^.svg.y := math.min(y1, y2),
            ^.svg.width := math.abs(x2 - x1),
            ^.svg.height := math.abs(y2 - y1),
            ^.svg.fill := "#8080FF", ^.svg.fillOpacity := "50%",
            ^.svg.stroke := "#8080FF", ^.svg.strokeWidth := 1
          ))
        case _ => None
      }

      val tempResidue = (P.mode, S.inputState) match {
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y)) =>
          val ge = GraphEntry(residue, x, y, 0)
          Some(SVGResidue.withKey("tempResidue")(SVGResidue.Props(_ => (), _ => (), ge, P.dc, selected = false)))
        case _ => None
      }

      <.svg.svg(
        ^.svg.width := P.width,
        ^.svg.height := P.height,
        ^.ref := "canvas",
        ^.onMouseMove ==> B.mouseMove,
        ^.onClick ==> B.mouseClick,
        ^.onMouseOut ==> B.mouseOut,
        ^.onMouseUp ==> B.mouseUp,
        ^.onMouseDown ==> B.mouseDown
      )(
        <.svg.g(^.svg.transform := s"translate($viewX $viewY) scale($viewScale)", ^.ref := "view")(
          bonds,
          tempBond,
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
    .shouldComponentUpdate {
      (T, P, S) =>
        cmp(T.props) != cmp(P) || T.state != S
    }
    .componentDidMount(scope => {
      scope.getDOMNode().addEventListener("contextmenu", (e: org.scalajs.dom.Event) => {
        e.preventDefault()
        false
      })
    })
    //.domType[dom.SVGSVGElement]
    .build
}
