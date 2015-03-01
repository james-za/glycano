package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._

import monocle.macros.Lenses
import monocle.Monocle._

import za.jwatson.glycanoweb._
import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.react.GlycanoApp.Mode.{PlaceAnnotation, PlaceSubstituent, PlaceResidue, Selection}
import za.jwatson.glycanoweb.react.GlycanoCanvas.InputState.BoxSelect
import za.jwatson.glycanoweb.render.{SubstituentShape, DisplayConv}
import za.jwatson.glycanoweb.structure.RGraph._

import za.jwatson.glycanoweb.structure._

import scala.scalajs.js.Dynamic
import scalajs.js
import org.scalajs.dom

import scalaz.Maybe.Just
import scalaz.effect.IO
import scalaz.std.vector._

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

  def button(e: ReactMouseEvent): Int = e.dynamic[Int](_.button)

  object Mouse {
    val Left = 0
    val Middle = 1
    val Right = 2
  }

  case class Props(mode: ExternalVar[Mode], dc: DisplayConv, width: Int = 800, height: Int = 600, graph: ExternalVar[RGraph],
                   selection: ExternalVar[(Set[ResidueId], Set[AnnotId])], view: View = View(), bondLabels: Boolean = false,
                   scaleSubstituents: Double = 1.0, limitUpdateRate: Boolean = true)

  def cmp(p: Props) = (p.mode.value, p.dc.conv, p.graph.value, p.view, p.selection.value, p.width, p.height, p.bondLabels, p.scaleSubstituents, p.limitUpdateRate)

  @Lenses case class State(inputState: InputState = InputState.Default)

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1)

  class Backend(t: BackendScope[Props, State]) {
    implicit def graph: RGraph = t.props.graph.value

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

    def resetMode(): Unit = {
      t.props.mode.set(Mode.Selection).unsafePerformIO()
      t.modState(State.inputState set InputState.Default)
    }

    def mouseClick(e: ReactMouseEvent): IO[Unit] =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Left, Mode.PlaceResidue(residue), InputState.AddResidue(x, y)) =>
          t.props.graph.mod {
            _ + GraphEntry(residue, x, y)
          }
        case (Mouse.Left, Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(link))) =>
          t.props.graph.mod {
            RGraph.residues ^|-? index(link.r) ^|-> GraphEntry.residue ^|-> Residue.subs ^|->
              at(link.position) modify { m => Just(m.orZero :+ st) }
          }
        case _ =>
          IO.ioUnit
      }

    def mouseDown(e: ReactMouseEvent): Unit =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Right, Mode.PlaceResidue(_), _) => resetMode()
        case (Mouse.Right, Mode.PlaceSubstituent(_), _) => resetMode()
        case (Mouse.Right, Mode.PlaceAnnotation(_), _) => resetMode()
        case (Mouse.Right, Mode.Selection, InputState.CreateBond(_, _, _)) =>
          t.modState(State.inputState set InputState.Default)
        case (Mouse.Left, Mode.Selection, InputState.CreateBond(from, (x, y), target)) =>
          for (to <- target) {
            t.props.graph.mod(RGraph.addBondRemovingOld(from)(to).exec).unsafePerformIO()
          }
          t.modState(State.inputState set InputState.Default)
        case _ =>
      }

    def closestLinkDsq(r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
      import scalaz.syntax.std.boolean._
      val links = for {
        ge <- r.graphEntry.toIterable
        (offset, i) <- t.props.dc.outline(ge.residue).zipWithIndex
        (ox, oy) = rotatePointRadians(offset, math.toRadians(ge.rotation))
        (dx, dy) = (ge.x + ox - x, ge.y + oy - y)
        dsq = dx * dx + dy * dy if dsq < tsq
      } yield Link(r, i + 1) -> dsq
      links.nonEmpty option links.minBy(_._2)
    }

    def closestLink(x: Double, y: Double, threshold: Double = 400): Option[Link] = {
      import scalaz.syntax.std.boolean._
      val links = for {
        r <- t.props.graph.value.residues.keys
        linkDsq <- closestLinkDsq(r, x, y, threshold)
      } yield linkDsq
      links.nonEmpty option links.minBy(_._2)._1
    }

    def mouseMove(e: ReactMouseEvent): Unit =
      if (!t.props.limitUpdateRate || updateReady()) (t.props.mode.value, t.state.inputState) match {
        case (Mode.Selection, BoxSelect(down, _)) =>
          for (pos <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.BoxSelect(down, pos))
          }
        case (Mode.PlaceResidue(_), _) =>
          for ((x, y) <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.AddResidue(x, y))
          }
        case (Mode.PlaceSubstituent(_), _) =>
          for ((x, y) <- clientToView(e.clientX, e.clientY)) {
            val link = closestLink(x, y)
            t.modState(State.inputState set InputState.AddSubstituent(x, y, link))
          }
        case (Mode.Selection, InputState.Drag(down@(x0, y0), _)) =>
          for ((x, y) <- clientToView(e.clientX, e.clientY)) {
            val offset = (x - x0, y - y0)
            t.modState(State.inputState set InputState.Drag(down, offset))
          }
        case (Mode.Selection, InputState.CreateBond(r, last, _)) =>
          for ((x, y) <- clientToView(e.clientX, e.clientY)) {
            val target = closestLink(x, y)
            t.modState(State.inputState set InputState.CreateBond(r, (x, y), target))
          }
        case _ =>
      }

    def inBounds(x: Double, y: Double) =
      0 <= x && x < t.props.width &&
        0 <= y && y < t.props.height

    def mouseOut(e: ReactMouseEvent): Unit =
      t.props.mode.value match {
        case Mode.PlaceResidue(_) | Mode.PlaceSubstituent(_) | Mode.PlaceAnnotation(_) =>
          for ((x, y) <- clientToCanvas(e.clientX, e.clientY)) {
            if (!inBounds(x, y))
              t.modState(State.inputState set InputState.Out)
          }
        case _ =>
      }

    def boxSelectDown(e: ReactMouseEvent): Unit =
      (button(e), t.props.mode.value) match {
        case (Mouse.Left, Selection) =>
          for (down <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.BoxSelect(down, down))
          }
        case _ =>
      }

    def mouseUp(e: ReactMouseEvent): Unit =
      (t.props.mode.value, t.state.inputState) match {
        case (Selection, BoxSelect((x1, y1), (x2, y2))) =>
          t.modState(State.inputState set InputState.Default)
          val (xMin, xMax) = if (x1 < x2) (x1, x2) else (x2, x1)
          val (yMin, yMax) = if (y1 < y2) (y1, y2) else (y2, y1)
          def inSelection(x: Double, y: Double) = xMin <= x && x < xMax && yMin <= y && y < yMax
          val residues = t.props.graph.value.residues.filter(e => inSelection(e._2.x, e._2.y)).keySet
          val annotations = t.props.graph.value.annotations.filter(e => inSelection(e._2.x, e._2.y)).keySet
          t.props.selection.set((residues, annotations)).unsafePerformIO()
        case (Mode.Selection, InputState.Drag(_, (dx, dy))) =>
          if (dx != 0 || dy != 0) {
            val (rs, as) = t.props.selection.value
            t.props.graph.mod(graph => graph.residues.filterKeys(rs.contains).foldLeft(graph) {
              case (g, (r, ge)) =>
                g.updated(r, Placement(ge.x + dx, ge.y + dy, ge.rotation))
            }).unsafePerformIO()
          }
          t.modState(State.inputState set InputState.Default)
        case (Mode.Selection, InputState.PreCreateBond(r)) =>
          for (to <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.CreateBond(r, to, None))
          }
        case _ =>
      }

    def residueMouseDown(r: ResidueId)(e: ReactMouseEvent): Unit =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Left, Mode.Selection, InputState.Default) =>
          for (down <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.Drag(down, (0, 0)))
            if (!t.props.selection.value._1.contains(r))
              t.props.selection.set((Set(r), Set.empty)).unsafePerformIO()
          }
        case _ =>
      }

    def handleMouseDown(r: ResidueId)(e: ReactMouseEvent): Unit =
      (button(e), t.props.mode.value) match {
        case (Mouse.Left, Mode.Selection) =>
          t.modState(State.inputState set InputState.PreCreateBond(r))
          for (link <- r.parent) t.props.graph.mod(_ - link).unsafePerformIO()
        case _ =>
      }
    
    def modInputState(inputState: InputState): Unit = t.modState(State.inputState set inputState)
  }

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case class AddResidue(x: Double, y: Double) extends InputState
    case class AddSubstituent(x: Double, y: Double, target: Option[Link]) extends InputState
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
    val (x, y) = rotatePointRadians(outline(i - 1), math.toRadians(ge.rotation))
    (ge.x + x, ge.y + y)
  }

  def rotatePointRadians(p: (Double, Double), a: Double, c: (Double, Double) = (0, 0)): (Double, Double) = {
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
      implicit val graph: RGraph = P.graph.value

      val viewX = P.view.x
      val viewY = P.view.y
      val viewScale = P.view.scale

      val outlines = for ((r, ge) <- P.graph.value.residues) yield {
        r -> P.dc.outline(ge.residue)
      }

      val (drag, dx, dy) = S.inputState match {
        case InputState.Drag(_, (ox, oy)) => (true, ox, oy)
        case _ => (false, 0.0, 0.0)
      }

      val entriesOffset = for ((r, ge) <- P.graph.value.residues) yield {
        val geSubbed = (P.mode.value, S.inputState) match {
          case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(Link(tr, tp)))) if tr == r =>
            ge &|-> GraphEntry.residue ^|-> Residue.subs ^|-> at(tp) modify { m => Just(m.orZero :+ st) }
          case _ => ge
        }

        val selected = P.selection.value._1.contains(r)
        val geOffset = if (drag && selected) geSubbed.copy(x = geSubbed.x + dx, y = geSubbed.y + dy) else geSubbed
        r -> geOffset
      }

      val bonds = for {
        (r, ge) <- entriesOffset.toSeq
        toLink @ Link(toRes, i) <- ge.parent
      } yield {
        val from = outlinePos(outlines(r), ge, 1)
        val to = outlinePos(outlines(toRes), entriesOffset(toRes), i)
        SVGBond.withKey("bond" + r.id)(SVGBond.Props(ge.residue.ano, Some(i), from, to, P.bondLabels))
      }

      val tempBond = (P.mode.value, S.inputState) match {
        case (Mode.Selection, InputState.CreateBond(r, mouse, target)) =>
          for (ge <- r.graphEntry) yield {
            val from = outlinePos(outlines(r), ge, 1)
            val targetLink = for {
              Link(rLink, pos) <- target
              geLink <- rLink.graphEntry
            } yield outlinePos(outlines(rLink), geLink, pos)
            val to = targetLink getOrElse mouse
            SVGBond.withKey("tempBond")(SVGBond.Props(ge.residue.ano, target.map(_.position), from, to, P.bondLabels))
          }
        case _ =>
          None
      }

      val residues = for ((r, ge) <- entriesOffset) yield {
        val selected = P.selection.value._1.contains(r)
        SVGResidue.withKey("residue" + r.id)(SVGResidue.Props(B.residueMouseDown(r), B.handleMouseDown(r), ge, P.dc, selected, P.scaleSubstituents))
      }

      val tempSubstituent = (P.mode.value, S.inputState) match {
        case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(x, y, None))=>
          val (shape, (w, h)) = SubstituentShape(st)
          val scale = P.scaleSubstituents
          val (mx, my) = (-w / 2.0, -h / 2.0)
          Some(shape(^.svg.transform := s"translate($x, $y) scale($scale) translate($mx, $my)"))
        case _ => None
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

      val tempResidue = (P.mode.value, S.inputState) match {
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y)) =>
          val ge = GraphEntry(residue, x, y, 0)
          Some(SVGResidue.withKey("tempResidue")(SVGResidue.Props(_ => (), _ => (), ge, P.dc, selected = false, P.scaleSubstituents)))
        case _ => None
      }

      <.svg.svg(
        ^.svg.width := P.width,
        ^.svg.height := P.height,
        ^.ref := "canvas",
        ^.onMouseMove ==> B.mouseMove,
        ^.onClick ~~> B.mouseClick _,
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
          tempSubstituent,
          selectionBox,
          tempResidue
        )
      )
    })
    .shouldComponentUpdate {
      (T, P, S) =>
        cmp(T.props) != cmp(P) || T.state != S
    }
    .componentDidMount { scope =>
      scope.getDOMNode().addEventListener("contextmenu", (e: org.scalajs.dom.Event) => {
        e.preventDefault()
        false
      })
    }
    //.domType[dom.SVGSVGElement]
    .build
}
