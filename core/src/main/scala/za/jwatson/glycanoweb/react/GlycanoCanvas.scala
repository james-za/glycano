package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Monocle._
import monocle.macros.Lenses
import org.scalajs.dom
import za.jwatson.glycanoweb._
import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.react.GlycanoApp.Mode.Selection
import za.jwatson.glycanoweb.react.GlycanoCanvas.InputState.BoxSelect
import za.jwatson.glycanoweb.render.{DisplayConv, SubstituentShape}
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

import scala.annotation.tailrec
import scala.scalajs.js
import scalaz.Maybe.Just
import scalaz.{Semigroup, Monoid, OptionT}
import scalaz.effect.IO
import scalaz.effect.IO.IOMonoid
import scalaz.std.anyVal.unitInstance
import scalaz.std.vector._
import scalaz.syntax.monad.ApplicativeIdV
import scalaz.syntax.std.option._
import scalaz.syntax.id._
import scalaz.syntax.std.boolean._

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

  implicit def undefOrMonoid[A: Semigroup]: Monoid[js.UndefOr[A]] = new Monoid[js.UndefOr[A]] {
    import js.UndefOr._
    def append(f1: js.UndefOr[A], f2: => js.UndefOr[A]) = (f1.isDefined, f2.isDefined) match {
      case (true, true) => Semigroup[A].append(f1.get, f2.get)
      case (true, false) => f1.get
      case (false, true) => f2.get
      case (false, false) => js.undefined
    }
    def zero: js.UndefOr[A] = js.undefined
  }

  def button(e: ReactMouseEvent): Int = e.dynamic[Int](_.button)

  object Mouse {
    val Left = 0
    val Middle = 1
    val Right = 2
  }

  case class Props(mode: ExternalVar[Mode], dc: DisplayConv, graph: ExternalVar[RGraph],
                   selection: ExternalVar[(Set[ResidueId], Set[AnnotId])], view: ExternalVar[View], bondLabels: Boolean,
                   scaleSubstituents: Double, limitUpdateRate: Boolean, annotationFontSize: Double,
                   fitBounds: ExternalVar[(Double, Double, Double, Double)])

  def cmp(p: Props) = (p.mode.value, p.dc.conv, p.graph.value, p.selection.value, p.view.value, p.bondLabels, p.scaleSubstituents, p.limitUpdateRate, p.annotationFontSize)

  @Lenses case class State(inputState: InputState = InputState.Default)

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1, width: Int = 800, height: Int = 600)

  class Backend(t: BackendScope[Props, State]) {
    implicit def graph: RGraph = t.props.graph.value

    def clientToViewIO(x: Double, y: Double)(f: ((Double, Double)) => IO[Unit]): IO[Unit] =
      clientToView(x, y).map(f).getOrElse(IO.ioUnit)
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

    def clientToCanvasIO(x: Double, y: Double)(f: ((Double, Double)) => IO[Unit]): IO[Unit] =
      clientToCanvas(x, y).map(f).getOrElse(IO.ioUnit)
    def clientToCanvas(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.html.Element]("canvas")(t).map(_.getDOMNode().asInstanceOf[dom.svg.SVG])
    } yield {
      val p = svg.createSVGPoint()
      p.x = x
      p.y = y
      val p2 = p.matrixTransform(svg.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def resetMode(): IO[Unit] = for {
      _ <- t.props.mode.set(Mode.Selection)
      _ <- t.modStateIO(State.inputState set InputState.Default)
    } yield ()

    def mouseClick(e: ReactMouseEvent): IO[Unit] =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Left, Mode.PlaceResidue(residue), InputState.AddResidue(x, y, children, parent)) =>
          t.props.graph.mod { g =>
            g + GraphEntry(residue, x, y, 0, children, parent)
          }
        case (Mouse.Left, Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(link))) =>
          t.props.graph.mod {
            RGraph.residues ^|-? index(link.r) ^|-> GraphEntry.residue ^|-> Residue.subs ^|->
              at(link.position) modify { m => Just(m.orZero :+ st) }
          }
        case (Mouse.Left, Mode.PlaceAnnotation, InputState.AddAnnotation(x, y)) =>
          t.props.graph.mod {
            val entry = AnnotId.next() -> Annot("Annotation", t.props.annotationFontSize, x, y)
            RGraph.annotations modify (_ + entry)
          }
        case _ =>
          IO.ioUnit
      }

    def mouseDown(e: ReactMouseEvent): IO[Unit] =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Right, Mode.PlaceResidue(_), _) => resetMode()
        case (Mouse.Right, Mode.PlaceSubstituent(_), _) => resetMode()
        case (Mouse.Right, Mode.PlaceAnnotation, _) => resetMode()
        case (Mouse.Right, Mode.Selection, InputState.CreateBond(_, _, _)) =>
          t.modStateIO(State.inputState set InputState.Default)
        case (Mouse.Left, Mode.Selection, InputState.CreateBond(from, (x, y), target)) =>
          for {
            _ <- target.map(to => t.props.graph.mod(RGraph.addBondRemovingOld(from)(to).exec)).orZero
            _ <- t.modStateIO(State.inputState set InputState.Default)
          } yield ()
        case _ => IO.ioUnit
      }

    def closestLinkDsq(r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
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

    def closestValidLinkDsq(from: ResidueId, r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
      val links = for {
        ge <- r.graphEntry.toIterable
        (offset, i) <- t.props.dc.outline(ge.residue).zipWithIndex
        endOutgoing = ge.residue.rt == ResidueType.End && i == 0
        beginAny = ge.residue.rt == ResidueType.Begin
        if !endOutgoing && !beginAny
        (ox, oy) = rotatePointRadians(offset, math.toRadians(ge.rotation))
        (dx, dy) = (ge.x + ox - x, ge.y + oy - y)
        dsq = dx * dx + dy * dy if dsq < tsq
      } yield Link(r, i + 1) -> dsq
      links.nonEmpty option links.minBy(_._2)
    }

    def closestValidLink(from: ResidueId, x: Double, y: Double, threshold: Double = 400): Option[Link] = {
      import scalaz.syntax.std.boolean._
      val g = t.props.graph.value
      val links = for {
        r <- g.residues.keys
        if r.id != from.id
        ge <- g.residues.get(r)
        linkDsq <- closestValidLinkDsq(from, r, x, y, threshold)
      } yield linkDsq
      links.nonEmpty option links.minBy(_._2)._1
    }

    def linkPosition(link: Link): Option[(Double, Double)] = {
      for {
        GraphEntry(residue, x, y, rot, _, _) <- graph.residues.get(link.r)
      } yield {
        val offset = t.props.dc.outline(residue)(link.position - 1)
        val (ox, oy) = rotatePointRadians(offset, math.toRadians(rot))
        (x + ox, y + oy)
      }
    }

    def mouseMove(e: ReactMouseEvent): IO[Unit] =
      if (!t.props.limitUpdateRate || updateReady()) (t.props.mode.value, t.state.inputState) match {
        case (Mode.Selection, BoxSelect(down, _)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case pos =>
              t.modStateIO(State.inputState set InputState.BoxSelect(down, pos))
          }
        case (Mode.PlaceResidue(residue), _) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val (_, w, h) = t.props.dc.boundsMemo(residue)
              val dsqThreshold: Double = 500 * 500
              val facing: (Double, Double) = (1, 0)
              val outline = t.props.dc.outline(residue)
              val outlinePositions = for (((ox, oy), i) <- outline.zipWithIndex) yield (x + ox, y + oy)
              val (hx, hy) = outlinePositions.head

              val lefts = for {
                (id, ge) <- graph.residues
                if ge.parent.isEmpty
                linkPos @ (lx, ly) <- linkPosition(Link(id, 1))
                if facing._1 * (lx - hx) + facing._2 * (ly - hy) < 0
                if ge.residue.rt != ResidueType.End

                (dx, dy) = (lx - x + w / 2, ly - y + h / 2)
                dsq = dx * dx + dy * dy
                //todo: limit using bounds or closest link
                if dsq < dsqThreshold
              } yield (id, linkPos, dsq)

              def linkMappings(src: Seq[(ResidueId, (Double, Double), Double)], dst: Seq[((Double, Double), Int)]): Map[Int, ResidueId] = {
                src match {
                  case (id, (x1, y1), _) :: rest =>
                    val mapping = for {
                      (_, i) <- dst.nonEmpty option dst.minBy {
                        case ((x2, y2), _) =>
                          val (dx, dy) = (x2 - x1, y2 - y1)
                          dx * dx + dy * dy
                      }
                      if i != 0
                    } yield (i + 1) -> id
                    linkMappings(rest, dst.filterNot(m => mapping.exists(_._1 == m._2))) ++ mapping
                  case _ => Map.empty
                }
              }
              val children = linkMappings(lefts.toList.sortBy(_._3), outlinePositions.zipWithIndex)

              val parents = for {
                (id, ge) <- graph.residues
                if linkPosition(Link(id, 1)).exists {
                  case (thx, thy) =>
                    facing._1 * (thx - hx) + facing._2 * (thy - hy) >= 0
                }
                i <- 2 to ge.residue.rt.linkage
                if !ge.children.contains(i)
                link = Link(id, i)
                (tx, ty) <- linkPosition(link)
                (dx, dy) = (tx - hx, ty - hy)
                dsq = dx * dx + dy * dy
                if dsq < dsqThreshold
              } yield (link, dsq)
              val parent = (residue.rt != ResidueType.End && parents.nonEmpty option parents.minBy(_._2)).map(_._1)

              for {
                _ <- t.modStateIO(State.inputState set InputState.AddResidue(x, y, children, parent))
              } yield ()
          }
        case (Mode.PlaceSubstituent(_), _) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val link = closestLink(x, y)
              t.modStateIO(State.inputState set InputState.AddSubstituent(x, y, link))
          }
        case (Mode.PlaceAnnotation, _) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              t.modStateIO(State.inputState set InputState.AddAnnotation(x, y))
          }
        case (Mode.Selection, InputState.Drag(down @ (x0, y0), _)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val offset = (x - x0, y - y0)
              t.modStateIO(State.inputState set InputState.Drag(down, offset))
          }
        case (_, InputState.DragView(down @ (x0, y0), (ox, oy))) =>
          if (true/*e.shiftKey*/) clientToCanvasIO(e.clientX, e.clientY) {
            case (x, y) =>
              val offset = (-(x - x0) / t.props.view.value.scale, -(y - y0) / t.props.view.value.scale)
              t.modStateIO(State.inputState set InputState.DragView(down, offset))
          } else {
            for {
              _ <- t.modStateIO(State.inputState set InputState.Default)
              _ <- t.props.view.mod({ View.x modify (_ + ox) } andThen { View.y modify (_ + oy) })
            } yield ()
          }
        case (Mode.Selection, InputState.CreateBond(r, last, _)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val target = closestValidLink(r, x, y)
              t.modStateIO(State.inputState set InputState.CreateBond(r, (x, y), target))
          }
        case _ => IO.ioUnit
      } else IO.ioUnit

    def inBounds(x: Double, y: Double) =
      0 <= x && x < t.props.view.value.width &&
        0 <= y && y < t.props.view.value.height

    def mouseOut(e: ReactMouseEvent): IO[Unit] =
      t.props.mode.value match {
        case Mode.PlaceResidue(_) | Mode.PlaceSubstituent(_) | Mode.PlaceAnnotation =>
          clientToCanvasIO(e.clientX, e.clientY) {
            case (x, y) if !inBounds(x, y) => t.modStateIO(State.inputState set InputState.Out)
            case _ => IO.ioUnit
          }
        case _ => IO.ioUnit
      }

    def boxSelectDown(e: ReactMouseEvent): IO[Unit] = {
      if (e.shiftKey) {
        t.state.inputState match {
          case InputState.DragView(_, _) => IO.ioUnit
          case _ =>
            clientToCanvasIO(e.clientX, e.clientY) { down =>
              t.modStateIO(State.inputState set InputState.DragView(down, (0, 0)))
            }
        }
      } else {
        (button(e), t.props.mode.value) match {
          case (Mouse.Left, Selection) =>
            clientToViewIO(e.clientX, e.clientY) { down =>
              t.modStateIO(State.inputState set InputState.BoxSelect(down, down))
            }
          case _ => IO.ioUnit
        }
      }
    }

    def mouseUp(e: ReactMouseEvent): IO[Unit] =
      (t.props.mode.value, t.state.inputState) match {
        case (Selection, BoxSelect((x1, y1), (x2, y2))) =>
          val (xMin, xMax) = if (x1 < x2) (x1, x2) else (x2, x1)
          val (yMin, yMax) = if (y1 < y2) (y1, y2) else (y2, y1)
          def inSelection(x: Double, y: Double) = xMin <= x && x < xMax && yMin <= y && y < yMax
          val residues = t.props.graph.value.residues.filter(e => inSelection(e._2.x, e._2.y)).keySet
          val annotations = t.props.graph.value.annotations.filter(e => inSelection(e._2.x, e._2.y)).keySet
          for {
            _ <- t.modStateIO(State.inputState set InputState.Default)
            _ <- t.props.selection.set((residues, annotations))
          } yield ()
        case (Mode.Selection, InputState.Drag(_, (dx, dy))) =>
          for {
            _ <- if (dx != 0 || dy != 0) {
              val (rs, as) = t.props.selection.value
              t.props.graph.mod { graph =>
                val g2 = graph.residues.filterKeys(rs.contains).foldLeft(graph) {
                  case (g, (r, ge)) =>
                    g.updated(r, Placement(ge.x + dx, ge.y + dy, ge.rotation))
                }
                val g3 = g2.annotations.filterKeys(as.contains).foldLeft(g2) {
                  case (g, (id, annot)) =>
                    (RGraph.annotations ^|-? index(id) modify (a => a.copy(x = a.x + dx, y = a.y + dy)))(g)
                }
                g3
              }
            } else IO.ioUnit
            _ <- t.modStateIO(State.inputState set InputState.Default)
          } yield ()
        case (_, InputState.DragView(_, (ox, oy))) =>
          for {
            _ <- t.modStateIO(State.inputState set InputState.Default)
            _ <- t.props.view.mod({ View.x modify (_ + ox) } andThen { View.y modify (_ + oy) })
          } yield ()
        case (Mode.Selection, InputState.PreCreateBond(r)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case to =>
              t.modStateIO(State.inputState set InputState.CreateBond(r, to, None))
          }
        case _ => IO.ioUnit
      }

    def residueMouseDown(r: ResidueId)(e: ReactMouseEvent): Unit =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Left, Mode.Selection, InputState.Default) =>
          for (down <- clientToView(e.clientX, e.clientY)) {
            t.modState(State.inputState set InputState.Drag(down, (0.0, 0.0)))
            if (!t.props.selection.value._1.contains(r))
              t.props.selection.set((Set(r), Set.empty)).unsafePerformIO()
          }
        case _ =>
      }

    def annotationMouseDown(id: AnnotId)(e: ReactMouseEvent): IO[Unit] =
      (button(e), t.props.mode.value, t.state.inputState) match {
        case (Mouse.Left, Mode.Selection, InputState.Default) =>
          clientToViewIO(e.clientX, e.clientY) {
            case down =>
              t.modState(State.inputState set InputState.Drag(down, (0.0, 0.0)))
              if (!t.props.selection.value._2.contains(id))
                t.props.selection.set((Set.empty, Set(id)))
              else IO.ioUnit
          }
        case _ => IO.ioUnit
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
    case class AddResidue(x: Double, y: Double, children: Map[Int, ResidueId], parent: Option[Link]) extends InputState
    case class AddSubstituent(x: Double, y: Double, target: Option[Link]) extends InputState
    case class BoxSelect(from: (Double, Double), to: (Double, Double)) extends InputState
    case class Drag(down: (Double, Double), offset: (Double, Double)) extends InputState
    case class DragView(down: (Double, Double), offset: (Double, Double)) extends InputState
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

  val Annotation = ReactComponentB[(ReactMouseEvent => IO[Unit], AnnotId, Annot, Boolean)]("Annotation")
    .initialState[(Double, Double)]((0, 0))
    .render { $ =>
      val (io, id, Annot(text, size, x, y, rot), selected) = $.props
      val (bw, bh) = $.state
      <.svg.g(
        ^.onMouseDown ~~> io,
        <.svg.text(text)(
          ^.svg.pointerEvents := "none",
          ^.svg.fontSize := size,
          ^.svg.x := x,
          ^.svg.y := y,
          ^.svg.transform := s"rotate($rot)"
        ),
        selected ?= <.svg.rect(
          ^.svg.x := x - 3, ^.svg.y := y - 3 - bh + 4,
          ^.svg.width := bw + 6, ^.svg.height := bh + 6,
          ^.svg.rx := 3, ^.svg.ry := 3,
          ^.svg.fill := "#404080", ^.svg.fillOpacity := "50%",
          ^.svg.stroke := "#404080", ^.svg.strokeWidth := 1
        )
      )
    }
    .componentDidMount { $ =>
      val e = $.getDOMNode().asInstanceOf[dom.svg.G]
      val bb = e.firstElementChild.asInstanceOf[dom.svg.Text].getBBox()
      $.setState((bb.width, bb.height))
    }
    .componentDidUpdate {
      case (scope, props, state) =>
        val annot1 = props._3
        val annot2 = scope.props._3
        if (annot1.text != annot2.text || annot1.size != annot2.size) {
          val e = scope.getDOMNode().asInstanceOf[dom.svg.G]
          val bb = e.firstElementChild.asInstanceOf[dom.svg.Text].getBBox()
          scope.setState((bb.width, bb.height))
        }
    }
    .shouldComponentUpdate((T, P, S) => T.props._2 != P._2 || T.props._3 != P._3 || T.props._4 != P._4 || T.state != S)
    .build

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def apply(props: Props, children: ReactNode*) = component.apply(props, children)
  val component = ReactComponentB[Props]("GlycanoCanvas")
    .initialState(State())
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      implicit val graph: RGraph = P.graph.value

      val (voX, voY) = S.inputState match {
        case InputState.DragView(down, offset) => offset
        case _ => (0.0, 0.0)
      }
      val viewX = P.view.value.x + voX
      val viewY = P.view.value.y + voY
      val viewScale = P.view.value.scale
      val viewWidth = P.view.value.width
      val viewHeight = P.view.value.height

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
        val anomer = ge.residue.rt match {
          case ResidueType.Begin => rootAnomer(r)
          case _ => ge.residue.ano
        }
        SVGBond.withKey("bond" + r.id)(SVGBond.Props(anomer, Some(i), from, to, P.bondLabels))
      }

      val tempBonds = (P.mode.value, S.inputState) match {
        case (Mode.Selection, InputState.CreateBond(r, mouse, target)) =>
          for (ge <- r.graphEntry.toSeq) yield {
            val from = outlinePos(outlines(r), ge, 1)
            val targetLink = for {
              Link(rLink, pos) <- target
              geLink <- rLink.graphEntry
            } yield outlinePos(outlines(rLink), geLink, pos)
            val to = targetLink getOrElse mouse
            SVGBond.withKey("tempBond")(SVGBond.Props(ge.residue.ano, target.map(_.position), from, to, P.bondLabels))
          }
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y, children, parent)) =>
          val from = for {
            (i, id) <- children.toSeq
            ge <- graph.residues.get(id)
          } yield {
            val fromPos = outlinePos(outlines(id), ge, 1)
            val (ox, oy) = P.dc.outline(residue)(i - 1)
            val toPos = (x + ox, y + oy)
            SVGBond.withKey("tempBond" + i)(SVGBond.Props(residue.ano, Some(i), fromPos, toPos, P.bondLabels))
          }

          val to = for {
            Link(id, i) <- parent.toSeq
            ge <- graph.residues.get(id)
          } yield {
            val (ox, oy) = P.dc.outline(residue).head
            val fromPos = (x + ox, y + oy)
            val toPos = outlinePos(outlines(id), ge, i)
            SVGBond.withKey("tempBond1")(SVGBond.Props(residue.ano, Some(i), fromPos, toPos, P.bondLabels))
          }

          from ++ to
        case _ =>
          Seq.empty
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
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y, _, _)) =>
          val ge = GraphEntry(residue, x, y, 0)
          Some(SVGResidue.withKey("tempResidue")(SVGResidue.Props(_ => (), _ => (), ge, P.dc, selected = false, P.scaleSubstituents)))
        case _ => None
      }

      val annotations = for ((id, annot) <- P.graph.value.annotations.toSeq) yield {
        val selected = P.selection.value._2.contains(id)
        val annot2 = if (drag && selected) annot.copy(x = annot.x + dx, y = annot.y + dy) else annot
        Annotation.withKey(id.id)((B.annotationMouseDown(id), id, annot2, selected))
      }

      val tempAnnotation = (P.mode.value, S.inputState) match {
        case (Mode.PlaceAnnotation, InputState.AddAnnotation(x, y)) =>
          Some(<.svg.text("Annotation")(
            ^.svg.fontSize := P.annotationFontSize,
            ^.svg.fillOpacity := "50%",
            ^.svg.x := x, ^.svg.y := y,
            ^.svg.pointerEvents := "none"
          ))
        case _ => None
      }

      <.svg.svg(
        ^.svg.width := viewWidth,
        ^.svg.height := viewHeight,
        ^.ref := "canvas",
        ^.id := "canvas",
        ^.onMouseMove ~~> B.mouseMove _,
        ^.onClick ~~> B.mouseClick _,
        ^.onMouseOut ~~> B.mouseOut _,
        ^.onMouseUp ~~> B.mouseUp _,
        ^.onMouseDown ~~> B.mouseDown _
      )(
        <.svg.g(^.svg.transform := s"translate(${viewWidth / 2} ${viewHeight / 2}) scale($viewScale) translate(${-viewX} ${-viewY})", ^.ref := "view")(
          <.svg.rect(
            ^.svg.transform := s"translate($viewX, $viewY) scale(${1.0 / viewScale}) translate(${-viewWidth / 2}, ${-viewHeight / 2})",
            ^.svg.fill := "white",
            ^.svg.width := P.view.value.width,
            ^.svg.height := P.view.value.height
          ),
          bonds,
          tempBonds,
          <.svg.rect(
            ^.svg.transform := s"translate($viewX, $viewY) scale(${1.0 / viewScale}) translate(${-viewWidth / 2}, ${-viewHeight / 2})",
            ^.svg.fill := "transparent",
            ^.svg.width := P.view.value.width,
            ^.svg.height := P.view.value.height,
            ^.onMouseDown ~~> B.boxSelectDown _
          ),
          <.svg.g(^.ref := "fit")(
            residues,
            tempSubstituent,
            selectionBox,
            tempResidue,
            annotations,
            tempAnnotation
          )
        )
      )
    })
    .shouldComponentUpdate {
      (T, P, S) =>
        cmp(T.props) != cmp(P) || T.state != S
    }
    .componentDidMount { $ =>
      $.getDOMNode().addEventListener("contextmenu", (e: org.scalajs.dom.Event) => {
        e.preventDefault()
        false
      })
    }
    .componentDidUpdate((scope, props, state) => {
      for {
        fit <- scope.refs[dom.html.Element]("fit").map(_.getDOMNode().asInstanceOf[dom.svg.G])

      } {
        val bb = fit.getBBox()
        val fb = (bb.x, bb.y, bb.width, bb.height)
        scope.props.fitBounds.set(fb).unsafePerformIO()
      }
    })
    //.domType[dom.SVGSVGElement]
    .build
}
