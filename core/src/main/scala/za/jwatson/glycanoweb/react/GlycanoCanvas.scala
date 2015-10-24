package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Lens
import monocle.Monocle._
import monocle.macros.Lenses
import org.scalajs.dom
import org.scalajs.dom.raw.SVGRect
import za.jwatson.glycanoweb._
import za.jwatson.glycanoweb.react.GlycanoCanvas.InputState.BoxSelect
import za.jwatson.glycanoweb.react.GlycanoApp._
import GlycanoApp.Mode.Select
import za.jwatson.glycanoweb.render.{DisplayConv, SubstituentShape}
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scalaz.effect.IO
import scalaz.effect.IO.IOMonoid
import scalaz.std.anyVal.unitInstance
import scalaz.std.vector._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.{Monoid, Semigroup}

object GlycanoCanvas {
  val refBounds = Ref[dom.svg.G]("bounds")

  case class Props(rvMode: ReusableVar[Mode], rvSelection: ReusableVar[Selection],
                   rvBounds: ReusableVar[Option[Bounds]], rvGraph: ReusableVar[RGraph],
                   rvView: ReusableVar[View], snap: Snap, annotationFontSize: Double,
                   displayConv: DisplayConv, limitUpdateRate: Boolean, scaleSubstituents: Double,
                   highlightBond: Option[ResidueId], bondLabels: Boolean)
  implicit val reuseDouble = Reusability.by_==[Double]
  implicit val reuseProps = Reusability.caseClass[Props]

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


  object Mouse {
    val Left = 0
    val Middle = 1
    val Right = 2
  }

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1, width: Int = 800, height: Int = 600) {
    def fitBounds(bounds: Bounds): View = {
      val sx = width / bounds.width
      val sy = height / bounds.height
      val fitScale = math.min(sx, sy)
      val fitX = bounds.x + bounds.width / 2
      val fitY = bounds.y + bounds.height / 2
      View(fitX, fitY, fitScale * 0.975, width, height)
    }
  }

  object View {
    implicit val reusability: Reusability[View] = Reusability.by_==
  }

  case class Bounds(x: Double, y: Double, width: Double, height: Double)

  object Bounds {
    implicit val reusability: Reusability[Bounds] = Reusability.by_==
  }

  class Backend($: BackendScope[Props, InputState]) extends OnUnmount {

    def clientToViewCB(x: Double, y: Double)(f: ((Double, Double)) => CallbackOption[Unit]) =
      CallbackOption.liftOptionLike(clientToView(x, y)).flatMap(f)
    def clientToView(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.svg.SVG]("canvas")($)
      view <- Ref[dom.svg.G]("view")($)
    } yield {
      val p = svg.createSVGPoint(); p.x = x; p.y = y
      val p2 = p.matrixTransform(view.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def clientToCanvasCB(x: Double, y: Double)(f: ((Double, Double)) => CallbackOption[Unit]) =
      CallbackOption.liftOptionLike(clientToCanvas(x, y)).flatMap(f)
    def clientToCanvas(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.svg.SVG]("canvas")($)
    } yield {
      val p = svg.createSVGPoint(); p.x = x; p.y = y
      val p2 = p.matrixTransform(svg.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def setRVarL[From : Reusability, To](rv: CallbackTo[ReusableVar[From]], lens: Lens[From, To]) =
      ReusableFn((from: From, to: To) => rv.flatMap(_.set(lens.set(to)(from))))
    def modRVarL[From : Reusability, To](rv: CallbackTo[ReusableVar[From]], lens: Lens[From, To]) =
      ReusableFn((from: From, f: To => To) => rv.flatMap(_.set(lens.modify(f)(from))))

    val setInputStateFn = ReusableFn($).setState
    val clientToViewFn = ReusableFn((clientToView _).tupled)

    val resetMode = for {
      p <- $.props
      _ <- p.rvMode.set(Mode.Select)
      _ <- $.setState(InputState.Default)
    } yield ()

    def render(props: Props, state: InputState) = {
      implicit val graph: RGraph = props.rvGraph.value
      val selection = props.rvSelection.value
      val mode = props.rvMode.value
      val view = props.rvView.value

      def selectionCentroid: (Double, Double) = {
        val residuePositions = for {
          r <- selection._1.toSeq
          ge <- r.graphEntry
        } yield (ge.x, ge.y)
        val annotationPositions = for {
          a <- selection._2.toSeq
          annot <- graph.annotations.get(a)
        } yield (annot.x, annot.y)
        val positions = residuePositions ++ annotationPositions
        val (sumX, sumY) = positions.foldLeft((0.0, 0.0))({ case ((x0, y0), (x1, y1)) => (x0 + x1, y0 + y1) })
        (sumX / positions.size, sumY / positions.size)
      }

      def snap(offset: (Double, Double), center: (Double, Double)): (Double, Double) = {
        val (ox, oy) = offset
        val (cx, cy) = center
        val snappedX = math.round((cx + ox) / props.snap.gridWidth).toDouble * props.snap.gridWidth - cx
        val snappedY = math.round((cy + oy) / props.snap.gridWidth).toDouble * props.snap.gridWidth - cy
        (snappedX, snappedY)
      }

      def rotatePointRadians(point: (Double, Double), radians: Double, around: (Double, Double) = (0, 0)): (Double, Double) = {
        val sin = math.sin(radians)
        val cos = math.cos(radians)

        val tx = point._1 - around._1
        val ty = point._2 - around._2

        val nx = tx * cos - ty * sin
        val ny = tx * sin + ty * cos

        (nx + around._1, ny + around._2)
      }

      def mouseClick(e: ReactMouseEvent): Callback =
        (button(e), mode, state) match {
          case (Mouse.Left, Mode.PlaceResidue(residue), InputState.AddResidue(x, y, children, parent)) =>
            props.rvGraph.mod { g =>
              g + GraphEntry(residue, x, y, 0, children, parent)
            }
          case (Mouse.Left, Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(link))) =>
            props.rvGraph.mod {
              RGraph.residues ^|-? index(link.r) ^|-> GraphEntry.residue ^|-> Residue.subs ^|->
                at(link.position) modify { m => Some(m.orZero :+ st) }
            }
          case (Mouse.Left, Mode.PlaceAnnotation, InputState.AddAnnotation(x, y)) =>
            props.rvGraph.mod {
              val entry = AnnotId.next() -> Annot("Annotation", props.annotationFontSize, x, y)
              RGraph.annotations modify (_ + entry)
            }
          case _ =>
            Callback.empty
        }

      def mouseDown(e: ReactMouseEvent): Callback =
        (button(e), mode, state) match {
          case (Mouse.Right, Mode.PlaceResidue(_), _) => resetMode
          case (Mouse.Right, Mode.PlaceSubstituent(_), _) => resetMode
          case (Mouse.Right, Mode.PlaceAnnotation, _) => resetMode
          case (Mouse.Right, Mode.CreateBond, _) => resetMode
          case (Mouse.Right, Mode.Select, InputState.CreateBond(_, _, _)) =>
            $.setState(InputState.Default)
          case (Mouse.Left, Mode.Select, InputState.CreateBond(from, (x, y), target)) =>
            for {
              to <- CallbackOption.liftOption(target)
              _ <- props.rvGraph.mod(RGraph.addBondRemovingOld(from.r)(to).exec)
              _ <- $.setState(InputState.Default)
            } yield ()
          case (Mouse.Left, Mode.CreateBond, InputState.CreateBondFree(rOpt, to)) =>
            for {
              _ <- $.setState(rOpt.fold[InputState](InputState.Default)(r => InputState.CreateBond(Link(r, 1), to, None)))
              _ <- props.rvMode.set(Mode.Select)
            } yield ()
          case _ => Callback.empty
        }

      def closestLinkDsq(r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
        val links = for {
          ge <- r.graphEntry.toIterable
          residueLinks = props.displayConv.links(ge.residue)
          i <- 1 to ge.residue.rt.linkage
          (lx, ly) = props.displayConv.linkPos(residueLinks, ge, i)
          (dx, dy) = (lx - x, ly - y)
          dsq = dx * dx + dy * dy if dsq < tsq
        } yield Link(r, i) -> dsq
        links.nonEmpty option links.minBy(_._2)
      }

      def closestLinkHandle(x: Double, y: Double): Option[ResidueId] = {
        val inRange = for {
          r <- graph.residues.keys.toSeq
          ge <- r.graphEntry
          residueLinks = props.displayConv.links(ge.residue)
          (lx, ly) = props.displayConv.linkPos(residueLinks, ge, 1)
          (dx, dy) = (lx - x, ly - y)
          dsq = dx * dx + dy * dy if dsq < 100 * 100
        } yield r -> dsq
        inRange.sortBy(_._2).headOption.map(_._1)
      }

      def closestLink(x: Double, y: Double, threshold: Double = 400): Option[Link] = {
        import scalaz.syntax.std.boolean._
        val links = for {
          r <- graph.residues.keys
          linkDsq <- closestLinkDsq(r, x, y, threshold)
        } yield linkDsq
        links.nonEmpty option links.minBy(_._2)._1
      }

      def closestValidLinkDsq(from: ResidueId, r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
        val links = for {
          ge <- r.graphEntry.toIterable
          residueLinks = props.displayConv.links(ge.residue)
          i <- 1 to ge.residue.rt.linkage
          (lx, ly) = props.displayConv.linkPos(residueLinks, ge, i)
          endOutgoing = ge.residue.rt == ResidueType.End && i == 0
          beginAny = ge.residue.rt == ResidueType.Begin
          if !endOutgoing && !beginAny
          (dx, dy) = (lx - x, ly - y)
          dsq = dx * dx + dy * dy if dsq < tsq
        } yield Link(r, i) -> dsq
        links.nonEmpty option links.minBy(_._2)
      }

      def closestValidLink(from: ResidueId, x: Double, y: Double, threshold: Double = 400): Option[Link] = {
        import scalaz.syntax.std.boolean._
        val links = for {
          r <- graph.residues.keys
          if r.id != from.id
          ge <- graph.residues.get(r)
          linkDsq <- closestValidLinkDsq(from, r, x, y, threshold)
        } yield linkDsq
        links.nonEmpty option links.minBy(_._2)._1
      }

      def linkPosition(link: Link): Option[(Double, Double)] = {
        for {
          ge <- graph.residues.get(link.r)
        } yield {
          props.displayConv.linkPos(props.displayConv.links(ge.residue), ge, link.position)
        }
      }

      def mouseMove(e: ReactMouseEvent): Callback =
        if (!props.limitUpdateRate || updateReady()) (mode, state) match {
          case (Mode.Select, BoxSelect(down, _)) =>
            clientToViewCB(e.clientX, e.clientY) {
              case pos =>
                $.setState(InputState.BoxSelect(down, pos))
            }
          case (Mode.PlaceResidue(residue), _) =>
            clientToViewCB(e.clientX, e.clientY) {
              case viewPos =>
                val (x, y) = if (props.snap.snapToGrid) snap(viewPos, (0, 0)) else viewPos
                val ((xm, ym), w, h) = props.displayConv.bounds(residue)
                val dsqThreshold: Double = 500 * 500
                val facing: (Double, Double) = (1, 0)
                val residueLinks = props.displayConv.links(residue)
                val linkPositions = for (((ox, oy), i) <- residueLinks.zipWithIndex) yield (x + ox - (xm + w / 2.0), y + oy - (ym + h / 2.0))
                val (hx, hy) = linkPositions.head

                val lefts = for {
                  (id, ge) <- graph.residues
                  if ge.parent.isEmpty
                  linkPos @ (lx, ly) <- linkPosition(Link(id, 1))
                  if facing._1 * (lx - hx) + facing._2 * (ly - hy) < 0
                  if ge.residue.rt != ResidueType.End

                  (dx, dy) = (lx - x, ly - y)
                  dsq = dx * dx + dy * dy
                  //todo: limit using bounds or closest link
                  if dsq < dsqThreshold
                } yield (id, linkPos, dsq)

                def linkMappings(src: List[(ResidueId, (Double, Double), Double)], dst: List[((Double, Double), Int)]): Map[Int, ResidueId] = {
                  src match {
                    case (id, (x1, y1), _) :: rest =>
                      val mapping = for {
                        (_, i) <- dst.nonEmpty option dst.minBy {
                          case ((x2, y2), _) =>
                            val (dx, dy) = (x2 - x1, y2 - y1)
                            dx * dx + dy * dy
                        }
                      } yield (i + 1) -> id
                      linkMappings(rest, dst.filterNot(m => mapping.exists(_._1 - 1 == m._2))) ++ mapping
                    case _ => Map.empty
                  }
                }
                val children = linkMappings(lefts.toList.sortBy(_._3), linkPositions.zipWithIndex.toList.tail)

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

                $.setState(InputState.AddResidue(x, y, children, parent))
            }
          case (Mode.PlaceSubstituent(_), _) =>
            clientToViewCB(e.clientX, e.clientY) {
              case (x, y) =>
                val link = closestLink(x, y)
                $.setState(InputState.AddSubstituent(x, y, link))
            }
          case (Mode.PlaceAnnotation, _) =>
            clientToViewCB(e.clientX, e.clientY) {
              case viewPos =>
                val (x, y) = if (props.snap.snapToGrid) snap(viewPos, (0, 0)) else viewPos
                $.setState(InputState.AddAnnotation(x, y))
            }
          case (Mode.Select, InputState.Drag(down @ (x0, y0), _, center)) =>
            clientToViewCB(e.clientX, e.clientY) {
              case (x, y) =>
                val offset = (x - x0, y - y0)
                val snappedOffset = if (props.snap.snapToGrid) snap(offset, center) else offset
                $.setState(InputState.Drag(down, snappedOffset, center))
            }
          case (_, InputState.DragView(down @ (x0, y0), (ox, oy))) =>
            if (true/*e.shiftKey*/) clientToCanvasCB(e.clientX, e.clientY) {
              case (x, y) =>
                val offset = (-(x - x0) / view.scale, -(y - y0) / view.scale)
                $.setState(InputState.DragView(down, offset))
            } else {
              for {
                _ <- $.setState(InputState.Default)
                _ <- props.rvView.mod({ View.x modify (_ + ox) } andThen { View.y modify (_ + oy) })
              } yield ()
            }
          case (Mode.Select, InputState.CreateBond(from, last, _)) =>
            clientToViewCB(e.clientX, e.clientY) {
              case (x, y) =>
                val target = closestValidLink(from.r, x, y)
                $.setState(InputState.CreateBond(from, (x, y), target))
            }
          case (Mode.Select, InputState.Rotate(r, _)) =>
            for {
              ge: GraphEntry <- CallbackOption.liftOption(r.graphEntry)
              _ <- clientToViewCB(e.clientX, e.clientY) {
                case (x, y) =>
                  val rot = math.toDegrees(js.Math.atan2(y - ge.y, x - ge.x)) + 90
                  val snapped = if (props.snap.snapRotation) {
                    val deg = props.snap.snapRotationDegrees
                    val rounded = math.round(rot / deg).toDouble * deg
                    rounded
                  } else rot
                  $.setState(InputState.Rotate(r, snapped))
              }
            } yield ()
          case (Mode.CreateBond, _) =>
            clientToViewCB(e.clientX, e.clientY) {
              case to @ (x, y) =>
                val closest = closestLinkHandle(x, y)
                $.setState(InputState.CreateBondFree(closest, to))
            }
          case _ => Callback.empty
        } else Callback.empty

      def inBounds(x: Double, y: Double) =
        0 <= x && x < view.width &&
          0 <= y && y < view.height

      def mouseOut(e: ReactMouseEvent): Callback =
        mode match {
          case Mode.PlaceResidue(_) | Mode.PlaceSubstituent(_) | Mode.PlaceAnnotation =>
            clientToCanvasCB(e.clientX, e.clientY) {
              case (x, y) if !inBounds(x, y) => $.setState(InputState.Out)
              case _ => Callback.empty
            }
          case _ => Callback.empty
        }

      def boxSelectDown(e: ReactMouseEvent): Callback = {
        if (e.shiftKey) {
          state match {
            case InputState.DragView(_, _) => Callback.empty
            case _ =>
              clientToCanvasCB(e.clientX, e.clientY) { down =>
                $.setState(InputState.DragView(down, (0, 0)))
              }
          }
        } else {
          (button(e), mode) match {
            case (Mouse.Left, Select) =>
              clientToViewCB(e.clientX, e.clientY) { down =>
                $.setState(InputState.BoxSelect(down, down))
              }
            case _ => Callback.empty
          }
        }
      }

      def mouseUp(e: ReactMouseEvent): Callback =
        (mode, state) match {
          case (Select, BoxSelect((x1, y1), (x2, y2))) =>
            val (xMin, xMax) = if (x1 < x2) (x1, x2) else (x2, x1)
            val (yMin, yMax) = if (y1 < y2) (y1, y2) else (y2, y1)
            def inSelection(x: Double, y: Double) = xMin <= x && x < xMax && yMin <= y && y < yMax
            val residues = graph.residues.filter(e => inSelection(e._2.x, e._2.y)).keySet
            val annotations = graph.annotations.filter(e => inSelection(e._2.x, e._2.y)).keySet
            for {
              _ <- $.setState(InputState.Default)
              _ <- props.rvSelection.set((residues, annotations))
            } yield ()
          case (Mode.Select, InputState.Drag(_, (dx, dy), _)) =>
            for {
              _ <- if (dx != 0 || dy != 0) {
                val (rs, as) = selection
                props.rvGraph.mod { graph =>
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
              } else Callback.empty
              _ <- $.setState(InputState.Default)
            } yield ()
          case (_, InputState.DragView(_, (ox, oy))) =>
            for {
              _ <- $.setState(InputState.Default)
              _ <- props.rvView.mod({ View.x modify (_ + ox) } andThen { View.y modify (_ + oy) })
            } yield ()
          case (Mode.Select, InputState.PreCreateBond(r)) =>
            clientToViewCB(e.clientX, e.clientY) {
              case to =>
                $.setState(InputState.CreateBond(Link(r, 1), to, None))
            }
          case (Mode.Select, InputState.Rotate(r, rot)) =>
            for {
              _ <- $.setState(InputState.Default)
              _ <- props.rvGraph.mod(RGraph.residues ^|-? index(r) ^|-> GraphEntry.rotation set rot)
            } yield ()
          case _ => Callback.empty
        }

      def annotationMouseDown(id: AnnotId)(e: ReactMouseEvent): Callback = (button(e), mode, state) match {
        case (Mouse.Left, Mode.Select, InputState.Default) =>
          for {
            _ <- clientToViewCB(e.clientX, e.clientY) {
              case down =>
                val annot = graph.annotations(id)
                val center = (annot.x, annot.y)
                $.setState(InputState.Drag(down, (0.0, 0.0), center))
            }
            _ <- CallbackOption.require(!selection._2.contains(id))
            _ <- props.rvSelection.set((Set.empty, Set(id)))
          } yield ()
        case _ => Callback.empty
      }

      val rvInputState = setInputStateFn.asVar(state)

      val (voX, voY) = state match {
        case InputState.DragView(down, offset) => offset
        case _ => (0.0, 0.0)
      }
      val viewX = view.x + voX
      val viewY = view.y + voY
      val viewScale = view.scale
      val viewWidth = view.width
      val viewHeight = view.height

      val allLinks = for ((r, ge) <- graph.residues) yield {
        r -> props.displayConv.links(ge.residue)
      }

      val (drag, dx, dy) = state match {
        case InputState.Drag(_, (ox, oy), _) => (true, ox, oy)
        case _ => (false, 0.0, 0.0)
      }

      val entriesOffset = for ((r, ge) <- graph.residues) yield {
        val ge2 = (mode, state) match {
          case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(Link(tr, tp)))) if tr == r =>
            ge &|-> GraphEntry.residue ^|-> Residue.subs ^|-> at(tp) modify { m => Some(m.orZero :+ st) }
          case (Mode.Select, InputState.Rotate(`r`, rot)) =>
            ge.copy(rotation = rot)
          case _ => ge
        }

        val selected = selection._1.contains(r)
        val geOffset = if (drag && selected) ge2.copy(x = ge2.x + dx, y = ge2.y + dy) else ge2
        r -> geOffset
      }

      val bonds = for {
        (r, ge) <- entriesOffset.toSeq
        toLink @ Link(toRes, i) <- ge.parent
      } yield {
          val from = props.displayConv.linkPos(allLinks(r), ge, 1)
          val to = props.displayConv.linkPos(allLinks(toRes), entriesOffset(toRes), i)
          val anomer = ge.residue.rt match {
            case ResidueType.Begin => rootAnomer(r)
            case _ => ge.residue.ano
          }
          val highlight = props.highlightBond.contains(r)
          SVGBond.C.withKey("bond" + r.id)(SVGBond.Props(anomer, Some(i), from, to, props.bondLabels, highlight))
        }

      val tempBonds = (mode, state) match {
        case (Mode.Select, InputState.CreateBond(fromLink, mouse, target)) =>
          for (ge <- fromLink.r.graphEntry.toSeq) yield {
            val from = props.displayConv.linkPos(allLinks(fromLink.r), ge, fromLink.position)
            val targetLink = for {
              Link(rLink, pos) <- target
              geLink <- rLink.graphEntry
            } yield props.displayConv.linkPos(allLinks(rLink), geLink, pos)
            val to = targetLink getOrElse mouse
            SVGBond.C.withKey("tempBond")(SVGBond.Props(ge.residue.ano, target.map(_.position), from, to, props.bondLabels))
          }
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y, children, parent)) =>
          val ((rx, ry), rw, rh) = props.displayConv.bounds(residue)
          val from = for {
            (i, id) <- children.toSeq
            ge <- graph.residues.get(id)
          } yield {
              val fromPos = props.displayConv.linkPos(allLinks(id), ge, 1)
              val (ox, oy) = props.displayConv.links(residue)(i - 1)
              val toPos = (x + ox - (rx + rw / 2.0), y + oy - (ry + rh / 2.0))
              SVGBond.C.withKey("tempBond" + i)(SVGBond.Props(residue.ano, Some(i), fromPos, toPos, props.bondLabels))
            }

          val to = for {
            Link(id, i) <- parent.toSeq
            ge <- graph.residues.get(id)
          } yield {
              val (ox, oy) = props.displayConv.links(residue).head
              val fromPos = (x + ox - (rx + rw / 2.0), y + oy - (ry + rh / 2.0))
              val toPos = props.displayConv.linkPos(allLinks(id), ge, i)
              SVGBond.C.withKey("tempBond1")(SVGBond.Props(residue.ano, Some(i), fromPos, toPos, props.bondLabels))
            }

          from ++ to
        case _ =>
          Seq.empty
      }

      val residues = for ((r, ge) <- entriesOffset.toSeq) yield {
        val selected = selection._1.contains(r)
        SVGResidue.C.withKey("residue" + r.id)(SVGResidue.Props(
          r, ge, props.displayConv, selected, props.scaleSubstituents,
          rvInputState, props.rvMode, ReusableFn(props.rvGraph.mod),
          props.rvSelection.set, clientToViewFn
        ))
      }

      val tempSubstituent = (mode, state) match {
        case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(x, y, None))=>
          val (shape, (w, h)) = SubstituentShape(st)
          val scale = props.scaleSubstituents
          val (mx, my) = (-w / 2.0, -h / 2.0)
          Some(shape(^.svg.transform := s"translate($x, $y) scale($scale) translate($mx, $my)"))
        case _ => None
      }

      val selectionBox = state match {
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

      val tempResidue = (mode, state) match {
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y, _, _)) =>
          val ge = GraphEntry(residue, x, y, 0)
          val tempResidueProps = (ge, props.displayConv, props.scaleSubstituents)
          Some(SVGResidue.T.withKey("tempResidue")(tempResidueProps))
        case _ => None
      }

      val annotations = for ((id, annot) <- graph.annotations.toSeq) yield {
        val selected = selection._2.contains(id)
        val annot2 = if (drag && selected) annot.copy(x = annot.x + dx, y = annot.y + dy) else annot
        Annotation.C.withKey(id.id)((annotationMouseDown(id), id, annot2, selected))
      }

      val tempAnnotation = (mode, state) match {
        case (Mode.PlaceAnnotation, InputState.AddAnnotation(x, y)) =>
          Some(<.svg.text("Annotation")(
            ^.svg.fontSize := props.annotationFontSize,
            ^.svg.fillOpacity := "50%",
            ^.svg.x := x, ^.svg.y := y,
            ^.svg.pointerEvents := "none"
          ))
        case _ => None
      }

      val gw = props.snap.gridWidth * viewScale

      val gridSnapIndicator: Seq[TagMod] = state match {
        case InputState.Drag((downX, downY), (ox, oy), (cx, cy)) if props.snap.snapToGrid =>
          val tx = cx + ox
          val ty = cy + oy
          val lineStyle = Seq(^.svg.strokeWidth := 0.75, ^.svg.stroke := "blue")
          Seq(
            <.svg.line(lineStyle, ^.svg.strokeDasharray := "5,10")(
              ^.svg.x1 := downX, ^.svg.y1 := downY,
              ^.svg.x2 := tx, ^.svg.y2 := ty
            ),
            <.svg.line(lineStyle)(
              ^.svg.x1 := tx - 100, ^.svg.y1 := ty,
              ^.svg.x2 := tx + 100, ^.svg.y2 := ty
            ),
            <.svg.line(lineStyle)(
              ^.svg.x1 := tx, ^.svg.y1 := ty - 100,
              ^.svg.x2 := tx, ^.svg.y2 := ty + 100
            )
          )
        case _ =>
          Seq.empty
      }

      val createBondFreeIndicator = state match {
        case InputState.CreateBondFree(Some(r), _) =>
          for {
            ge <- r.graphEntry
            outline = props.displayConv.links(ge.residue)
            (x, y) = props.displayConv.linkPos(outline, ge, 1)
          } yield <.svg.circle(^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 15, ^.svg.fill := "grey", ^.svg.fillOpacity := "50%")
        case InputState.CreateBondFree(None, (x, y)) =>
          Some(<.svg.circle(^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 15, ^.svg.fill := "grey", ^.svg.fillOpacity := "50%"))
        case _ =>
          None
      }

      <.svg.svg(
        ^.svg.width := viewWidth,
        ^.svg.height := viewHeight,
        ^.ref := "canvas",
        ^.id := "canvas",
        ^.onMouseMove ==> mouseMove,
        ^.onClick ==> mouseClick,
        ^.onMouseOut ==> mouseOut,
        ^.onMouseUp ==> mouseUp,
        ^.onMouseDown ==> mouseDown
      )(
        <.svg.defs(
          <.svg.pattern(
            ^.svg.id := "gridPattern", ^.svg.patternUnits := "userSpaceOnUse",
            ^.svg.x := -viewX * viewScale + viewWidth / 2,
            ^.svg.y := -viewY * viewScale + viewHeight / 2,
            ^.svg.width := gw, ^.svg.height := gw
          ) (
            <.svg.rect(^.svg.width := gw, ^.svg.height := gw, ^.svg.fill := "white"),
            <.svg.path(^.svg.d := s"M0,${gw}V0H$gw", ^.svg.fill := "none", ^.svg.strokeWidth := 0.5, ^.svg.stroke := "black")
          )
        ),
        <.svg.g(
          ^.svg.transform := s"translate(${viewWidth / 2} ${viewHeight / 2}) scale($viewScale) translate(${-viewX} ${-viewY})",
          ^.ref := "view"
        )(
          <.svg.rect(
            ^.svg.transform := s"translate($viewX, $viewY) scale(${1.0 / viewScale}) translate(${-viewWidth / 2}, ${-viewHeight / 2})",
            ^.svg.width := view.width,
            ^.svg.height := view.height,
            ^.svg.fill := (if (props.snap.showGrid) "url(#gridPattern)" else "white")
          ),
          gridSnapIndicator,
          bonds,
          tempBonds,
          <.svg.rect(
            ^.svg.transform := s"translate($viewX, $viewY) scale(${1.0 / viewScale}) translate(${-viewWidth / 2}, ${-viewHeight / 2})",
            ^.svg.fill := "transparent",
            ^.svg.width := view.width,
            ^.svg.height := view.height,
            ^.onMouseDown ==> boxSelectDown
          ),
          <.svg.g(^.ref := refBounds)(
            residues,
            tempSubstituent,
            selectionBox,
            tempResidue,
            annotations,
            tempAnnotation
          ),
          createBondFreeIndicator
        )
      )
    }
  }

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case class AddResidue(x: Double, y: Double, children: Map[Int, ResidueId], parent: Option[Link]) extends InputState
    case class AddSubstituent(x: Double, y: Double, target: Option[Link]) extends InputState
    case class BoxSelect(from: (Double, Double), to: (Double, Double)) extends InputState
    case class Drag(down: (Double, Double), offset: (Double, Double), center: (Double, Double)) extends InputState
    case class DragView(down: (Double, Double), offset: (Double, Double)) extends InputState
    case class PreCreateBond(r: ResidueId) extends InputState
    case class CreateBond(from: Link, to: (Double, Double), target: Option[Link]) extends InputState
    case class CreateBondFree(r: Option[ResidueId], to: (Double, Double)) extends InputState
    case object PostCreateBond extends InputState
    case class Rotate(r: ResidueId, rotation: Double) extends InputState
    case class AddAnnotation(x: Double, y: Double) extends InputState
    case object Out extends InputState

    implicit val reusability: Reusability[InputState] = Reusability.by_==
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  val C = ReactComponentB[Props]("GlycanoCanvas")
    .initialState[InputState](InputState.Default)
    .renderBackend[Backend]
    .domType[dom.svg.SVG]
    .configure(EventListener[dom.Event].install("contextmenu", _ => _.preventDefaultCB))
    .configure(Reusability.shouldComponentUpdate)
    .componentDidUpdate { c =>
      for {
        bounds <- CallbackTo {
          refBounds(c.$).toOption.map { g =>
            val bb = g.getBBox()
            Bounds(bb.x, bb.y, bb.width, bb.height)
          }
        }
        _ <- c.$.props.rvBounds.set(bounds)
      } yield ()
    }
    .build
}
