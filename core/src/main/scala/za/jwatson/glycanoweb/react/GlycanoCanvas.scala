package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
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
import za.jwatson.glycanoweb.react.GlycanoApp.{AppStateL, AppState, Mode}
import GlycanoApp.Mode.Selection
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
  private val updateInterval = 16
  private var lastUpdated: Double = js.Date.now()
  def updateReady(): Boolean = {
    val time = js.Date.now()
    if (time - lastUpdated > updateInterval) {
      lastUpdated = time
      true
    } else false
  }

  implicit val reuseAppState: Reusability[AppState] = Reusability.by_==

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

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1, width: Int = 800, height: Int = 600)

  object View {
    implicit val reusability: Reusability[View] = Reusability.by_==
  }

  case class Bounds(x: Double, y: Double, width: Double, height: Double)

  object Bounds {
    implicit val reusability: Reusability[Bounds] = Reusability.by_==
  }

  class Backend($: BackendScope[ReusableVar[AppState], InputState]) extends OnUnmount {
    def appState: AppState = $.props.value
    implicit def graph: RGraph = appState.graph

    def clientToViewIO(x: Double, y: Double)(f: ((Double, Double)) => IO[Unit]): IO[Unit] =
      clientToView(x, y).fold(IO.ioUnit)(f)
    def clientToView(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.svg.SVG]("canvas")($).map(_.getDOMNode())
      view <- Ref[dom.svg.G]("view")($).map(_.getDOMNode())
    } yield {
      val p = svg.createSVGPoint(); p.x = x; p.y = y
      val p2 = p.matrixTransform(view.getScreenCTM().inverse())
      (p2.x, p2.y)
    }

    def clientToCanvasIO(x: Double, y: Double)(f: ((Double, Double)) => IO[Unit]): IO[Unit] =
      clientToCanvas(x, y).map(f).getOrElse(IO.ioUnit)
    def clientToCanvas(x: Double, y: Double): js.UndefOr[(Double, Double)] = for {
      svg <- Ref[dom.svg.SVG]("canvas")($).map(_.getDOMNode())
    } yield {
      val p = svg.createSVGPoint(); p.x = x; p.y = y
      val p2 = p.matrixTransform(svg.getScreenCTM().inverse())
      (p2.x, p2.y)
    }
    
    def selectionCentroid: (Double, Double) = {
      val residuePositions = for {
        r <- $.props.value.selection._1.toSeq
        ge <- r.graphEntry
      } yield (ge.x, ge.y)
      val annotationPositions = for {
        a <- $.props.value.selection._2.toSeq
        annot <- graph.annotations.get(a)
      } yield (annot.x, annot.y)
      val positions = residuePositions ++ annotationPositions
      val (sumX, sumY) = positions.foldLeft((0.0, 0.0))({ case ((x0, y0), (x1, y1)) => (x0 + x1, y0 + y1) })
      (sumX / positions.size, sumY / positions.size)
    }
    
    def snap(offset: (Double, Double), center: (Double, Double)): (Double, Double) = {
      val (ox, oy) = offset
      val (cx, cy) = center
      val snappedX = math.round((cx + ox) / appState.gridWidth).toDouble * appState.gridWidth - cx
      val snappedY = math.round((cy + oy) / appState.gridWidth).toDouble * appState.gridWidth - cy
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

    def resetMode(): IO[Unit] = for {
      _ <- $.props.setL(AppState.mode)(Mode.Selection)
      _ <- $.setStateIO(InputState.Default)
    } yield ()

    def mouseClick(e: ReactMouseEvent): IO[Unit] =
      (button(e), $.props.value.mode, $.state) match {
        case (Mouse.Left, Mode.PlaceResidue(residue), InputState.AddResidue(x, y, children, parent)) =>
          $.props.modL(AppStateL.graphL) { g =>
            g + GraphEntry(residue, x, y, 0, children, parent)
          }
        case (Mouse.Left, Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(link))) =>
          $.props.modL(AppStateL.graphL) {
            RGraph.residues ^|-? index(link.r) ^|-> GraphEntry.residue ^|-> Residue.subs ^|->
              at(link.position) modify { m => Some(m.orZero :+ st) }
          }
        case (Mouse.Left, Mode.PlaceAnnotation, InputState.AddAnnotation(x, y)) =>
          $.props.modL(AppStateL.graphL) {
            val entry = AnnotId.next() -> Annot("Annotation", $.props.value.annotationFontSize, x, y)
            RGraph.annotations modify (_ + entry)
          }
        case _ =>
          IO.ioUnit
      }

    def mouseDown(e: ReactMouseEvent): IO[Unit] =
      (button(e), $.props.value.mode, $.state) match {
        case (Mouse.Right, Mode.PlaceResidue(_), _) => resetMode()
        case (Mouse.Right, Mode.PlaceSubstituent(_), _) => resetMode()
        case (Mouse.Right, Mode.PlaceAnnotation, _) => resetMode()
        case (Mouse.Right, Mode.Selection, InputState.CreateBond(_, _, _)) =>
          $.setStateIO(InputState.Default)
        case (Mouse.Left, Mode.Selection, InputState.CreateBond(from, (x, y), target)) =>
          for {
            _ <- target.map(to => $.props.modL(AppStateL.graphL)(RGraph.addBondRemovingOld(from)(to).exec)).orZero
            _ <- $.setStateIO(InputState.Default)
          } yield ()
        case _ => IO.ioUnit
      }

    def closestLinkDsq(r: ResidueId, x: Double, y: Double, tsq: Double = Double.MaxValue): Option[(Link, Double)] = {
      val links = for {
        ge <- r.graphEntry.toIterable
        residueLinks = appState.displayConv.links(ge.residue)
        i <- 1 to ge.residue.rt.linkage
        (lx, ly) = appState.displayConv.linkPos(residueLinks, ge, i)
        (dx, dy) = (lx - x, ly - y)
        dsq = dx * dx + dy * dy if dsq < tsq
      } yield Link(r, i) -> dsq
      links.nonEmpty option links.minBy(_._2)
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
        residueLinks = appState.displayConv.links(ge.residue)
        i <- 1 to ge.residue.rt.linkage
        (lx, ly) = appState.displayConv.linkPos(residueLinks, ge, i)
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
      val g = graph
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
        ge <- graph.residues.get(link.r)
      } yield {
        appState.displayConv.linkPos(appState.displayConv.links(ge.residue), ge, link.position)
      }
    }

    def mouseMove(e: ReactMouseEvent): IO[Unit] =
      if (!appState.limitUpdateRate || updateReady()) ($.props.value.mode, $.state) match {
        case (Mode.Selection, BoxSelect(down, _)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case pos =>
              $.setStateIO(InputState.BoxSelect(down, pos))
          }
        case (Mode.PlaceResidue(residue), _) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val ((xm, ym), w, h) = appState.displayConv.bounds(residue)
              val dsqThreshold: Double = 500 * 500
              val facing: (Double, Double) = (1, 0)
              val residueLinks = appState.displayConv.links(residue)
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

              for {
                _ <- $.setStateIO(InputState.AddResidue(x, y, children, parent))
              } yield ()
          }
        case (Mode.PlaceSubstituent(_), _) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val link = closestLink(x, y)
              $.setStateIO(InputState.AddSubstituent(x, y, link))
          }
        case (Mode.PlaceAnnotation, _) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              $.setStateIO(InputState.AddAnnotation(x, y))
          }
        case (Mode.Selection, InputState.Drag(down @ (x0, y0), _, center)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val offset = (x - x0, y - y0)
              val snappedOffset = if (appState.snapToGrid) snap(offset, center) else offset
              $.setStateIO(InputState.Drag(down, snappedOffset, center))
          }
        case (_, InputState.DragView(down @ (x0, y0), (ox, oy))) =>
          if (true/*e.shiftKey*/) clientToCanvasIO(e.clientX, e.clientY) {
            case (x, y) =>
              val offset = (-(x - x0) / $.props.value.view.scale, -(y - y0) / $.props.value.view.scale)
              $.setStateIO(InputState.DragView(down, offset))
          } else {
            for {
              _ <- $.setStateIO(InputState.Default)
              _ <- $.props.modL(AppState.view)({ View.x modify (_ + ox) } andThen { View.y modify (_ + oy) })
            } yield ()
          }
        case (Mode.Selection, InputState.CreateBond(r, last, _)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case (x, y) =>
              val target = closestValidLink(r, x, y)
              $.setStateIO(InputState.CreateBond(r, (x, y), target))
          }
        case (Mode.Selection, InputState.Rotate(r, _)) =>
          (for {
            x1 <- r.x
            y1 <- r.y
            (x2, y2) <- clientToView(e.clientX, e.clientY).toOption
          } yield {
            val rot = math.toDegrees(js.Math.atan2(y2 - y1, x2 - x1)) + 90
            val snapped = if (appState.snapRotation) {
              val deg = appState.snapRotationDegrees
              val rounded = math.round(rot / deg).toDouble * deg
              rounded
            } else rot
            $.setStateIO(InputState.Rotate(r, snapped))
          }).getOrElse(IO.ioUnit)
        case _ => IO.ioUnit
      } else IO.ioUnit

    def inBounds(x: Double, y: Double) =
      0 <= x && x < $.props.value.view.width &&
        0 <= y && y < $.props.value.view.height

    def mouseOut(e: ReactMouseEvent): IO[Unit] =
      $.props.value.mode match {
        case Mode.PlaceResidue(_) | Mode.PlaceSubstituent(_) | Mode.PlaceAnnotation =>
          clientToCanvasIO(e.clientX, e.clientY) {
            case (x, y) if !inBounds(x, y) => $.setStateIO(InputState.Out)
            case _ => IO.ioUnit
          }
        case _ => IO.ioUnit
      }

    def boxSelectDown(e: ReactMouseEvent): IO[Unit] = {
      if (e.shiftKey) {
        $.state match {
          case InputState.DragView(_, _) => IO.ioUnit
          case _ =>
            clientToCanvasIO(e.clientX, e.clientY) { down =>
              $.setStateIO(InputState.DragView(down, (0, 0)))
            }
        }
      } else {
        (button(e), $.props.value.mode) match {
          case (Mouse.Left, Selection) =>
            clientToViewIO(e.clientX, e.clientY) { down =>
              $.setStateIO(InputState.BoxSelect(down, down))
            }
          case _ => IO.ioUnit
        }
      }
    }

    def mouseUp(e: ReactMouseEvent): IO[Unit] =
      ($.props.value.mode, $.state) match {
        case (Selection, BoxSelect((x1, y1), (x2, y2))) =>
          val (xMin, xMax) = if (x1 < x2) (x1, x2) else (x2, x1)
          val (yMin, yMax) = if (y1 < y2) (y1, y2) else (y2, y1)
          def inSelection(x: Double, y: Double) = xMin <= x && x < xMax && yMin <= y && y < yMax
          val residues = graph.residues.filter(e => inSelection(e._2.x, e._2.y)).keySet
          val annotations = graph.annotations.filter(e => inSelection(e._2.x, e._2.y)).keySet
          for {
            _ <- $.setStateIO(InputState.Default)
            _ <- $.props.setL(AppState.selection)((residues, annotations))
          } yield ()
        case (Mode.Selection, InputState.Drag(_, (dx, dy), _)) =>
          for {
            _ <- if (dx != 0 || dy != 0) {
              val (rs, as) = $.props.value.selection
              $.props.modL(AppStateL.graphL) { graph =>
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
            _ <- $.setStateIO(InputState.Default)
          } yield ()
        case (_, InputState.DragView(_, (ox, oy))) =>
          for {
            _ <- $.setStateIO(InputState.Default)
            _ <- $.props.modL(AppState.view)({ View.x modify (_ + ox) } andThen { View.y modify (_ + oy) })
          } yield ()
        case (Mode.Selection, InputState.PreCreateBond(r)) =>
          clientToViewIO(e.clientX, e.clientY) {
            case to =>
              $.setStateIO(InputState.CreateBond(r, to, None))
          }
        case (Mode.Selection, InputState.Rotate(r, rot)) =>
          for {
            _ <- $.setStateIO(InputState.Default)
            _ <- $.props.modL(AppStateL.graphL)(RGraph.residues ^|-? index(r) ^|-> GraphEntry.rotation set rot)
          } yield ()
        case _ => IO.ioUnit
      }

    def annotationMouseDown(id: AnnotId)(e: ReactMouseEvent): IO[Unit] =
      (button(e), $.props.value.mode, $.state) match {
        case (Mouse.Left, Mode.Selection, InputState.Default) =>
          clientToViewIO(e.clientX, e.clientY) {
            case down =>
              val annot = graph.annotations(id)
              val center = (annot.x, annot.y)
              $.setState(InputState.Drag(down, (0.0, 0.0), center))
              if (!$.props.value.selection._2.contains(id))
                $.props.setL(AppState.selection)((Set.empty, Set(id)))
              else IO.ioUnit
          }
        case _ => IO.ioUnit
      }

    def setRVarL[From : Reusability, To](rv: ReusableVar[From], lens: Lens[From, To]) =
      ReusableFn((from: From, to: To) => rv.set(lens.set(to)(from)))
    def modRVarL[From : Reusability, To](rv: ReusableVar[From], lens: Lens[From, To]) =
      ReusableFn((from: From, f: To => To) => rv.set(lens.modify(f)(from)))

    val setInputStateFn = ReusableFn($).setStateIO
    val setModeFn = setRVarL($.props, AppState.mode)
    val setGraphFn = setRVarL($.props, AppStateL.graphL)
    val setSelectionFn = setRVarL($.props, AppState.selection)
    val modGraphFn = modRVarL($.props, AppStateL.graphL)
    val clientToViewFn = ReusableFn((clientToView _).tupled)
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
    case class CreateBond(r: ResidueId, to: (Double, Double), target: Option[Link]) extends InputState
    case object PostCreateBond extends InputState
    case class Rotate(r: ResidueId, rotation: Double) extends InputState
    case class AddAnnotation(x: Double, y: Double) extends InputState
    case object Out extends InputState

    implicit val reusability: Reusability[InputState] = Reusability.by_==
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def apply(props: ReusableVar[AppState], children: ReactNode*) = component.apply(props, children)
  val component = ReactComponentB[ReusableVar[AppState]]("GlycanoCanvas")
    .initialState[InputState](InputState.Default)
    .backend(new Backend(_))
    .render { $ =>
      val appState = $.props.value
      implicit val graph: RGraph = appState.graph

      val rvInputState = $.backend.setInputStateFn.asVar($.state)
      val rvMode = $.backend.setModeFn($.props.value).asVar(appState.mode)
      val rvGraph = $.backend.setGraphFn($.props.value).asVar(appState.graph)

      val (voX, voY) = $.state match {
        case InputState.DragView(down, offset) => offset
        case _ => (0.0, 0.0)
      }
      val viewX = appState.view.x + voX
      val viewY = appState.view.y + voY
      val viewScale = appState.view.scale
      val viewWidth = appState.view.width
      val viewHeight = appState.view.height

      val allLinks = for ((r, ge) <- graph.residues) yield {
        r -> appState.displayConv.links(ge.residue)
      }

      val (drag, dx, dy) = $.state match {
        case InputState.Drag(_, (ox, oy), _) => (true, ox, oy)
        case _ => (false, 0.0, 0.0)
      }

      val entriesOffset = for ((r, ge) <- graph.residues) yield {
        val ge2 = (appState.mode, $.state) match {
          case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(_, _, Some(Link(tr, tp)))) if tr == r =>
            ge &|-> GraphEntry.residue ^|-> Residue.subs ^|-> at(tp) modify { m => Some(m.orZero :+ st) }
          case (Mode.Selection, InputState.Rotate(`r`, rot)) =>
            ge.copy(rotation = rot)
          case _ => ge
        }

        val selected = appState.selection._1.contains(r)
        val geOffset = if (drag && selected) ge2.copy(x = ge2.x + dx, y = ge2.y + dy) else ge2
        r -> geOffset
      }

      val bonds = for {
        (r, ge) <- entriesOffset.toSeq
        toLink @ Link(toRes, i) <- ge.parent
      } yield {
        val from = appState.displayConv.linkPos(allLinks(r), ge, 1)
        val to = appState.displayConv.linkPos(allLinks(toRes), entriesOffset(toRes), i)
        val anomer = ge.residue.rt match {
          case ResidueType.Begin => rootAnomer(r)
          case _ => ge.residue.ano
        }
        val highlight = appState.highlightBond.contains(r)
        SVGBond.withKey("bond" + r.id)(SVGBond.Props(anomer, Some(i), from, to, appState.bondLabels, highlight))
      }

      val tempBonds = (appState.mode, $.state) match {
        case (Mode.Selection, InputState.CreateBond(r, mouse, target)) =>
          for (ge <- r.graphEntry.toSeq) yield {
            val from = appState.displayConv.linkPos(allLinks(r), ge, 1)
            val targetLink = for {
              Link(rLink, pos) <- target
              geLink <- rLink.graphEntry
            } yield appState.displayConv.linkPos(allLinks(rLink), geLink, pos)
            val to = targetLink getOrElse mouse
            SVGBond.withKey("tempBond")(SVGBond.Props(ge.residue.ano, target.map(_.position), from, to, appState.bondLabels))
          }
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y, children, parent)) =>
          val ((rx, ry), rw, rh) = appState.displayConv.bounds(residue)
          val from = for {
            (i, id) <- children.toSeq
            ge <- graph.residues.get(id)
          } yield {
            val fromPos = appState.displayConv.linkPos(allLinks(id), ge, 1)
            val (ox, oy) = appState.displayConv.links(residue)(i - 1)
            val toPos = (x + ox - (rx + rw / 2.0), y + oy - (ry + rh / 2.0))
            SVGBond.withKey("tempBond" + i)(SVGBond.Props(residue.ano, Some(i), fromPos, toPos, appState.bondLabels))
          }

          val to = for {
            Link(id, i) <- parent.toSeq
            ge <- graph.residues.get(id)
          } yield {
            val (ox, oy) = appState.displayConv.links(residue).head
            val fromPos = (x + ox - (rx + rw / 2.0), y + oy - (ry + rh / 2.0))
            val toPos = appState.displayConv.linkPos(allLinks(id), ge, i)
            SVGBond.withKey("tempBond1")(SVGBond.Props(residue.ano, Some(i), fromPos, toPos, appState.bondLabels))
          }

          from ++ to
        case _ =>
          Seq.empty
      }

      val residues = for ((r, ge) <- entriesOffset.toSeq) yield {
        val selected = appState.selection._1.contains(r)
        SVGResidue.C.withKey("residue" + r.id)(SVGResidue.Props(
          r, ge, appState.displayConv, selected, appState.scaleSubstituents,
          rvInputState, rvMode,
          $.backend.modGraphFn($.props.value), $.backend.setSelectionFn($.props.value), $.backend.clientToViewFn
        ))
      }

      val tempSubstituent = (appState.mode, $.state) match {
        case (Mode.PlaceSubstituent(st), InputState.AddSubstituent(x, y, None))=>
          val (shape, (w, h)) = SubstituentShape(st)
          val scale = appState.scaleSubstituents
          val (mx, my) = (-w / 2.0, -h / 2.0)
          Some(shape(^.svg.transform := s"translate($x, $y) scale($scale) translate($mx, $my)"))
        case _ => None
      }

      val selectionBox = $.state match {
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

      val tempResidue = (appState.mode, $.state) match {
        case (Mode.PlaceResidue(residue), InputState.AddResidue(x, y, _, _)) =>
          val ge = GraphEntry(residue, x, y, 0)
          val props = (ge, appState.displayConv, appState.scaleSubstituents)
          Some(SVGResidue.T.withKey("tempResidue")(props))
        case _ => None
      }

      val annotations = for ((id, annot) <- graph.annotations.toSeq) yield {
        val selected = appState.selection._2.contains(id)
        val annot2 = if (drag && selected) annot.copy(x = annot.x + dx, y = annot.y + dy) else annot
        Annotation.withKey(id.id)(($.backend.annotationMouseDown(id), id, annot2, selected))
      }

      val tempAnnotation = (appState.mode, $.state) match {
        case (Mode.PlaceAnnotation, InputState.AddAnnotation(x, y)) =>
          Some(<.svg.text("Annotation")(
            ^.svg.fontSize := appState.annotationFontSize,
            ^.svg.fillOpacity := "50%",
            ^.svg.x := x, ^.svg.y := y,
            ^.svg.pointerEvents := "none"
          ))
        case _ => None
      }

      val gw = appState.gridWidth * viewScale
      
      val gridSnapIndicator: Seq[TagMod] = $.state match {
        case InputState.Drag((downX, downY), (ox, oy), (cx, cy)) if appState.snapToGrid =>
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

      <.svg.svg(
        ^.svg.width := viewWidth,
        ^.svg.height := viewHeight,
        ^.ref := "canvas",
        ^.id := "canvas",
        ^.onMouseMove ~~> $.backend.mouseMove _,
        ^.onClick ~~> $.backend.mouseClick _,
        ^.onMouseOut ~~> $.backend.mouseOut _,
        ^.onMouseUp ~~> $.backend.mouseUp _,
        ^.onMouseDown ~~> $.backend.mouseDown _
      )(
        <.svg.defs(
          <.svg.pattern(
            ^.svg.id := "gridPattern", ^.svg.patternUnits := "userSpaceOnUse",
            ^.svg.x := -viewX * viewScale + viewWidth / 2,
            ^.svg.y := -viewY * viewScale + viewHeight / 2,
            ^.svg.width := gw, ^.svg.height := gw
          ) (
            <.svg.path(^.svg.d := s"M0,${gw}V0H$gw", ^.svg.fill := "none", ^.svg.strokeWidth := 0.5, ^.svg.stroke := "black")
          )
        ),
        <.svg.g(
          ^.svg.transform := s"translate(${viewWidth / 2} ${viewHeight / 2}) scale($viewScale) translate(${-viewX} ${-viewY})",
          ^.ref := "view"
        )(
          <.svg.rect(
            ^.svg.transform := s"translate($viewX, $viewY) scale(${1.0 / viewScale}) translate(${-viewWidth / 2}, ${-viewHeight / 2})",
            ^.svg.width := appState.view.width,
            ^.svg.height := appState.view.height,
            ^.svg.fill := (if (appState.showGrid) "url(#gridPattern)" else "white")
          ),
          gridSnapIndicator,
          bonds,
          tempBonds,
          <.svg.rect(
            ^.svg.transform := s"translate($viewX, $viewY) scale(${1.0 / viewScale}) translate(${-viewWidth / 2}, ${-viewHeight / 2})",
            ^.svg.fill := "transparent",
            ^.svg.width := appState.view.width,
            ^.svg.height := appState.view.height,
            ^.onMouseDown ~~> $.backend.boxSelectDown _
          ),
          <.svg.g(^.ref := "bounds")(
            residues,
            tempSubstituent,
            selectionBox,
            tempResidue,
            annotations,
            tempAnnotation
          )
        )
      )
    }
    .domType[dom.svg.SVG]
    .configure(EventListener[dom.Event].installIO("contextmenu", _ => e => IO(e.preventDefault())))
    .configure(Reusability.shouldComponentUpdate)
    .componentDidUpdateIO { (scope, props, state) =>
      val bounds = for (g <- scope.refs[dom.svg.G]("bounds")) yield {
        val bb = g.getDOMNode().getBBox()
        Bounds(bb.x, bb.y, bb.width, bb.height)
      }
      scope.props.setL(AppState.bounds)(bounds.toOption)
    }
    .build
}
