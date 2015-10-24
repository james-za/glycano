package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Reusability, ~=>, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.{Selection, AppStateL, Mode}
import za.jwatson.glycanoweb.react.GlycanoCanvas.{Mouse, InputState}
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph.GraphEntry
import za.jwatson.glycanoweb.structure.{AnnotId, ResidueId, RGraph, Residue}

import scala.scalajs.js.UndefOr
import scalajs.js
import scalaz.effect.IO

object SVGResidue {
//  case class Props(residueMouseDown: ReactMouseEvent => Unit,
//                   handleMouseDown: ReactMouseEvent => Unit,
//                   rotatorMouseDown: ReactMouseEvent => Unit,
//                   ge: GraphEntry, dc: DisplayConv,
//                   selected: Boolean, scaleSubstituents: Double)
  case class Props(r: ResidueId, ge: GraphEntry, dc: DisplayConv, selected: Boolean, scaleSubstituents: Double,
                   inputState: ReusableVar[InputState], mode: ReusableVar[Mode],
                   modGraph: (RGraph => RGraph) ~=> Callback,
                   setSelection: Selection ~=> Callback,
                   clientToViewFn: (Double, Double) ~=> UndefOr[(Double, Double)])

  implicit val reuseDouble: Reusability[Double] = Reusability.by_==
  implicit val reuseProps: Reusability[Props] = Reusability.caseClass[Props]

  class Backend($: BackendScope[Props, Boolean]) {
    def render(props: Props, state: Boolean) = {
      def handleMouseDown(e: ReactMouseEvent) =
        (button(e), props.mode.value) match {
          case (Mouse.Left, Mode.Select) =>
            for {
              _ <- props.inputState.set(InputState.PreCreateBond(props.r)).toCBO
              link <- CallbackOption.liftOption(props.ge.parent)
              _ <- props.modGraph(_ - link)
            } yield ()
          case _ => CallbackOption.empty
        }

      def rotatorMouseDown(e: ReactMouseEvent) =
        (button(e), props.mode.value, props.inputState.value) match {
          case (Mouse.Left, Mode.Select, InputState.Default) =>
            props.inputState.set(InputState.Rotate(props.r, props.ge.rotation))
          case _ => Callback.empty
        }

      def residueMouseDown(e: ReactMouseEvent) =
        (button(e), props.mode.value, props.inputState.value) match {
          case (Mouse.Left, Mode.Select, InputState.Default) =>
            val down = props.clientToViewFn((e.clientX, e.clientY))
            val center = (props.ge.x, props.ge.y)
            for {
              d <- CallbackOption.liftOptionLike(down)
              _ <- props.inputState.set(InputState.Drag(d, (0.0, 0.0), center))
              _ <- CallbackOption.unless(props.selected)
              _ <- props.setSelection((Set(props.r), Set.empty))
            } yield ()
          case _ => CallbackOption.empty
        }

      val Props(r, ge, dc, selected, scaleSubstituents, _, _, _, _, _) = props

      val GraphEntry(_, x, y, rot, _, _) = ge
      val ((x0, y0), w, h) = dc.bounds(ge.residue)
      val (cx, cy) = (x0 + w / 2.0, y0 + h / 2.0)
      val (residue, handle) = dc.shapes(ge.residue)

      val residueLinks = dc.links(ge.residue)
      val substituents = for {
        (i, sts) <- ge.residue.subs.toSeq
      } yield {
          val (x1, y1) = residueLinks(i - 1)
          <.svg.g(^.svg.transform := s"translate($x1, $y1) scale($scaleSubstituents)")(
            SVGSubstituentStack.C.withKey(i)(sts)
          )
        }

      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
        selected ?= <.svg.rect(
          ^.svg.x := -(w / 2.0 + 5), ^.svg.y := -(h / 2.0 + 5),
          ^.svg.width := w + 10, ^.svg.height := h + 10,
          ^.svg.rx := 5, ^.svg.ry := 5,
          ^.svg.fill := "#404080", ^.svg.fillOpacity := "50%",
          ^.svg.stroke := "#404080", ^.svg.strokeWidth := 1
        ),
        selected ?= <.svg.line(^.svg.x1 := 0, ^.svg.y1 := 0, ^.svg.x2 := 0, ^.svg.y2 := -(h / 2.0 + 20)),
        selected ?= <.svg.circle(
          ^.svg.cx := 0, ^.svg.cy := -(h / 2.0 + 20),
          ^.svg.r := 8,
          ^.svg.fill := "#808080",
          ^.svg.stroke := "#404040",
          ^.onMouseDown ==> rotatorMouseDown
        ),
        <.svg.g(^.svg.transform := s"translate(${-cx} ${-cy})")(
          residue(
            ^.onMouseDown ==> residueMouseDown
          ),
          handle(
            ^.onMouseOver --> $.setState(true),
            ^.onMouseOut --> $.setState(false),
            ^.onMouseDown ==> handleMouseDown,
            state ?= Seq(^.svg.strokeWidth := "3", ^.svg.stroke := "blue")
          ),
          props.dc.name == "UCT" ?= substituents
        )
      )
    }
  }

  val C = ReactComponentB[Props]("SVGResidue")
    .initialState(false)
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate[Props, Boolean, Backend, TopNode])
    .build

  val T = ReactComponentB[(GraphEntry, DisplayConv, Double)]("SVGTempResidue")
    .render_P { props =>
      val (ge, dc, scaleSubstituents) = props
      val GraphEntry(res, x, y, rot, _, _) = ge

      val ((x0, y0), w, h) = dc.bounds(res)
      val (cx, cy) = (x0 + w / 2.0, y0 + h / 2.0)
      val (residue, handle) = dc.shapes(res)

      val residueLinks = dc.links(ge.residue)
      val substituents = for ((i, sts) <- ge.residue.subs.toSeq) yield {
        val (x1, y1) = residueLinks(i - 1)
        <.svg.g(^.svg.transform := s"translate($x1, $y1) scale($scaleSubstituents)")(
          SVGSubstituentStack.C.withKey(i)(sts)
        )
      }

      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
        <.svg.g(^.svg.transform := s"translate(${-cx} ${-cy})")(
          residue, handle, dc.name == "UCT" ?= substituents
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
