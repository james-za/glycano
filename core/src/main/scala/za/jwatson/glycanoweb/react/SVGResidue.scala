package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph.GraphEntry
import za.jwatson.glycanoweb.structure.Residue

import scalajs.js

object SVGResidue {
  case class Props(residueMouseDown: ReactMouseEvent => Unit,
                   handleMouseDown: ReactMouseEvent => Unit,
                   rotatorMouseDown: ReactMouseEvent => Unit,
                   ge: GraphEntry, dc: DisplayConv,
                   selected: Boolean, scaleSubstituents: Double)

  class Backend(t: BackendScope[Props, Boolean]) {
    def handleMouseOver(): Unit = t.setState(true)
    def handleMouseOut(): Unit = t.setState(false)
  }

  def apply(props: Props, children: ReactNode*) = component(props, children)
  def withKey(key: js.Any) = component.withKey(key)
  val component = ReactComponentB[Props]("SVGResidue")
    .initialState(false)
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      val GraphEntry(_, x, y, rot, _, _) = P.ge
      val ((x0, y0), w, h) = P.dc.boundsMemo(P.ge.residue)
      val (cx, cy) = (x0 + w / 2.0, y0 + h / 2.0)
      val (residue, handle) = P.dc.shapes(P.ge.residue)

      val outline = P.dc.outline(P.ge.residue)
      val substituents = for {
        (i, sts) <- P.ge.residue.subs.toSeq
      } yield {
        val (x1, y1) = outline(i - 1)
        <.svg.g(^.svg.transform := s"translate($x1, $y1) scale(${P.scaleSubstituents})")(
          SVGSubstituentStack.withKey(i)(SVGSubstituentStack.Props(sts))
        )
      }

      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
        P.selected ?= <.svg.rect(
          ^.svg.x := -(w / 2.0 + 5), ^.svg.y := -(h / 2.0 + 5),
          ^.svg.width := w + 10, ^.svg.height := h + 10,
          ^.svg.rx := 5, ^.svg.ry := 5,
          ^.svg.fill := "#404080", ^.svg.fillOpacity := "50%",
          ^.svg.stroke := "#404080", ^.svg.strokeWidth := 1
        ),
        P.selected ?= <.svg.line(^.svg.x1 := 0, ^.svg.y1 := 0, ^.svg.x2 := 0, ^.svg.y2 := -(h / 2.0 + 20)),
        P.selected ?= <.svg.circle(
          ^.svg.cx := 0, ^.svg.cy := -(h / 2.0 + 20),
          ^.svg.r := 8,
          ^.svg.fill := "#808080",
          ^.svg.stroke := "#404040",
          ^.onMouseDown ==> P.rotatorMouseDown),
        <.svg.g(^.svg.transform := s"translate(${-cx} ${-cy})")(
          residue(
            ^.onMouseDown ==> P.residueMouseDown
          ),
          handle(
            ^.onMouseOver --> B.handleMouseOver(),
            ^.onMouseOut --> B.handleMouseOut(),
            ^.onMouseDown ==> P.handleMouseDown,
            S ?= Seq(^.svg.strokeWidth := "3", ^.svg.stroke := "blue")
          ),
          substituents
        )
      )
    })
    .shouldComponentUpdate((T, P, S) => {
      T.props.dc.conv != P.dc.conv ||
      T.props.ge != P.ge ||
      T.props.selected != P.selected ||
      T.props.scaleSubstituents != P.scaleSubstituents ||
      T.state != S
    })
    .build
}
