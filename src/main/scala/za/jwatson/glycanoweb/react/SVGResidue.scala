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
      val (_, w, h) = P.dc.boundsMemo(P.ge.residue)
      val (residue, handle) = P.dc.shapes(P.ge.residue)

      val outline = P.dc.outline(P.ge.residue)
      val substituents = for {
        (i, sts) <- P.ge.residue.subs.toSeq
      } yield {
        val (x0, y0) = outline(i - 1)
        <.svg.g(^.svg.transform := s"translate($x0, $y0) scale(${P.scaleSubstituents})")(
          SVGSubstituentStack.withKey(i)(SVGSubstituentStack.Props(sts))
        )
      }

      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
        P.selected ?= <.svg.rect(
          ^.svg.x := -5, ^.svg.y := -5,
          ^.svg.width := w + 10, ^.svg.height := h + 10,
          ^.svg.rx := 5, ^.svg.ry := 5,
          ^.svg.fill := "#404080", ^.svg.fillOpacity := "50%",
          ^.svg.stroke := "#404080", ^.svg.strokeWidth := 1
        ),
        <.svg.g(
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
