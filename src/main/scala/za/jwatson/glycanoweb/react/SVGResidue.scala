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
                   r: Residue, ge: GraphEntry, dc: DisplayConv,
                   selected: Boolean)

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
      val GraphEntry(x, y, rot, _, _, subs) = P.ge
      val (_, w, h) = P.dc.boundsMemo(P.r.anomer, P.r.absolute, P.r.rt, subs)
      val (residue, handle) = P.dc.shapes(P.r.anomer, P.r.absolute, P.r.rt, subs)

      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
        P.selected ?= <.svg.rect(
          ^.svg.x := -5, ^.svg.y := -5,
          ^.svg.width := w + 10, ^.svg.height := h + 10,
          ^.svg.rx := 5, ^.svg.ry := 5,
          ^.svg.fill := "#404080", "fillOpacity".reactAttr := "50%",
          ^.svg.stroke := "#404080", "strokeWidth".reactAttr := 1
        ),
        <.svg.g(
          residue(
            ^.onMouseDown ==> P.residueMouseDown
          ),
          handle(
            ^.onMouseOver --> B.handleMouseOver(),
            ^.onMouseOut --> B.handleMouseOut(),
            S ?= Seq("strokeWidth".reactAttr := "3", ^.svg.stroke := "blue")
          )
        )
      )
    })
    .shouldComponentUpdate((T, P, S) => {
      T.props.dc.conv  != P.dc.conv  ||
      T.props.r        != P.r        ||
      T.props.ge       != P.ge       ||
      T.props.selected != P.selected ||
      T.state          != S
    })
    .build
}
