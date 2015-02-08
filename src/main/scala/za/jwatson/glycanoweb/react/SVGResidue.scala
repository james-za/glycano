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
                   r: Residue, ge: GraphEntry, dc: DisplayConv)

  class Backend(t: BackendScope[Props, Boolean]) {
    def hoverHandle(): Unit = t.setState(true)
    def leaveHandle(): Unit = t.setState(false)
  }

  def apply(props: Props, children: ReactNode*) = component(props, children)
  def withKey(key: js.Any) = component.withKey(key)
  val component = ReactComponentB[Props]("SVGResidue")
    .initialState(false)
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      val GraphEntry(x, y, rot, _, _, subs) = P.ge
      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)", ^.onMouseDown ==> P.residueMouseDown)(
        P.dc.group(P.r, subs, handleHover = S, B.hoverHandle, B.leaveHandle, P.handleMouseDown)
      )
    })
    .build
}
