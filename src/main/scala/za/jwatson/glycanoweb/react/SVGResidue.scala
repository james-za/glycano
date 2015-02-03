package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph.GraphEntry
import za.jwatson.glycanoweb.structure.Residue

object SVGResidue {
  case class Props(r: Residue, ge: GraphEntry, dc: DisplayConv)

  class Backend(t: BackendScope[Props, Boolean]) {
    def hoverHandle(): Unit = t.setState(true)
    def leaveHandle(): Unit = t.setState(false)
  }

  def apply(props: Props, children: ReactNode*) = component(props, children)
  val component = ReactComponentB[Props]("SVGResidue")
    .initialState(false)
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      val GraphEntry(x, y, rot, _, _, subs) = P.ge
      <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
        P.dc.group(P.r, subs, hh = S, () => B.hoverHandle(), () => B.leaveHandle())
      )
    })
    .build
}
