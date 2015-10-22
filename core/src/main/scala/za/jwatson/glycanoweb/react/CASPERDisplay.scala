package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{FunctionalComponent, ReactNode, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.CASPER
import za.jwatson.glycanoweb.structure.{ResidueId, RGraph}

object CASPERDisplay {
  case class Props(graph: RGraph, selection: Set[ResidueId])
  def apply(props: Props, children: ReactNode*) = component(props)
  val component = FunctionalComponent[Props] { props =>
    val casper = CASPER.getStrings(props.selection)(props.graph).values.mkString("; ")
    <.form(c"form-inline")(
      div"form-group"(^.width := "100%")(
        <.input(^.id := "casper", ^.`type`:="text", c"form-control", ^.readOnly := "true", ^.width := "100%")(
          ^.value := casper
        )
      )
    )
  }
}
