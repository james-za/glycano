package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{ReactNode, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.CASPER
import za.jwatson.glycanoweb.structure.{ResidueId, RGraph}

object CASPERDisplay {
  case class Props(graph: RGraph, selection: Set[ResidueId])
  def apply(props: Props, children: ReactNode*) = component(props, children)
  val component = ReactComponentB[Props]("CASPERDisplay")
    .render((P, C) => {
      val casper = CASPER.getStrings(P.selection)(P.graph).values.mkString("; ")
      <.form(^.cls := "form-inline")(
        <.div(^.cls := "form-group", ^.width := "100%")(
          <.input(^.id := "casper", ^.`type`:="text", ^.cls := "form-control", ^.readOnly := "true", ^.width := "100%")(
            ^.value := casper
          )
        )
      )
    })
    .build
}
