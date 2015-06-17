package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.extra.ReusableVar
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import org.scalajs.dom
import za.jwatson.glycanoweb.render.SubstituentShape
import za.jwatson.glycanoweb.structure.{RGraph, ResidueId, SubstituentType, Link}

object SubStatus {
  val C = ReactComponentB
    .apply[(ResidueId, Int, Int, SubstituentType, ReusableVar[RGraph])]("SubStatus")
    .render { props =>
      val (id, i, j, st, graph) = props
      val (sub, (w, h)) = SubstituentShape(st)
      div"row"(
        div"col-xs-3"(
          <.svg.svg(
            ^.display.block,
            ^.svg.height := 30.px,
            ^.svg.viewBox := s"0 0 $w $h",
            ^.marginLeft := "auto",
            ^.marginRight := "auto"
          )(sub)
        ),
        div"col-xs-5"(s"$i-${st.symbol}"),
        div"col-xs-4"(
          <.button(c"btn btn-link", ^.onClick ~~> graph.mod(_ - (Link(id, i), j)))("remove")
        )
      )
    }
    .domType[dom.html.Div]
    .build
}
