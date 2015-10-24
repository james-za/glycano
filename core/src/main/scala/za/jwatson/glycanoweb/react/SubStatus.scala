package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{Callback, ReactComponentB}
import japgolly.scalajs.react.extra.{~=>, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.render.SubstituentShape
import za.jwatson.glycanoweb.structure.{Link, RGraph, SubstituentType}

object SubStatus {
  //todo: only pass function to remove link instead of rvGraph
  val C = ReactComponentB[(Link, Int, SubstituentType, (RGraph => RGraph) ~=> Callback)]("SubStatus")
    .render_P { props =>
      val (link, j, st, modGraph) = props
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
        div"col-xs-5"(s"${link.position}-${st.symbol}"),
        div"col-xs-4"(
          <.button(c"btn btn-link", ^.onClick --> modGraph(_ - (link, j)))("remove")
        )
      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
