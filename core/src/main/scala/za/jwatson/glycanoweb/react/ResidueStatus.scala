package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.Residue

object ResidueStatus {
  val C = ReactComponentB[(Residue, DisplayConv)]("ResidueStatus")
    .render_P { props =>
      val (r, dc) = props
      val (residue, handle) = dc.shapes(r)
      val ((x, y), w, h) = dc.bounds(r)
      <.svg.svg(
        ^.display.`inline-block`,
        ^.svg.width := 40.px,
        ^.svg.height := 30.px,
        ^.svg.viewBox := s"${x - 10} ${y - 10} ${w + 20} ${h + 20}"
      )(<.svg.g(residue, handle))
    }
    .domType[dom.svg.SVG]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
