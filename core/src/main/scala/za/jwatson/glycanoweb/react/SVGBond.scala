package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.Reusability
import za.jwatson.glycanoweb.structure.Anomer

import scalajs.js

import japgolly.scalajs.react.{ReactNode, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._

object SVGBond {
  case class Props(ano: Anomer, target: Option[Int], from: (Double, Double), to: (Double, Double),
                   label: Boolean = false, highlight: Boolean = false)

  implicit val reuseProps: Reusability[Props] = Reusability.by_==

  val C = ReactComponentB[Props]("SVGBond")
    .render_P { props =>
      val (x1, y1) = props.from
      val (x2, y2) = props.to
      val angle = math.toDegrees(math.atan2(y2 - y1, x2 - x1))
      val midX = (x1 + x2) / 2
      val midY = (y1 + y2) / 2
      val stroke = if (props.highlight) "blue" else "black"
      <.svg.g(
        <.svg.line(
          ^.svg.x1 := x1, ^.svg.y1 := y1,
          ^.svg.x2 := x2, ^.svg.y2 := y2,
          ^.svg.stroke := stroke, ^.svg.strokeWidth := 7,
          (props.ano == Anomer.Beta) ?= (^.svg.strokeDasharray := "15 10")
        ),
        props.label ?= <.svg.text(
          ^.svg.pointerEvents := "none",
          ^.svg.transform := s"translate($midX, $midY) rotate($angle) translate(0, -6)",
          ^.svg.fontSize := 20,
          ^.svg.textAnchor := "middle"
        )(
          props.ano.desc + props.target.fold("")(_.toString)
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
