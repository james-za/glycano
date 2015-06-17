package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.{ReusableVar, Reusability}
import japgolly.scalajs.react.extra.Reusability._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.render.SubstituentShape
import za.jwatson.glycanoweb.structure._

object SubstituentPanel {
  type Props = (ReusableVar[Mode], Double)

  implicit val reuseDouble: Reusability[Double] = Reusability.by_==

  val choicesAno = Anomer.Anomers.map(ano => ano -> ano.desc).toMap
  val choicesAbs = Absolute.Absolutes.map(abs => abs -> abs.desc).toMap

  def apply(props: Props, children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[Props]("ResiduePanel")
    .render { props =>
      val (mode, scale) = props

      val substPages = div"btn-group"("data-toggle".reactAttr := "buttons")(
        for (st <- SubstituentType.SubstituentTypes) yield {
          val (shape, (w, h)) = SubstituentShape(st)
          val icon = <.svg.svg(
            ^.svg.width := w * scale,
            ^.svg.height := h * scale,
            ^.svg.viewBox := s"0 0 $w $h"
          )(shape)
          val active = mode.value match {
            case Mode.PlaceSubstituent(`st`) => true
            case _ => false
          }
          val click = mode.set(if (active) Mode.Selection else Mode.PlaceSubstituent(st))
          <.span(
            <.button(
              ^.cls := s"btn btn-default",
              active ?= c"active",
              ^.title := st.name,
              ^.padding := 2.px,
              ^.onClick ~~> click
            )(icon)
          )
        }
      )

      div"panel panel-default"(
        div"panel-heading"("Substituents"),
        div"panel-body text-center"(
          div"row"(div"col-xs-12"(substPages))
        )
      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
