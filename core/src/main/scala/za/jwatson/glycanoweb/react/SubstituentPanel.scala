package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.{ReusableVar, Reusability}
import japgolly.scalajs.react.extra.Reusability._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.{AppState, Mode}
import za.jwatson.glycanoweb.render.SubstituentShape
import za.jwatson.glycanoweb.structure._

object SubstituentPanel {
  case class Props(rvMode: ReusableVar[Mode], scaleSubstituents: Double)
  implicit val reuseDouble: Reusability[Double] = Reusability.by_==
  implicit val reuseProps = Reusability.caseClass[Props]

  val choicesAno = Anomer.Anomers.map(ano => ano -> ano.desc).toMap
  val choicesAbs = Absolute.Absolutes.map(abs => abs -> abs.desc).toMap

  val C = ReactComponentB[Props]("ResiduePanel")
    .render_P { p =>
      val substPages = div"btn-group"("data-toggle".reactAttr := "buttons")(
        for (st <- SubstituentType.SubstituentTypes) yield {
          val (shape, (w, h)) = SubstituentShape(st)
          val icon = <.svg.svg(
            ^.svg.width := w * p.scaleSubstituents,
            ^.svg.height := h * p.scaleSubstituents,
            ^.svg.viewBox := s"0 0 $w $h"
          )(shape)
          val active = p.rvMode.value match {
            case Mode.PlaceSubstituent(`st`) => true
            case _ => false
          }
          val click = p.rvMode.set(if (active) Mode.Select else Mode.PlaceSubstituent(st))
          <.span(
            <.button(
              ^.cls := s"btn btn-default",
              active ?= c"active",
              ^.title := st.name,
              ^.padding := 2.px,
              ^.onClick --> click
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
