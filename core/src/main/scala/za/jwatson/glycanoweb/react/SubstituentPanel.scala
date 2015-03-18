package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.{ReactComponentB, _}
import monocle.macros.Lenses
import org.scalajs.dom
import za.jwatson.glycanoweb.macros.AltEq
import za.jwatson.glycanoweb.macros.AltEq.{altEq, exclude}
import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.render.SubstituentShape
import za.jwatson.glycanoweb.structure._

import scalaz.std.AllInstances._, scalaz.syntax.equal._

object SubstituentPanel {
  case class Props(mode: ExternalVar[Mode], scaleSubstituents: Double)

  val choicesAno = Anomer.Anomers.map(ano => ano -> ano.desc).toMap
  val choicesAbs = Absolute.Absolutes.map(abs => abs -> abs.desc).toMap

  def apply(props: Props, children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[Props]("ResiduePanel")
    .render { props =>
      val scale = props.scaleSubstituents

      val substPages = <.div(^.cls := "btn-group", "data-toggle".reactAttr := "buttons")(
        for (st <- SubstituentType.SubstituentTypes) yield {
          val (shape, (w, h)) = SubstituentShape(st)
          val icon = <.svg.svg(
            ^.svg.width := w * scale,
            ^.svg.height := h * scale,
            ^.svg.viewBox := s"0 0 $w $h"
          )(shape)
          val active = props.mode.value match {
            case Mode.PlaceSubstituent(`st`) => true
            case _ => false
          }
          def click(active: Boolean) = props.mode.set(if (active) Mode.Selection else Mode.PlaceSubstituent(st))
          <.span(
            <.button(
              ^.cls := s"btn btn-default",
              active ?= (^.cls := "active"),
              ^.title := st.name,
              ^.padding := 2.px,
              ^.onClick ~~> click(active)
            )(icon)
          )
        }
      )

      <.div(^.cls := "panel panel-default")(
        <.div(^.cls := "panel-heading")("Substituents"),
        <.div(^.cls := "panel-body text-center")(
          <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(substPages))
        )
      )
    }
    .shouldComponentUpdate((T, P, S) => T.props.mode.value != P.mode.value || T.props.scaleSubstituents != P.scaleSubstituents)
    .domType[dom.html.Div]
    .build
}
