package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{ReactComponentB, _}
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.macros.Lenses
import org.scalajs.dom
import za.jwatson.glycanoweb.render.{SubstituentShape, DisplayConv}
import za.jwatson.glycanoweb.structure._

object SubstituentPanel {
  case class Props(onChange: State => Unit, scaleSubstituents: Double)

  @Lenses case class State(st: Option[SubstituentType])

  class Backend(t: BackendScope[Props, State]) {
    def setSubstituentType(st: Option[SubstituentType]): Unit = {
      t.modState { old =>
        val state = State.st.set(st)(old)
        t.props.onChange(state)
        state
      }
    }

    def clickSubstituent(st: SubstituentType): Unit = {
      setSubstituentType(if (t.state.st.contains[SubstituentType](st)) None else Some(st))
    }
  }

  val choicesAno = Anomer.Anomers.map(ano => ano -> ano.desc).toMap
  val choicesAbs = Absolute.Absolutes.map(abs => abs -> abs.desc).toMap

  def apply(props: Props, children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[Props]("ResiduePanel")
    .initialState(State(None))
    .backend(new Backend(_))
    .render((P, S, B) => {
      val scale = P.scaleSubstituents
      val substPages = <.div(^.cls := "btn-group", "data-toggle".reactAttr := "buttons")(
        for (st <- SubstituentType.SubstituentTypes) yield {
          val (shape, (w, h)) = SubstituentShape(st)
          val icon = <.svg.svg(
            ^.svg.width := w * scale,
            ^.svg.height := h * scale,
            ^.svg.viewBox := s"0 0 $w $h"
          )(shape)
          <.span(
            <.button(
              ^.cls := s"btn btn-default",
              S.st.contains(st) ?= (^.cls := "active"),
              ^.title := st.name,
              ^.padding := 2.px,
              ^.onClick --> B.clickSubstituent(st)
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
    })
    .shouldComponentUpdate((T, P, S) => T.props.scaleSubstituents != P.scaleSubstituents || T.state != S)
    .domType[dom.html.Div]
    .build
}
