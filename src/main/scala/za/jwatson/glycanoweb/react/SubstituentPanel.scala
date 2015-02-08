package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{ReactComponentB, _}
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.macros.Lenses
import org.scalajs.dom
import za.jwatson.glycanoweb.render.{SubstituentShape, DisplayConv}
import za.jwatson.glycanoweb.structure._

object SubstituentPanel {
  case class Props(/*dc: DisplayConv, */onChange: State => Unit)

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

  def apply(props: Props, children: ReactTag*) = component(props, children)
  val component = ReactComponentB[Props]("ResiduePanel")
    .initialState(State(None))
    .backend(new Backend(_))
    .render((P, S, B) => {

      val substPages = <.div(^.cls := "btn-group", "data-toggle".reactAttr := "buttons")(
        for (st <- SubstituentType.SubstituentTypes) yield <.span(
          <.button(
            ^.cls := s"btn btn-default",
            S.st.contains(st) ?= (^.cls := "active"),
            ^.title := st.name,
            ^.padding := 2.px,
            ^.onClick --> B.clickSubstituent(st)
          )(
            SubstituentShape(st, scale = 1.0)
          )
        )
      )

      <.div(^.cls := "panel panel-default")(
        <.div(^.cls := "panel-heading")("Substituents"),
        <.div(^.cls := "panel-body text-center")(
          <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(substPages))
        )
      )
    })
    .domType[dom.HTMLDivElement]
    .build
}
