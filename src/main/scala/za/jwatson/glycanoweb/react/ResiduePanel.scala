package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ReactComponentB
import monocle.macros.Lenses
import za.jwatson.glycanoweb.structure.{ResidueCategory, Anomer, Absolute, ResidueType}
import org.scalajs.dom

object ResiduePanel {
  case class Props(onChange: State => Unit)

  @Lenses case class State(ano: Anomer, abs: Absolute, rt: Option[ResidueType], page: ResidueCategory)

  class Backend(t: BackendScope[Props, State]) {
    def setAnomer(anomer: Option[Anomer]): Unit = for (ano <- anomer) {
      t.modState { old =>
        val state = State.ano.set(ano)(old)
        t.props.onChange(state)
        state
      }
    }
    def setAbsolute(absolute: Option[Absolute]): Unit = for (abs <- absolute) {
      t.modState { old =>
        val state = State.abs.set(abs)(old)
        t.props.onChange(state)
        state
      }
    }
    def setResidueType(rt: Option[ResidueType]): Unit = {
      t.modState { old =>
        val state = State.rt.set(rt)(old)
        t.props.onChange(state)
        state
      }
    }

    def clickResidue(rt: ResidueType): Unit = {
      setResidueType(if (t.state.rt.contains[ResidueType](rt)) None else Some(rt))
    }

    def clickPage(cat: ResidueCategory): Unit = {
      t.modState(State.page set cat)
    }
  }

  val choicesAno = Anomer.Anomers.map(ano => ano -> ano.desc).toMap
  val choicesAbs = Absolute.Absolutes.map(abs => abs -> abs.desc).toMap

  def apply(props: Props, children: ReactTag*) = component(props, children)
  val component = ReactComponentB[Props]("ResiduePanel")
    .initialState(State(Anomer.Alpha, Absolute.D, None, ResidueCategory.Aldose))
    .backend(new Backend(_))
    .render((P, S, B) => {

      val residueTabs = <.ul(^.cls := "nav nav-tabs", ^.role := "tablist")(
        for (cat <- ResidueCategory.ResidueCategories) yield <.li(
          <.a(
            ^.href := "#",
            ^.onClick --> B.clickPage(cat),
            ^.role := "tab",
            "data-toggle".reactAttr := "tab",
            (S.page == cat) ?= (^.cls := "active")
          )(cat.name)
        )
      )

      val residueConfig = <.div(^.cls := "btn-toolbar", ^.role := "toolbar", ^.display.`inline-block`)(
        RadioGroupMap[Anomer].apply(RadioGroupMap.Props[Anomer](B.setAnomer, choicesAno, S.ano, toggle = false)),
        RadioGroupMap[Absolute].apply(RadioGroupMap.Props[Absolute](B.setAbsolute, choicesAbs, S.abs, toggle = false))
      )

      val residuePages = <.div(^.cls := "btn-group", "data-toggle".reactAttr := "buttons")(
        <.div(^.cls := "tab-content")(
          <.div(^.role := "tabpanel", ^.cls := "tab-pane active")(
            for (rt <- ResidueType.ResidueTypeCategories(S.page)) yield <.span(
              <.label(^.cls := s"btn btn-default", ^.title := rt.desc, ^.padding := 2.px)(
                <.input(^.`type` := "checkbox", ^.display.none, S.rt.contains(rt) ?= (^.checked := "true"), ^.onChange --> B.clickResidue(rt)),
                <.svg.svg(
                  ^.svg.width := 50,
                  ^.svg.height := 40
                )(GlycanoCanvas.defaultShape(S.ano, S.abs, rt, scale = 0.4))
              )
            )
          )
        )
      )

      <.div(^.cls := "panel panel-default")(
        <.div(^.cls := "panel-heading")("Residues"),
        <.div(^.cls := "panel-body text-center")(
          <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(residueTabs)),
          <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(residueConfig)),
          <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(residuePages))
        )
      )
    })
    .domType[dom.HTMLDivElement]
    .build
}
