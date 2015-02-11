package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ReactComponentB
import monocle.macros.Lenses
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure._
import org.scalajs.dom

object ResiduePanel {
  case class Props(dc: DisplayConv, onChange: State => Unit)

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
            for (rt <- ResidueType.ResidueTypeCategories(S.page)) yield {
              val (_, w, h) = P.dc.boundsMemo(S.ano, S.abs, rt, Map.empty)
              val scale = 0.4
              val (residue, handle) = P.dc.shapes(S.ano, S.abs, rt, Map.empty)
              <.span(
                <.button(^.cls := s"btn btn-default", S.rt.contains(rt) ?= (^.cls := "active"), ^.title := rt.desc, ^.padding := 2.px, ^.onClick --> B.clickResidue(rt))(
                  <.svg.svg(
                    ^.svg.width := (w + 20) * scale,
                    ^.svg.height := (h + 20) * scale
                  )(
                    <.svg.g(^.svg.transform := s"scale($scale) translate(10, 0)")(
                      <.svg.g(residue, handle)
                    )
                  )
                )
              )
            }
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
    .shouldComponentUpdate {
      (T, P, S) =>
        T.props.dc.conv != P.dc.conv || T.state != S
    }
    .domType[dom.html.Div]
    .build
}
