package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ReactComponentB
import monocle.macros.Lenses
import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.react.bootstrap.RadioGroupMap
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure._
import org.scalajs.dom

object ResiduePanel {
  case class Props(dc: DisplayConv, ano: Anomer, abs: Absolute, rt: Option[ResidueType],
                   modState: (GlycanoApp.AppState => GlycanoApp.AppState) => Unit)

  @Lenses case class State(page: ResidueCategory)

  class Backend(t: BackendScope[Props, State]) {
    def setAnomer(anomer: Option[Anomer]): Unit = for (ano <- anomer) {
      t.props.modState(GlycanoApp.AppState.placeAnomer set ano)
    }

    def setAbsolute(absolute: Option[Absolute]): Unit = for (abs <- absolute) {
      t.props.modState(GlycanoApp.AppState.placeAbsolute set abs)
    }

    def clickResidue(rt: ResidueType): Unit = t.props.modState {
      if (!t.props.rt.contains[ResidueType](rt)) {
        GlycanoApp.AppState.mode set Mode.PlaceResidue(Residue(t.props.ano, t.props.abs, rt))
      } else {
        GlycanoApp.AppState.mode set Mode.Selection
      }
    }

    def clickPage(cat: ResidueCategory): Unit = {
      t.modState(State.page set cat)
    }
  }

  val choicesAno = Anomer.Anomers.map(ano => ano -> ano.desc).toMap
  val choicesAbs = Absolute.Absolutes.map(abs => abs -> abs.desc).toMap

  def apply(props: Props, children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[Props]("ResiduePanel")
    .initialState(State(ResidueCategory.Aldose))
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
        RadioGroupMap[Anomer].apply(RadioGroupMap.Props[Anomer](B.setAnomer, choicesAno, P.ano, toggle = false)),
        RadioGroupMap[Absolute].apply(RadioGroupMap.Props[Absolute](B.setAbsolute, choicesAbs, P.abs, toggle = false))
      )

      val residuePages = <.div(^.cls := "btn-group", "data-toggle".reactAttr := "buttons")(
        <.div(^.cls := "tab-content")(
          <.div(^.role := "tabpanel", ^.cls := "tab-pane active")(
            for (rt <- ResidueType.ResidueTypeCategories(S.page)) yield {
              val res: Residue = Residue(P.ano, P.abs, rt)
              val (_, w, h) = P.dc.boundsMemo(res)
              val scale = 0.4
              val (residue, handle) = P.dc.shapes(res)
              <.span(
                <.button(^.cls := s"btn btn-default", P.rt.contains(rt) ?= (^.cls := "active"), ^.title := rt.desc, ^.padding := 2.px, ^.onClick --> B.clickResidue(rt))(
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
        T.props.dc.conv != P.dc.conv ||
        T.props.ano != P.ano ||
        T.props.abs != P.abs ||
        T.props.rt != P.rt ||
        T.state != S
    }
    .domType[dom.html.Div]
    .build
}
