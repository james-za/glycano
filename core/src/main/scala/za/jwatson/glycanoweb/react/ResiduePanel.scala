package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._

import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.react.bootstrap.RadioGroupMap
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure._
import org.scalajs.dom

import scalaz.effect.IO

object ResiduePanel {
  case class Props(ano: ReusableVar[Anomer], abs: ReusableVar[Absolute],
                   mode: ReusableVar[Mode], dc: DisplayConv)

  implicit val reuseProps: Reusability[Props] = Reusability.caseclass4(Props.unapply)

  class Backend($: BackendScope[Props, ResidueCategory]) {
    def setAnomer(anomer: Option[Anomer]): IO[Unit] = anomer.fold(IO.ioUnit)($.props.ano.set)
    def setAbsolute(absolute: Option[Absolute]): IO[Unit] = absolute.fold(IO.ioUnit)($.props.abs.set)

    def clickResidue(rt: ResidueType): IO[Unit] = $.props.mode.mod {
      case Mode.PlaceResidue(r) if r.rt == rt => Mode.Selection
      case _ => Mode.PlaceResidue(Residue($.props.ano.value, $.props.abs.value, rt))
    }

    val setAnoFn: Option[Anomer] ~=> IO[Unit] = ReusableFn(setAnomer)
    val setAbsFn: Option[Absolute] ~=> IO[Unit] = ReusableFn(setAbsolute)
    val getNameAnoFn: Anomer ~=> String = ReusableFn(_.symbol)
    val getNameAbsFn: Absolute ~=> String = ReusableFn(_.symbol)
  }

  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  val C = ReactComponentB[Props]("ResiduePanel")
    .initialState[ResidueCategory](ResidueCategory.Aldose)
    .backend(new Backend(_))
    .render { $ =>
      val residueTabs = <.ul(^.cls := "nav nav-tabs", ^.role := "tablist")(
        for (cat <- ResidueCategory.ResidueCategories) yield <.li(
          <.a(
            ^.href := "#",
            ^.onClick ~~> $.setStateIO(cat),
            ^.role := "tab",
            "data-toggle".reactAttr := "tab",
            ($.state == cat) ?= (^.cls := "active")
          )(cat.name)
        )
      )

      val rvAno = ReusableVar[Option[Anomer]](Some($.props.ano.value))($.backend.setAnoFn)
      val rvAbs = ReusableVar[Option[Absolute]](Some($.props.abs.value))($.backend.setAbsFn)

      val residueConfig = <.div(^.cls := "btn-toolbar", ^.role := "toolbar", ^.display.`inline-block`)(
        RadioAnomer(RadioGroupMap.Props[Anomer](rvAno, Anomer.Anomers, $.backend.getNameAnoFn, toggle = false)),
        RadioAbsolute(RadioGroupMap.Props[Absolute](rvAbs, Absolute.Absolutes, $.backend.getNameAbsFn, toggle = false))
      )

      val residuePages = <.div(^.cls := "btn-group", "data-toggle".reactAttr := "buttons")(
        <.div(^.cls := "tab-content")(
          <.div(^.role := "tabpanel", ^.cls := "tab-pane active")(
            for (rt <- ResidueType.ResidueTypeCategories($.state)) yield {
              val res = Residue($.props.ano.value, $.props.abs.value, rt)
              val ((x, y), w, h) = $.props.dc.bounds(res)
              val scale = 0.4
              val (residue, handle) = $.props.dc.shapes(res)
              val selected = $.props.mode.value match {
                case Mode.PlaceResidue(r) if r.rt == rt => true
                case _ => false
              }
              <.span(
                <.button(^.cls := s"btn btn-default", selected ?= (^.cls := "active"), ^.title := rt.desc, ^.padding := 2.px, ^.onClick ~~> $.backend.clickResidue(rt))(
                  <.svg.svg(
                    ^.svg.width := (w + 20) * scale,
                    ^.svg.height := (h + 20) * scale
                  )(
                    <.svg.g(^.svg.transform := s"scale($scale) translate(${10 - x} ${10 - y})")(
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
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
