package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.convention.Convention.Palette

import monocle.Monocle._

import za.jwatson.glycanoweb.react.GlycanoApp.{AppState, Mode}
import za.jwatson.glycanoweb.react.bootstrap.RadioGroupMap
import za.jwatson.glycanoweb.render.{SubstituentShape, DisplayConv}
import za.jwatson.glycanoweb.structure._
import org.scalajs.dom

import scalaz.effect.IO

object ResiduePanel {
  case class Props(rvAppState: ReusableVar[AppState], conventions: Map[String, DisplayConv])

  implicit val reuseDouble: Reusability[Double] = Reusability.by_==
  implicit val reuseConventions: Reusability[Map[String, DisplayConv]] = Reusability.by_==
  implicit val reuseState: Reusability[Map[DisplayConv, Palette]] = Reusability.by_==
  implicit val reuseProps: Reusability[Props] = Reusability.caseclass2(Props.unapply)

  class Backend($: BackendScope[Props, Map[DisplayConv, Palette]]) {
    def clickResidue(rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): IO[Unit] = $.props.rvAppState.modL(AppState.mode) {
      case Mode.PlaceResidue(r) if r.rt == rt && r.subs == subs => Mode.Selection
      case _ => Mode.PlaceResidue(Residue(
        $.props.rvAppState.value.placeAnomer,
        $.props.rvAppState.value.placeAbsolute,
        rt, subs
      ))
    }

    def setOption[A](lens: monocle.Lens[AppState, A])(a: Option[A]): IO[Unit] = a.fold(IO.ioUnit)($.props.rvAppState.setL(lens))
    val setAnoFn: Option[Anomer] ~=> IO[Unit] = ReusableFn(setOption(AppState.placeAnomer))
    val setAbsFn: Option[Absolute] ~=> IO[Unit] = ReusableFn(setOption(AppState.placeAbsolute))
    val getNameAnoFn: Anomer ~=> String = ReusableFn(_.symbol)
    val getNameAbsFn: Absolute ~=> String = ReusableFn(_.symbol)
  }

  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  val reuseAppState = Reusability.by((s: AppState) => (s.placeAnomer, s.placeAbsolute, s.mode, s.displayConv, s.scaleSubstituents))
  val C = ReactComponentB[Props]("ResiduePanel")
    .initialStateP[Map[DisplayConv, Palette]] { props =>
      props.conventions.map {
        case (_, dc) => dc -> dc.conv.palettes.head
      }
    }
    .backend(new Backend(_))
    .render { $ =>
      val appState = $.props.rvAppState.value
      val residueTabs = <.ul(c"nav nav-tabs", ^.role := "tablist", ^.marginBottom := 5.px)(
        for (pal <- appState.displayConv.conv.palettes :+ Palette.Repeat) yield {
          val f = index[Map[DisplayConv, Palette], DisplayConv, Palette](appState.displayConv).set(pal)
          <.li(
            <.a(
              ^.href := "#",
              ^.onClick ~~> $.modStateIO(f),
              ^.role := "tab",
              "data-toggle".reactAttr := "tab",
              ^.padding := "4px 7px"
            )(pal.name),
            $.state.get(appState.displayConv).contains(pal) ?= c"active"
          )
        }
      )

      val rvAno = $.backend.setAnoFn.asVar(Some(appState.placeAnomer))
      val rvAbs = $.backend.setAbsFn.asVar(Some(appState.placeAbsolute))

      val residueConfig = div"btn-toolbar"(^.role := "toolbar", ^.display.`inline-block`)(
        RadioAnomer(RadioGroupMap.Props[Anomer](rvAno, Anomer.Anomers, $.backend.getNameAnoFn, toggle = false)),
        RadioAbsolute(RadioGroupMap.Props[Absolute](rvAbs, Absolute.Absolutes, $.backend.getNameAbsFn, toggle = false))
      )

      val residuePages = div"btn-group"("data-toggle".reactAttr := "buttons")(
        div"tab-content"(
          div"tab-pane active"(^.role := "tabpanel")(
            for ((rt, subs) <- $.state(appState.displayConv).residues) yield {
              val res = Residue(appState.placeAnomer, appState.placeAbsolute, rt, subs)
              val ((x, y), w, h) = appState.displayConv.bounds(res)
              val scale = 0.4
              val (residue, handle) = appState.displayConv.shapes(res)

              val residueLinks = appState.displayConv.links(res)
              val substituents = for ((i, sts) <- subs.toSeq) yield {
                val (x1, y1) = residueLinks(i - 1)
                <.svg.g(^.svg.transform := s"translate($x1, $y1) scale(${appState.scaleSubstituents})")(
                  SVGSubstituentStack.withKey(i)(sts)
                )
              }
              val selected = appState.mode match {
                case Mode.PlaceResidue(r) if r.rt == rt && r.subs == subs => true
                case _ => false
              }
              <.span(
                <.button(^.cls := s"btn btn-default", selected ?= c"active", ^.title := rt.desc, ^.padding := 2.px, ^.onClick ~~> $.backend.clickResidue(rt, subs))(
                  <.svg.svg(
                    ^.svg.width := (w + 20) * scale,
                    ^.svg.height := (h + 20) * scale
                  )(
                    <.svg.g(^.svg.transform := s"scale($scale) translate(${10 - x} ${10 - y})")(
                      <.svg.g(residue, handle, appState.displayConv.name == "UCT" ?= substituents)
                    )
                  )
                )
              )
            }
          )
        )
      )

      div"panel panel-default"(
        div"panel-heading"("Residues"),
        div"panel-body text-center"(
          div"row"(div"col-xs-12"(residueTabs)),
          div"row"(div"col-xs-12"(residueConfig)),
          div"row"(div"col-xs-12"(residuePages))
        )
      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
