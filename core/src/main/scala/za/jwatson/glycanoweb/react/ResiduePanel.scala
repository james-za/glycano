package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
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
  case class Props(rvMode: ReusableVar[Mode], rvPlaceAnomer: ReusableVar[Anomer], rvPlaceAbsolute: ReusableVar[Absolute],
                   displayConv: DisplayConv, scaleSubstituents: Double, conventions: Map[String, DisplayConv])

  implicit val reuseDouble: Reusability[Double] = Reusability.by_==
  implicit val reuseConventions: Reusability[Map[String, DisplayConv]] = Reusability.by_==
  implicit val reuseState: Reusability[Map[DisplayConv, Palette]] = Reusability.by_==
  implicit val reuseProps: Reusability[Props] = Reusability.caseClass[Props]

  class Backend($: BackendScope[Props, Map[DisplayConv, Palette]]) {
    def clickResidue(rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]) = for {
      p <- $.props
      _ <- p.rvMode.mod {
        case Mode.PlaceResidue(r) if r.rt == rt && r.subs == subs => Mode.Select
        case _ => Mode.PlaceResidue(Residue(
          p.rvPlaceAnomer.value,
          p.rvPlaceAbsolute.value,
          rt, subs
        ))
      }
    } yield ()

    val setAnoFn: Option[Anomer] ~=> Callback = ReusableFn(a => for {
      v <- CallbackOption.liftOption(a)
      p <- $.props
      _ <- p.rvPlaceAnomer.set(v)
    } yield ())
    val setAbsFn: Option[Absolute] ~=> Callback = ReusableFn(a => for {
      v <- CallbackOption.liftOption(a)
      p <- $.props
      _ <- p.rvPlaceAbsolute.set(v)
    } yield ())
    val getNameAnoFn: Anomer ~=> String = ReusableFn(_.symbol)
    val getNameAbsFn: Absolute ~=> String = ReusableFn(_.symbol)

    def render(p: Props, state: Map[DisplayConv, Palette]) = {
      val residueTabs = <.ul(c"nav nav-tabs", ^.role := "tablist", ^.marginBottom := 5.px)(
        for (pal <- p.displayConv.conv.palettes :+ Palette.Repeat) yield {
          val f = index[Map[DisplayConv, Palette], DisplayConv, Palette](p.displayConv).set(pal)
          <.li(
            <.a(
              ^.href := "#",
              ^.onClick --> $.modState(f),
              ^.role := "tab",
              "data-toggle".reactAttr := "tab",
              ^.padding := "4px 7px"
            )(pal.name),
            state.get(p.displayConv).contains(pal) ?= c"active"
          )
        }
      )

      val rvAno = setAnoFn.asVar(Some(p.rvPlaceAnomer.value))
      val rvAbs = setAbsFn.asVar(Some(p.rvPlaceAbsolute.value))

      val residueConfig = div"btn-toolbar"(^.role := "toolbar", ^.display.`inline-block`)(
        RadioAnomer(RadioGroupMap.Props[Anomer](rvAno, Anomer.Anomers, getNameAnoFn, toggle = false)),
        RadioAbsolute(RadioGroupMap.Props[Absolute](rvAbs, Absolute.Absolutes, getNameAbsFn, toggle = false))
      )

      val residuePages = div"btn-group"("data-toggle".reactAttr := "buttons")(
        div"tab-content"(
          div"tab-pane active"(^.role := "tabpanel")(
            for ((rt, subs) <- state(p.displayConv).residues) yield {
              val res = Residue(p.rvPlaceAnomer.value, p.rvPlaceAbsolute.value, rt, subs)
              val ((x, y), w, h) = p.displayConv.bounds(res)
              val scale = 0.4
              val (residue, handle) = p.displayConv.shapes(res)

              val residueLinks = p.displayConv.links(res)
              val substituents = for ((i, sts) <- subs.toSeq) yield {
                val (x1, y1) = residueLinks(i - 1)
                <.svg.g(^.svg.transform := s"translate($x1, $y1) scale(${p.scaleSubstituents})")(
                  SVGSubstituentStack.C.withKey(i)(sts)
                )
              }
              val selected = p.rvMode.value match {
                case Mode.PlaceResidue(r) if r.rt == rt && r.subs == subs => true
                case _ => false
              }
              <.span(
                <.button(^.cls := s"btn btn-default", selected ?= c"active", ^.title := res.desc, ^.padding := 2.px, ^.onClick --> clickResidue(rt, subs))(
                  <.svg.svg(
                    ^.svg.width := (w + 20) * scale,
                    ^.svg.height := (h + 20) * scale
                  )(
                    <.svg.g(^.svg.transform := s"scale($scale) translate(${10 - x} ${10 - y})")(
                      <.svg.g(residue, handle, p.displayConv.name == "UCT" ?= substituents)
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
  }

  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  val reuseAppState = Reusability.by((s: AppState) => (s.placeAnomer, s.placeAbsolute, s.mode, s.displayConv, s.scaleSubstituents))
  val C = ReactComponentB[Props]("ResiduePanel")
    .initialState_P[Map[DisplayConv, Palette]] { props =>
      props.conventions.map {
        case (_, dc) => dc -> dc.conv.palettes.head
      }
    }
    .renderBackend[Backend]
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
