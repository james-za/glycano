package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.convention.Convention.Palette

import monocle.Monocle._

import za.jwatson.glycanoweb.react.GlycanoApp.Mode
import za.jwatson.glycanoweb.react.bootstrap.RadioGroupMap
import za.jwatson.glycanoweb.render.{SubstituentShape, DisplayConv}
import za.jwatson.glycanoweb.structure._
import org.scalajs.dom

import scalaz.effect.IO

object ResiduePanel {
  case class Props(ano: ReusableVar[Anomer], abs: ReusableVar[Absolute],
                   mode: ReusableVar[Mode], dc: DisplayConv,
                   scaleSubstituents: Double, conventions: Map[String, DisplayConv])

  implicit val reuseDouble: Reusability[Double] = Reusability.by_==
  implicit val reuseConventions: Reusability[Map[String, DisplayConv]] = Reusability.by_==
  implicit val reuseState: Reusability[Map[DisplayConv, Palette]] = Reusability.by_==
  implicit val reuseProps: Reusability[Props] = Reusability.caseclass6(Props.unapply)

  class Backend($: BackendScope[Props, Map[DisplayConv, Palette]]) {
    def setAnomer(anomer: Option[Anomer]): IO[Unit] = anomer.fold(IO.ioUnit)($.props.ano.set)
    def setAbsolute(absolute: Option[Absolute]): IO[Unit] = absolute.fold(IO.ioUnit)($.props.abs.set)

    def clickResidue(rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): IO[Unit] = $.props.mode.mod {
      case Mode.PlaceResidue(r) if r.rt == rt && r.subs == subs => Mode.Selection
      case _ => Mode.PlaceResidue(Residue($.props.ano.value, $.props.abs.value, rt, subs))
    }

    val setAnoFn: Option[Anomer] ~=> IO[Unit] = ReusableFn(setAnomer)
    val setAbsFn: Option[Absolute] ~=> IO[Unit] = ReusableFn(setAbsolute)
    val getNameAnoFn: Anomer ~=> String = ReusableFn(_.symbol)
    val getNameAbsFn: Absolute ~=> String = ReusableFn(_.symbol)
  }

  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  val C = ReactComponentB[Props]("ResiduePanel")
    .initialStateP[Map[DisplayConv, Palette]] { props =>
      props.conventions.map {
        case (_, dc) => dc -> dc.conv.palettes.head
      }
    }
    .backend(new Backend(_))
    .render { $ =>
      val residueTabs = <.ul(c"nav nav-tabs", ^.role := "tablist", ^.marginBottom := 5.px)(
        for (pal <- $.props.dc.conv.palettes :+ Palette.Repeat) yield {
          val f = index[Map[DisplayConv, Palette], DisplayConv, Palette]($.props.dc).set(pal)
          <.li(
            <.a(
              ^.href := "#",
              ^.onClick ~~> $.modStateIO(f),
              ^.role := "tab",
              "data-toggle".reactAttr := "tab",
              ^.padding := "4px 7px"
            )(pal.name),
            $.state.get($.props.dc).contains(pal) ?= c"active"
          )
        }
      )

      val rvAno = ReusableVar[Option[Anomer]](Some($.props.ano.value))($.backend.setAnoFn)
      val rvAbs = ReusableVar[Option[Absolute]](Some($.props.abs.value))($.backend.setAbsFn)

      val residueConfig = div"btn-toolbar"(^.role := "toolbar", ^.display.`inline-block`)(
        RadioAnomer(RadioGroupMap.Props[Anomer](rvAno, Anomer.Anomers, $.backend.getNameAnoFn, toggle = false)),
        RadioAbsolute(RadioGroupMap.Props[Absolute](rvAbs, Absolute.Absolutes, $.backend.getNameAbsFn, toggle = false))
      )

      val residuePages = div"btn-group"("data-toggle".reactAttr := "buttons")(
        div"tab-content"(
          div"tab-pane active"(^.role := "tabpanel")(
            for ((rt, subs) <- $.state($.props.dc).residues) yield {
              val res = Residue($.props.ano.value, $.props.abs.value, rt, subs)
              val ((x, y), w, h) = $.props.dc.bounds(res)
              val scale = 0.4
              val (residue, handle) = $.props.dc.shapes(res)

              val residueLinks = $.props.dc.links(res)
              val substituents = for ((i, sts) <- subs.toSeq) yield {
                val (x1, y1) = residueLinks(i - 1)
                <.svg.g(^.svg.transform := s"translate($x1, $y1) scale(${$.props.scaleSubstituents})")(
                  SVGSubstituentStack.withKey(i)(sts)
                )
              }
              val selected = $.props.mode.value match {
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
                      <.svg.g(residue, handle, $.props.dc.name == "UCT" ?= substituents)
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
