package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.{EventListener, OnUnmount, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.{AppState, AppStateL}
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure.{ResidueId, RGraph, ResidueType}

object BondStatus {
  case class Props(rvGraph: ReusableVar[RGraph], bond: Bond, rvHighlightBond: ReusableVar[Option[ResidueId]], displayConv: DisplayConv)
  implicit val reuseProps = Reusability.caseClass[Props]

  class Backend($: BackendScope[Props, Unit]) extends OnUnmount {
    val mouseEnter = $.props.flatMap(p => p.rvHighlightBond.set(Some(p.bond.from)))
    val mouseLeave = $.props.flatMap(p => p.rvHighlightBond.set(None))

    def render(p: Props) = {
      val hl = p.rvHighlightBond.value.contains(p.bond.from)
      implicit val g: RGraph = p.rvGraph.value
      div"row"(
        for {
          ge1 <- g.residues.get(p.bond.from)
          ge2 <- g.residues.get(p.bond.to.r)
          anomer = ge1.residue.rt match {
            case ResidueType.Begin => rootAnomer(p.bond.from)
            case _ => ge1.residue.ano
          }
        } yield Seq(
          div"col-xs-6"(
            ResidueStatus.C((ge1.residue, p.displayConv)),
            <.svg.svg(
              ^.display.`inline-block`,
              ^.svg.width := 40.px,
              ^.svg.height := 30.px,
              ^.svg.viewBox := "0 0 120 90"
            )(SVGBond.C(SVGBond.Props(anomer, None, (0, 45), (120, 45), highlight = hl))),
            ResidueStatus.C((ge2.residue, p.displayConv))
          ),
          div"col-xs-2"(
            <.p(s"${ge1.residue.ano.desc}-${p.bond.to.position}")
          ),
          div"col-xs-4"(
            <.button(
              c"btn btn-link", "remove bond",
              ^.onClick ==> (preventDefault(_: ReactMouseEvent) >> p.rvGraph.mod(_ - p.bond))
            )
          )
        )
      )
    }
  }

  val C = ReactComponentB[Props]("BondStatus")
    .stateless
    .renderBackend[Backend]
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .configure(EventListener.install("mouseenter", _.backend.mouseEnter))
    .configure(EventListener.install("mouseleave", _.backend.mouseLeave))
    .build
}