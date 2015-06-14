package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.extra.{OnUnmount, EventListener, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import org.scalajs.dom
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.{ResidueId, ResidueType, RGraph}
import za.jwatson.glycanoweb.structure.RGraph._

object BondStatus {
  val C = ReactComponentB[(Bond, ReusableVar[RGraph], DisplayConv, ReusableVar[Option[ResidueId]])]("BondStatus")
    .stateless
    .backend(_ => new OnUnmount.Backend)
    .render { $ =>
      val (bond, graph, dc, highlightBond) = $.props
      val hl = highlightBond.value.contains(bond)
      implicit val g: RGraph = graph.value
      div"row"(
        for {
          ge1 <- g.residues.get(bond.from)
          ge2 <- g.residues.get(bond.to.r)
          anomer = ge1.residue.rt match {
            case ResidueType.Begin => rootAnomer(bond.from)
            case _ => ge1.residue.ano
          }
        } yield Seq(
          div"col-xs-6"()(
            ResidueStatus.C((ge1.residue, dc)),
            <.svg.svg(
              ^.display.`inline-block`,
              ^.svg.width := 40.px,
              ^.svg.height := 30.px,
              ^.svg.viewBox := "0 0 120 90"
            )(SVGBond(SVGBond.Props(anomer, None, (0, 45), (120, 45), highlight = hl))),
            ResidueStatus.C((ge2.residue, dc))
          ),
          div"col-xs-2"(
            <.p(s"${ge1.residue.ano.desc}-${bond.to.position}")
          ),
          div"col-xs-4"(
            <.button(c"btn btn-link", ^.onClick ~~> graph.mod(_ - bond))("remove bond")
          )
        )
      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .configure(EventListener.installIO("mouseenter", $ => $.props._4.set(Some($.props._1.from))))
    .configure(EventListener.installIO("mouseleave", _.props._4.set(None)))
    .build
}