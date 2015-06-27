package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import japgolly.scalajs.react.extra.{OnUnmount, EventListener, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import org.scalajs.dom
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.{ResidueId, ResidueType, RGraph}
import za.jwatson.glycanoweb.structure.RGraph._

import scalaz.effect.IO

object BondStatus {
  type Props = (Bond, ReusableVar[RGraph], DisplayConv, ReusableVar[Option[ResidueId]])
  class Backend($: BackendScope[Props, Unit]) extends OnUnmount {
    val mouseEnterIO = for {
      from <- IO($.props._1.from)
      _ <- $.props._4.set(Some(from))
    } yield ()
  }
  val C = ReactComponentB[Props]("BondStatus")
    .stateless
    .backend(new Backend(_))
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
          div"col-xs-6"(
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
            <.button(
              c"btn btn-link", "remove bond",
              ^.onClick ~~> (for {
                _ <- graph.mod(_ - bond)
                _ <- highlightBond.set(None)
              } yield ())
            )
          )
        )
      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .configure(EventListener.installIO("mouseenter", _.backend.mouseEnterIO))
    .configure(EventListener.installIO("mouseleave", _.props._4.set(None)))
    .build

}