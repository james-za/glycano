package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import japgolly.scalajs.react.extra.{OnUnmount, EventListener, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.{AppStateL, AppState}
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.{ResidueId, ResidueType, RGraph}
import za.jwatson.glycanoweb.structure.RGraph._

import scalaz.effect.IO

object BondStatus {
  case class Props(bond: Bond, rvAppState: ReusableVar[AppState])
  class Backend($: BackendScope[Props, Unit]) extends OnUnmount {
    val mouseEnterIO = for {
      from <- IO($.props.bond.from)
      _ <- $.props.rvAppState.setL(AppState.highlightBond)(Some(from))
    } yield ()
  }
  val reuseAppState = Reusability.by((s: AppState) => (s.graph, s.displayConv, s.highlightBond))
  implicit val reusability: Reusability[Props] = Reusability.caseclass2(Props.unapply)
  val C = ReactComponentB[Props]("BondStatus")
    .stateless
    .backend(new Backend(_))
    .render { $ =>
      val Props(bond, RVarValue(appState)) = $.props
      val hl = appState.highlightBond.contains(bond.from)
      implicit val g: RGraph = appState.graph
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
            ResidueStatus.C((ge1.residue, appState.displayConv)),
            <.svg.svg(
              ^.display.`inline-block`,
              ^.svg.width := 40.px,
              ^.svg.height := 30.px,
              ^.svg.viewBox := "0 0 120 90"
            )(SVGBond(SVGBond.Props(anomer, None, (0, 45), (120, 45), highlight = hl))),
            ResidueStatus.C((ge2.residue, appState.displayConv))
          ),
          div"col-xs-2"(
            <.p(s"${ge1.residue.ano.desc}-${bond.to.position}")
          ),
          div"col-xs-4"(
            <.button(
              c"btn btn-link", "remove bond",
              ^.onClick ~~> $.props.rvAppState.modL(AppStateL.graphL)(_ - bond)
            )
          )
        )
      )
    }
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .configure(EventListener.installIO("mouseenter", _.backend.mouseEnterIO))
    .configure(EventListener.installIO("mouseleave", _.props.rvAppState.setL(AppState.highlightBond)(None)))
    .build

}