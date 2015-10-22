package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.{EventListener, OnUnmount, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.{AppState, AppStateL}
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure.{RGraph, ResidueType}

object BondStatus {
  case class Props(bond: Bond, rvAppState: ReusableVar[AppState])

  class Backend($: BackendScope[Props, Unit]) extends OnUnmount {
    val mouseEnter = for {
      p <- $.props
      _ <- p.rvAppState.setL(AppState.highlightBond)(Some(p.bond.from))
    } yield ()
    val mouseLeave = for {
      p <- $.props
      _ <- p.rvAppState.setL(AppState.highlightBond)(None)
    } yield ()

    def render(p: Props) = {
      val appState = p.rvAppState.value
      val hl = appState.highlightBond.contains(p.bond.from)
      implicit val g: RGraph = appState.graph
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
            <.p(s"${ge1.residue.ano.desc}-${p.bond.to.position}")
          ),
          div"col-xs-4"(
            <.button(
              c"btn btn-link", "remove bond",
              ^.onClick ==> (preventDefault(_: ReactMouseEvent) >> p.rvAppState.modL(AppStateL.graphL)(_ - p.bond))
            )
          )
        )
      )
    }
  }

  val reuseAppState = Reusability.by((s: AppState) => (s.graph, s.displayConv, s.highlightBond))
  implicit val reusability = Reusability.caseClass[Props]

  val C = ReactComponentB[Props]("BondStatus")
    .stateless
    .renderBackend[Backend]
    .domType[dom.html.Div]
    .configure(Reusability.shouldComponentUpdate)
    .configure(EventListener.install("mouseenter", _.backend.mouseEnter))
    .configure(EventListener.install("mouseleave", _.backend.mouseLeave))
    .build
}