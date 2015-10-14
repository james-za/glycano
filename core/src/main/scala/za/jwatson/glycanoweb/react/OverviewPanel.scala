package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{ReusableFn, ~=>, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import monocle.Monocle._
import za.jwatson.glycanoweb.react.GlycanoApp.{AppStateL, AppState}
import za.jwatson.glycanoweb.react.bootstrap.RadioGroupMap
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph.{GraphEntry, Bond}
import za.jwatson.glycanoweb.structure._

import scala.util.{Success, Failure, Try}
import scalaz.effect.IO

object OverviewPanel {
  def selected(in: Set[ResidueId]) = AppStateL.graphL ^|-> RGraph.residues ^|->> filterIndex(in.contains) ^|-> GraphEntry.residue
  class Backend($: BackendScope[ReusableVar[AppState], Unit]) {
    implicit val reuseSelection: Reusability[Set[ResidueId]] = Reusability.by_==

    val setSelAnoFn = ReusableFn { (sel: Set[ResidueId], ano: Option[Anomer]) =>
      ano.fold(IO.ioUnit)(ano => $.props.mod(selected(sel) ^|-> Residue.ano set ano))
    }
    val setSelAbsFn = ReusableFn { (sel: Set[ResidueId], abs: Option[Absolute]) =>
      abs.fold(IO.ioUnit)(abs => $.props.mod(selected(sel) ^|-> Residue.abs set abs))
    }

    val getNameAnoFn = ReusableFn((_: Anomer).desc)
    val getNameAbsFn = ReusableFn((_: Absolute).desc)
  }

  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  val reuseAppState: Reusability[AppState] = Reusability.by((s: AppState) => (s.graph, s.selection, s.displayConv, s.highlightBond, s.view))(Reusability.by_==)
  val C = ReactComponentB[ReusableVar[AppState]]("OverviewPanel")
    .stateless
    .backend(new Backend(_))
    .render { $ =>
      val appState = $.props.value
      implicit val graph: RGraph = appState.graph
      val (rs, as) = appState.selection
      val rsel = graph.residues.filterKeys(rs.contains)
      val asel = graph.annotations.filterKeys(as.contains)

      div"row"(div"col-xs-12"(
        div"panel panel-default"(
          div"panel-body"(
            for (hd <- rsel.values.headOption) yield {
              val repeat = rsel.values.exists(_.residue.rt.category == ResidueCategory.Repeat)
              def unanimously[A](f: GraphEntry => A) = if (!repeat && rsel.values.tail.forall(f(_) == f(hd))) Some(f(hd)) else None
              val rvSelAno = $.backend.setSelAnoFn(rs).asVar(unanimously(_.residue.ano))
              val rvSelAbs = $.backend.setSelAbsFn(rs).asVar(unanimously(_.residue.abs))
              div"btn-toolbar"(^.role := "toolbar", ^.display.`inline-block`)(
                RadioAnomer(RadioGroupMap.Props[Anomer](rvSelAno, Anomer.Anomers, $.backend.getNameAnoFn)),
                RadioAbsolute(RadioGroupMap.Props[Absolute](rvSelAbs, Absolute.Absolutes, $.backend.getNameAbsFn))
              )
            },
            rsel.toList match {
              case Nil => ""
              case (id, ge) :: Nil =>
                val rvAppStateBondStatus = $.props.withReusability(BondStatus.reuseAppState)
                val first = for (link <- ge.parent.toSeq) yield BondStatus.C(BondStatus.Props(Bond(id, link), rvAppStateBondStatus))
                val rest = for ((i, from) <- ge.children.toSeq) yield BondStatus.C(BondStatus.Props(Bond(from, Link(id, i)), rvAppStateBondStatus))
                val subs = for {
                  (i, stack) <- ge.residue.subs.toSeq
                  (st, j) <- stack.zipWithIndex
                } yield SubStatus.C((id, i, j, st, $.props.withReusability(SubStatus.reuseAppState)))
                first ++ rest ++ subs
              case resList =>
                for ((id, ge) <- resList) yield {
                  div"row"(
                    div"col-xs-12"(s"${ge.residue.desc}")
                  )
                }
            }
          )
        ),
        div"panel panel-default"(
          div"panel-body"(
            for (hd <- asel.values.headOption) yield {
              val annotationText = if (asel.values.forall(_.text == hd.text)) hd.text else ""
              val fontSize = if (asel.values.forall(_.size == hd.size)) hd.size.toString else ""

              Seq(
                div"form-group input-group"(
                  "Text", <.input(
                    c"form-control",
                    ^.`type` := "text",
                    ^.value := annotationText,
                    ^.onChange ~~> preventingDefaultIOF((e: ReactEventI) => $.props.mod(AppStateL.graphL ^|-> RGraph.annotations ^|->> filterIndex(asel.contains) ^|-> Annot.text set e.target.value))
                  )
                ),
                div"form-group input-group"(
                  "Font Size", <.input(
                    c"form-control",
                    ^.`type` := "number",
                    ^.defaultValue := fontSize,
                    ^.onChange ~~> preventingDefaultIOF((e: ReactEventI) => Try(e.target.value.toDouble) match {
                      case Failure(exception) => IO.ioUnit
                      case Success(value) => $.props.mod(AppStateL.graphL ^|-> RGraph.annotations ^|->> filterIndex(asel.contains) ^|-> Annot.size set value)
                    })
                  )
                )
              )
            }
          )
        )
      ))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
