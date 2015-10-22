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
  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  def selectedResidues(in: Set[ResidueId]) = AppStateL.graphL ^|-> RGraph.residues ^|->> filterIndex(in.contains) ^|-> GraphEntry.residue
  def selectedAnnotations(asel: Map[AnnotId, Annot]) = AppStateL.graphL ^|-> RGraph.annotations ^|->> filterIndex(asel.contains)

  class Backend($: BackendScope[ReusableVar[AppState], Unit]) {
    val setSelAnoFn = ReusableFn[Set[ResidueId], Option[Anomer], Callback] { (sel, ano) =>
      for {
        anomer <- CallbackOption.liftOption(ano)
        rvAppState <- $.props
        _ <- rvAppState.mod(selectedResidues(sel) ^|-> Residue.ano set anomer)
      } yield ()
    }
    val setSelAbsFn = ReusableFn[Set[ResidueId], Option[Absolute], Callback] { (sel, abs) =>
      for {
        absolute <- CallbackOption.liftOption(abs)
        rvAppState <- $.props
        _ <- rvAppState.mod(selectedResidues(sel) ^|-> Residue.abs set absolute)
      } yield ()
    }

    val getNameAnoFn = ReusableFn((_: Anomer).desc)
    val getNameAbsFn = ReusableFn((_: Absolute).desc)

    def changeText(asel: Map[AnnotId, Annot])(e: ReactEventI) = for {
      _ <- e.preventDefaultCB
      rvAppState <- $.props
      _ <- rvAppState.mod(selectedAnnotations(asel) ^|-> Annot.text set e.target.value)
    } yield ()

    def changeFontSize(asel: Map[AnnotId, Annot])(e: ReactEventI) = for {
      _ <- e.preventDefaultCB
      rvAppState <- $.props
      value <- CallbackOption.liftOptionLike(Try(e.target.value.toDouble))
      _ <- rvAppState.mod(selectedAnnotations(asel) ^|-> Annot.size set value)
    } yield ()

    def render(rvAppState: ReusableVar[AppState]) = {
      val appState = rvAppState.value
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
              val rvSelAno = setSelAnoFn(rs).asVar(unanimously(_.residue.ano))
              val rvSelAbs = setSelAbsFn(rs).asVar(unanimously(_.residue.abs))
              div"btn-toolbar"(^.role := "toolbar", ^.display.`inline-block`)(
                RadioAnomer(RadioGroupMap.Props[Anomer](rvSelAno, Anomer.Anomers, getNameAnoFn)),
                RadioAbsolute(RadioGroupMap.Props[Absolute](rvSelAbs, Absolute.Absolutes, getNameAbsFn))
              )
            },
            rsel.toList match {
              case Nil => ""
              case (id, ge) :: Nil =>
                val rvAppStateBondStatus = rvAppState.withReusability(BondStatus.reuseAppState)
                val first = for (link <- ge.parent.toSeq) yield BondStatus.C(BondStatus.Props(Bond(id, link), rvAppStateBondStatus))
                val rest = for ((i, from) <- ge.children.toSeq) yield BondStatus.C(BondStatus.Props(Bond(from, Link(id, i)), rvAppStateBondStatus))
                val subs = for {
                  (i, stack) <- ge.residue.subs.toSeq
                  (st, j) <- stack.zipWithIndex
                } yield SubStatus.C((id, i, j, st, rvAppState.withReusability(SubStatus.reuseAppState)))
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
                    ^.onChange ==> changeText(asel)
                  )
                ),
                div"form-group input-group"(
                  "Font Size", <.input(
                    c"form-control",
                    ^.`type` := "number",
                    ^.defaultValue := fontSize,
                    ^.onChange ==> changeFontSize(asel)
                  )
                )
              )
            }
          )
        )
      ))
    }
  }

  val reuseAppState: Reusability[AppState] = Reusability.by((s: AppState) => (s.graph, s.selection, s.displayConv, s.highlightBond, s.view))(Reusability.by_==)
  val C = ReactComponentB[ReusableVar[AppState]]("OverviewPanel")
    .stateless
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
