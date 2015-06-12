package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{ReusableFn, ~=>, Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import monocle.Monocle._
import za.jwatson.glycanoweb.react.bootstrap.RadioGroupMap
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph.{GraphEntry, Bond}
import za.jwatson.glycanoweb.structure._

import scala.util.{Success, Failure, Try}
import scalaz.effect.IO

object OverviewPanel {
  def selected(in: Set[ResidueId]) = RGraph.residues ^|->> filterIndex(in.contains) ^|-> GraphEntry.residue
  class Backend($: BackendScope[Props, Unit]) {
    implicit val reuseSelection: Reusability[Set[ResidueId]] = Reusability.by_==

    val setSelAnoFn = ReusableFn { (sel: Set[ResidueId], ano: Option[Anomer]) =>
      ano.fold(IO.ioUnit)(ano => $.props._1.mod(selected(sel) ^|-> Residue.ano set ano))
    }
    val setSelAbsFn = ReusableFn { (sel: Set[ResidueId], abs: Option[Absolute]) =>
      abs.fold(IO.ioUnit)(abs => $.props._1.mod(selected(sel) ^|-> Residue.abs set abs))
    }

    val getNameAnoFn = ReusableFn((_: Anomer).desc)
    val getNameAbsFn = ReusableFn((_: Absolute).desc)
  }

  val RadioAnomer = RadioGroupMap[Anomer]
  val RadioAbsolute = RadioGroupMap[Absolute]

  type Props = (ReusableVar[RGraph], (Set[ResidueId], Set[AnnotId]), DisplayConv, ReusableVar[Option[ResidueId]])

  implicit val reuseSelection: Reusability[(Set[ResidueId], Set[AnnotId])] = Reusability.by_==
  val C = ReactComponentB[Props]("OverviewPanel")
    .stateless
    .backend(new Backend(_))
    .render { $ =>
      val (rvGraph, (rs, as), dc, rvHighlightBond) = $.props
      implicit val graph: RGraph = rvGraph.value
      val rsel = graph.residues.filterKeys(rs.contains)
      val asel = graph.annotations.filterKeys(as.contains)

      <.div(^.cls := "row")(<.div(^.cls := "col-xs-12")(
        <.div(^.cls := "panel panel-default")(
          <.div(^.cls := "panel-body")(
            for (hd <- rsel.values.headOption) yield {
              val repeat = rsel.values.exists(_.residue.rt.category == ResidueCategory.Repeat)
              def unanimously[A](f: GraphEntry => A) = if (!repeat && rsel.values.tail.forall(f(_) == f(hd))) Some(f(hd)) else None
              val rvSelAno = $.backend.setSelAnoFn(rs).asVar(unanimously(_.residue.ano))
              val rvSelAbs = $.backend.setSelAbsFn(rs).asVar(unanimously(_.residue.abs))
              <.div(^.cls := "btn-toolbar", ^.role := "toolbar", ^.display.`inline-block`)(
                RadioAnomer(RadioGroupMap.Props[Anomer](rvSelAno, Anomer.Anomers, $.backend.getNameAnoFn)),
                RadioAbsolute(RadioGroupMap.Props[Absolute](rvSelAbs, Absolute.Absolutes, $.backend.getNameAbsFn))
              )
            },
            rsel.toList match {
              case Nil => ""
              case (id, ge) :: Nil =>
                val first = for (link <- ge.parent.toSeq) yield BondStatus.C((Bond(id, link), rvGraph, dc, rvHighlightBond))
                val rest = for ((i, from) <- ge.children.toSeq) yield BondStatus.C((Bond(from, Link(id, i)), rvGraph, dc, rvHighlightBond))
                val subs = for {
                  (i, stack) <- ge.residue.subs.toSeq
                  (st, j) <- stack.zipWithIndex
                } yield SubStatus.C((id, i, j, st, rvGraph))
                first ++ rest ++ subs
              case resList =>
                for ((id, ge) <- resList) yield {
                  <.div(^.cls := "row")(
                    <.div(^.cls := "col-xs-12")(s"${ge.residue.desc}")
                  )
                }
            }
          )
        ),
        <.div(^.cls := "panel panel-default")(
          <.div(^.cls := "panel-body")(
            for (hd <- asel.values.headOption) yield {
              val annotationText = if (asel.values.forall(_.text == hd.text)) hd.text else ""
              val fontSize = if (asel.values.forall(_.size == hd.size)) hd.size.toString else ""

              <.div(
                <.div(^.cls := "form-group input-group")(
                  "Text", <.input(
                    ^.cls := "form-control",
                    ^.`type` := "text",
                    ^.value := annotationText,
                    ^.onChange ~~> ((e: ReactEventI) => for {
                      _ <- e.preventDefaultIO
                      _ <- $.props._1.mod(RGraph.annotations ^|->> filterIndex(asel.contains) ^|-> Annot.text set e.target.value)
                    } yield ())
                  )
                ),
                <.div(^.cls := "form-group input-group")(
                  "Font Size", <.input(
                    ^.cls := "form-control",
                    ^.`type` := "number",
                    ^.value := fontSize,
                    ^.onChange ~~> ((e: ReactEventI) => for {
                      _ <- e.preventDefaultIO
                      _ <- Try(e.target.value.toDouble) match {
                        case Failure(exception) => IO.ioUnit
                        case Success(value) => $.props._1.mod(RGraph.annotations ^|->> filterIndex(asel.contains) ^|-> Annot.size set value)
                      }
                    } yield ())
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
