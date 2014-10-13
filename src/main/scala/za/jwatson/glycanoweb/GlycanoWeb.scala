package za.jwatson.glycanoweb

import importedjs.filereaderjs.{FileReaderJS, Opts}
import importedjs.paper
import org.scalajs.dom
import org.scalajs.dom.{HTMLInputElement, MouseEvent}
import org.scalajs.jquery.{jQuery => $, _}
import rx._
import za.jwatson.glycanoweb.render.{DisplayConv, GlycanoCanvas}
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer._
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure.ResidueCategory.Aldose
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.{BootstrapScalatags => bs}

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import scalatags.JsDom.svgTags
import scalaz.std.option._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

object GlycanoWeb extends JSApp {
  val anomeric = rx.Var[Anomer](Alpha)
  val absolute = rx.Var[Absolute](D)
  val residueCategory = rx.Var[ResidueCategory](Aldose)
  val residueType = rx.Var[Option[ResidueType]](None)
  val substituentType = rx.Var[Option[SubstituentType]](None)
  val showModeSelect = Rx(residueType().isDefined || substituentType().isDefined)
  val displayConv = rx.Var[DisplayConv](DisplayConv.convUCT)
  val bondLabels = rx.Var[Boolean](false)

  val conventionEditor = new ConventionEditor("conventionEditor")

  val iconWidth = 50
  val iconHeight = 40

  def setResidueType(rt: Option[ResidueType], skipToggle: Boolean = false): Unit = {
    val old = residueType()
    for (o <- old if !skipToggle && rt != old)
      $("#rt-" + o.desc).button("toggle")
    residueType() = rt
    if (substituentType().isDefined) setSubstituentType(none)
    if (residueType().isDefined && displayConv() != DisplayConv.convUCT)
      setResidueType(none)
  }

  def setSubstituentType(st: Option[SubstituentType], skipToggle: Boolean = false): Unit = {
    val old = substituentType()
    for (o <- old if !skipToggle && st != old)
      $("#st-" + o.symbol).button("toggle")
    substituentType() = st
    if (residueType().isDefined) setResidueType(none)
    if (substituentType().isDefined && displayConv() != DisplayConv.convUCT)
      setSubstituentType(none)
  }

  Obs(displayConv) {
    if (displayConv() != DisplayConv.convUCT) {
      setResidueType(none)
      setSubstituentType(none)
    }
  }

  def main(): Unit = {

    dom.document.body.appendChild(GlycanoPage.render)
    dom.document.body.appendChild(conventionEditor.renderModal)
    conventionEditor.setText(ConventionEditor.textUCT)

    val cv = canvas(display.`inline-block`, verticalAlign:="top", id:="stage", tabindex:=1).render
    dom.document.getElementById("stage-panel").appendChild(cv)
    val glycanoCanvas = new GlycanoCanvas(cv)

    implicit def graph: RGraph = glycanoCanvas.graph()

    def resizeCanvas(): Unit = {
      val w = $("#stage-panel").width()
      val top = $("#stage-panel").offset().asInstanceOf[js.Dynamic].top.asInstanceOf[Double]
      val docHeight = $(js.Dynamic.global.window).height()
      val h = docHeight - top - 15 - 45
      $("#stage").width(w).height(h)
      glycanoCanvas.scope.view.viewSize = new paper.Size(w, h)
      glycanoCanvas.scope.view.draw()
    }
    //dom.window.onload = {_: dom.Event => resizeCanvas()}
    $(dom.window).resize((e: JQueryEventObject) => resizeCanvas(): js.Any)

    val iconBounds = new paper.Rectangle(0, 0, iconWidth, iconHeight)

    val iconMap = (for{
      ano <- Seq[Anomer](Alpha, Beta)
      abs <- Seq[Absolute](D, L)
      rt <- ResidueType.ResidueTypes
    } yield {
      val g = glycanoCanvas.ctx.createIcon(rt, abs, ano, iconBounds)
      val svg = svgTags.svg(display:="block", width:=iconWidth.px, height:=iconHeight.px)(raw(g.outerHTML))
      (ano, abs, rt) -> svg
    }).toMap

    val substIconMap = (for (st <- SubstituentType.SubstituentTypes) yield {
      val g = glycanoCanvas.ctx.createIcon(st, iconBounds)
      st -> svgTags.svg(display:="block", width:=iconWidth.px, height:=iconHeight.px)(raw(g.outerHTML))
    }).toMap

    for ((st, icon) <- substIconMap.toSeq) {
      $("#st-" + st.symbol + " .svg").html(icon.toString())
    }


    val rtPageId = rx.Rx { (anomeric(), absolute(), residueCategory()) }
    rx.Obs(rtPageId) {
      val (abs, ano, cat) = rtPageId()
      for(rt <- ResidueType.ResidueTypes) {
        $("#rt-" + rt.desc + " .svg").html(iconMap((abs, ano, rt)).toString())
      }
    }

    $(".btn").button()
    $("input[name=conv-uct]").click()

    val tooltipsterConfig = js.Dynamic.literal(
      theme = "tooltipster-glycano": js.Any,
      position = "bottom": js.Any,
      delay = 0: js.Any,
      offsetY = -5: js.Any,
      speed = 250: js.Any
    )
    $(".residue").tooltipster(tooltipsterConfig)
    $(".substituent").tooltipster(tooltipsterConfig)

    for ((cat, i) <- ResidueCategory.ResidueCategories.zipWithIndex)
      $("#rc-" + cat.name).click { () =>
        $(s"#myTab li:eq($i) a").asInstanceOf[js.Dynamic].tab("show")
      }

    for (ano <- Anomer.Anomers) $("#ano-" + ano.symbol).click(() => anomeric() = ano)
    for (abs <- Absolute.Absolutes) $("#abs-" + abs.symbol).click(() => absolute() = abs)

    for (rt <- ResidueType.ResidueTypes) {
      val rtElem = dom.document.getElementById("rt-" + rt.desc)
      $(rtElem).click(null, (eo: JQueryEventObject) => {
        val rtOld = residueType()
        if (rtOld.contains(rt)) {
          setResidueType(none, skipToggle = true)
        } else {
          setResidueType(rt.some)
        }
      }: js.Any)
    }
    for (st <- SubstituentType.SubstituentTypes) {
      val stElem = dom.document.getElementById("st-" + st.symbol)
      $(stElem).click(null, (eo: JQueryEventObject) => {
        val stOld = substituentType()
        if (stOld.contains(st)) {
          setSubstituentType(none, skipToggle = true)
        } else {
          setSubstituentType(st.some)
        }
      }: js.Any)
    }


    Obs(showModeSelect) {
      showModeSelect() ?
        dom.document.getElementById("mode-select").removeAttribute("disabled") |
        dom.document.getElementById("mode-select").setAttribute("disabled", "disabled")
    }

    $("body").on("contextmenu", "#stage", null, (eo: JQueryEventObject) => {
      false
    }: js.Any)

    resizeCanvas()

    $("#cat-Aldose").addClass("active")

    dom.document.getElementById("mode-select").onclick = (_: MouseEvent) => {
      if(showModeSelect()) {
        glycanoCanvas.cancelPlace()
        glycanoCanvas.cancelSubst()
      }
    }

    /* examples */
//    {
//      def randomPoint(bounds: paper.Rectangle): paper.Point = {
//        paper.Point(
//          math.random * bounds.width,
//          math.random * bounds.height)
//      }
//      import za.jwatson.glycanoweb.structure.ResidueType._
//      glycanoCanvas.addResidue(Alpha, D, Ara, randomPoint(glycanoCanvas.scope.view.bounds))
//      glycanoCanvas.addResidue(Alpha, L, Ido, randomPoint(glycanoCanvas.scope.view.bounds))
//      glycanoCanvas.addResidue(Beta, D, Lyx, randomPoint(glycanoCanvas.scope.view.bounds))
//    }

    val overviewTitle = Rx {
      val sel = glycanoCanvas.selection()
      sel.size match {
        case 0 => ""
        case 1 => sel.head.desc
        case n => s"$n residues"
      }
    }
    Obs(overviewTitle)($("#overview-title").html(overviewTitle()))

    def filename = {
      val fn = $("#filename").value().asInstanceOf[String]
      fn.isEmpty ? "glycano" | fn
    }

    $("#save-gly").click(null, (eo: JQueryEventObject) => {
      import Gly._
      import upickle._
      val gly = write(Gly.from(glycanoCanvas))(rwGly)
      val base64 = dom.window.btoa(gly)
      val dataUrl = "data:text/plain;base64," + base64
      glycanoCanvas.scope.view.draw()
      $("#save-gly").attr("href", dataUrl).attr("download", filename + ".gly")
    }: js.Any)

    $("#save-png").click(null, (eo: JQueryEventObject) => {
      val dataUrl = cv.toDataURL("PNG")
      glycanoCanvas.scope.view.draw()
      $("#save-png").attr("href", dataUrl).attr("download", filename + ".png")
    }: js.Any)

    $("#save-svg").click(null, (eo: JQueryEventObject) => {
      import js.Dynamic.{global => g}
      val svg = glycanoCanvas.scope.project.exportSVG(js.Dynamic.literal(asString = true: js.Any))
      val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(svg)).asInstanceOf[String])
      val dataUrl = "data:image/svg+xml;base64," + base64
      $("#save-svg").attr("href", dataUrl).attr("download", filename + ".svg")
    }: js.Any)

    $("#navbar-clear-btn").click(null, (eo: JQueryEventObject) => {
      glycanoCanvas.clearAll()
      glycanoCanvas.redraw()
    }: js.Any)

    val fileReaderOpts = Opts.load((e: dom.ProgressEvent, file: dom.File) => {
      import upickle._, Gly._
      val str = e.target.asInstanceOf[js.Dynamic].result.asInstanceOf[String]
      val gly = read[Gly](str)(rwGly)

      glycanoCanvas.loadGly(gly)
      $("#filename").value(file.name)
    })
    fileReaderOpts.readAsDefault = "Text"
    FileReaderJS.setupInput($("#upload-file").get(0).asInstanceOf[HTMLInputElement], fileReaderOpts)

    val casperText = Rx {
      glycanoCanvas.residues()
      glycanoCanvas.bonds()
      val sel = glycanoCanvas.selection()
      CASPER.getStrings(sel).values.mkString("; ")
    }
    Obs(casperText) {
      dom.document.getElementById("casper").setAttribute("value", casperText())
    }

    def deleteBond(b: Bond) = {
      span(cls:="pull-right", button(cls:="btn btn-link btn-xs", "remove bond", onclick:={() =>
        glycanoCanvas.removeBond(b)
        glycanoCanvas.addToHistory()
        glycanoCanvas.redraw()
      }))
    }

    def deleteSubstituent(s: Substituent) = {
      span(cls:="pull-right", button(cls:="btn btn-link btn-xs", "remove", onclick:={() =>
        glycanoCanvas.removeSubstituent(s)
        glycanoCanvas.redraw()
      }))
    }

    val overviewIconSize = new paper.Rectangle(0, 0, 40, 30)
    val overviewBondIconSize = new paper.Rectangle(0, 0, 60, 30)
    val overviewSubstituentSize = new paper.Rectangle(0, 0, 30, 30)

    def overviewBond(b: Bond) = {
      val from = b.from
      val to = b.to.residue
      val svgFrom = glycanoCanvas.ctx.createIcon(b.from.rt, b.from.absolute, b.from.anomer, overviewIconSize)
      val svgBond = glycanoCanvas.ctx.createBondIcon(b.from.anomer, overviewBondIconSize)
      val svgTo = glycanoCanvas.ctx.createIcon(b.to.residue, overviewIconSize)
      div(Seq(svgFrom, svgBond, svgTo).map(g =>
        span(
          marginRight:=4.px,
          svgTags.svg(
            display.`inline-block`,
            width:=overviewIconSize.width.px,
            height:=overviewIconSize.height.px,
            raw(g.outerHTML)
          )
        )
      ): _*)
    }

    def overviewSubstituent(s: Substituent) = {
      val svgSubstituent = glycanoCanvas.ctx.createIcon(s.st, overviewSubstituentSize)
      span(
        svgTags.svg(
          display.`inline-block`,
          width:=overviewSubstituentSize.width.px,
          height:=overviewSubstituentSize.height.px,
          raw(svgSubstituent.outerHTML)
        )
      )
    }

    def setBondHL(b: Bond): Unit = {
      val e = dom.document.getElementById(s"bond-row-${b.from.id}")
      e.onmouseover = {(_: MouseEvent) => glycanoCanvas.ctx.bondHL(b.some)}
      e.onmouseout = {(_: MouseEvent) => glycanoCanvas.ctx.bondHL(none)}
    }

    val overviewContent = Rx {
      glycanoCanvas.selection().toList match {
        case Nil => div()
        case res :: Nil =>
          val first = for(parent @ Link(to, _) <- res.parent) yield {
            val b = Bond(res, parent)
            bs.row(
              bs.col(xs=10)(overviewBond(b)), bs.col(xs=2)(deleteBond(b)),
              onmouseover := {() => glycanoCanvas.ctx.bondHL(b.some); glycanoCanvas.redraw()},
              onmouseout := {() => glycanoCanvas.ctx.bondHL(none); glycanoCanvas.redraw()}
            )
          }
          val rest = for(ch <- res.children.toSeq; (i, src) <- ch.toSeq.sortBy(_._1)) yield {
            val b = Bond(src, Link(res, i))
            bs.row(
              bs.col(xs=10)(overviewBond(b)), bs.col(xs=2)(deleteBond(b)),
              onmouseover := {() => glycanoCanvas.ctx.bondHL(b.some); glycanoCanvas.redraw()},
              onmouseout := {() => glycanoCanvas.ctx.bondHL(none); glycanoCanvas.redraw()}
            )
          }
          val substs = for {
            (i, ss) <- res.substituents
            s <- ss
          } yield {
            bs.row(
              bs.col(xs=2)(i), bs.col(xs=2)(overviewSubstituent(s)), bs.col(xs=6)(s.st.name), bs.col(xs=2)(deleteSubstituent(s)),
              onmouseover := {() => glycanoCanvas.ctx.substituentHL(s.some); glycanoCanvas.redraw()},
              onmouseout := {() => glycanoCanvas.ctx.substituentHL(none); glycanoCanvas.redraw()}
            )
          }
          div((first ++ rest ++ substs).toSeq)
        case ress => div(ress.map(r => div(r.desc)))
      }
    }

    val ov = dom.document.getElementById("overview-body")
    Obs(overviewContent) {
      while(ov.hasChildNodes()) ov.removeChild(ov.firstChild)
      ov.appendChild(overviewContent().render)
    }

    glycanoCanvas.redraw()

  }

}