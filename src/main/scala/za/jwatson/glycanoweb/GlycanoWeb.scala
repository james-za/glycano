package za.jwatson.glycanoweb

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.{jQuery => $, _}
import org.scalajs.dom
import za.jwatson.glycanoweb.structure._
import scalatags.JsDom._
import scalatags.JsDom.all._
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Alpha, Beta}
import org.scalajs.dom.{MouseEvent, HTMLCanvasElement}
import importedjs.paper
import za.jwatson.glycanoweb.render.GlycanoCanvas
import rx._

@JSExport
object GlycanoWeb {

  trait RichJQuery extends org.scalajs.jquery.JQuery {
    def button(state: js.String = ???): JQuery = ???
    def tooltip(options: js.Dynamic = ???): JQuery = ???
    def tooltipster(options: js.Dynamic = ???): JQuery = ???
  }
  import scala.language.implicitConversions
  implicit def richJQuery(jq: JQuery) = jq.asInstanceOf[RichJQuery]

  val anomeric = rx.Var[Anomer](Alpha)
  val absolute = rx.Var[Absolute](D)
  val residueType = rx.Var[Option[ResidueType]](None)

  @JSExport
  def main(): Unit = {

    val iconWidth = 50
    val iconHeight = 40

    import BootstrapScalatags._

    /** Toggles for absolute and anomeric configuration */
    val residueConfig =
      btnToolbar(display.`inline-block`)(
        btnGroup(
          radioButton(inputName = "anomeric", classes = "active")(id:=Alpha.symbol, Alpha.desc),
          radioButton(inputName = "anomeric")(id:=Beta.symbol, Beta.desc)
        ),
        btnGroup(
          radioButton(inputName = "absolute", classes = "active")(id:=D.symbol, D.desc),
          radioButton(inputName = "absolute")(id:=L.symbol, L.desc)
        )
      )

    /** Selection of residue type to create */
    val residuePages = btnGroup(id:="aldoses")(
      for (rt <- ResidueType.Aldoses) yield span(
        checkboxButton(inputName = "rt", classes = "residue", innerMods = Seq(display.none))(id:=rt.desc, title:=rt.desc, padding:="2px")(
          div(cls:="svg")
        )
      )
    )

    /** Actions related to creating residues */
    val residuePanel =
      panel(Default)(
        panelHeading("Residues"),
        panelBody(classes = "text-center")(
          row(col(md=12)(residueConfig)),
          row(col(md=12)(residuePages))
        )
      )

    /** Selection of substituent to add to residues */
    val substituentPanel =
      panel(Default)(
        panelHeading("Substituents"),
        panelBody("")
      )

    /** Displaying and parsing CASPER text format */
    val casperForm =
      /*form("role".attr:="form")(*/
        div(cls:="form-group input-group")(
          input(`type`:="text", cls:="form-control"),
          span(cls:="input-group-btn")(btn()("ASDF")),
          span(cls:="input-group-btn")(btn()("ASDF"))
        )
      /*)*/

    /** Main canvas element */
    val stagePanel =
      panel()(id:="stage-panel")(
        canvas(display.`inline-block`, verticalAlign:="top", id:="stage"/*,
          "width".attr:="auto", "height".attr:="auto"*/, tabindex:=1)
      )

    /** Overview of selected residues */
    val overviewPanel =
      panel(Primary)(
        panelHeading(glyphIcon("list-alt")(marginRight:=15.px), span(id:="overview-title")),
        panelBody()(id:="overview-body")
      )

    /** Page header and common operations */
    val glycanoNavbar =
      navbar(Default)(containerFluid(
        div(cls:="navbar-header")(
          button(`type`:="button", cls:="navbar-toggle",
            "data-toggle".attr:="collapse",
            "data-target".attr:="#glycano-navbar-collapse"),
          a(cls:="navbar-brand", href:="#")("Glycano")
        ),
        div(cls:="collapse navbar-collapse", id:="glycano-navbar-collapse")(
          ul(cls:="nav navbar-nav")(
            li(a(href:="#")("ASDF"))
          )
        )
      ))

    /** Main container */
    $("body").html(containerFluid(
      row(glycanoNavbar),
      row(
        col(md=3)(
          row(col(md=12)(residuePanel)),
          row(col(md=12)(substituentPanel))
        ),
        col(md=6)(
          row(col(md=12)(casperForm)),
          row(col(md=12)(stagePanel))
        ),
        col(md=3)(overviewPanel)
      )
    ).toString())

    val cv = $("#stage").get(0).asInstanceOf[HTMLCanvasElement]
    val glycanoCanvas = new GlycanoCanvas(cv)

    def resizeCanvas(): Unit = {
      val w = $("#stage-panel").width()
      val top = $("#stage-panel").offset().asInstanceOf[js.Dynamic].top.asInstanceOf[Double]
      val docHeight = $(js.Dynamic.global.window).height()
      val h = docHeight - top - 15
      $("#stage").width(w).height(h)
      glycanoCanvas.scope.view.viewSize = new paper.Size(w, h)
      glycanoCanvas.scope.view.draw()
    }
    dom.onload = {_: dom.Event => resizeCanvas()}
    $(dom.window).resize((e: JQueryEventObject) => resizeCanvas(): js.Any)

    val iconBounds = new paper.Rectangle(0, 0, iconWidth, iconHeight)

    val iconMap = (for{
      ano <- Seq[Anomer](Alpha, Beta)
      abs <- Seq[Absolute](D, L)
      rt <- ResidueType.ResidueTypes
    } yield {
      val g = glycanoCanvas.convention().createIcon(rt, abs, ano, iconBounds)
      (ano, abs, rt) -> svgTags.svg(display:="block", width:=iconWidth.px, height:=iconHeight.px)(raw(g.outerHTML))
    }).toMap

    val rtPageId = rx.Rx {
      (anomeric(), absolute())
    }
    rx.Obs(rtPageId) {
      val (abs, ano) = rtPageId()
      for(rt <- ResidueType.ResidueTypes) {
        $("#" + rt.desc + " .svg").html(iconMap((abs, ano, rt)).toString())
      }
    }

    $(".btn").button()

    val tooltipsterConfig = js.Dynamic.literal(
      theme = "tooltipster-glycano": js.Any,
      position = "bottom": js.Any,
      delay = 0: js.Any,
      offsetY = -5: js.Any,
      speed = 250: js.Any
    )
    $(".residue").tooltipster(tooltipsterConfig)
    $("#" + D.symbol).click(() => absolute() = D)
    $("#" + L.symbol).click(() => absolute() = L)
    $("#" + Alpha.symbol).click(() => anomeric() = Alpha)
    $("#" + Beta.symbol).click(() => anomeric() = Beta)
    for(rt <- ResidueType.ResidueTypes) {
      val rtElem = dom.document.getElementById(rt.desc)
      rtElem.onclick = (e: MouseEvent) => {
        val wasActive = $(rtElem).hasClass("active")
        $(".residue.active").not(rtElem).removeClass("active").button("reset")
        residueType() = if(wasActive) None else Some(rt)
      }
    }

    $("body").on("contextmenu", "#stage", null, (eo: JQueryEventObject) => {
      false
    }: js.Any)

//    val r1 = Residue(ResidueType.Gal, Alpha, D)
//    val r2 = Residue(ResidueType.Gal, Beta, D)
//    val r3 = Residue(ResidueType.Gal, Alpha, L)
//    val r4 = Residue(ResidueType.Xyl, Beta, L)
//    val r5 = Residue(ResidueType.Rib, Beta, L)
//
//    glycanoCanvas.residues ++= Seq(r1, r2, r3, r4, r5)
//
//    glycanoCanvas.bonds ++= Seq(
//      r4.bond(r2, 1),
//      r3.bond(r2, 2),
//      r2.bond(r1, 1),
//      r5.bond(r1, 2)
//    )
//
//    for(r <- residues) {
//      glycanoCanvas.addResidue(r, randomPoint(glycanoCanvas.scope.view.bounds))
//      println(r)
//    }
//
//    for(b <- bonds) {
//      glycanoCanvas.addBond(b)
//    }

    resizeCanvas()

    import ResidueType._
    glycanoCanvas.addResidue(Alpha, D, Ara, randomPoint(glycanoCanvas.scope.view.bounds))
    glycanoCanvas.addResidue(Alpha, L, Ido, randomPoint(glycanoCanvas.scope.view.bounds))
    glycanoCanvas.addResidue(Beta, D, Lyx, randomPoint(glycanoCanvas.scope.view.bounds))

    val overviewTitle = Rx {
      val sel = glycanoCanvas.selection()
      sel.size match {
        case 0 => ""
        case 1 => sel.head.desc
        case n => s"$n residues"
      }
    }
    Obs(overviewTitle)($("#overview-title").html(overviewTitle()))

    val overviewContent = Rx {
      val sel = glycanoCanvas.selection()
      sel.size match {
        case 0 => ""
        case 1 =>
          div(
            for {
              b <- sel.head.bonds.toSeq.sortBy(_._1).map(_._2)
            } yield div(s"${b.from.residue.desc}(${b.from.position}->${b.to.position})${b.to.residue.desc}")
          ).toString()
        case n => div(sel.toSeq.map(s => div(s.desc))).toString()
      }
    }
    Obs(overviewContent)($("#overview-body").html(overviewContent()))

    glycanoCanvas.scope.view.draw()

  }

  def randomPoint(bounds: paper.Rectangle): paper.Point = {
    paper.Point(
      math.random * bounds.width,
      math.random * bounds.height)
  }
}