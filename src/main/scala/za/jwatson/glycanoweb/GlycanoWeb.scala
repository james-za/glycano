package za.jwatson.glycanoweb

import importedjs.paper
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.jquery.{jQuery => $, _}
import rx._
import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Alpha, Beta}
import za.jwatson.glycanoweb.structure.Residue.Link
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.structure.RGraph._

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom._
import scalatags.JsDom.all._

@JSExport
object GlycanoWeb {

  trait RichJQuery extends org.scalajs.jquery.JQuery {
    def button(state: js.String = ???): JQuery = ???
    def tooltip(options: js.Dynamic = ???): JQuery = ???
    def tooltipster(options: js.Dynamic): JQuery = ???
  }
  import scala.language.implicitConversions
  implicit def richJQuery(jq: JQuery) = jq.asInstanceOf[RichJQuery]

  val anomeric = rx.Var[Anomer](Alpha)
  val absolute = rx.Var[Absolute](D)
  val residueType = rx.Var[Option[ResidueType]](None)

  val conventionEditor = new ConventionEditor("conventionEditor")

  @JSExport
  def main(): Unit = {

    val iconWidth = 50
    val iconHeight = 40

    import za.jwatson.glycanoweb.BootstrapScalatags._

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
          row(col(xs=12)(residueConfig)),
          row(col(xs=12)(residuePages))
        )
      )

    /** Selection of substituent to add to residues */
    val substituentPanel =
      panel(Default)(
        panelHeading("Substituents"),
        panelBody("")(id:="subst")
      )

    /** Displaying and parsing CASPER text format */
    val casperForm =
      /*form("role".attr:="form")(*/
        div(cls:="form-group input-group")(
          input(id:="casper", `type`:="text", cls:="form-control"),
          span(cls:="input-group-btn")(btn()(id:="casper-parse", "Parse"))
        )
      /*)*/

    /** Main canvas element */
    val stagePanel = panel()(id:="stage-panel")()

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
            li(a(href:="#")("ASDF")),
            li(conventionEditor.createNavButton)
          )
        )
      ))



//    val modeCreate =
//      col(xs=6)(btn(Primary, Lg, block = true)("Create")(id:="mode-create", onclick:={
//        if(residueType().isEmpty) residueType() = Some(ResidueType.Glycero)
//      }))
    val modeSelect =
      col(xs=12)(btn(Primary, Lg, block = true)(glyphIcon("hand-up")(marginRight:=15.px), "Selection Mode")(id:="mode-select", onclick:={() =>
        if(residueType().isDefined) residueType() = None
      }))

    /** Main container */
    val mainContainer = containerFluid(
      row(glycanoNavbar),
      row(
        col(xs=3)(
          row(col(xs=12)(residuePanel)),
          row(col(xs=12)(substituentPanel))
        ),
        col(xs=6)(
          row(col(xs=12)(casperForm)),
          row(col(xs=12)(stagePanel)),
          row(modeSelect)
        ),
        col(xs=3)(overviewPanel)
      )
    )

    dom.document.body.appendChild(mainContainer.render)
    dom.document.body.appendChild(conventionEditor.renderModal)

    val cv = canvas(display.`inline-block`, verticalAlign:="top", id:="stage", tabindex:=1).render
    dom.document.getElementById("stage-panel").appendChild(cv)
    val glycanoCanvas = new GlycanoCanvas(cv)

    implicit def graph = glycanoCanvas.graph()

    def resizeCanvas(): Unit = {
      val w = $("#stage-panel").width()
      val top = $("#stage-panel").offset().asInstanceOf[js.Dynamic].top.asInstanceOf[Double]
      val docHeight = $(js.Dynamic.global.window).height()
      val h = docHeight - top - 15 - 45
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

    val substIconMap = (for (st <- SubstituentType.substituentTypes) yield {
      val g = glycanoCanvas.convention().createIcon(st, iconBounds)
      st -> svgTags.svg(display:="block", width:=iconWidth.px, height:=iconHeight.px)(raw(g.outerHTML))
    }).toMap

    val substPage = div(for ((st, icon) <- substIconMap.toSeq) yield span(
      checkboxButton(inputName = "rt", classes = "residue", innerMods = Seq(display.none))(id := "st-" + st.symbol, title := st.name, padding := "2px")(
        div(icon)
      )
    ))

    dom.document.getElementById("subst").appendChild(substPage.render)


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
        residueType() = if($(rtElem).hasClass("active")) None else Some(rt)
      }
    }

    Obs(residueType) {
      residueType().fold {
        dom.document.getElementById("mode-select").setAttribute("disabled", "disabled")
        $(".residue.active").removeClass("active").button("reset")
      } { rt =>
        dom.document.getElementById("mode-select").removeAttribute("disabled")
        val elem = dom.document.getElementById(rt.desc)
        $(".residue.active").not(elem).removeClass("active").button("reset")
        //$(elem).button("toggle")
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

    import za.jwatson.glycanoweb.structure.ResidueType._
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

    val casperText = Rx {
      glycanoCanvas.residues()
      glycanoCanvas.bonds()
      val sel = glycanoCanvas.selection()
      CASPER.getStrings(sel).values.mkString("; ")
    }
    Obs(casperText) {
      dom.document.getElementById("casper").setAttribute("value", casperText())
    }

    val overviewContent = Rx {
      glycanoCanvas.selection().toList match {
        case Nil => div()
        case res :: Nil =>
          val first = for(Link(to, p) <- res.parent) yield div(s"${res.desc}(1->$p)${to.desc}")
          val rest = for(ch <- res.children.toSeq; (i, src) <- ch) yield div(s"${src.desc}(1->$i)${res.desc}")
          div((first ++ rest).toSeq)
        case ress => div(ress.map(r => div(r.desc)))
      }
    }
    val ov = dom.document.getElementById("overview-body")
    Obs(overviewContent) {
      while(ov.hasChildNodes()) ov.removeChild(ov.firstChild)
      ov.appendChild(overviewContent().render)
    }

    glycanoCanvas.scope.view.draw()

  }

  def randomPoint(bounds: paper.Rectangle): paper.Point = {
    paper.Point(
      math.random * bounds.width,
      math.random * bounds.height)
  }
}