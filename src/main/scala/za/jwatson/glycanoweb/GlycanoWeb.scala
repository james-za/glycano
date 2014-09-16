package za.jwatson.glycanoweb

import importedjs.filereaderjs.{FileReaderJS, Opts}
import importedjs.paper
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, HTMLInputElement}
import org.scalajs.jquery.{jQuery => $, _}
import rx._
import upickle._
import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer._
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure.ResidueCategory.Aldose
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom._
import scalatags.JsDom.all._
import scalaz.std.option._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

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
  val residueCategory = rx.Var[ResidueCategory](Aldose)
  val residueType = rx.Var[Option[ResidueType]](None)
  val substituentType = rx.Var[Option[SubstituentType]](None)
  val showModeSelect = Rx((residueType() orElse substituentType()).isDefined)

  val conventionEditor = new ConventionEditor("conventionEditor")

  def setResidueType(rt: Option[ResidueType], skipToggle: Boolean = false): Unit = {
    val old = residueType()
    for (o <- old if !skipToggle && rt != old)
      $("#rt-" + o.desc).button("toggle")
    residueType() = rt
    if (substituentType().isDefined) setSubstituentType(none)
  }

  def setSubstituentType(st: Option[SubstituentType], skipToggle: Boolean = false): Unit = {
    val old = substituentType()
    for (o <- old if !skipToggle && st != old)
      $("#st-" + o.symbol).button("toggle")
    substituentType() = st
    if (residueType().isDefined) setResidueType(none)
  }

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
//    val residuePages = btnGroup(id:="aldoses")(
//      for (rt <- ResidueType.Aldoses) yield span(
//        checkboxButton(inputName = "rt-" + rt.desc, classes = "residue", innerMods = Seq(display.none))
//          (id := "rt-" + rt.desc, title := rt.desc, padding := "2px")
//          (div(cls := "svg"))
//      )
//    )
    val residuePages = div(
      ul(cls:="nav nav-tabs", "role".attr:="tablist", id:="res-tabs")(
        for (cat <- ResidueCategory.ResidueCategories) yield
          li(a(href:=("#cat-" + cat.name), "role".attr:="tab", "data-toggle".attr:="tab")(
            cat.name
          ))
      ),
      btnGroup(id:="aldoses")(
        div(cls:="tab-content")(
          for (cat <- ResidueCategory.ResidueCategories) yield div(cls:="tab-pane", id:=("cat-" + cat.name))(
            for (rt <- ResidueType.ResidueTypeCategories(cat)) yield span(
              checkboxButton(inputName = "rt-" + rt.desc, classes = "residue", innerMods = Seq(display.none))
                (id := "rt-" + rt.desc, title := rt.desc, padding := "2px")
                (div(cls := "svg"))
            )
          )
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

    /** Selection of substituent type to add to residues */
    val substituentPages = btnGroup(id:="substituents")(
      for (st <- SubstituentType.SubstituentTypes) yield span(
        checkboxButton(inputName = "st-" + st.symbol, classes = "substituent", innerMods = Seq(display.none))
          (id := "st-" + st.symbol, title := st.name, padding := "2px")
          (div(cls := "svg"))
      )
    )

    /** Selection of substituent to add to residues */
    val substituentPanel =
      panel(Default)(
        panelHeading("Substituents"),
        panelBody(classes = "text-center")(
          row(col(xs=12)(substituentPages))
        )
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

    val saveDropdown = li(cls:="dropdown")(
      a(href:="#", cls:="dropdown-toggle", "data-toggle".attr:="dropdown", "Save", span(cls:="caret")),
      ul(cls:="dropdown-menu", "role".attr:="menu")(
        li(a(href:="#", "Glycano (gly)", id:="save-gly")),
        li(cls:="divider"),
        li(a(href:="#", "Image (png)", id:="save-png")),
        li(a(href:="#", "Vector (svg)", id:="save-svg"))
      )
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
            li(p(cls:="navbar-text", "Load:")),
            form(cls:="navbar-form navbar-left")(
              formGroup(input(tpe:="file", cls:="form-control", id:="upload-file"))
            ),
            li(p(cls:="navbar-text", "Filename:")),
            form(cls:="navbar-form navbar-left")(
              formGroup(input(id:="filename", tpe:="text", cls:="form-control", placeholder:="Filename", value:="glycano"))
            ),
            saveDropdown,
            li(a(href:="#")("")),
            li(conventionEditor.createNavButton)
          )
        )
      ))



//    val modeCreate =
//      col(xs=6)(btn(Primary, Lg, block = true)("Create")(id:="mode-create", onclick:={
//        if(residueType().isEmpty) residueType() = Some(ResidueType.Glycero)
//      }))
    val modeSelect =
      col(xs=12)(
        btn(Primary, Lg, block = true)(glyphIcon("hand-up")(marginRight:=15.px), "Selection Mode", id:="mode-select")
      )

    val conventionPanel =
      panel(Default)(
        panelHeading("Convention"),
        panelBody(classes = "text-center")(
          btnGroup(
            radioButton(Default, "conv-uct")("UCT"),
            radioButton(Default, "conv-cfg")("CFG"),
            radioButton(Default, "conv-oxford")("Oxford")
          )
        )
      )

    /** Main container */
    val mainContainer = containerFluid(
      row(glycanoNavbar),
      row(
        col(xs=3)(
          row(col(xs=12)(conventionPanel)),
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
    $("#" + conventionEditor.textAreaId).`val`(ConventionEditor.testText)

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
    //dom.window.onload = {_: dom.Event => resizeCanvas()}
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

    val substIconMap = (for (st <- SubstituentType.SubstituentTypes) yield {
      val g = glycanoCanvas.convention().createIcon(st, iconBounds)
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
    $("#" + D.symbol).click(() => absolute() = D)
    $("#" + L.symbol).click(() => absolute() = L)
    $("#" + Alpha.symbol).click(() => anomeric() = Alpha)
    $("#" + Beta.symbol).click(() => anomeric() = Beta)
    for (rt <- ResidueType.Aldoses) {
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

    def filename = {
      val fn = $("#filename").value().asInstanceOf[String]
      fn.isEmpty ? "glycano" | fn
    }

    $("#save-gly").click(null, (eo: JQueryEventObject) => {
      import upickle._
      val gly = write(Gly.from(glycanoCanvas))
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
      val svg = glycanoCanvas.scope.project.exportSVG(js.Dynamic.literal(asString = true: js.Any))
      val base64 = dom.window.btoa(svg.asInstanceOf[String])
      val dataUrl = "data:image/svg+xml;base64," + base64
      $("#save-svg").attr("href", dataUrl).attr("download", filename + ".svg")
    }: js.Any)

    val fileReaderOpts = Opts.load((e: dom.ProgressEvent, file: dom.File) => {
      val str = e.target.asInstanceOf[js.Dynamic].result.asInstanceOf[String]
      val gly = read[Gly](str)

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