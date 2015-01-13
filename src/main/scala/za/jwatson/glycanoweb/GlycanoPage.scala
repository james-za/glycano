package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.{BootstrapScalatags => bs}
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure._

import org.scalajs.jquery.{jQuery => jQ}

import scalatags.JsDom.all._
import GlycanoWeb._

object GlycanoPage {
  /** Toggles for molecular class */
  val residueCategories =
    bs.btnToolbar(display.`inline-block`)(
      bs.radioGroup[ResidueCategory]("category", ResidueCategory.ResidueCategories, cat => "rc-" + cat.name, _.name)
    )

  /** Toggles for absolute and anomeric configuration */
  val residueConfig =
    bs.btnToolbar(display.`inline-block`)(
      bs.radioGroup[Anomer]("anomer", Anomer.Anomers, ano => "ano-" + ano.symbol, _.desc),
      bs.radioGroup[Absolute]("absolute", Absolute.Absolutes, abs => "abs-" + abs.symbol, _.desc)
    )

  /** Selection of residue type to create */
  val residueTabs =
    ul(cls:="nav nav-tabs", "role".attr:="tablist", id:="res-tabs")(
      for (cat <- ResidueCategory.ResidueCategories) yield
        li(a(href:=("#cat-" + cat.name), "role".attr:="tab", "data-toggle".attr:="tab")(
          cat.name
        ))
    )
  val residuePages =
    bs.btnGroup(id:="aldoses")(
      div(cls:="tab-content")(
        for (cat <- ResidueCategory.ResidueCategories) yield div(cls:="tab-pane", id:=("cat-" + cat.name))(
          for (rt <- ResidueType.ResidueTypeCategories(cat)) yield span(
            bs.checkboxButton(inputName = "rt-" + rt.desc, classes = "residue", innerMods = Seq(display.none))
              (id := "rt-" + rt.desc, title := rt.desc, padding := "2px")
              (div(cls := "svg"))
          )
        )
      )
    )

  /** Actions related to creating residues */
  val residuePanel =
    bs.panel(bs.Default)(
      bs.panelHeading("Residues"),
      bs.panelBody(classes = "text-center")(
        bs.row(bs.col(xs=12)(residueTabs)),
        bs.row(bs.col(xs=12)(residueConfig)),
        bs.row(bs.col(xs=12)(residuePages))
      )
    )

  /** Selection of substituent type to add to residues */
  val substituentPages = bs.btnGroup(id:="substituents")(
    for (st <- SubstituentType.SubstituentTypes) yield span(
      bs.checkboxButton(inputName = "st-" + st.symbol, classes = "substituent", innerMods = Seq(display.none))
        (id := "st-" + st.symbol, title := st.name, padding := "2px")
        (div(cls := "svg"))
    )
  )

  /** Selection of substituent to add to residues */
  val substituentPanel =
    bs.panel(bs.Default)(
      bs.panelHeading("Substituents"),
      bs.panelBody(classes = "text-center")(
        bs.row(bs.col(xs=12)(substituentPages))
      )
    )

  /** Displaying and parsing CASPER text format */
  val casperForm =
  form("role".attr:="form form-inline")(
    div(cls:="form-group input-group", width:="100%")(
      input(id:="casper", `type`:="text", cls:="form-control")/*,
      span(cls:="input-group-btn")(bs.btn()(id:="casper-parse", "Parse"))*/
    )
  )

  /** Main canvas element */
  val stagePanel = bs.panel()(id:="stage-panel")()

  /** Overview of selected residues */
  val overviewPanel =
    bs.panel(bs.Primary)(
      bs.panelHeading(bs.glyphIcon("list-alt")(marginRight:=15.px), span(id:="overview-title")),
      bs.panelBody()(id:="overview-body")
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
    bs.navbar(bs.Default)(bs.containerFluid(
      div(cls:="navbar-header")(
        button(
          `type`:="button",
          cls:="navbar-toggle collapsed",
          "data-toggle".attr:="collapse",
          "data-target".attr:="#glycano-navbar-collapse"
        )(
          span(cls:="sr-only", "Toggle Navigation"),
          span(cls:="icon-bar"),
          span(cls:="icon-bar"),
          span(cls:="icon-bar")
        ),
        a(cls:="navbar-brand", href:="#")("Glycano")
      ),
      div(cls:="collapse navbar-collapse", id:="glycano-navbar-collapse")(
        p(cls:="navbar-text", "Load:"),
        form(cls:="navbar-form navbar-left")(
          bs.formGroup(
            input(tpe:="file", cls:="form-control", id:="upload-file")
          )
        ),
        p(cls:="navbar-text", "Filename:"),
        form(cls:="navbar-form navbar-left")(
          bs.formGroup(
            input(id:="filename", tpe:="text", cls:="form-control", placeholder:="Filename", value:="glycano")
          )
        ),
        ul(cls:="nav navbar-nav")(
          saveDropdown
        ),
        form(cls:="navbar-form navbar-left")(
          bs.formGroup(
            label(cls:="checkbox-inline")(
              input(`type`:="checkbox", id:="bondlabel", value:="bondlabel", onchange:={() =>
                GlycanoWeb.bondLabels() = jQ("#bondlabel").is(":checked")
              }),
              "Bond Labels"
            )
          )
        ),
        " ",
        bs.navBtn()(id:="navbar-clear-btn", "Clear All"), " ",
        bs.navBtn()(id:="navbar-delete-btn", "Delete"), " ",
        bs.navBtn()(id:="navbar-cut-btn", "Cut"), " ",
        bs.navBtn()(id:="navbar-copy-btn", "Copy"), " ",
        bs.navBtn()(id:="navbar-paste-btn", "Paste"), " ",
        bs.navBtn()(id:="navbar-undo-btn", bs.glyphIcon("chevron-left"), " Undo"), " ",
        bs.navBtn()(id:="navbar-redo-btn", bs.glyphIcon("chevron-right"), " Redo"), " ",
        bs.navBtn()(id:="navbar-annotation-btn", bs.glyphIcon("font"), " Add Annotation"), " ",
        bs.navBtn()(id:="navbar-zoom-out-btn", bs.glyphIcon("zoom-out")), " ",
        bs.navBtn()(id:="navbar-zoom-reset-btn", "100%"), " ",
        bs.navBtn()(id:="navbar-zoom-in-btn", bs.glyphIcon("zoom-in")), " "/*,
        conventionEditor.createNavButton*/
      )
    ))



  //    val modeCreate =
  //      col(xs=6)(btn(Primary, Lg, block = true)("Create")(id:="mode-create", onclick:={() =>
  //        if(residueType().isEmpty) residueType() = Some(ResidueType.Glycero)
  //      }))
  val modeSelect =
    bs.col(xs=12)(
      bs.btn(bs.Primary, bs.Lg, block = true)(bs.glyphIcon("hand-up")(marginRight:=15.px), "Selection Mode", id:="mode-select")
    )

  val conventionPanel =
    bs.panel(bs.Default)(
      bs.panelHeading("Convention"),
      bs.panelBody(classes = "text-center")(
        bs.btnGroup(
          bs.radioButton(bs.Default, "conv-uct")("UCT")(onclick := {() => displayConv() = DisplayConv.convUCT}),
          bs.radioButton(bs.Default, "conv-cfg")("CFG")(onclick := {() => displayConv() = DisplayConv.convCFG})
        )
      )
    )

  /** Main container */
  val mainContainer = bs.containerFluid(
    bs.row(glycanoNavbar),
    bs.row(
      bs.col(xs=3)(
        bs.row(bs.col(xs=12)(conventionPanel)),
        bs.row(bs.col(xs=12)(residuePanel)),
        bs.row(bs.col(xs=12)(substituentPanel))
      ),
      bs.col(xs=6)(
        bs.row(bs.col(xs=12)(casperForm)),
        bs.row(bs.col(xs=12)(stagePanel)),
        bs.row(modeSelect)
      ),
      bs.col(xs=3)(overviewPanel)
    )
  )

  def render = {
    mainContainer.render
  }
}
