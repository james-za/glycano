package za.jwatson.glycanoweb

import shapeless.HNil

import scalatags.JsDom._
import scalatags.JsDom.all._
import org.scalajs.dom._


object BootstrapScalatags {
  //implicit class RichString(s: String) { def c = new Cls(s) }
  
  val container = div(cls:="container")
  val containerFluid = div(cls:="container-fluid")

  val row = div(cls:="row")

  val colSizes = Seq("lg", "md", "sm", "xs")
  def col(lg: Int = -1, md: Int = -1, sm: Int = -1, xs: Int = -1,
           lgOffset: Int = -1, mdOffset: Int = -1,
           smOffset: Int = -1, xsOffset: Int = -1) = {
    val sizes = colSizes zip Seq(lg, md, sm, xs)
    val offsets = colSizes zip Seq(lgOffset, mdOffset, smOffset, xsOffset)
    val sizeClasses = for((size, span) <- sizes if span > 0 && span <= 12) yield s"col-$size-$span"
    val offsetClasses = for((size, span) <- offsets if span > 0 && span <= 12) yield s"col-$size-offset-$span"
    val classes = sizeClasses ++ offsetClasses mkString " "
    div(cls:=classes)
  }

  case class Style(name: String)
  object Default extends Style("default")
  object Primary extends Style("primary")
  object Success extends Style("success")
  object Info extends Style("info")
  object Warning extends Style("warning")
  object Danger extends Style("danger")
  object LinkStyle extends Style("link")

  case class Size(name: String)
  object Lg extends Size("lg")
  object Md extends Size("md")
  object Sm extends Size("sm")
  object Xs extends Size("xs")

  def navbar(style: Style = Default): TypedTag[HTMLElement] = tags2.nav(cls:=s"navbar navbar-${style.name}", "role".attr:="navigation")

  def panel(style: Style = Default): TypedTag[HTMLDivElement] = div(cls:=s"panel panel-${style.name}")
  def panelHeading: TypedTag[HTMLDivElement] = div(cls:="panel-heading")
  def panelBody(classes: String = ""): TypedTag[HTMLDivElement] = div(cls:=s"panel-body $classes")
  def panelFooter: TypedTag[HTMLDivElement] = div(cls:="panel-footer")

  def btn(style: Style = Default, size: Size = Md, block: Boolean = false): TypedTag[HTMLButtonElement] =
    button(cls:=s"btn btn-${style.name} btn-${size.name}${if(block) " btn-block" else ""}")
  def navBtn(style: Style = Default, size: Size = Md, block: Boolean = false): TypedTag[HTMLButtonElement] =
    button(cls:=s"btn btn-${style.name} btn-${size.name}${if(block) " btn-block" else ""} navbar-btn")
  //def btn(style: Style = Default): TypedTag[HTMLButtonElement] = button(cls:=s"btn btn-${style.name}")

  def radioButton(style: Style = Default, inputName: String, classes: String = "", innerMods: Seq[Modifier] = Seq.empty): TypedTag[HTMLLabelElement] =
    label(cls:=s"btn btn-${style.name} $classes")(
      input(`type`:="radio", "name".attr:=inputName, innerMods)
    )

  def checkboxButton(style: Style = Default, inputName: String, classes: String = "", innerMods: Seq[Modifier] = Seq.empty): TypedTag[HTMLLabelElement] =
    label(cls:=s"btn btn-${style.name} $classes")(
      input(`type`:="checkbox", "name".attr:=inputName, innerMods)
    )

  val btnGroup = div(cls:="btn-group", "data-toggle".attr:="buttons")

  val btnToolbar = div(cls:="btn-toolbar", "role".attr:="toolbar")
  def glyphIcon(icon: String): TypedTag[HTMLSpanElement] = span(cls:=s"glyphicon glyphicon-$icon")

  def modal(modalId: String, header: Option[Frag] = None, body: Option[Frag] = None, footer: Option[Frag] = None): TypedTag[HTMLDivElement] = {
    val titleId = modalId + "Title"
    val modalContent =
      header.map(modalHeader(titleId, _)) ++
      body.map(modalBody) ++
      footer.map(modalFooter)
    div(
      cls:="modal fade", id:=modalId, tabindex:=(-1), "role".attr:="dialog",
      "aria-labelledby".attr:=titleId, "aria-hidden".attr:="true"
    )(
      div(cls:="modal-dialog")(div(cls:="modal-content")(modalContent.toSeq))
    )
  }
  def modalHeader(titleId: String, headerTag: Frag): TypedTag[HTMLDivElement] = {
    div(cls := "modal-header")(
      button(tpe := "button", cls := "close", "data-dismiss".attr := "modal")(
        span("aria-hidden".attr := "true")("×"), span(cls := "sr-only")("Close")),
      h4(cls := "modal-title", id := titleId)(headerTag)
    )
  }
  def modalBody(bodyTag: Frag): TypedTag[HTMLDivElement] = div(cls:="modal-body")(bodyTag)
  def modalFooter(footerTag: Frag): TypedTag[HTMLDivElement] = div(cls:="modal-footer")(footerTag)

  val formGroup = div(cls:="form-group")
}
