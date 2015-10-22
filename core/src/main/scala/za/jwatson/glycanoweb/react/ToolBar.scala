package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Iso
import za.jwatson.glycanoweb.react.GlycanoApp.{AppState, AppStateL, Mode}
import za.jwatson.glycanoweb.react.bootstrap.{Dropdown, Bootstrap}
import za.jwatson.glycanoweb.structure.{AnnotId, ResidueId, RGraph}

import scala.util.{Success, Failure, Try}
import scalaz.effect.IO

object ToolBar {
  def clampIso(min: Double, max: Double): Iso[Double, Double] =
    Iso[Double, Double](x => x)(x => math.max(min, math.min(max, x)))
  def multIso(mult: Double): Iso[Double, Double] =
    Iso[Double, Double](_ * mult)(_ / mult)

  def toolbtni_(tag: TagMod, name: String, action: => Callback, disabled: Boolean = false) =
    <.button(c"btn btn-default btn-sm text-center")(
      tag, <.br, name,
      ^.onClick ==> (preventDefault(_: ReactMouseEvent) >> action),
      disabled ?= (^.disabled := true)
    )

  def toolbtni(icon: String, name: String, action: => Callback, disabled: Boolean = false) =
    <.button(c"btn btn-default btn-sm text-center")(
      <.i(c"fa fa-lg fa-$icon"), <.br, name,
      ^.onClick ==> (preventDefault(_: ReactMouseEvent) >> action),
      disabled ?= (^.disabled := true)
    )

  def tooltogglei[A](icon: String, name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    toolbtni(icon, name, rv.modL(lens)(!_), disabled)(lens.get(rv.value) ?= c"active")
  def tooltogglei_[A](icon: TagMod, name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    toolbtni_(icon, name, rv.modL(lens)(!_), disabled)(lens.get(rv.value) ?= c"active")

  def navcheckbox(name: String, checked: Boolean, action: => Callback, disabled: Boolean): ReactTag =
    div"form-group"(
      <.label(c"checkbox-inline")(
        <.input(
          ^.checked := checked,
          ^.`type` := "checkbox",
          ^.onChange --> action,
          disabled ?= (^.disabled := true)
        ),
        name
      )
    )

  def navcheckbox[A](name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    navcheckbox(name, lens.get(rv.value), rv.modL(lens)(!_), disabled)

  def toolnumber(name: String, value: Double, action: Double => Callback, disabled: Boolean, dropdown: Boolean): ReactTag =
    div"form-group"(
      dropdown ?= Seq(^.marginLeft := 5.px, ^.marginRight := 5.px, ^.marginBottom := 0),
      <.label(name, ^.paddingRight := 5.px),
      <.input(
        c"form-control",
        ^.defaultValue := value,
        ^.`type` := "number",
        ^.onChange ==> { (e: ReactEventI) =>
          CallbackOption.liftOption(Try(e.target.value.toDouble).toOption.filter(_ != value)).flatMap(action(_).toCBO)
        },
        !dropdown ?= (^.width := 80.px),
        disabled ?= (^.disabled := true)
      )
    )

  def toolnumber[A](name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Double], disabled: Boolean = false, dropdown: Boolean = false): ReactTag =
    toolnumber(name, lens.get(rv.value), rv.setL(lens), disabled, dropdown)

  class Backend(val $: BackendScope[ReusableVar[AppState], String]) {
    def render(rvAppState: ReusableVar[AppState], state: String) = {
      val appState = rvAppState.value
      val emptySelection = appState.selection match {
        case (rs, as) => rs.isEmpty && as.isEmpty
      }
      val btnAnnotation = toolbtni(
        "edit", "Add Annotation",
        rvAppState.modL(AppState.mode) {
          case Mode.PlaceAnnotation => Mode.Selection
          case _ => Mode.PlaceAnnotation
        }
      )(appState.mode == Mode.PlaceAnnotation ?= c"active")

      div"row"(
        div"col-xs-12"(
          div"btn-toolbar"(^.marginBottom := 20.px)(
            div"btn-group"(
              toolbtni("file", "Clear All", rvAppState.setL(AppStateL.graphL)(RGraph()), appState.graph.isEmpty)
            ),
            div"btn-group"(
              toolbtni("cut", "Cut", rvAppState.mod(_.doCut), emptySelection),
              toolbtni("copy", "Copy", rvAppState.mod(_.doCopy), emptySelection),
              toolbtni("paste", "Paste", rvAppState.mod(_.doPaste), appState.buffer.isEmpty),
              toolbtni("trash", "Delete", rvAppState.mod(_.doDelete), emptySelection)
            ),
            div"btn-group"(
              toolbtni("undo", "Undo", rvAppState.mod(GlycanoApp.undo), appState.undoPosition + 1 >= appState.history.length),
              toolbtni("repeat", "Redo", rvAppState.mod(GlycanoApp.redo), appState.undoPosition == 0)
            ),
            div"btn-group"(
              toolbtni("chain", "Add Bond", rvAppState.setL(AppState.mode)(Mode.CreateBond), appState.graph.isEmpty)
            ),
            div"btn-group"(
              tooltogglei_(<.b("\u03B1|\u03B2"), "Bond Labels", rvAppState, AppState.bondLabels)
            ),
            Dropdown.Toggle(
              (Bootstrap.Sm, btnAnnotation),
              toolnumber("Font Size:", rvAppState, AppState.annotationFontSize, dropdown = true)
            ),
            Dropdown.Toggle(
              (Bootstrap.Sm, Seq(
                tooltogglei("magnet", "Snap To Grid", rvAppState, AppState.snapToGrid),
                tooltogglei("th", "Show Grid", rvAppState, AppState.showGrid)
              )),
              toolnumber("Grid Spacing:", rvAppState, AppState.gridWidth, dropdown = true)
            ),
            Dropdown.Toggle(
              (Bootstrap.Sm, tooltogglei_(Seq(<.i(c"fa fa-lg fa-magnet"), <.b("º")), "Snap Rotation", rvAppState, AppState.snapRotation)),
              toolnumber("Snap To (º):", rvAppState, AppState.snapRotationDegrees, dropdown = true)
            )
          )
        )
      )
    }
  }

  implicit val reuseSelection: Reusability[(Set[ResidueId], Set[AnnotId])] = Reusability.by_==
  implicit val reuseAppState: Reusability[AppState] =
    Reusability.by((a: AppState) => (
      a.annotationFontSize,
      a.bondLabels,
      a.gridWidth,
      a.showGrid,
      a.snapRotation,
      a.snapRotationDegrees,
      a.snapToGrid,
      a.undoPosition,
      a.history.length,
      a.buffer.isEmpty,
      a.mode,
      a.view,
      a.selection
    ))(Reusability.by_==)

  val C = ReactComponentB[ReusableVar[AppState]]("ToolBar")
    .initialState("glycano")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
