package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
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

  def toolbtni_(tag: TagMod, name: String, action: => IO[Unit], disabled: Boolean = false) =
    <.button(c"btn btn-default btn-sm text-center")(
      tag, <.br, name,
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ()),
      disabled ?= (^.disabled := true)
    )

  def toolbtni(icon: String, name: String, action: => IO[Unit], disabled: Boolean = false) =
    <.button(c"btn btn-default btn-sm text-center")(
      <.i(c"fa fa-lg fa-$icon"), <.br, name,
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ()),
      disabled ?= (^.disabled := true)
    )

  def tooltogglei[A](icon: String, name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    toolbtni(icon, name, rv.modL(lens)(!_), disabled)(lens.get(rv.value) ?= c"active")
  def tooltogglei_[A](icon: TagMod, name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    toolbtni_(icon, name, rv.modL(lens)(!_), disabled)(lens.get(rv.value) ?= c"active")

  def navcheckbox(name: String, checked: Boolean, action: => IO[Unit], disabled: Boolean): ReactTag =
    div"form-group"(
      <.label(c"checkbox-inline")(
        <.input(
          ^.checked := checked,
          ^.`type` := "checkbox",
          ^.onChange ~~> action,
          disabled ?= (^.disabled := true)
        ),
        name
      )
    )

  def navcheckbox[A](name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    navcheckbox(name, lens.get(rv.value), rv.modL(lens)(!_), disabled)

  def toolnumber(name: String, value: Double, action: Double => IO[Unit], disabled: Boolean, dropdown: Boolean): ReactTag =
    div"form-group"(
      dropdown ?= Seq(^.marginLeft := 5.px, ^.marginRight := 5.px, ^.marginBottom := 0),
      <.label(name, ^.paddingRight := 5.px),
      <.input(
        c"form-control",
        ^.defaultValue := value,
        ^.`type` := "number",
        ^.onChange ~~> { (e: ReactEventI) =>
          Try(e.target.value.toDouble).toOption.filter(_ != value).map(action).getOrElse(IO.ioUnit)
        },
        !dropdown ?= (^.width := 80.px),
        disabled ?= (^.disabled := true)
      )
    )

  def toolnumber[A](name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Double], disabled: Boolean = false, dropdown: Boolean = false): ReactTag =
    toolnumber(name, lens.get(rv.value), rv.setL(lens), disabled, dropdown)

  class Backend(val $: BackendScope[ReusableVar[AppState], String]) {
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
    .backend(new Backend(_))
    .render { $ =>
      val appState = $.props
      val s = appState.value
      val emptySelection = s.selection match {
        case (rs, as) => rs.isEmpty && as.isEmpty
      }
      val btnAnnotation = toolbtni(
        "edit", "Add Annotation",
        appState.modL(AppState.mode) {
          case Mode.PlaceAnnotation => Mode.Selection
          case _ => Mode.PlaceAnnotation
        }
      )(s.mode == Mode.PlaceAnnotation ?= c"active")

      div"row"(
        div"col-xs-12"(
          div"btn-toolbar"(^.marginBottom := 20.px)(
            div"btn-group"(
              toolbtni("file", "Clear All", appState.setL(AppStateL.graphL)(RGraph()), s.graph.isEmpty)
            ),
            div"btn-group"(
              toolbtni("cut", "Cut", appState.mod(GlycanoApp.cutS.exec), emptySelection),
              toolbtni("copy", "Copy", appState.mod(GlycanoApp.copyS.exec), emptySelection),
              toolbtni("paste", "Paste", appState.mod(GlycanoApp.pasteS.exec), s.buffer.isEmpty),
              toolbtni("trash", "Delete", appState.mod(GlycanoApp.deleteS.exec), emptySelection)
            ),
            div"btn-group"(
              toolbtni("undo", "Undo", appState.mod(GlycanoApp.undo), s.undoPosition + 1 >= s.history.length),
              toolbtni("repeat", "Redo", appState.mod(GlycanoApp.redo), s.undoPosition == 0)
            ),
            Dropdown.Toggle(
              (Bootstrap.Sm, btnAnnotation),
              toolnumber("Font Size:", appState, AppState.annotationFontSize, dropdown = true)
            ),
            div"btn-group"(
              tooltogglei_(<.b("\u03B1|\u03B2"), "Bond Labels", appState, AppState.bondLabels)
            ),
            Dropdown.Toggle(
              (Bootstrap.Sm, Seq(
                tooltogglei("magnet", "Snap To Grid", appState, AppState.snapToGrid),
                tooltogglei("th", "Show Grid", appState, AppState.showGrid)
              )),
              toolnumber("Grid Spacing:", appState, AppState.gridWidth, dropdown = true)
            ),
            Dropdown.Toggle(
              (Bootstrap.Sm, tooltogglei_(Seq(<.i(c"fa fa-lg fa-magnet"), <.b("º")), "Snap Rotation", appState, AppState.snapRotation)),
              toolnumber("Snap To (º):", appState, AppState.snapRotationDegrees, dropdown = true)
            )
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
