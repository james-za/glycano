package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.{Reusability, ReusableVar}
import monocle.Iso
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.AppState
import za.jwatson.glycanoweb.react.GlycanoCanvas.{Bounds, View}

import scala.collection.immutable.NumericRange
import scala.util.Try
import scalaz.effect.IO
import scalaz.syntax.bind.{^ => _, _}
import scalaz.std.function._

object ZoomToolbar {
  def clampIso(min: Double, max: Double): Iso[Double, Double] =
    Iso[Double, Double](x => x)(x => math.max(min, math.min(max, x)))

  def multIso(mult: Double): Iso[Double, Double] =
    Iso[Double, Double](_ * mult)(_ / mult)

  def navrange(range: NumericRange[Double], value: Double, action: Double => Callback, disabled: Boolean): ReactTag =
    div"form-group"(<.input(
      c"form-control",
      ^.`type` := "range",
      "min".reactAttr := range.start,
      "max".reactAttr := range.end,
      ^.step := range.step,
      ^.value := value,
      ^.onChange ==> ((e: ReactEventI) => action(Try(e.target.value.toDouble).getOrElse(value))),
      disabled ?= (^.disabled := true)
    ))

  def navrange[A](range: NumericRange[Double], rv: ReusableVar[A], lens: monocle.Lens[A, Double], disabled: Boolean = false): ReactTag =
    navrange(range, lens.get(rv.value), rv.setL(lens), disabled)

  class Backend($: BackendScope[ReusableVar[AppState], Unit]) {
    def handler(f: ReusableVar[AppState] => Callback) = (e: ReactMouseEvent) => preventDefault(e) >> $.props.flatMap(f)
    val clickCenter = handler(props => props.modL(AppState.view)(v => props.value.bounds.fold(v)(v.fitBounds)))
    val clickReset = handler(_.setL(AppState.view ^|-> View.scale)(1.0))
    val clickZoomIn = handler(_.modL(AppState.view ^|-> View.scale)(_ / 1.1))
    val clickZoomOut = handler(_.modL(AppState.view ^|-> View.scale)(_ * 1.1))

    def zoomChange(e: ReactEventI): Callback = for {
      scale <- CallbackOption.liftOptionLike(Try(e.target.value.toDouble * 0.01))
      rvAppState <- $.props
      _ <- rvAppState.setL(AppState.view ^|-> View.scale)(scale)
    } yield ()

    def render(rvAppState: ReusableVar[AppState]) = {
      val view = rvAppState.value.view
      <.form(c"form-inline text-right")(
        div"form-group"(
          for (element <- Seq(
            <.button(c"btn btn-sm", "Fit everything in view", ^.onClick ==> clickCenter),
            <.button(c"btn btn-sm", "Reset to 100%", ^.onClick ==> clickReset),
            <.input(
              c"form-control",
              ^.value := f"${view.scale * 100.0}%.2f %%",
              ^.`type` := "text",
              ^.readOnly := true,
              ^.width := 100.px
            ),
            <.button(c"btn btn-sm", <.i(c"fa fa-search-minus"), ^.onClick ==> clickZoomIn),
            navrange(0.01 to 200.0 by 0.01, rvAppState, AppState.view ^|-> View.scale ^<-> multIso(100)),
            <.button(c"btn btn-sm", <.i(c"fa fa-search-plus"), ^.onClick ==> clickZoomOut)
          )) yield element(^.margin := "0 5px")
        )
      )
    }
  }

  val reuseAppState = Reusability.by((s: AppState) => (s.view, s.bounds))
  val C = ReactComponentB[ReusableVar[AppState]]("ZoomToolbar")
    .stateless
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
