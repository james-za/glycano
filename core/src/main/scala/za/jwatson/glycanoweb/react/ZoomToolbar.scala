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

  case class Props(rvView: ReusableVar[View], bounds: Option[Bounds])
  implicit val reuseProps = Reusability.caseClass[Props]

  class Backend($: BackendScope[Props, Unit]) {
    def clickCenter(e: ReactMouseEvent) = for {
      _ <- e.preventDefaultCB
      p <- $.props
      b <- CallbackOption.liftOption(p.bounds)
      _ <- p.rvView.mod(_.fitBounds(b))
    } yield ()
    val clickReset = (e: ReactMouseEvent) => e.preventDefaultCB >> $.props.flatMap(p => p.rvView.setL(View.scale)(1.0))
    val clickZoomIn = (e: ReactMouseEvent) => e.preventDefaultCB >> $.props.flatMap(p => p.rvView.modL(View.scale)(_ / 1.1))
    val clickZoomOut = (e: ReactMouseEvent) => e.preventDefaultCB >> $.props.flatMap(p => p.rvView.modL(View.scale)(_ * 1.1))
    def changeZoomSlider(e: ReactEventI): Callback = for {
      value <- CallbackOption.liftOptionLike(Try(e.target.value.toDouble))
      _ <- e.preventDefaultCB
      p <- $.props
      _ <- p.rvView.setL(View.scale)(value / 100)
    } yield ()

    def render(props: Props) = {
      val view = props.rvView.value
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
            div"form-group"(<.input(
              c"form-control",
              ^.`type` := "range",
              "min".reactAttr := 0.01,
              "max".reactAttr := 200.0,
              ^.step := 0.01,
              ^.value := view.scale * 100,
              ^.onChange ==> changeZoomSlider
            )),
            <.button(c"btn btn-sm", <.i(c"fa fa-search-plus"), ^.onClick ==> clickZoomOut)
          )) yield element(^.margin := "0 5px")
        )
      )
    }
  }

  val C = ReactComponentB[Props]("ZoomToolbar")
    .stateless
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
