package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.{Reusability, ReusableVar}
import monocle.Iso
import org.scalajs.dom
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

  def navrange(range: NumericRange[Double], value: Double, action: Double => IO[Unit], disabled: Boolean): ReactTag =
    div"form-group"(<.input(
      c"form-control",
      ^.`type` := "range",
      "min".reactAttr := range.start,
      "max".reactAttr := range.end,
      ^.step := range.step,
      ^.value := value,
      ^.onChange ~~> ((e: ReactEventI) => action(Try(e.target.value.toDouble).getOrElse(value))),
      disabled ?= (^.disabled := true)
    ))

  def navrange[A](range: NumericRange[Double], rv: ReusableVar[A], lens: monocle.Lens[A, Double], disabled: Boolean = false): ReactTag =
    navrange(range, lens.get(rv.value), rv.setL(lens), disabled)

  case class Props(view: ReusableVar[View], bounds: Option[Bounds])

  implicit val reuseProps = Reusability.caseclass2(Props.unapply)

  class Backend($: BackendScope[Props, Unit]) {
    def clickCenter = preventingDefaultIO($.props.view.mod { v =>
      $.props.bounds.fold(v) {
        case Bounds(x, y, width, height) =>
          val sx = v.width / width
          val sy = v.height / height
          val scale = math.min(sx, sy)
          View(x + width / 2, y + height / 2, scale * 0.975, v.width, v.height)
      }
    })

    def zoomChange(e: ReactEventI) = {
      val scale = Try(e.target.value.toDouble * 0.01)
      scale.toOption.fold(IO.ioUnit)($.props.view.setL(View.scale))
    }
  }

  val C = ReactComponentB[Props]("ZoomToolbar")
    .stateless
    .backend(new Backend(_))
    .render { $ =>
      <.form(c"form-inline text-right")(
        div"form-group"(
          for (element <- Seq(
            <.button(c"btn btn-sm", "Fit everything in view", ^.onClick ~~> $.backend.clickCenter),
            <.button(c"btn btn-sm", "Reset to 100%", ^.onClick ~~> preventingDefaultIO($.props.view.setL(View.scale)(1.0))),
            <.input(
              c"form-control",
              ^.value := f"${$.props.view.value.scale * 100.0}%.2f %%",
              ^.`type` := "text",
              ^.readOnly := true,
              ^.width := 100.px
            ),
            <.button(c"btn btn-sm", <.i(c"fa fa-search-minus"), ^.onClick ~~> preventingDefaultIO($.props.view.modL(View.scale)(_ / 1.1))),
            navrange(0.01 to 200.0 by 0.01, $.props.view, View.scale ^<-> multIso(100)),
            <.button(c"btn btn-sm", <.i(c"fa fa-search-plus"), ^.onClick ~~> preventingDefaultIO($.props.view.modL(View.scale)(_ * 1.1)))
          )) yield element(^.margin := "0 5px")
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
