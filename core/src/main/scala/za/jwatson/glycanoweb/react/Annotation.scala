package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.structure.{Annot, AnnotId}

import scalajs.js

import scalaz.effect.IO

object Annotation {
  type Props = (ReactMouseEvent => Callback, AnnotId, Annot, Boolean)
  implicit val reusability: Reusability[Props] = Reusability.by((p: Props) => (p._2, p._3, p._4))
  class Backend($: BackendScope[Props, (Double, Double)]) {
    val updateTextBBox = for {
      text <- CallbackOption.liftOptionLike($.refs[dom.svg.Text]("text"))
      bb = text.getDOMNode().getBBox()
      _ <- $.setState((bb.width, bb.height))
    } yield ()

    def render(props: Props, state: (Double, Double)) = {
      val (callback, id, Annot(text, size, x, y, rot), selected) = props
      val (bw, bh) = state
      <.svg.g(
        ^.onMouseDown ==> callback,
        <.svg.text(text)(
          ^.ref := "text",
          ^.svg.pointerEvents := "none",
          ^.svg.fontSize := size,
          ^.svg.x := x,
          ^.svg.y := y,
          ^.svg.transform := s"rotate($rot)"
        ),
        selected ?= <.svg.rect(
          ^.svg.x := x - 3, ^.svg.y := y - 3 - bh + 4,
          ^.svg.width := bw + 6, ^.svg.height := bh + 6,
          ^.svg.rx := 3, ^.svg.ry := 3,
          ^.svg.fill := "#404080", ^.svg.fillOpacity := "50%",
          ^.svg.stroke := "#404080", ^.svg.strokeWidth := 1
        )
      )
    }
  }

  implicit val reuseState = Reusability.by_==[(Double, Double)]

  val C = ReactComponentB[Props]("Annotation")
    .initialState[(Double, Double)]((0, 0))
    .renderBackend[Backend]
    .componentDidMount(_.backend.updateTextBBox)
    .componentDidUpdate { c =>
      // update bounds if annotation content changed
      val annot1 = c.prevProps._3
      val annot2 = c.$.props._3
      val contentChanged = annot1.text != annot2.text || annot1.size != annot2.size
      c.$.backend.updateTextBBox.filter(_ => contentChanged)
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
