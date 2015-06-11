package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.structure.{Annot, AnnotId}

import scalajs.js

import scalaz.effect.IO

object Annotation {
  type Props = (ReactMouseEvent => IO[Unit], AnnotId, Annot, Boolean)
  def apply(props: Props, children: ReactNode*) = component(props, children)
  def withKey(key: js.Any) = component.withKey(key)
  val component = ReactComponentB[Props]("Annotation")
    .initialState[(Double, Double)]((0, 0))
    .render { $ =>
      val (io, id, Annot(text, size, x, y, rot), selected) = $.props
      val (bw, bh) = $.state
      <.svg.g(
        ^.onMouseDown ~~> io,
        <.svg.text(text)(
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
    .componentDidMount { $ =>
      val e = $.getDOMNode().asInstanceOf[dom.svg.G]
      val bb = e.firstElementChild.asInstanceOf[dom.svg.Text].getBBox()
      $.setState((bb.width, bb.height))
    }
    .componentDidUpdate {
      case (scope, props, state) =>
        val annot1 = props._3
        val annot2 = scope.props._3
        if (annot1.text != annot2.text || annot1.size != annot2.size) {
          val e = scope.getDOMNode().asInstanceOf[dom.svg.G]
          val bb = e.firstElementChild.asInstanceOf[dom.svg.Text].getBBox()
          scope.setState((bb.width, bb.height))
        }
    }
    .shouldComponentUpdate((T, P, S) => T.props._2 != P._2 || T.props._3 != P._3 || T.props._4 != P._4 || T.state != S)
    .build
}
