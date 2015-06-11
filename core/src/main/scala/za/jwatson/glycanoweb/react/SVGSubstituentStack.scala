package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import za.jwatson.glycanoweb.render.SubstituentShape
import za.jwatson.glycanoweb.structure.SubstituentType

import scala.scalajs.js

object SVGSubstituentStack {
  def apply(props: Vector[SubstituentType], children: ReactNode*) = component(props, children)
  def withKey(key: js.Any) = component.withKey(key)
  implicit val reuseProps: Reusability[Vector[SubstituentType]] = Reusability.by_==
  val component = ReactComponentB[Vector[SubstituentType]]("SVGBond")
    .render { props =>
      val (shapes, sizes) = props.map(SubstituentShape.apply).unzip
      val (widths, heights) = sizes.unzip
      val stack = for {
        h0 <- heights.headOption.toSeq
        ys = heights.scanLeft(-h0 / 2.0)(_ + _)
        xs = widths.map(-_ / 2.0)
        (shape, (x, y)) <- shapes zip (xs zip ys)
      } yield {
        shape(^.svg.transform := s"translate($x, $y)")
      }
      <.svg.g(stack)
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
