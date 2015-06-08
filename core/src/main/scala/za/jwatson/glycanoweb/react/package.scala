package za.jwatson.glycanoweb

import japgolly.scalajs.react.ReactComponentC.{ReqProps, DefaultProps, ConstProps}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability

import scala.scalajs.js
import scala.scalajs.js.UndefOr

package object react {
  abstract class Component[P, S, +B, +N <: TopNode](val component: ReqProps[P, S, B, N]) {
    def set(key: UndefOr[js.Any], ref: UndefOr[String]): ReqProps[P, S, B, N] = component.set(key, ref)
    def apply(props: P, children: ReactNode*): ReactComponentU[P, S, B, N] = component.apply(props, children)
    def withProps(p: => P): ConstProps[P, S, B, N] = component.withProps(p)
    def withDefaultProps(p: => P): DefaultProps[P, S, B, N] = component.withDefaultProps(p)
    def withKey(k: js.Any): ReqProps[P, S, B, N] = component.withKey(k)
    def withRef(r: String): ReqProps[P, S, B, N] = component.withRef(r)
  }

  //implicit val reusabilityFloat: Reusability[Float] = Reusability.by_==[Float]
  //implicit val reusabilityDouble: Reusability[Double] = Reusability.by_==[Double]
}
