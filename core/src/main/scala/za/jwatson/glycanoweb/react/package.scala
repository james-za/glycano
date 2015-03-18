package za.jwatson.glycanoweb

import japgolly.scalajs.react.extra.ExternalVar

import scalaz.Equal

package object react {
  implicit def extenalVarEqual[T : Equal]: Equal[ExternalVar[T]] = Equal.equalBy(_.value)
}
