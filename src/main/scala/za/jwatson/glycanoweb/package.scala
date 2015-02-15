package za.jwatson

import scala.scalajs.js

package object glycanoweb {
  implicit class ToDynamic(a: Any) {
    def dynamic[T](f: js.Dynamic => Any): T = f(a.asInstanceOf[js.Dynamic]).asInstanceOf[T]
  }
}
