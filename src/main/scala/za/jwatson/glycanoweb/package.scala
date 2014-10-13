package za.jwatson

import org.scalajs.jquery.JQuery

import scala.scalajs.js

package object glycanoweb {
  trait RichJQuery extends org.scalajs.jquery.JQuery {
    def button(state: js.String = ???): JQuery = ???
    def tooltip(options: js.Dynamic = ???): JQuery = ???
    def tooltipster(options: js.Dynamic): JQuery = ???
  }

  import scala.language.implicitConversions
  implicit def richJQuery(jq: JQuery): RichJQuery = jq.asInstanceOf[RichJQuery]
}
