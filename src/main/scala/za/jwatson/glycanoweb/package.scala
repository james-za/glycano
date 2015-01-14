package za.jwatson

import org.scalajs.jquery.JQuery

import scala.scalajs.js

package object glycanoweb {
  trait RichJQuery extends org.scalajs.jquery.JQuery {
    def button(state: String = js.native): JQuery = js.native
    def tooltip(options: js.Dynamic = js.native): JQuery = js.native
    def tooltipster(options: js.Dynamic): JQuery = js.native
  }

  import scala.language.implicitConversions
  implicit def richJQuery(jq: JQuery): RichJQuery = jq.asInstanceOf[RichJQuery]
}
