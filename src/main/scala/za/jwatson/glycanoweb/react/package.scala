package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.Attr
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.MonocleReact._

import monocle.std._
import monocle.macros.{Lenses, Lenser}
import org.scalajs.dom

import org.scalajs.dom.HTMLButtonElement

import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph._
import za.jwatson.glycanoweb.structure._

import za.jwatson.glycanoweb.{BootstrapScalatags => bs, GlyAnnot}

import scala.scalajs.js.annotation.JSName




  object GlyphIcon {
    def apply(icon: String, children: ReactNode*) = component(icon, children)
    val component = ReactComponentB[String]("GlyphIcon")
      .render((P, C) => {
        <.span(^.cls := "glyphicon glyphicon-" + P)(C)
      })
      .shouldComponentUpdate((T, P, S) => T.props != P)
      .build
  }

  object RadioGroupMap {
    case class Props[A](onChange: Option[A] => Unit, choices: Map[A, String], selected: A, toggle: Boolean = false)

    class Backend[A](t: BackendScope[Props[A], Unit]) {
      def handleClick(a: A): Unit =
        t.props.onChange(if (t.props.toggle && t.props.selected == a) None else Some(a))
    }

    def apply[A] = ReactComponentB[Props[A]]("RadioGroupMap")
      .stateless
      .backend(new Backend(_))
      .render((P, C, S, B) => {
        <.div(^.cls := "btn-group", Attr("data-toggle") := "buttons")(
          for ((value, label) <- P.choices) yield <.button(label)(
            ^.cls := (if (P.selected == value) "btn btn-default active" else "btn btn-default"),
            ^.onClick --> B.handleClick(value),
            ^.key := value.##
          )
        )(C)
      })
      .domType[dom.HTMLDivElement]
      .build
  }

  object RadioGroup {
    case class Props(onChange: Option[String] => Unit, choices: Seq[String], selected: String, toggle: Boolean = false)

    class Backend(t: BackendScope[Props, Unit]) {
      def handleClick(a: String): Unit =
        t.props.onChange(if (t.props.toggle && t.props.selected == a) None else Some(a))
    }

    def apply() = component
    val component = ReactComponentB[Props]("RadioGroup")
      .stateless
      .backend(new Backend(_))
      .render((P, C, S, B) => {
        <.div(^.cls := "btn-group", Attr("data-toggle") := "buttons")(
          for (value <- P.choices) yield <.button(value)(
            ^.cls := (if (P.selected == value) "btn btn-default active" else "btn btn-default"),
            ^.onClick --> B.handleClick(value)
          )
        )(C)
      })
      .shouldComponentUpdate((T, P, S) => T.props.choices != P.choices || T.props.selected != P.selected || T.props.toggle != P.toggle)
      .domType[dom.HTMLDivElement]
      .build
  }

  object Button {
    case class Props(onClick: () => Unit,
                     style: bs.Style = bs.Default,
                     size: bs.Size = bs.Md,
                     block: Boolean = false,
                     nav: Boolean = false,
                     pressed: Boolean = false)

    def apply(props: Props, children: ReactNode*) = component(props, children)
    def withKey(key: scalajs.js.Any) = component.withKey(key)
    val component = ReactComponentB[Props]("Button")
      .initialState(false)
      .noBackend
      .render((P, C, S, B) => {
        val block = if (P.block) " btn-block" else ""
        val nav = if (P.nav) " navbar-btn" else ""
        <.button(
          ^.cls := s"btn btn-${P.style.name} btn-${P.size.name}$block$nav",
          ^.onClick --> P.onClick()
        )(C)
      })
      .shouldComponentUpdate((T, P, S) => {
        T.props.style != P.style ||
        T.props.size != P.size ||
        T.props.block != P.block ||
        T.props.nav != P.nav ||
        T.props.pressed != P.pressed
      })
      .domType[dom.HTMLButtonElement]
      .build
  }
//
//  object Checkbox {
//    val component = ReactComponentB[Unit]("Checkbox")
//      .initialState(false)
//      .
//  }





//  val ConventionGroup = RadioGroupMap[DisplayConv](RadioGroupMap.Props(_ => (), DisplayConv.conventions().toMap.map(_.swap)))
//  val AnomerGroup = RadioGroupMap[Anomer](RadioGroupMap.Props(_ => (), Anomer.AnomerMap.map(_.swap)))
//  val AbsoluteGroup = RadioGroupMap[Absolute](RadioGroupMap.Props(_ => (), Absolute.AbsoluteMap.map(_.swap)))



