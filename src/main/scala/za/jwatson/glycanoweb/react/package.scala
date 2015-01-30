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
      .render((P, C) => <.span(^.cls := "glyphicon glyphicon-" + P)(C))
      .build
  }

  object RadioGroupMap {
    case class Props[A](onChange: Option[A] => Unit, choices: Map[A, String], toggle: Boolean = false)

    class Backend[A](t: BackendScope[Props[A], Option[A]]) {
      def handleClick(a: A): Unit =
        if (!t.state.contains[A](a)) t.modState { old =>
          val b = if (t.props.toggle && old.contains[A](a)) None else Some(a)
          t.props.onChange(b)
          b
        }
    }

    def apply[A] = ReactComponentB[Props[A]]("RadioGroupMap")
      .initialStateP[Option[A]](_.choices.keys.headOption)
      .backend(new Backend[A](_))
      .render((P, C, S, B) =>
        <.div(^.cls := "btn-group", Attr("data-toggle") := "buttons")(
          for ((value, label) <- P.choices) yield <.button(label)(
            ^.cls := (if (S.contains[A](value)) "btn btn-default active" else "btn btn-default"),
            ^.onClick --> B.handleClick(value)
          )
        )(C)
      )
      .domType[dom.HTMLDivElement]
      .build
  }

  object RadioGroup {
    case class Props(onChange: Option[String] => Unit, choices: Seq[String], toggle: Boolean = false)

    class Backend(t: BackendScope[Props, Option[String]]) {
      def handleClick(a: String): Unit =
        if (!t.state.contains[String](a)) t.modState { old =>
          val b = if (t.props.toggle && old.contains[String](a)) None else Some(a)
          t.props.onChange(b)
          b
        }
    }

    def apply() = component
    val component = ReactComponentB[Props]("RadioGroup")
      .initialStateP[Option[String]](_.choices.headOption)
      .backend(new Backend(_))
      .render((P, C, S, B) =>
        <.div(^.cls := "btn-group", Attr("data-toggle") := "buttons")(
          for (value <- P.choices) yield <.button(value)(
            ^.cls := (if (S.contains[String](value)) "btn btn-default active" else "btn btn-default"),
            ^.onClick --> B.handleClick(value)
          )
        )(C)
      )
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



