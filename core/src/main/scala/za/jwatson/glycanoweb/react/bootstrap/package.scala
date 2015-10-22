package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.Element
import za.jwatson.glycanoweb.react.bootstrap.{Bootstrap => bs}

import scala.annotation.tailrec
import scalaz.effect.IO

package object bootstrap {

  object NavbarHeader {
    def apply(navbarId: String, children: ReactNode*) = component(navbarId, children)
    val component = ReactComponentB[String]("NavbarHeader")
      .renderPC { ($, navbarId, C) =>
        div"navbar-header"(
          <.button(
            ^.`type` := "button",
            c"navbar-toggle collapsed",
            "data-toggle".reactAttr := "collapse",
            "data-target".reactAttr := s"#$navbarId"
          )(
            <.span(c"sr-only", "Toggle Navigation"),
            <.span(c"icon-bar"),
            <.span(c"icon-bar"),
            <.span(c"icon-bar")
          ),
          <.a(c"navbar-brand", ^.href := "#")(C)
        )
      }
      .domType[dom.html.Div]
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object FormInput {
    case class Props(`type`: String, onChange: ReactEventI => Callback)

    implicit val reuseProps: Reusability[Props] = Reusability.by((_: Props).`type`)

    def apply(props: Props, children: ReactNode*) = component(props, children)
    val component = ReactComponentB[Props]("Input")
      .renderPC { ($, P, C) =>
        <.input(
          ^.`type` := P.`type`,
          c"form-control",
          ^.onChange ==> P.onChange
        )(C)
      }
      .domType[dom.html.Input]
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object Button {
    case class Props(onClick: Callback,
                     style: bs.Style = bs.Default,
                     size: bs.Size = bs.Md,
                     block: Boolean = false,
                     nav: Boolean = false,
                     pressed: Boolean = false)

    implicit val reuseBSStyle: Reusability[bs.Style] = Reusability.by_==
    implicit val reuseBSSize: Reusability[bs.Size] = Reusability.by_==
    implicit val reuseProps: Reusability[Props] = Reusability.by((p: Props) => (p.style, p.size, p.block, p.nav, p.pressed))

    def apply(props: Props, children: ReactNode*) = component(props, children)
    def withKey(key: scalajs.js.Any) = component.withKey(key)
    val component = ReactComponentB[Props]("Button")
      .initialState(false)
      .noBackend
      .renderPCS(($, P, C, S) => {
        val block = if (P.block) " btn-block" else ""
        val nav = if (P.nav) " navbar-btn" else ""
        <.button(
          ^.cls := s"btn btn-${P.style.name} btn-${P.size.name}$block$nav",
          ^.onClick --> P.onClick
        )(C)
      })
      .domType[dom.html.Button]
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object GlyphIcon {
    def apply(icon: String, children: ReactNode*) = component(icon, children)
    val component = ReactComponentB[String]("GlyphIcon")
      .renderPC { ($, props, children) =>
        <.span(c"glyphicon glyphicon-" + props)(children)
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object RadioGroupMap {
    val defaultName: Any ~=> String = ReusableFn(_.toString)
    case class Props[A](selected: ReusableVar[Option[A]], choices: Seq[A], name: A ~=> String = defaultName, toggle: Boolean = false)

    class Backend[A]($: BackendScope[Props[A], Unit]) {
      def handleClick(a: A): Callback = for {
        props <- $.props
        _ <- props.selected.mod(s => if (props.toggle && s.contains(a)) None else Some(a))
      } yield ()
    }

    implicit def reuseChoices[A]: Reusability[Seq[A]] = Reusability.by_==
    implicit def reuseProps[A]: Reusability[Props[A]] = Reusability.caseClass[Props[A]]

    def apply[A] = ReactComponentB[Props[A]]("RadioGroupMap")
      .stateless
      .backend(new Backend(_))
      .render { $ =>
        div"btn-group"(
          for (value <- $.props.choices) yield {
            val selected = $.props.selected.value.contains(value)
            <.label($.props.name(value))(
              c"btn btn-default",
              selected ?= c"active",
              ^.onClick --> $.backend.handleClick(value),
              ^.key := value.##
            )
          }
        )
      }
      .domType[dom.html.Div]
      .configure(Reusability.shouldComponentUpdate[Props[A], Unit, Backend[A], dom.html.Div])
      .build
  }

  object Dropdown {
    implicit val reuseReactTag: Reusability[TagMod] = Reusability.byRef
    val Toggle = ReactComponentB[(Bootstrap.Size, TagMod)]("Dropdown.Toggle")
      .initialState(false)
      .backend(_ => new OnUnmount.Backend)
      .render { $ =>
        val (size, button) = $.props
        div"btn-group"(
          ^.display.flex,
          $.state ?= c"open"
        )(
          button,
          <.button(
            c"btn btn-default btn-$size dropdown-toggle",
            ^.ref := "toggle", ^.tpe := "button",
            ^.onClick --> $.modState(!_)
          )(<.span(c"caret")),
          <.ul(c"dropdown-menu", ^.ref := "dropmenu")($.propsChildren)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .configure(EventListener[dom.Event].install(
        "click",
        $ => e => {
          val node = e.target.asInstanceOf[dom.Node]
          for {
            toggleElement <- CallbackOption.liftOptionLike($.refs[dom.Element]("toggle"))
            if !hasParent(node, toggleElement.getDOMNode())
            dropMenu <- CallbackOption.liftOptionLike($.refs[dom.Element]("dropmenu"))
            if !hasParent(node, dropMenu.getDOMNode())
            _ <- $.setState(false)
          } yield ()
        },
        _ => dom.document.body
      ))
      .build
    val Hover = ReactComponentB[(Bootstrap.Size, TagMod)]("Dropdown.Hover")
      .initialState(false)
      .backend(_ => new OnUnmount.Backend)
      .render { $ =>
        val (size, button) = $.props
        div"btn-group"(
          ^.display.flex,
          $.state ?= c"open"
        )(
          button,
          <.button(
            ^.tpe := "button",
            c"btn btn-default btn-$size dropdown-toggle",
            ^.onMouseOver --> $.setState(true),
            ^.onMouseOut --> $.setState(false),
            <.span(c"caret")
          ),
          <.ul(c"dropdown-menu")($.propsChildren)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
