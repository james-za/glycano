package za.jwatson.glycanoweb

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => macros1}
import scala.reflect.macros.whitebox

package object macros {
  class MacrosImpl(val c: whitebox.Context) {
    import c.universe._
    //def extractCaseClassDef(trees: Seq[Tree]): Option[(Modifiers, TypeName, List[])]

    def debug1(targs: List[Type], tpname: TypeName): Boolean = {
      val ttargs = targs
      val arg1 = tq"A"
      val ttpname = tpname
      ttargs.headOption.contains(tpname)
    }

    def altEqImpl(annottees: Tree*): Tree = {
      import scalaz.{Tree => _, _}, Scalaz._

      implicit val positionMonoid = Monoid.instance[Position]((_, _) => c.enclosingPosition, c.enclosingPosition)

      val cls: (Option[Position], String) \/ Tree = for {
        q"""
          $mods class ${tpname: TypeName}[..$tparams] $ctorMods(...${paramss: List[List[ValDef]]})
          extends { ..$earlydefns } with ..${parents: List[Tree]} { $self =>
            ..${body: List[c.Tree]}
          }
        """ <- annottees.headOption \/> (annottees.headOption.map(_.pos), "must annotate a case class definition")
        _ = {
          println(parents.mkString("\n"))
        }
        _ <- parents.collectFirst {
          case tq"AltEq[${ttpname: TypeName}]" if ttpname == tpname => ()
          case q"${tq"AltEq[${ttpname: TypeName}]"}(...$argss)" if ttpname == tpname => ()
        } \/> (parents.headOption.map(_.pos) -> s"$tpname must extend AltEq[$tpname]")
        paramList <- paramss.headOption \/> (None, "case class has no constructor")
        paramFirst <- paramList.headOption \/> (None, "empty case class")
        paramRest = paramList.tail
      } yield {
        val checks = for {
          q"${pmods: Modifiers} val $pname: $ptype = $_" <- paramList
          if !pmods.annotations.exists(q"new exclude()".equalsStructure)
        } yield {

          val pannots = pmods.annotations
          q"$pname.!=(other.$pname)"
        }
        val inequality = checks.reduceLeft((a, b) => q"$a.||($b)")
        val tpt = tq"$tpname[..$tparams]"
        val method = q"override def altEq[..$tparams](other: $tpt): Boolean = $inequality"

        val result = q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss)
          extends { ..$earlydefns } with ..$parents { $self =>
            ..${method :: body}
          }
        """

        result
      }

      cls.fold[Tree]({
        case (Some(pos), err) => c.abort(pos, err)
        case (None, err) => c.abort(c.enclosingPosition, err)
      }, tree => tree)
    }
  }


  object AltEq {
    class altEq extends StaticAnnotation {
      def macroTransform(annottees: Any*): Any = macro MacrosImpl.altEqImpl
    }

    class exclude extends StaticAnnotation
  }
  trait AltEq[T] {
    def altEq(other: T): Boolean = ???
  }
}
