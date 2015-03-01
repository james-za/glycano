package za.jwatson.glycanoweb

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => macros1}
import scala.reflect.macros.whitebox

package object macros {
  class MacrosImpl(val c: whitebox.Context) {
    import c.universe._

    type CaseClassDefs = (Modifiers, TypeName, List[Tree], Modifiers, List[List[ValDef]], List[Tree], List[Tree], Tree, List[Tree])
    def extractCaseClassDef(trees: Seq[Tree]): Option[CaseClassDefs] = for {
      q"""
        ${mods: Modifiers} class ${tpeName: TypeName}[..${tpeParams: List[Tree]}] ${ctorMods: Modifiers}(...${ctorParamLists: List[List[ValDef]]})
        extends { ..${earlyDefns: List[Tree]} } with ..${parents: List[Tree]} { ${self: Tree} =>
          ..${body: List[Tree]}
        }
      """ <- trees.headOption if mods.hasFlag(Flag.CASE)
    } yield (mods, tpeName, tpeParams, ctorMods, ctorParamLists, earlyDefns, parents, self, body)

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
        (mods, tpeName, tpeParams, ctorMods, ctorParamLists, earlyDefns, parents, self, body) <- extractCaseClassDef(annottees) \/>
          (annottees.headOption.map(_.pos), "must annotate a case class definition")
        _ <- parents.collectFirst {
          case tq"AltEq[${tpeArgParent: TypeName}]" if tpeArgParent == tpeName => ()
          case q"${tq"AltEq[${tpeArgParent: TypeName}]"}(...$_)" if tpeArgParent == tpeName => ()
        } \/> (parents.headOption.map(_.pos) -> s"$tpeName must extend AltEq[$tpeName]")
        paramList <- ctorParamLists.headOption \/> (None, "case class has no constructor")
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
        val tpt = tq"$tpeName[..$tpeParams]"
        val method = q"override def altEq[..$tpeParams](other: $tpt): Boolean = $inequality"

        val result = q"""
          $mods class $tpeName[..$tpeParams] $ctorMods(...$ctorParamLists)
          extends { ..$earlyDefns } with ..$parents { $self =>
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
