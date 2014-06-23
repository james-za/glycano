package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.structure.{Bond, Residue}

import scalaz.Scalaz._
import scalaz._

object CASPER {
  val predecessors: ((Int, Bond)) => Boolean = { case (i: Int, _: Bond) => i > 1 }

  def getStrings(residues: Seq[Residue]): Map[Residue, String] = {
    val roots = residues.filter(_.bonds.get(1) == None)
    val strings = roots map getString
    Map(roots zip strings: _*)
  }

  def getString(root: Residue): String = {
    val tree = residueTree(root)
    tree.cobind({ st =>
      st.loc
      ""
    }).flatten.reverseIterator.mkString

    val loc = tree.loc

    def recurseLoc(loc: TreeLoc[String]): Unit = {
      val left = loc.left ? "]" | ""
      val right = loc.right ? "[" | ""
      loc.modifyLabel(_ + left + right)
      loc.foreach()
      recurseLoc()
    }

    ""
  }

  def residueTree(root: Residue): Tree[String] = {
    val leaves = root.bonds.withFilter(_._1 > 1).map({
      case (i, b) => residueTree(b.from.residue)
    }).toSeq
    root.symbol.node(leaves: _*)
  }
}
