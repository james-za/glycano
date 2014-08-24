package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.render.GlycanoCanvas
import za.jwatson.glycanoweb.structure.{Residue, RGraph}

case class Gly(graph: RGraph, positions: Map[Residue, (Double, Double)])

object Gly {
  def from(glycanoCanvas: GlycanoCanvas): Gly = {
    import glycanoCanvas.RichResidue

    val graph = glycanoCanvas.graph()
    val positions = for {
      r <- glycanoCanvas.graph().residues
      item <- r.getItem
      p = item.position
    } yield r -> (p.x, p.y)

    Gly(graph, positions.toMap)
  }
}
