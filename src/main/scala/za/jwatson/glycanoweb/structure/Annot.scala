package za.jwatson.glycanoweb.structure

import monocle.macros.Lenses

@Lenses case class Annot(text: String, size: Double, x: Double = 0, y: Double = 0, rot: Double = 0)

case class AnnotId(id: Int)

object AnnotId {
  var id: Int = 0
  def nextId(): Int = { id += 1; id }
  def next(): AnnotId = AnnotId(nextId())
}