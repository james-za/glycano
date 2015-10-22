package za.jwatson.glycanoweb.structure

import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses

@Lenses case class Annot(text: String, size: Double, x: Double = 0, y: Double = 0, rot: Double = 0)

object Annot {
  implicit val reusability = Reusability.by_==[Annot]
}

case class AnnotId(id: Int)

object AnnotId {
  var id: Int = 0
  def nextId(): Int = { id += 1; id }
  def next(): AnnotId = AnnotId(nextId())

  implicit val reusability: Reusability[AnnotId] = Reusability.by_==
}