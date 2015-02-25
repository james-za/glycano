package za.jwatson.glycanoweb.structure

case class Substituent(id: Int, st: SubstituentType)

object Substituent {
  var nextId: Int = 0

  def next(st: SubstituentType): Substituent = {
    nextId += 1
    new Substituent(nextId, st)
  }
}