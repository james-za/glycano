package za.jwatson.glycanoweb.render

trait DiffMap[A, B] {
  val updateWhenCreating: Boolean
  def updateItem(source: A, item: B): Unit
  def createItem(source: A): B
  def removeItem(source: A, item: B): Unit

  var items = Map.empty[A, B]

  def :=(source: A): Unit = apply(Set(source))
  def :=(sources: Seq[A]): Unit = apply(sources.toSet)
  def :=(sources: Set[A]): Unit = apply(sources)
  def apply(source: A): Unit = apply(Set(source))
  def apply(sources: Seq[A]): Unit = apply(sources.toSet)
  def apply(sources: Set[A]): Unit = {
    def updateAll(all: Iterable[(A, B)]) = {
      for ((source, item) <- all) updateItem(source, item)
    }
    val itemSet = items.keySet
    val removed = itemSet diff sources
    val created = sources diff itemSet map (s => s -> createItem(s))
    for(r <- removed) removeItem(r, items(r))
    items --= removed
    updateAll(items)
    items ++= created
    if(updateWhenCreating) updateAll(created)
  }
}