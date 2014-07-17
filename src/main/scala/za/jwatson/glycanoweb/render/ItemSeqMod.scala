package za.jwatson.glycanoweb.render

trait ItemSeqMod[T, S] {
  val updateWhenCreating: Boolean
  def update(item: T, source: S): Unit
  def create(source: S): T
  def remove(item: T, source: S): Unit

  var items = Map.empty[S, T]

  def apply(source: S): Unit = apply(Set(source))
  def apply(sources: Seq[S]): Unit = apply(sources.toSet)
  def apply(sources: Set[S]): Unit = {
    def updateAll(all: Iterable[(S, T)]) = {
      for ((source, item) <- all) update(item, source)
    }
    val itemSet = items.keySet
    val removed = itemSet diff sources
    val created = sources diff itemSet map (s => s -> create(s))
    for(r <- removed) remove(items(r), r)
    items --= removed
    updateAll(items)
    items ++= created
    if(updateWhenCreating) updateAll(created)
  }
}