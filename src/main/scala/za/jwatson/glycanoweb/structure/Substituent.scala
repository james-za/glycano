package za.jwatson.glycanoweb.structure

case class Substituent(id: Int, st: SubstituentType)

case class SubstituentType(symbol: String, name: String)

object Substituent {
  var nextId: Int = 0

  def apply(st: SubstituentType) = {
    nextId += 1
    new Substituent(nextId, st)
  }
}

object SubstituentType {
  private def st(s: String, n: String) = new SubstituentType(s, n)
  private def st(t: String) = new SubstituentType(t, t)
  val p = st("P")
  val s = st("S", "Sulphur")
  val n = st("N", "N-linkage")
  val ac = st("Ac", "Acetyl")
  val deoxy = st("Deoxy")
  val methyl = st("Methyl")
  val cooh = st("COOH")


  val substituentTypes = Seq(p, s, n, ac, deoxy, methyl, cooh)
  val substituentsMap = (for (st <- substituentTypes) yield st.symbol -> st).toMap
}