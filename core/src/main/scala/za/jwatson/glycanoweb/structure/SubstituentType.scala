package za.jwatson.glycanoweb.structure

case class SubstituentType(symbol: String, name: String)

object SubstituentType {
  private def st(s: String, n: String) = new SubstituentType(s, n)
  private def st(t: String) = new SubstituentType(t, t)
  val p = st("P", "Phosphorus")
  val s = st("S", "Sulphur")
  val n = st("N", "N-linkage")
  val ac = st("Ac", "Acetyl")
  val deoxy = st("Deoxy")
  val methyl = st("Methyl")
  val cooh = st("COOH", "Carboxylic acid")
  val r = st("R", "R")

  val SubstituentTypes = Seq(p, s, n, ac, deoxy, methyl, cooh, r)
  val SubstituentsMap = (for (st <- SubstituentTypes) yield st.symbol -> st).toMap

  def unapply(str: String): Option[SubstituentType] = SubstituentsMap.get(str)
}