package za.jwatson.glycanoweb.render

import za.jwatson.glycanoweb.structure.{Substituent, SubstituentType}
import SubstituentShape._

import importedjs.paper.Implicits._
import importedjs.{paper => p}
import importedjs.paper.{Point => P}

import scala.scalajs.js

case class SubstituentShape(s: Substituent) {
  val item = subst(s.st)
  item.scale(0.7, 0.7)
}

object SubstituentShape {

  def closedPath(pts: p.Segment*): p.Path = {
    val path = new p.Path(js.Array[p.Segment](pts: _*))
    path.closePath()
    path
  }

  def closedPathScaled(scale: Double, pts: p.Segment*): p.Path = {
    val path = new p.Path(js.Array[p.Segment](pts: _*))
    path.closePath()
    path.scale(scale, scale)
    path
  }

  //  val substBacks = {
  //    import za.jwatson.glycanoweb.structure.{SubstituentType => ST}
  //    Map(
  //      ST.n -> p.Path.RoundRectangle(new p.Rectangle(-10, -15, 20, 30), new p.Size(5, 5)),
  //      ST.cooh -> p.Path.Circle(P(0, 0), 10),
  //      ST.methyl -> p.Path.Rectangle(new p.Rectangle(-5, -15, 10, 30)),
  //      ST.deoxy -> p.Path.Circle(P(0, 0), 8),
  //      ST.s -> p.Path.Circle(P(0, 0), 15),
  //      ST.p -> p.Path.Circle(P(0, 0), 15),
  //      ST.ac -> closedPath(P(0, 20), P(-20, 20), P(20, 20))
  //    )
  //  }

  //val substs = SubstituentType.substituentTypes.map(st => st -> subst(st)).toMap

  //for (a <- substs.values) a.remove()

  def subst(st: SubstituentType): p.Item = {
    import za.jwatson.glycanoweb.structure.{SubstituentType => ST, Substituent}
    def pt(s: Int, c: String, t: String) = {
      val d = new p.PointText(P(0, 0))
      d.fontSize = s
      d.fillColor = new p.Color(c)
      d.content = t
      d.position = P(0, 0)
      d
    }
    def rrb(db: p.Rectangle, s: Double, r: Double, fill: String, stroke: String) = {
      val rect = new p.Rectangle(db.x - s, db.y - s, db.width + s * 2, db.height + s * 2)
      val b = p.Path.RoundRectangle(rect, new p.Size(r, r))
      b.fillColor = new p.Color(fill)
      b.strokeColor = stroke
      b
    }
    def cb(db: p.Rectangle, s: Double, fill: String, stroke: String) = {
      val b = p.Path.Circle(db.center, math.max(db.width, db.height) / 2 + s)
      b.fillColor = new p.Color(fill)
      b.strokeColor = stroke
      b
    }
    def tb(db: p.Rectangle, fill: String, stroke: String) = {
      val c = db.center
      val b = closedPath(P(c.x, c.y - 30), P(c.x - 25, c.y + 15), P(c.x + 25, c.y + 15))
      b.fillColor = new p.Color(fill)
      b.strokeColor = stroke
      b
    }
    val d = st match {
      case ST.n => pt(30, "black", "N")
      case ST.cooh =>
        val path = new p.Path("""M 0,14.355469 2.2460938,7.421875 C 7.4218645,9.2448552 11.181626,10.82363 13.525391,12.158203 12.906885,6.2663426 12.581365,2.2136123 12.548828,0 l 7.080078,0 c -0.09768,3.2227258 -0.472027,7.2591801 -1.123047,12.109375 3.35284,-1.692646 7.193982,-3.2551444 11.523438,-4.6875 l 2.246094,6.933594 c -4.134146,1.367244 -8.186877,2.278702 -12.158204,2.734375 1.985652,1.725314 4.785129,4.801483 8.398438,9.228515 L 22.65625,30.46875 C 20.768205,27.89718 18.53839,24.397835 15.966797,19.970703 13.557926,24.560595 11.442043,28.059941 9.6191406,30.46875 L 3.8574219,26.318359 C 7.6334528,21.663463 10.335273,18.587294 11.962891,17.089844 7.763661,16.276098 3.7760348,15.364641 0,14.355469""")
        path.fillColor = new p.Color("black")
        path
      case ST.methyl =>
        //val path1 = closedPathScaled(scale = 22, P(0, 0), P(-0.25, 1), new p.Segment(P(0.25, 1), P(-0.375, 1.5), P(0.375, 1.5)))
        val path = new p.Path("""M0,0 L-0.25,1 C-0.375,1.5 0.375,1.5 0.25,1 Z""")
        path.fillColor = new p.Color("black")
        path.scale(22, 22)
        path
      case ST.deoxy =>
        val path = new p.Path("""M 476.82,418.45 L 486.73,428.41 C 487.28,428.96 487.38,429.06 487.38,429.46 C 487.38,430.01 486.93,430.46 486.39,430.46 C 485.99,430.46 485.79,430.26 485.34,429.81 L 475.38,419.85 L 465.36,429.81 C 464.82,430.36 464.72,430.46 464.32,430.46 C 463.82,430.46 463.32,430.01 463.32,429.46 C 463.32,429.06 463.52,428.86 463.97,428.41 L 473.88,418.45 L 463.97,408.54 C 463.47,408.04 463.32,407.74 463.32,407.45 C 463.32,406.9 463.82,406.45 464.32,406.45 C 464.72,406.45 464.82,406.55 465.36,407.1 L 475.33,417.06 L 485.29,407.1 C 485.79,406.6 486.09,406.45 486.39,406.45 C 486.98,406.45 487.38,406.9 487.38,407.45 C 487.38,407.84 487.28,407.94 486.73,408.49 L 476.82,418.45 z """)
        path.fillColor = new p.Color("black")
        //        path.strokeWidth = 12
        //        path.strokeCap = "square"
        path
      case ST.s => pt(30, "black", "S")
      case ST.p => pt(30, "white", "P")
      case ST.ac => pt(24, "black", "Ac")
    }
    val db = d.strokeBounds
    val b = st match {
      case ST.n => rrb(db, 5, 5, fill = "#86CEFF", stroke = "black")
      case ST.cooh => cb(db, 5, fill = "white", stroke = "")
      case ST.methyl => rrb(db, 5, 5, fill = "white", stroke = "")
      case ST.deoxy => cb(db, 6, fill = "white", stroke = "black")
      case ST.s => cb(db, 5, fill = "#FFFF00", stroke = "black")
      case ST.p => cb(db, 5, fill = "#8E008E", stroke = "black")
      case ST.ac => tb(db, fill = "white", stroke = "black")
    }
    new p.Group(js.Array[p.Item](b, d))
  }
}