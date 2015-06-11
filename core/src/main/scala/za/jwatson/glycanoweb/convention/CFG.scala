package za.jwatson.glycanoweb.convention

object CFG {
  val text =
    """
      |convention "CFG" {
      |   def CFGCircle = Circle(r="30")
      |   def CFGSquare = Rect(width="60", height="60")
      |   def CFGSquareTR = Polygon(points="0,0 60,0 60,60")
      |   def CFGDiamond = Polygon(points="30,0 60,30 30,60 0,30")
      |   def CFGDiamondT = Polygon(points="0,30 30,0 60,30")
      |   def CFGDiamondB = Polygon(points="60,30 30,60 0,30")
      |   def CFGDiamondL = Polygon(points="30,60 0,30 30,0")
      |   def CFGDiamondR = Polygon(points="30,0 60,30 30,60")
      |   def CFGTriangle = Polygon(points="0,60 30,0 60,60")
      |   def CFGStar = Star(n="5", r1="-10", r2="-30")
      |
      |   default
      |   -> #1 [primary] CFGCircle
      |   -> #2 [outline, links] CFGCircle
      |   -> style [primary] { fill: #FFFFFF }
      |   -> style [outline] { stroke: #000000; stroke-width: 3 }
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <>
      |   -> #1 [primary] CFGCircle
      |   -> #2 [outline, links] CFGCircle
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <N, Ac>
      |   -> #1 [primary] CFGSquare
      |   -> #2 [outline, links] CFGSquare
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <N>
      |   -> #1 [secondary] CFGSquare
      |   -> #2 [primary] CFGSquareTR
      |   -> #3 [thin] CFGSquareTR
      |   -> #4 [outline, links] CFGSquare
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <COOH>
      |   -> #1 [secondary] CFGDiamond
      |   -> #2 [primary] CFGDiamondB
      |   -> #3 [thin] CFGDiamondB
      |   -> #4 [outline, links] CFGDiamond
      |
      |   (Fuc) <>
      |   -> #1 [primary] CFGTriangle
      |   -> #2 [outline, links] CFGTriangle
      |
      |   (Rib, Ara, Xyl, Lyx) <>
      |   -> #1 [primary] CFGStar
      |   -> #2 [outline, links] CFGStar
      |
      |   (All) -> style [primary] { fill: #7D007D }
      |   (Alt) -> style [primary] { fill: #FA6400 }
      |   (Glc) -> style [primary] { fill: #0000FA }
      |   (Man) -> style [primary] { fill: #00C832 }
      |   (Gul) -> style [primary] { fill: #FA78FA }
      |   (Ido) -> style [primary] { fill: #966432 }
      |   (Gal) -> style [primary] { fill: #FFFF00 }
      |   (Tal) -> style [primary] { fill: #FA0000 }
      |
      |   (Rib) -> style [primary] { fill: #0000FA }
      |   (Ara) -> style [primary] { fill: #00C832 }
      |   (Xyl) -> style [primary] { fill: #FA6400 }
      |   (Lyx) -> style [primary] { fill: #FFFF00 }
      |
      |   * -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   * -> style [thin] { fill: none; stroke: #000000; stroke-width: 1 }
      |   * -> style [secondary] { fill: #FFFFFF }
      |}
    """.stripMargin
}