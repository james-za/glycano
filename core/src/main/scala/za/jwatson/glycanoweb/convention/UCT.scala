package za.jwatson.glycanoweb.convention

object UCT {
  val text =
    """
      |convention "UCT" {
      |   def Triangle = Polygon(points="40,35 0,70 0,0")
      |   def Diamond = Polygon(points="80,40 40,80 0,40 40,0")
      |   def Arrow = Polygon(points="90,30 60,60 0,60 0,0 60,0")
      |   def Hexagon = Polygon(points="90,40 65,80 25,80 0,40 25,0 65,0")
      |   def Seven = Polygon(points="90,40 65,80 25,80 0,40 25,0 45,15 65,0")
      |   def Eight = Polygon(points="90,40 65,80 45,65 25,80 0,40 25,0 45,15 65,0")
      |
      |   def TriangleL = Path(d="M20,35 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def DiamondL  = Path(d="M40,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def ArrowL    = Path(d="M45,30 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def HexagonL  = Path(d="M45,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def SevenL    = Path(d="M45,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def EightL    = Path(d="M45,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |
      |   default
      |   -> #1 [primary] Hexagon
      |   -> #3 [outline, links] Hexagon
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |   -> style [primary] { fill: #FFFFFF }
      |   -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   -> style [handle] { fill: #FFFFFF; stroke: #000000; stroke-width: 1 }
      |
      |   (Glycero)
      |   -> #1 [primary] Triangle
      |   -> #3 [outline, links] Triangle
      |   -> #5 [handle] Rect(x="30", y="25", width="20", height="20", rx="5", ry="5")
      |   (Erythro, Threo)
      |   -> #1 [primary] Diamond
      |   -> #3 [outline, links] Diamond
      |   -> #5 [handle] Rect(x="70", y="30", width="20", height="20", rx="5", ry="5")
      |   (Ara, Lyx, Rib, Xyl)
      |   -> #1 [primary] Arrow
      |   -> #3 [outline, links] Arrow
      |   -> #5 [handle] Rect(x="80", y="20", width="20", height="20", rx="5", ry="5")
      |   (Ido, All, Alt, Gal, Glc, Gul, Man, Tal)
      |   -> #1 [primary] Hexagon
      |   -> #3 [outline, links] Hexagon
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (Ara) -> #2 [secondary] Polygon(points="0,0 0,60 30,60 30,0")
      |   (Rib) -> #2 [secondary] Polygon(points="0,0 0,30 90,30 60,0")
      |   (Ido) -> #2 [secondary] Polygon(points="25,0 65,0 25,80 65,80")
      |   (All) -> #2 [secondary] Polygon(points="0,40 25,0 65,80 25,80")
      |   (Alt) -> #2 [secondary] Polygon(points="45,0 65,0 90,40 45,40")
      |   (Gal) -> #2 [secondary] Polygon(points="0,40 90,40 65,80 25,80")
      |   (Gul) -> #2 [secondary] Polygon(points="25,0 65,0 25,80 0,40 90,40 65,80")
      |   (Man) -> #2 [secondary] Polygon(points="0,40 25,0 45,0 45,80 25,80")
      |
      |   (Glycero) -> style [primary] { fill: #FFFFFF }
      |   (Erythro) -> style [primary] { fill: #FFFFFF }
      |   (Threo) -> style [primary] { fill: #000000 }
      |   D (Ara) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   L (Ara) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   (Lyx) -> style [primary] { fill: #000000 }
      |   D (Rib) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   L (Rib) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   (Xyl) -> style [primary] { fill: #FFA0A0 }
      |   D (Ido) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #BF6000 }
      |   L (Ido) -> style [primary] { fill: #BF6000 } -> style [secondary] { fill: #000000 }
      |   D (All) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   L (All) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   D (Alt) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   L (Alt) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   D (Gal) -> style [primary] { fill: #FFFF00 } -> style [secondary] { fill: #000000 }
      |   L (Gal) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFF00 }
      |   (Glc) -> style [primary] { fill: #0000FF }
      |   D (Gul) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   L (Gul) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   D (Man) -> style [primary] { fill: #00FF00 } -> style [secondary] { fill: #000000 }
      |   L (Man) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #00FF00 }
      |   (Tal) -> style [primary] { fill: #000000 }
      |
      |   L (Glycero,
      |      Three)
      |     -> #4 [lshape] TriangleL
      |   L (Erythro, Threo,
      |      Four)
      |     -> #4 [lshape] DiamondL
      |   L (Ara, Lyx, Rib, Xyl,
      |      Rul, Xul)
      |     -> #4 [lshape] ArrowL
      |   L (Ido, All, Alt, Gal, Glc, Gul, Man, Tal,
      |      Fru, Psi, Sor, Tag)
      |     -> #4 [lshape] HexagonL
      |   L (AltHep)
      |     -> #4 [lshape] SevenL
      |   L (ManOct)
      |     -> #4 [lshape] EightL
      |
      |   a * -> style [handle] { fill: #FFFFFF; stroke: #000000; stroke-width: 1 }
      |   b * -> style [handle] { fill: #000000; stroke: #000000; stroke-width: 1 }
      |
      |   * -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   * -> style [lshape] { fill: #000000; stroke: #FFFFFF; stroke-width: 1 }
      |
      |   (Three)
      |   -> #0 [primary] Triangle
      |   -> #2 [outline, links] Triangle
      |   -> #3 [outlinefront] Triangle
      |   -> #5 [handle] Rect(x="30", y="25", width="20", height="20", rx="5", ry="5")
      |
      |   (Four)
      |   -> #0 [primary] Diamond
      |   -> #2 [outline, links] Diamond
      |   -> #3 [outlinefront] Diamond
      |   -> #5 [handle] Rect(x="70", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (Rul, Xul)
      |   -> #0 [primary] Arrow
      |   -> #2 [outline, links] Arrow
      |   -> #3 [outlinefront] Arrow
      |   -> #5 [handle] Rect(x="80", y="20", width="20", height="20", rx="5", ry="5")
      |
      |   (Fru, Psi, Sor, Tag)
      |   -> #0 [primary] Hexagon
      |   -> #2 [outline, links] Hexagon
      |   -> #3 [outlinefront] Hexagon
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (AltHep)
      |   -> #0 [primary] Seven
      |   -> #2 [outline, links] Seven
      |   -> #3 [outlinefront] Seven
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (ManOct)
      |   -> #0 [primary] Eight
      |   -> #2 [outline, links] Eight
      |   -> #3 [outlinefront] Eight
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (Sor) -> #1 [secondary] Polygon(points="0,40 90,40 65,80 25,80")
      |   (Tag) -> #1 [secondary] Polygon(points="90,40 65,0 45,0 45,80 65,80")
      |
      |   (Three, Four, Rul, Fru, Sor, Tag, AltHep, ManOct) -> style [primary] { fill: #FFFFFF }
      |   (Xul, Psi) -> style [primary] { fill: #808080 }
      |   (Sor) -> style [secondary] { fill: #808080 }
      |   (Tag) -> style [secondary] { fill: #808080 }
      |
      |   (Three, Four, Rul, Xul, Fru, Psi, Sor, Tag, AltHep, ManOct)
      |   -> style [outline] { fill: none; stroke: #000000; stroke-width: 11 }
      |   -> style [outlinefront] { fill: none; stroke: #FFFFFF; stroke-width: 9 }
      |}
    """.stripMargin
}
