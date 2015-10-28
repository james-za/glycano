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
      |   def Nine = Polygon(points="15,10 20,45 -20,45 -15,10 -45,0 -25,-45 0,-20 25,-45 45,0")
      |
      |   def TriangleL = Path(d="M20,35 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def DiamondL  = Path(d="M40,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def ArrowL    = Path(d="M45,30 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def HexagonL  = Path(d="M45,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def SevenL    = Path(d="M45,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def EightL    = Path(d="M45,40 m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |   def NineL     = Path(d="M0,0   m-18,-22 v44 h36 v-4 h-32 v-40 Z")
      |
      |   def AGlycerol = Path(d="M-7,-7h28v-28h14v28h28v14h-70z")
      |   def AErythritol = Path(d="M-7,-7h28v-28h14v28h14v-28h14v28h28v14h-98z")
      |   def AThreitol = Path(d="M-7,-7h98v14h-28v28h-14v-28h-14v28h-14v-28h-28z")
      |   def AMesoRibitol = Path(d="M-7,-7h28v-28h14v28h14v-28h14v28h14v-28h14v28h28v14h-126z")
      |   def AArabinitol = Path(d="M-7,-7h126v14h-28v28h-14v-28h-14v28h-14v-28h-14v28h-14v-28h-28z")
      |   def AMesoXylitol = Path(d="M-7,-7h56v-28h14v28h56v14h-28v28h-14v-28h-42v28h-14v-28h-28z")
      |   def AGlucitol = Path(d="M-7,-7h28v-28h14v28h14v-28h14v28h14v-28h14v28h14v-28h14v28h28v14h-154z")
      |   def AUnknownA = Path(d="M-7,-7h154v14h-28v28h-14v-28h-14v28h-14v-28h-14v28h-14v-28h-14v28h-14v-28h-28z")
      |   def AUnknownB = Path(d="M-7,-7h28v-28h14v28h70v-28h14v28h28v14h-56v28h-14v-28h-14v28h-14v-28h-56z")
      |   def AMesoAllitol = Path(d="M-7,-7h56v-28h14v28h14v-28h14v28h56v14h-28v28h-14v-28h-70v28h-14v-28h-28z")
      |
      |   palette "Aldoses" {
      |      Glycero, Erythro, Threo, Ara, Lyx, Rib, Xyl, Ido, All, Alt, Gal, Glc, Gul, Man, Tal
      |   }
      |   palette "Ketoses" {
      |      Three, Four, Rul, Xul, Fru, Psi, Sor, Tag, AltHep, ManOct, ManOct1COOH3Deoxy, Non, Non1COOH5NAc
      |   }
      |   palette "Alditols" {
      |      Glycerol, Erythritol, Threitol, MesoRibitol, Arabinitol, MesoXylitol, Glucitol, UnknownA, UnknownB, MesoAllitol
      |   }
      |   palette "6-Deoxy Sugars" {
      |      Glc6Deoxy, Gal6Deoxy, Man6Deoxy
      |   }
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
      |   (Glc) <6Deoxy> -> style [primary] { fill: #FF0000 }
      |   D (Gal) <6Deoxy> -> style [primary] { fill: #FF0000 } -> style [secondary] { fill: #000000 }
      |   L (Gal) <6Deoxy> -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FF0000 }
      |   D (Man) <6Deoxy> -> style [primary] { fill: #FF0000 } -> style [secondary] { fill: #000000 }
      |   L (Man) <6Deoxy> -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FF0000 }
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
      |   L (Non)
      |     -> #4 [lshape] NineL
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
      |   (Non)
      |   -> #0 [primary] Nine
      |   -> #2 [outline, links] Nine
      |   -> #3 [outlinefront] Nine
      |   -> #5 [handle] Rect(x="5", y="-5", width="20", height="20", rx="5", ry="5")
      |
      |   (Sor) -> #1 [secondary] Polygon(points="0,40 90,40 65,80 25,80")
      |   (Tag) -> #1 [secondary] Polygon(points="90,40 65,0 45,0 45,80 65,80")
      |
      |   (Three, Four, Rul, Fru, Sor, Tag, AltHep, ManOct) -> style [primary] { fill: #FFFFFF }
      |   (Xul, Psi) -> style [primary] { fill: #808080 }
      |   (Sor) -> style [secondary] { fill: #808080 }
      |   (Tag) -> style [secondary] { fill: #808080 }
      |
      |   (ManOct) <1COOH, 3Deoxy> -> style [primary] { fill: #ffd600 }
      |   (Non) -> style [primary] { fill: #ffffff }
      |   (Non) <1COOH, 5N, 5Ac> -> style [primary] { fill: #d100d1 }
      |
      |   (Three, Four, Rul, Xul, Fru, Psi, Sor, Tag, AltHep, ManOct, Non)
      |   -> style [outline] { fill: none; stroke: #000000; stroke-width: 11 }
      |   -> style [outlinefront] { fill: none; stroke: #FFFFFF; stroke-width: 9 }
      |
      |   (Glycerol)
      |   -> #0 [primary] AGlycerol
      |   -> #2 [border] AGlycerol
      |   -> #3 [links] Polygon(points="56,0 0,0 28,-28")
      |   -> #4 [outline] Polygon(points="-7,-35 63,7")
      |   (Erythritol)
      |   -> #0 [primary] AErythritol
      |   -> #2 [border] AErythritol
      |   -> #3 [links] Polygon(points="84,0 0,0 28,-28 56,-28")
      |   -> #4 [outline] Polygon(points="-7,-35 91,7")
      |   (Threitol)
      |   -> #0 [primary] AThreitol
      |   -> #2 [border] AThreitol
      |   -> #3 [links] Polygon(points="84,0 56,28 28,28 0,0")
      |   -> #4 [outline] Polygon(points="-7,-7 91,35")
      |   (MesoRibitol)
      |   -> #0 [primary] AMesoRibitol
      |   -> #2 [border] AMesoRibitol
      |   -> #3 [links] Polygon(points="112,0 0,0 28,-28 56,-28 84,-28")
      |   -> #4 [outline] Polygon(points="-7,-35 119,7")
      |   (Arabinitol)
      |   -> #0 [primary] AArabinitol
      |   -> #2 [border] AArabinitol
      |   -> #3 [links] Polygon(points="112,0 84,28 56,28 28,28 0,0")
      |   -> #4 [outline] Polygon(points="-7,-7 119,35")
      |   (MesoXylitol)
      |   -> #0 [primary] AMesoXylitol
      |   -> #1 [secondary] Path(d="M35,-7h14v-28h14v28h14v14h-42z")
      |   -> #2 [border] AMesoXylitol
      |   -> #3 [links] Polygon(points="112,0 84,28 56,-28 28,28 0,0")
      |   -> #4 [outline] Polygon(points="-7,-35 119,35")
      |   (Glucitol)
      |   -> #0 [primary] AGlucitol
      |   -> #2 [border] AGlucitol
      |   -> #3 [links] Polygon(points="140,0 0,0 28,-28 56,-28 84,-28 112,-28")
      |   -> #4 [outline] Polygon(points="-7,-35 147,7")
      |   (UnknownA)
      |   -> #0 [primary] AUnknownA
      |   -> #2 [border] AUnknownA
      |   -> #3 [links] Polygon(points="140,0 112,28 84,28 56,28 28,28 0,0")
      |   -> #4 [outline] Polygon(points="-7,-7 147,35")
      |   (UnknownB)
      |   -> #0 [primary] AUnknownB
      |   -> #1 [secondary] Path(d="M49,-7h42v42h-14v-28h-14v28h-14z")
      |   -> #2 [border] AUnknownB
      |   -> #3 [links] Polygon(points="140,0 84,28 56,28 0,0 28,-28 112,-28")
      |   -> #4 [outline] Polygon(points="-7,-35 147,35")
      |   (MesoAllitol)
      |   -> #0 [primary] AMesoAllitol
      |   -> #1 [secondary] Path(d="M35,-7h14v-28h14v28h14v-28h14v28h14v14h-70z")
      |   -> #2 [border] AMesoAllitol
      |   -> #3 [links] Polygon(points="140,0 112,28 28,28 0,0 56,-28 84,-28")
      |   -> #4 [outline] Polygon(points="-7,-35 147,35")
      |
      |   (Glycerol, Erythritol, Threitol, MesoRibitol, Arabinitol, MesoXylitol, Glucitol, UnknownA, UnknownB, MesoAllitol)
      |   -> style [border] { fill: none; stroke: #000000; stroke-width: 1 }
      |   -> style [links] { fill: none }
      |   -> style [outline] { fill: none; stroke: none }
      |
      |   (Glycerol, Erythritol, MesoRibitol, Glucitol, UnknownB)
      |   -> style [primary] { fill: #FFFFFF }
      |   -> style [secondary] { fill: #000000 }
      |
      |   (Threitol, Arabinitol, MesoXylitol, UnknownA, MesoAllitol)
      |   -> style [primary] { fill: #000000 }
      |   -> style [secondary] { fill: #FFFFFF }
      |}
    """.stripMargin
}
