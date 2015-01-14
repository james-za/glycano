package importedjs

import importedjs.paper.PaperScope
import org.scalajs.dom.HTMLCanvasElement

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSName("paper")
object Paper extends PaperScope {
  //def setup(canvas: HTMLCanvasElement): Unit = js.native
}
  
package paper {

import org.scalajs.dom.{MouseEvent, HTMLImageElement, SVGElement}

object Implicits {
  //import org.scalajs.dom.extensions.{Color => _, _}
  import scala.language.implicitConversions
  implicit def toColor(str: String): Color = new Color(str)
  implicit def toSegment(p: Point): Segment = p.asInstanceOf[Segment]
  implicit class AnimateItem[T <: Item](item: T) {
    def animate(duration: Double)(frame: (T, Double) => Unit): Unit = {
      Animation.create(item, duration, (i: T, t: Double) => { frame(i, t); true }, frame)
    }
    def animate(duration: Double)(frame: (T, Double) => Boolean, done: (T, Double) => Unit): Unit = {
      Animation.create(item, duration, frame, done)
    }
  }
}
  
object Animation {
  var animations = collection.immutable.Queue[Animation[_]]()
  class Animation[T <: Item](item: T, duration: Double, frame: (T, Double) => Boolean, done: (T, Double) => _) {
    var time: Double = 0.0
    def step(dt: Double): Boolean = {
      time += dt
      if (time > duration) {
        done(item, duration)
        false
      } else frame(item, time)
    }
  }
  def create[T <: Item](item: T, duration: Double, frame: (T, Double) => Boolean, done: (T, Double) => Unit): Unit = {
    animations enqueue new Animation[T](item, duration, frame, done)
  }
  def onFrameHandler: js.Function1[FrameEvent, _] = (e: FrameEvent) => {
    animations = animations.filter(_.step(e.delta))
  }
}

trait KeyModifiers extends js.Object {
  var shift: Boolean = js.native
  var control: Boolean = js.native
  var option: Boolean = js.native
  var command: Boolean = js.native
  var capsLock: Boolean = js.native
}

@JSName("paper.PaperScope")
class PaperScope extends js.Object {
  var version: Double = js.native
  var project: Project = js.native
  var projects: js.Array[Project] = js.native
  var view: View = js.native
  var views: js.Array[View] = js.native
  var tool: Tool = js.native
  var tools: js.Array[Tool] = js.native
  def install(scope: js.Any): js.Dynamic = js.native
  def setup(canvas: HTMLCanvasElement): js.Dynamic = js.native
  def setup(canvas: String): js.Dynamic = js.native
  def clear(): js.Dynamic = js.native
  def remove(): js.Dynamic = js.native
}

@JSName("paper.PaperScope")
object PaperScope extends js.Object {
  def get(id: String): PaperScope = js.native
  def each(iter: js.Function0[Any]): js.Dynamic = js.native
}

@JSName("paper.PaperScript")
class PaperScript extends js.Object {
}

@JSName("paper.PaperScript")
object PaperScript extends js.Object {
  def compile(code: String): String = js.native
  def evaluate(code: String, scope: PaperScope): js.Any = js.native
}

@JSName("paper.Point")
class Point protected () extends js.Object {
  def this(x: Double, y: Double) = this()
  def this(values: js.Array[js.Any]) = this()
  def this(`object`: js.Any) = this()
  def this(size: Size) = this()
  def this(point: Point) = this()
  var x: Double = js.native
  var y: Double = js.native
  var length: Double = js.native
  var angle: Double = js.native
  var angleInRadians: Double = js.native
  var quadrant: Double = js.native
  var selected: Boolean = js.native
  def add(point: Point): Point = js.native
  def add(x: Double, y: Double): Point = js.native
  def add(value: Double): Point = js.native
  def subtract(point: Point): Point = js.native
  def subtract(x: Double, y: Double): Point = js.native
  def subtract(value: Double): Point = js.native
  def multiply(point: Point): Point = js.native
  def multiply(x: Double, y: Double): Point = js.native
  def multiply(value: Double): Point = js.native
  def divide(point: Point): Point = js.native
  def divide(x: Double, y: Double): Point = js.native
  def divide(value: Double): Point = js.native
  @JSName("clone") def clonePoint(insert: Boolean = js.native): Point = js.native
  override def toString(): String = js.native
  def transform(matrix: Matrix): Point = js.native
  def getDistance(point: Point, squared: Boolean): Double = js.native
  def normalize(length: Double = js.native): js.Dynamic = js.native
  def getAngle(): Double = js.native
  def getAngleInRadians(): Double = js.native
  def getDirectedAngle(point: Point): Double = js.native
  def rotate(angle: Double, center: Point): Point = js.native
  def isInside(rect: Rectangle): Boolean = js.native
  def isClose(point: Point, tolerance: Double): Boolean = js.native
  def isColinear(point: Point): Boolean = js.native
  def isOrthogonal(point: Point): Boolean = js.native
  def isZero(): Boolean = js.native
  def isNan(): Boolean = js.native
  def dot(point: Point): Double = js.native
  def cross(point: Point): Double = js.native
  def project(point: Point): Point = js.native
}

@JSName("paper.Point")
object Points extends js.Object {
  def min(point1: Point, point2: Point): Point = js.native
  def max(point1: Point, point2: Point): Point = js.native
}

object Point {
  def apply(x: Double, y: Double): Point = new Point(x, y)
}

@JSName("paper.Size")
class Size extends js.Object {
  def this(width: Double, height: Double) = this()
  var width: Double = js.native
  var height: Double = js.native
  override def toString(): String = js.native
  @JSName("clone") def cloneSize(insert: Boolean = js.native): Size = js.native
  def isZero(): Boolean = js.native
  def isNan(): Boolean = js.native
  def round(): Size = js.native
  def ceil(): Size = js.native
  def floor(): Size = js.native
  def abs(): Size = js.native
}

@JSName("paper.Size")
object Size extends js.Object {
  def min(size1: Size, size2: Size): Size = js.native
  def max(size1: Size, size2: Size): Size = js.native
  def random(): Size = js.native
}

@JSName("paper.Rectangle")
class Rectangle protected () extends js.Object {
  def this(point: Point, size: Size) = this()
  def this(x: Double, y: Double, width: Double, height: Double) = this()
  def this(point1: Point, point2: Point) = this()
  def this(rect: Rectangle) = this()
  var x: Double = js.native
  var y: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  var point: Point = js.native
  var size: Size = js.native
  var left: Double = js.native
  var top: Double = js.native
  var right: Double = js.native
  var bottom: Double = js.native
  var center: Point = js.native
  var topLeft: Point = js.native
  var topRight: Point = js.native
  var bottomLeft: Point = js.native
  var bottomRight: Point = js.native
  var leftCenter: Point = js.native
  var topCenter: Point = js.native
  var rightCenter: Point = js.native
  var bottomCenter: Point = js.native
  def isEmpty(): Boolean = js.native
  override def toString(): String = js.native
  def contains(point: Point): Boolean = js.native
  def contains(rect: Rectangle): Boolean = js.native
  def intersects(rect: Rectangle): Boolean = js.native
  def intersect(rect: Rectangle): Rectangle = js.native
  def unite(rect: Rectangle): Rectangle = js.native
  def include(point: Point): js.Dynamic = js.native
}

@JSName("paper.Matrix")
class Matrix protected () extends js.Object {
  def this(a: Double, c: Double, b: Double, d: Double, tx: Double, ty: Double) = this()
  var scaleX: Double = js.native
  var scaleY: Double = js.native
  var shearX: Double = js.native
  var shearY: Double = js.native
  var translateX: Double = js.native
  var translateY: Double = js.native
  var values: js.Array[Double] = js.native
  var rotation: Double = js.native
  @JSName("clone") def cloneMatrix(insert: Boolean = js.native): Matrix = js.native
  def set(a: Double, c: Double, b: Double, d: Double, tx: Double, ty: Double): Matrix = js.native
  def scale(scale: Double, center: Point = js.native): Matrix = js.native
  @JSName("scale") def scale2(hor: Double, ver: Double, center: Point = js.native): Matrix = js.native
  def translate(point: Point): Matrix = js.native
  def translate(dx: Double, dy: Double): Matrix = js.native
  def rotate(angle: Double, center: Point): Matrix = js.native
  def rotate(angle: Double, x: Double, y: Double): Matrix = js.native
  def shear(point: Point, center: Point = js.native): Matrix = js.native
  @JSName("shear") def shear2(hor: Double, ver: Double, center: Point = js.native): Matrix = js.native
  override def toString(): String = js.native
  def concatenate(mx: Matrix): Matrix = js.native
  def preConcatenate(mx: Matrix): Matrix = js.native
  def transform(point: Point): Matrix = js.native
  def transform(src: js.Array[Double], srcOff: Double, dst: js.Array[Double], dstOff: Double, numPts: Double): js.Array[Double] = js.native
  def inverseTransform(point: Point): Matrix = js.native
  def isIdentity(): Boolean = js.native
  def isInvertible(): Boolean = js.native
  def isSingular(): Boolean = js.native
  def createInverse(): Matrix = js.native
  def setToScale(hor: Double, ver: Double): Matrix = js.native
  def setToTranslation(dx: Double, dy: Double): Matrix = js.native
  def setToShear(hor: Double, ver: Double): Matrix = js.native
  def setToRotation(angle: Double, x: Double, y: Double): Matrix = js.native
  def applyToContext(ctx: js.Any, reset: Boolean = js.native): js.Dynamic = js.native
}

@JSName("paper.Matrix")
object Matrix extends js.Object {
  def getScaleInstance(hor: Double, ver: Double): Matrix = js.native
  def getTranslateInstance(dx: Double, dy: Double): Matrix = js.native
  def getShearInstance(hor: Double, ver: Double, center: Point): Matrix = js.native
  def getRotateInstance(angle: Double, x: Double, y: Double): Matrix = js.native
}

@JSName("paper.Item")
class Item extends js.Object {
  var id: Double = js.native
  var name: js.UndefOr[String] = js.native
  var position: Point = js.native
  var rotation: Double = js.native
  var applyMatrix: Boolean = js.native
  var style: PathStyle = js.native
  var visible: Boolean = js.native
  var blendMode: String = js.native
  var opacity: Double = js.native
  var guide: Double = js.native
  var selected: Boolean = js.native
  var clipMask: Boolean = js.native
  var project: Project = js.native
  var layer: Layer = js.native
  var parent: Item = js.native
  var children: js.Array[Item] = js.native
  var firstChild: Item = js.native
  var lastChild: Item = js.native
  var nextSibling: Item = js.native
  var previousSibling: Item = js.native
  var index: Double = js.native
  var bounds: Rectangle = js.native
  var strokeBounds: Rectangle = js.native
  var handleBounds: Rectangle = js.native
  var strokeColor: Color = js.native
  var strokeWidth: Double = js.native
  var strokeCap: String = js.native
  var strokeJoin: String = js.native
  var dashOffset: Double = js.native
  var dashArray: js.Array[Double] = js.native
  var miterLimit: Double = js.native
  var fillColor: Color = js.native
  @JSName("fillColor") var fillColorString: String = js.native
  def addChild(item: Item): js.Dynamic = js.native
  def insertChild(index: js.Any, item: Item): js.Dynamic = js.native
  def addChildren(items: js.Array[Item]): js.Dynamic = js.native
  def insertChildren(index: js.Any, items: js.Array[Item]): js.Dynamic = js.native
  def insertAbove(item: Item): Boolean = js.native
  def insertBelow(item: Item): Boolean = js.native
  def remove(): Boolean = js.native
  def removeChildren(): js.Array[Item] = js.native
  def removeChildren(from: Double, to: Double = js.native): js.Array[Item] = js.native
  def reverseChildren(): js.Dynamic = js.native
  def hasChildren(): Boolean = js.native
  def isAbove(item: Item): Boolean = js.native
  def isBelow(item: Item): Boolean = js.native
  def isParent(item: Item): Boolean = js.native
  def isChild(item: Item): Boolean = js.native
  def isDescendant(item: Item): Boolean = js.native
  def isAncestor(item: Item): Boolean = js.native
  def isGroupedWith(item: Item): Boolean = js.native
  def contains(point: Point): Boolean = js.native
  def matches(data: js.Dynamic): Boolean = js.native
  def getItems(data: js.Dynamic): js.Array[Item] = js.native
  def getItem(data: js.Dynamic): Item = js.native
  def scale(scale: Double, center: Point): js.Dynamic = js.native
  def scale(hor: Double, ver: Double, center: Point = js.native): js.Dynamic = js.native
  def translate(delta: Double): js.Dynamic = js.native
  def rotate(angle: Double, center: Point = js.native): js.Dynamic = js.native
  def shear(point: Point, center: Point): js.Dynamic = js.native
  def shear(hor: Double, ver: Double, center: Point): js.Dynamic = js.native
  def transform(matrix: Matrix, flags: js.Array[js.Any] = js.native): js.Dynamic = js.native
  def fitBounds(rectangle: Rectangle, fill: Boolean = js.native): js.Dynamic = js.native
  def removeOn(`object`: js.Any): js.Dynamic = js.native
  def removeOnMove(): js.Dynamic = js.native
  def removeOnDown(): js.Dynamic = js.native
  def removeOnDrag(): js.Dynamic = js.native
  def removeOnUp(): js.Dynamic = js.native
  @JSName("clone") def cloneItem(insert: Boolean = js.native): Item = js.native
  def exportSVG(options: js.Dynamic = js.native): SVGElement = js.native
  def importSVG(svg: SVGElement): Item = js.native
  def exportJSON(options: js.Dynamic = js.native): String = js.native
  def importJSON(json: String): Item = js.native

  def globalToLocal(p: Point): Point = js.native
  def localToGlobal(p: Point): Point = js.native

  var onFrame: js.Function1[FrameEvent, _] = js.native
  var onMouseDown: js.Function1[PaperMouseEvent, _] = js.native
  var onMouseUp: js.Function1[PaperMouseEvent, _] = js.native
  var onClick: js.Function1[PaperMouseEvent, _] = js.native
  var onDoubleClick: js.Function1[PaperMouseEvent, _] = js.native
  var onMouseMove: js.Function1[PaperMouseEvent, _] = js.native
  var onMouseEnter: js.Function1[PaperMouseEvent, _] = js.native
  var onMouseLeave: js.Function1[PaperMouseEvent, _] = js.native
}

trait FrameEvent extends js.Object {
  def time: Double = js.native
  def delta: Double = js.native
  def count: Int = js.native
}

trait PaperMouseEvent extends js.Object {
  def `type`: String = js.native
  def point: Point = js.native
  def target: Item = js.native
  def delta: Point = js.native
}

@JSName("paper.Group")
class Group protected () extends Item {
  def this(children: js.Array[Item]) = this()
  var clipped: Boolean = js.native
}

@JSName("paper.Layer")
class Layer extends Group {
  def this(children: js.Array[Item]) = this()
  def activate(): js.Dynamic = js.native
}

@JSName("paper.PlacedItem")
class PlacedItem extends Item {
  var matrix: Matrix = js.native
}

@JSName("paper.Raster")
class Raster protected () extends PlacedItem {
  def this(imageId: String) = this()
  var size: Size = js.native
  var width: Double = js.native
  var height: Double = js.native
  var ppi: Double = js.native
  var image: HTMLImageElement = js.native
  def getSubImage(rect: Rectangle): HTMLCanvasElement = js.native
  def drawImage(image: HTMLImageElement, point: Point): js.Dynamic = js.native
  def getAverageColor(path: Path): Color = js.native
  def getAverageColor(rect: Rectangle): Color = js.native
  def getAverageColor(point: Point): Color = js.native
  def getPixel(x: Double, y: Double): RgbColor = js.native
  def getPixel(point: Point): RgbColor = js.native
  def setPixel(x: Double, y: Double, color: Color): js.Dynamic = js.native
  def setPixel(point: Point, color: Color): js.Dynamic = js.native
  def createData(size: Size): js.Any = js.native
  def getData(rect: Rectangle): js.Any = js.native
  def setData(data: js.Any, point: Point): js.Any = js.native
}

@JSName("paper.PlacedSymbol")
class PlacedSymbol protected () extends PlacedItem {
  def this(symbol: Symbol, matrix: Matrix) = this()
  def this(symbol: Symbol, point: Point = js.native) = this()
  var symbol: Symbol = js.native
}

@JSName("paper.HitResult")
class HitResult extends js.Object {
  var `type`: String = js.native
  var name: String = js.native
  var item: Item = js.native
  var location: CurveLocation = js.native
  var segment: Segment = js.native
  var point: Point = js.native
}

@JSName("paper.PathItem")
class PathItem extends Item {
  def smooth(): js.Dynamic = js.native
  def moveTo(point: Point): js.Dynamic = js.native
  def moveTo(x: Double, y: Double): js.Dynamic = js.native
  def lineTo(point: Point): js.Dynamic = js.native
  def lineTo(x: Double, y: Double): js.Dynamic = js.native
  def cubicCurveTo(handle1: Point, handle2: Point, to: Point): js.Dynamic = js.native
  def quadraticCurveTo(handle: Point, to: Point): js.Dynamic = js.native
  def curveTo(through: Point, to: Point, parameter: js.Any = js.native): js.Dynamic = js.native
  def arcTo(through: Point, to: Point): js.Dynamic = js.native
  def arcTo(to: Point, clockwise: Boolean = js.native): js.Dynamic = js.native
  def closePath(): js.Dynamic = js.native
  def moveBy(point: Point): js.Dynamic = js.native
  def moveBy(x: Double, y: Double): js.Dynamic = js.native
  def lineBy(vector: Point): js.Dynamic = js.native
  def lineBy(x: Double, y: Double): js.Dynamic = js.native
  def curveBy(throughVector: Point, toVector: Point, parameter: js.Any = js.native): js.Dynamic = js.native
  def arcBy(throughVector: Point, toVector: Point): js.Dynamic = js.native
}

@JSName("paper.Path")
class Path extends PathItem {
  def this(segments: js.Array[Segment]) = this()
  def this(pathData: String) = this()
  //def this(points: js.Array[Point]) = this()
  var segments: js.Array[Segment] = js.native
  var firstSegment: Segment = js.native
  var lastSegment: Segment = js.native
  var curves: js.Array[Curve] = js.native
  var firstCurve: Curve = js.native
  var lastCurve: Curve = js.native
  var closed: Boolean = js.native
  var fullySelected: Boolean = js.native
  var clockwise: Boolean = js.native
  var length: Double = js.native
  def add(segment: Segment): Segment = js.native
  def insert(index: Double, segment: Segment): Segment = js.native
  def addSegments(segments: js.Array[Segment]): js.Array[Segment] = js.native
  def insertSegments(index: Double, segments: js.Array[Segment]): js.Array[Segment] = js.native
  def removeSegment(index: Double): Segment = js.native
  def removeSegments(): js.Array[Segment] = js.native
  def removeSegments(from: Double, to: Double = js.native): js.Array[Segment] = js.native
  def flatten(maxDistance: Double): js.Dynamic = js.native
  def simplify(tolerance: Double = js.native): js.Dynamic = js.native
  def reverse(): js.Dynamic = js.native
  def join(path: Path): js.Dynamic = js.native
  def getLocationAt(offset: Double, isParameter: Boolean = js.native): CurveLocation = js.native
  def getPointAt(offset: Double, isParameter: Boolean = js.native): Point = js.native
  def getTangentAt(offset: Double, isParameter: Boolean = js.native): Point = js.native
  def getNormalAt(offset: Double, isParameter: Boolean = js.native): Point = js.native
  def getNearestLocation(point: Point): CurveLocation = js.native
  def getNearestPoint(point: Point): Point = js.native
  @JSName("clone") def clonePath(insert: Boolean = js.native): Path = js.native
}

@JSName("paper.Path")
object Path extends js.Object {
  def Line(pt1: Point, pt2: Point): Path = js.native
  def Rectangle(point: Point, size: js.Any): Path = js.native
  def Rectangle(point1: Point, point2: Point): Path = js.native
  def Rectangle(rect: Rectangle): Path = js.native
  def RoundRectangle(rect: Rectangle, size: Size): Path = js.native
  def Oval(rect: Rectangle, circumscribed: Boolean = js.native): Path = js.native
  def Circle(center: Point, radius: Double): Path = js.native
  def Arc(from: Point, through: Point, to: Point): Path = js.native
  def RegularPolygon(center: Point, numSides: Double, radius: Double): Path = js.native
  def Star(center: Point, numPoints: Double, radius1: Double, radius2: Double): Path = js.native
}

@JSName("paper.CompoundPath")
class CompoundPath protected () extends PathItem {
  def this(paths: js.Array[Path]) = this()
  def simplify(): js.Dynamic = js.native
}

@JSName("paper.Segment")
class Segment protected () extends js.Object {
  def this(point: Point = js.native, handleIn: Point = js.native, handleOut: Point = js.native) = this()
  var point: Point = js.native
  var handleIn: Point = js.native
  var handleOut: Point = js.native
  var selected: Boolean = js.native
  var index: Double = js.native
  var path: Path = js.native
  var curve: Curve = js.native
  var next: Segment = js.native
  var previous: Segment = js.native
  def reverse(): Segment = js.native
  def remove(): js.Dynamic = js.native
  override def toString(): String = js.native
}

@JSName("paper.SegmentPoint")
class SegmentPoint extends Point {
  def set(x: Double, y: Double): SegmentPoint = js.native
  def getX(): Double = js.native
  def setX(x: Double): js.Dynamic = js.native
  def getY(): Double = js.native
  def setY(y: Double): js.Dynamic = js.native
  override def isZero(): Boolean = js.native
  def setSelected(selected: Boolean): js.Dynamic = js.native
  def isSelected(): Boolean = js.native
}

@JSName("paper.Curve")
class Curve protected () extends js.Object {
  def this(segment1: Segment, segment2: Segment) = this()
  var point1: Point = js.native
  var point2: Point = js.native
  var handle1: Point = js.native
  var handle2: Point = js.native
  var segment1: Segment = js.native
  var segment2: Segment = js.native
  var path: Path = js.native
  var index: Double = js.native
  var next: Curve = js.native
  var previous: Curve = js.native
  var selected: Boolean = js.native
  var length: Double = js.native
  def isLinear(): Boolean = js.native
  def getParameterAt(offset: Double, start: Double = js.native): Double = js.native
  def getPoint(parameter: Double): Point = js.native
  def getTangent(parameter: Double): Point = js.native
  def getNormal(parameter: Double): Point = js.native
  def getParameter(point: Point): Double = js.native
  def reverse(): Curve = js.native
  @JSName("clone") def cloneCurve(insert: Boolean = js.native): Curve = js.native
  override def toString(): String = js.native
}

@JSName("paper.PathStyle")
class PathStyle extends js.Object {
  var strokeColor: js.Any = js.native
  var strokeWidth: Double = js.native
  var strokeCap: String = js.native
  var strokeJoin: String = js.native
  var dashOffset: Double = js.native
  var dashArray: js.Array[Double] = js.native
  var miterLimit: Double = js.native
  var fillColor: Color = js.native
}

@JSName("paper.CurveLocation")
class CurveLocation protected () extends js.Object {
  def this(curve: Curve, parameter: Double, point: Point, distance: Double) = this()
  var segment: Segment = js.native
  var curve: Curve = js.native
  var path: Path = js.native
  var index: Double = js.native
  var offset: Double = js.native
  var curveOffset: Double = js.native
  var parameter: Double = js.native
  var point: Point = js.native
  var tangent: Point = js.native
  var normal: Point = js.native
  var distance: Double = js.native
  override def toString(): String = js.native
}

@JSName("paper.Project")
class Project extends js.Object {
  var currentStyle: PathStyle = js.native
  var index: Double = js.native
  var selectedItems: js.Array[Item] = js.native
  var layers: js.Array[Layer] = js.native
  var activeLayer: Layer = js.native
  var symbols: js.Array[Symbol] = js.native
  var views: js.Array[View] = js.native
  var activeView: View = js.native
  def activate(): js.Dynamic = js.native
  def remove(): js.Dynamic = js.native
  def selectAll(): js.Dynamic = js.native
  def deselectAll(): js.Dynamic = js.native
  def hitTest(point: Point, options: js.Any = js.native): HitResult = js.native
  def exportSVG(options: js.Dynamic = js.native): SVGElement = js.native
}

@JSName("paper.Symbol")
class Symbol protected () extends js.Object {
  def this(item: Item) = this()
  var project: Project = js.native
  var definition: Item = js.native
  def place(position: Point = js.native): PlacedSymbol = js.native
  @JSName("clone") def cloneSymbol(insert: Boolean = js.native): Symbol = js.native
}

@JSName("paper.Color")
class Color extends js.Object {
  def this(s: String) = this()
  def this(r: Double, g: Double, b: Double) = this()
  def this(r: Double, g: Double, b: Double, a: Double) = this()
  var `type`: String = js.native
  var alpha: Double = js.native
  var red: Double = js.native
  var green: Double = js.native
  var blue: Double = js.native
  var gray: Double = js.native
  var hue: Double = js.native
  var saturation: Double = js.native
  var brightness: Double = js.native
  var lightness: Double = js.native
  @JSName("clone") def cloneColor(insert: Boolean = js.native): Color = js.native
  def hasAlpha(): Boolean = js.native
  override def toString(): String = js.native
  def toCssString(): String = js.native
}

@JSName("paper.GrayColor")
class GrayColor protected () extends Color {
  def this(gray: Double, alpha: Double = js.native) = this()
}

@JSName("paper.RgbColor")
class RgbColor protected () extends Color {
  def this(red: Double, green: Double, blue: Double, alpha: Double = js.native) = this()
}

@JSName("paper.HsbColor")
class HsbColor protected () extends Color {
  def this(hue: Double, saturation: Double, brightness: Double, alpha: Double = js.native) = this()
}

@JSName("paper.HlsColor")
class HlsColor protected () extends Color {
  def this(hue: Double, saturation: Double, lightness: Double, alpha: Double = js.native) = this()
}

@JSName("paper.GradientColor")
class GradientColor protected () extends Color {
  def this(gradient: Gradient, origin: Point, destination: Point, hilight: Boolean) = this()
  var origin: Point = js.native
  var destination: Point = js.native
  var hilite: Boolean = js.native
  @JSName("clone") def cloneGradientColor(insert: Boolean = js.native): GradientColor = js.native
  def equals(color: GradientColor): Boolean = js.native
  def transform(matrix: js.Any): js.Dynamic = js.native
}

@JSName("paper.Gradient")
class Gradient protected () extends js.Object {
  def this(stops: js.Array[GradientStop], `type`: String = js.native) = this()
  var stops: js.Array[GradientStop] = js.native
  @JSName("clone") def cloneGradient(insert: Boolean = js.native): Gradient = js.native
}

@JSName("paper.GradientStop")
class GradientStop protected () extends js.Object {
  def this(color: Color = js.native, rampPoint: Double = js.native) = this()
  var rampPoint: Double = js.native
  var color: Color = js.native
  @JSName("clone") def cloneGradientStop(insert: Boolean = js.native): GradientStop = js.native
}

@JSName("paper.View")
class View protected () extends js.Object {
  def this(canvas: HTMLCanvasElement) = this()
  var canvas: HTMLCanvasElement = js.native
  var viewSize: Size = js.native
  var bounds: Rectangle = js.native
  var size: Size = js.native
  var center: Point = js.native
  var zoom: Double = js.native
  def activate(): js.Dynamic = js.native
  def remove(): js.Dynamic = js.native
  def isVisible(): Boolean = js.native
  def scrollBy(point: Point): js.Dynamic = js.native
  def scrollBy(x: Double, y: Double): js.Dynamic = js.native
  def setViewSize(width: Double, height: Double): js.Dynamic = js.native
  def draw(): js.Dynamic = js.native
  var onFrame: js.Function1[js.Any, Any] = js.native
  var onResize: js.Function1[js.Any, Any] = js.native
  def viewToProject(point: Point): Point = js.native
  def projectToView(point: Point): Point = js.native
}

@JSName("paper.Tool")
class Tool extends js.Object {
  var eventInterval: Double = js.native
  var minDistance: Double = js.native
  var maxDistance: Double = js.native
  var fixedDistance: Double = js.native
  def activate(): js.Dynamic = js.native
  def remove(): js.Dynamic = js.native
  var onMouseDown: js.Function1[ToolEvent, _] = js.native
  var onMouseDrag: js.Function1[ToolEvent, _] = js.native
  var onMouseMove: js.Function1[ToolEvent, _] = js.native
  var onMouseUp: js.Function1[ToolEvent, _] = js.native
  var onKeyDown: js.Function1[ToolEvent, _] = js.native
  var onKeyUp: js.Function1[ToolEvent, _] = js.native
}

@JSName("paper.ToolEvent")
class ToolEvent extends js.Object {
  var event: MouseEvent = js.native
  var `type`: String = js.native
  var point: Point = js.native
  var lastPoint: Point = js.native
  var downPoint: Point = js.native
  var middlePoint: Point = js.native
  var delta: Point = js.native
  var count: Double = js.native
  var item: Item = js.native
  override def toString(): String = js.native
}

@JSName("paper.Key")
class Key extends js.Object {
}

@JSName("paper.Key")
object Key extends js.Object {
  def isDown(key: String): Boolean = js.native
}

@JSName("paper.KeyEvent")
class KeyEvent extends js.Object {
  var `type`: String = js.native
  var character: String = js.native
  var key: String = js.native
  override def toString(): String = js.native
}

@JSName("paper.TextItem")
class TextItem extends Item {
  var content: String = js.native
  var characterStyle: CharacterStyle = js.native
  var paragraphStyle: ParagraphStyle = js.native
  var fontSize: Double = js.native
  var fontWeight: String = js.native
}

@JSName("paper.PointText")
class PointText () extends TextItem {
  def this(point: Point) = this()
  var point: Point = js.native
}

@JSName("paper.CharacterStyle")
class CharacterStyle extends js.Object {
  var font: String = js.native
  var fontSize: Double = js.native
}

@JSName("paper.ParagraphStyle")
class ParagraphStyle extends js.Object {
  var justification: String = js.native
}

}