package importedjs

import importedjs.paper.PaperScope
import org.scalajs.dom.HTMLCanvasElement

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSName("paper")
object Paper extends PaperScope {
  //def setup(canvas: HTMLCanvasElement): Unit = ???
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
  var shift: Boolean = ???
  var control: Boolean = ???
  var option: Boolean = ???
  var command: Boolean = ???
  var capsLock: Boolean = ???
}

@JSName("paper.PaperScope")
class PaperScope extends js.Object {
  var version: Double = ???
  var project: Project = ???
  var projects: js.Array[Project] = ???
  var view: View = ???
  var views: js.Array[View] = ???
  var tool: Tool = ???
  var tools: js.Array[Tool] = ???
  def install(scope: js.Any): js.Dynamic = ???
  def setup(canvas: HTMLCanvasElement): js.Dynamic = ???
  def setup(canvas: String): js.Dynamic = ???
  def clear(): js.Dynamic = ???
  def remove(): js.Dynamic = ???
}

@JSName("paper.PaperScope")
object PaperScope extends js.Object {
  def get(id: String): PaperScope = ???
  def each(iter: js.Function0[Any]): js.Dynamic = ???
}

@JSName("paper.PaperScript")
class PaperScript extends js.Object {
}

@JSName("paper.PaperScript")
object PaperScript extends js.Object {
  def compile(code: String): String = ???
  def evaluate(code: String, scope: PaperScope): js.Any = ???
}

@JSName("paper.Point")
class Point protected () extends js.Object {
  def this(x: Double, y: Double) = this()
  def this(values: js.Array[js.Any]) = this()
  def this(`object`: js.Any) = this()
  def this(size: Size) = this()
  def this(point: Point) = this()
  var x: Double = ???
  var y: Double = ???
  var length: Double = ???
  var angle: Double = ???
  var angleInRadians: Double = ???
  var quadrant: Double = ???
  var selected: Boolean = ???
  def add(point: Point): Point = ???
  def add(x: Double, y: Double): Point = ???
  def add(value: Double): Point = ???
  def subtract(point: Point): Point = ???
  def subtract(x: Double, y: Double): Point = ???
  def subtract(value: Double): Point = ???
  def multiply(point: Point): Point = ???
  def multiply(x: Double, y: Double): Point = ???
  def multiply(value: Double): Point = ???
  def divide(point: Point): Point = ???
  def divide(x: Double, y: Double): Point = ???
  def divide(value: Double): Point = ???
  @JSName("clone") def clonePoint(insert: Boolean = ???): Point = ???
  override def toString(): String = ???
  def transform(matrix: Matrix): Point = ???
  def getDistance(point: Point, squared: Boolean): Double = ???
  def normalize(length: Double = ???): js.Dynamic = ???
  def getAngle(): Double = ???
  def getAngleInRadians(): Double = ???
  def getDirectedAngle(point: Point): Double = ???
  def rotate(angle: Double, center: Point): Point = ???
  def isInside(rect: Rectangle): Boolean = ???
  def isClose(point: Point, tolerance: Double): Boolean = ???
  def isColinear(point: Point): Boolean = ???
  def isOrthogonal(point: Point): Boolean = ???
  def isZero(): Boolean = ???
  def isNan(): Boolean = ???
  def dot(point: Point): Double = ???
  def cross(point: Point): Double = ???
  def project(point: Point): Point = ???
}

@JSName("paper.Point")
object Points extends js.Object {
  def min(point1: Point, point2: Point): Point = ???
  def max(point1: Point, point2: Point): Point = ???
}

object Point {
  def apply(x: Double, y: Double): Point = new Point(x, y)
}

@JSName("paper.Size")
class Size extends js.Object {
  def this(width: Double, height: Double) = this()
  var width: Double = ???
  var height: Double = ???
  override def toString(): String = ???
  @JSName("clone") def cloneSize(insert: Boolean = ???): Size = ???
  def isZero(): Boolean = ???
  def isNan(): Boolean = ???
  def round(): Size = ???
  def ceil(): Size = ???
  def floor(): Size = ???
  def abs(): Size = ???
}

@JSName("paper.Size")
object Size extends js.Object {
  def min(size1: Size, size2: Size): Size = ???
  def max(size1: Size, size2: Size): Size = ???
  def random(): Size = ???
}

@JSName("paper.Rectangle")
class Rectangle protected () extends js.Object {
  def this(point: Point, size: Size) = this()
  def this(x: Double, y: Double, width: Double, height: Double) = this()
  def this(point1: Point, point2: Point) = this()
  def this(rect: Rectangle) = this()
  var x: Double = ???
  var y: Double = ???
  var width: Double = ???
  var height: Double = ???
  var point: Point = ???
  var size: Size = ???
  var left: Double = ???
  var top: Double = ???
  var right: Double = ???
  var bottom: Double = ???
  var center: Point = ???
  var topLeft: Point = ???
  var topRight: Point = ???
  var bottomLeft: Point = ???
  var bottomRight: Point = ???
  var leftCenter: Point = ???
  var topCenter: Point = ???
  var rightCenter: Point = ???
  var bottomCenter: Point = ???
  def isEmpty(): Boolean = ???
  override def toString(): String = ???
  def contains(point: Point): Boolean = ???
  def contains(rect: Rectangle): Boolean = ???
  def intersects(rect: Rectangle): Boolean = ???
  def intersect(rect: Rectangle): Rectangle = ???
  def unite(rect: Rectangle): Rectangle = ???
  def include(point: Point): js.Dynamic = ???
}

@JSName("paper.Matrix")
class Matrix protected () extends js.Object {
  def this(a: Double, c: Double, b: Double, d: Double, tx: Double, ty: Double) = this()
  var scaleX: Double = ???
  var scaleY: Double = ???
  var shearX: Double = ???
  var shearY: Double = ???
  var translateX: Double = ???
  var translateY: Double = ???
  var values: js.Array[Double] = ???
  var rotation: Double = ???
  @JSName("clone") def cloneMatrix(insert: Boolean = ???): Matrix = ???
  def set(a: Double, c: Double, b: Double, d: Double, tx: Double, ty: Double): Matrix = ???
  def scale(scale: Double, center: Point = ???): Matrix = ???
  @JSName("scale") def scale2(hor: Double, ver: Double, center: Point = ???): Matrix = ???
  def translate(point: Point): Matrix = ???
  def translate(dx: Double, dy: Double): Matrix = ???
  def rotate(angle: Double, center: Point): Matrix = ???
  def rotate(angle: Double, x: Double, y: Double): Matrix = ???
  def shear(point: Point, center: Point = ???): Matrix = ???
  @JSName("shear") def shear2(hor: Double, ver: Double, center: Point = ???): Matrix = ???
  override def toString(): String = ???
  def concatenate(mx: Matrix): Matrix = ???
  def preConcatenate(mx: Matrix): Matrix = ???
  def transform(point: Point): Matrix = ???
  def transform(src: js.Array[Double], srcOff: Double, dst: js.Array[Double], dstOff: Double, numPts: Double): js.Array[Double] = ???
  def inverseTransform(point: Point): Matrix = ???
  def isIdentity(): Boolean = ???
  def isInvertible(): Boolean = ???
  def isSingular(): Boolean = ???
  def createInverse(): Matrix = ???
  def setToScale(hor: Double, ver: Double): Matrix = ???
  def setToTranslation(dx: Double, dy: Double): Matrix = ???
  def setToShear(hor: Double, ver: Double): Matrix = ???
  def setToRotation(angle: Double, x: Double, y: Double): Matrix = ???
  def applyToContext(ctx: js.Any, reset: Boolean = ???): js.Dynamic = ???
}

@JSName("paper.Matrix")
object Matrix extends js.Object {
  def getScaleInstance(hor: Double, ver: Double): Matrix = ???
  def getTranslateInstance(dx: Double, dy: Double): Matrix = ???
  def getShearInstance(hor: Double, ver: Double, center: Point): Matrix = ???
  def getRotateInstance(angle: Double, x: Double, y: Double): Matrix = ???
}

@JSName("paper.Item")
class Item extends js.Object {
  var id: Double = ???
  var name: js.UndefOr[String] = ???
  var position: Point = ???
  var rotation: Double = ???
  var applyMatrix: Boolean = ???
  var style: PathStyle = ???
  var visible: Boolean = ???
  var blendMode: String = ???
  var opacity: Double = ???
  var guide: Double = ???
  var selected: Boolean = ???
  var clipMask: Boolean = ???
  var project: Project = ???
  var layer: Layer = ???
  var parent: Item = ???
  var children: js.Array[Item] = ???
  var firstChild: Item = ???
  var lastChild: Item = ???
  var nextSibling: Item = ???
  var previousSibling: Item = ???
  var index: Double = ???
  var bounds: Rectangle = ???
  var strokeBounds: Rectangle = ???
  var handleBounds: Rectangle = ???
  var strokeColor: Color = ???
  var strokeWidth: Double = ???
  var strokeCap: String = ???
  var strokeJoin: String = ???
  var dashOffset: Double = ???
  var dashArray: js.Array[Double] = ???
  var miterLimit: Double = ???
  var fillColor: Color = ???
  @JSName("fillColor") var fillColorString: String = ???
  def addChild(item: Item): js.Dynamic = ???
  def insertChild(index: js.Any, item: Item): js.Dynamic = ???
  def addChildren(items: js.Array[Item]): js.Dynamic = ???
  def insertChildren(index: js.Any, items: js.Array[Item]): js.Dynamic = ???
  def insertAbove(item: Item): Boolean = ???
  def insertBelow(item: Item): Boolean = ???
  def remove(): Boolean = ???
  def removeChildren(): js.Array[Item] = ???
  def removeChildren(from: Double, to: Double = ???): js.Array[Item] = ???
  def reverseChildren(): js.Dynamic = ???
  def hasChildren(): Boolean = ???
  def isAbove(item: Item): Boolean = ???
  def isBelow(item: Item): Boolean = ???
  def isParent(item: Item): Boolean = ???
  def isChild(item: Item): Boolean = ???
  def isDescendant(item: Item): Boolean = ???
  def isAncestor(item: Item): Boolean = ???
  def isGroupedWith(item: Item): Boolean = ???
  def contains(point: Point): Boolean = ???
  def matches(data: js.Dynamic): Boolean = ???
  def getItems(data: js.Dynamic): js.Array[Item] = ???
  def getItem(data: js.Dynamic): Item = ???
  def scale(scale: Double, center: Point): js.Dynamic = ???
  def scale(hor: Double, ver: Double, center: Point = ???): js.Dynamic = ???
  def translate(delta: Double): js.Dynamic = ???
  def rotate(angle: Double, center: Point = ???): js.Dynamic = ???
  def shear(point: Point, center: Point): js.Dynamic = ???
  def shear(hor: Double, ver: Double, center: Point): js.Dynamic = ???
  def transform(matrix: Matrix, flags: js.Array[js.Any] = ???): js.Dynamic = ???
  def fitBounds(rectangle: Rectangle, fill: Boolean = ???): js.Dynamic = ???
  def removeOn(`object`: js.Any): js.Dynamic = ???
  def removeOnMove(): js.Dynamic = ???
  def removeOnDown(): js.Dynamic = ???
  def removeOnDrag(): js.Dynamic = ???
  def removeOnUp(): js.Dynamic = ???
  @JSName("clone") def cloneItem(insert: Boolean = ???): Item = ???
  def exportSVG(options: js.Dynamic = ???): SVGElement = ???
  def importSVG(svg: SVGElement): Item = ???
  def exportJSON(options: js.Dynamic = ???): String = ???
  def importJSON(json: String): Item = ???

  def globalToLocal(p: Point): Point = ???
  def localToGlobal(p: Point): Point = ???

  var onFrame: js.Function1[FrameEvent, _] = ???
  var onMouseDown: js.Function1[PaperMouseEvent, _] = ???
  var onMouseUp: js.Function1[PaperMouseEvent, _] = ???
  var onClick: js.Function1[PaperMouseEvent, _] = ???
  var onDoubleClick: js.Function1[PaperMouseEvent, _] = ???
  var onMouseMove: js.Function1[PaperMouseEvent, _] = ???
  var onMouseEnter: js.Function1[PaperMouseEvent, _] = ???
  var onMouseLeave: js.Function1[PaperMouseEvent, _] = ???
}

trait FrameEvent extends js.Object {
  def time: Double
  def delta: Double
  def count: Int
}

trait PaperMouseEvent extends js.Object {
  def `type`: String
  def point: Point
  def target: Item
  def delta: Point
}

@JSName("paper.Group")
class Group protected () extends Item {
  def this(children: js.Array[Item]) = this()
  var clipped: Boolean = ???
}

@JSName("paper.Layer")
class Layer extends Group {
  def this(children: js.Array[Item]) = this()
  def activate(): js.Dynamic = ???
}

@JSName("paper.PlacedItem")
class PlacedItem extends Item {
  var matrix: Matrix = ???
}

@JSName("paper.Raster")
class Raster protected () extends PlacedItem {
  def this(imageId: String) = this()
  var size: Size = ???
  var width: Double = ???
  var height: Double = ???
  var ppi: Double = ???
  var image: HTMLImageElement = ???
  def getSubImage(rect: Rectangle): HTMLCanvasElement = ???
  def drawImage(image: HTMLImageElement, point: Point): js.Dynamic = ???
  def getAverageColor(path: Path): Color = ???
  def getAverageColor(rect: Rectangle): Color = ???
  def getAverageColor(point: Point): Color = ???
  def getPixel(x: Double, y: Double): RgbColor = ???
  def getPixel(point: Point): RgbColor = ???
  def setPixel(x: Double, y: Double, color: Color): js.Dynamic = ???
  def setPixel(point: Point, color: Color): js.Dynamic = ???
  def createData(size: Size): js.Any = ???
  def getData(rect: Rectangle): js.Any = ???
  def setData(data: js.Any, point: Point): js.Any = ???
}

@JSName("paper.PlacedSymbol")
class PlacedSymbol protected () extends PlacedItem {
  def this(symbol: Symbol, matrix: Matrix) = this()
  def this(symbol: Symbol, point: Point = ???) = this()
  var symbol: Symbol = ???
}

@JSName("paper.HitResult")
class HitResult extends js.Object {
  var `type`: String = ???
  var name: String = ???
  var item: Item = ???
  var location: CurveLocation = ???
  var segment: Segment = ???
  var point: Point = ???
}

@JSName("paper.PathItem")
class PathItem extends Item {
  def smooth(): js.Dynamic = ???
  def moveTo(point: Point): js.Dynamic = ???
  def moveTo(x: Double, y: Double): js.Dynamic = ???
  def lineTo(point: Point): js.Dynamic = ???
  def lineTo(x: Double, y: Double): js.Dynamic = ???
  def cubicCurveTo(handle1: Point, handle2: Point, to: Point): js.Dynamic = ???
  def quadraticCurveTo(handle: Point, to: Point): js.Dynamic = ???
  def curveTo(through: Point, to: Point, parameter: js.Any = ???): js.Dynamic = ???
  def arcTo(through: Point, to: Point): js.Dynamic = ???
  def arcTo(to: Point, clockwise: Boolean = ???): js.Dynamic = ???
  def closePath(): js.Dynamic = ???
  def moveBy(point: Point): js.Dynamic = ???
  def moveBy(x: Double, y: Double): js.Dynamic = ???
  def lineBy(vector: Point): js.Dynamic = ???
  def lineBy(x: Double, y: Double): js.Dynamic = ???
  def curveBy(throughVector: Point, toVector: Point, parameter: js.Any = ???): js.Dynamic = ???
  def arcBy(throughVector: Point, toVector: Point): js.Dynamic = ???
}

@JSName("paper.Path")
class Path extends PathItem {
  def this(segments: js.Array[Segment]) = this()
  def this(pathData: String) = this()
  //def this(points: js.Array[Point]) = this()
  var segments: js.Array[Segment] = ???
  var firstSegment: Segment = ???
  var lastSegment: Segment = ???
  var curves: js.Array[Curve] = ???
  var firstCurve: Curve = ???
  var lastCurve: Curve = ???
  var closed: Boolean = ???
  var fullySelected: Boolean = ???
  var clockwise: Boolean = ???
  var length: Double = ???
  def add(segment: Segment): Segment = ???
  def insert(index: Double, segment: Segment): Segment = ???
  def addSegments(segments: js.Array[Segment]): js.Array[Segment] = ???
  def insertSegments(index: Double, segments: js.Array[Segment]): js.Array[Segment] = ???
  def removeSegment(index: Double): Segment = ???
  def removeSegments(): js.Array[Segment] = ???
  def removeSegments(from: Double, to: Double = ???): js.Array[Segment] = ???
  def flatten(maxDistance: Double): js.Dynamic = ???
  def simplify(tolerance: Double = ???): js.Dynamic = ???
  def reverse(): js.Dynamic = ???
  def join(path: Path): js.Dynamic = ???
  def getLocationAt(offset: Double, isParameter: Boolean = ???): CurveLocation = ???
  def getPointAt(offset: Double, isParameter: Boolean = ???): Point = ???
  def getTangentAt(offset: Double, isParameter: Boolean = ???): Point = ???
  def getNormalAt(offset: Double, isParameter: Boolean = ???): Point = ???
  def getNearestLocation(point: Point): CurveLocation = ???
  def getNearestPoint(point: Point): Point = ???
  @JSName("clone") def clonePath(insert: Boolean = ???): Path = ???
}

@JSName("paper.Path")
object Path extends js.Object {
  def Line(pt1: Point, pt2: Point): Path = ???
  def Rectangle(point: Point, size: js.Any): Path = ???
  def Rectangle(point1: Point, point2: Point): Path = ???
  def Rectangle(rect: Rectangle): Path = ???
  def RoundRectangle(rect: Rectangle, size: Size): Path = ???
  def Oval(rect: Rectangle, circumscribed: Boolean = ???): Path = ???
  def Circle(center: Point, radius: Double): Path = ???
  def Arc(from: Point, through: Point, to: Point): Path = ???
  def RegularPolygon(center: Point, numSides: Double, radius: Double): Path = ???
  def Star(center: Point, numPoints: Double, radius1: Double, radius2: Double): Path = ???
}

@JSName("paper.CompoundPath")
class CompoundPath protected () extends PathItem {
  def this(paths: js.Array[Path]) = this()
  def simplify(): js.Dynamic = ???
}

@JSName("paper.Segment")
class Segment protected () extends js.Object {
  def this(point: Point = ???, handleIn: Point = ???, handleOut: Point = ???) = this()
  var point: Point = ???
  var handleIn: Point = ???
  var handleOut: Point = ???
  var selected: Boolean = ???
  var index: Double = ???
  var path: Path = ???
  var curve: Curve = ???
  var next: Segment = ???
  var previous: Segment = ???
  def reverse(): Segment = ???
  def remove(): js.Dynamic = ???
  override def toString(): String = ???
}

@JSName("paper.SegmentPoint")
class SegmentPoint extends Point {
  def set(x: Double, y: Double): SegmentPoint = ???
  def getX(): Double = ???
  def setX(x: Double): js.Dynamic = ???
  def getY(): Double = ???
  def setY(y: Double): js.Dynamic = ???
  override def isZero(): Boolean = ???
  def setSelected(selected: Boolean): js.Dynamic = ???
  def isSelected(): Boolean = ???
}

@JSName("paper.Curve")
class Curve protected () extends js.Object {
  def this(segment1: Segment, segment2: Segment) = this()
  var point1: Point = ???
  var point2: Point = ???
  var handle1: Point = ???
  var handle2: Point = ???
  var segment1: Segment = ???
  var segment2: Segment = ???
  var path: Path = ???
  var index: Double = ???
  var next: Curve = ???
  var previous: Curve = ???
  var selected: Boolean = ???
  var length: Double = ???
  def isLinear(): Boolean = ???
  def getParameterAt(offset: Double, start: Double = ???): Double = ???
  def getPoint(parameter: Double): Point = ???
  def getTangent(parameter: Double): Point = ???
  def getNormal(parameter: Double): Point = ???
  def getParameter(point: Point): Double = ???
  def reverse(): Curve = ???
  @JSName("clone") def cloneCurve(insert: Boolean = ???): Curve = ???
  override def toString(): String = ???
}

@JSName("paper.PathStyle")
class PathStyle extends js.Object {
  var strokeColor: js.Any = ???
  var strokeWidth: Double = ???
  var strokeCap: String = ???
  var strokeJoin: String = ???
  var dashOffset: Double = ???
  var dashArray: js.Array[Double] = ???
  var miterLimit: Double = ???
  var fillColor: Color = ???
}

@JSName("paper.CurveLocation")
class CurveLocation protected () extends js.Object {
  def this(curve: Curve, parameter: Double, point: Point, distance: Double) = this()
  var segment: Segment = ???
  var curve: Curve = ???
  var path: Path = ???
  var index: Double = ???
  var offset: Double = ???
  var curveOffset: Double = ???
  var parameter: Double = ???
  var point: Point = ???
  var tangent: Point = ???
  var normal: Point = ???
  var distance: Double = ???
  override def toString(): String = ???
}

@JSName("paper.Project")
class Project extends js.Object {
  var currentStyle: PathStyle = ???
  var index: Double = ???
  var selectedItems: js.Array[Item] = ???
  var layers: js.Array[Layer] = ???
  var activeLayer: Layer = ???
  var symbols: js.Array[Symbol] = ???
  var views: js.Array[View] = ???
  var activeView: View = ???
  def activate(): js.Dynamic = ???
  def remove(): js.Dynamic = ???
  def selectAll(): js.Dynamic = ???
  def deselectAll(): js.Dynamic = ???
  def hitTest(point: Point, options: js.Any = ???): HitResult = ???
  def exportSVG(options: js.Dynamic = ???): SVGElement = ???
}

@JSName("paper.Symbol")
class Symbol protected () extends js.Object {
  def this(item: Item) = this()
  var project: Project = ???
  var definition: Item = ???
  def place(position: Point = ???): PlacedSymbol = ???
  @JSName("clone") def cloneSymbol(insert: Boolean = ???): Symbol = ???
}

@JSName("paper.Color")
class Color extends js.Object {
  def this(s: String) = this()
  def this(r: Double, g: Double, b: Double) = this()
  def this(r: Double, g: Double, b: Double, a: Double) = this()
  var `type`: String = ???
  var alpha: Double = ???
  var red: Double = ???
  var green: Double = ???
  var blue: Double = ???
  var gray: Double = ???
  var hue: Double = ???
  var saturation: Double = ???
  var brightness: Double = ???
  var lightness: Double = ???
  @JSName("clone") def cloneColor(insert: Boolean = ???): Color = ???
  def hasAlpha(): Boolean = ???
  override def toString(): String = ???
  def toCssString(): String = ???
}

@JSName("paper.GrayColor")
class GrayColor protected () extends Color {
  def this(gray: Double, alpha: Double = ???) = this()
}

@JSName("paper.RgbColor")
class RgbColor protected () extends Color {
  def this(red: Double, green: Double, blue: Double, alpha: Double = ???) = this()
}

@JSName("paper.HsbColor")
class HsbColor protected () extends Color {
  def this(hue: Double, saturation: Double, brightness: Double, alpha: Double = ???) = this()
}

@JSName("paper.HlsColor")
class HlsColor protected () extends Color {
  def this(hue: Double, saturation: Double, lightness: Double, alpha: Double = ???) = this()
}

@JSName("paper.GradientColor")
class GradientColor protected () extends Color {
  def this(gradient: Gradient, origin: Point, destination: Point, hilight: Boolean) = this()
  var origin: Point = ???
  var destination: Point = ???
  var hilite: Boolean = ???
  @JSName("clone") def cloneGradientColor(insert: Boolean = ???): GradientColor = ???
  def equals(color: GradientColor): Boolean = ???
  def transform(matrix: js.Any): js.Dynamic = ???
}

@JSName("paper.Gradient")
class Gradient protected () extends js.Object {
  def this(stops: js.Array[GradientStop], `type`: String = ???) = this()
  var stops: js.Array[GradientStop] = ???
  @JSName("clone") def cloneGradient(insert: Boolean = ???): Gradient = ???
}

@JSName("paper.GradientStop")
class GradientStop protected () extends js.Object {
  def this(color: Color = ???, rampPoint: Double = ???) = this()
  var rampPoint: Double = ???
  var color: Color = ???
  @JSName("clone") def cloneGradientStop(insert: Boolean = ???): GradientStop = ???
}

@JSName("paper.View")
class View protected () extends js.Object {
  def this(canvas: HTMLCanvasElement) = this()
  var canvas: HTMLCanvasElement = ???
  var viewSize: Size = ???
  var bounds: Rectangle = ???
  var size: Size = ???
  var center: Point = ???
  var zoom: Double = ???
  def activate(): js.Dynamic = ???
  def remove(): js.Dynamic = ???
  def isVisible(): Boolean = ???
  def scrollBy(point: Point): js.Dynamic = ???
  def scrollBy(x: Double, y: Double): js.Dynamic = ???
  def setViewSize(width: Double, height: Double): js.Dynamic = ???
  def draw(): js.Dynamic = ???
  var onFrame: js.Function1[js.Any, Any] = ???
  var onResize: js.Function1[js.Any, Any] = ???
  def viewToProject(point: Point): Point = ???
  def projectToView(point: Point): Point = ???
}

@JSName("paper.Tool")
class Tool extends js.Object {
  var eventInterval: Double = ???
  var minDistance: Double = ???
  var maxDistance: Double = ???
  var fixedDistance: Double = ???
  def activate(): js.Dynamic = ???
  def remove(): js.Dynamic = ???
  var onMouseDown: js.Function1[ToolEvent, _] = ???
  var onMouseDrag: js.Function1[ToolEvent, _] = ???
  var onMouseMove: js.Function1[ToolEvent, _] = ???
  var onMouseUp: js.Function1[ToolEvent, _] = ???
  var onKeyDown: js.Function1[ToolEvent, _] = ???
  var onKeyUp: js.Function1[ToolEvent, _] = ???
}

@JSName("paper.ToolEvent")
class ToolEvent extends js.Object {
  var event: MouseEvent = ???
  var `type`: String = ???
  var point: Point = ???
  var lastPoint: Point = ???
  var downPoint: Point = ???
  var middlePoint: Point = ???
  var delta: Point = ???
  var count: Double = ???
  var item: Item = ???
  override def toString(): String = ???
}

@JSName("paper.Key")
class Key extends js.Object {
}

@JSName("paper.Key")
object Key extends js.Object {
  def isDown(key: String): Boolean = ???
}

@JSName("paper.KeyEvent")
class KeyEvent extends js.Object {
  var `type`: String = ???
  var character: String = ???
  var key: String = ???
  override def toString(): String = ???
}

@JSName("paper.TextItem")
class TextItem extends Item {
  var content: String = ???
  var characterStyle: CharacterStyle = ???
  var paragraphStyle: ParagraphStyle = ???
  var fontSize: Double = ???
  var fontWeight: String = ???
}

@JSName("paper.PointText")
class PointText protected () extends TextItem {
  def this(point: Point) = this()
  var point: Point = ???
}

@JSName("paper.CharacterStyle")
class CharacterStyle extends js.Object {
  var font: String = ???
  var fontSize: Double = ???
}

@JSName("paper.ParagraphStyle")
class ParagraphStyle extends js.Object {
  var justification: String = ???
}

}