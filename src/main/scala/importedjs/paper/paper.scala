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

import org.scalajs.dom.{HTMLImageElement, SVGElement}

object Implicits {
  import scala.language.implicitConversions
  implicit def pointToSegment(p: Point): Segment = p.asInstanceOf[Segment]
}

trait KeyModifiers extends js.Object {
  var shift: js.Boolean = ???
  var control: js.Boolean = ???
  var option: js.Boolean = ???
  var command: js.Boolean = ???
  var capsLock: js.Boolean = ???
}

@JSName("paper.PaperScope")
class PaperScope extends js.Object {
  var version: js.Number = ???
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
  def get(id: js.String): PaperScope = ???
  def each(iter: js.Function0[Any]): js.Dynamic = ???
}

@JSName("paper.PaperScript")
class PaperScript extends js.Object {
}

@JSName("paper.PaperScript")
object PaperScript extends js.Object {
  def compile(code: js.String): js.String = ???
  def evaluate(code: js.String, scope: PaperScope): js.Any = ???
}

@JSName("paper.Point")
class Point protected () extends js.Object {
  def this(x: js.Number, y: js.Number) = this()
  def this(values: js.Array[js.Any]) = this()
  def this(`object`: js.Any) = this()
  def this(size: Size) = this()
  def this(point: Point) = this()
  var x: js.Number = ???
  var y: js.Number = ???
  var length: js.Number = ???
  var angle: js.Number = ???
  var angleInRadians: js.Number = ???
  var quadrant: js.Number = ???
  var selected: js.Boolean = ???
  def add(point: Point): Point = ???
  def add(x: js.Number, y: js.Number): Point = ???
  def add(value: js.Number): Point = ???
  def subtract(point: Point): Point = ???
  def subtract(x: js.Number, y: js.Number): Point = ???
  def subtract(value: js.Number): Point = ???
  def multiply(point: Point): Point = ???
  def multiply(x: js.Number, y: js.Number): Point = ???
  def multiply(value: js.Number): Point = ???
  def divide(point: Point): Point = ???
  def divide(x: js.Number, y: js.Number): Point = ???
  def divide(value: js.Number): Point = ???
  @JSName("clone") def clonePoint(insert: Boolean = ???): Point = ???
  override def toString(): String = ???
  def transform(matrix: Matrix): Point = ???
  def getDistance(point: Point, squared: js.Boolean): js.Number = ???
  def normalize(length: js.Number = ???): js.Dynamic = ???
  def getAngle(): js.Number = ???
  def getAngleInRadians(): js.Number = ???
  def getDirectedAngle(point: Point): js.Number = ???
  def rotate(angle: js.Number, center: Point): Point = ???
  def isInside(rect: Rectangle): js.Boolean = ???
  def isClose(point: Point, tolerance: js.Number): js.Boolean = ???
  def isColinear(point: Point): js.Boolean = ???
  def isOrthogonal(point: Point): js.Boolean = ???
  def isZero(): js.Boolean = ???
  def isNan(): js.Boolean = ???
  def dot(point: Point): js.Number = ???
  def cross(point: Point): js.Number = ???
  def project(point: Point): Point = ???
}

@JSName("paper.Point")
object Points extends js.Object {
  def min(point1: Point, point2: Point): Point = ???
  def max(point1: Point, point2: Point): Point = ???
}

object Point {
  def apply(x: js.Number, y: js.Number): Point = new Point(x, y)
}

@JSName("paper.Size")
class Size extends js.Object {
  def this(width: js.Number, height: js.Number) = this()
  var width: js.Number = ???
  var height: js.Number = ???
  override def toString(): String = ???
  @JSName("clone") def cloneSize(insert: Boolean = ???): Size = ???
  def isZero(): js.Boolean = ???
  def isNan(): js.Boolean = ???
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
  def this(x: js.Number, y: js.Number, width: js.Number, height: js.Number) = this()
  def this(point1: Point, point2: Point) = this()
  def this(rect: Rectangle) = this()
  var x: js.Number = ???
  var y: js.Number = ???
  var width: js.Number = ???
  var height: js.Number = ???
  var point: Point = ???
  var size: Size = ???
  var left: js.Number = ???
  var top: js.Number = ???
  var right: js.Number = ???
  var bottom: js.Number = ???
  var center: Point = ???
  var topLeft: Point = ???
  var topRight: Point = ???
  var bottomLeft: Point = ???
  var bottomRight: Point = ???
  var leftCenter: Point = ???
  var topCenter: Point = ???
  var rightCenter: Point = ???
  var bottomCenter: Point = ???
  def isEmpty(): js.Boolean = ???
  override def toString(): String = ???
  def contains(point: Point): js.Boolean = ???
  def contains(rect: Rectangle): js.Boolean = ???
  def intersects(rect: Rectangle): js.Boolean = ???
  def intersect(rect: Rectangle): Rectangle = ???
  def unite(rect: Rectangle): Rectangle = ???
  def include(point: Point): js.Dynamic = ???
}

@JSName("paper.Matrix")
class Matrix protected () extends js.Object {
  def this(a: js.Number, c: js.Number, b: js.Number, d: js.Number, tx: js.Number, ty: js.Number) = this()
  var scaleX: js.Number = ???
  var scaleY: js.Number = ???
  var shearX: js.Number = ???
  var shearY: js.Number = ???
  var translateX: js.Number = ???
  var translateY: js.Number = ???
  var values: js.Array[js.Number] = ???
  var rotation: js.Number = ???
  @JSName("clone") def cloneMatrix(insert: Boolean = ???): Matrix = ???
  def set(a: js.Number, c: js.Number, b: js.Number, d: js.Number, tx: js.Number, ty: js.Number): Matrix = ???
  def scale(scale: js.Number, center: Point = ???): Matrix = ???
  @JSName("scale") def scale2(hor: js.Number, ver: js.Number, center: Point = ???): Matrix = ???
  def translate(point: Point): Matrix = ???
  def translate(dx: js.Number, dy: js.Number): Matrix = ???
  def rotate(angle: js.Number, center: Point): Matrix = ???
  def rotate(angle: js.Number, x: js.Number, y: js.Number): Matrix = ???
  def shear(point: Point, center: Point = ???): Matrix = ???
  @JSName("shear") def shear2(hor: js.Number, ver: js.Number, center: Point = ???): Matrix = ???
  override def toString(): String = ???
  def concatenate(mx: Matrix): Matrix = ???
  def preConcatenate(mx: Matrix): Matrix = ???
  def transform(point: Point): Matrix = ???
  def transform(src: js.Array[js.Number], srcOff: js.Number, dst: js.Array[js.Number], dstOff: js.Number, numPts: js.Number): js.Array[js.Number] = ???
  def inverseTransform(point: Point): Matrix = ???
  def isIdentity(): js.Boolean = ???
  def isInvertible(): js.Boolean = ???
  def isSingular(): js.Boolean = ???
  def createInverse(): Matrix = ???
  def setToScale(hor: js.Number, ver: js.Number): Matrix = ???
  def setToTranslation(dx: js.Number, dy: js.Number): Matrix = ???
  def setToShear(hor: js.Number, ver: js.Number): Matrix = ???
  def setToRotation(angle: js.Number, x: js.Number, y: js.Number): Matrix = ???
  def applyToContext(ctx: js.Any, reset: js.Boolean = ???): js.Dynamic = ???
}

@JSName("paper.Matrix")
object Matrix extends js.Object {
  def getScaleInstance(hor: js.Number, ver: js.Number): Matrix = ???
  def getTranslateInstance(dx: js.Number, dy: js.Number): Matrix = ???
  def getShearInstance(hor: js.Number, ver: js.Number, center: Point): Matrix = ???
  def getRotateInstance(angle: js.Number, x: js.Number, y: js.Number): Matrix = ???
}

@JSName("paper.Item")
class Item extends js.Object {
  var id: js.Number = ???
  var name: js.String = ???
  var position: Point = ???
  var style: PathStyle = ???
  var visible: js.Boolean = ???
  var blendMode: js.String = ???
  var opacity: js.Number = ???
  var guide: js.Number = ???
  var selected: js.Boolean = ???
  var clipMask: js.Boolean = ???
  var project: Project = ???
  var layer: Layer = ???
  var parent: Item = ???
  var children: js.Array[Item] = ???
  var firstChild: Item = ???
  var lastChild: Item = ???
  var nextSibling: Item = ???
  var previousSibling: Item = ???
  var index: js.Number = ???
  var bounds: Rectangle = ???
  var strokeBounds: Rectangle = ???
  var handleBounds: Rectangle = ???
  var strokeColor: js.Any = ???
  var strokeWidth: js.Number = ???
  var strokeCap: js.String = ???
  var strokeJoin: js.String = ???
  var dashOffset: js.Number = ???
  var dashArray: js.Array[js.Number] = ???
  var miterLimit: js.Number = ???
  var fillColor: Color = ???
  @JSName("fillColor") var fillColorString: js.String = ???
  def addChild(item: Item): js.Dynamic = ???
  def insertChild(index: js.Any, item: Item): js.Dynamic = ???
  def addChildren(items: js.Array[Item]): js.Dynamic = ???
  def insertChildren(index: js.Any, items: js.Array[Item]): js.Dynamic = ???
  def insertAbove(item: Item): js.Boolean = ???
  def insertBelow(item: Item): js.Boolean = ???
  def remove(): js.Boolean = ???
  def removeChildren(): js.Array[Item] = ???
  def removeChildren(from: js.Number, to: js.Number = ???): js.Array[Item] = ???
  def reverseChildren(): js.Dynamic = ???
  def hasChildren(): js.Boolean = ???
  def isAbove(item: Item): js.Boolean = ???
  def isBelow(item: Item): js.Boolean = ???
  def isParent(item: Item): js.Boolean = ???
  def isChild(item: Item): js.Boolean = ???
  def isDescendant(item: Item): js.Boolean = ???
  def isAncestor(item: Item): js.Boolean = ???
  def isGroupedWith(item: Item): js.Boolean = ???
  def contains(point: Point): js.Boolean = ???
  def matches(data: js.Dynamic): js.Boolean = ???
  def getItems(data: js.Dynamic): js.Array[Item] = ???
  def getItem(data: js.Dynamic): Item = ???
  def scale(scale: js.Number, center: Point): js.Dynamic = ???
  def scale(hor: js.Number, ver: js.Number, center: Point = ???): js.Dynamic = ???
  def translate(delta: js.Number): js.Dynamic = ???
  def rotate(angle: js.Number, center: Point): js.Dynamic = ???
  def shear(point: Point, center: Point): js.Dynamic = ???
  def shear(hor: js.Number, ver: js.Number, center: Point): js.Dynamic = ???
  def transform(matrix: Matrix, flags: js.Array[js.Any] = ???): js.Dynamic = ???
  def fitBounds(rectangle: Rectangle, fill: js.Boolean = ???): js.Dynamic = ???
  def removeOn(`object`: js.Any): js.Dynamic = ???
  def removeOnMove(): js.Dynamic = ???
  def removeOnDown(): js.Dynamic = ???
  def removeOnDrag(): js.Dynamic = ???
  def removeOnUp(): js.Dynamic = ???
  @JSName("clone") def cloneItem(insert: Boolean = ???): Item = ???
  def exportSVG(options: js.Dynamic = ???): SVGElement = ???
  def importSVG(svg: SVGElement): Item = ???
  def exportJSON(options: js.Dynamic = ???): js.String = ???
  def importJSON(json: js.String): Item = ???
}

@JSName("paper.Group")
class Group protected () extends Item {
  def this(children: js.Array[Item]) = this()
  var clipped: js.Boolean = ???
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
  def this(imageId: js.String) = this()
  var size: Size = ???
  var width: js.Number = ???
  var height: js.Number = ???
  var ppi: js.Number = ???
  var image: HTMLImageElement = ???
  def getSubImage(rect: Rectangle): HTMLCanvasElement = ???
  def drawImage(image: HTMLImageElement, point: Point): js.Dynamic = ???
  def getAverageColor(path: Path): Color = ???
  def getAverageColor(rect: Rectangle): Color = ???
  def getAverageColor(point: Point): Color = ???
  def getPixel(x: js.Number, y: js.Number): RgbColor = ???
  def getPixel(point: Point): RgbColor = ???
  def setPixel(x: js.Number, y: js.Number, color: Color): js.Dynamic = ???
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
  var `type`: js.String = ???
  var name: js.String = ???
  var item: Item = ???
  var location: CurveLocation = ???
  var segment: Segment = ???
  var point: Point = ???
}

@JSName("paper.PathItem")
class PathItem extends Item {
  def smooth(): js.Dynamic = ???
  def moveTo(point: Point): js.Dynamic = ???
  def moveTo(x: js.Number, y: js.Number): js.Dynamic = ???
  def lineTo(point: Point): js.Dynamic = ???
  def lineTo(x: js.Number, y: js.Number): js.Dynamic = ???
  def cubicCurveTo(handle1: Point, handle2: Point, to: Point): js.Dynamic = ???
  def quadraticCurveTo(handle: Point, to: Point): js.Dynamic = ???
  def curveTo(through: Point, to: Point, parameter: js.Any = ???): js.Dynamic = ???
  def arcTo(through: Point, to: Point): js.Dynamic = ???
  def arcTo(to: Point, clockwise: js.Boolean = ???): js.Dynamic = ???
  def closePath(): js.Dynamic = ???
  def moveBy(point: Point): js.Dynamic = ???
  def moveBy(x: js.Number, y: js.Number): js.Dynamic = ???
  def lineBy(vector: Point): js.Dynamic = ???
  def lineBy(x: js.Number, y: js.Number): js.Dynamic = ???
  def curveBy(throughVector: Point, toVector: Point, parameter: js.Any = ???): js.Dynamic = ???
  def arcBy(throughVector: Point, toVector: Point): js.Dynamic = ???
}

@JSName("paper.Path")
class Path extends PathItem {
  def this(segments: js.Array[Segment]) = this()
  //def this(points: js.Array[Point]) = this()
  var segments: js.Array[Segment] = ???
  var firstSegment: Segment = ???
  var lastSegment: Segment = ???
  var curves: js.Array[Curve] = ???
  var firstCurve: Curve = ???
  var lastCurve: Curve = ???
  var closed: js.Boolean = ???
  var fullySelected: js.Boolean = ???
  var clockwise: js.Boolean = ???
  var length: js.Number = ???
  def add(segment: Segment): Segment = ???
  def insert(index: js.Number, segment: Segment): Segment = ???
  def addSegments(segments: js.Array[Segment]): js.Array[Segment] = ???
  def insertSegments(index: js.Number, segments: js.Array[Segment]): js.Array[Segment] = ???
  def removeSegment(index: js.Number): Segment = ???
  def removeSegments(): js.Array[Segment] = ???
  def removeSegments(from: js.Number, to: js.Number = ???): js.Array[Segment] = ???
  def flatten(maxDistance: js.Number): js.Dynamic = ???
  def simplify(tolerance: js.Number = ???): js.Dynamic = ???
  def reverse(): js.Dynamic = ???
  def join(path: Path): js.Dynamic = ???
  def getLocationAt(offset: js.Number, isParameter: js.Boolean = ???): CurveLocation = ???
  def getPointAt(offset: js.Number, isParameter: js.Boolean = ???): Point = ???
  def getTangentAt(offset: js.Number, isParameter: js.Boolean = ???): Point = ???
  def getNormalAt(offset: js.Number, isParameter: js.Boolean = ???): Point = ???
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
  def Oval(rect: Rectangle, circumscribed: js.Boolean = ???): Path = ???
  def Circle(center: Point, radius: js.Number): Path = ???
  def Arc(from: Point, through: Point, to: Point): Path = ???
  def RegularPolygon(center: Point, numSides: js.Number, radius: js.Number): Path = ???
  def Star(center: Point, numPoints: js.Number, radius1: js.Number, radius2: js.Number): Path = ???
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
  var selected: js.Boolean = ???
  var index: js.Number = ???
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
  def set(x: js.Number, y: js.Number): SegmentPoint = ???
  def getX(): js.Number = ???
  def setX(x: js.Number): js.Dynamic = ???
  def getY(): js.Number = ???
  def setY(y: js.Number): js.Dynamic = ???
  override def isZero(): js.Boolean = ???
  def setSelected(selected: js.Boolean): js.Dynamic = ???
  def isSelected(): js.Boolean = ???
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
  var index: js.Number = ???
  var next: Curve = ???
  var previous: Curve = ???
  var selected: js.Boolean = ???
  var length: js.Number = ???
  def isLinear(): js.Boolean = ???
  def getParameterAt(offset: js.Number, start: js.Number = ???): js.Number = ???
  def getPoint(parameter: js.Number): Point = ???
  def getTangent(parameter: js.Number): Point = ???
  def getNormal(parameter: js.Number): Point = ???
  def getParameter(point: Point): js.Number = ???
  def reverse(): Curve = ???
  @JSName("clone") def cloneCurve(insert: Boolean = ???): Curve = ???
  override def toString(): String = ???
}

@JSName("paper.PathStyle")
class PathStyle extends js.Object {
  var strokeColor: js.Any = ???
  var strokeWidth: js.Number = ???
  var strokeCap: js.String = ???
  var strokeJoin: js.String = ???
  var dashOffset: js.Number = ???
  var dashArray: js.Array[js.Number] = ???
  var miterLimit: js.Number = ???
  var fillColor: Color = ???
}

@JSName("paper.CurveLocation")
class CurveLocation protected () extends js.Object {
  def this(curve: Curve, parameter: js.Number, point: Point, distance: js.Number) = this()
  var segment: Segment = ???
  var curve: Curve = ???
  var path: Path = ???
  var index: js.Number = ???
  var offset: js.Number = ???
  var curveOffset: js.Number = ???
  var parameter: js.Number = ???
  var point: Point = ???
  var tangent: Point = ???
  var normal: Point = ???
  var distance: js.Number = ???
  override def toString(): String = ???
}

@JSName("paper.Project")
class Project extends js.Object {
  var currentStyle: PathStyle = ???
  var index: js.Number = ???
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
  def this(s: js.String) = this()
  def this(r: js.Number, g: js.Number, b: js.Number) = this()
  def this(r: js.Number, g: js.Number, b: js.Number, a: js.Number) = this()
  var `type`: js.String = ???
  var alpha: js.Number = ???
  var red: js.Number = ???
  var green: js.Number = ???
  var blue: js.Number = ???
  var gray: js.Number = ???
  var hue: js.Number = ???
  var saturation: js.Number = ???
  var brightness: js.Number = ???
  var lightness: js.Number = ???
  @JSName("clone") def cloneColor(insert: Boolean = ???): Color = ???
  def hasAlpha(): js.Boolean = ???
  override def toString(): String = ???
  def toCssString(): js.String = ???
}

@JSName("paper.GrayColor")
class GrayColor protected () extends Color {
  def this(gray: js.Number, alpha: js.Number = ???) = this()
}

@JSName("paper.RgbColor")
class RgbColor protected () extends Color {
  def this(red: js.Number, green: js.Number, blue: js.Number, alpha: js.Number = ???) = this()
}

@JSName("paper.HsbColor")
class HsbColor protected () extends Color {
  def this(hue: js.Number, saturation: js.Number, brightness: js.Number, alpha: js.Number = ???) = this()
}

@JSName("paper.HlsColor")
class HlsColor protected () extends Color {
  def this(hue: js.Number, saturation: js.Number, lightness: js.Number, alpha: js.Number = ???) = this()
}

@JSName("paper.GradientColor")
class GradientColor protected () extends Color {
  def this(gradient: Gradient, origin: Point, destination: Point, hilight: js.Boolean) = this()
  var origin: Point = ???
  var destination: Point = ???
  var hilite: js.Boolean = ???
  @JSName("clone") def cloneGradientColor(insert: Boolean = ???): GradientColor = ???
  def equals(color: GradientColor): js.Boolean = ???
  def transform(matrix: js.Any): js.Dynamic = ???
}

@JSName("paper.Gradient")
class Gradient protected () extends js.Object {
  def this(stops: js.Array[GradientStop], `type`: js.String = ???) = this()
  var stops: js.Array[GradientStop] = ???
  @JSName("clone") def cloneGradient(insert: Boolean = ???): Gradient = ???
}

@JSName("paper.GradientStop")
class GradientStop protected () extends js.Object {
  def this(color: Color = ???, rampPoint: js.Number = ???) = this()
  var rampPoint: js.Number = ???
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
  var zoom: js.Number = ???
  def activate(): js.Dynamic = ???
  def remove(): js.Dynamic = ???
  def isVisible(): js.Boolean = ???
  def scrollBy(point: Point): js.Dynamic = ???
  def scrollBy(x: js.Number, y: js.Number): js.Dynamic = ???
  def setViewSize(width: js.Number, height: js.Number): js.Dynamic = ???
  def draw(): js.Dynamic = ???
  var onFrame: js.Function1[js.Any, Any] = ???
  var onResize: js.Function1[js.Any, Any] = ???
  def viewToProject(point: Point): Point = ???
  def projectToView(point: Point): Point = ???
}

@JSName("paper.Tool")
class Tool extends js.Object {
  var eventInterval: js.Number = ???
  var minDistance: js.Number = ???
  var maxDistance: js.Number = ???
  var fixedDistance: js.Number = ???
  def activate(): js.Dynamic = ???
  def remove(): js.Dynamic = ???
  var onMouseDown: js.Function1[ToolEvent, Unit] = ???
  var onMouseDrag: js.Function1[ToolEvent, Unit] = ???
  var onMouseMove: js.Function1[ToolEvent, Unit] = ???
  var onMouseUp: js.Function1[ToolEvent, Unit] = ???
  var onKeyDown: js.Function1[ToolEvent, Unit] = ???
  var onKeyUp: js.Function1[ToolEvent, Unit] = ???
}

@JSName("paper.ToolEvent")
class ToolEvent extends js.Object {
  var `type`: js.String = ???
  var point: Point = ???
  var lastPoint: Point = ???
  var downPoint: Point = ???
  var middlePoint: Point = ???
  var delta: Point = ???
  var count: js.Number = ???
  var item: Item = ???
  override def toString(): String = ???
}

@JSName("paper.Key")
class Key extends js.Object {
}

@JSName("paper.Key")
object Key extends js.Object {
  def isDown(key: js.String): js.Boolean = ???
}

@JSName("paper.KeyEvent")
class KeyEvent extends js.Object {
  var `type`: js.String = ???
  var character: js.String = ???
  var key: js.String = ???
  override def toString(): String = ???
}

@JSName("paper.TextItem")
class TextItem extends Item {
  var content: js.String = ???
  var characterStyle: CharacterStyle = ???
  var paragraphStyle: ParagraphStyle = ???
}

@JSName("paper.PointText")
class PointText protected () extends TextItem {
  def this(point: Point) = this()
  var point: Point = ???
}

@JSName("paper.CharacterStyle")
class CharacterStyle extends js.Object {
  var font: js.String = ???
  var fontSize: js.Number = ???
}

@JSName("paper.ParagraphStyle")
class ParagraphStyle extends js.Object {
  var justification: js.String = ???
}

}