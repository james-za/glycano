package importedjs.d3

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.dom._

trait Selectors extends js.Object {
  def select(selector: js.String): Selection = ???
  def select(element: EventTarget): Selection = ???
  def selectAll(selector: js.String): Selection = ???
  def selectAll(elements: js.Array[EventTarget]): Selection = ???
}

trait Event extends js.Object {
  var dx: js.Number = ???
  var dy: js.Number = ???
  var clientX: js.Number = ???
  var clientY: js.Number = ???
  var translate: js.Array[js.Number] = ???
  var scale: js.Number = ???
  var sourceEvent: Event = ???
  var x: js.Number = ???
  var y: js.Number = ???
  var keyCode: js.Number = ???
  var altKey: js.Any = ???
  var `type`: js.String = ???
}

trait Base extends Selectors {
  var behavior: behavior.Behavior = ???
  var event: Event = ???
  def ascending[T](a: T, b: T): js.Number = ???
  def descending[T](a: T, b: T): js.Number = ???
  def min[T, U](arr: js.Array[T], map: js.Function1[T, U]): U = ???
  def min[T](arr: js.Array[T]): T = ???
  def max[T, U](arr: js.Array[T], map: js.Function1[T, U]): U = ???
  def max[T](arr: js.Array[T]): T = ???
  def extent[T, U](arr: js.Array[T], map: js.Function1[T, U]): js.Array[U] = ???
  def extent[T](arr: js.Array[T]): js.Array[T] = ???
  def sum[T](arr: js.Array[T], map: js.Function1[T, js.Number]): js.Number = ???
  def sum(arr: js.Array[js.Number]): js.Number = ???
  def mean[T](arr: js.Array[T], map: js.Function1[T, js.Number]): js.Number = ???
  def mean(arr: js.Array[js.Number]): js.Number = ???
  def median[T](arr: js.Array[T], map: js.Function1[T, js.Number]): js.Number = ???
  def median(arr: js.Array[js.Number]): js.Number = ???
  var quantile: js.Function2[js.Array[js.Number], js.Number, js.Number] = ???
  def bisect[T](arr: js.Array[T], x: T, low: js.Number = ???, high: js.Number = ???): js.Number = ???
  def bisectLeft[T](arr: js.Array[T], x: T, low: js.Number = ???, high: js.Number = ???): js.Number = ???
  def bisectRight[T](arr: js.Array[T], x: T, low: js.Number = ???, high: js.Number = ???): js.Number = ???
  def bisector(accessor: js.Function2[js.Any, js.Number, Any]): js.Dynamic = ???
  def shuffle[T](arr: js.Array[T]): js.Array[T] = ???
  def permute(arr: js.Array[js.Any], indexes: js.Array[js.Any]): js.Array[js.Any] = ???
  def zip(arrs: js.Any*): js.Array[js.Any] = ???
  def transform(definition: js.String): js.Dynamic = ???
  def transpose(matrix: js.Array[js.Any]): js.Array[js.Any] = ???
  def keys(map: js.Any): js.Array[js.String] = ???
  def values(map: js.Array[js.Any]): js.Array[js.Any] = ???
  def entries(map: js.Any): js.Array[js.Any] = ???
  def merge(map: js.Any*): js.Array[js.Any] = ???
  def range(stop: js.Number, step: js.Number = ???): js.Array[js.Number] = ???
  def range(start: js.Number, stop: js.Number = ???, step: js.Number = ???): js.Array[js.Number] = ???
  def nest(): Nest = ???
  def xhr(url: js.String, callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def xhr(url: js.String, mime: js.String, callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def text(url: js.String, callback: js.Function1[js.String, Unit] = ???): Xhr = ???
  def text(url: js.String, mime: js.String, callback: js.Function1[js.String, Unit] = ???): Xhr = ???
  var json: js.Function2[js.String, js.Function2[js.Any, js.Any, Unit], Xhr] = ???
  def xml(url: js.String, callback: js.Function1[Document, Unit] = ???): Xhr = ???
  def xml(url: js.String, mime: js.String, callback: js.Function1[Document, Unit] = ???): Xhr = ???
  var html: js.Function2[js.String, js.Function1[DocumentFragment, Unit], Xhr] = ???
  var csv: Dsv = ???
  var tsv: Dsv = ???
  var time: time.Time = ???
  var scale: scale.ScaleBase = ???
  var interpolate: transition.BaseInterpolate = ???
  var interpolateNumber: transition.BaseInterpolate = ???
  var interpolateRound: transition.BaseInterpolate = ???
  var interpolateString: transition.BaseInterpolate = ???
  var interpolateRgb: transition.BaseInterpolate = ???
  var interpolateHsl: transition.BaseInterpolate = ???
  var interpolateHcl: transition.BaseInterpolate = ???
  var interpolateLab: transition.BaseInterpolate = ???
  var interpolateArray: transition.BaseInterpolate = ???
  var interpolateObject: transition.BaseInterpolate = ???
  var interpolateTransform: transition.BaseInterpolate = ???
  var interpolators: js.Array[transition.InterpolateFactory] = ???
  var layout: layout.Layout = ???
  var svg: svg.Svg = ???
  var random: Random = ???
  def format(specifier: js.String): js.Function1[js.Number, js.String] = ???
  def formatPrefix(value: js.Number, precision: js.Number = ???): MetricPrefix = ???
  var version: js.String = ???
  def selection(): Selection = ???
  var ns: js.Any = ???
  var ease: js.Function = ???
  def rgb(r: js.Number, g: js.Number, b: js.Number): color.RGBColor = ???
  def rgb(c: js.String): color.RGBColor = ???
  def hcl(h: js.Number, c: js.Number, l: js.Number): color.HCLColor = ???
  def hcl(c: js.String): color.HCLColor = ???
  def hsl(h: js.Number, s: js.Number, l: js.Number): color.HSLColor = ???
  def hsl(c: js.String): color.HSLColor = ???
  def lab(l: js.Number, a: js.Number, b: js.Number): color.LABColor = ???
  def lab(c: js.String): color.LABColor = ???
  var geo: geo.Geo = ???
  var geom: geom.Geom = ???
  def mouse(container: js.Any): js.Array[js.Number] = ???
  def touches(container: js.Any): js.Array[js.Array[js.Number]] = ???
  def functor[R, T](value: js.Function1[R, T]): js.Function1[R, T] = ???
  def functor[T](value: T): js.Function1[js.Any, T] = ???
  def map(`object`: js.Any = ???): Map = ???
  def set(array: js.Array[js.Any] = ???): Set = ???
  def dispatch(types: js.String*): Dispatch = ???
  def rebind(target: js.Any, source: js.Any, names: js.Any*): js.Dynamic = ???
  def requote(str: js.String): js.String = ???
  var timer: js.Any = ???
  def transition(): transition.Transition = ???
  def round(x: js.Number, n: js.Number): js.Number = ???
}

trait Dispatch extends js.Object {
  @JSBracketAccess
  def apply(event: js.String): js.Any = ???
  @JSBracketAccess
  def update(event: js.String, v: js.Any): Unit = ???
  def on(`type`: js.String): js.Dynamic = ???
  def on(`type`: js.String, listener: js.Any): js.Dynamic = ???
}

trait MetricPrefix extends js.Object {
  var scale: js.Function1[js.Number, js.Number] = ???
  var symbol: js.String = ???
}

trait Xhr extends js.Object {
  def header(name: js.String): js.String = ???
  def header(name: js.String, value: js.String): Xhr = ???
  def mimeType(): js.String = ???
  def mimeType(`type`: js.String): Xhr = ???
  def response(): js.Function1[XMLHttpRequest, Any] = ???
  def response(value: js.Function1[XMLHttpRequest, Any]): Xhr = ???
  def get(callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def post(callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def post(data: js.Any, callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def send(method: js.String, callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def send(method: js.String, data: js.Any, callback: js.Function1[XMLHttpRequest, Unit] = ???): Xhr = ???
  def abort(): Xhr = ???
  var on: js.Function2[js.String, js.Function2[js.Any, js.Number, Any], Xhr] = ???
}

trait Dsv extends js.Object {
  def apply(url: js.String, callback: js.Function2[js.Any, js.Array[js.Any], Unit] = ???): Xhr = ???
  def parse(string: js.String): js.Array[js.Any] = ???
  def parseRows(string: js.String, accessor: js.Function2[js.Array[js.Any], js.Number, Any]): js.Dynamic = ???
  def format(rows: js.Array[js.Any]): js.String = ???
}

trait Selection extends js.Array[js.Any] with Selectors {
  def attr(name: js.String): js.String = ???
  def attr(name: js.String, value: js.Any): Selection = ???
  def attr(name: js.String, valueFunction: js.Function2[js.Any, js.Number, Any]): Selection = ???
  def attr(attrValueMap: Object): Selection = ???
  def classed(name: js.String): js.String = ???
  def classed(name: js.String, value: js.Any): Selection = ???
  def classed(name: js.String, valueFunction: js.Function2[js.Any, js.Number, Any]): Selection = ???
  def style(name: js.String): js.String = ???
  def style(name: js.String, value: js.Any, priority: js.String = ???): Selection = ???
  def style(name: js.String, valueFunction: js.Function2[js.Any, js.Number, Any], priority: js.String = ???): Selection = ???
  def style(styleValueMap: Object): Selection = ???
  def property(name: js.String): Unit = ???
  def property(name: js.String, value: js.Any): Selection = ???
  def property(name: js.String, valueFunction: js.Function2[js.Any, js.Number, Any]): Selection = ???
  def property(propertyValueMap: Object): Selection = ???
  def text(): js.String = ???
  def text(value: js.Any): Selection = ???
  def text(valueFunction: js.Function2[js.Any, js.Number, Any]): Selection = ???
  def html(): js.String = ???
  def html(value: js.Any): Selection = ???
  def html(valueFunction: js.Function2[js.Any, js.Number, Any]): Selection = ???
  var append: js.Function1[js.String, Selection] = ???
  var insert: js.Function2[js.String, js.String, Selection] = ???
  var remove: js.Function0[Selection] = ???
  var empty: js.Function0[js.Boolean] = ???
  def data(values: js.Function2[js.Any, js.Number, js.Array[js.Any]], key: js.Function2[js.Any, js.Number, Any] = ???): UpdateSelection = ???
  def data(): js.Array[js.Any] = ???
  def datum(values: js.Function2[js.Any, js.Number, Any]): UpdateSelection = ???
  def datum(): js.Dynamic = ???
  def filter(filter: js.Function2[js.Any, js.Number, js.Boolean], thisArg: js.Any = ???): UpdateSelection = ???
  def call(callback: js.Function, args: js.Any*): Selection = ???
  def each(eachFunction: js.Function2[js.Any, js.Number, Any]): Selection = ???
  def on(`type`: js.String): js.Function2[js.Any, js.Number, Any] = ???
  def on(`type`: js.String, listener: js.Function2[js.Any, js.Number, Any], capture: js.Boolean = ???): Selection = ???
  def size(): js.Number = ???
  def transition(): transition.Transition = ???
  def sort[T](comparator: js.Function2[T, T, js.Number] = ???): Selection = ???
  var order: js.Function0[Selection] = ???
  var node: js.Function0[Element] = ???
}

trait EnterSelection extends js.Object {
  var append: js.Function1[js.String, Selection] = ???
  var insert: js.Function2[js.String, js.String, Selection] = ???
  var select: js.Function1[js.String, Selection] = ???
  var empty: js.Function0[js.Boolean] = ???
  var node: js.Function0[Element] = ???
  var call: js.Function1[js.Function1[EnterSelection, Unit], EnterSelection] = ???
  var size: js.Function0[js.Number] = ???
}

trait UpdateSelection extends Selection {
  var enter: js.Function0[EnterSelection] = ???
  var update: js.Function0[Selection] = ???
  var exit: js.Function0[Selection] = ???
}

trait NestKeyValue extends js.Object {
  var key: js.String = ???
  var values: js.Any = ???
}

trait Nest extends js.Object {
  def key(keyFunction: js.Function2[js.Any, js.Number, js.String]): Nest = ???
  def sortKeys(comparator: js.Function2[js.Any, js.Any, js.Number]): Nest = ???
  def sortValues(comparator: js.Function2[js.Any, js.Any, js.Number]): Nest = ???
  def rollup(rollupFunction: js.Function2[js.Any, js.Number, Any]): Nest = ???
  def map(values: js.Array[js.Any]): js.Dynamic = ???
  def entries(values: js.Array[js.Any]): js.Array[NestKeyValue] = ???
}

trait Map extends js.Object {
  def has(key: js.String): js.Boolean = ???
  def get(key: js.String): js.Dynamic = ???
  def set[T](key: js.String, value: T): T = ???
  def remove(key: js.String): js.Boolean = ???
  def keys(): js.Array[js.String] = ???
  def values(): js.Array[js.Any] = ???
  def entries(): js.Array[js.Any] = ???
  def forEach(func: js.Function2[js.String, js.Any, Unit]): Unit = ???
}

trait Set extends js.Object {
  def has(value: js.Any): js.Boolean = ???
  def add(value: js.Any): js.Dynamic = ???
  def remove(value: js.Any): js.Boolean = ???
  def values(): js.Array[js.Any] = ???
  def forEach(func: js.Function1[js.Any, Unit]): Unit = ???
}

trait Random extends js.Object {
  def normal(mean: js.Number = ???, deviation: js.Number = ???): js.Function0[js.Number] = ???
  def logNormal(mean: js.Number = ???, deviation: js.Number = ???): js.Function0[js.Number] = ???
  def irwinHall(count: js.Number): js.Function0[js.Number] = ???
}