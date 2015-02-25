package importedjs.filereaderjs

import org.scalajs.dom._

import scalajs.js

object FileReaderJS extends js.Object {
  def enabled: Boolean = js.native
  def setupInput(element: html.Input, options: FileReaderOpts): Unit = js.native
  def setupDrop(node: Node, options: FileReaderOpts): Unit = js.native
  def setupClipboard(node: Node, options: FileReaderOpts): Unit = js.native
  def setSync(sync: Boolean): Unit = js.native
  def getSync(): Boolean = js.native
  def opts: FileReaderOpts = js.native
}

trait FileReaderOpts extends js.Object {
  var dragClass: String = js.native
  var accept: String = js.native
  var readAsMap: js.Object = js.native
  var readAsDefault: String = js.native
  var on: FileReaderEvents = js.native
}

trait FileReaderEvents extends js.Object {
  var beforestart: js.Function2[ProgressEvent, File, Boolean] = js.native
  var loadstart: js.Function2[ProgressEvent, File, _] = js.native
  var progress: js.Function2[ProgressEvent, File, _] = js.native
  var load: js.Function2[ProgressEvent, File, _] = js.native
  var error: js.Function2[ProgressEvent, File, _] = js.native
  var loadend: js.Function2[ProgressEvent, File, _] = js.native
  var abort: js.Function2[ProgressEvent, File, _] = js.native
  var skip: js.Function2[ProgressEvent, File, _] = js.native
  var groupstart: js.Function1[Group, _] = js.native
}

trait Group extends js.Object {
  def groupID: String = js.native
  def files: FileList = js.native
  def started: js.Date = js.native
}

object Opts {
  def load(load: js.Function2[ProgressEvent, File, _]): FileReaderOpts =
    js.Dynamic.literal(
      on = js.Dynamic.literal(load = load)
    ).asInstanceOf[FileReaderOpts]
}