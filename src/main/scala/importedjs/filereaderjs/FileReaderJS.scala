package importedjs.filereaderjs

import org.scalajs.dom._

import scalajs.js

object FileReaderJS extends js.Object {
  def enabled: Boolean = ???
  def setupInput(element: HTMLInputElement, options: FileReaderOpts): Unit = ???
  def setupDrop(node: Node, options: FileReaderOpts): Unit = ???
  def setupClipboard(node: Node, options: FileReaderOpts): Unit = ???
  def setSync(sync: Boolean): Unit = ???
  def getSync(): Boolean = ???
  def opts: FileReaderOpts = ???
}

trait FileReaderOpts extends js.Object {
  var dragClass: String
  var accept: String
  var readAsMap: js.Object
  var readAsDefault: String
  var on: FileReaderEvents
}

trait FileReaderEvents extends js.Object {
  var beforestart: js.Function2[ProgressEvent, File, Boolean]
  var loadstart: js.Function2[ProgressEvent, File, _]
  var progress: js.Function2[ProgressEvent, File, _]
  var load: js.Function2[ProgressEvent, File, _]
  var error: js.Function2[ProgressEvent, File, _]
  var loadend: js.Function2[ProgressEvent, File, _]
  var abort: js.Function2[ProgressEvent, File, _]
  var skip: js.Function2[ProgressEvent, File, _]
  var groupstart: js.Function1[Group, _]
}

trait Group extends js.Object {
  def groupID: String
  def files: FileList
  def started: js.Date
}

object Opts {
  def load(load: js.Function2[ProgressEvent, File, _]): FileReaderOpts =
    js.Dynamic.literal(
      on = js.Dynamic.literal(load = load)
    ).asInstanceOf[FileReaderOpts]
}