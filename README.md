# Glycano
An editor for glycan structure diagrams (http://glycano.cs.uct.ac.za).

Uses [Scala.js](http://www.scala-js.org/) to compile to javascript, with bindings to [React](https://facebook.github.io/react/) provided through [scalajs-react](https://japgolly.github.io/scalajs-react/).

## Building
Requires [sbt](http://www.scala-sbt.org/).

#### Development
* `sbt`
* `~fastOptJS`
* Browse to `http://localhost:12345/core/target/scala-2.11/classes/index-fastopt.html`.
* Source changes detected by sbt will trigger recompilation; press enter to stop monitoring.

#### Fully Optimised
* `sbt`
* `fullOptJS`
* Browse to `http://localhost:12345/core/target/scala-2.11/classes/index-fullopt.html`.

#### Deploying
* `sbt`
* `fullOptJS`
* Copy `core/target/scala-2.11/glycanoweb-opt.js` to `core/target/scala-2.11/classes/js/glycanoweb-opt.js`.
* Use `core/target/scala-2.11/classes/index.html` (including `favicon.*` and all files in the sub-directories `css`, `fonts`, and `js`).
