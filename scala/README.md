CAP
===

Type checker for CAP.

SBT >= 0.13.9 is required to compile and run the program.

Install dependencies and build with: `sbt compile`

Run tests with `sbt test`.

Examples of how to write expressions can be found in `src/test/scala/ar/uba/dc/cap/TypecheckingTest.scala`.

Launch an interactive interpreter with `sbt console`. It is recommended to execute `import ar.uba.dc.cap.dsl._` to include the DSL for CAP expressions.
