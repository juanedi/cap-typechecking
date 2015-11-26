CAP
===

Type checker para CAP.

Se requiere SBT (versión mínima 0.13.9) para compilar y ejecutar el programa.

Para instalar dependencias y compilar: `sbt compile`

Los tests se ejecutan con `sbt test`.

En el archivo `src/test/scala/ar/uba/dc/cap/SubtypingTest.scala` se encuentran ejemplos de cómo escribir expresiones.

Para ejecutar un intérprete interactivo: `sbt console`. Se debe ejecutar `import ar.uba.dc.cap.dsl._` para poder utilizar el DSL de expresiones.
