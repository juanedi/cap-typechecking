CAP
===

Type checker para CAP.

Se requiere Cabal (versión mínima 1.20) para compilar y ejecutar el programa. Para instalar dependencias y compilar:

```
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
```

Los tests se ejecutan con `cabal test`.

En el archivo `test/TypecheckerTest.hs` se encuentran ejemplos de cómo escribir expresiones.

Para ejecutar un intérprete interactivo: `cabal repl`.
