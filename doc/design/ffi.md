# Interacting with WASM Host

This is to define a way Cavia can declare imported functions from the WASM host and external types. For instance,
```scala
@extern("host", "__putchar")
def putChar(ch: char): Unit = ???
```
This will make `putChar` a primitive operation. The following declaration is equivalent:
```scala
@extern("host", "__putchar")
val putChar: char -> Unit = ???
```
The RHS is always ignored in `extern` definitions.

> Shall we relax a syntax to allow RHS-less value definitions?

The following declares an external type:
```scala
@extern
type Document
```
The type `Document` becomes an opaque type in a language. It is then possible to declare host environment APIs with these opaque types:
```scala
@extern
type Element

@extern("host", "__get_element_by_id")
def getElementById(doc: Document): Element = ???
```

During code generation, `@extern` value definitions generate `import` statements in the output WASM module. It is up to the host to ensure that the declared (and thus imported) primitives are provided when instantiating the WASM module. All `@extern` types are erased to `externref`.

