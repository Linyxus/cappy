// Regression: classes extending stdlib traits with init bodies must
// link. dotty's Mixin phase renames a trait's `<init>` to `$init$` and
// preserves dotc's "constructor result type = enclosing class"
// convention. The Python encoder used to special-case only
// `isClassConstructor` (the narrow `<init>` test), so `$init$` flowed
// through with `result = <trait>`, while erasure-normalized call sites
// emitted `result = Unit`. Mismatch → unresolved instance method at
// link time. The fix patches both via `isConstructor` (broad test).

class MyIter extends Iterator[String] {
  private var n = 0
  def hasNext: Boolean = n < 3
  def next(): String =
    val s = s"item-$n"
    n += 1
    s
}

object Test:
  def main(args: Array[String]): Unit =
    val it = new MyIter
    while it.hasNext do println(it.next())
