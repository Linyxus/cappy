// Regression guard for the PyFieldName owner-aware mangling fix.
// Without owner-mangling, `Derived.tag` (public ctor val) and `Base.tag`
// (private val with no accessor) collide on the same Python `self.tag`
// slot. JVM-style ctor chaining (subclass body then super ctor) lets
// the parent's field initializer overwrite the subclass-set value with
// `"base"`, breaking `d.baseTag` lookups. The fix routes the truly-
// private parent field through an owner-mangled storage slot so the
// two never alias.
class Base:
  private val tag: String = "base"
  def baseTag: String = tag

class Derived(val tag: String) extends Base

@main def run(): Unit =
  val d = Derived("derived")
  println(d.tag)
  println(d.baseTag)
