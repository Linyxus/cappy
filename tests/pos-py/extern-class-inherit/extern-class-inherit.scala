// Comprehensive coverage of "Scala class extends Python class". The
// baseline `Mod`/`Counter` cases verify the basic shape; later cases
// stress edge boundaries (zero-arg super, mixed-type args, non-literal
// args, method override, template method, state isolation, init
// order, Scala field + Python attr coexistence, singleton object).
//
// One *facade* per Python class. Scala subclasses extend the facade
// without re-annotating with `@extern` — re-annotating would turn the
// subclass itself into a facade and bypass codegen of the super-init
// chain.

import scala.python.*

// ====== Facade declarations (one per Python class) ====================

@extern("mod_inherit", "Mod")
class Mod(seedArg: Int):
  @name("get_seed") def getSeed(): Int = native
  def describe(): String = native

@extern("mod_inherit", "Counter")
class Counter(startArg: Int):
  def bump(): Int = native

@extern("mod_inherit", "Default")
class Default():
  def tag(): String = native

@extern("mod_inherit", "Point3D")
class Point3D(x: Int, y: String, z: Double):
  @name("get_x") def getX(): Int    = native
  @name("get_y") def getY(): String = native
  @name("get_z") def getZ(): Double = native

@extern("mod_inherit", "Stateful")
class Stateful(init: Int):
  @name("get_count") def getCount(): Int = native

@extern("mod_inherit", "Tracking")
class Tracking(name: String):
  @name("get_name") def getName(): String = native

@extern("mod_inherit", "Worker")
class Worker():
  @name("do_work") def doWork(): String = native
  def execute(): String = native

@extern("mod_inherit", "Failer")
class Failer(shouldFail: Boolean):
  def ok(): Boolean = native

// ====== Baseline ======================================================

class Mod2 extends Mod(42)

class Mid extends Mod(7):
  def describeMid: String = "mid(seed=" + this.getSeed() + ")"

class Leaf extends Mid

// ====== Edge cases ====================================================

// Zero-arg Python parent — `super().__init__()` with no trailing args.
class DefaultSub extends Default()

// Multi-arg Python parent with mixed primitive types.
class Point3DSub extends Point3D(10, "scala", 2.5)

// Non-literal super-arg flowing from the subclass's own ctor parameter.
class ModNonLiteral(n: Int) extends Mod(n * 3)

// State isolation: two subclasses must not share state.
class StatefulSubA extends Stateful(10)
class StatefulSubB extends Stateful(20)

// Init order: Python parent's `__init__` prints "py-init:<name>" (with
// flush); Scala body prints "scala-body:<name>". Python line must come
// first because super-init is chained at the top of the Scala ctor.
class TrackingSub(n: String) extends Tracking(n):
  println("scala-body:" + n)

// Method override (Scala-caller route): a Scala subclass overrides a
// Python facade method. Calls *from Scala* dispatch to the override
// because Scala-side calls go through the Scala-encoded method name.
class ModOverride extends Mod(5):
  override def describe(): String = "scala-describe(" + this.getSeed() + ")"

// Template-method (Python-caller route, documented LIMITATION): the
// Python parent's `execute` calls `self.do_work()` — that's a Python
// MRO lookup for the literal identifier `do_work`. The Scala override
// emits under a Scala-encoded name, so Python's MRO still resolves to
// the parent's `do_work`. The expected output reflects this limit.
class WorkerSub extends Worker():
  override def doWork(): String = "scala-work"

// Scala field + Python attribute coexistence: both live on `self`.
class ModWithExtraField extends Mod(8):
  val tag: String = "scala-tag"
  def combined: String = tag + "/" + this.getSeed()

// Singleton: a Scala `object` extending a Python class. Constructed
// once with literal super-args.
object ModSingleton extends Mod(99)

// Successful path through a Python parent that *could* fail. We
// intentionally don't exercise the failing branch from a Scala
// try/catch: in v1 the foreign `RuntimeError` propagates *outside*
// the Scala exception hierarchy, so even `catch case _: Throwable`
// does not catch it. Exposing that gap would crash the test process.
class FailerOK extends Failer(false)

// Generic Scala class extending a Python class — type parameter must
// not interfere with the foreign super-init chain.
class Holder[T](item: T) extends Mod(13):
  def get: T = item

@main def externClassInherit(): Unit =
  // -- Baseline --
  val m = Mod(11)
  println("m.getSeed=" + m.getSeed())
  println("m.describe=" + m.describe())
  val m2 = Mod2()
  println("m2.getSeed=" + m2.getSeed())
  println("m2.describe=" + m2.describe())
  val leaf = Leaf()
  println("leaf.getSeed=" + leaf.getSeed())
  println("leaf.describeMid=" + leaf.describeMid)
  val c = Counter(10)
  println("c.bump=" + c.bump())
  println("c.bump=" + c.bump())
  println("c.bump=" + c.bump())

  // -- Zero-arg Python parent --
  val d = DefaultSub()
  println("d.tag=" + d.tag())

  // -- Multi-arg mixed types --
  val p = Point3DSub()
  println("p.x=" + p.getX() + " y=" + p.getY() + " z=" + p.getZ())

  // -- Non-literal super-arg --
  val nl = ModNonLiteral(4)
  println("nl.getSeed=" + nl.getSeed())

  // -- State isolation --
  val sa = StatefulSubA()
  val sb = StatefulSubB()
  println("sa.count=" + sa.getCount() + " sb.count=" + sb.getCount())

  // -- Init order --
  println("--init-order--")
  val tr = TrackingSub("alpha")
  println("tr.name=" + tr.getName())

  // -- Method override (Scala caller) --
  val mo = ModOverride()
  println("mo.describe=" + mo.describe())

  // -- Template-method (Python caller, documents the limit) --
  val ws = WorkerSub()
  println("ws.doWork=" + ws.doWork())
  println("ws.execute=" + ws.execute())

  // -- Scala field + Python attribute coexistence --
  val ef = ModWithExtraField()
  println("ef.combined=" + ef.combined)

  // -- Singleton --
  println("singleton.getSeed=" + ModSingleton.getSeed())

  // -- Foreign parent's `__init__` succeeds --
  // We intentionally don't exercise the failing branch: Python
  // exceptions raised inside the foreign `__init__` propagate
  // *outside* Scala's `Throwable` hierarchy, so a Scala `try/catch`
  // can't intercept them. Triggering the failure would crash the
  // test process. See FailerOK declaration for the full caveat.
  val okF = FailerOK()
  println("failerOK.ok=" + okF.ok())

  // -- Generic Scala class --
  val hi: Holder[String] = Holder("hello")
  val hn: Holder[Int]    = Holder(123)
  println("hi.get=" + hi.get + " hi.seed=" + hi.getSeed())
  println("hn.get=" + hn.get + " hn.seed=" + hn.getSeed())
