// Cover three shapes of "Scala class extends Python class":
//
//  (1) `class Scala extends ExternPy(args)` — the basic case from
//      `test2.scala`. The Scala class declares no methods of its own;
//      Python MRO resolves method lookups against the foreign parent.
//
//  (2) `Scala` declares `native` facade methods (with `@name` mapping
//      to Python-side names); access goes through the same dynamic
//      path as ordinary `@extern` member calls.
//
//  (3) Two-level chain: `Leaf extends Mid extends ExternPy(args)`. The
//      mid-Scala class has the foreign super; the leaf inherits the
//      mid normally. Verifies the foreign-super machinery doesn't
//      assume the foreign parent is one hop above the leaf.

import scala.python.*

@extern("mod_inherit", "Mod")
class Mod(seedArg: Int):
  // Scala camelCase names map to snake_case Python methods via @name.
  @name("get_seed") def getSeed(): Int = native
  def describe(): String = native

@extern("mod_inherit", "Counter")
class Counter(startArg: Int):
  def bump(): Int = native

// (1) trivial Scala subclass of Python class with literal super-args.
class Mod2 extends Mod(42)

// (3) two-level chain rooted in a Python class.
class Mid extends Mod(7):
  def describeMid: String = "mid(seed=" + this.getSeed() + ")"

class Leaf extends Mid

@main def externClassInherit(): Unit =
  val m = Mod(11)
  println("m.getSeed=" + m.getSeed())
  println("m.describe=" + m.describe())

  // (1) Subclass with literal super-args. The synthesized `__init__`
  //     chains into `mod_inherit.Mod.__init__(self, 42)`, so the
  //     foreign instance gets its `_seed` attribute set.
  val m2 = Mod2()
  println("m2.getSeed=" + m2.getSeed())
  println("m2.describe=" + m2.describe())

  // (3) Chain: Leaf -> Mid -> Mod. The mid layer's own method works
  //     on a Leaf instance because Python's MRO walks back to it.
  val leaf = Leaf()
  println("leaf.getSeed=" + leaf.getSeed())
  println("leaf.describeMid=" + leaf.describeMid)

  // (2) Inheriting a Python method that mutates the foreign instance.
  val c = Counter(10)
  println("c.bump=" + c.bump())
  println("c.bump=" + c.bump())
  println("c.bump=" + c.bump())
