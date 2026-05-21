package dotty.tools.benchmarks.py.dispatch

/** Trait default method vs concrete override. `inherited` keeps the trait
 *  default; `overridden` replaces it; `mixed` is bimorphic on `score`. The
 *  abstract `bonus` gives an always-overridden baseline. */
trait Scorer:
  def score(x: Int): Int = x * x + 1
  def bonus(x: Int): Int

class InheritScorer(val mult: Int) extends Scorer:
  def bonus(x: Int): Int = x * mult

class OverrideScorer(val mult: Int) extends Scorer:
  override def score(x: Int): Int = x * mult + x
  def bonus(x: Int): Int = x + mult

class TraitDefaultBench:
  var size: Int = 0
  var inherited: Array[Scorer] = Array.empty
  var overridden: Array[Scorer] = Array.empty
  var mixed: Array[Scorer] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    inherited = Array.tabulate(size)(i => InheritScorer(i % 7 + 1))
    overridden = Array.tabulate(size)(i => OverrideScorer(i % 7 + 1))
    mixed = Array.tabulate(size) { i =>
      if i % 2 == 0 then InheritScorer(i % 7 + 1) else OverrideScorer(i % 7 + 1)
    }

  val operations: Map[String, () => Any] = Map(
    "inheritedScore" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += inherited(i).score(i)
        i += 1
      s
    },
    "overriddenScore" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += overridden(i).score(i)
        i += 1
      s
    },
    "mixedScore" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += mixed(i).score(i)
        i += 1
      s
    },
    "abstractBonus" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += inherited(i).bonus(i)
        i += 1
      s
    },
  )

@main def main(): Unit = ()
