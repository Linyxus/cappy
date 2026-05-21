package dotty.tools.benchmarks.py.dispatch

/** Method call through an N-level inheritance chain with `super` re-dispatch at
 *  each level. `shallow` is a 1-level override, `deep` chains four `super`
 *  calls, `deepMixed` is megamorphic across the whole chain. */
abstract class Layer0:
  def compute(x: Int): Int

class Layer1(val bias: Int) extends Layer0:
  def compute(x: Int): Int = x + bias

class Layer2(bias: Int, val scale: Int) extends Layer1(bias):
  override def compute(x: Int): Int = super.compute(x) * scale

class Layer3(bias: Int, scale: Int, val shift: Int) extends Layer2(bias, scale):
  override def compute(x: Int): Int = super.compute(x) + shift

class Layer4(bias: Int, scale: Int, shift: Int, val md: Int) extends Layer3(bias, scale, shift):
  override def compute(x: Int): Int = super.compute(x) % md

class DeepInheritanceBench:
  var size: Int = 0
  var shallow: Array[Layer0] = Array.empty
  var deep: Array[Layer0] = Array.empty
  var deepMixed: Array[Layer0] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    shallow = Array.tabulate(size)(i => Layer1(i % 5 + 1))
    deep = Array.tabulate(size)(i => Layer4(i % 5 + 1, i % 3 + 2, i % 4 + 1, i % 13 + 7))
    deepMixed = Array.tabulate(size) { i =>
      (i % 4) match
        case 0 => Layer1(i % 5 + 1)
        case 1 => Layer2(i % 5 + 1, i % 3 + 2)
        case 2 => Layer3(i % 5 + 1, i % 3 + 2, i % 4 + 1)
        case _ => Layer4(i % 5 + 1, i % 3 + 2, i % 4 + 1, i % 13 + 7)
    }

  val operations: Map[String, () => Any] = Map(
    "shallowCompute" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += shallow(i).compute(i % 7)
        i += 1
      s
    },
    "deepCompute" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += deep(i).compute(i % 7)
        i += 1
      s
    },
    "deepMixedCompute" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += deepMixed(i).compute(i % 7)
        i += 1
      s
    },
  )

@main def main(): Unit = ()
