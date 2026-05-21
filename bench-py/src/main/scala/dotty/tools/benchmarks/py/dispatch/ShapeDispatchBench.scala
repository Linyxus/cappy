package dotty.tools.benchmarks.py.dispatch

/** Megamorphic vs monomorphic virtual dispatch over a trait method. The mixed
 *  array drives a 4-target call site; the homogeneous arrays give monomorphic
 *  and bimorphic baselines for a future devirtualization pass to beat. */
trait Shape:
  def area: Int
  def perimeter: Int

class Circle(val r: Int) extends Shape:
  def area: Int = r * r * 3
  def perimeter: Int = r * 6

class Rect(val w: Int, val h: Int) extends Shape:
  def area: Int = w * h
  def perimeter: Int = (w + h) * 2

class Triangle(val b: Int, val hh: Int) extends Shape:
  def area: Int = b * hh / 2
  def perimeter: Int = b + hh + (b * b + hh * hh)

class Square(val s: Int) extends Shape:
  def area: Int = s * s
  def perimeter: Int = s * 4

class ShapeDispatchBench:
  var size: Int = 0
  var shapes: Array[Shape] = Array.empty
  var monoShapes: Array[Shape] = Array.empty
  var biShapes: Array[Shape] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    shapes = Array.tabulate(size) { i =>
      (i % 4) match
        case 0 => Circle(i % 13 + 1)
        case 1 => Rect(i % 7 + 1, i % 5 + 1)
        case 2 => Triangle(i % 6 + 1, i % 9 + 1)
        case _ => Square(i % 11 + 1)
    }
    monoShapes = Array.tabulate(size)(i => Rect(i % 7 + 1, i % 5 + 1))
    biShapes = Array.tabulate(size) { i =>
      if i % 2 == 0 then Circle(i % 13 + 1) else Rect(i % 7 + 1, i % 5 + 1)
    }

  val operations: Map[String, () => Any] = Map(
    "megaArea" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += shapes(i).area
        i += 1
      s
    },
    "monoArea" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += monoShapes(i).area
        i += 1
      s
    },
    "megaPerimeter" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += shapes(i).perimeter
        i += 1
      s
    },
    "bimorphicArea" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += biShapes(i).area
        i += 1
      s
    },
  )

@main def main(): Unit = ()
