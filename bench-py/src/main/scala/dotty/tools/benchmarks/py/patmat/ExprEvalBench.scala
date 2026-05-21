package dotty.tools.benchmarks.py.patmat

/** Sealed-trait ADT walked by pattern matching. `eval` is pure recursion +
 *  match; `render` adds string interpolation over the same tree. */
sealed trait Expr
case class Num(value: Int)          extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Neg(expr: Expr)          extends Expr

class ExprEvalBench:
  var size: Int = 0
  var tree: Expr = Num(0)

  def setup(size: Int): Unit =
    this.size = size
    tree = build(size)

  def build(n: Int): Expr =
    if n <= 1 then Num(n)
    else Add(build(n / 2), Mul(Num(n % 7), Neg(build(n / 3))))

  def eval(expr: Expr): Int = expr match
    case Num(value)    => value
    case Add(lhs, rhs) => eval(lhs) + eval(rhs)
    case Mul(lhs, rhs) => eval(lhs) * eval(rhs)
    case Neg(inner)    => -eval(inner)

  def render(expr: Expr): String = expr match
    case Num(value)    => value.toString
    case Add(lhs, rhs) => s"(${render(lhs)} + ${render(rhs)})"
    case Mul(lhs, rhs) => s"(${render(lhs)} * ${render(rhs)})"
    case Neg(inner)    => s"-${render(inner)}"

  val operations: Map[String, () => Any] = Map(
    "eval"   -> (() => eval(tree)),
    "render" -> (() => render(tree)),
  )

@main def main(): Unit = ()
