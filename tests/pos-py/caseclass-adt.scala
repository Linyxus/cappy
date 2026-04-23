object CaseclassAdt:
  sealed trait Expr
  case class Num(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Mul(lhs: Expr, rhs: Expr) extends Expr
  case class Neg(expr: Expr) extends Expr

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

@main def run(): Unit =
  val expr = CaseclassAdt.Add(
    CaseclassAdt.Num(2),
    CaseclassAdt.Mul(CaseclassAdt.Num(3), CaseclassAdt.Neg(CaseclassAdt.Num(4)))
  )

  println(expr)
  println(CaseclassAdt.eval(expr))
  println(CaseclassAdt.render(expr))
  println(expr == CaseclassAdt.Add(
    CaseclassAdt.Num(2),
    CaseclassAdt.Mul(CaseclassAdt.Num(3), CaseclassAdt.Neg(CaseclassAdt.Num(4)))
  ))
  println(expr.copy(rhs = CaseclassAdt.Num(5)))
  println(expr == CaseclassAdt.Add(
    CaseclassAdt.Num(2),
    CaseclassAdt.Mul(CaseclassAdt.Num(3), CaseclassAdt.Neg(CaseclassAdt.Num(4)))
  ))
  println(CaseclassAdt.Num(5) == expr)
  println(CaseclassAdt.render(CaseclassAdt.Neg(CaseclassAdt.Add(CaseclassAdt.Num(1), CaseclassAdt.Num(2)))))
