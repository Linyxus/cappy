/*
 * Regression fence for the `genMatchExpr` / `genExpr(If)` pendingLocalDefs
 * scoping bug. Scala 3's PatternMatcher wraps expression-position
 * match cases in `Return(matchEnd, <body>)`. Our `genExpr(Return)`
 * pushes a `PyLabelReturn` to `pendingLocalDefs` and returns `PyUnitLit`.
 * Without per-case scoping, every case body's `PyLabelReturn` leaks to
 * the enclosing scope and runs unconditionally before the match dispatch
 * — so the first case always "wins" regardless of the selector.
 *
 * Pre-fix symptom: every call below would return the first arm's value.
 * Post-fix: dispatch picks the correct arm.
 */

private def stringMatch(key: String): String =
  key.toUpperCase() match
    case "A" => "alpha"
    case "B" => "beta"
    case "C" => "gamma"
    case _   => throw new IllegalArgumentException(key)

private def intMatch(n: Int): String =
  n match
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case _ => throw new IllegalArgumentException(n.toString)

private def ifChain(n: Int): String =
  // Nested if-as-expression — each branch is a match that itself would
  // emit a `PyLabelReturn`. Scopes must compose.
  if n < 0 then
    n match
      case -1 => "neg-one"
      case _  => "neg-other"
  else if n == 0 then "zero"
  else
    n match
      case 1 => "pos-one"
      case _ => "pos-other"

@main def matchExprCaseScoping(): Unit =
  // String match — the exact shape from `_String.normalizeCharsetName`
  // that first surfaced the bug.
  println("str:a:" + stringMatch("a"))     // expect "alpha"
  println("str:B:" + stringMatch("B"))     // expect "beta"
  println("str:c:" + stringMatch("c"))     // expect "gamma"

  // Int match — confirms the same shape works for non-String selectors.
  println("int:1:" + intMatch(1))          // expect "one"
  println("int:2:" + intMatch(2))          // expect "two"
  println("int:3:" + intMatch(3))          // expect "three"

  // Default arm (throw) must be reached, not the first arm.
  val strBogus =
    try
      stringMatch("zz")
      "no-throw"
    catch case _: IllegalArgumentException => "iae"
  println("str:zz:" + strBogus)

  val intBogus =
    try
      intMatch(99)
      "no-throw"
    catch case _: IllegalArgumentException => "iae"
  println("int:99:" + intBogus)

  // If-expression with match branches. Regression for the if-branch
  // scoping fix.
  println("if:-5:" + ifChain(-5))    // expect "neg-other"
  println("if:-1:" + ifChain(-1))    // expect "neg-one"
  println("if:0:" + ifChain(0))      // expect "zero"
  println("if:1:" + ifChain(1))      // expect "pos-one"
  println("if:7:" + ifChain(7))      // expect "pos-other"
