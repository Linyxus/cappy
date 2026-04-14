def add(a: Int, b: Int): Int = a + b

def abs(x: Int): Int =
  if x >= 0 then x else -x

def sign(x: Int): String =
  if x > 0 then "positive"
  else if x < 0 then "negative"
  else "zero"

def factorial(n: Int): Int =
  var result = 1
  var i = 1
  while i <= n do
    result = result * i
    i = i + 1
  result

@main def test2(): Unit =
  println(add(40, 2))
  println(abs(-7))
  println(abs(3))
  println(sign(5))
  println(sign(-1))
  println(sign(0))
  println(factorial(5))
