def fibo(n: i32, k: i32 -> Unit): Unit =
  if n <= 1 then k(1)
  else fibo(n - 1, (x1: i32) => fibo(n - 2, (x2: i32) => k(x1 + x2)))
def main(): Unit =
  fibo(20, (x: i32) => #i32println(x))
  ()

