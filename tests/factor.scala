struct Var(var x: i32)
def get(v: Var^): i32 = v.x
def set(v: Var^, newX: i32): Unit = v.x = newX
def printFactors(now: i32, num: i32): Unit =
  if now <= num then
    val rem = num % now
    if rem == 0 then
      #i32println(now)
    printFactors(now + 1, num)
def main(): Unit =
  printFactors(1, 10000)
  ()
