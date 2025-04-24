struct Counter(var num: i32)
val numCalls: Counter^ = Counter(0)
def recordCall(): Unit =
  numCalls.num = numCalls.num + 1
def printNumCalls(): Unit =
  #i32println(numCalls.num)
def checkPrime(x: i32, divisor: i32): bool =
  recordCall()
  if divisor >= x then true
  else
    val rem = x % divisor
    (rem != 0) && checkPrime(x, divisor + 1)
def printPrimes(now: i32, limit: i32): Unit =
  if checkPrime(now, 2) then
    #i32println(now)
  if now < limit then
    printPrimes(now + 1, limit)
def main(): Unit =
  printPrimes(2, 50)
  printNumCalls()
  ()
