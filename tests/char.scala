// Library functions
// 1. String I/O
def printChar(ch: char): Unit = #putchar(ch)
def print(str: array[char]^): Unit =
  def recur(cur: i32): Unit =
    if cur < str.size then
      printChar(str(cur))
      recur(cur + 1)
  recur(0)
def println(str: array[char]^): Unit =
  print(str)
  printChar('\n')
// 1.1 String operations
def concatStr(s1: array[char]^, s2: array[char]^): array[char]^ =
  val result = newArray(s1.size + s2.size, '0')
  def copy1(cur: i32): Unit =
    if cur < s1.size then
      result(cur) = s1(cur)
      copy1(cur+1)
  def copy2(cur: i32): Unit =
    if cur < s2.size then
      result(s1.size + cur) = s2(cur)
      copy2(cur+1)
  copy1(0)
  copy2(0)
  result
extension (s1: array[char]^)
  def concat(s2: array[char]^): array[char]^ = concatStr(s1, s2)
// end of library functions

def main(): Unit =
  println("hello, world")
  val s = "heyhey".concat(", wasm")
  println(s)
  val s1 = "你好".concat("世界")
  println(s1)

