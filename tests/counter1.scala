struct Counter(var x: i32)
val c: Counter^ = Counter(0)
def main(): Unit =
  #i32println(c.x)
  ()
