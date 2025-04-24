def main(): Unit =
  val x = if true then true else false
  val y = 
    if true then
      val z = 0
      z
    else 0
