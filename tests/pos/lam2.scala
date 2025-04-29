struct Box(unbox: () -> () ->{cap} Unit)
def main(): Unit =
  def foo(f: Box): Unit = f.unbox()()
  ()
