def run(runner: () => Unit): Unit =
  runner()
def main(): Unit =
  run: () =>
    #i32println(42)
  ()
