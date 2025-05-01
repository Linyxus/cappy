struct Ref()
def main(): Unit =
  val a = Ref()
  val b: Ref = #unsafeAsPure(a)
