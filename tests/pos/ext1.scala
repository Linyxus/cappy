struct Ref(var data: i32)
extension (self: Ref^)
  def inc(): Unit = self.data = self.data + 1
  def dec(): Unit = self.data = self.data - 1
def main(): Unit =
  val x = Ref(0)
  x.inc()

