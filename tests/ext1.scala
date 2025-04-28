struct Ref(var data: i32)
extension (self: Ref^)
  def inc(): Unit = ()
  def dec(): Unit = ()

