struct IO()
def useIO[R](op: IO^ => R): R = op(IO())
def test1(base: IO^): Unit =
  val ok1 = useIO: io =>
    0
  val ok2 = useIO: io =>
    () => ()
  val bad3 = useIO: io =>  // error, as expected
    io
