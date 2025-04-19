def test[IO](io: IO^{cap}): Unit =
  val t1 = () => io
  val t2 = [X] => io
  val t3 = (a: i64) => (b: i64) => #i64add(a, b)
  val t4 = (myio: IO^{cap}) => 
    val t = io
    myio
    t
  val t5 = (io: IO^{cap}) => io
