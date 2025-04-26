def test[IO](a: IO^, b: IO^): Unit =
  val f: () ->{} () ->{a} () ->{b} Unit = sorry()

  val t1 = () =>
    f()
    ()
  
  val t2 = () =>
    f()()
    ()

  val t3 = () =>
    f()()()
    ()
