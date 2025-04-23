def test(): Unit =
  def unbox[cap C](boxed: () -> () ->{C} Unit): Unit =
    boxed()()
  ()
