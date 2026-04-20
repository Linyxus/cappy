enum Day:
  case Mon, Tue, Wed

enum Color:
  case Red, Green, Blue

@main def javalibLangEnum(): Unit =
  val values = Day.values
  println("values:" + values.length + ":" + values(0).toString() + ":" + values(2).toString())
  println("ordinal:" + Day.Mon.ordinal + ":" + Day.Wed.ordinal)
  println("valueof:" + Day.valueOf("Tue").ordinal + ":" + Color.valueOf("Blue").toString())
  println("compare:" + Integer.compare(Day.Mon.ordinal, Day.Tue.ordinal) + ":" + Integer.compare(Day.Wed.ordinal, Day.Wed.ordinal) + ":" + Integer.compare(Day.Wed.ordinal, Day.Mon.ordinal))
  println("tostring:" + Color.Red.toString() + ":" + (Color.Red == Color.valueOf("Red")))
