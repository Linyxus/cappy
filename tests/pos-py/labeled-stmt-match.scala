def selector(): Int = 1

@main def labeledStmtMatch(): Unit =
  println("begin")

  selector() match
    case 1 => println("arm-one")
    case _ => println("arm-other")

  println("after")

  // Non-1 selector — exercises the default arm. Uses a local def so the
  // literal argument isn't constant-folded.
  def chosen(n: Int): Unit =
    n match
      case 1 => println("chosen-one")
      case _ => println("chosen-other")

  chosen(5)
  println("end")
