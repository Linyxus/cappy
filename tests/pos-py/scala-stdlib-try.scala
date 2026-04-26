// Broad coverage of scala.util.Try / Success / Failure operations.
// Don't print Failure(...) directly: the exception toString varies. Instead
// print isFailure plus the simple class name of the captured throwable.

import scala.util.{Try, Success, Failure}

@main def scalaStdlibTry(): Unit =
  // 1. construction
  val ok: Try[Int] = Try(21 * 2)
  val bad: Try[Int] = Try(1 / 0)
  val ok2: Try[Int] = Success(7)
  val bad2: Try[Int] = Failure(new IllegalStateException("nope"))
  println("build:ok=" + ok.isSuccess + ":bad=" + bad.isFailure + ":ok2=" + ok2.isSuccess + ":bad2=" + bad2.isFailure)

  // 2. predicates / accessors (success path only — get on Failure throws)
  println("access:" + ok.isSuccess + ":" + ok.isFailure + ":" + bad.isSuccess + ":" + bad.isFailure)
  println("getSuccess:" + ok.get)
  println("badClass:" + bad.failed.get.getClass.getSimpleName)
  println("bad2Class:" + bad2.failed.get.getClass.getSimpleName)

  // 3. map / flatMap
  val mapped = ok.map(_ + 1)
  val mappedBad = bad.map(_ + 1)
  val flat = ok.flatMap(x => Try(x * 2))
  val flatBad = ok.flatMap(_ => Try(throw new RuntimeException("boom2")))
  println("transform:" + mapped.get + ":" + mappedBad.isFailure + ":" + flat.get + ":" + flatBad.isFailure)

  // 4. recovery
  val recovered = bad.recover { case _: ArithmeticException => -1 }
  val recoveredWith = bad.recoverWith { case _: ArithmeticException => Success(-2) }
  val notRecovered = ok.recover { case _: RuntimeException => -1 }
  println("recovery:" + recovered.get + ":" + recoveredWith.get + ":" + notRecovered.get)

  // 5. aggregate: getOrElse
  println("getOrElse:" + ok.getOrElse(-1) + ":" + bad.getOrElse(-1))

  // 6. convert
  println("toOption:" + ok.toOption + ":" + bad.toOption)
