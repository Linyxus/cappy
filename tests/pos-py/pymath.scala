import scala.python.PyMath

@main def pyMathFacade(): Unit =
  println("pi-close:" + (PyMath.pi > 3.14159 && PyMath.pi < 3.14160))
  println("sqrt4:" + PyMath.sqrt(4.0))
  println("fma:" + PyMath.fma(2.0, 3.0, 4.0))
  println("exp2-5:" + PyMath.exp2(5.0))

  println("log-base:" + PyMath.log(8.0, 2.0))
  println("perm-default:" + PyMath.perm(5))
  println("gcd:" + PyMath.gcd(48, 18, 30))
  println("lcm:" + PyMath.lcm(4, 6, 10))

  println("isclose-kw:" + PyMath.isclose(1.0, 1.1, relTol = 0.2, absTol = 0.0))
  println("nextafter-steps:" + (PyMath.nextafter(1.0, 2.0, steps = 2) > 1.0))
  println("prod-start:" + PyMath.prod(Array(2, 3, 4), start = 5))

  println("dist:" + PyMath.dist(Array(0.0, 0.0), Array(3.0, 4.0)))
  println("frexp:" + PyMath.frexp(8.0))
