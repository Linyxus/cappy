// Regression test for `mutable.ArrayBuffer[Int]` (and other primitive
// element types) read via `apply`. After `SpecializeFunctions` rewrites
// the call site to `apply$mcII$sp` (etc.), the runtime needs the
// matching method on subclasses of `Function1` even though the bytecode
// only carries the unspecialized `apply` member: the JVM resolves it
// via Function1's interface default methods. The Python backend mirrors
// this through a `__getattr__` forwarder on the Function0/1/2 runtime
// classes.
import scala.collection.mutable.ArrayBuffer

@main def scalaArraybufferSpecializedApply(): Unit =
  val ints = ArrayBuffer.empty[Int]
  var i = 0
  while i < 4 do
    ints += i
    i += 1
  println("int(0)=" + ints(0))
  println("int(1)=" + ints(1))
  println("int(3)=" + ints(3))

  val longs = ArrayBuffer.empty[Long]
  var j = 0L
  while j < 3 do
    longs += j * 1000L
    j += 1
  println("long(0)=" + longs(0))
  println("long(2)=" + longs(2))

  val doubles = ArrayBuffer.empty[Double]
  doubles += 1.5
  doubles += 2.5
  println("double(1)=" + doubles(1))
