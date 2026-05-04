// Phase 1+2 of Category A migration in notes/shrink-runtime.md.
// Exercises the .scala-ported scala.runtime.{,Volatile}{Int,Long,Double,
// Float,Boolean,Byte,Char,Short,Object}Ref classes:
//   - factory: `XRef.create(v)` and `XRef.zero()`
//   - constructor + elem r/w
//   - `toString` per type (matches JVM)
//   - closure capture (forces real `IntRef`/`ObjectRef` allocation by
//     CapturedVars on the assignment path)
//
// Closure capture is the most demanding case: dotty rewrites
// `var x = 0; () => x = v` to `Assign(Apply(Select(xRef, elem), Nil), v)`
// after Erasure. CapturedVars rewrites that to a setter call on the Ref
// class so backends emit a uniform method invocation.

import scala.runtime.*

@main def runtimeRefs(): Unit =
  // -- Non-volatile Refs (factory + constructor + elem r/w + toString) ---
  println(IntRef.create(7).elem)        // 7
  println(IntRef.zero().elem)           // 0
  val ir = new IntRef(3)
  ir.elem = 4
  println(ir)                           // 4
  println(LongRef.create(9L).elem)      // 9
  val dr = DoubleRef.create(1.5)
  dr.elem = 2.5
  println(dr)                           // 2.5
  println(FloatRef.zero().elem)         // 0.0
  println(BooleanRef.create(true))      // true
  val br = BooleanRef.create(false)
  br.elem = true
  println(br)                           // true
  println(ByteRef.create(7.toByte))     // 7
  val cr = CharRef.create('a')
  cr.elem = 'b'
  println(cr)                           // b
  println(ShortRef.create(11.toShort))  // 11
  val or = ObjectRef.create("hello")
  or.elem = "world"
  println(or)                           // world

  // -- Volatile Refs: nominally distinct from non-volatile --
  val vir = VolatileIntRef.create(5)
  vir.elem = 8
  println(vir)                          // 8
  println(vir.isInstanceOf[VolatileIntRef])  // true
  println(vir.isInstanceOf[IntRef])          // false (distinct classes)
  println(VolatileLongRef.zero().elem)       // 0
  println(VolatileObjectRef.create("x"))     // x

  // -- Closure capture path: var assignment from inside a lambda forces
  //    the CapturedVars rewrite. The `Assign(getter-Apply, rhs)` form
  //    must route through a setter call.
  var i = 0
  List(1, 2, 3).foreach(_ => i += 1)
  println(i)                            // 3

  var s: String = "init"
  val mutate: () => Unit = () => s = "mutated"
  mutate()
  println(s)                            // mutated

  var d: Double = 0.0
  (1 to 4).foreach(n => d += n.toDouble)
  println(d)                            // 10.0
