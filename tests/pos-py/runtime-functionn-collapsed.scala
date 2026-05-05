// Phase-3b shrink-runtime regression guard.
// `notes/shrink-runtime.md` Phase 3b drops the runtime stubs for
// `scala.Function0`..`scala.Function22`. The `library-py`-compiled
// `.pyir` for each FunctionN now provides the concrete bodies for
// `apply_mc<X><Y>_sp` (specialization forwarders), `andThen` /
// `compose` (Function1) and `tupled` / `curried` (Function2+).
//
// Coverage:
//   - direct closure invocation
//   - Function1.compose  / Function1.andThen  (Function1.pyir bodies)
//   - Function2.tupled   / Function2.curried (Function2.pyir bodies)
//   - primitive specialization on a primitive container — exercises
//     the `apply_mcII_sp` forwarder defined on Function1.pyir.

@main def runtimeFunctionnCollapsed(): Unit =
  // Direct call.
  val f: Int => Int = _ + 1
  println(f(40))                          // 41

  // Function1.compose: (g andThen f)(x) = f(g(x))
  // Equivalent to: f.compose(g) in Scala.
  val g: Int => Int = _ * 2
  val h1 = f.compose(g)
  println(h1(20))                         // f(g(20)) = f(40) = 41

  // Function1.andThen.
  val h2 = f.andThen(g)
  println(h2(20))                         // g(f(20)) = g(21) = 42

  // Function2.tupled.
  val add: (Int, Int) => Int = _ + _
  println(add.tupled((40, 2)))            // 42

  // Function2.curried.
  val curr = add.curried
  println(curr(40)(2))                    // 42

  // Primitive specialization on a primitive collection. The `apply` call
  // on `xs` lowers to `apply_mcII_sp__I__I` post-`SpecializeFunctions`;
  // the body lives on Function1.pyir and forwards to the unspecialized
  // boxed `apply` which dispatches to the user's override (here, the
  // ArrayBuffer's element-at index).
  val xs = scala.collection.mutable.ArrayBuffer(10, 20, 30)
  println(xs(1))                          // 20
