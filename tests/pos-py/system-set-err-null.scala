// JDK semantics: `System.setOut(null)` / `System.setErr(null)` is
// PERMITTED — the field becomes null and only subsequent USE of
// `System.out` / `System.err` then NPEs. Pylib previously called
// `requireNonNull` eagerly inside the setter, which broke
// `tests/run/i2772.scala` (closure captures the OLD `System.err`,
// then nulls it; calling `oldErr.write(0)` should succeed because
// the closure holds the pre-null reference).
//
// This fixture asserts the contract directly: `setErr(null)` returns
// normally, the captured pre-null reference still works, and reading
// `System.err` after the null-set does NOT NPE at the read site
// (only at first USE of the result, matching the JVM).

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}

@main def systemSetErrNull(): Unit =
  // Capture the original stream first.
  val captured = new ByteArrayOutputStream()
  val oldErr = System.err
  System.setErr(new PrintStream(captured))

  // Now null the field. Must NOT throw — JDK behaviour.
  System.setErr(null)

  // The pre-null reference still works.
  val a: () => Unit = () => write0(captured)
  a()
  println(captured.toByteArray().mkString(","))   // 0

  // Restore so the harness's stderr capture is sane on exit.
  System.setErr(oldErr)
  println("done")

def write0(out: OutputStream): Unit =
  out.write(0)
