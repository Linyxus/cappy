// Phase-2 shrink-runtime regression guard.
// `notes/shrink-runtime.md` Category B drops the runtime stubs for
// `scala.annotation.Annotation` and `scala.annotation.StaticAnnotation`.
// After the drop, the `library-py`-compiled `.pyir` for both takes over.
// User-defined annotations must still:
//   - resolve the base class at link time (no Unresolved-class diagnostic)
//   - allow `extends StaticAnnotation` to type-check
//   - apply to a class declaration without running the annotation
//     reflectively (our backend doesn't expose `getAnnotations`)
//
// This fixture verifies the link side: the bundle compiles, runs, and
// the annotated class is otherwise unaffected.

import scala.annotation.StaticAnnotation

class myMark(val tag: String) extends StaticAnnotation

@myMark("hello")
class Tagged(val n: Int):
  override def toString: String = s"Tagged($n)"

@main def runtimeAnnotationBase(): Unit =
  val t = new Tagged(7)
  println(t)
  println(t.n)
  println(classOf[Tagged].getName)
