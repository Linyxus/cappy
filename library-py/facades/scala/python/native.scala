package scala.python

/** Placeholder body for facade members.
 *
 *  `inline` so that every call site is replaced by the throw at typer time;
 *  no IR call to `scala.python.native` is ever generated, which sidesteps the
 *  fact that Phase 0 does not yet skip facade bodies at codegen.
 */
inline def native[T]: T =
  throw new NotImplementedError("scala.python.native")
