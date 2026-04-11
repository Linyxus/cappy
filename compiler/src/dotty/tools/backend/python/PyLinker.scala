package dotty.tools.backend.python

import dotty.tools.backend.python.ir.*

import java.io.PrintWriter

/** Bundles multiple PyModules into a single self-contained Python file.
  *
  * Output structure:
  *   1. Runtime (inlined)
  *   2. All class definitions and module singleton instantiations
  *   3. `if __name__ == "__main__"` entry point (if applicable)
  *
  * This eliminates cross-module imports entirely — everything lives in one scope.
  */
object PyLinker:

  /** Link all modules into a single Python source file.
    *
    * @param modules     all generated PyModules from the compilation unit
    * @param runtime     the runtime Python source to inline at the top
    * @param out         writer for the output
    */
  def link(modules: List[PyModule], runtime: String, out: PrintWriter): Unit =
    // Separate main entry point stmts from regular modules
    val (mainStmts, linkedModules) = extractMainStmts(modules)
    PyEmitter.emitBundle(runtime, linkedModules, mainStmts, out)

  /** Extract `if __name__ == "__main__"` statements from modules.
    * These are moved to the very end of the bundled output.
    * Returns (mainStmts, modulesWithMainStmtsRemoved).
    */
  private def extractMainStmts(modules: List[PyModule]): (List[PyStmt], List[PyModule]) =
    var mainStmts = List.empty[PyStmt]
    val cleaned = modules.map { module =>
      val (mains, rest) = module.initStmts.partition(isMainGuard)
      if mains.nonEmpty then mainStmts = mains
      module.copy(initStmts = rest)
    }
    (mainStmts, cleaned)

  /** Check if a statement is an `if __name__ == "__main__"` guard. */
  private def isMainGuard(stmt: PyStmt): Boolean = stmt match
    case PyStmt.If(PyExpr.Compare(PyExpr.Name(PyName("__name__")), _, _), _, _, _) => true
    case _ => false
