package dotty.tools.cappyrepl

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

/** Per-session REPL state. Mirrors `dotty.tools.repl.State`.
 *
 *  @param objectIndex          Index of the most recently emitted wrapper
 *                              (`cappy_line_<objectIndex>`). The next compile
 *                              produces `cappy_line_<objectIndex+1>`.
 *  @param valIndex             Counter for synthetic `valresN` bindings emitted
 *                              when the user types a free expression.
 *  @param imports              Per-wrapper imports the user introduced
 *                              (e.g. `import scala.collection.mutable.*`),
 *                              re-applied in subsequent runs.
 *  @param invalidObjectIndexes Wrappers whose runtime initialization failed
 *                              and should not be re-imported.
 *  @param context              Latest compiler `Context`.
 */
final case class CappyReplState(
    objectIndex:          Int,
    valIndex:             Int,
    imports:              Map[Int, List[tpd.Import]],
    invalidObjectIndexes: Set[Int],
    context:              Context
):
  def validObjectIndexes: Seq[Int] =
    (1 to objectIndex).filterNot(invalidObjectIndexes.contains(_))
