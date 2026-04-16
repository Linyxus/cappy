package scala

/** Exception thrown on a failed pattern match.
 *
 *  The compiler's pattern-match lowering synthesizes
 *  `throw new MatchError(scrut)` for non-exhaustive matches.
 */
class MatchError(obj: Any) extends RuntimeException(
  if obj == null then "null" else obj.toString
)
