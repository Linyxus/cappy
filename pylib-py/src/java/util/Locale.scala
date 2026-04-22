package java.util

/** Link-time stub. Stdlib references Locale-aware overloads
 *  (`String.format(Locale, ...)`); pos-py tests don't exercise locale
 *  semantics. */
class Locale

object Locale:
  val ROOT: Locale = new Locale
  val US: Locale = new Locale
  val ENGLISH: Locale = new Locale
