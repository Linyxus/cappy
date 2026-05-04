package java.nio.charset

import java.nio.charset as charset

final class StandardCharsets private ()

object StandardCharsets:
  // JDK exposes these as `public static final Charset` fields. Modeling them
  // as `val`s (rather than `def`s) keeps the same erased shape: dependent
  // libraries (e.g. `scala.io.Codec`'s `final val UTF8 = Codec(UTF_8)`) emit
  // their `.pyir` references against the field, and link-time resolution
  // requires a matching field on this object (Wave 6 item 10, t12290).
  val US_ASCII: Charset = charset.US_ASCII
  val ISO_8859_1: Charset = charset.ISO_8859_1
  val UTF_8: Charset = charset.UTF_8
  val UTF_16BE: Charset = charset.UTF_16BE
  val UTF_16LE: Charset = charset.UTF_16LE
  val UTF_16: Charset = charset.UTF_16
