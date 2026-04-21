package java.net

import java.io.UnsupportedEncodingException
import java.lang.ThrowablesSupport
import java.nio.charset.Charset

import scala.python.runtime.PyUrlParse

object URLDecoder:
  @Deprecated
  def decode(s: String): String =
    decode(s, Charset.defaultCharset())

  def decode(s: String, enc: String): String =
    val checked = ThrowablesSupport.requireNonNull(enc)
    if !Charset.isSupported(checked) then
      throw new UnsupportedEncodingException(checked)
    decode(s, Charset.forName(checked))

  def decode(s: String, charset: Charset): String =
    PyUrlParse.unquotePlus(
      ThrowablesSupport.requireNonNull(s),
      ThrowablesSupport.requireNonNull(charset).name()
    )
