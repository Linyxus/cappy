package java.net

import java.io.UnsupportedEncodingException
import java.lang.ThrowablesSupport
import java.nio.charset.Charset

import scala.python.runtime.PyUrlParse

object URLEncoder:
  @Deprecated
  def encode(s: String): String =
    encode(s, Charset.defaultCharset())

  def encode(s: String, enc: String): String =
    val checked = ThrowablesSupport.requireNonNull(enc)
    if !Charset.isSupported(checked) then
      throw new UnsupportedEncodingException(checked)
    encode(s, Charset.forName(checked))

  def encode(s: String, charset: Charset): String =
    val encoded =
      PyUrlParse.quotePlus(
        ThrowablesSupport.requireNonNull(s),
        "*",
        ThrowablesSupport.requireNonNull(charset).name()
      )
    encoded.replace("~", "%7E")
