package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyUrlParse:
  @extern("urllib.parse")
  private object urllib extends PyAny:
    @name("urlparse")
    def urlparse(url: String): PyDynamic = native

    @name("urljoin")
    def urljoin(base: String, rel: String): String = native

    @name("quote")
    def quote0(s: String, safe: String, encoding: String): String = native

    @name("unquote")
    def unquote0(s: String, encoding: String, errors: String): String = native

    @name("quote_plus")
    def quotePlus0(s: String, safe: String, encoding: String): String = native

    @name("unquote_plus")
    def unquotePlus0(s: String, encoding: String, errors: String): String = native

  def parse(url: String): ParsedUrl =
    new ParsedUrl(urlparse = urllib.urlparse(url).asInstanceOf[PyDynamic])

  def join(base: String, rel: String): String =
    urllib.urljoin(base, rel)

  def quote(s: String, safe: String, encoding: String): String =
    urllib.quote0(s, safe, encoding)

  def unquote(s: String, encoding: String): String =
    urllib.unquote0(s, encoding, "strict")

  def quotePlus(s: String, safe: String, encoding: String): String =
    urllib.quotePlus0(s, safe, encoding)

  def unquotePlus(s: String, encoding: String): String =
    urllib.unquotePlus0(s, encoding, "strict")

final class ParsedUrl private[runtime] (private val urlparse: PyDynamic):
  def scheme: String =
    urlparse.scheme.asInstanceOf[String]

  def authority: String =
    urlparse.netloc.asInstanceOf[String]

  def path: String =
    val path0 = urlparse.path.asInstanceOf[String]
    val params0 = urlparse.params.asInstanceOf[String]
    if params0 == null || params0 == "" then path0
    else path0 + ";" + params0

  def query: String =
    urlparse.query.asInstanceOf[String]

  def fragment: String =
    urlparse.fragment.asInstanceOf[String]
