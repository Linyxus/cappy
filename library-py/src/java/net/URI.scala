package java.net

import java.io.Serializable
import java.lang.ThrowablesSupport

import java.util.internal.MurmurHash3.{finalizeHash, mix, mixLast}

import scala.python.runtime.PyUrlParse

final class URI(private val origStr: String) extends Serializable with Comparable[URI]:
  import URI.*

  ThrowablesSupport.requireNonNull(origStr)
  validate(origStr)

  private val parsed = PyUrlParse.parse(origStr)
  private val _scheme: String | Null = nullIfEmpty(parsed.scheme)
  private val _authority: String | Null = nullIfEmpty(parsed.authority)
  validateAuthoritySyntax(_authority, origStr)
  private val _path: String | Null = parsed.path
  private val _query: String | Null = nullIfEmpty(parsed.query)
  private val _fragment: String | Null = nullIfEmpty(parsed.fragment)
  private val _isAbsolute = _scheme != null
  private val _isOpaque = _isAbsolute && _authority == null && _path != null && !_path.startsWith("/")
  private val authorityParts = parseAuthority(_authority, origStr)
  private val _userInfo: String | Null = authorityParts.userInfo
  private val _host: String | Null = authorityParts.host
  private val _port = authorityParts.port
  private val _schemeSpecificPart: String | Null = schemeSpecificPartOf(_authority, _path, _query)

  def this(scheme: String, ssp: String, fragment: String) =
    this(URI.uriStr(scheme, ssp, fragment))

  def this(
      scheme: String,
      userInfo: String,
      host: String,
      port: Int,
      path: String,
      query: String,
      fragment: String
  ) =
    this(URI.uriStr(scheme, userInfo, host, port, path, query, fragment))
    parseServerAuthority()

  def this(scheme: String, host: String, path: String, fragment: String) =
    this(scheme, null.asInstanceOf[String], host, -1, path, null.asInstanceOf[String], fragment)

  def this(scheme: String, authority: String, path: String, query: String, fragment: String) =
    this(URI.uriStr(scheme, authority, path, query, fragment))

  def compareTo(that: URI): Int =
    def comparePathQueryFragment(): Int =
      val cmpPath = escapeAwareCompare(this._path, that._path)
      if cmpPath != 0 then cmpPath
      else
        val cmpQuery = escapeAwareCompare(this._query, that._query)
        if cmpQuery != 0 then cmpQuery
        else escapeAwareCompare(this._fragment, that._fragment)

    val cmpScheme = caseInsensitiveCompare(this._scheme, that._scheme)
    if cmpScheme != 0 then cmpScheme
    else
      val cmpOpaque = java.lang.Boolean.compare(this.isOpaque(), that.isOpaque())
      if cmpOpaque != 0 then cmpOpaque
      else if this.isOpaque() then
        val cmpSsp = escapeAwareCompare(this._schemeSpecificPart, that._schemeSpecificPart)
        if cmpSsp != 0 then cmpSsp
        else comparePathQueryFragment()
      else if this._host != null && that._host != null then
        val cmpUserInfo = escapeAwareCompare(this._userInfo, that._userInfo)
        if cmpUserInfo != 0 then cmpUserInfo
        else
          val cmpHost = caseInsensitiveCompare(this._host, that._host)
          if cmpHost != 0 then cmpHost
          else
            val cmpPort = this._port - that._port
            if cmpPort != 0 then cmpPort
            else comparePathQueryFragment()
      else
        val cmpAuthority = escapeAwareCompare(this._authority, that._authority)
        if cmpAuthority != 0 then cmpAuthority
        else comparePathQueryFragment()

  override def equals(that: Any): Boolean =
    that match
      case that: URI => compareTo(that) == 0
      case _         => false

  override def hashCode(): Int =
    def normalizeEscapesHash(str: String | Null): Int =
      if str == null then 0 else normalizeEscapes(str.asInstanceOf[String]).asInstanceOf[String].hashCode()

    var acc = uriSeed
    acc = mix(acc, if _scheme == null then 0 else _scheme.toLowerCase().hashCode())
    if isOpaque() then
      acc = mix(acc, normalizeEscapesHash(_schemeSpecificPart))
    else if _host != null then
      acc = mix(acc, normalizeEscapesHash(_userInfo))
      acc = mix(acc, _host.toLowerCase().hashCode())
      acc = mix(acc, _port.hashCode())
    else
      acc = mix(acc, normalizeEscapesHash(_authority))
    acc = mix(acc, normalizeEscapesHash(_path))
    acc = mix(acc, normalizeEscapesHash(_query))
    acc = mixLast(acc, normalizeEscapesHash(_fragment))
    finalizeHash(acc, 3)

  def getAuthority(): String = decodeComponent(_authority).asInstanceOf[String]
  def getFragment(): String = decodeComponent(_fragment).asInstanceOf[String]
  def getHost(): String = _host.asInstanceOf[String]
  def getPath(): String = decodeComponent(_path).asInstanceOf[String]
  def getPort(): Int = _port
  def getQuery(): String = decodeComponent(_query).asInstanceOf[String]
  def getRawAuthority(): String = _authority.asInstanceOf[String]
  def getRawFragment(): String = _fragment.asInstanceOf[String]
  def getRawPath(): String = _path.asInstanceOf[String]
  def getRawQuery(): String = _query.asInstanceOf[String]
  def getRawSchemeSpecificPart(): String = _schemeSpecificPart.asInstanceOf[String]
  def getRawUserInfo(): String = _userInfo.asInstanceOf[String]
  def getScheme(): String = _scheme.asInstanceOf[String]
  def getSchemeSpecificPart(): String = decodeComponent(_schemeSpecificPart).asInstanceOf[String]
  def getUserInfo(): String = decodeComponent(_userInfo).asInstanceOf[String]

  def isAbsolute(): Boolean =
    _isAbsolute

  def isOpaque(): Boolean =
    _isOpaque

  def normalize(): URI =
    if _isOpaque || _path == null then this
    else
      val newPath = normalizePath(_path)
      if newPath == _path then this
      else new URI(getScheme(), getRawAuthority(), newPath, getRawQuery(), getRawFragment())

  def parseServerAuthority(): URI =
    if _authority != null && _host == null then
      throw new URISyntaxException(origStr, "No Host in URI")
    else
      this

  def relativize(uri: URI): URI =
    if this.isOpaque() || uri.isOpaque() || this._scheme != uri._scheme ||
        escapeAwareCompare(this._authority, uri._authority) != 0
    then
      uri
    else
      val thisN = this.normalize()
      val uriN = uri.normalize()
      if uriN.getRawPath().startsWith(thisN.getRawPath()) then
        val newPath = uriN.getRawPath().substring(thisN.getRawPath().length())
        new URI(
          scheme = null.asInstanceOf[String],
          authority = null.asInstanceOf[String],
          path = if newPath.startsWith("/") then newPath.substring(1) else newPath,
          query = uri.getRawQuery(),
          fragment = uri.getRawFragment()
        )
      else uri

  def resolve(str: String): URI =
    resolve(URI.create(str))

  def resolve(uri: URI): URI =
    if uri.isAbsolute() || this.isOpaque() then uri
    else new URI(PyUrlParse.join(this.toString(), uri.toString()))

  def toASCIIString(): String =
    quoteNonASCII(origStr).asInstanceOf[String]

  override def toString(): String =
    origStr

object URI:
  private final val Utf8 = "UTF-8"
  private final val UserInfoSafe = "%!$&'()*+,;=:-._~"
  private final val PathSafe = "%!$&'()*+,;=:@/-._~"
  private final val AuthoritySafe = "%!$&'()*+,;=:@[]-._~"
  private final val IllegalSafe = "%!$&'()*+,;=:/?@[]-._~"
  private final val uriSeed = 53722356

  private final class AuthorityParts(
      val userInfo: String | Null,
      val host: String | Null,
      val port: Int
  )

  def create(str: String): URI =
    try new URI(str)
    catch
      case e: URISyntaxException => throw new IllegalArgumentException(e)

  private def nullIfEmpty(value: String): String | Null =
    if value == null || value == "" then null else value

  private def schemeSpecificPartOf(authority: String | Null, path: String | Null, query: String | Null): String =
    val builder = new java.lang.StringBuilder()
    if authority != null then builder.append("//").append(authority)
    if path != null then builder.append(path)
    if query != null then builder.append('?').append(query)
    builder.toString()

  private def parseAuthority(authority: String | Null, input: String): AuthorityParts =
    if authority == null then
      new AuthorityParts(null, null, -1)
    else
      val at = authority.lastIndexOf('@')
      val userInfo =
        if at >= 0 then authority.substring(0, at)
        else null
      val hostPort =
        if at >= 0 then authority.substring(at + 1)
        else authority

      if hostPort == null || hostPort == "" then
        new AuthorityParts(userInfo, null, -1)
      else if hostPort.startsWith("[") then
        val end = hostPort.indexOf(']')
        if end < 0 || hostPort.indexOf('[', 1) >= 0 || hostPort.indexOf(']', end + 1) >= 0 then
          throw new URISyntaxException(input, "Malformed IPv6 address")
        val host = hostPort.substring(1, end)
        val rest = hostPort.substring(end + 1)
        if rest == "" then
          new AuthorityParts(userInfo, host, -1)
        else if rest.startsWith(":") && isAllDigits(rest.substring(1)) then
          new AuthorityParts(userInfo, host, parsePort(rest.substring(1), input))
        else if rest.startsWith(":") then
          new AuthorityParts(userInfo, null, -1)
        else
          throw new URISyntaxException(input, "Malformed authority")
      else
        val colon = hostPort.indexOf(':')
        if colon < 0 then
          new AuthorityParts(userInfo, hostPort, -1)
        else if hostPort.indexOf(':', colon + 1) >= 0 then
          new AuthorityParts(userInfo, null, -1)
        else
          val host = hostPort.substring(0, colon)
          val portText = hostPort.substring(colon + 1)
          if portText == "" || !isAllDigits(portText) || host == "" then
            new AuthorityParts(userInfo, null, -1)
          else
            new AuthorityParts(userInfo, host, parsePort(portText, input))

  private def parsePort(portText: String, input: String): Int =
    try Integer.parseInt(portText)
    catch
      case _: NumberFormatException =>
        throw new URISyntaxException(input, "Malformed port")

  private def validate(input: String): Unit =
    var i = 0
    while i < input.length() do
      val ch = input.charAt(i)
      if ch <= ' ' || ch == 0x7f then
        throw new URISyntaxException(input, "Illegal character", i)
      if ch == '%' then
        if i + 2 >= input.length() || !isHexDigit(input.charAt(i + 1)) || !isHexDigit(input.charAt(i + 2)) then
          throw new URISyntaxException(input, "Malformed escape pair", i)
        i += 2
      i += 1

    val colon = input.indexOf(':')
    if colon > 0 then
      val firstSlash = input.indexOf('/')
      val firstQuery = input.indexOf('?')
      val firstFragment = input.indexOf('#')
      val stop = firstNonNegative(firstSlash, firstQuery, firstFragment)
      if stop == -1 || colon < stop then
        validateScheme(input.substring(0, colon), input)

  private def validateScheme(scheme: String, input: String): Unit =
    if scheme == null || scheme == "" || !Character.isLetter(scheme.charAt(0)) then
      throw new URISyntaxException(input, "Malformed scheme")
    var i = 1
    while i < scheme.length() do
      val ch = scheme.charAt(i)
      if !Character.isLetterOrDigit(ch) && ch != '+' && ch != '-' && ch != '.' then
        throw new URISyntaxException(input, "Malformed scheme", i)
      i += 1

  private def validateAuthoritySyntax(authority: String | Null, input: String): Unit =
    if authority != null then
      val open = authority.indexOf('[')
      val close = authority.indexOf(']')
      if (open >= 0 && close < 0) || (open < 0 && close >= 0) || (open > close && close >= 0) then
        throw new URISyntaxException(input, "Malformed IPv6 address")

  private def firstNonNegative(a: Int, b: Int, c: Int): Int =
    var result = -1
    if a >= 0 then result = a
    if b >= 0 && (result < 0 || b < result) then result = b
    if c >= 0 && (result < 0 || c < result) then result = c
    result

  private def isHexDigit(ch: Char): Boolean =
    (ch >= '0' && ch <= '9') ||
      (ch >= 'a' && ch <= 'f') ||
      (ch >= 'A' && ch <= 'F')

  private def isAllDigits(value: String): Boolean =
    if value == null || value == "" then false
    else
      var i = 0
      while i < value.length() do
        if !Character.isDigit(value.charAt(i)) then
          return false
        i += 1
      true

  private def normalizePath(origPath: String): String =
    val segments = origPath.split("/", -1)
    val out = new Array[String](segments.length + 1)
    val isAbsPath = segments.length != 0 && segments(0) == ""
    val startIdx = if isAbsPath then 1 else 0
    if isAbsPath then out(0) = ""
    var inIdx = startIdx
    var outIdx = startIdx

    while inIdx != segments.length do
      val segment = segments(inIdx)
      inIdx += 1
      if segment == "." then
        if inIdx == segments.length then
          out(outIdx) = ""
          outIdx += 1
      else if segment == ".." then
        val okToDrop =
          outIdx != startIdx && {
            val lastSegment = out(outIdx - 1)
            lastSegment != ".." && lastSegment != ""
          }
        if okToDrop then
          if inIdx == segments.length then
            out(outIdx - 1) = ""
          else
            outIdx -= 1
        else
          out(outIdx) = ".."
          outIdx += 1
      else if segment == "" && inIdx != segments.length then
        ()
      else
        out(outIdx) = segment
        outIdx += 1

    val prependDot = outIdx != 0 && !isAbsPath && out(0).indexOf(':') >= 0
    val builder = new java.lang.StringBuilder()
    if prependDot then
      builder.append('.')
      if outIdx != 0 then builder.append('/')
    var i = 0
    while i < outIdx do
      if i != 0 then builder.append('/')
      builder.append(out(i))
      i += 1
    builder.toString()

  private def uriStr(scheme: String, ssp: String, fragment: String): String =
    val builder = new java.lang.StringBuilder()
    if scheme != null then builder.append(scheme).append(':')
    if ssp != null then builder.append(quoteIllegal(ssp))
    if fragment != null then builder.append('#').append(quoteIllegal(fragment))
    builder.toString()

  private def uriStr(
      scheme: String,
      userInfo: String,
      host: String,
      port: Int,
      path: String,
      query: String,
      fragment: String
  ): String =
    val builder = new java.lang.StringBuilder()
    if scheme != null then builder.append(scheme).append(':')
    if userInfo != null || host != null || port != -1 then
      builder.append("//")
    if userInfo != null then
      builder.append(quoteUserInfo(userInfo)).append('@')
    if host != null then
      if host.indexOf(':') >= 0 && !(host.startsWith("[") && host.endsWith("]")) then
        builder.append('[').append(host).append(']')
      else
        builder.append(host)
    if port != -1 then
      builder.append(':').append(port)
    if path != null then
      builder.append(quotePath(path))
    if query != null then
      builder.append('?').append(quoteIllegal(query))
    if fragment != null then
      builder.append('#').append(quoteIllegal(fragment))
    builder.toString()

  private def uriStr(
      scheme: String,
      authority: String,
      path: String,
      query: String,
      fragment: String
  ): String =
    val builder = new java.lang.StringBuilder()
    if scheme != null then builder.append(scheme).append(':')
    if authority != null then
      builder.append("//").append(quoteAuthority(authority))
    if path != null then
      builder.append(quotePath(path))
    if query != null then
      builder.append('?').append(quoteIllegal(query))
    if fragment != null then
      builder.append('#').append(quoteIllegal(fragment))
    builder.toString()

  private def quoteUserInfo(str: String): String =
    PyUrlParse.quote(str, UserInfoSafe, Utf8)

  private def quotePath(str: String): String =
    PyUrlParse.quote(str, PathSafe, Utf8)

  private def quoteAuthority(str: String): String =
    PyUrlParse.quote(str, AuthoritySafe, Utf8)

  private def quoteIllegal(str: String): String =
    PyUrlParse.quote(str, IllegalSafe, Utf8)

  private def decodeComponent(str: String | Null): String | Null =
    if str == null then null
    else PyUrlParse.unquote(str, Utf8)

  private def quoteNonASCII(str: String | Null): String | Null =
    if str == null then
      null
    else
      val out = new java.lang.StringBuilder()
      var i = 0
      while i < str.length() do
        val ch = str.charAt(i)
        if ch <= 0x7f then
          out.append(ch)
          i += 1
        else
          val start = i
          i += 1
          while i < str.length() && str.charAt(i) > 0x7f do
            i += 1
          appendPercentEncoded(out, str.substring(start, i).getBytes(Utf8))
      out.toString()

  private def appendPercentEncoded(out: java.lang.StringBuilder, bytes: Array[Byte]): Unit =
    var i = 0
    while i < bytes.length do
      val value = bytes(i) & 0xff
      out.append('%')
      if value <= 0x0f then out.append('0')
      out.append(Integer.toHexString(value).toUpperCase())
      i += 1

  private def caseInsensitiveCompare(x: String | Null, y: String | Null): Int =
    if x == null then
      if y == null then 0 else -1
    else if y == null then 1
    else x.compareToIgnoreCase(y)

  private def escapeAwareCompare(x: String | Null, y: String | Null): Int =
    if x == null then
      if y == null then 0 else -1
    else if y == null then 1
    else
      val xx = x.asInstanceOf[String]
      val yy = y.asInstanceOf[String]
      var i = 0
      while i < xx.length() && i < yy.length() do
        val diff = xx.charAt(i) - yy.charAt(i)
        if diff != 0 then
          return diff
        else if xx.charAt(i) == '%' then
          val cmp = xx.substring(i + 1, i + 3).compareToIgnoreCase(yy.substring(i + 1, i + 3))
          if cmp != 0 then
            return cmp
          i += 3
        else
          i += 1
      xx.length() - yy.length()

  private def normalizeEscapes(str: String | Null): String | Null =
    if str == null then
      null
    else
      val out = new java.lang.StringBuilder()
      var i = 0
      while i < str.length() do
        if str.charAt(i) == '%' then
          out.append(str.substring(i, i + 3).toUpperCase())
          i += 3
        else
          out.append(str.charAt(i))
          i += 1
      out.toString()
