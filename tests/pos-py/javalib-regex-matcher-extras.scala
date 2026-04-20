import java.lang.StringBuilder
import java.util.regex.Pattern

@main def javalibRegexMatcherExtras(): Unit =
  // appendReplacement with named group reference ${name}
  val named = Pattern.compile("(?<prefix>[A-Z]+)(?<suffix>[0-9]+)").matcher("AB12 CD34")
  val sbNamed = new StringBuilder()
  while named.find() do
    named.appendReplacement(sbNamed, "${suffix}-${prefix}")
  named.appendTail(sbNamed)
  println("named-replace:" + sbNamed.toString())

  // appendReplacement: literal $ and \ via backslash escapes
  val lit = Pattern.compile("x").matcher("axa")
  val sbLit = new StringBuilder()
  lit.find()
  lit.appendReplacement(sbLit, "\\$\\\\")
  lit.appendTail(sbLit)
  println("literal-escapes:" + sbLit.toString())

  // Possessive quantifier smoke: a*+ does not backtrack, so a*+a fails on "aaa"
  val possessive = Pattern.compile("a*+a").matcher("aaa")
  println("possessive:" + possessive.matches())

  // Atomic group smoke: (?>a|ab)c commits to "a", fails on "abc"
  val atomic = Pattern.compile("(?>a|ab)c").matcher("abc")
  println("atomic:" + atomic.matches())

  // Look-behind smoke: (?<=foo)bar matches only when preceded by "foo"
  val lb = Pattern.compile("(?<=foo)bar").matcher("foobar")
  println("look-behind:" + lb.find() + ":" + lb.group())
  val lbNo = Pattern.compile("(?<=foo)bar").matcher("xxbar")
  println("look-behind-no:" + lbNo.find())

  // Unicode property class: \p{L} matches any letter
  val prop = Pattern.compile("\\p{L}+").matcher("abc123xyz")
  val sbProp = new StringBuilder()
  while prop.find() do
    sbProp.append(prop.group())
    sbProp.append('|')
  println("property:" + sbProp.toString())
