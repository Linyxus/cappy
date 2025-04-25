struct ParserInfo(what: array[i32]^)
struct Parser(runParser: () => Unit, info: ParserInfo^)
def main(): Unit =
  val p = Parser(() => (), ParserInfo(newArray[i32](10, 0)))
  p.runParser()
  ()
