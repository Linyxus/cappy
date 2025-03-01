import cappy.*
import io.*

@main def hello(): Unit =
  val source = SourceFile("test", """
(x: Int) => (y: Int) => add(x, (x: Int) => y)
""".strip())
  Compiler.compile(source)

  