import cappy.*
import core.*
import io.*
import tokenizer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val x = (x: Int) => 
  x
val y = (y: Int) =>
  x(y)
""")
  val tokens = Tokenizer.tokenize(source)
  tokens.foreach(println)
