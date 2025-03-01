package cappy

class ParsingSuite extends munit.FunSuite:
  for source <- loadAllSourceFiles("tests/parsing/pos") do
    test(s"parsing test: ${source.name}"):
      val result = Compiler.parse(source)
      assert(result.isInstanceOf[Compiler.ParseResult.Ok])
