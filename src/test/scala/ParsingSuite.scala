package cavia

class ParsingSuite extends munit.FunSuite:
  for source <- loadAllSourceFiles("tests/parsing/pos") do
    test(s"pos parsing test: ${source.name}"):
      val result = Compiler.parse(source)
      assert(result.isInstanceOf[Compiler.ParseResult.Ok])
