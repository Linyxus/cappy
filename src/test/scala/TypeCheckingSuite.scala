package cavia

class TypeCheckingSuite extends munit.FunSuite:
  for source <- loadAllSourceFiles("tests/pos") do
    test(s"pos typechecking test: ${source.name}"):
      val parsed = Compiler.parse(source)
      assert(parsed.isInstanceOf[Compiler.ParseResult.Ok])
      val defs = parsed.asInstanceOf[Compiler.ParseResult.Ok].result
      val typedModule = Compiler.typecheck(defs)
      assert(typedModule.isRight)
