package cavia
import typechecking.*
import java.nio.file.*
import cavia.reporting.Printer

class TypeCheckingSuite extends munit.FunSuite:
  def writeOutput(content: String, path: Path): Unit =
    if Files.exists(path) then
      Files.delete(path)
    Files.createFile(path)
    Files.writeString(path, content)

  for testCase <- loadAllTestcases("tests/pos") do
    test(s"pos typechecking test: ${testCase.path.getFileName}"):
      val parsed = Compiler.parse(testCase.source)
      assert(parsed.isInstanceOf[Compiler.ParseResult.Ok])
      val defs = parsed.asInstanceOf[Compiler.ParseResult.Ok].result
      val typedModule = Compiler.typecheck(defs)
      assert(typedModule.isRight)
      val module = typedModule.getOrElse(???)
      if testCase.checkFile.isDefined then
        val expected = testCase.checkFile.get
        val actualStr = ExprPrinter.show(module)(using TypeChecker.Context.empty)
        val outputPath = testCase.path.resolveSibling(testCase.path.getFileName.toString.replace(".scala", ".actual"))
        val expectedPath = testCase.path.resolveSibling(testCase.path.getFileName.toString.replace(".scala", ".check"))
        writeOutput(actualStr, outputPath)
        val hintStr = s"To overwrite the expected output, run\n\n  mv ${outputPath.toString} ${expectedPath.toString}\n\n"
        assertEquals(actualStr.strip, expected.strip, hintStr)

  for testCase <- loadAllTestcases("tests/neg") do
    test(s"neg typechecking test: ${testCase.path.getFileName}"):
      val parsed = Compiler.parse(testCase.source)
      assert(parsed.isInstanceOf[Compiler.ParseResult.Ok])
      val defs = parsed.asInstanceOf[Compiler.ParseResult.Ok].result
      Compiler.typecheck(defs) match
        case Left(err) => 
          testCase.checkFile match
            case None =>
            case Some(expectedStr) =>
              val actualStr = err.asMessage.show
              val outputPath = testCase.path.resolveSibling(testCase.path.getFileName.toString.replace(".scala", ".actual"))
              val expectedPath = testCase.path.resolveSibling(testCase.path.getFileName.toString.replace(".scala", ".check"))
              writeOutput(actualStr, outputPath)
              val hintStr = s"To overwrite the expected output, run\n\n  mv ${outputPath.toString} ${expectedPath.toString}\n\n"
              assertEquals(actualStr.strip, expectedStr.strip, hintStr)
        case Right(module) =>
          fail(s"Typechecking should have failed for ${testCase.path.getFileName}")
