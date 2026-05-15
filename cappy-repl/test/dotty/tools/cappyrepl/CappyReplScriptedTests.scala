package dotty.tools.cappyrepl

import org.junit.Test

/** File-driven Cappy REPL tests.
 *
 *  Each file under `cappy-repl/test-resources/cappy-repl/` is both the
 *  input script and its own expected transcript. Lines that start with
 *  `cappy> ` are inputs; everything else is expected output. The test
 *  feeds each input through a fresh driver + Python subprocess and
 *  diffs the resulting (input + EOL + captured output) sequence against
 *  the file content.
 *
 *  Selective run: `sbt -Dcappy.tests.filter=vals cappy-repl/test`.
 *
 *  Modelled on `dotty.tools.repl.ScriptedTests`.
 */
class CappyReplScriptedTests extends CappyReplTest:

  @Test def scriptedTests(): Unit =
    testFiles("test-resources/cappy-repl")
end CappyReplScriptedTests
