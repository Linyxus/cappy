package dotty.tools.benchmarks.py.interp

/** A Brainfuck-subset interpreter (no I/O) over a generated program.
 *  The hot loop dispatches on `Char` opcode literals; a precomputed jump
 *  table resolves `[`/`]` branches. */
class BrainfuckBench:
  var size: Int = 0
  var program: Array[Char] = Array.empty
  var jumpTable: Array[Int] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    // Build: set cell0 high, then a counted loop that moves work around,
    // repeated so total executed ops scale with `size`.
    val sb = new StringBuilder()
    var i = 0
    while i < 50 do { sb.append('+'); i += 1 }
    val reps = math.max(1, size / 8)
    var r = 0
    while r < reps do
      sb.append('[')
      sb.append('>')
      sb.append('+')
      sb.append('<')
      sb.append('-')
      sb.append(']')
      sb.append('>')
      sb.append('[')
      sb.append('<')
      sb.append('+')
      sb.append('>')
      sb.append('-')
      sb.append(']')
      sb.append('<')
      r += 1
    val prog = sb.toString.toCharArray
    program = prog
    // Precompute matching-bracket jump table.
    val jt = new Array[Int](prog.length)
    val stack = new Array[Int](prog.length)
    var sp = 0
    var k = 0
    while k < prog.length do
      if prog(k) == '[' then
        stack(sp) = k; sp += 1
      else if prog(k) == ']' then
        sp -= 1
        val open = stack(sp)
        jt(open) = k
        jt(k) = open
      k += 1
    jumpTable = jt

  val operations: Map[String, () => Any] = Map(
    "run" -> { () =>
      val tape = new Array[Int](256)
      var dp = 0
      var ip = 0
      while ip < program.length do
        program(ip) match
          case '+' => tape(dp) += 1
          case '-' => tape(dp) -= 1
          case '>' => dp += 1
          case '<' => dp -= 1
          case '[' => if tape(dp) == 0 then ip = jumpTable(ip)
          case ']' => if tape(dp) != 0 then ip = jumpTable(ip)
          case _   => ()
        ip += 1
      tape(0) + tape(1) + tape(2)
    },
  )

@main def main(): Unit = ()
