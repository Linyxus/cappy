package dotty.tools.benchmarks.py.interp

/** A regex-lite NFA simulated with an active-state set over a generated
 *  input. No ADT in the hot path — pure nested `while` loops with flat
 *  `Array[Int]`/`Array[Boolean]` indexing. */
class NFAMatcherBench:
  var size: Int = 0
  var nStates: Int = 0
  var transitions: Array[Int] = Array.empty // flat: state * 26 + char -> next (-1 dead)
  var accept: Array[Boolean] = Array.empty
  var input: Array[Int] = Array.empty       // char ordinals 0..25

  def setup(size: Int): Unit =
    this.size = size
    val n = math.max(2, size / 4)
    nStates = n
    val tr = new Array[Int](n * 26)
    var i = 0
    while i < tr.length do { tr(i) = -1; i += 1 }
    // Chain: state s on char (s % 26) advances to s+1; also a self-loop on
    // char 0 so several states can be simultaneously active.
    var s = 0
    while s < n - 1 do
      tr(s * 26 + (s % 26)) = s + 1
      tr(s * 26 + 0) = s
      s += 1
    transitions = tr
    val acc = new Array[Boolean](n)
    acc(n - 1) = true
    accept = acc
    val in = new Array[Int](size)
    var k = 0
    while k < size do { in(k) = (k % n) % 26; k += 1 }
    input = in

  val operations: Map[String, () => Any] = Map(
    "simulate" -> { () =>
      var cur = new Array[Boolean](nStates)
      var nxt = new Array[Boolean](nStates)
      cur(0) = true
      var i = 0
      while i < input.length do
        val c = input(i)
        var s = 0
        while s < nStates do
          if cur(s) then
            val ns = transitions(s * 26 + c)
            if ns >= 0 then nxt(ns) = true
          s += 1
        // swap buffers and clear the old one
        val tmp = cur
        cur = nxt
        nxt = tmp
        var k = 0
        while k < nStates do { nxt(k) = false; k += 1 }
        i += 1
      var matched = false
      var s = 0
      while s < nStates do
        if cur(s) && accept(s) then matched = true
        s += 1
      matched
    },
  )

@main def main(): Unit = ()
