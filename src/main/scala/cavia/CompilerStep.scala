package cavia
import reporting.*

/** A step in the compiler. */
abstract class CompilerStep[-A, +B]:
  def run(input: A): Outcome[B]
  def fuse[C](other: CompilerStep[B, C]): CompilerStep[A, C] =
    val self = this
    new CompilerStep[A, C]:
      def run(input: A): Outcome[C] =
        self.run(input).andThen: b =>
          other.run(b)
  def execute(input: A): Option[B] =
    val outcome = run(input)
    outcome.getMessages.foreach: msg =>
      println(msg.show)
    outcome.getOption
