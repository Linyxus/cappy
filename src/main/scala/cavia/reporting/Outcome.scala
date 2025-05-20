package cavia
package reporting

/** Outcome of a compilation step. */
trait Outcome[+A]:
  def getOption: Option[A]
  def getMessages: List[Message]

  def isSuccess: Boolean = getOption.isDefined
  def get: A = getOption.get

  def moreMessages(msgs: List[Message]): Outcome[A] =
    val self = this
    new Outcome[A]:
      def getOption = self.getOption
      def getMessages = self.getMessages ++ msgs

  def toFailure: Outcome[Nothing] =
    val self = this
    new Outcome[Nothing]:
      def getOption = None
      def getMessages = self.getMessages

  def andThen[B](other: A => Outcome[B]): Outcome[B] =
    getOption match
      case None => this.toFailure
      case Some(a) => other(a).moreMessages(getMessages)
    
object Outcome:
  def simpleSuccess[A](a: A): Outcome[A] =
    new Outcome[A]:
      def getOption = Some(a)
      def getMessages = Nil

  def simpleFailure[A](msg: Message): Outcome[A] =
    new Outcome[A]:
      def getOption = None
      def getMessages = List(msg)
