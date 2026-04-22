package java.util.concurrent

import java.io.Serializable
import java.util.{AbstractQueue, Collection, Iterator, NoSuchElementException, Objects}
import java.util.Objects.requireNonNull

import scala.python.runtime.PyThreading

class ConcurrentLinkedQueue[E]() extends AbstractQueue[E] with java.util.Queue[E] with Serializable:
  import ConcurrentLinkedQueue._

  private val lock = PyThreading.newRLock()
  private var head: Node[E] | Null = null
  private var last: Node[E] | Null = null
  private var size0 = 0

  def this(c: Collection[_ <: E]) =
    this()
    addAll(c)

  override def offer(e: E): Boolean =
    lock.acquire()
    try
      val node = new Node[E](requireNonNull(e))
      val oldLast = last
      last = node
      size0 += 1
      if oldLast != null then
        oldLast.next = node
      else
        head = node
      true
    finally lock.release()

  override def add(e: E): Boolean =
    offer(e)

  override def poll(): E =
    lock.acquire()
    try poll0()
    finally lock.release()

  override def peek(): E =
    lock.acquire()
    try
      if head == null then null.asInstanceOf[E]
      else head.asInstanceOf[Node[E]].value
    finally lock.release()

  override def remove(): E =
    lock.acquire()
    try
      val value = poll0()
      if value == null then
        throw new NoSuchElementException()
      value
    finally lock.release()

  override def element(): E =
    lock.acquire()
    try
      if head == null then
        throw new NoSuchElementException()
      head.asInstanceOf[Node[E]].value
    finally lock.release()

  override def isEmpty(): Boolean =
    lock.acquire()
    try size0 == 0
    finally lock.release()

  override def size(): Int =
    lock.acquire()
    try size0
    finally lock.release()

  override def clear(): Unit =
    lock.acquire()
    try
      head = null
      last = null
      size0 = 0
    finally lock.release()

  override def iterator(): Iterator[E] =
    new SnapshotIterator(snapshotHead())

  private def poll0(): E =
    if head == null then
      null.asInstanceOf[E]
    else
      val oldHead = head.asInstanceOf[Node[E]]
      head = oldHead.next
      if head == null then
        last = null
      size0 -= 1
      oldHead.next = null
      oldHead.value

  private def snapshotHead(): Node[Node[E]] | Null =
    lock.acquire()
    try
      if head == null then
        null
      else
        val snapshotRoot = new Node[Node[E]](head.asInstanceOf[Node[E]])
        var source = head.asInstanceOf[Node[E]].next
        var current = snapshotRoot
        while source != null do
          val copied = new Node[Node[E]](source.asInstanceOf[Node[E]])
          current.next = copied
          current = copied
          source = source.asInstanceOf[Node[E]].next
        snapshotRoot
    finally lock.release()

  private def removeNode(target: Node[E] | Null): Unit =
    if target != null then
      lock.acquire()
      try
        if head.asInstanceOf[AnyRef] eq target.asInstanceOf[AnyRef] then
          poll0()
        else if head != null then
          var previous = head.asInstanceOf[Node[E]]
          var current = previous.next
          while current != null && (current.asInstanceOf[AnyRef] ne target.asInstanceOf[AnyRef]) do
            previous = current.asInstanceOf[Node[E]]
            current = current.asInstanceOf[Node[E]].next

          if current != null then
            size0 -= 1
            previous.next = current.asInstanceOf[Node[E]].next
            if last.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then
              last = previous
      finally lock.release()

  private final class SnapshotIterator(snapshotRoot: Node[Node[E]] | Null) extends Iterator[E]:
    private var nextNode = snapshotRoot
    private var lastNode: Node[Node[E]] | Null = null

    def hasNext(): Boolean =
      nextNode != null

    def next(): E =
      if nextNode == null then
        throw new NoSuchElementException()
      val node = nextNode.asInstanceOf[Node[Node[E]]]
      lastNode = node
      nextNode = node.next
      node.value.value

    override def remove(): Unit =
      if lastNode == null then
        throw new IllegalStateException()
      removeNode(lastNode.asInstanceOf[Node[Node[E]]].value)
      lastNode = null

object ConcurrentLinkedQueue:
  private final class Node[T](
      var value: T,
      var next: Node[T] | Null = null
  )
