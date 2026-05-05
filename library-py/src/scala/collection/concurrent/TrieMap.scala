/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package concurrent

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.GrowableBuilder
import scala.util.hashing.Hashing

/** A `TrieMap` override for the Python backend (`-scalapy`).
  *
  *  The upstream `scala.collection.concurrent.TrieMap` is implemented on top
  *  of four Java helper classes (`INodeBase`, `MainNode`, `CNodeBase`,
  *  `BasicNode`) plus `java.util.concurrent.atomic.AtomicReferenceFieldUpdater`,
  *  none of which are available as `.pyir` for the Python linker. The Python
  *  backend is single-threaded, so we replace `TrieMap` with a thin wrapper
  *  around `mutable.HashMap` that preserves the public API of
  *  `scala.collection.concurrent.Map` (`putIfAbsent`, `replace`, `remove(k,v)`,
  *  `apply`, `get`, `put`, `update`, `+=`, `-=`, `iterator`, `size`, etc.).
  *
  *  Atomicity is trivially provided by single-threaded execution: every
  *  operation on the underlying `mutable.HashMap` runs to completion before
  *  the next one starts.
  *
  *  Custom `Hashing` / `Equiv` (the `(hashf, ef)` ctor) is honoured at
  *  lookup time by wrapping every key in a `KeyBox` whose `hashCode` /
  *  `equals` route through the user-supplied functions. The default no-arg
  *  ctor passes `Hashing.default` / `Equiv.universal`, which round-trip
  *  through `K###` and `K#==` — preserving the original semantics of the
  *  thin-wrapper version.
  */
@SerialVersionUID(1L)
final class TrieMap[K, V] private (
    private val hashf: Hashing[K],
    private val ef: Equiv[K],
    private val underlying: mutable.HashMap[TrieMap.KeyBox[K], V]
)
  extends scala.collection.mutable.AbstractMap[K, V]
    with scala.collection.concurrent.Map[K, V]
    with scala.collection.mutable.MapOps[K, V, TrieMap, TrieMap[K, V]]
    with scala.collection.MapFactoryDefaults[K, V, TrieMap, mutable.Iterable]
    with DefaultSerializable {

  def this() =
    this(Hashing.default[K], Equiv.universal[K], new mutable.HashMap[TrieMap.KeyBox[K], V])

  def this(hashf: Hashing[K], ef: Equiv[K]) =
    this(hashf, ef, new mutable.HashMap[TrieMap.KeyBox[K], V])

  override def mapFactory: MapFactory[TrieMap] = TrieMap

  override protected def className: String = "TrieMap"

  private def box(k: K): TrieMap.KeyBox[K] = new TrieMap.KeyBox(k, hashf, ef)

  // --- core mutable.Map API ---------------------------------------------

  override def get(key: K): Option[V] = underlying.get(box(key))

  override def iterator: Iterator[(K, V)] =
    underlying.iterator.map { case (kb, v) => (kb.key, v) }

  override def size: Int = underlying.size

  override def knownSize: Int = underlying.knownSize

  override def isEmpty: Boolean = underlying.isEmpty

  override def contains(key: K): Boolean = underlying.contains(box(key))

  override def apply(key: K): V = underlying.apply(box(key))

  override def put(key: K, value: V): Option[V] = underlying.put(box(key), value)

  override def update(key: K, value: V): Unit = underlying.update(box(key), value)

  override def remove(key: K): Option[V] = underlying.remove(box(key))

  def addOne(kv: (K, V)): this.type = {
    underlying.update(box(kv._1), kv._2)
    this
  }

  def subtractOne(key: K): this.type = {
    underlying.remove(box(key))
    this
  }

  override def clear(): Unit = underlying.clear()

  override def clone(): TrieMap[K, V] = new TrieMap(hashf, ef, underlying.clone())

  // --- concurrent.Map API -----------------------------------------------

  def putIfAbsent(key: K, value: V): Option[V] = {
    val b = box(key)
    underlying.get(b) match {
      case some @ Some(_) => some
      case None =>
        underlying.update(b, value)
        None
    }
  }

  def remove(key: K, value: V): Boolean = {
    val b = box(key)
    underlying.get(b) match {
      case Some(v) if v == value =>
        underlying.remove(b)
        true
      case _ => false
    }
  }

  def replace(key: K, oldValue: V, newValue: V): Boolean = {
    val b = box(key)
    underlying.get(b) match {
      case Some(v) if v == oldValue =>
        underlying.update(b, newValue)
        true
      case _ => false
    }
  }

  def replace(key: K, value: V): Option[V] = {
    val b = box(key)
    underlying.get(b) match {
      case some @ Some(_) =>
        underlying.update(b, value)
        some
      case None => None
    }
  }

  // --- snapshot helpers (single-threaded: just return clones) -----------

  /** Returns an independent (cloned) copy of this map, since the Python
    *  backend is single-threaded and no concurrent rewriting is needed.
    */
  def snapshot(): TrieMap[K, V] = new TrieMap(hashf, ef, underlying.clone())

  /** Returns a read-only view of a snapshot of this map. The view is a
   *  plain `HashMap[K, V]` keyed by raw `K` (Hashing/Equiv are baked into
   *  the box; once unwrapped, raw `==` semantics apply).
   */
  def readOnlySnapshot(): scala.collection.Map[K, V] = {
    val snap = scala.collection.mutable.HashMap.empty[K, V]
    iterator.foreach { case (k, v) => snap += k -> v }
    snap
  }

  def isReadOnly: Boolean = false

  def nonReadOnly: Boolean = true
}

@SerialVersionUID(1L)
object TrieMap extends MapFactory[TrieMap] {

  def empty[K, V]: TrieMap[K, V] = new TrieMap[K, V]

  def from[K, V](it: IterableOnce[(K, V)]^): TrieMap[K, V] = {
    val m = new TrieMap[K, V]
    m ++= it
    m
  }

  def newBuilder[K, V]: mutable.GrowableBuilder[(K, V), TrieMap[K, V]] =
    new GrowableBuilder(empty[K, V])

  /** Wraps a key so the underlying `mutable.HashMap` routes
   *  `hashCode` / `equals` through the user-supplied `Hashing` /
   *  `Equiv`. The default ctor uses `Hashing.default` / `Equiv.universal`,
   *  which round-trip through `K###` / `K#==`, matching the original
   *  thin-wrapper semantics.
   */
  private[concurrent] final class KeyBox[K](
      val key: K,
      private val hashf: Hashing[K],
      private val ef: Equiv[K]
  ) {
    override def hashCode(): Int = hashf.hash(key)
    override def equals(that: Any): Boolean = that match {
      case other: KeyBox[?] => ef.equiv(key, other.key.asInstanceOf[K])
      case _                => false
    }
  }
}
