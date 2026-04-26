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
  */
@SerialVersionUID(1L)
final class TrieMap[K, V] private (private val underlying: mutable.HashMap[K, V])
  extends scala.collection.mutable.AbstractMap[K, V]
    with scala.collection.concurrent.Map[K, V]
    with scala.collection.mutable.MapOps[K, V, TrieMap, TrieMap[K, V]]
    with scala.collection.MapFactoryDefaults[K, V, TrieMap, mutable.Iterable]
    with DefaultSerializable {

  def this() = this(new mutable.HashMap[K, V])

  override def mapFactory: MapFactory[TrieMap] = TrieMap

  override protected def className: String = "TrieMap"

  // --- core mutable.Map API ---------------------------------------------

  override def get(key: K): Option[V] = underlying.get(key)

  override def iterator: Iterator[(K, V)] = underlying.iterator

  override def size: Int = underlying.size

  override def knownSize: Int = underlying.knownSize

  override def isEmpty: Boolean = underlying.isEmpty

  override def contains(key: K): Boolean = underlying.contains(key)

  override def apply(key: K): V = underlying.apply(key)

  override def put(key: K, value: V): Option[V] = underlying.put(key, value)

  override def update(key: K, value: V): Unit = underlying.update(key, value)

  override def remove(key: K): Option[V] = underlying.remove(key)

  def addOne(kv: (K, V)): this.type = {
    underlying.addOne(kv)
    this
  }

  def subtractOne(key: K): this.type = {
    underlying.subtractOne(key)
    this
  }

  override def clear(): Unit = underlying.clear()

  override def clone(): TrieMap[K, V] = new TrieMap(underlying.clone())

  // --- concurrent.Map API -----------------------------------------------

  def putIfAbsent(key: K, value: V): Option[V] = underlying.get(key) match {
    case some @ Some(_) => some
    case None =>
      underlying.update(key, value)
      None
  }

  def remove(key: K, value: V): Boolean = underlying.get(key) match {
    case Some(v) if v == value =>
      underlying.remove(key)
      true
    case _ => false
  }

  def replace(key: K, oldValue: V, newValue: V): Boolean = underlying.get(key) match {
    case Some(v) if v == oldValue =>
      underlying.update(key, newValue)
      true
    case _ => false
  }

  def replace(key: K, value: V): Option[V] = underlying.get(key) match {
    case some @ Some(_) =>
      underlying.update(key, value)
      some
    case None => None
  }

  // --- snapshot helpers (single-threaded: just return clones) -----------

  /** Returns an independent (cloned) copy of this map, since the Python
    *  backend is single-threaded and no concurrent rewriting is needed.
    */
  def snapshot(): TrieMap[K, V] = new TrieMap(underlying.clone())

  /** Returns a read-only view of a snapshot of this map. */
  def readOnlySnapshot(): scala.collection.Map[K, V] = underlying.clone()

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
}
