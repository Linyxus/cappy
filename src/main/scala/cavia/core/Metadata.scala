package cavia.core

import scala.collection.mutable
import java.util.IdentityHashMap
import scala.jdk.CollectionConverters._

trait MetaKey:
  type Value

class Metadata:
  private val _store = new IdentityHashMap[MetaKey, Any]().asScala

  def put(key: MetaKey, value: key.Value): Unit =
    _store += (key -> value)

  def get(key: MetaKey): Option[key.Value] =
    _store.get(key).map(_.asInstanceOf[key.Value])

  def apply(key: MetaKey): key.Value =
    get(key).get

  def contains(key: MetaKey): Boolean =
    _store.contains(key)

  def remove(key: MetaKey): Unit =
    _store -= key

trait HasMetadata:
  private var _metadata: Metadata = Metadata()
  def meta: Metadata = _metadata

  def setMeta(newMeta: Metadata): Unit =
    _metadata = newMeta

  def withMeta(newMeta: Metadata): this.type =
    setMeta(newMeta)
    this

  def withMetaFrom(other: HasMetadata): this.type =
    setMeta(other.meta)
    this
