package java.util.concurrent

import java.util.Map
import java.util.function.{BiConsumer, BiFunction, Function}

trait ConcurrentMap[K, V] extends Map[K, V]:
  def putIfAbsent(key: K, value: V): V
  def remove(key: Any, value: Any): Boolean
  def replace(key: K, oldValue: V, newValue: V): Boolean
  def replace(key: K, value: V): V

  // Re-declare Map's default methods on ConcurrentMap so they appear in
  // ConcurrentMap's own pyir. The linker's `requireExactInstanceMethod`
  // only checks own methods (not inherited), so without these
  // re-declarations stdlib calls like `ConcurrentMap.compute` would not
  // resolve. Body delegates to super (the Map default).
  override def getOrDefault(key: Any, defaultValue: V): V =
    super.getOrDefault(key, defaultValue)
  override def forEach(action: BiConsumer[_ >: K, _ >: V]): Unit =
    super.forEach(action)
  override def replaceAll(function: BiFunction[_ >: K, _ >: V, _ <: V]): Unit =
    super.replaceAll(function)
  override def computeIfAbsent(key: K, mappingFunction: Function[_ >: K, _ <: V]): V =
    super.computeIfAbsent(key, mappingFunction)
  override def computeIfPresent(key: K, remappingFunction: BiFunction[_ >: K, _ >: V, _ <: V]): V =
    super.computeIfPresent(key, remappingFunction)
  override def compute(key: K, remappingFunction: BiFunction[_ >: K, _ >: V, _ <: V]): V =
    super.compute(key, remappingFunction)
  override def merge(key: K, value: V, remappingFunction: BiFunction[_ >: V, _ >: V, _ <: V]): V =
    super.merge(key, value, remappingFunction)
