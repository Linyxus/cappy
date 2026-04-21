package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyThreading:
  @extern("threading")
  private object threading extends PyDynamic

  @extern("builtins")
  private[runtime] object builtins extends PyAny:
    @name("hasattr")
    def hasAttr(obj: Any, name: String): Boolean = native

    @name("getattr")
    def getAttr(obj: Any, name: String): Any = native

    @name("delattr")
    def delAttr(obj: Any, name: String): Unit = native

  def timer(intervalMillis: Long, function: Any): PyTimer =
    new PyTimer(threading.Timer(intervalMillis.toDouble / 1000.0, function).asInstanceOf[PyDynamic])

  def newThread(target: Any, name: String | Null = null, daemon: Boolean = false): PyThread =
    val raw =
      if name == null then
        threading.Thread(target = target, daemon = daemon).asInstanceOf[PyDynamic]
      else
        threading.Thread(target = target, name = name.asInstanceOf[String], daemon = daemon).asInstanceOf[PyDynamic]
    new PyThread(raw)

  def currentThread(): PyThread =
    new PyThread(threading.current_thread().asInstanceOf[PyDynamic])

  def sleep(millis: Long): Unit =
    PyTime.sleep(millis.toDouble / 1000.0)

  def newEvent(): PyEvent =
    new PyEvent(threading.Event().asInstanceOf[PyDynamic])

  def newLock(): PyLock =
    new PyLock(threading.Lock().asInstanceOf[PyDynamic])

  def newRLock(): PyRLock =
    new PyRLock(threading.RLock().asInstanceOf[PyDynamic])

  def newCondition(lock: PyRLock): PyCondition =
    new PyCondition(threading.Condition(lock.underlying).asInstanceOf[PyDynamic])

  def newSemaphore(permits: Int): PySemaphore =
    new PySemaphore(threading.Semaphore(permits).asInstanceOf[PyDynamic])

  def newBarrier(parties: Int): PyBarrier =
    new PyBarrier(threading.Barrier(parties).asInstanceOf[PyDynamic])

  def newLocal(): PyLocal =
    new PyLocal(threading.local().asInstanceOf[PyDynamic])

final class PyTimer private[runtime] (private val underlying: PyDynamic):
  def start(): Unit =
    underlying.start()

  def cancel(): Unit =
    underlying.cancel()

final class PyThread private[runtime] (private[runtime] val underlying: PyDynamic):
  def start(): Unit =
    underlying.start()

  def join(): Unit =
    underlying.join()

  def join(timeoutMillis: Long): Unit =
    underlying.join(timeoutMillis.toDouble / 1000.0)

  def isAlive(): Boolean =
    underlying.is_alive().asInstanceOf[Boolean]

  def getName(): String =
    underlying.name.asInstanceOf[String]

  def setName(name: String): Unit =
    underlying.updateDynamic("name")(name)

  def isDaemon(): Boolean =
    underlying.daemon.asInstanceOf[Boolean]

  def setDaemon(daemon: Boolean): Unit =
    underlying.updateDynamic("daemon")(daemon)

  def getIdent(): Long | Null =
    underlying.ident.asInstanceOf[Long | Null]

final class PyEvent private[runtime] (private val underlying: PyDynamic):
  def set(): Unit =
    underlying.set()

  def clear(): Unit =
    underlying.clear()

  def waitReady(): Boolean =
    underlying.applyDynamic("wait")().asInstanceOf[Boolean]

  def waitReady(timeoutMillis: Long): Boolean =
    underlying.applyDynamic("wait")(timeoutMillis.toDouble / 1000.0).asInstanceOf[Boolean]

  def isSet(): Boolean =
    underlying.is_set().asInstanceOf[Boolean]

final class PyLock private[runtime] (private[runtime] val underlying: PyDynamic):
  def acquire(): Boolean =
    underlying.acquire().asInstanceOf[Boolean]

  def tryAcquire(): Boolean =
    underlying.applyDynamicNamed("acquire")(("blocking", false)).asInstanceOf[Boolean]

  def lock(): Unit =
    acquire()
    ()

  def release(): Unit =
    underlying.release()

  def unlock(): Unit =
    release()

final class PyRLock private[runtime] (private[runtime] val underlying: PyDynamic):
  def acquire(): Boolean =
    underlying.acquire().asInstanceOf[Boolean]

  def tryAcquire(): Boolean =
    underlying.applyDynamicNamed("acquire")(("blocking", false)).asInstanceOf[Boolean]

  def lock(): Unit =
    acquire()
    ()

  def release(): Unit =
    underlying.release()

  def unlock(): Unit =
    release()

final class PyCondition private[runtime] (private[runtime] val underlying: PyDynamic):
  def waitReady(): Boolean =
    underlying.applyDynamic("wait")().asInstanceOf[Boolean]

  def waitReady(timeoutMillis: Long): Boolean =
    underlying.applyDynamic("wait")(timeoutMillis.toDouble / 1000.0).asInstanceOf[Boolean]

  def notifyOne(): Unit =
    underlying.applyDynamic("notify")(1)

  def notifyAllThreads(): Unit =
    underlying.notify_all()

final class PySemaphore private[runtime] (private[runtime] val underlying: PyDynamic):
  def acquire(): Boolean =
    underlying.acquire().asInstanceOf[Boolean]

  def release(): Unit =
    underlying.release()

final class PyBarrier private[runtime] (private[runtime] val underlying: PyDynamic):
  def waitTurn(): Int =
    underlying.applyDynamic("wait")().asInstanceOf[Int]

final class PyLocal private[runtime] (private val underlying: PyDynamic):
  def hasAttr(name: String): Boolean =
    PyThreading.builtins.hasAttr(underlying, name)

  def getAttr(name: String): Any =
    PyThreading.builtins.getAttr(underlying, name)

  def setAttr(name: String, value: Any): Unit =
    underlying.updateDynamic(name)(value)

  def delAttr(name: String): Unit =
    PyThreading.builtins.delAttr(underlying, name)
