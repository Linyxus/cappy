package java.util.concurrent

import java.util.Random

class ThreadLocalRandom extends Random:
  private var initialized = false
  initialized = true

  override def setSeed(seed: Long): Unit =
    if initialized then
      throw new UnsupportedOperationException()
    super.setSeed(seed)

object ThreadLocalRandom:
  private final class LocalRandomHolder extends ThreadLocal[ThreadLocalRandom]:
    override def initialValue(): ThreadLocalRandom =
      new ThreadLocalRandom()

  private var localRandom0: LocalRandomHolder | Null = null

  def current(): ThreadLocalRandom =
    val holder = localRandom0
    if holder != null then
      holder.asInstanceOf[LocalRandomHolder].get()
    else
      val created = new LocalRandomHolder()
      localRandom0 = created
      created.get()
