@main def run(): Unit =
  // Range.map flowing through IterableOps.map -> IndexedSeqView.Map(self, f).
  // Pre-fix the closure failed the Function1 type guard at the
  // synthesized constructor dispatcher.
  val r = (0 until 4)
  val mapped = r.map(i => i * 10)
  mapped.foreach(println)

  // Iterator.map exercises the same Function1 dispatch via
  // Iterator__anon_9.
  val it = (0 until 3).iterator.map(_ + 1)
  while it.hasNext do println(it.next())

  // filter follows the same shape (Function1 -> SeqView.Filter).
  val filtered = (0 until 5).filter(_ > 1)
  filtered.foreach(println)
