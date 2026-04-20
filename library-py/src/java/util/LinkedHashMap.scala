package java.util

class LinkedHashMap[K, V](initialCapacity: Int, loadFactor: Float, accessOrder: Boolean)
    extends HashMap[K, V](initialCapacity, loadFactor) with SequencedMap[K, V]:

  def this(initialCapacity: Int, loadFactor: Float) =
    this(initialCapacity, loadFactor, false)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR, false)

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY, HashMap.DEFAULT_LOAD_FACTOR, false)

  def this(m: Map[_ <: K, _ <: V]) =
    this(m.size())
    putAll(m)

  override protected[util] def nodeWasAccessed(node: HashMap.Node[K, V]): Unit =
    if accessOrder then
      moveToTail(node)

  override protected[util] def nodeWasAdded(node: HashMap.Node[K, V]): Unit =
    super.nodeWasAdded(node)
    val eldestEntry = eldestNode()
    if eldestEntry != null && removeEldestEntry(eldestEntry) then
      removeNode(eldestEntry)

  protected def removeEldestEntry(eldest: Map.Entry[K, V]): Boolean =
    false

  override def clone(): AnyRef =
    val result = new LinkedHashMap[K, V](size(), HashMap.DEFAULT_LOAD_FACTOR, accessOrder)
    result.putAll(this)
    result
