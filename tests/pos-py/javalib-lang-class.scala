trait Marker

class Base

class Derived extends Base, Marker

class NameCache extends java.lang.ClassValue[String]:
  var calls = 0

  override def computeValue(clazz: Class[?]): String =
    calls += 1
    clazz.getName() + "#" + calls

@main def javalibLangClass(): Unit =
  val stringCls = classOf[String]
  println("class-of:string:" + stringCls.getName() + ":" + stringCls.toString())

  val intCls = classOf[Int]
  println("class-of:int:" + intCls.getName() + ":" + intCls.isPrimitive() + ":" + intCls.toString())

  val arrayCls = classOf[scala.Array[Int]]
  println(
    "class-of:array:" +
      arrayCls.getName() + ":" +
      arrayCls.isArray() + ":" +
      arrayCls.getComponentType().getName() + ":" +
      arrayCls.toString()
  )

  val derivedCls = classOf[Derived]
  val interfaces = derivedCls.getInterfaces()
  println(
    "derived:" +
      derivedCls.getSuperclass().getName() + ":" +
      interfaces.length + ":" +
      interfaces(0).getName()
  )

  val derived = new Derived
  println("getclass:" + derived.getClass().getName())
  println(
    "isinstance:" +
      classOf[Base].isInstance(derived) + ":" +
      classOf[Marker].isInstance(derived) + ":" +
      classOf[Derived].isInstance(new Base)
  )
  println(
    "assignable:" +
      classOf[Base].isAssignableFrom(classOf[Derived]) + ":" +
      classOf[Marker].isAssignableFrom(classOf[Derived]) + ":" +
      classOf[Derived].isAssignableFrom(classOf[Base])
  )
  println(
    "array-assign:" +
      classOf[scala.Array[Object]].isAssignableFrom(classOf[scala.Array[String]]) + ":" +
      classOf[scala.Array[Int]].isAssignableFrom(classOf[scala.Array[Double]])
  )

  val stringArray = new scala.Array[String](1).asInstanceOf[AnyRef]
  println(
    "array-instance:" +
      classOf[scala.Array[String]].isInstance(stringArray) + ":" +
      classOf[scala.Array[Object]].isInstance(stringArray) + ":" +
      classOf[scala.Array[Int]].isInstance(stringArray)
  )

  val cache = new NameCache
  val first = cache.get(classOf[String])
  val second = cache.get(classOf[String])
  cache.remove(classOf[String])
  val third = cache.get(classOf[String])
  println("classvalue-cache:" + first + ":" + second + ":" + third + ":" + cache.calls)
  // Note: `Class.forName(String)` is plan-out-of-scope and is NOT
  // asserted here. It currently fails at **link time** (the method
  // isn't on the runtime Class provider), not at runtime — a neg-py
  // test would be the right home, but neg-py currently only covers
  // typer errors. Tracked as a coverage gap.
