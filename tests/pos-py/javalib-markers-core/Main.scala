final class MarkerResource extends java.io.Closeable, java.io.Flushable:
  var closeCount = 0
  var flushCount = 0

  def close(): Unit =
    closeCount += 1

  def flush(): Unit =
    flushCount += 1

final class MarkerTask extends java.lang.Runnable:
  var runCount = 0

  def run(): Unit =
    runCount += 1

final class MarkerAnnotation extends java.lang.annotation.Annotation:
  def annotationType(): Class[? <: java.lang.annotation.Annotation] =
    null

final class MarkerBundle extends java.lang.Cloneable, java.io.Serializable,
    java.util.RandomAccess, java.lang.constant.Constable, java.lang.constant.ConstantDesc

@main def javalibMarkersCore(): Unit =
  val resource = new MarkerResource
  val autoCloseable: java.lang.AutoCloseable = resource
  val closeable: java.io.Closeable = resource
  val flushable: java.io.Flushable = resource
  flushable.flush()
  autoCloseable.close()
  closeable.close()
  println(
    "resource:" + resource.closeCount + ":" + resource.flushCount + ":" +
      resource.isInstanceOf[java.lang.AutoCloseable]
  )

  val task = new MarkerTask
  val runnable: java.lang.Runnable = task
  runnable.run()
  task.run()
  println("runnable:" + task.runCount)

  val bundle = new MarkerBundle
  println(
    "markers:" +
      bundle.isInstanceOf[java.lang.Cloneable] + ":" +
      bundle.isInstanceOf[java.io.Serializable] + ":" +
      bundle.isInstanceOf[java.util.RandomAccess] + ":" +
      bundle.isInstanceOf[java.lang.constant.Constable] + ":" +
      bundle.isInstanceOf[java.lang.constant.ConstantDesc]
  )

  val annotation: java.lang.annotation.Annotation = new MarkerAnnotation
  println(
    "annotation:" + (annotation.annotationType() == null) + ":" +
      annotation.isInstanceOf[java.lang.annotation.Annotation]
  )
