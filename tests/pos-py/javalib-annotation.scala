final class MarkerAnnotation extends java.lang.annotation.Annotation:
  def annotationType(): Class[_ <: java.lang.annotation.Annotation] =
    null

@main def markersAnnotation(): Unit =
  println("annotation:" + ((new MarkerAnnotation).annotationType() == null))
