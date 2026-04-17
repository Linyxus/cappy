final class MarkerCloneable extends java.lang.Cloneable

@main def markersCloneable(): Unit =
  println("cloneable:" + (new MarkerCloneable).isInstanceOf[java.lang.Cloneable])
