final class MarkerRandomAccess extends java.util.RandomAccess

@main def markersRandomAccess(): Unit =
  println("random-access:" + (new MarkerRandomAccess).isInstanceOf[java.util.RandomAccess])
