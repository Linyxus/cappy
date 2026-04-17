final class MarkerSerializable extends java.io.Serializable

@main def markersSerializable(): Unit =
  println("serializable:" + (new MarkerSerializable).isInstanceOf[java.io.Serializable])
