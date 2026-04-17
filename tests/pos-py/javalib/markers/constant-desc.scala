final class MarkerConstantDesc extends java.lang.constant.ConstantDesc

@main def markersConstantDesc(): Unit =
  println("constant-desc:" + (new MarkerConstantDesc).isInstanceOf[java.lang.constant.ConstantDesc])
