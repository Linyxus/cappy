package scala.python

import scala.annotation.StaticAnnotation

final class extern(val module: String, val path: String*) extends StaticAnnotation

final class name(val value: String) extends StaticAnnotation
