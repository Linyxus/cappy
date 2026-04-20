package java.io

class File(private val path: String) {
  def this(parent: String, child: String) =
    this(
      if (parent == null || parent.isEmpty) child
      else parent + "/" + child
    )

  def getPath(): String =
    path

  override def toString(): String =
    path
}
