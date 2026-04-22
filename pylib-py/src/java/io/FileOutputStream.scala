package java.io

class FileOutputStream(file: File) extends OutputStream {
  def this(name: String) =
    this(new File(name))

  override def write(b: Int): Unit =
    throw new UnsupportedOperationException("FileOutputStream is not yet supported on ScalaPy")
}
