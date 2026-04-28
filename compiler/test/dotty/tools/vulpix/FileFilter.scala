package dotty.tools.vulpix

sealed trait FileFilter {
  def accept(file: String): Boolean
}

object FileFilter {
  def exclude(file: String): FileFilter = exclude(file :: Nil)

  def exclude(file: String, files: String*): FileFilter =
    exclude(file :: files.toList)

  def exclude(files: List[String]): FileFilter = new FileFilter {
    private val excluded = files.toSet
    def accept(file: String): Boolean = !excluded.contains(file)
  }

  def include(files: List[String]): FileFilter = new FileFilter {
    private val included = files.toSet
    def accept(file: String): Boolean = included.contains(file)
  }

  def predicate(pred: String => Boolean): FileFilter = new FileFilter {
    def accept(file: String): Boolean = pred(file)
  }

  def and(a: FileFilter, b: FileFilter): FileFilter = new FileFilter {
    def accept(file: String): Boolean = a.accept(file) && b.accept(file)
  }

  object NoFilter extends FileFilter {
    def accept(file: String) = true
  }
}
