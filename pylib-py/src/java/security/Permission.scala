package java.security

abstract class Permission(name: String) extends Guard with Serializable:
  def implies(p: Permission): Boolean
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def getActions(): String

  override def toString(): String =
    s"ClassName ${this.getClass().getName()} ${getActions()}"
