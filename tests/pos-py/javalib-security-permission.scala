import java.security.{AccessControlException, Permission}

final class NamedPermission(val permissionName: String, val actions: String) extends Permission(permissionName):
  override def checkGuard(o: Any): Unit = ()

  override def implies(p: Permission): Boolean =
    p match
      case other: NamedPermission =>
        permissionName == other.permissionName && actions == other.actions
      case _ =>
        false

  override def equals(obj: Any): Boolean =
    obj match
      case other: NamedPermission =>
        permissionName == other.permissionName && actions == other.actions
      case _ =>
        false

  override def hashCode(): Int =
    31 * permissionName.hashCode() + actions.hashCode()

  override def getActions(): String = actions

@main def javalibSecurityPermission(): Unit =
  val perm = new NamedPermission("read", "rw")
  val same = new NamedPermission("read", "rw")
  val other = new NamedPermission("write", "rw")

  println("perm:" + perm.permissionName + ":" + perm.getActions())
  println("tostring:" + (perm.toString() == s"ClassName ${perm.getClass().getName()} ${perm.getActions()}"))
  println("implies:" + perm.implies(same))
  println("implies:" + perm.implies(other))

  val ace = new AccessControlException("denied", perm)
  val acePerm = ace.getPermission().asInstanceOf[NamedPermission]
  println("ace:" + ace.getMessage() + ":" + acePerm.permissionName)
