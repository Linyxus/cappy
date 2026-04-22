package java.security

import java.lang.SecurityException

class AccessControlException(s: String, p: Permission | Null = null) extends SecurityException(s):
  def getPermission(): Permission | Null = p
