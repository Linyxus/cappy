package cavia
package io
import scala.sys.process.*

object ShellUtils:
  enum ShellResult:
    case Ok(msg: String)
    case Err(msg: String, code: Int)

    def isOk: Boolean = this match
      case Ok(_) => true
      case _ => false

    def exitCode: Int = this match
      case Ok(_) => 0
      case Err(_, code) => code

  def executableExists(executable: String): Boolean =
    execute("which", executable).isOk

  def execute(cmd: String*): ShellResult =
    val stdOut = new StringBuilder
    val stdErr = new StringBuilder
    val logger = ProcessLogger(
      (s: String) => stdOut.append(s),
      (s: String) => stdErr.append(s),
    )
    val code = cmd ! logger
    if code == 0 then
      ShellResult.Ok(stdOut.toString)
    else
      ShellResult.Err(stdErr.toString, code)
