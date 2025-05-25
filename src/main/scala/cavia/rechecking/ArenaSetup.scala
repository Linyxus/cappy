package cavia
package rechecking

import core.*
import ast.expr.Expr
import Expr.*
import typechecking.*
import TypeChecker.Context

class ArenaSetup extends Rechecker:
  override def recheck(expr: Term)(using Context): Term =
    super.recheck(expr)
