package cavia
package rechecking

import core.*
import ast.Expr
import typechecking.*
import TypeChecker.Context

object FindTailCall extends Rechecker:
  import Expr.*
  val IsTailCall = new MetaKey:
    type Value = Unit

  extension (self: Term)
    def confirm: Term =
      self.meta.put(IsTailCall, ())
      self
