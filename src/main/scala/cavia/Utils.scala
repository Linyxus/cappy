package cavia

import scala.util.boundary, boundary.Label, boundary.break

def hopefully[E, X, R](op: Label[Either[E, X]] ?=> X): Either[E, X] = boundary:
  Right(op)

extension [E, X](mx: Either[E, X])
  def !![R](using l: Label[Either[E, R]]): X = mx match
    case Left(err) => break(Left(err))
    case Right(value) => value
  
