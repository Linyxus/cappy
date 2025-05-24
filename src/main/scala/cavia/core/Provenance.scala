package cavia
package core

import reporting.*

sealed trait Provenance:
  def exists: Boolean
case object NoProvenance extends Provenance:
  def exists: Boolean = false
case class TypeVarProvenance(
  typeParamName: String,
  createdAt: SourcePos,
) extends Provenance:
  def exists: Boolean = true

trait HasProvenance:
  private var _provenance: Provenance = NoProvenance
  def provenance: Provenance = _provenance
  def setProvenance(p: Provenance): Unit = _provenance = p
  def withProvenance(p: Provenance): this.type =
    setProvenance(p)
    this
