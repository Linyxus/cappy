package dotty.tools.backend.python.ir

// ─── Position ──────────────────────────────────────────────────────────

case class PyPos(source: String, line: Int, column: Int)

object PyPos:
  val No: PyPos = PyPos("", 0, 0)

// ─── Names ─────────────────────────────────────────────────────────────

case class PyName(value: String) extends AnyVal

case class QualName(parts: List[String]):
  def dot(n: String): QualName = QualName(parts :+ n)
  override def toString: String = parts.mkString(".")

object QualName:
  def apply(s: String): QualName = QualName(s.split('.').toList)

// ─── Module ────────────────────────────────────────────────────────────

case class PyModule(
    name: QualName,
    imports: List[PyImport],
    defs: List[PyTopLevelDef],
    initStmts: List[PyStmt],
    pos: PyPos
)

// ─── Imports ───────────────────────────────────────────────────────────

enum PyImport:
  case Import(module: QualName, alias: Option[PyName])
  case FromImport(module: QualName, names: List[(PyName, Option[PyName])])

// ─── Top-level definitions ─────────────────────────────────────────────

enum PyTopLevelDef:
  case TLClassDef(cls: PyClassDef)
  case TLFuncDef(func: PyFuncDef)
  case TLAssign(target: PyName, value: PyExpr)

// ─── Class ─────────────────────────────────────────────────────────────

enum PyClassKind:
  case Class
  case ModuleClass
  case Trait

case class PyClassDef(
    name: PyName,
    kind: PyClassKind,
    bases: List[PyExpr],
    members: List[PyClassMember],
    pos: PyPos
)

enum PyClassMember:
  case Method(func: PyFuncDef, decorators: List[PyDecorator])
  case FieldInit(target: PyExpr, value: PyExpr)
  case NestedClass(cls: PyClassDef)
  case Stmt(stmt: PyStmt)
  case Pass

enum PyDecorator:
  case StaticMethod
  case ClassMethod
  case Property
  case Custom(expr: PyExpr)

// ─── Function ──────────────────────────────────────────────────────────

case class PyFuncDef(
    name: PyName,
    params: List[PyParam],
    body: List[PyStmt],
    returnType: Option[PyTypeAnnot],
    pos: PyPos
)

case class PyParam(
    name: PyName,
    typeAnnot: Option[PyTypeAnnot],
    default: Option[PyExpr]
)

// ─── Statements ────────────────────────────────────────────────────────

enum PyStmt:
  case ExprStmt(expr: PyExpr)
  case Assign(target: PyExpr, value: PyExpr)
  case AugAssign(target: PyExpr, op: PyBinOp, value: PyExpr)
  case Return(value: Option[PyExpr])
  case If(cond: PyExpr, body: List[PyStmt], elifs: List[(PyExpr, List[PyStmt])], elseBody: Option[List[PyStmt]])
  case While(cond: PyExpr, body: List[PyStmt])
  case For(target: PyExpr, iter: PyExpr, body: List[PyStmt])
  case Try(body: List[PyStmt], handlers: List[PyExceptHandler], elseBody: Option[List[PyStmt]], finallyBody: Option[List[PyStmt]])
  case Raise(exception: Option[PyExpr])
  case Assert(test: PyExpr, msg: Option[PyExpr])
  case Pass
  case Break
  case Continue
  case Del(target: PyExpr)
  case VarDef(name: PyName, tpe: Option[PyTypeAnnot], mutable: Boolean, rhs: PyExpr)
  case Block(stmts: List[PyStmt])
  case Labeled(label: PyName, body: List[PyStmt])
  case LabelReturn(label: PyName, value: PyExpr)
  case FuncDefStmt(func: PyFuncDef)
  case ClassDefStmt(cls: PyClassDef)

case class PyExceptHandler(
    exnType: Option[PyExpr],
    name: Option[PyName],
    body: List[PyStmt],
    pos: PyPos
)

// ─── Expressions ───────────────────────────────────────────────────────

enum PyExpr:
  // Literals
  case NoneLit
  case BoolLit(value: Boolean)
  case IntLit(value: Long)
  case FloatLit(value: Double)
  case StringLit(value: String)

  // Names & access
  case Name(name: PyName)
  case QualRef(qual: QualName)
  case Attr(obj: PyExpr, name: PyName)
  case Subscript(obj: PyExpr, index: PyExpr)

  // Calls
  case Call(func: PyExpr, args: List[PyExpr])
  case New(cls: PyExpr, args: List[PyExpr])

  // Operators
  case BinOp(op: PyBinOp, lhs: PyExpr, rhs: PyExpr)
  case UnaryOp(op: PyUnaryOp, operand: PyExpr)
  case BoolOp(op: PyBoolOp, values: List[PyExpr])
  case Compare(left: PyExpr, ops: List[PyCmpOp], rights: List[PyExpr])

  // Conditionals & lambdas
  case IfExpr(cond: PyExpr, thenExpr: PyExpr, elseExpr: PyExpr)
  case Lambda(params: List[PyParam], body: PyExpr)

  // Collections
  case TupleLit(elems: List[PyExpr])
  case ListLit(elems: List[PyExpr])
  case DictLit(keys: List[PyExpr], values: List[PyExpr])

  // Scala-specific: type ops
  case IsInstance(obj: PyExpr, cls: PyExpr)
  case Cast(obj: PyExpr, cls: PyExpr)

  // Scala-specific: object model
  case LoadModule(qualName: QualName)
  case SuperRef(self: PyExpr, parentClass: Option[PyExpr])
  case This

  // Scala-specific: numeric wrapping
  case IntWrap32(expr: PyExpr)
  case IntWrap64(expr: PyExpr)
  case FloatWrap(expr: PyExpr)

  // Scala-specific: strings
  case StringConcat(parts: List[PyExpr])

  // Scala-specific: arrays
  case NewArray(length: PyExpr, init: PyExpr)
  case ArrayLit(elems: List[PyExpr])
  case ArraySelect(array: PyExpr, index: PyExpr)
  case ArrayLength(array: PyExpr)

  // Scala-specific: exceptions
  case WrapThrowable(expr: PyExpr)
  case UnwrapThrowable(expr: PyExpr)

// ─── Operators ─────────────────────────────────────────────────────────

enum PyBinOp:
  case Add, Sub, Mul, Div, FloorDiv, Mod, Pow
  case BitOr, BitAnd, BitXor, LShift, RShift

enum PyUnaryOp:
  case Not, Neg, Pos, Invert

enum PyBoolOp:
  case And, Or

enum PyCmpOp:
  case Eq, NotEq, Lt, LtE, Gt, GtE, Is, IsNot

// ─── Type annotations ──────────────────────────────────────────────────

enum PyTypeAnnot:
  case Named(name: String)
  case Qualified(qual: QualName)
  case Parameterized(base: PyTypeAnnot, args: List[PyTypeAnnot])
  case Union(members: List[PyTypeAnnot])
  case Optional(inner: PyTypeAnnot)
  case Any
