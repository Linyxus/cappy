package dotty.tools.backend.python.ir

// ─── Position ──────────────────────────────────────────────────────────

/** Source position attached to every IR node for error reporting and source maps. */
case class PyPos(source: String, line: Int, column: Int)

object PyPos:
  val No: PyPos = PyPos("", 0, 0)

// ─── Names ─────────────────────────────────────────────────────────────

/** A simple Python identifier. */
case class PyName(value: String) extends AnyVal

/** A qualified dotted name for module/class references. */
case class QualName(parts: List[String]):
  def dot(n: String): QualName = QualName(parts :+ n)
  override def toString: String = parts.mkString(".")

object QualName:
  def apply(s: String): QualName = QualName(s.split('.').toList)

// ─── Module (top-level compilation unit) ───────────────────────────────

/** A Python source file. One per Scala class/object after Flatten. */
case class PyModule(
    name: QualName,
    imports: List[PyImport],
    defs: List[PyTopLevelDef],
    initStmts: List[PyStmt], // module-level init (singleton creation, etc.)
    pos: PyPos
)

// ─── Imports ───────────────────────────────────────────────────────────

enum PyImport:
  /** `import foo.bar` or `import foo.bar as alias` */
  case Import(module: QualName, alias: Option[PyName])
  /** `from foo.bar import baz, quux as q` */
  case FromImport(module: QualName, names: List[(PyName, Option[PyName])])

// ─── Top-level definitions ─────────────────────────────────────────────

enum PyTopLevelDef:
  case TLClassDef(cls: PyClassDef)
  case TLFuncDef(func: PyFuncDef)
  /** Module-level assignment, e.g., `Foo_MODULE = Foo()` for singletons. */
  case TLAssign(target: PyName, value: PyExpr)

// ─── Class ─────────────────────────────────────────────────────────────

enum PyClassKind:
  case Class       // regular Scala class
  case ModuleClass // Scala object (singleton)
  case Trait       // Scala trait -> Python class for multiple inheritance

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

enum PyStmt(val pos: PyPos):

  // -- Core Python statements --

  /** Expression used as statement (function calls, etc.). */
  case ExprStmt(expr: PyExpr)(using p: PyPos) extends PyStmt(p)

  /** `target = value` */
  case Assign(target: PyExpr, value: PyExpr)(using p: PyPos) extends PyStmt(p)

  /** `target op= value` */
  case AugAssign(target: PyExpr, op: PyBinOp, value: PyExpr)(using p: PyPos) extends PyStmt(p)

  /** `return value` or bare `return`. */
  case Return(value: Option[PyExpr])(using p: PyPos) extends PyStmt(p)

  /** `if / elif / else` chain. */
  case If(
      cond: PyExpr,
      body: List[PyStmt],
      elifs: List[(PyExpr, List[PyStmt])],
      elseBody: Option[List[PyStmt]]
  )(using p: PyPos) extends PyStmt(p)

  /** `while cond: body` */
  case While(cond: PyExpr, body: List[PyStmt])(using p: PyPos) extends PyStmt(p)

  /** `for target in iter: body` */
  case For(target: PyExpr, iter: PyExpr, body: List[PyStmt])(using p: PyPos)
      extends PyStmt(p)

  /** `try / except / else / finally` */
  case Try(
      body: List[PyStmt],
      handlers: List[PyExceptHandler],
      elseBody: Option[List[PyStmt]],
      finallyBody: Option[List[PyStmt]]
  )(using p: PyPos) extends PyStmt(p)

  /** `raise expr` or bare `raise`. */
  case Raise(exception: Option[PyExpr])(using p: PyPos) extends PyStmt(p)

  /** `assert test, msg` */
  case Assert(test: PyExpr, msg: Option[PyExpr])(using p: PyPos) extends PyStmt(p)

  case Pass(using p: PyPos) extends PyStmt(p)
  case Break(using p: PyPos) extends PyStmt(p)
  case Continue(using p: PyPos) extends PyStmt(p)

  /** `del target` */
  case Del(target: PyExpr)(using p: PyPos) extends PyStmt(p)

  // -- IR-level constructs (lowered during emission) --

  /** Local variable definition. Emits as `name = rhs` or `name: type = rhs`. */
  case VarDef(name: PyName, tpe: Option[PyTypeAnnot], mutable: Boolean, rhs: PyExpr)(
      using p: PyPos
  ) extends PyStmt(p)

  /** Grouping construct, flattened into parent scope during emission. */
  case Block(stmts: List[PyStmt])(using p: PyPos) extends PyStmt(p)

  /** Labeled block for non-local returns.
    * Emitter lowers to: `try: body except _NonLocalReturn_<label> as e: ...`
    */
  case Labeled(label: PyName, body: List[PyStmt])(using p: PyPos) extends PyStmt(p)

  /** Return to a labeled block. Emitter lowers to: `raise _NonLocalReturn_<label>(value)` */
  case LabelReturn(label: PyName, value: PyExpr)(using p: PyPos) extends PyStmt(p)

  /** Inline function definition (for lifted closures as local defs). */
  case FuncDefStmt(func: PyFuncDef)(using p: PyPos) extends PyStmt(p)

  /** Inline class definition (for nested/local classes). */
  case ClassDefStmt(cls: PyClassDef)(using p: PyPos) extends PyStmt(p)

/** Exception handler in a try statement. */
case class PyExceptHandler(
    exnType: Option[PyExpr], // None = bare `except:`
    name: Option[PyName],    // `except E as name:`
    body: List[PyStmt],
    pos: PyPos
)

// ─── Expressions ───────────────────────────────────────────────────────

enum PyExpr(val pos: PyPos):

  // -- Literals --

  case NoneLit(using p: PyPos) extends PyExpr(p)
  case BoolLit(value: Boolean)(using p: PyPos) extends PyExpr(p)
  case IntLit(value: Long)(using p: PyPos) extends PyExpr(p)
  case FloatLit(value: Double)(using p: PyPos) extends PyExpr(p)
  case StringLit(value: String)(using p: PyPos) extends PyExpr(p)

  // -- Name references --

  /** Local variable or parameter reference. */
  case Name(name: PyName)(using p: PyPos) extends PyExpr(p)

  /** Qualified reference for cross-module access. */
  case QualRef(qual: QualName)(using p: PyPos) extends PyExpr(p)

  /** Attribute access: `obj.attr` */
  case Attr(obj: PyExpr, name: PyName)(using p: PyPos) extends PyExpr(p)

  /** Subscript: `obj[index]` */
  case Subscript(obj: PyExpr, index: PyExpr)(using p: PyPos) extends PyExpr(p)

  // -- Calls --

  /** Function/method call: `func(args...)` */
  case Call(func: PyExpr, args: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  /** Object construction: `Cls(args...)`.
    * Semantically same as Call in Python, but kept separate in the IR
    * to distinguish instantiation from method calls for clarity.
    */
  case New(cls: PyExpr, args: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  // -- Operators --

  /** Binary operation: `lhs op rhs` */
  case BinOp(op: PyBinOp, lhs: PyExpr, rhs: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** Unary operation: `op operand` */
  case UnaryOp(op: PyUnaryOp, operand: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** Short-circuiting boolean: `a and b and c` or `a or b or c` */
  case BoolOp(op: PyBoolOp, values: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  /** Comparison chain: `a < b <= c` */
  case Compare(left: PyExpr, ops: List[PyCmpOp], rights: List[PyExpr])(using p: PyPos)
      extends PyExpr(p)

  // -- Conditionals & lambdas --

  /** Ternary: `thenExpr if cond else elseExpr` */
  case IfExpr(cond: PyExpr, thenExpr: PyExpr, elseExpr: PyExpr)(using p: PyPos)
      extends PyExpr(p)

  /** Lambda: `lambda params: body` */
  case Lambda(params: List[PyParam], body: PyExpr)(using p: PyPos) extends PyExpr(p)

  // -- Collection constructors --

  /** Tuple: `(a, b, c)` */
  case TupleLit(elems: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  /** List: `[a, b, c]` */
  case ListLit(elems: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  /** Dict: `{k1: v1, k2: v2}` */
  case DictLit(keys: List[PyExpr], values: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  // -- Scala-specific: type operations --

  /** `isinstance(obj, cls)` for Scala's `isInstanceOf` */
  case IsInstance(obj: PyExpr, cls: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** Type cast. No-op at runtime in Python, but explicit in IR
    * for potential runtime checks. Emitter can produce identity or a check.
    */
  case Cast(obj: PyExpr, cls: PyExpr)(using p: PyPos) extends PyExpr(p)

  // -- Scala-specific: object model --

  /** Load a Scala object (singleton) instance.
    * Emitter produces reference to module-level singleton variable.
    */
  case LoadModule(qualName: QualName)(using p: PyPos) extends PyExpr(p)

  /** Super reference. `parentClass = None` -> `super()`;
    * `parentClass = Some(T)` -> `T` (for `T.method(self, ...)` direct call).
    */
  case SuperRef(self: PyExpr, parentClass: Option[PyExpr])(using p: PyPos)
      extends PyExpr(p)

  /** `self` / `this` reference. */
  case This(using p: PyPos) extends PyExpr(p)

  // -- Scala-specific: numeric wrapping --

  /** 32-bit signed integer wrapping. Emitter produces `_rt.i32(expr)`. */
  case IntWrap32(expr: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** 64-bit signed integer wrapping. Emitter produces `_rt.i64(expr)`. */
  case IntWrap64(expr: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** 32-bit IEEE 754 float truncation. Emitter produces `_rt.f32(expr)`. */
  case FloatWrap(expr: PyExpr)(using p: PyPos) extends PyExpr(p)

  // -- Scala-specific: strings --

  /** String concatenation. Emitter can produce f-string or `+` chain. */
  case StringConcat(parts: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  // -- Scala-specific: arrays --

  /** Array creation: `[init] * length` */
  case NewArray(length: PyExpr, init: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** Array literal: `[e1, e2, ...]` */
  case ArrayLit(elems: List[PyExpr])(using p: PyPos) extends PyExpr(p)

  /** Array element access: `arr[index]` (separate from Subscript for bounds checking). */
  case ArraySelect(array: PyExpr, index: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** Array/collection length: `len(expr)` */
  case ArrayLength(array: PyExpr)(using p: PyPos) extends PyExpr(p)

  // -- Scala-specific: exceptions --

  /** Wrap a Scala Throwable so it's raisable in Python (extends BaseException). */
  case WrapThrowable(expr: PyExpr)(using p: PyPos) extends PyExpr(p)

  /** Unwrap a caught Python exception to get the Scala Throwable. */
  case UnwrapThrowable(expr: PyExpr)(using p: PyPos) extends PyExpr(p)

// ─── Operators ─────────────────────────────────────────────────────────

enum PyBinOp:
  case Add, Sub, Mul, Div, FloorDiv, Mod, Pow
  case BitOr, BitAnd, BitXor, LShift, RShift

enum PyUnaryOp:
  case Not, Neg, Pos, Invert // `not x`, `-x`, `+x`, `~x`

enum PyBoolOp:
  case And, Or

enum PyCmpOp:
  case Eq, NotEq, Lt, LtE, Gt, GtE, Is, IsNot

// ─── Type annotations (optional, for generated Python type hints) ──────

enum PyTypeAnnot:
  case Named(name: String)                                          // int, str, float, bool
  case Qualified(qual: QualName)                                    // module.ClassName
  case Parameterized(base: PyTypeAnnot, args: List[PyTypeAnnot])    // List[int], Dict[str, int]
  case Union(members: List[PyTypeAnnot])                            // int | str
  case Optional(inner: PyTypeAnnot)                                 // Optional[X]
  case Any
