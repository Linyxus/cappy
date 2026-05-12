import sys


def _flush_print(s):
    """Force stdout flush so Python-side prints interleave deterministically
    with Scala-side `_scpy_println` output in directory tests."""
    print(s, flush=True)


class Mod(object):
    def __init__(self, seed):
        self._seed = seed

    def get_seed(self):
        return self._seed

    def describe(self):
        return f"Mod(seed={self._seed})"


class Counter(object):
    def __init__(self, start):
        self._count = start

    def bump(self):
        self._count += 1
        return self._count


class Default(object):
    """Zero-arg `__init__`; verifies the super-call emits with no trailing args."""
    def __init__(self):
        self._tag = "default"

    def tag(self):
        return self._tag


class Point3D(object):
    """Multi-arg ctor with mixed primitive types (Int, String, Double)."""
    def __init__(self, x, y, z):
        self._x = x
        self._y = y
        self._z = z

    def get_x(self):
        return self._x

    def get_y(self):
        return self._y

    def get_z(self):
        return self._z


class Stateful(object):
    """Each instance must hold its own state — no class-level leakage."""
    def __init__(self, init):
        self._count = init

    def get_count(self):
        return self._count


class Tracking(object):
    """Prints inside `__init__` (flushed) so we can observe Python-init vs
    Scala-body ordering."""
    def __init__(self, name):
        _flush_print(f"py-init:{name}")
        self._name = name

    def get_name(self):
        return self._name


class Worker(object):
    """Template-method-style parent: `execute` calls `self.do_work()`.

    NOTE: Scala-side `override def doWork(): String` on a Scala subclass
    emits the override under a Scala-encoded method name (e.g.
    `doWork__Ljava_dlang_dString`), not the Python identifier `do_work`.
    Python's MRO lookup for `self.do_work()` therefore still finds the
    Python parent's `do_work`. The test locks this in as the documented
    v1 limitation."""
    def do_work(self):
        return "py-work"

    def execute(self):
        return self.do_work() + "-done"


class Failer(object):
    """Raises in `__init__` to verify the exception escapes the Scala synthesized ctor."""
    def __init__(self, should_fail):
        if should_fail:
            raise RuntimeError("py-init-failed")
        self._ok = True

    def ok(self):
        return self._ok
