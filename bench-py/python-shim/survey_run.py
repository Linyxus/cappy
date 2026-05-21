"""Runtime survey: load one bench bundle, instantiate it, run setup, and call
each operation ONCE inside a try/except. Prints per-op OK / EXC. This surfaces
runtime codegen bugs that a clean compile does not (a bundle can compile yet
raise when executed). Inputs via env, mirroring the pyperf shim.
"""
from __future__ import annotations
import os, runpy, signal, sys, traceback

TIMEOUT = int(os.environ.get("BENCH_TIMEOUT", "20"))  # seconds per setup/op


class _Timeout(Exception):
    pass


def _alarm(_sig, _frame):
    raise _Timeout()


signal.signal(signal.SIGALRM, _alarm)


def _guarded(label, fn):
    """Run fn() under a SIGALRM timeout; returns (ok, value_or_exc)."""
    signal.alarm(TIMEOUT)
    try:
        return True, fn()
    finally:
        signal.alarm(0)


def _find_noarg_ctor(cls):
    cands = [n for n in dir(cls) if n.startswith("_scpy_ctor_") and n.endswith("__void__V")]
    if not cands:
        raise AttributeError(f"{cls.__name__}: no no-arg ctor")
    return getattr(cls, cands[0])


def _find_method(obj, name):
    pre = name + "__"
    m = [a for a in dir(obj) if a.startswith(pre) and not a[len(pre):].startswith("anonfun_")]
    if not m:
        raise AttributeError(f"{type(obj).__name__}: no member {name!r}")
    if len(m) > 1:
        raise AttributeError(f"{type(obj).__name__}: ambiguous {name!r}: {m!r}")
    return getattr(obj, m[0])


def main() -> int:
    bundle = os.environ["BENCH_BUNDLE"]
    qual = os.environ["BENCH_QUAL"]
    size = int(os.environ.get("BENCH_SIZE", "64"))
    ops = os.environ["BENCH_OPS"].split(",")
    g = runpy.run_path(bundle, run_name="__main__")
    clsname = qual.replace(".", "_")
    if clsname not in g:
        print(f"  LOADFAIL: class {clsname!r} not in bundle")
        return 1
    cls = g[clsname]
    scpy_new = g["_scpy_new"]
    bad = 0
    try:
        b = scpy_new(cls, _find_noarg_ctor(cls))
        _guarded("setup", lambda: _find_method(b, "setup")(size))
        opsmap = _find_method(b, "operations")()
        apply = _find_method(opsmap, "apply")
    except _Timeout:
        print(f"  SETUPFAIL: Timeout after {TIMEOUT}s")
        return 1
    except Exception as e:
        print(f"  SETUPFAIL: {type(e).__name__}: {e}")
        traceback.print_exc()
        return 1
    for op in ops:
        try:
            fn = apply(op)
            _guarded(op, fn)
            print(f"  OK   {op}")
        except _Timeout:
            bad += 1
            print(f"  EXC  {op}: Timeout after {TIMEOUT}s")
        except Exception as e:
            bad += 1
            print(f"  EXC  {op}: {type(e).__name__}: {e}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
