"""pyperf entry-point for a single Scala bench bundle.

The companion JVM Driver (see ``bench-py/driver-src/Driver.scala``) compiles
each ``*.scala`` bench source through ``bin/spc`` to a Python bundle, then
invokes this script per-bundle inside ``uv run``. Pyperf takes care of
subprocess fan-out, calibration, warmup, and JSON output; we just register one
``bench_time_func`` per ``(op, size)`` pair.

Inputs come from the environment so pyperf's own ``argv`` parsing stays intact:

* ``BENCH_BUNDLE``       absolute path to ``<BenchClass>.py``
* ``BENCH_QUAL``         Scala-qualified class name, e.g.
                         ``dotty.tools.benchmarks.py.numeric.NumericLoopBench``
* ``BENCH_SIZES``        comma-separated sizes (default ``64,1024``)
* ``BENCH_OPS``          comma-separated ops (default ``sumLoop``)
* ``BENCH_INNER_LOOPS``  manual unroll factor (default ``10``).

Pyperf flags (``--processes``, ``--values``, ``--warmups``, ``--min-time``,
``--output``, ``--append``, ``--debug-single-value``, ``--worker``, etc.) are
passed through ``sys.argv`` and consumed by ``pyperf.Runner()``.
"""
from __future__ import annotations

import os
import runpy
import sys
import time
import types

import pyperf


def _load_bundle(bundle_path: str) -> dict:
    """Execute the spc bundle as ``__main__`` and return its globals.

    The bundle uses ``from __main__ import _scpy_*`` for forward references
    to symbols it defines later, so it MUST run with ``__name__ ==
    "__main__"``. ``runpy.run_path`` registers the executed file under
    ``sys.modules['__main__']``, which makes those self-imports resolve.
    The bundle's own ``if __name__ == "__main__":`` block also fires, but
    each bench file's stub ``@main def main()`` is a no-op."""
    return runpy.run_path(bundle_path, run_name="__main__")


def _find_noarg_ctor(cls):
    """Scala-emitted classes do NOT run their constructor from Python's
    ``cls()`` — that only triggers ``__init__``, which JVM-zero-inits fields
    and (for a no-arg primary ctor) returns without running the Scala
    ``<init>`` body. The body (which assigns ``val`` fields like
    ``operations``) lives in a ``_scpy_ctor_<classid>__void__V`` helper that
    Scala call sites reach via ``_scpy_new``. Find that helper so we can do
    the same. Bench classes have exactly one no-arg ctor."""
    cands = [
        n for n in dir(cls)
        if n.startswith("_scpy_ctor_") and n.endswith("__void__V")
    ]
    if not cands:
        raise AttributeError(f"{cls.__name__} has no no-arg ctor (_scpy_ctor_*__void__V)")
    if len(cands) > 1:
        raise AttributeError(f"{cls.__name__} has multiple no-arg ctors: {cands!r}")
    return getattr(cls, cands[0])


def _find_method(obj, scala_name: str):
    """spc emits ``<name>__<argTypes>__<retType>``. Anonymous functions
    nested in the body of ``<name>`` get sibling names like
    ``<name>__anonfun_<N>__...`` and ``<name>__anonfun_adapted_<N>__...``.
    Filter those out before disambiguating; they are never the public
    method we want to call."""
    prefix = scala_name + "__"
    matches = [
        a for a in dir(obj)
        if a.startswith(prefix) and not a[len(prefix):].startswith("anonfun_")
    ]
    if not matches:
        raise AttributeError(f"{type(obj).__name__} has no member matching {scala_name!r}")
    if len(matches) > 1:
        raise AttributeError(
            f"{type(obj).__name__} has multiple members matching {scala_name!r}: {matches!r}"
        )
    return getattr(obj, matches[0])


def _make_time_func(unroll: int):
    """Build a ``time_func(loops, fn)`` whose body is ``unroll`` straight-
    line ``fn()`` calls inside a Python ``for _ in range(loops)``. Source
    is generated and ``exec``'d so the bytecode is straight-line — pyperf
    then divides the elapsed time by ``loops * inner_loops`` to get
    per-call latency."""
    body = "\n        ".join(["fn()"] * unroll)
    src = (
        "def time_op(loops, fn):\n"
        "    t0 = perf_counter()\n"
        "    for _ in range(loops):\n"
        f"        {body}\n"
        "    return perf_counter() - t0\n"
    )
    ns = {"perf_counter": time.perf_counter}
    exec(src, ns)
    return ns["time_op"]


def main() -> None:
    bundle = os.environ["BENCH_BUNDLE"]
    qual = os.environ["BENCH_QUAL"]
    sizes = [int(s) for s in os.environ.get("BENCH_SIZES", "64,1024").split(",")]
    ops = os.environ.get("BENCH_OPS", "sumLoop").split(",")
    unroll = int(os.environ.get("BENCH_INNER_LOOPS", "10"))

    bundle_globals = _load_bundle(bundle)
    bench_cls_name = qual.replace(".", "_")
    if bench_cls_name not in bundle_globals:
        raise AttributeError(
            f"bundle {bundle!r} has no class named {bench_cls_name!r}"
        )
    bench_cls = bundle_globals[bench_cls_name]
    scpy_new = bundle_globals["_scpy_new"]
    bench_ctor = _find_noarg_ctor(bench_cls)

    # Make sure the worker subprocesses see the same BENCH_* env so they
    # can re-load the same bundle and re-resolve the same op. pyperf
    # otherwise filters environment to a small allowlist. Injected into
    # sys.argv so it goes through pyperf's normal arg parsing.
    if not any(a.startswith("--inherit-environ") for a in sys.argv[1:]):
        sys.argv.insert(
            1,
            "--inherit-environ=BENCH_BUNDLE,BENCH_QUAL,BENCH_SIZES,BENCH_OPS,BENCH_INNER_LOOPS",
        )

    time_op = _make_time_func(unroll)
    runner = pyperf.Runner()
    # Bench name includes the immediate package (the category) so distinct
    # categories don't collide in the pyperf JSON suite.
    parts = qual.split(".")
    bench_name = ".".join(parts[-2:])  # e.g. numeric.NumericLoopBench

    for size in sizes:
        bench = scpy_new(bench_cls, bench_ctor)
        _find_method(bench, "setup")(size)
        ops_map = _find_method(bench, "operations")()
        ops_apply = _find_method(ops_map, "apply")
        for op in ops:
            fn = ops_apply(op)  # _scpy_Fn0; callable via __call__
            runner.bench_time_func(
                f"{bench_name}.{op}@{size}",
                time_op,
                fn,
                inner_loops=unroll,
            )


if __name__ == "__main__":
    main()
