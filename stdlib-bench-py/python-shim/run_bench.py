"""pyperf entry-point for a single Scala bench bundle.

The companion JVM Driver (see ``stdlib-bench-py/driver-src/Driver.scala``)
compiles each ``*.scala`` bench source through ``bin/scpyc`` to a Python
bundle, then invokes this script per-bundle inside ``uv run``. Pyperf takes
care of subprocess fan-out, calibration, warmup, and JSON output; we just
register one ``bench_time_func`` per ``(op, size)`` pair.

Inputs come from the environment so pyperf's own ``argv`` parsing stays
intact:

* ``BENCH_BUNDLE``       absolute path to ``<BenchClass>.py``
* ``BENCH_QUAL``         Scala-qualified class name, e.g.
                         ``dotty.tools.benchmarks.py.mutable.HashMapBench``
* ``BENCH_SIZES``        comma-separated sizes (default ``16,256,4096``)
* ``BENCH_OPS``          comma-separated ops (default ``build,access,transform,mutate``)
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
    """Execute the scpyc bundle as ``__main__`` and return its globals.

    The bundle uses ``from __main__ import _scpy_*`` for forward references
    to symbols it defines later, so it MUST run with ``__name__ ==
    "__main__"``. ``runpy.run_path`` registers the executed file under
    ``sys.modules['__main__']``, which makes those self-imports resolve.
    The bundle's own ``if __name__ == "__main__":`` block also fires, but
    each bench file's stub ``@main def main()`` is a no-op."""
    return runpy.run_path(bundle_path, run_name="__main__")


def _find_method(obj, scala_name: str):
    """scpyc emits ``<name>__<argTypes>__<retType>``. Anonymous functions
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
    sizes = [int(s) for s in os.environ.get("BENCH_SIZES", "16,256,4096").split(",")]
    ops = os.environ.get("BENCH_OPS", "build,access,transform,mutate").split(",")
    unroll = int(os.environ.get("BENCH_INNER_LOOPS", "10"))

    bundle_globals = _load_bundle(bundle)
    bench_cls_name = qual.replace(".", "_")
    if bench_cls_name not in bundle_globals:
        raise AttributeError(
            f"bundle {bundle!r} has no class named {bench_cls_name!r}"
        )
    bench_cls = bundle_globals[bench_cls_name]

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
    # Bench name includes the immediate package so immutable/mutable
    # HashMap/HashSet pairs don't collide in the pyperf JSON suite.
    parts = qual.split(".")
    bench_name = ".".join(parts[-2:])  # e.g. mutable.HashMapBench

    for size in sizes:
        bench = bench_cls()
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
