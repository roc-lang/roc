#!/usr/bin/env python3
"""Vendor the MinGW C runtime linker inputs used by the `*mingw` test platforms.

Where these files come from
---------------------------
Everything written by this script is produced by the *installed Zig toolchain*
(Zig 0.16.0), not by a system MinGW install:

  * `crt2.obj` (exe startup), `dllcrt2.obj` (DLL startup) and `libmingw32.lib`
    -- Zig's bundled mingw-w64 sources, compiled on demand for the requested
    target and cached in Zig's global cache.
  * `zigc.lib`, `compiler_rt.lib` -- Zig's own C-runtime shims and compiler_rt,
    compiled the same way.
  * `api-ms-win-crt-*.lib`, `advapi32.lib`, `kernel32.lib`, `ntdll.lib`,
    `shell32.lib`, `user32.lib`, `ws2_32.lib` -- import libraries Zig generates
    from the `.def` files it ships for the Universal CRT and the Win32 system
    DLLs.

Why they are checked in
-----------------------
A `*mingw` target links ONLY what the platform declares in its `targets:` block,
plus `/nodefaultlib` (see `src/cli/linker.zig`, the `.windows` / `.mingw` arm).
So each test platform's `platform/targets/<target>/` directory has to hold the
runtime next to `host.lib`. `test/fx` holds the canonical checked-in copy and
`build.zig` copies it into the other test platforms at build time. This mirrors
what is already done for musl (`crt1.o` / `libc.a`).

How to regenerate
-----------------
    zig version          # must print 0.16.0
    python ci/vendor_mingw_runtime.py

Add `--check` to rebuild into a temp dir and diff against the checked-in files
instead of overwriting them.

Unlike the roc-platform-template-go vendoring script this does NOT canonicalize
the archives/objects for byte reproducibility: these are test fixtures, and the
bytes only have to link, not to hash-match across machines.

New files must be force-added (`git add -f`) unless `.gitignore` already has a
matching `!` exception -- `*.lib` and `*.obj` are ignored globally.
"""

from __future__ import annotations

import argparse
import os
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
REQUIRED_ZIG_VERSION = "0.16.0"

COMPILED_ARTIFACTS = (
    "crt2.obj",
    "dllcrt2.obj",
    "libmingw32.lib",
    "zigc.lib",
    "compiler_rt.lib",
)
IMPORT_LIBRARIES = (
    "api-ms-win-crt-conio-l1-1-0.lib",
    "api-ms-win-crt-convert-l1-1-0.lib",
    "api-ms-win-crt-environment-l1-1-0.lib",
    "api-ms-win-crt-filesystem-l1-1-0.lib",
    "api-ms-win-crt-heap-l1-1-0.lib",
    "api-ms-win-crt-locale-l1-1-0.lib",
    "api-ms-win-crt-math-l1-1-0.lib",
    "api-ms-win-crt-multibyte-l1-1-0.lib",
    "api-ms-win-crt-private-l1-1-0.lib",
    "api-ms-win-crt-process-l1-1-0.lib",
    "api-ms-win-crt-runtime-l1-1-0.lib",
    "api-ms-win-crt-stdio-l1-1-0.lib",
    "api-ms-win-crt-string-l1-1-0.lib",
    "api-ms-win-crt-time-l1-1-0.lib",
    "api-ms-win-crt-utility-l1-1-0.lib",
    "advapi32.lib",
    "kernel32.lib",
    "ntdll.lib",
    "shell32.lib",
    "user32.lib",
    # Winsock: the http-headers host calls socket/bind/listen/accept. Under
    # `-lldmingw /nodefaultlib` its `.drectve /defaultlib:ws2_32` is ignored, so
    # the import lib has to be an explicit platform input. Zig only generates it
    # when something links it, hence `-lws2_32` on the probe below.
    "ws2_32.lib",
)
ARTIFACTS = COMPILED_ARTIFACTS + IMPORT_LIBRARIES

# `crt2.obj` (mingw-w64 crtexe.c) only shows up on an EXE link line and
# `dllcrt2.obj` (crtdll.c) only on a DLL link line, so the probe is linked
# twice and each artifact is harvested from the trace that mentions it.
DLL_LINK_ARTIFACTS = ("dllcrt2.obj",)
EXE_LINK_ARTIFACTS = tuple(n for n in ARTIFACTS if n not in DLL_LINK_ARTIFACTS)

TARGETS = {
    "x64mingw": "x86_64-windows-gnu",
    "arm64mingw": "aarch64-windows-gnu",
}

# Pulling in pthreads makes the probe reference enough of the runtime that the
# driver emits the full default link line, which is what we harvest.
PROBE_SOURCE = """\
#include <pthread.h>

static void *thread_main(void *unused) {
    return unused;
}

int main(void) {
    pthread_t thread;
    if (pthread_create(&thread, 0, thread_main, 0) != 0) {
        return 1;
    }
    return pthread_join(thread, 0);
}
"""


def locate_artifacts(trace: str, wanted: tuple) -> dict:
    """Pick the wanted artifact paths out of the `lld-link ...` line of a trace."""
    found = {}
    for line in trace.splitlines():
        if not line.startswith("lld-link "):
            continue
        # posix=False: the trace holds Windows paths, and POSIX-mode splitting
        # would swallow their backslashes.
        for token in shlex.split(line, posix=False):
            token = token.strip('"')
            normalized = token.replace("\\", "/")
            for artifact in wanted:
                if normalized.endswith("/" + artifact):
                    found[artifact] = Path(token)
    return found


def build_runtime(roc_target: str, zig_target: str, work_dir: Path) -> Path:
    """Compile the probe for `zig_target` and return the dir holding the runtime."""
    target_work = work_dir / roc_target
    output_dir = target_work / "out"
    output_dir.mkdir(parents=True)
    probe = target_work / "probe.c"
    probe.write_text(PROBE_SOURCE, encoding="utf-8")

    # Keep the caches inside the (short) work dir so long Windows paths and a
    # polluted user cache cannot affect the result.
    env = os.environ.copy()
    for name in ("ZIG_GLOBAL_CACHE_DIR", "ZIG_LOCAL_CACHE_DIR"):
        cache = target_work / name.lower().replace("_", "-")
        cache.mkdir()
        env[name] = str(cache)

    def link(kind: str, extra: list, output_name: str, wanted: tuple) -> dict:
        result = subprocess.run(
            [
                "zig", "cc",
                "-target", zig_target,
                "-O2", "-g0", "-fno-sanitize=all", "-s", "-v",
            ] + extra + [
                str(probe),
                "-o", str(target_work / output_name),
            ],
            env=env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
        )
        trace_path = target_work / ("zig-cc-" + kind + ".trace")
        trace_path.write_text(result.stderr, encoding="utf-8")
        if result.returncode != 0:
            raise RuntimeError(
                "zig cc (" + kind + ") failed for " + roc_target + "; trace: " + str(trace_path)
            )
        discovered = locate_artifacts(result.stderr, wanted)
        missing = [n for n in wanted if not discovered.get(n, Path()).is_file()]
        if missing:
            raise RuntimeError(
                "Could not locate " + ", ".join(missing) + " for " + roc_target
                + "; trace: " + str(trace_path)
            )
        return discovered

    discovered = link("exe", ["-lws2_32"], "probe.exe", EXE_LINK_ARTIFACTS)
    discovered.update(link("dll", ["-shared"], "probe.dll", DLL_LINK_ARTIFACTS))

    for name in ARTIFACTS:
        shutil.copy2(discovered[name], output_dir / name)
    return output_dir


def main() -> None:
    parser = argparse.ArgumentParser(description="Vendor Zig's MinGW runtime into test/fx")
    parser.add_argument(
        "--check",
        action="store_true",
        help="rebuild and byte-compare instead of overwriting the checked-in files",
    )
    parser.add_argument(
        "--work-dir",
        help="scratch directory to build in (default: a fresh temp dir, removed on exit)",
    )
    args = parser.parse_args()

    version = subprocess.check_output(["zig", "version"], text=True).strip()
    if version != REQUIRED_ZIG_VERSION:
        raise SystemExit("Zig " + REQUIRED_ZIG_VERSION + " is required; found " + version)

    if args.work_dir:
        work_dir = Path(args.work_dir).resolve()
        work_dir.mkdir(parents=True, exist_ok=True)
        cleanup = False
    else:
        work_dir = Path(tempfile.mkdtemp(prefix="roc-mingw-rt."))
        cleanup = True

    failures = []
    try:
        for roc_target, zig_target in TARGETS.items():
            output_dir = build_runtime(roc_target, zig_target, work_dir)
            destination_dir = ROOT / "test" / "fx" / "platform" / "targets" / roc_target
            total = 0
            for name in ARTIFACTS:
                source = output_dir / name
                destination = destination_dir / name
                total += source.stat().st_size
                if args.check:
                    if not destination.is_file():
                        failures.append("missing: " + str(destination))
                    elif destination.read_bytes() != source.read_bytes():
                        failures.append("differs: " + str(destination))
                else:
                    destination_dir.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(source, destination)
            print(roc_target + ": " + str(len(ARTIFACTS)) + " files, " + str(total) + " bytes")
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        raise SystemExit(str(error)) from None
    finally:
        if cleanup:
            shutil.rmtree(work_dir, ignore_errors=True)

    if failures:
        print("\n".join(failures), file=sys.stderr)
        raise SystemExit("checked-in MinGW runtime is out of date")
    if args.check:
        print("Checked-in MinGW runtime matches a fresh build.")
    else:
        print("MinGW runtime vendored into test/fx/platform/targets/.")


if __name__ == "__main__":
    main()
