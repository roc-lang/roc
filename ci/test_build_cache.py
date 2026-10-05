#!/usr/bin/env python3
"""Check actual builtin compiler/bake caching in a private source snapshot.

The host builtin compiler deliberately uses Debug. This is a graph correctness
check, not a compiler performance benchmark. The original checkout is untouched.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile


SOURCE = Path(__file__).resolve().parents[1]
OUTPUT_NAMES = ("Builtin.bin", "builtin_indices.zig", "Builtin.artifact.bin")


def snapshot(root):
    root.mkdir()
    for directory in ("src", "vendor", "test", "ci"):
        shutil.copytree(SOURCE / directory, root / directory,
                        ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "*.pyo"))
    for name in ("build.zig", "build.zig.zon", "design.md", "legal_details", "README.md"):
        shutil.copyfile(SOURCE / name, root / name)
    # A detached HEAD is enough for the build's declared version reader. It
    # lets us exercise an actual HEAD-only change without touching repository Git.
    (root / ".git").mkdir()
    (root / ".git/HEAD").write_text("a" * 40 + "\n")


def graph(root, work, zig, label, options, expected_cached, baseline=None):
    command = [zig, "build", "run-test-builtin-bake-reproducible", "--verbose",
               "--summary", "all", "--cache-poison=disallowed", "--cache-dir",
               str(work / "cache"), "--prefix", str(work / "out"), *options]
    result = subprocess.run(command, cwd=root, capture_output=True, text=True)
    output = result.stdout + result.stderr
    (work / f"{label}.log").write_text(output)
    if result.returncode:
        raise AssertionError(output)

    compile_args = None
    bakes = {}
    for line in result.stderr.splitlines():
        args = shlex.split(line.removeprefix("info(verbose): "))
        if "--name" in args and args[args.index("--name") + 1] == "builtin_compiler":
            compile_args = args
        if args and Path(args[0]).name == "builtin_compiler" and len(args) == 5:
            outputs = tuple(Path(arg) for arg in args[2:])
            match = re.fullmatch(r"bake-([0-2])-Builtin\.bin", outputs[0].name)
            if match:
                index = int(match.group(1))
                assert tuple(path.name for path in outputs) == tuple(
                    f"bake-{index}-{name}" for name in OUTPUT_NAMES)
                bakes[index] = (Path(args[0]), outputs)
    assert compile_args, "builtin compiler command missing from verbose graph"
    assert not any(arg.startswith("-Mcompiler_version=") for arg in compile_args), \
        "human version metadata leaked into the host compiler"
    if not bakes and expected_cached:
        # Zig prints cached compile commands, but omits cached Run commands.
        # An identical compiler command and three explicitly cached named Run
        # summaries identify the already-observed outputs. Check their bytes too.
        assert tuple(compile_args) == baseline["compiler_args"]
        bakes = {index: (Path(baseline["compiler"]), tuple(map(Path, outputs)))
                 for index, outputs in enumerate(baseline["outputs"])}
    assert set(bakes) == {0, 1, 2}, "expected three distinct builtin bake Run commands"
    assert len({paths[0].parent for _, paths in bakes.values()}) == 3, \
        "bake processes share a cache result"
    assert len({exe for exe, _ in bakes.values()}) == 1

    identity_path = Path(next(arg.split("=", 1)[1] for arg in compile_args
                              if arg.startswith("-Mcompiler_identity=")))
    identity = re.search(r'compiler_compatibility_id = "([0-9a-f]{64})"',
                         identity_path.read_text()).group(1)
    hashes = tuple(tuple(hashlib.sha256(path.read_bytes()).hexdigest()
                         for path in bakes[index][1]) for index in range(3))
    assert hashes[0] == hashes[1] == hashes[2], "independent builtin bakes differ"

    summaries = [line for line in result.stderr.splitlines()
                 if "compile exe builtin_compiler " in line or "run exe builtin_compiler " in line]
    assert len(summaries) >= 4, "builtin compiler/bake summary missing"
    if expected_cached:
        assert all("cached" in line or "reused" in line for line in summaries), "\n".join(summaries)
        for index in range(3):
            assert any(f"run exe builtin_compiler (bake-{index}-Builtin.bin) cached" in line
                       for line in summaries), "\n".join(summaries)
    else:
        assert any("compile exe builtin_compiler " in line and "success" in line
                   for line in summaries), "builtin compiler did not rebuild"
        assert sum(bool(re.search(r"run exe builtin_compiler \([^)]*\) success", line))
                   for line in summaries) == 3, \
            "expected three independently executed bake processes"

    state = {"identity": identity, "compiler": str(bakes[0][0]),
             "compiler_args": tuple(compile_args),
             "outputs": tuple(tuple(str(path) for path in bakes[index][1]) for index in range(3)),
             "hashes": hashes}
    print(f"{label}: Debug builtin compiler and three bakes {'cached' if expected_cached else 'executed'}",
          flush=True)
    return state


def check(work, zig, options):
    root = work / "source"
    snapshot(root)
    states = {}
    states["baseline"] = graph(root, work, zig, "baseline", options, False)
    baseline = states["baseline"]
    states["unchanged"] = graph(root, work, zig, "unchanged", options, True, baseline)
    assert states["unchanged"] == baseline
    (root / ".git/HEAD").write_text("b" * 40 + "\n")
    states["head-only"] = graph(root, work, zig, "head-only", options, True, baseline)
    assert states["head-only"] == baseline
    states["human-version"] = graph(root, work, zig, "human-version",
                                    [*options, "-Dcompiler-version=cache-probe-version"], True, baseline)
    assert states["human-version"] == baseline
    with (root / "README.md").open("a") as readme:
        readme.write("\nDocumentation-only cache probe.\n")
    (root / ".git/HEAD").write_text("c" * 40 + "\n")
    states["docs-only"] = graph(root, work, zig, "docs-only", options, True, baseline)
    assert states["docs-only"] == baseline
    test_source = root / "src/compile/test/cache_graph_probe.zig"
    test_source.write_text("// dedicated test-only change\n")
    states["test-only"] = graph(root, work, zig, "test-only", options, True, baseline)
    assert states["test-only"] == baseline
    production = root / "src/build/builtin_compiler/main.zig"
    original = production.read_bytes()
    try:
        production.write_bytes(original + b"\n// declared production input cache probe\n")
        states["production-edit"] = graph(root, work, zig, "production-edit", options, False)
        edited = states["production-edit"]
        assert edited["identity"] != baseline["identity"]
        assert edited["compiler"] != baseline["compiler"]
        assert edited["outputs"] != baseline["outputs"]
    finally:
        production.write_bytes(original)
    states["production-restored"] = graph(root, work, zig, "production-restored", options, True, baseline)
    assert states["production-restored"] == baseline
    (work / "results.json").write_text(json.dumps(states, indent=2) + "\n")
    print("Actual build graph cache checks passed.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("zig", type=lambda value: str(Path(value).resolve()))
    parser.add_argument("--work-dir", type=Path, help="Keep snapshot, logs and results in a new directory")
    args, options = parser.parse_known_args()
    if options[:1] == ["--"]:
        options = options[1:]
    if args.work_dir:
        args.work_dir.mkdir()
        check(args.work_dir.resolve(), args.zig, options)
    else:
        with tempfile.TemporaryDirectory(prefix="roc-build-cache-") as directory:
            check(Path(directory), args.zig, options)


if __name__ == "__main__":
    main()
