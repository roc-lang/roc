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


def graph(root, work, zig, label, options, expected_cached, baseline=None,
          changed_stages=frozenset(), refreshed_stages=frozenset()):
    zig_lib = [option for option in options if option.startswith("--zig-lib=")]
    remaining_options = [option for option in options if option not in zig_lib]
    # Zig 0.17 requires its special library override before steps/other options.
    command = [zig, "build", *zig_lib, "run-test-builtin-bake-reproducible", "--verbose",
               "--summary", "all", "--cache-poison=disallowed", "--cache-dir",
               str(work / "cache"), "--prefix", str(work / "out"), *remaining_options]
    result = subprocess.run(command, cwd=root, capture_output=True, text=True)
    output = result.stdout + result.stderr
    (work / f"{label}.log").write_text(output)
    if result.returncode:
        raise AssertionError(output)

    compile_args = None
    bakes = {}
    stages = {}
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
        if args and Path(args[0]).name == "compiler_identity":
            for index, arg in enumerate(args):
                if arg == "--file" and args[index + 1] in ("toolchain-contents", "dependency-contents"):
                    stages[args[index + 1]] = str(Path(args[index + 2]))
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
    if not stages and expected_cached:
        stages = {name: data["path"] for name, data in baseline["stages"].items()}
    assert "toolchain-contents" in stages, "independent toolchain digest input missing"
    stage_state = {name: {"path": path, "hash": hashlib.sha256(Path(path).read_bytes()).hexdigest()}
                   for name, path in stages.items()}
    if baseline:
        assert stage_state.keys() == baseline["stages"].keys(), "large input stage set changed"
        producers = {"toolchain-contents": "lib", "dependency-contents": "include"}
        for name, data in stage_state.items():
            expected_result = "success" if name in changed_stages else "cached"
            if name in changed_stages:
                assert data != baseline["stages"][name], f"{name} content change was ignored"
            else:
                assert data == baseline["stages"][name], f"unchanged {name} stage was replaced"
            summaries = {
                f"run exe compiler_identity ({Path(data['path']).name})": {expected_result},
                f"WriteFile {producers[name]}": {expected_result},
            }
            if name in refreshed_stages:
                # Restoring an edited file may refresh WriteFiles' manifest and
                # copy, while its original digest/compiler/bakes remain cached.
                summaries[f"WriteFile {producers[name]}"] = {"cached", "success"}
            for summary, expected_results in summaries.items():
                assert any(any(f"{summary} {expected}" in line for expected in expected_results)
                           for line in result.stderr.splitlines()), \
                    f"{name}: expected {summary} {expected_results}"

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
             "stages": stage_state,
             "outputs": tuple(tuple(str(path) for path in bakes[index][1]) for index in range(3)),
             "hashes": hashes}
    print(f"{label}: Debug builtin compiler and three bakes {'cached' if expected_cached else 'executed'}",
          flush=True)
    return state


def check(work, zig, options):
    root = work / "source"
    snapshot(root)
    dependency_header = None
    if not any(option.startswith(("-Droc-deps-path", "-Dllvm-path", "-Dsystem-llvm")) for option in options):
        # The Debug bake graph does not link LLVM. A small controlled mutable
        # bundle exercises its real dependency identity stage without requiring
        # a released bootstrap bundle or introducing a large native link.
        bundle = work / "dependencies"
        (bundle / "include").mkdir(parents=True)
        (bundle / "lib").mkdir()
        dependency_header = bundle / "include/cache_probe.h"
        dependency_header.write_text("// declared dependency header\n")
        options = [*options, f"-Droc-deps-path={bundle}"]
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
    if not any(option.startswith(("-Doptimize=", "-Dtarget=")) for option in options):
        states["outer-mode"] = graph(root, work, zig, "outer-mode",
                                     [*options, "-Doptimize=ReleaseFast"], True, baseline)
        assert states["outer-mode"] == baseline
        states["outer-target"] = graph(root, work, zig, "outer-target",
                                       [*options, "-Dtarget=x86_64-windows-gnu"], True, baseline)
        assert states["outer-target"] == baseline
        states["outer-native-abi"] = graph(root, work, zig, "outer-native-abi",
                                           [*options, "-Dtarget=x86_64-linux-gnu"], True, baseline)
        assert states["outer-native-abi"] == baseline
    production = root / "src/build/builtin_compiler/main.zig"
    original = production.read_bytes()
    try:
        production.write_bytes(original + b"\n// declared production input cache probe\n")
        states["production-edit"] = graph(root, work, zig, "production-edit", options, False, baseline)
        edited = states["production-edit"]
        assert edited["identity"] != baseline["identity"]
        assert edited["compiler"] != baseline["compiler"]
        assert edited["outputs"] != baseline["outputs"]
    finally:
        production.write_bytes(original)
    states["production-restored"] = graph(root, work, zig, "production-restored", options, True, baseline)
    assert states["production-restored"] == baseline
    if dependency_header is not None:
        original_header = dependency_header.read_bytes()
        try:
            dependency_header.write_bytes(original_header + b"// changed declared header\n")
            states["dependency-edit"] = graph(
                root, work, zig, "dependency-edit", options, False, baseline,
                changed_stages=frozenset({"dependency-contents"}))
            edited = states["dependency-edit"]
            assert edited["identity"] != baseline["identity"]
            assert edited["compiler"] != baseline["compiler"]
            assert edited["outputs"] != baseline["outputs"]
        finally:
            dependency_header.write_bytes(original_header)
        states["dependency-restored"] = graph(
            root, work, zig, "dependency-restored", options, True, baseline,
            refreshed_stages=frozenset({"dependency-contents"}))
        assert states["dependency-restored"] == baseline
        states["dependency-restored-unchanged"] = graph(
            root, work, zig, "dependency-restored-unchanged", options, True, baseline)
        assert states["dependency-restored-unchanged"] == baseline
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
