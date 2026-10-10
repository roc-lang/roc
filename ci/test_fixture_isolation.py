#!/usr/bin/env python3
"""Exercise the real Zig fixture graph with concurrent modes and cache roots."""
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import shutil
import shlex
import subprocess
import sys
import tempfile


BUILD = r'''
const std = @import("std");
const Plan = @import("src/build/test_fixtures.zig").Plan;

pub fn build(b: *std.Build) void {
    var plan = Plan{ .b = b };
    const tag = b.option([]const u8, "tag", "Fixture build mode") orelse "default";
    const generated = b.addWriteFiles();
    const first = b.addWriteFiles();
    const second = b.addWriteFiles();
    plan.copy(first, generated.add("first.a", tag), "test/first/platform/targets/native/libhost.a");
    plan.copy(second, generated.add("second.a", tag), "test/second/platform/targets/native/libhost.a");
    plan.addUpdateStep(&.{&first.step});
    const verify = b.step("verify", "Check private fixture roots");
    for ([_]*std.Build.Step{ &first.step, &second.step }, 0..) |host, index| {
        const deps = &.{host};
        const cached = plan.cachedRoot(deps);
        const run = b.addSystemCommand(&.{ "python3" });
        run.addFileArg(b.path("ci/probe.py"));
        run.addArg(tag);
        run.addArg(if (index == 0) "first" else "second");
        run.addDirectoryArg(cached);
        run.addDirectoryArg(b.path("."));
        run.setCwd(plan.mutableRoot(deps));
        run.expectExitCode(0);
        verify.dependOn(&run.step);
    }
}
'''

PROBE = r'''
from pathlib import Path
import sys
import time

tag, selected, cached_path, source_path = sys.argv[1:]
root = Path.cwd()
cached = Path(cached_path)
source = Path(source_path)
fixture = Path("test") / selected / "platform/targets/native/libhost.a"
other = "second" if selected == "first" else "first"
assert (root / fixture).read_text() == tag
assert (cached / fixture).read_text() == tag
assert not (root / "test" / other / "platform/targets/native/libhost.a").exists()
assert (root / "test/input.roc").read_text() == "fixture input\n"
assert (root / "test/first/platform/targets/native/crt1.o").read_bytes() == b"tracked runtime"
assert (root / "test/first/platform/targets/native/libc.a").read_bytes() == b"tracked archive"
for prepared in (root, cached):
    for imported in ("README.md", "CONTRIBUTING/profiling/bench_repeated_check_ORIGINAL.roc"):
        assert (prepared / imported).read_bytes() == (source / imported).read_bytes()
    assert not list((prepared / "ci").rglob("*.pyc"))
    assert not list((prepared / "ci").rglob("*.pyo"))
# A fixture runner may create binaries and rewrite a copied input. These must
# stay private to this run, even while the other graph is executing.
(root / fixture).write_text("runtime mutation")
(root / "test/input.roc").write_text(tag)
(root / "test/generated-app").write_text(tag)
time.sleep(0.1)
assert (root / "test/generated-app").read_text() == tag
assert (cached / fixture).read_text() == tag
assert (cached / "test/input.roc").read_text() == "fixture input\n"
assert not (cached / "test/generated-app").exists()
print(f"{tag}/{selected}: isolated fixture root")
'''


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"Usage: {sys.argv[0]} /path/to/zig")
    zig = str(Path(sys.argv[1]).resolve())
    source = Path(__file__).resolve().parents[1] / "src/build/test_fixtures.zig"
    with tempfile.TemporaryDirectory(prefix="roc-fixture-isolation-") as temp:
        root = Path(temp)
        for name in ("src/build", "vendor", "ci", "test/first/platform/targets/native", "CONTRIBUTING/profiling"):
            (root / name).mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, root / "src/build/test_fixtures.zig")
        (root / "build.zig").write_text(BUILD)
        (root / "ci/probe.py").write_text(PROBE)
        for name in ("build.zig.zon", "design.md", "legal_details"):
            (root / name).write_text("fixture metadata\n")
        (root / "build.zig.zon").write_text('.{ .name = .fixture_probe, .version = "0.0.0", .fingerprint = 0xceb1d2ec669ad2f3, .minimum_zig_version = "0.17.0", .paths = .{""} }\n')
        (root / "test/input.roc").write_text("fixture input\n")
        (root / "README.md").write_text("readme string import\n")
        (root / "CONTRIBUTING/profiling/bench_repeated_check_ORIGINAL.roc").write_text("profiling string import\n")
        target = root / "test/first/platform/targets/native"
        (target / "crt1.o").write_bytes(b"tracked runtime")
        (target / "libc.a").write_bytes(b"tracked archive")
        # Stale generated checkout artifacts must never become fixture inputs.
        (target / "libhost.a").write_text("stale checkout host")

        def build(tag: str) -> set[str]:
            command = [zig, "build", "verify", "--verbose", "--cache-poison=disallowed", f"-Dtag={tag}", "--cache-dir", str(root / f"cache-{tag}"), "--prefix", str(root / f"out-{tag}")]
            result = subprocess.run(command, cwd=root, capture_output=True, text=True)
            if result.returncode:
                raise AssertionError(result.stdout + result.stderr)
            roots = set()
            for line in result.stderr.splitlines():
                args = shlex.split(line)
                for index, arg in enumerate(args):
                    if arg.endswith("/ci/probe.py"):
                        roots.add(args[index + 3])
            assert len(roots) == 2, result.stdout + result.stderr
            return roots

        with ThreadPoolExecutor(max_workers=2) as pool:
            baseline = list(pool.map(build, ("debug", "fast")))
            # Importing Python checks may add bytecode to the checkout. Neither
            # creating nor changing it may change the prepared fixture inputs.
            bytecode = root / "ci/__pycache__"
            bytecode.mkdir()
            (bytecode / "probe.cpython-314.pyc").write_bytes(b"generated bytecode")
            (root / "ci/probe.pyo").write_bytes(b"generated optimized bytecode")
            assert list(pool.map(build, ("debug", "fast"))) == baseline
            (bytecode / "probe.cpython-314.pyc").write_bytes(b"changed bytecode")
            # Identical concurrent graphs also share immutable cache entries,
            # while their mutable runner directories must remain independent.
            assert list(pool.map(build, ("debug", "debug"))) == [baseline[0], baseline[0]]
            # External string imports are content dependencies of the retained
            # root. Editing either import must stage new bytes; restoring it
            # must reuse the original roots without a checkout host mutation.
            for imported in ("README.md", "CONTRIBUTING/profiling/bench_repeated_check_ORIGINAL.roc"):
                path = root / imported
                original = path.read_bytes()
                path.write_bytes(original + b"changed imported contents\n")
                changed = list(pool.map(build, ("debug", "fast")))
                assert all(now != before for now, before in zip(changed, baseline))
                path.write_bytes(original)
                assert list(pool.map(build, ("debug", "fast"))) == baseline
        assert (root / "test/input.roc").read_text() == "fixture input\n"
        assert (target / "libhost.a").read_text() == "stale checkout host"
        assert not (root / "test/generated-app").exists()
        publication = subprocess.run(
            [zig, "build", "update-test-fixtures", "-Dtag=published", "--cache-poison=disallowed"],
            cwd=root, capture_output=True, text=True)
        assert publication.returncode == 0, publication.stdout + publication.stderr
        assert (target / "libhost.a").read_text() == "published"
        assert not (root / "test/second/platform/targets/native/libhost.a").exists()
        assert (root / "test/input.roc").read_text() == "fixture input\n"
    print("Concurrent fixture graph isolation checks passed.")


if __name__ == "__main__":
    main()
