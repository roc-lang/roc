#!/usr/bin/env python3
"""Exercise the real Zig fixture graph with concurrent modes and cache roots."""
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import shutil
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
    const verify = b.step("verify", "Check private fixture roots");
    for ([_]*std.Build.Step{ &first.step, &second.step }, 0..) |host, index| {
        const deps = &.{host};
        const cached = plan.cachedRoot(deps);
        const run = b.addSystemCommand(&.{ "python3" });
        run.addFileArg(b.path("ci/probe.py"));
        run.addArg(tag);
        run.addArg(if (index == 0) "first" else "second");
        run.addDirectoryArg(cached);
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

tag, selected, cached_path = sys.argv[1:]
root = Path.cwd()
cached = Path(cached_path)
fixture = Path("test") / selected / "platform/targets/native/libhost.a"
other = "second" if selected == "first" else "first"
assert (root / fixture).read_text() == tag
assert (cached / fixture).read_text() == tag
assert not (root / "test" / other / "platform/targets/native/libhost.a").exists()
assert (root / "test/input.roc").read_text() == "fixture input\n"
assert (root / "test/first/platform/targets/native/crt1.o").read_bytes() == b"tracked runtime"
assert (root / "test/first/platform/targets/native/libc.a").read_bytes() == b"tracked archive"
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
        for name in ("src/build", "vendor", "ci", "test/first/platform/targets/native"):
            (root / name).mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, root / "src/build/test_fixtures.zig")
        (root / "build.zig").write_text(BUILD)
        (root / "ci/probe.py").write_text(PROBE)
        for name in ("build.zig.zon", "design.md", "legal_details"):
            (root / name).write_text("fixture metadata\n")
        (root / "build.zig.zon").write_text('.{ .name = .fixture_probe, .version = "0.0.0", .fingerprint = 0xceb1d2ec669ad2f3, .minimum_zig_version = "0.17.0", .paths = .{""} }\n')
        (root / "test/input.roc").write_text("fixture input\n")
        target = root / "test/first/platform/targets/native"
        (target / "crt1.o").write_bytes(b"tracked runtime")
        (target / "libc.a").write_bytes(b"tracked archive")
        # Stale generated checkout artifacts must never become fixture inputs.
        (target / "libhost.a").write_text("stale checkout host")

        def build(tag: str) -> str:
            command = [zig, "build", "verify", "--cache-poison=disallowed", f"-Dtag={tag}", "--cache-dir", str(root / f"cache-{tag}"), "--prefix", str(root / f"out-{tag}")]
            result = subprocess.run(command, cwd=root, capture_output=True, text=True)
            if result.returncode:
                raise AssertionError(result.stdout + result.stderr)
            return result.stdout + result.stderr

        with ThreadPoolExecutor(max_workers=2) as pool:
            list(pool.map(build, ("debug", "fast")))
            # Identical concurrent graphs also share immutable cache entries,
            # while their mutable runner directories must remain independent.
            list(pool.map(build, ("debug", "debug")))
        assert (root / "test/input.roc").read_text() == "fixture input\n"
        assert (target / "libhost.a").read_text() == "stale checkout host"
        assert not (root / "test/generated-app").exists()
    print("Concurrent fixture graph isolation checks passed.")


if __name__ == "__main__":
    main()
