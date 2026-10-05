#!/usr/bin/env python3
"""Check actual generated compiler identities without compiling the Roc CLI.

Builds a small Debug MiniCI leaf, then imports its real generated Options and
compiler identity modules into constant-only cross-target objects. This checks
cache correctness; it is not a compiler performance benchmark.
"""

import argparse
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[1]
OS_RANGE_TESTS = r'''
const std = @import("std");
const options = @import("build_options");
const Range = std.Target.Os.TaggedVersionRange;
const semver: std.SemanticVersion.Range = .{
    .min = .{ .major = 5, .minor = 10, .patch = 1 },
    .max = .{ .major = 6, .minor = 1, .patch = 2 },
};
const glibc: std.SemanticVersion = .{ .major = 2, .minor = 31, .patch = 1 };

fn distinct(comptime before: Range, comptime after: Range) !void {
    @setEvalBranchQuota(1_000_000);
    try std.testing.expect(!std.mem.eql(u8,
        options.compilerOsRangeEncoding(before),
        options.compilerOsRangeEncoding(after),
    ));
}

test "Prior display formatting collides for nested kernel bounds" {
    const before: Range = .{ .linux = .{ .range = semver, .glibc = glibc, .android = 29 } };
    comptime var after = before;
    after.linux.range.min.major += 1;
    const old_before = comptime std.fmt.comptimePrint("{any}", .{before});
    const old_after = comptime std.fmt.comptimePrint("{any}", .{after});
    try std.testing.expectEqualStrings(old_before, old_after);
    try distinct(before, after);
}

test "Linux kernel bounds, glibc components and Android API all participate" {
    const before: Range = .{ .linux = .{ .range = semver, .glibc = glibc, .android = 29 } };
    inline for (.{ "min", "max" }) |bound| {
        inline for (.{ "major", "minor", "patch" }) |component| {
            comptime var after = before;
            @field(@field(after.linux.range, bound), component) += 1;
            try distinct(before, after);
        }
    }
    inline for (.{ "major", "minor", "patch" }) |component| {
        comptime var after = before;
        @field(after.linux.glibc, component) += 1;
        try distinct(before, after);
    }
    comptime var after = before;
    after.linux.android += 1;
    try distinct(before, after);
}

test "Hurd kernel bounds and glibc components all participate" {
    const before: Range = .{ .hurd = .{ .range = semver, .glibc = glibc } };
    inline for (.{ "min", "max" }) |bound| {
        inline for (.{ "major", "minor", "patch" }) |component| {
            comptime var after = before;
            @field(@field(after.hurd.range, bound), component) += 1;
            try distinct(before, after);
        }
    }
    inline for (.{ "major", "minor", "patch" }) |component| {
        comptime var after = before;
        @field(after.hurd.glibc, component) += 1;
        try distinct(before, after);
    }
}

test "Semver bounds and optional pre/build strings retain exact framing" {
    const before: Range = .{ .semver = semver };
    inline for (.{ "min", "max" }) |bound| {
        inline for (.{ "major", "minor", "patch" }) |component| {
            comptime var after = before;
            @field(@field(after.semver, bound), component) += 1;
            try distinct(before, after);
        }
        inline for (.{ "pre", "build" }) |field| {
            inline for (.{ "", "a", "aa", "\";build=other" }) |text| {
                comptime var after = before;
                @field(@field(after.semver, bound), field) = text;
                try distinct(before, after);
            }
            comptime var short = before;
            @field(@field(short.semver, bound), field) = "a";
            comptime var long = before;
            @field(@field(long.semver, bound), field) = "aa";
            try distinct(short, long);
        }
    }
    comptime var first = before;
    first.semver.min.pre = "a";
    first.semver.min.build = "bc";
    comptime var second = before;
    second.semver.min.pre = "ab";
    second.semver.min.build = "c";
    try distinct(first, second);
}

test "Known and unknown Windows version bounds retain both backing values" {
    const before: Range = .{ .windows = .{ .min = .win10, .max = .win11_kr } };
    inline for (.{ "min", "max" }) |bound| {
        comptime var named = before;
        @field(named.windows, bound) = .win10_rs5;
        try distinct(before, named);
        comptime var unknown_a = before;
        @field(unknown_a.windows, bound) = @fromBackingInt(0x0A000099);
        comptime var unknown_b = before;
        @field(unknown_b.windows, bound) = @fromBackingInt(0x0A00009A);
        try distinct(before, unknown_a);
        try distinct(unknown_a, unknown_b);
    }
    try std.testing.expect(std.mem.indexOf(u8,
        options.compilerOsRangeEncoding(.{ .windows = .{
            .min = @fromBackingInt(0x0A000099), .max = .win11_kr,
        } }), "167772313") != null);
}

test "Version range union tags are explicit" {
    const encoded = options.compilerOsRangeEncoding(.{ .none = {} });
    try std.testing.expectEqualStrings("{\"none\":{}}", encoded);
    try distinct(.{ .none = {} }, .{ .semver = semver });
    try distinct(.{ .semver = semver }, .{ .hurd = .{ .range = semver, .glibc = glibc } });
}
'''

CASES = (
    ("linux", "fast", "x86_64-linux.5.10...6.0-musl", "baseline"),
    ("safe", "safe", "x86_64-linux.5.10...6.0-musl", "baseline"),
    ("debug", "debug", "x86_64-linux.5.10...6.0-musl", "baseline"),
    ("small", "small", "x86_64-linux.5.10...6.0-musl", "baseline"),
    ("linux-min", "fast", "x86_64-linux.6.0...6.0-musl", "baseline"),
    ("linux-max", "fast", "x86_64-linux.5.10...6.1-musl", "baseline"),
    ("features", "fast", "x86_64-linux.5.10...6.0-musl", "baseline+avx2"),
    ("glibc", "fast", "x86_64-linux.5.10...6.0-gnu.2.31", "baseline"),
    ("glibc-version", "fast", "x86_64-linux.5.10...6.0-gnu.2.32", "baseline"),
    ("android", "fast", "aarch64-linux.5.10...6.0-android.29", "baseline"),
    ("android-api", "fast", "aarch64-linux.5.10...6.0-android.30", "baseline"),
    ("windows", "fast", "x86_64-windows.win10...win11_kr-gnu", "baseline"),
    ("windows-min", "fast", "x86_64-windows.win10_rs5...win11_kr-gnu", "baseline"),
    ("windows-max", "fast", "x86_64-windows.win10...win11_br-gnu", "baseline"),
    ("macos", "fast", "aarch64-macos.11.0...12.0-none", "baseline"),
    ("macos-min", "fast", "aarch64-macos.12.0...12.0-none", "baseline"),
    ("macos-max", "fast", "aarch64-macos.11.0...13.0-none", "baseline"),
    ("aarch64-linux", "fast", "aarch64-linux.5.10...6.0-musl", "baseline"),
    ("freestanding", "fast", "wasm32-freestanding-none", "baseline"),
)


def run(command, work, label, env):
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, env=env)
    (work / f"{label}.log").write_text(result.stdout + result.stderr)
    (work / f"{label}-command.json").write_text(json.dumps(command, indent=2) + "\n")
    assert result.returncode == 0, (f"See {work / f'{label}.log'}\n" +
                                  "\n".join((result.stdout + result.stderr).splitlines()[:30]))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("zig")
    parser.add_argument("--work-dir", type=Path)
    args, options = parser.parse_known_args()
    if options[:1] == ["--"]:
        options = options[1:]
    work = (args.work_dir or Path(tempfile.mkdtemp(prefix="roc-artifact-identity-"))).resolve()
    work.mkdir(parents=True, exist_ok=True)
    assert not (work / "results.json").exists(), "Use a new work directory to retain evidence"
    env = os.environ.copy()
    env.setdefault("ZIG_GLOBAL_CACHE_DIR", str(work / "global-cache"))
    if not any(option.startswith(("-Droc-deps-path", "-Dllvm-path", "-Dsystem-llvm"))
               for option in options):
        for directory in ("include", "lib"):
            (work / "dependencies" / directory).mkdir(parents=True)
        options.append(f"-Droc-deps-path={work / 'dependencies'}")
    zig_lib = [option for option in options if option.startswith("--zig-lib=")]
    options = [option for option in options if option not in zig_lib]
    graph = run([args.zig, "build", *zig_lib, "run-test-zig-minici", "-j1", "--verbose",
                 "--summary", "all", "--cache-poison=disallowed", "--cache-dir", str(work / "cache"),
                 "--prefix", str(work / "out"), *options, "--",
                 "--test-filter=__artifact_identity_probe_no_tests__"], work, "graph", env)
    compile_args = next(shlex.split(line.removeprefix("info(verbose): "))
                        for line in graph.stderr.splitlines()
                        if "-Mcompiler_identity=" in line and "--name minici_test " in line)
    modules = [next(arg for arg in compile_args if arg.startswith(prefix))
               for prefix in ("-Mbuild_options=", "-Mcompiler_identity=")]
    stdlib = compile_args[compile_args.index("--zig-lib-dir") + 1]
    common = ["--dep", "build_options"]
    generated_modules = ["--dep", "compiler_identity", *modules, "--zig-lib-dir", stdlib,
                         "--cache-dir", str(work / "cache"), "--global-cache-dir", env["ZIG_GLOBAL_CACHE_DIR"]]
    fixture = work / "range-tests.zig"
    fixture.write_text(OS_RANGE_TESTS)
    tests = run([args.zig, "test", "-Osafe", "-fllvm", *common, f"-Mroot={fixture}",
                 *generated_modules], work, "os-range-tests", env)
    assert "All 6 tests passed." in tests.stderr, tests.stderr
    fixture = work / "identity-object.zig"
    fixture.write_text('const options = @import("build_options");\n'
                       'const text = "ROC_FINAL_ID:" ++ options.compiler_compatibility_id;\n'
                       'export const production_identity: [text.len]u8 = text.*;\n')
    results = {}
    for label, mode, target, cpu in CASES:
        output = work / f"{label}.o"
        run([args.zig, "build-obj", f"-O{mode}", "-fllvm", "-target", target, "-mcpu", cpu,
             *common, f"-Mroot={fixture}", *generated_modules, f"-femit-bin={output}"],
            work, label, env)
        identities = set(re.findall(rb"ROC_FINAL_ID:([0-9a-f]{64})", output.read_bytes()))
        assert len(identities) == 1, f"{label}: expected one emitted identity"
        results[label] = next(iter(identities)).decode()
    assert len(set(results.values())) == len(CASES), "Distinct semantic targets/modes collided"
    base = Path(modules[1].split("=", 1)[1]).read_text()
    base_id = re.search(r'compiler_compatibility_id = "([0-9a-f]{64})"', base).group(1)
    (work / "results.json").write_text(json.dumps({
        "base_identity": base_id, "case_identities": results,
        "synthetic_range_tests": 6, "cross_objects_executed": False,
    }, indent=2) + "\n")
    print(f"Six OS-range tests and {len(CASES)} actual generated-module mode/target identities passed.")
    print(work)


if __name__ == "__main__":
    main()
