"""Paired optimized-runtime measurement for roc-lang/roc#13035.

No timing threshold: report the measurements, not a machine-dependent verdict.
Build outputs remain under zig-out so perf/disassembly can inspect the same pair.
"""

import argparse
from pathlib import Path
import statistics
import subprocess
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roc", type=Path, default=Path("zig-out/bin/roc"))
    parser.add_argument("--loops", type=int, default=1_000_000)
    parser.add_argument("--samples", type=int, default=9)
    args = parser.parse_args()
    if args.loops < 1 or args.samples < 3:
        parser.error("use positive loops and at least three samples")

    root = Path(__file__).resolve().parents[3]
    output = root / "zig-out/repros/issue-13035"
    output.mkdir(parents=True, exist_ok=True)
    source = Path(__file__).with_name("inhabited.roc").read_text()
    assert source.count("Try(List(U8), [E])") == 2
    sources = {
        "empty": source.replace("Try(List(U8), [E])", "Try(List(U8), [])"),
        "inhabited": source,
    }
    binaries = {}
    for name, text in sources.items():
        path = output / f"{name}.roc"
        path.write_text(text)
        binary = output / f"{name}.bin"
        binary.unlink(missing_ok=True)
        build = subprocess.run(
            [str(args.roc.resolve()), "build", "--opt=speed", str(path),
             f"--output={binary}"],
            check=False,
        )
        # Roc returns 2 for warnings; only the empty-row control is expected
        # to warn (its Err arm cannot match). Errors still fail the benchmark.
        if build.returncode not in ((0, 2) if name == "empty" else (0,)):
            build.check_returncode()
        assert binary.is_file(), f"Build did not produce {binary}"
        binaries[name] = binary

    # The issue's alternating lengths prevent a single constant run length.
    expected = str(1000 * args.loops + args.loops // 2)

    def run(name):
        start = time.perf_counter()
        result = subprocess.run(
            [str(binaries[name]), str(args.loops)],
            capture_output=True, text=True, check=True,
        )
        elapsed = time.perf_counter() - start
        assert result.stdout.strip() == expected, (name, result.stdout, expected)
        return elapsed

    for name in binaries:
        run(name)
    measurements = {name: [] for name in binaries}
    for sample in range(args.samples):
        # Alternate order to reduce systematic drift bias.
        order = ("empty", "inhabited") if sample % 2 == 0 else ("inhabited", "empty")
        for name in order:
            measurements[name].append(run(name))
    for name, values in measurements.items():
        print(f"{name}: seconds = " + ", ".join(f"{v:.6f}" for v in values))
        print(f"{name}: median = {statistics.median(values):.6f}s")
    ratio = statistics.median(measurements["inhabited"]) / statistics.median(
        measurements["empty"]
    )
    print(f"inhabited / empty median ratio = {ratio:.3f}x")
    print(f"All outputs matched {expected}. Binaries: {output}")


if __name__ == "__main__":
    main()
