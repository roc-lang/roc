#!/usr/bin/env python3
"""Paired whole-compiler ARC measurements when the pinned CI driver is unavailable.

Uses the CI suite's syntax fixtures, separate warm caches for each compiler,
both lowering strategies, randomized pair order, and GNU time's peak RSS.
This is a local Linux comparison, not a replacement for ci/perf/bench.roc.
"""

import argparse
import csv
import math
from pathlib import Path
import random
import shutil
import statistics
import subprocess
import tempfile
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--pairs", type=int, default=30)
    args = parser.parse_args()
    if args.pairs < 10:
        parser.error("--pairs must be at least 10")
    repo = Path(__file__).resolve().parents[3]
    binaries = {"base": args.base.resolve(), "candidate": args.candidate.resolve()}
    args.out.parent.mkdir(parents=True, exist_ok=True)
    rng = random.Random(13043)
    cases = []
    for specialize in ("yes", "no"):
        for fixture in ("single", "modules"):
            for command in ("check", "interpreter", "dev", "build-dev"):
                cases.append((specialize, fixture, command, "cold"))
        for command in ("check", "dev"):
            for scenario in ("warm", "edit-entry", "edit-leaf", "edit-shared"):
                cases.append((specialize, "modules", command, scenario))
    samples = {case: [] for case in cases}

    with tempfile.TemporaryDirectory(prefix="arc-bench-", dir=args.out.parent) as temp:
        work = Path(temp).resolve()
        # Compare equivalent artifact placements, not an installed executable
        # against a copied one. Keep each CLI's executable name `roc` too.
        for side, binary in binaries.items():
            executable = work / "compilers" / side / "roc"
            executable.parent.mkdir(parents=True)
            shutil.copy2(binary, executable)
            binaries[side] = executable
        projects = {}
        for number, case in enumerate(cases):
            specialize, fixture, command, scenario = case
            for side in binaries:
                directory = work / str(number) / side
                directory.mkdir(parents=True)
                if fixture == "modules":
                    shutil.copytree(repo / "test/perf/all_syntax_modules", directory / "src")
                    source = directory / "src/main.roc"
                else:
                    source = repo / "test/echo/all_syntax_test.roc"
                projects[case, side] = directory, source

        def run(case, side, iteration):
            specialize, fixture, command, scenario = case
            directory, source = projects[case, side]
            edits = {"edit-entry": "main.roc", "edit-leaf": "Leaf.roc", "edit-shared": "Shared.roc"}
            if scenario in edits:
                original = repo / "test/perf/all_syntax_modules" / edits[scenario]
                text = original.read_text()
                assert "perf_edit = 0" in text
                (source.parent / edits[scenario]).write_text(
                    text.replace("perf_edit = 0", f"perf_edit = {iteration}")
                )
            cmd = [str(binaries[side])]
            if command == "check":
                cmd += ["check"]
            elif command == "build-dev":
                cmd += ["build", "--opt=dev", f"--output={directory / 'app'}"]
            else:
                cmd += [f"--opt={command}"]
            cmd += [f"--specialize={specialize}"]
            if scenario == "cold":
                cmd += ["--no-cache"]
            cmd += [str(source)]
            rss = directory / "rss.txt"
            started = time.perf_counter_ns()
            result = subprocess.run(
                ["/usr/bin/time", "-f", "%M", "-o", str(rss), *cmd],
                cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                timeout=120,
            )
            elapsed = time.perf_counter_ns() - started
            if result.returncode:
                raise RuntimeError(f"{cmd!r}\n{result.stdout.decode()}\n{result.stderr.decode()}")
            return elapsed, int(rss.read_text().strip())

        for iteration in range(1, 3):
            for case in cases:
                for side in binaries:
                    run(case, side, iteration)

        with args.out.open("w", newline="") as output:
            writer = csv.writer(output, delimiter="\t")
            writer.writerow(("case", "pair", "side", "elapsed_ns", "peak_rss_kib"))
            for pair in range(args.pairs):
                order = list(cases)
                rng.shuffle(order)
                for case in order:
                    sides = list(binaries)
                    rng.shuffle(sides)
                    measured = {}
                    for side in sides:
                        measured[side] = run(case, side, pair + 3)
                        writer.writerow(("/".join(case), pair, side, *measured[side]))
                    samples[case].append(measured)
                output.flush()
                print(f"Completed pair {pair + 1}/{args.pairs}", flush=True)

    # Distribution-free median intervals, Bonferroni corrected across cases.
    n = args.pairs
    limit = 0.05 / (2 * len(cases))
    lower = -1
    probability = 0.0
    for k in range(n // 2):
        probability += math.comb(n, k) / 2**n
        if probability > limit:
            break
        lower = k
    print("case\tcandidate/base\t95% family interval\tbase ms\tcandidate ms\tRSS ratio")
    for case, pairs in samples.items():
        ratios = sorted(pair["candidate"][0] / pair["base"][0] for pair in pairs)
        low, high = (ratios[lower], ratios[n - lower - 1]) if lower >= 0 else (0, math.inf)
        base = statistics.median(pair["base"][0] for pair in pairs) / 1e6
        candidate = statistics.median(pair["candidate"][0] for pair in pairs) / 1e6
        rss = statistics.median(pair["candidate"][1] / pair["base"][1] for pair in pairs)
        print(f"{'/'.join(case)}\t{statistics.median(ratios):.4f}\t"
              f"[{low:.4f}, {high:.4f}]\t{base:.2f}\t{candidate:.2f}\t{rss:.4f}")


if __name__ == "__main__":
    main()
