#!/usr/bin/env python3
"""Eval interpreter coverage analysis tool.

Runs kcov coverage on the Roc eval test suite and reports results in formats
useful for humans and LLMs. Designed to help identify uncovered interpreter
code so new eval tests can be written to increase coverage.

Usage:
    # Full run: clean, rebuild, collect coverage, print summary
    python3 CONTRIBUTING/eval_coverage.py

    # Reuse last kcov run (fast, no rebuild)
    python3 CONTRIBUTING/eval_coverage.py --use-last-run

    # Show uncovered line ranges with source context
    python3 CONTRIBUTING/eval_coverage.py --use-last-run --format lines

    # JSON output for LLM consumption
    python3 CONTRIBUTING/eval_coverage.py --use-last-run --format json

    # Focus on a specific file
    python3 CONTRIBUTING/eval_coverage.py --use-last-run --format lines --file interpreter

    # Show top 3 files by uncovered lines
    python3 CONTRIBUTING/eval_coverage.py --use-last-run --top 3
"""

import argparse
import json
import os
import platform
import shutil
import subprocess
import sys
from pathlib import Path

# Paths relative to repo root
KCOV_OUTPUT_DIR = "kcov-output/eval"
KCOV_RESULT_DIR = "kcov-output/eval/eval-coverage-runner"
COVERAGE_JSON = f"{KCOV_RESULT_DIR}/coverage.json"
CODECOV_JSON = f"{KCOV_RESULT_DIR}/codecov.json"
EVAL_SRC_DIR = "src/eval"


def get_repo_root():
    """Find the repo root by looking for build.zig."""
    path = Path(__file__).resolve().parent.parent
    if (path / "build.zig").exists():
        return path
    # Fallback: try cwd
    cwd = Path.cwd()
    if (cwd / "build.zig").exists():
        return cwd
    print("Error: cannot find repo root (no build.zig found).", file=sys.stderr)
    sys.exit(1)


def check_platform():
    """Check that kcov coverage is supported on this platform."""
    system = platform.system()
    machine = platform.machine()

    if system == "Darwin":
        # macOS: both arm64 and x86_64 supported
        return

    if system == "Linux":
        if machine in ("aarch64", "arm64"):
            return
        print(
            f"Error: kcov coverage is not supported on Linux {machine}.\n"
            "\n"
            "Zig 0.15.2 generates invalid DWARF .debug_line sections on x86_64,\n"
            "which prevents kcov from finding source files. Only arm64 Linux works.\n"
            "\n"
            "Supported platforms:\n"
            "  - macOS (arm64, x86_64)\n"
            "  - Linux arm64\n"
            "\n"
            "On Linux arm64 you also need: apt install libdw-dev libcurl4-openssl-dev",
            file=sys.stderr,
        )
        sys.exit(1)

    print(
        f"Error: kcov coverage is not supported on {system}.\n"
        "Supported platforms: macOS, Linux arm64.",
        file=sys.stderr,
    )
    sys.exit(1)


def clean_old_data(root):
    """Remove previous kcov output."""
    output_dir = root / KCOV_OUTPUT_DIR
    if output_dir.exists():
        shutil.rmtree(output_dir)
        print(f"Cleaned {output_dir}")


def run_coverage(root):
    """Run zig build coverage-eval."""
    print("Running: zig build coverage-eval")
    print("(This builds kcov, the eval test runner, then runs all eval tests")
    print(" single-threaded under kcov instrumentation. This takes a while.)\n")
    result = subprocess.run(
        ["zig", "build", "coverage-eval"],
        cwd=root,
    )
    if result.returncode != 0:
        print("\nzig build coverage-eval failed.", file=sys.stderr)
        sys.exit(result.returncode)
    print()


def load_summary(root):
    """Load coverage.json (per-file summary)."""
    path = root / COVERAGE_JSON
    if not path.exists():
        print(
            f"Error: {COVERAGE_JSON} not found.\n"
            "Run without --use-last-run to collect coverage first.",
            file=sys.stderr,
        )
        sys.exit(1)
    with open(path) as f:
        return json.load(f)


def load_line_data(root):
    """Load codecov.json (per-line hit counts)."""
    path = root / CODECOV_JSON
    if not path.exists():
        print(
            f"Error: {CODECOV_JSON} not found.\n"
            "Run without --use-last-run to collect coverage first.",
            file=sys.stderr,
        )
        sys.exit(1)
    with open(path) as f:
        return json.load(f)["coverage"]


def parse_hit_count(value):
    """Parse kcov hit string like '0/3' -> (hits, total_probes)."""
    parts = value.split("/")
    return int(parts[0]), int(parts[1])


def get_uncovered_ranges(line_data):
    """Convert per-line data into contiguous uncovered ranges.

    Returns list of (start_line, end_line) tuples for uncovered ranges.
    """
    uncovered = sorted(
        int(line)
        for line, hits in line_data.items()
        if parse_hit_count(hits)[0] == 0
    )
    if not uncovered:
        return []

    ranges = []
    start = uncovered[0]
    prev = uncovered[0]
    for line in uncovered[1:]:
        if line == prev + 1:
            prev = line
        else:
            ranges.append((start, prev))
            start = line
            prev = line
    ranges.append((start, prev))
    return ranges


def file_sort_key(file_entry):
    """Sort files by uncovered lines descending."""
    total = int(file_entry["total_lines"])
    covered = int(file_entry["covered_lines"])
    return -(total - covered)


def filter_files(summary, line_data, file_pattern, exclude_test):
    """Filter file lists by pattern and test exclusion."""
    filtered_summary = []
    filtered_line_data = {}

    for f in summary["files"]:
        basename = Path(f["file"]).name
        rel = f["file"]  # full path in coverage.json

        if exclude_test and "/test/" in rel:
            continue
        if file_pattern and file_pattern.lower() not in rel.lower():
            continue

        filtered_summary.append(f)

        # Match summary file to codecov key (codecov uses relative names)
        for key in line_data:
            # codecov keys are like "interpreter.zig" or "test/helpers.zig"
            if rel.endswith(key) or basename == Path(key).name:
                filtered_line_data[key] = line_data[key]

    return filtered_summary, filtered_line_data


def format_summary(summary, top_n):
    """Format a human-readable coverage summary table."""
    files = sorted(summary["files"], key=file_sort_key)
    if top_n:
        files = files[:top_n]

    lines = []
    lines.append(f"Eval coverage: {summary['percent_covered']}% "
                 f"({summary['covered_lines']}/{summary['total_lines']} lines)")
    lines.append(f"Date: {summary.get('date', 'unknown')}")
    lines.append("")

    # Table header
    header = f"{'File':<35} {'Coverage':>8} {'Covered':>8} {'Total':>7} {'Uncovered':>10}"
    lines.append(header)
    lines.append("-" * len(header))

    for f in files:
        name = Path(f["file"]).name
        total = int(f["total_lines"])
        covered = int(f["covered_lines"])
        uncovered = total - covered
        pct = f["percent_covered"]
        lines.append(
            f"{name:<35} {pct:>7}% {covered:>8} {total:>7} {uncovered:>10}"
        )

    return "\n".join(lines)


def format_lines(summary, line_data, root, top_n, context):
    """Format uncovered line ranges with source context."""
    files = sorted(summary["files"], key=file_sort_key)
    if top_n:
        files = files[:top_n]

    sections = []

    for f in files:
        rel_path = f["file"]
        basename = Path(rel_path).name
        total = int(f["total_lines"])
        covered = int(f["covered_lines"])
        uncovered = total - covered
        pct = f["percent_covered"]

        if uncovered == 0:
            continue

        # Find matching codecov key
        codecov_key = None
        for key in line_data:
            if rel_path.endswith(key) or basename == Path(key).name:
                codecov_key = key
                break

        if codecov_key is None:
            continue

        ranges = get_uncovered_ranges(line_data[codecov_key])
        if not ranges:
            continue

        section_lines = []
        section_lines.append(f"## {basename} — {pct}% covered ({uncovered} uncovered lines)")
        section_lines.append("")

        # Try to read source for context
        source_path = root / EVAL_SRC_DIR / codecov_key
        source_lines = None
        if source_path.exists():
            with open(source_path) as sf:
                source_lines = sf.readlines()

        for start, end in ranges:
            ctx_start = max(1, start - context)
            ctx_end = end + context

            section_lines.append(f"### Lines {start}-{end} (uncovered)")

            if source_lines:
                section_lines.append("```zig")
                for i in range(ctx_start, min(ctx_end + 1, len(source_lines) + 1)):
                    prefix = ">" if start <= i <= end else " "
                    line_text = source_lines[i - 1].rstrip()
                    section_lines.append(f"{prefix} {i:>5} | {line_text}")
                section_lines.append("```")
            section_lines.append("")

        sections.append("\n".join(section_lines))

    header = (
        f"Eval coverage: {summary['percent_covered']}% "
        f"({summary['covered_lines']}/{summary['total_lines']} lines)\n"
    )
    return header + "\n" + "\n".join(sections)


def format_json(summary, line_data, top_n):
    """Format structured JSON output for LLM consumption."""
    files = sorted(summary["files"], key=file_sort_key)
    if top_n:
        files = files[:top_n]

    result = {
        "overall": {
            "percent_covered": float(summary["percent_covered"]),
            "covered_lines": summary["covered_lines"],
            "total_lines": summary["total_lines"],
            "date": summary.get("date", "unknown"),
        },
        "files": [],
    }

    for f in files:
        rel_path = f["file"]
        basename = Path(rel_path).name
        total = int(f["total_lines"])
        covered = int(f["covered_lines"])

        # Find matching codecov key
        codecov_key = None
        for key in line_data:
            if rel_path.endswith(key) or basename == Path(key).name:
                codecov_key = key
                break

        ranges = []
        if codecov_key and codecov_key in line_data:
            ranges = get_uncovered_ranges(line_data[codecov_key])

        result["files"].append({
            "file": basename,
            "path": rel_path,
            "percent_covered": float(f["percent_covered"]),
            "covered_lines": covered,
            "total_lines": total,
            "uncovered_lines": total - covered,
            "uncovered_ranges": [
                {"start": s, "end": e} for s, e in ranges
            ],
        })

    return json.dumps(result, indent=2)


def main():
    parser = argparse.ArgumentParser(
        description="Eval interpreter coverage analysis tool.",
        epilog=(
            "examples:\n"
            "  %(prog)s                                  # full run\n"
            "  %(prog)s --use-last-run                   # reuse cached data\n"
            "  %(prog)s --use-last-run -f lines          # show uncovered source\n"
            "  %(prog)s --use-last-run -f json           # structured output\n"
            "  %(prog)s --use-last-run -f lines --file interpreter\n"
            "  %(prog)s --use-last-run --top 3\n"
            "  %(prog)s --use-last-run -f lines --file interpreter --top 5 --context 5\n"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--use-last-run",
        action="store_true",
        help="Skip cleanup and rebuild; analyze existing kcov data.",
    )
    parser.add_argument(
        "--format", "-f",
        choices=["summary", "json", "lines"],
        default="summary",
        help=(
            "Output format. 'summary' (default): coverage table. "
            "'json': structured data with uncovered ranges. "
            "'lines': uncovered source code with context."
        ),
    )
    parser.add_argument(
        "--file",
        metavar="PATTERN",
        help="Filter to files whose path contains PATTERN (case-insensitive).",
    )
    parser.add_argument(
        "--top",
        metavar="N",
        type=int,
        help="Show only the top N files ranked by uncovered line count.",
    )
    parser.add_argument(
        "--context",
        metavar="N",
        type=int,
        default=2,
        help="Lines of source context around uncovered ranges (default: 2, used with --format lines).",
    )
    parser.add_argument(
        "--include-test-files",
        action="store_true",
        help="Include test infrastructure files (test/, parallel_runner, etc.) in output.",
    )

    args = parser.parse_args()
    root = get_repo_root()

    if not args.use_last_run:
        check_platform()
        clean_old_data(root)
        run_coverage(root)

    # Load data
    summary = load_summary(root)
    line_data = load_line_data(root)

    # Filter
    exclude_test = not args.include_test_files
    summary_files, filtered_line_data = filter_files(
        summary, line_data, args.file, exclude_test
    )

    # Build a filtered summary dict for formatting
    filtered_summary = dict(summary)
    filtered_summary["files"] = summary_files

    # Recalculate totals when filtering
    if args.file or exclude_test:
        total = sum(int(f["total_lines"]) for f in summary_files)
        covered = sum(int(f["covered_lines"]) for f in summary_files)
        filtered_summary["total_lines"] = total
        filtered_summary["covered_lines"] = covered
        filtered_summary["percent_covered"] = (
            f"{covered / total * 100:.2f}" if total > 0 else "0.00"
        )

    # Format and print
    if args.format == "summary":
        print(format_summary(filtered_summary, args.top))
    elif args.format == "json":
        print(format_json(filtered_summary, filtered_line_data, args.top))
    elif args.format == "lines":
        print(format_lines(
            filtered_summary, filtered_line_data, root, args.top, args.context
        ))


if __name__ == "__main__":
    main()
