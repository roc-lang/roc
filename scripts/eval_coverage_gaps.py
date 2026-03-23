#!/usr/bin/env python3
"""Analyze kcov coverage data for eval tests and report uncovered gaps.

Usage:
    zig build coverage-eval
    python3 scripts/eval_coverage_gaps.py [--file FILE] [--min-gap N] [--context N]

Options:
    --file FILE     Analyze a specific file (default: interpreter.zig)
    --min-gap N     Minimum gap size to report (default: 3)
    --context N     Lines of source context to show (default: 3)
    --all           Show all files, not just the specified one
"""

import json
import argparse
import sys
from pathlib import Path


def find_coverage_json():
    """Find the codecov.json file in kcov output."""
    base = Path("kcov-output/eval/eval-test-runner")
    # Follow symlink if needed
    if base.is_symlink():
        base = base.resolve()
    codecov = base / "codecov.json"
    if not codecov.exists():
        print("ERROR: Coverage data not found. Run 'zig build coverage-eval' first.",
              file=sys.stderr)
        sys.exit(1)
    return codecov


def find_source_file(basename):
    """Find the full path to a source file given its basename."""
    # Search in src/eval/
    for p in Path("src/eval").rglob(basename):
        return p
    return None


def parse_coverage(codecov_path, target_file):
    """Parse codecov.json and return (covered_lines, uncovered_lines) for target."""
    with open(codecov_path) as f:
        data = json.load(f)

    coverage = data.get("coverage", {})
    if target_file not in coverage:
        # Try matching by basename
        matches = [k for k in coverage if k.endswith(target_file) or target_file.endswith(k)]
        if not matches:
            print(f"ERROR: '{target_file}' not found in coverage data.", file=sys.stderr)
            print(f"Available files: {', '.join(sorted(coverage.keys()))}", file=sys.stderr)
            sys.exit(1)
        target_file = matches[0]

    lines = coverage[target_file]
    covered = sorted(int(k) for k, v in lines.items() if not v.startswith("0/"))
    uncovered = sorted(int(k) for k, v in lines.items() if v.startswith("0/"))
    return target_file, covered, uncovered


def group_ranges(line_numbers):
    """Group line numbers into contiguous ranges."""
    if not line_numbers:
        return []
    ranges = []
    start = prev = line_numbers[0]
    for l in line_numbers[1:]:
        if l == prev + 1:
            prev = l
        else:
            ranges.append((start, prev))
            start = prev = l
    ranges.append((start, prev))
    return ranges


def read_source_lines(filepath, start, end, context=0):
    """Read source lines from a file."""
    try:
        with open(filepath) as f:
            all_lines = f.readlines()
        # Adjust for 0-based indexing
        s = max(0, start - 1 - context)
        e = min(len(all_lines), end + context)
        result = []
        for i in range(s, e):
            line_num = i + 1
            if start <= line_num <= end:
                marker = ">>"
            else:
                marker = "  "
            result.append(f"  {marker} {line_num:5d} | {all_lines[i].rstrip()}")
        return "\n".join(result)
    except FileNotFoundError:
        return f"  (source file not found: {filepath})"


def print_summary(target_file, covered, uncovered):
    """Print coverage summary."""
    total = len(covered) + len(uncovered)
    pct = 100 * len(covered) / total if total > 0 else 0
    print(f"\n{'='*60}")
    print(f"COVERAGE GAPS: {target_file}")
    print(f"{'='*60}")
    print(f"  Covered:   {len(covered):5d} lines")
    print(f"  Uncovered: {len(uncovered):5d} lines")
    print(f"  Total:     {total:5d} lines")
    print(f"  Coverage:  {pct:.1f}%")


def print_all_files_summary(codecov_path):
    """Print summary for all files."""
    with open(codecov_path) as f:
        data = json.load(f)

    coverage = data.get("coverage", {})
    print(f"\n{'='*60}")
    print("ALL FILES COVERAGE SUMMARY")
    print(f"{'='*60}")

    rows = []
    for fname, lines in sorted(coverage.items()):
        total = len(lines)
        uncovered = sum(1 for v in lines.values() if v.startswith("0/"))
        covered = total - uncovered
        pct = 100 * covered / total if total > 0 else 0
        rows.append((fname, covered, uncovered, total, pct))

    # Sort by uncovered count descending
    rows.sort(key=lambda r: r[2], reverse=True)
    for fname, covered, uncovered, total, pct in rows:
        bar = "#" * int(pct / 2) + "." * (50 - int(pct / 2))
        print(f"  {fname:40s} {pct:5.1f}%  {bar}  ({uncovered} uncovered)")
    print()


def main():
    parser = argparse.ArgumentParser(description="Analyze eval test coverage gaps")
    parser.add_argument("--file", default="interpreter.zig",
                        help="File to analyze (default: interpreter.zig)")
    parser.add_argument("--min-gap", type=int, default=3,
                        help="Minimum gap size to report (default: 3)")
    parser.add_argument("--context", type=int, default=3,
                        help="Lines of source context (default: 3)")
    parser.add_argument("--all", action="store_true",
                        help="Show summary for all files")
    args = parser.parse_args()

    codecov_path = find_coverage_json()

    if args.all:
        print_all_files_summary(codecov_path)

    target_file, covered, uncovered = parse_coverage(codecov_path, args.file)
    print_summary(target_file, covered, uncovered)

    # Find source file
    source_path = find_source_file(target_file)

    # Group into ranges
    ranges = group_ranges(uncovered)
    ranges.sort(key=lambda r: r[1] - r[0], reverse=True)

    # Filter by min-gap
    ranges = [(s, e) for s, e in ranges if (e - s + 1) >= args.min_gap]

    print(f"\n  {len(ranges)} uncovered ranges of {args.min_gap}+ lines:\n")

    for i, (start, end) in enumerate(ranges):
        size = end - start + 1
        print(f"  --- Gap #{i+1}: lines {start}-{end} ({size} lines) ---")
        if source_path:
            print(read_source_lines(str(source_path), start, end, context=args.context))
        print()

        # Stop after 50 gaps to avoid overwhelming output
        if i >= 49:
            remaining = len(ranges) - 50
            print(f"  ... and {remaining} more gaps. Use --min-gap to filter.\n")
            break


if __name__ == "__main__":
    main()
