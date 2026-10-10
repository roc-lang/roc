#!/usr/bin/env python3
"""Print the distinct instruction mnemonics in an x86-64 ELF's function bodies.

`objdump -d` decodes everything between one symbol and the next as
instructions. Function symbols do not tile `.text`: the bytes after a
function's last `ret` are padding, string literals, or whatever stale data
the object writer left in the gap (the self-hosted backend's objects carry
leftover DWARF and old code there). Decoded as instructions, that data
produces arbitrary mnemonics, including ones above the baseline, so a
disassembly of the whole section reports instructions that no execution can
reach, and which ones it reports depends on how the garbage happens to
align.

This keeps only the instructions that lie inside a sized function symbol,
which is the part of `.text` that is known to be code. Padding after a
function and data between functions are left out; an above-baseline
instruction in a function body is still reported.
"""

import bisect
import re
import subprocess
import sys

# ELF symbol types that name executable code. Type OBJECT symbols can also live
# in `.text` (the self-hosted backend places its lazily generated lookup tables
# there), and those are data.
CODE_SYMBOL_TYPES = frozenset(("FUNC", "GNU_IFUNC"))
INSTRUCTION_LINE = re.compile(r"^\s*([0-9a-f]+):\s+(\S+)")


def run(*command: str) -> str:
    return subprocess.run(command, check=True, capture_output=True, text=True).stdout


def function_ranges(binary: str) -> list[tuple[int, int]]:
    """Return the sorted, merged [start, end) ranges of sized function symbols."""
    ranges = []
    # Columns: Num: Value Size Type Bind Vis Ndx Name. Value is hex and Size is
    # decimal, except that readelf switches Size to hex when it is large.
    for line in run("readelf", "--syms", "--wide", binary).splitlines():
        fields = line.split()
        if len(fields) < 8 or fields[3] not in CODE_SYMBOL_TYPES or fields[6] == "UND":
            continue
        start = int(fields[1], 16)
        size = int(fields[2], 16) if fields[2].startswith("0x") else int(fields[2])
        if size > 0:
            ranges.append((start, start + size))
    ranges.sort()

    merged: list[tuple[int, int]] = []
    for start, end in ranges:
        if merged and start <= merged[-1][1]:
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            merged.append((start, end))
    return merged


def main() -> None:
    binary = sys.argv[1]
    ranges = function_ranges(binary)
    if not ranges:
        raise SystemExit(f"{binary} has no sized function symbols; the scan would pass vacuously")
    starts = [start for start, _ in ranges]

    mnemonics = set()
    listing = run("objdump", "-d", "--no-show-raw-insn", "--section=.text", binary)
    for line in listing.splitlines():
        match = INSTRUCTION_LINE.match(line)
        if match is None:
            continue
        address = int(match.group(1), 16)
        index = bisect.bisect_right(starts, address) - 1
        if index >= 0 and address < ranges[index][1]:
            mnemonics.add(match.group(2))
    print("\n".join(sorted(mnemonics)))


if __name__ == "__main__":
    main()
