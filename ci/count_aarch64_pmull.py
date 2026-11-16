#!/usr/bin/env python3
"""Count PMULL/PMULL2 instructions in an aarch64 ELF's executable sections.

PMULL comes from the AES extension rather than base NEON, so an Armv8.0-A
binary must contain none. This reads the section table and scans aligned
32-bit words directly, so it needs no cross disassembler; runners that can
disassemble aarch64 are not guaranteed.

Every executable section is scanned, not just `.text`: a fast dev link places
the Roc program's code in its own `.text.roc` section beside the host's
`.text`.
"""

import struct
import sys

# PMULL (0x0EE0E000) and PMULL2 (0x4EE0E000), with the Rm, Rn, and Rd operand
# fields masked out.
OPERAND_MASK = 0xFFE0FC00
ENCODINGS = (0x0EE0E000, 0x4EE0E000)

SHT_NOBITS = 8
SHF_EXECINSTR = 0x4


def executable_sections(data: bytes) -> list[bytes]:
    (e_shoff,) = struct.unpack_from("<Q", data, 0x28)
    (e_shentsize,) = struct.unpack_from("<H", data, 0x3A)
    (e_shnum,) = struct.unpack_from("<H", data, 0x3C)

    sections = []
    for index in range(e_shnum):
        start = e_shoff + index * e_shentsize
        (sh_type,) = struct.unpack_from("<I", data, start + 0x04)
        (sh_flags,) = struct.unpack_from("<Q", data, start + 0x08)
        if sh_type == SHT_NOBITS or not sh_flags & SHF_EXECINSTR:
            continue
        (offset,) = struct.unpack_from("<Q", data, start + 0x18)
        (size,) = struct.unpack_from("<Q", data, start + 0x20)
        if size:
            sections.append(data[offset : offset + size])
    return sections


def main() -> None:
    with open(sys.argv[1], "rb") as handle:
        sections = executable_sections(handle.read())
    if not sections:
        raise SystemExit("no non-empty executable section found; the scan would pass vacuously")
    count = sum(
        1
        for code in sections
        for offset in range(0, len(code) - 3, 4)
        if int.from_bytes(code[offset : offset + 4], "little") & OPERAND_MASK in ENCODINGS
    )
    print(count)


if __name__ == "__main__":
    main()
