#!/usr/bin/env python3
"""Count PMULL/PMULL2 instructions in an aarch64 ELF's .text section.

PMULL comes from the AES extension rather than base NEON, so an Armv8.0-A
binary must contain none. This reads the section table and scans aligned
32-bit words directly, so it needs no cross disassembler; runners that can
disassemble aarch64 are not guaranteed.
"""

import struct
import sys

# PMULL (0x0EE0E000) and PMULL2 (0x4EE0E000), with the Rm, Rn, and Rd operand
# fields masked out.
OPERAND_MASK = 0xFFE0FC00
ENCODINGS = (0x0EE0E000, 0x4EE0E000)


def text_section(data: bytes) -> bytes:
    (e_shoff,) = struct.unpack_from("<Q", data, 0x28)
    (e_shentsize,) = struct.unpack_from("<H", data, 0x3A)
    (e_shnum,) = struct.unpack_from("<H", data, 0x3C)
    (e_shstrndx,) = struct.unpack_from("<H", data, 0x3E)

    def header(index: int) -> bytes:
        start = e_shoff + index * e_shentsize
        return data[start : start + e_shentsize]

    (names_offset,) = struct.unpack_from("<Q", header(e_shstrndx), 0x18)
    for index in range(e_shnum):
        section = header(index)
        (name_offset,) = struct.unpack_from("<I", section, 0)
        start = names_offset + name_offset
        name = data[start : data.index(b"\0", start)].decode()
        if name == ".text":
            (offset,) = struct.unpack_from("<Q", section, 0x18)
            (size,) = struct.unpack_from("<Q", section, 0x20)
            return data[offset : offset + size]
    raise SystemExit("no .text section found")


def main() -> None:
    with open(sys.argv[1], "rb") as handle:
        text = text_section(handle.read())
    if not text:
        raise SystemExit("empty .text section; the scan would pass vacuously")
    count = sum(
        1
        for offset in range(0, len(text) - 3, 4)
        if int.from_bytes(text[offset : offset + 4], "little") & OPERAND_MASK in ENCODINGS
    )
    print(count)


if __name__ == "__main__":
    main()
