#!/usr/bin/env python3
"""Count AArch64 instruction classes within one ELF function symbol.

Decode fixed opcode fields so codegen checks need no cross disassembler.
Usage: count_aarch64_instructions.py <binary> <symbol> <class>
"""

import struct
import sys

CLASSES = {
    "umov-byte": (0xFFE1FC00, (0x0E013C00, 0x4E013C00)),
    "extr": (0xFFE00000, (0x13800000, 0x93C00000)),
    "tbl": (0xFFE09C00, (0x0E000000, 0x4E000000)),
    "ext": (0xFFE08400, (0x6E000000,)),
}


def symbol_body(data: bytes, wanted: str) -> bytes:
    if data[:6] != b"\x7fELF\x02\x01" or struct.unpack_from("<H", data, 18)[0] != 183:
        raise SystemExit("expected a little-endian ELF64 AArch64 binary")
    (offset,) = struct.unpack_from("<Q", data, 0x28)
    entry_size, count = struct.unpack_from("<HH", data, 0x3A)
    sections = [struct.unpack_from("<IIQQQQIIQQ", data, offset + i * entry_size) for i in range(count)]
    for section in sections:
        if section[1] != 2:  # SHT_SYMTAB
            continue
        names = sections[section[6]][4]
        for entry in range(section[4], section[4] + section[5], section[9]):
            name, _, _, index, address, size = struct.unpack_from("<IBBHQQ", data, entry)
            start = names + name
            if data[start : data.index(b"\0", start)].decode() != wanted:
                continue
            if size == 0 or size % 4:
                raise SystemExit(f"empty or unaligned function: {wanted}")
            body_section = sections[index]
            start = body_section[4] + address - body_section[3]
            return data[start : start + size]
    raise SystemExit(f"no symbol named {wanted}")


def main() -> None:
    binary, symbol, class_name = sys.argv[1:]
    with open(binary, "rb") as handle:
        body = symbol_body(handle.read(), symbol)
    mask, encodings = CLASSES[class_name]
    print(sum(word & mask in encodings for (word,) in struct.iter_unpack("<I", body)))


if __name__ == "__main__":
    main()
