# Pretty-printers for Roc values in gdb.
#
# `roc build` inlines this script into every binary's .debug_gdb_scripts
# section, so gdb auto-loads the version that matches the compiler that
# built the binary (subject to gdb's one-time auto-load opt-in, e.g.
# `set auto-load python-scripts on`). It can also be loaded manually with
# `source roc_gdb.py`.
#
# The type names and layouts here must match the DWARF emitted by
# MonoLlvmCodeGen.zig and the runtime representations in
# src/builtins/str.zig and src/builtins/list.zig.

import gdb
import gdb.printing

DEC_ONE = 10**18
SEAMLESS_SLICE_TAG = 1


def _type_name(val):
    ty = val.type.strip_typedefs()
    return ty.name or ty.tag


def _word_bytes(val):
    return val["length"].type.sizeof


class RocStrPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        word = _word_bytes(self.val)
        length = int(self.val["length"])
        small_bit = 1 << (word * 8 - 1)
        if length & small_bit:
            # Small string: all three words are inline UTF-8; the final
            # byte holds the length with its high bit set.
            size = word * 3
            raw = bytes(self.val.bytes)[:size]
            n = raw[size - 1] & 0x7F
            return '"' + raw[:n].decode("utf-8", "replace") + '"'
        if length == 0:
            return '""'
        ptr = int(self.val["bytes"])
        if ptr == 0:
            return '""'
        data = gdb.selected_inferior().read_memory(ptr, length).tobytes()
        return '"' + data.decode("utf-8", "replace") + '"'

    def display_hint(self):
        return "string"


class RocListPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "List(len=%d)" % int(self.val["length"])

    def children(self):
        length = int(self.val["length"])
        ptr = self.val["bytes"]
        if int(ptr) == 0:
            return
        elem = ptr.cast(ptr.type.target().pointer())
        for i in range(length):
            yield ("[%d]" % i, elem[i])

    def display_hint(self):
        return "array"


class RocDecPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        v = int(self.val)
        sign = "-" if v < 0 else ""
        whole, frac = divmod(abs(v), DEC_ONE)
        digits = ("%018d" % frac).rstrip("0")
        if digits:
            return "%s%d.%s" % (sign, whole, digits)
        return "%s%d" % (sign, whole)


def _lookup(val):
    name = _type_name(val)
    if name == "Str":
        return RocStrPrinter(val)
    if name == "List":
        return RocListPrinter(val)
    if name == "Dec":
        return RocDecPrinter(val)
    return None


def register(objfile):
    (objfile or gdb).pretty_printers.append(_lookup)


register(gdb.current_objfile())
