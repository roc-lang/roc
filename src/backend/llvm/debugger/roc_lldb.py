# Summary providers for Roc values in lldb.
#
# Load with `command script import /path/to/roc_lldb.py` (one line in
# ~/.lldbinit), or via an editor debug configuration's initCommands.
#
# The type names and layouts here must match the DWARF emitted by
# MonoLlvmCodeGen.zig and the runtime representations in
# src/builtins/str.zig and src/builtins/list.zig.

import lldb

DEC_ONE = 10**18


def _word_bytes(valobj):
    return valobj.GetTarget().GetAddressByteSize()


def roc_str_summary(valobj, _internal_dict):
    err = lldb.SBError()
    word = _word_bytes(valobj)
    length = valobj.GetChildMemberWithName("length").GetValueAsUnsigned()
    small_bit = 1 << (word * 8 - 1)
    if length & small_bit:
        # Small string: all three words are inline UTF-8; the final byte
        # holds the length with its high bit set.
        size = word * 3
        data = valobj.GetData()
        raw = bytearray(data.ReadRawData(err, 0, size))
        if err.Fail():
            return "<unreadable Str>"
        n = raw[size - 1] & 0x7F
        return '"' + bytes(raw[:n]).decode("utf-8", "replace") + '"'
    if length == 0:
        return '""'
    ptr = valobj.GetChildMemberWithName("bytes").GetValueAsUnsigned()
    if ptr == 0:
        return '""'
    process = valobj.GetProcess()
    data = process.ReadMemory(ptr, length, err)
    if err.Fail():
        return "<unreadable Str>"
    return '"' + data.decode("utf-8", "replace") + '"'


def roc_list_summary(valobj, _internal_dict):
    length = valobj.GetChildMemberWithName("length").GetValueAsUnsigned()
    return "List(len=%d)" % length


def roc_dec_summary(valobj, _internal_dict):
    err = lldb.SBError()
    data = valobj.GetData()
    raw = bytearray(data.ReadRawData(err, 0, 16))
    if err.Fail():
        return "<unreadable Dec>"
    v = int.from_bytes(bytes(raw), "little", signed=True)
    sign = "-" if v < 0 else ""
    whole, frac = divmod(abs(v), DEC_ONE)
    digits = ("%018d" % frac).rstrip("0")
    if digits:
        return "%s%d.%s" % (sign, whole, digits)
    return "%s%d" % (sign, whole)


def __lldb_init_module(debugger, _internal_dict):
    debugger.HandleCommand(
        'type summary add -w roc -F roc_lldb.roc_str_summary Str'
    )
    debugger.HandleCommand(
        'type summary add -w roc -F roc_lldb.roc_list_summary List'
    )
    debugger.HandleCommand(
        'type summary add -w roc -F roc_lldb.roc_dec_summary Dec'
    )
    debugger.HandleCommand("type category enable roc")
