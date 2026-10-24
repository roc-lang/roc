//! Locks the runtime representation constants that the gdb and lldb scripts in
//! `debugger/` mirror in Python. Each script carries a constants block that must
//! equal `python_block`, which is rendered from the canonical definitions in
//! `src/builtins`, so a representation change there fails this test until the
//! scripts follow.

const std = @import("std");
const builtins = @import("builtins");

const RocStr = builtins.str.RocStr;
const RocDec = builtins.dec.RocDec;

comptime {
    std.debug.assert(RocDec.one_point_zero_i128 == std.math.pow(i128, 10, RocDec.decimal_places));
}

/// The constants block both debugger scripts must contain verbatim.
pub const python_block = std.fmt.comptimePrint(
    \\# Runtime representation constants. A test in
    \\# src/backend/llvm/debugger_constants.zig renders this block from the
    \\# canonical definitions in src/builtins and requires it verbatim.
    \\DEC_FRACTION_DIGITS = {d}
    \\DEC_ONE = 10**DEC_FRACTION_DIGITS
    \\STR_WORD_COUNT = {d}
    \\SMALL_STR_FLAG = 0x{X:0>2}
    \\SMALL_STR_LEN_MASK = 0x{X:0>2}
    \\
, .{
    RocDec.decimal_places,
    RocStr.word_count,
    RocStr.small_str_flag,
    RocStr.small_str_len_mask,
});

test "gdb script representation constants match builtins" {
    try std.testing.expect(std.mem.find(u8, @embedFile("debugger/roc_gdb.py"), python_block) != null);
}

test "lldb script representation constants match builtins" {
    try std.testing.expect(std.mem.find(u8, @embedFile("debugger/roc_lldb.py"), python_block) != null);
}
