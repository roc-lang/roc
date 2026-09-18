//! Patching aarch64 instruction immediates for relocations.
//!
//! Adapted from the Zig compiler's `src/link/aarch64.zig` at
//! https://codeberg.org/ziglang/zig, licensed under the MIT license. Thanks,
//! Zig team! The bit layouts are the ones in the Arm Architecture Reference
//! Manual; this file writes them directly instead of going through Zig's
//! instruction encoder.

const std = @import("std");

fn readInst(code: *const [4]u8) u32 {
    return std.mem.readInt(u32, code, .little);
}

fn writeInst(code: *[4]u8, inst: u32) void {
    std.mem.writeInt(u32, code, inst, .little);
}

/// The 12-bit immediate of an ADD/SUB (immediate) instruction, bits 21..10.
pub fn writeAddImm(code: *[4]u8, value: u12) void {
    const inst = readInst(code);
    writeInst(code, (inst & ~(@as(u32, 0xfff) << 10)) | (@as(u32, value) << 10));
}

/// The 12-bit scaled offset of a load/store (unsigned immediate), bits 21..10.
pub fn writeLoadStoreImm(code: *[4]u8, value: u12) void {
    writeAddImm(code, value);
}

/// The 21-bit immediate of ADR/ADRP: immlo in bits 30..29, immhi in bits 23..5.
pub fn writeAdrImm(code: *[4]u8, value: i21) void {
    const bits: u21 = @bitCast(value);
    const immlo: u32 = bits & 0x3;
    const immhi: u32 = (bits >> 2) & 0x7ffff;
    const inst = readInst(code);
    const cleared = inst & ~((@as(u32, 0x3) << 29) | (@as(u32, 0x7ffff) << 5));
    writeInst(code, cleared | (immlo << 29) | (immhi << 5));
}

/// The 26-bit displacement of B/BL, in words, bits 25..0.
pub fn writeBranchImm(code: *[4]u8, disp: i28) void {
    const words: i26 = @intCast(@shrExact(disp, 2));
    const bits: u26 = @bitCast(words);
    const inst = readInst(code);
    writeInst(code, (inst & ~@as(u32, 0x3ffffff)) | @as(u32, bits));
}

/// The 19-bit displacement of a conditional branch or compare-and-branch, in
/// words, bits 23..5.
pub fn writeCondBranchImm(code: *[4]u8, disp: i21) void {
    const words: i19 = @intCast(@shrExact(disp, 2));
    const bits: u19 = @bitCast(words);
    const inst = readInst(code);
    writeInst(code, (inst & ~(@as(u32, 0x7ffff) << 5)) | (@as(u32, bits) << 5));
}

/// The displacement in pages between the instruction at `source` and
/// `target`, as ADRP encodes it.
pub fn calcNumberOfPages(source: i64, target: i64) error{Overflow}!i21 {
    return std.math.cast(i21, (target >> 12) - (source >> 12)) orelse error.Overflow;
}

/// Read back an ADD/SUB immediate, for formats that keep the addend in the
/// instruction.
pub fn readAddImm(code: *const [4]u8) u12 {
    return @truncate(readInst(code) >> 10);
}

/// Read back an ADR/ADRP immediate (in pages for ADRP).
pub fn readAdrImm(code: *const [4]u8) i21 {
    const inst = readInst(code);
    const immlo: u21 = @truncate((inst >> 29) & 0x3);
    const immhi: u21 = @truncate((inst >> 5) & 0x7ffff);
    return @bitCast((immhi << 2) | immlo);
}

/// Read back a B/BL displacement in bytes.
pub fn readBranchImm(code: *const [4]u8) i28 {
    const bits: u26 = @truncate(readInst(code) & 0x3ffffff);
    const words: i26 = @bitCast(bits);
    return @as(i28, words) << 2;
}

/// Whether the instruction is an ADD/SUB (immediate), as opposed to a
/// load/store (unsigned immediate), which shares a relocation kind with it.
pub fn isAddSubImmediate(code: *const [4]u8) bool {
    return (readInst(code) >> 23) & 0x3f == 0b100010;
}

/// The log2 of the access size of a load/store (unsigned immediate), which
/// scales its 12-bit offset.
pub fn loadStoreScale(code: *const [4]u8) u4 {
    const inst = readInst(code);
    const size: u4 = @intCast(inst >> 30);
    const is_vector = (inst >> 26) & 1 == 1;
    const opc_high = (inst >> 23) & 1 == 1;
    if (is_vector and opc_high and size == 0) return 4;
    return size;
}

/// Encode a page offset into a load/store or ADD, honoring the access size.
pub fn writePageOffset(code: *[4]u8, target: u64) error{UnexpectedRemainder}!void {
    const low: u12 = @truncate(target);
    if (isAddSubImmediate(code)) {
        writeAddImm(code, low);
        return;
    }
    const scale = loadStoreScale(code);
    const unit: u12 = @as(u12, 1) << scale;
    if (low % unit != 0) return error.UnexpectedRemainder;
    writeLoadStoreImm(code, low / unit);
}

test "adrp immediate round-trips" {
    var code = [4]u8{ 0x10, 0x00, 0x00, 0x90 };
    writeAdrImm(&code, -5);
    try std.testing.expectEqual(@as(i21, -5), readAdrImm(&code));
    writeAdrImm(&code, 0x7ffff);
    try std.testing.expectEqual(@as(i21, 0x7ffff), readAdrImm(&code));
}

test "branch displacement round-trips" {
    var code = [4]u8{ 0x00, 0x00, 0x00, 0x94 };
    writeBranchImm(&code, -0x1000);
    try std.testing.expectEqual(@as(i28, -0x1000), readBranchImm(&code));
    try std.testing.expectEqual(@as(u32, 0x94000000), readInst(&code) & 0xfc000000);
}

test "load/store scale follows the access size" {
    // ldr x0, [x1] ; size=11 -> scale 3
    try std.testing.expectEqual(@as(u4, 3), loadStoreScale(&[4]u8{ 0x20, 0x00, 0x40, 0xf9 }));
    // ldr w0, [x1] ; size=10 -> scale 2
    try std.testing.expectEqual(@as(u4, 2), loadStoreScale(&[4]u8{ 0x20, 0x00, 0x40, 0xb9 }));
    // ldr q0, [x1] ; size=00, V=1, opc=11 -> scale 4
    try std.testing.expectEqual(@as(u4, 4), loadStoreScale(&[4]u8{ 0x20, 0x00, 0xc0, 0x3d }));
    // add x0, x1, #0 is not a load/store
    try std.testing.expect(isAddSubImmediate(&[4]u8{ 0x20, 0x00, 0x00, 0x91 }));
}
