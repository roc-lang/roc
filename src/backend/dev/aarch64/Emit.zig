//! aarch64 (ARM64) instruction encoding.
//!
//! This module provides low-level aarch64 instruction encoding for the dev backend.
//! It generates machine code bytes that can be executed directly on aarch64 processors.
//!
//! Note: All aarch64 instructions are 32 bits (4 bytes) and must be 4-byte aligned.

const std = @import("std");
const Registers = @import("Registers.zig");
const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;
const RegisterWidth = Registers.RegisterWidth;

const Relocation = @import("../Relocation.zig").Relocation;

/// aarch64 instruction emitter for generating machine code.
const Emit = @This();

allocator: std.mem.Allocator,
buf: std.ArrayList(u8),
relocs: std.ArrayList(Relocation),

pub fn init(allocator: std.mem.Allocator) Emit {
    return .{
        .allocator = allocator,
        .buf = .{},
        .relocs = .{},
    };
}

pub fn deinit(self: *Emit) void {
    self.buf.deinit(self.allocator);
    self.relocs.deinit(self.allocator);
}

/// Get the current code offset
pub fn codeOffset(self: *const Emit) u64 {
    return @intCast(self.buf.items.len);
}

/// Emit a 32-bit instruction (little-endian)
fn emit32(self: *Emit, inst: u32) !void {
    try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(inst)));
}

// Movement instructions

/// MOV reg, reg (register to register)
/// For normal registers: ORR Xd, XZR, Xm
/// For SP as source: ADD Xd, SP, #0 (since ORR treats reg 31 as XZR, not SP)
pub fn movRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
    // Special case: when source is SP, use ADD Xd, SP, #0
    // because ORR treats register 31 as XZR, not SP
    if (src == .ZRSP) {
        // ADD <Xd>, SP, #0
        // ARM encoding: sf 0 0 1 0 0 0 1 shift(2) imm12(12) Rn(5) Rd(5)
        // For ADD immediate with SP, Rn=31 is interpreted as SP
        const sf = width.sf();
        const inst: u32 = (@as(u32, sf) << 31) |
            (0b0010001 << 24) | // ADD immediate opcode
            (0b00 << 22) | // shift = 0
            (0 << 10) | // imm12 = 0
            (@as(u32, src.enc()) << 5) | // Rn = SP (31)
            dst.enc(); // Rd
        try self.emit32(inst);
    } else {
        // ORR <Xd>, XZR, <Xm>
        // 31 30 29 28 27 26 25 24 23 22 21 20    16 15      10 9    5 4    0
        // sf  0  1  0  1  0  1  0  0  0  0  Rm[4:0]  0  0  0  0  0  0  Rn[4:0]  Rd[4:0]
        const sf = width.sf();
        const inst: u32 = (@as(u32, sf) << 31) |
            (0b0101010 << 24) |
            (0b000 << 21) |
            (@as(u32, src.enc()) << 16) |
            (0b000000 << 10) |
            (@as(u32, GeneralReg.ZRSP.enc()) << 5) | // XZR as Rn
            dst.enc();
        try self.emit32(inst);
    }
}

/// MOVZ - Move with zero (load immediate, clearing other bits)
/// Used for loading 16-bit immediates at specified position
pub fn movz(self: *Emit, width: RegisterWidth, dst: GeneralReg, imm: u16, shift: u6) !void {
    // MOVZ <Xd>, #<imm16>, LSL #<shift>
    // 31 30 29 28 27 26 25 24 23 22 21 20               5 4    0
    // sf  1  0  1  0  0  1  0  1  hw[1:0]  imm16[15:0]  Rd[4:0]
    const sf = width.sf();
    const hw: u2 = @truncate(shift >> 4); // shift / 16
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b10100101 << 23) |
        (@as(u32, hw) << 21) |
        (@as(u32, imm) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// MOVK - Move with keep (load immediate, keeping other bits)
/// Used for loading 16-bit immediates into specific position
pub fn movk(self: *Emit, width: RegisterWidth, dst: GeneralReg, imm: u16, shift: u6) !void {
    // MOVK <Xd>, #<imm16>, LSL #<shift>
    // 31 30 29 28 27 26 25 24 23 22 21 20               5 4    0
    // sf  1  1  1  0  0  1  0  1  hw[1:0]  imm16[15:0]  Rd[4:0]
    const sf = width.sf();
    const hw: u2 = @truncate(shift >> 4);
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b11100101 << 23) |
        (@as(u32, hw) << 21) |
        (@as(u32, imm) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// Load a 64-bit immediate into a register
/// Uses MOVZ + MOVK sequence as needed
pub fn movRegImm64(self: *Emit, dst: GeneralReg, imm: u64) !void {
    // For 64-bit immediates, we need up to 4 MOVZ/MOVK instructions
    const imm16_0: u16 = @truncate(imm);
    const imm16_1: u16 = @truncate(imm >> 16);
    const imm16_2: u16 = @truncate(imm >> 32);
    const imm16_3: u16 = @truncate(imm >> 48);

    // Find first non-zero chunk for MOVZ
    if (imm16_0 != 0 or (imm16_1 == 0 and imm16_2 == 0 and imm16_3 == 0)) {
        try self.movz(.w64, dst, imm16_0, 0);
        if (imm16_1 != 0) try self.movk(.w64, dst, imm16_1, 16);
        if (imm16_2 != 0) try self.movk(.w64, dst, imm16_2, 32);
        if (imm16_3 != 0) try self.movk(.w64, dst, imm16_3, 48);
    } else if (imm16_1 != 0) {
        try self.movz(.w64, dst, imm16_1, 16);
        if (imm16_2 != 0) try self.movk(.w64, dst, imm16_2, 32);
        if (imm16_3 != 0) try self.movk(.w64, dst, imm16_3, 48);
    } else if (imm16_2 != 0) {
        try self.movz(.w64, dst, imm16_2, 32);
        if (imm16_3 != 0) try self.movk(.w64, dst, imm16_3, 48);
    } else {
        try self.movz(.w64, dst, imm16_3, 48);
    }
}

// Arithmetic instructions

/// ADD reg, reg, reg
pub fn addRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // ADD <Xd>, <Xn>, <Xm>
    // 31 30 29 28 27 26 25 24 23 22 21 20    16 15      10 9    5 4    0
    // sf  0  0  0  1  0  1  1  shift  0  Rm[4:0]  imm6[5:0]  Rn[4:0]  Rd[4:0]
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0001011 << 24) |
        (0b00 << 22) | // shift = LSL
        (0 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000000 << 10) | // imm6 = 0
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// SUB reg, reg, reg
pub fn subRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // SUB <Xd>, <Xn>, <Xm>
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b1001011 << 24) | // Different from ADD
        (0b00 << 22) |
        (0 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000000 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// MUL reg, reg, reg (alias for MADD with XZR as addend)
pub fn mulRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // MADD <Xd>, <Xn>, <Xm>, XZR
    // 31 30 29 28 27 26 25 24 23 21 20    16 15 14    10 9    5 4    0
    // sf  0  0  1  1  0  1  1  0  0  0  Rm[4:0]  0  Ra[4:0]  Rn[4:0]  Rd[4:0]
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0011011000 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0 << 15) |
        (@as(u32, GeneralReg.ZRSP.enc()) << 10) | // XZR as Ra
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// SDIV reg, reg, reg (signed divide)
pub fn sdivRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // SDIV <Xd>, <Xn>, <Xm>
    // sf 0 0 1 1 0 1 0 1 1 0 Rm 0 0 0 0 1 1 Rn Rd
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0011010110 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000011 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// UDIV reg, reg, reg (unsigned divide)
pub fn udivRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // UDIV <Xd>, <Xn>, <Xm>
    // sf 0 0 1 1 0 1 0 1 1 0 Rm 0 0 0 0 1 0 Rn Rd
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0011010110 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000010 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// MSUB reg, reg, reg, reg (multiply-subtract: dst = ra - rn * rm)
pub fn msubRegRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, rn: GeneralReg, rm: GeneralReg, ra: GeneralReg) !void {
    // MSUB <Xd>, <Xn>, <Xm>, <Xa>
    // sf 0 0 1 1 0 1 1 0 0 0 Rm 1 Ra Rn Rd
    // Result: Xa - Xn * Xm
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0011011000 << 21) |
        (@as(u32, rm.enc()) << 16) |
        (1 << 15) | // o0=1 for MSUB (vs o0=0 for MADD)
        (@as(u32, ra.enc()) << 10) |
        (@as(u32, rn.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// ADD reg, reg, imm12 (add immediate)
pub fn addRegRegImm12(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: u12) !void {
    // ADD <Xd>, <Xn>, #<imm>{, <shift>}
    // sf 0 0 1 0 0 0 1 sh imm12 Rn Rd
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0010001 << 24) |
        (0 << 22) | // sh=0 (no shift)
        (@as(u32, imm) << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// SUB reg, reg, imm12 (subtract immediate)
pub fn subRegRegImm12(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: u12) !void {
    // SUB <Xd>, <Xn>, #<imm>{, <shift>}
    // sf 1 0 1 0 0 0 1 sh imm12 Rn Rd
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b1010001 << 24) |
        (0 << 22) |
        (@as(u32, imm) << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// NEG reg, reg (negate - alias for SUB from XZR)
pub fn negRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
    try self.subRegRegReg(width, dst, .ZRSP, src);
}

/// CMP reg, reg (compare - alias for SUBS with XZR destination)
pub fn cmpRegReg(self: *Emit, width: RegisterWidth, lhs: GeneralReg, rhs: GeneralReg) !void {
    // SUBS XZR, Xn, Xm
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b1101011 << 24) |
        (0b00 << 22) |
        (0 << 21) |
        (@as(u32, rhs.enc()) << 16) |
        (0b000000 << 10) |
        (@as(u32, lhs.enc()) << 5) |
        GeneralReg.ZRSP.enc();
    try self.emit32(inst);
}

/// CMP reg, imm12 (compare immediate - alias for SUBS with XZR destination)
pub fn cmpRegImm12(self: *Emit, width: RegisterWidth, reg: GeneralReg, imm: u12) !void {
    // SUBS XZR, Xn, #imm
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b1110001 << 24) |
        (0 << 22) |
        (@as(u32, imm) << 10) |
        (@as(u32, reg.enc()) << 5) |
        GeneralReg.ZRSP.enc();
    try self.emit32(inst);
}

/// AND reg, reg, reg
pub fn andRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // AND <Xd>, <Xn>, <Xm>
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0001010 << 24) |
        (0b00 << 22) |
        (0 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000000 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// ORR reg, reg, reg
pub fn orrRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // ORR <Xd>, <Xn>, <Xm>
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0101010 << 24) |
        (0b00 << 22) |
        (0 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000000 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// EOR reg, reg, reg (exclusive OR)
pub fn eorRegRegReg(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
    // EOR <Xd>, <Xn>, <Xm>
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b1001010 << 24) |
        (0b00 << 22) |
        (0 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b000000 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// LSL reg, reg, imm (logical shift left by immediate)
pub fn lslRegRegImm(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, shift: u6) !void {
    // UBFM with special encoding for LSL
    // LSL <Xd>, <Xn>, #shift is UBFM <Xd>, <Xn>, #(-shift MOD 64), #(63-shift)
    const sf = width.sf();
    const reg_size: u6 = if (width == .w64) 63 else 31;
    const immr: u6 = @truncate((@as(u7, reg_size) + 1 - shift) & reg_size);
    const imms: u6 = reg_size - shift;
    const n: u1 = sf;
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b10 << 29) |
        (0b100110 << 23) |
        (@as(u32, n) << 22) |
        (@as(u32, immr) << 16) |
        (@as(u32, imms) << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// LSR reg, reg, imm (logical shift right by immediate)
pub fn lsrRegRegImm(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, shift: u6) !void {
    // UBFM <Xd>, <Xn>, #shift, #63 (or #31 for 32-bit)
    const sf = width.sf();
    const reg_size: u6 = if (width == .w64) 63 else 31;
    const n: u1 = sf;
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b10 << 29) |
        (0b100110 << 23) |
        (@as(u32, n) << 22) |
        (@as(u32, shift) << 16) |
        (@as(u32, reg_size) << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// ASR reg, reg, imm (arithmetic shift right by immediate)
pub fn asrRegRegImm(self: *Emit, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, shift: u6) !void {
    // SBFM <Xd>, <Xn>, #shift, #63 (or #31 for 32-bit)
    const sf = width.sf();
    const reg_size: u6 = if (width == .w64) 63 else 31;
    const n: u1 = sf;
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b00 << 29) |
        (0b100110 << 23) |
        (@as(u32, n) << 22) |
        (@as(u32, shift) << 16) |
        (@as(u32, reg_size) << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

// Control flow instructions

/// RET (return to address in LR)
pub fn ret(self: *Emit) !void {
    // RET {<Xn>} defaults to X30 (LR)
    // 1101011 0010 11111 000000 Rn[4:0] 00000
    const inst: u32 = (0b1101011 << 25) |
        (0b0010 << 21) |
        (0b11111 << 16) |
        (0b000000 << 10) |
        (@as(u32, GeneralReg.LR.enc()) << 5) |
        0b00000;
    try self.emit32(inst);
}

/// BL (branch with link - function call)
pub fn bl(self: *Emit, offset_bytes: i32) !void {
    // BL <label>
    // 1 00101 imm26
    // Note: offset must be multiple of 4 for valid instruction alignment
    // All ARM64 instructions are 4 bytes, so offsets should always be aligned
    std.debug.assert(@mod(offset_bytes, 4) == 0);
    const offset_words: i26 = @intCast(@divExact(offset_bytes, 4));
    const imm26: u26 = @bitCast(offset_words);
    const inst: u32 = (@as(u32, 0b100101) << 26) | imm26;
    try self.emit32(inst);
}

/// B (unconditional branch)
pub fn b(self: *Emit, offset_bytes: i32) !void {
    // B <label>
    // 0 00101 imm26
    const offset_words = @divExact(offset_bytes, 4);
    const imm26: u26 = @bitCast(@as(i26, @truncate(offset_words)));
    const inst: u32 = (@as(u32, 0b000101) << 26) | imm26;
    try self.emit32(inst);
}

/// Condition codes for conditional branches
pub const Condition = enum(u4) {
    eq = 0b0000, // Equal (Z=1)
    ne = 0b0001, // Not equal (Z=0)
    cs = 0b0010, // Carry set / unsigned >= (C=1)
    cc = 0b0011, // Carry clear / unsigned < (C=0)
    mi = 0b0100, // Minus / negative (N=1)
    pl = 0b0101, // Plus / positive or zero (N=0)
    vs = 0b0110, // Overflow (V=1)
    vc = 0b0111, // No overflow (V=0)
    hi = 0b1000, // Unsigned > (C=1 and Z=0)
    ls = 0b1001, // Unsigned <= (C=0 or Z=1)
    ge = 0b1010, // Signed >= (N=V)
    lt = 0b1011, // Signed < (N!=V)
    gt = 0b1100, // Signed > (Z=0 and N=V)
    le = 0b1101, // Signed <= (Z=1 or N!=V)
    al = 0b1110, // Always
    nv = 0b1111, // Never (reserved)

    pub fn invert(self: Condition) Condition {
        return @enumFromInt(@intFromEnum(self) ^ 1);
    }
};

/// B.cond (conditional branch)
pub fn bcond(self: *Emit, cond: Condition, offset_bytes: i32) !void {
    // B.cond <label>
    // 0101010 0 imm19 0 cond
    const offset_words = @divExact(offset_bytes, 4);
    const imm19: u19 = @bitCast(@as(i19, @truncate(offset_words)));
    const inst: u32 = (0b01010100 << 24) |
        (@as(u32, imm19) << 5) |
        (0 << 4) |
        @intFromEnum(cond);
    try self.emit32(inst);
}

/// CBZ (compare and branch if zero)
pub fn cbz(self: *Emit, width: RegisterWidth, reg: GeneralReg, offset_bytes: i32) !void {
    // CBZ <Xt>, <label>
    // sf 011010 0 imm19 Rt
    const sf = width.sf();
    const offset_words = @divExact(offset_bytes, 4);
    const imm19: u19 = @bitCast(@as(i19, @truncate(offset_words)));
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0110100 << 24) |
        (@as(u32, imm19) << 5) |
        reg.enc();
    try self.emit32(inst);
}

/// CBNZ (compare and branch if non-zero)
pub fn cbnz(self: *Emit, width: RegisterWidth, reg: GeneralReg, offset_bytes: i32) !void {
    // CBNZ <Xt>, <label>
    // sf 011010 1 imm19 Rt
    const sf = width.sf();
    const offset_words = @divExact(offset_bytes, 4);
    const imm19: u19 = @bitCast(@as(i19, @truncate(offset_words)));
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0110101 << 24) |
        (@as(u32, imm19) << 5) |
        reg.enc();
    try self.emit32(inst);
}

/// CSEL (conditional select)
pub fn csel(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg, cond: Condition) !void {
    // CSEL <Xd>, <Xn>, <Xm>, <cond>
    // sf 0 0 11010100 Rm cond 0 0 Rn Rd
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0011010100 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (@as(u32, @intFromEnum(cond)) << 12) |
        (0b00 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// CSINC (conditional select increment - used for CSET)
pub fn csinc(self: *Emit, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg, cond: Condition) !void {
    // CSINC <Xd>, <Xn>, <Xm>, <cond>
    // sf 0 0 11010100 Rm cond 0 1 Rn Rd
    const sf = width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b0011010100 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (@as(u32, @intFromEnum(cond)) << 12) |
        (0b01 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// CSET (conditional set - alias for CSINC with XZR)
pub fn cset(self: *Emit, width: RegisterWidth, dst: GeneralReg, cond: Condition) !void {
    // CSET <Xd>, <cond> is CSINC <Xd>, XZR, XZR, invert(cond)
    try self.csinc(width, dst, .ZRSP, .ZRSP, cond.invert());
}

// Memory instructions

/// LDR (load register) with unsigned offset
pub fn ldrRegMemUoff(self: *Emit, width: RegisterWidth, dst: GeneralReg, base: GeneralReg, uoffset: u12) !void {
    // LDR <Xt>, [<Xn|SP>, #<pimm>]
    // 1x 111 0 01 01 imm12 Rn Rt (64-bit)
    // 0x 111 0 01 01 imm12 Rn Rt (32-bit)
    const size: u2 = if (width == .w64) 0b11 else 0b10;
    const inst: u32 = (@as(u32, size) << 30) |
        (0b111001 << 24) |
        (0b01 << 22) |
        (@as(u32, uoffset) << 10) |
        (@as(u32, base.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// STR (store register) with unsigned offset
pub fn strRegMemUoff(self: *Emit, width: RegisterWidth, src: GeneralReg, base: GeneralReg, uoffset: u12) !void {
    // STR <Xt>, [<Xn|SP>, #<pimm>]
    // 1x 111 0 01 00 imm12 Rn Rt
    const size: u2 = if (width == .w64) 0b11 else 0b10;
    const inst: u32 = (@as(u32, size) << 30) |
        (0b111001 << 24) |
        (0b00 << 22) |
        (@as(u32, uoffset) << 10) |
        (@as(u32, base.enc()) << 5) |
        src.enc();
    try self.emit32(inst);
}

/// LDUR (load register unscaled) with signed offset
pub fn ldurRegMem(self: *Emit, width: RegisterWidth, dst: GeneralReg, base: GeneralReg, offset: i9) !void {
    // LDUR <Xt>, [<Xn|SP>, #<simm>]
    // 1x 111 0 00 01 0 imm9 00 Rn Rt
    const size: u2 = if (width == .w64) 0b11 else 0b10;
    const imm9: u9 = @bitCast(offset);
    const inst: u32 = (@as(u32, size) << 30) |
        (0b111000 << 24) |
        (0b01 << 22) |
        (0 << 21) |
        (@as(u32, imm9) << 12) |
        (0b00 << 10) |
        (@as(u32, base.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// STUR (store register unscaled) with signed offset
pub fn sturRegMem(self: *Emit, width: RegisterWidth, src: GeneralReg, base: GeneralReg, offset: i9) !void {
    // STUR <Xt>, [<Xn|SP>, #<simm>]
    // 1x 111 0 00 00 0 imm9 00 Rn Rt
    const size: u2 = if (width == .w64) 0b11 else 0b10;
    const imm9: u9 = @bitCast(offset);
    const inst: u32 = (@as(u32, size) << 30) |
        (0b111000 << 24) |
        (0b00 << 22) |
        (0 << 21) |
        (@as(u32, imm9) << 12) |
        (0b00 << 10) |
        (@as(u32, base.enc()) << 5) |
        src.enc();
    try self.emit32(inst);
}

/// LDRB (load register byte, zero-extend)
pub fn ldrbRegMem(self: *Emit, dst: GeneralReg, base: GeneralReg, uoffset: u12) !void {
    // LDRB <Wt>, [<Xn|SP>, #<pimm>]
    // 00 111 0 01 01 imm12 Rn Rt
    const inst: u32 = (0b00 << 30) |
        (0b111001 << 24) |
        (0b01 << 22) |
        (@as(u32, uoffset) << 10) |
        (@as(u32, base.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// STRB (store register byte)
pub fn strbRegMem(self: *Emit, src: GeneralReg, base: GeneralReg, uoffset: u12) !void {
    // STRB <Wt>, [<Xn|SP>, #<pimm>]
    // 00 111 0 01 00 imm12 Rn Rt
    const inst: u32 = (0b00 << 30) |
        (0b111001 << 24) |
        (0b00 << 22) |
        (@as(u32, uoffset) << 10) |
        (@as(u32, base.enc()) << 5) |
        src.enc();
    try self.emit32(inst);
}

// Stack instructions

/// STP (store pair) - commonly used for pushing to stack
pub fn stpPreIndex(self: *Emit, width: RegisterWidth, reg1: GeneralReg, reg2: GeneralReg, base: GeneralReg, imm_offset: i7) !void {
    // STP <Xt1>, <Xt2>, [<Xn|SP>, #<imm>]!
    // ARM encoding: opc 101 V 0110 imm7 Rt2 Rn Rt
    // bits 31-30: opc (10 for 64-bit, 00 for 32-bit)
    // bits 29-27: 101
    // bit 26: V (0 for scalar)
    // bits 25-22: 0110 (pre-index marker)
    // bits 21-15: imm7 (signed, scaled by register size)
    // bits 14-10: Rt2
    // bits 9-5: Rn (base register)
    // bits 4-0: Rt
    const opc: u2 = if (width == .w64) 0b10 else 0b00;
    const imm7: u7 = @bitCast(imm_offset);
    const inst: u32 = (@as(u32, opc) << 30) |
        (0b101 << 27) |
        (0b0 << 26) |
        (0b0110 << 22) | // Pre-index: bits 25-22 = 0110
        (@as(u32, imm7) << 15) |
        (@as(u32, reg2.enc()) << 10) |
        (@as(u32, base.enc()) << 5) |
        reg1.enc();
    try self.emit32(inst);
}

/// LDP (load pair) - commonly used for popping from stack
pub fn ldpPostIndex(self: *Emit, width: RegisterWidth, reg1: GeneralReg, reg2: GeneralReg, base: GeneralReg, imm_offset: i7) !void {
    // LDP <Xt1>, <Xt2>, [<Xn|SP>], #<imm>
    // ARM encoding: opc 101 V opc2 L imm7 Rt2 Rn Rt
    // bits 31-30: opc (10 for 64-bit, 00 for 32-bit)
    // bits 29-27: 101
    // bit 26: V (0 for scalar)
    // bits 25-23: 001 (post-index addressing mode)
    // bit 22: L (1 for load LDP, 0 for store STP)
    // bits 21-15: imm7 (signed, scaled by register size)
    // bits 14-10: Rt2
    // bits 9-5: Rn (base register)
    // bits 4-0: Rt
    const opc: u2 = if (width == .w64) 0b10 else 0b00;
    const imm7: u7 = @bitCast(imm_offset);
    const inst: u32 = (@as(u32, opc) << 30) |
        (0b101 << 27) |
        (0b0 << 26) |
        (0b001 << 23) | // Post-index mode: bits 25-23 = 001
        (0b1 << 22) | // L=1 for LDP (load)
        (@as(u32, imm7) << 15) |
        (@as(u32, reg2.enc()) << 10) |
        (@as(u32, base.enc()) << 5) |
        reg1.enc();
    try self.emit32(inst);
}

// Floating-point instructions

/// Float type for instruction encoding
pub const FloatType = enum(u1) {
    single = 0, // 32-bit float (S registers)
    double = 1, // 64-bit float (D registers)
};

/// FMOV (floating-point move register)
pub fn fmovRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
    // FMOV <Sd>, <Sn> or FMOV <Dd>, <Dn>
    // 0 0 0 11110 ftype 1 0000 00 10000 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b0000 << 17) |
        (0b00 << 15) |
        (0b10000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FMOV from general register to float register
pub fn fmovFloatFromGen(self: *Emit, ftype: FloatType, dst: FloatReg, src: GeneralReg) !void {
    // FMOV <Sd>, <Wn> (single) or FMOV <Dd>, <Xn> (double)
    const sf: u1 = @intFromEnum(ftype);
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b00 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b00 << 19) |
        (0b111 << 16) |
        (0b000000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FMOV from float register to general register
pub fn fmovGenFromFloat(self: *Emit, ftype: FloatType, dst: GeneralReg, src: FloatReg) !void {
    // FMOV <Wd>, <Sn> (single) or FMOV <Xd>, <Dn> (double)
    const sf: u1 = @intFromEnum(ftype);
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b00 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b00 << 19) |
        (0b110 << 16) |
        (0b000000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FADD (floating-point add)
pub fn faddRegRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
    // FADD <Sd>, <Sn>, <Sm> or FADD <Dd>, <Dn>, <Dm>
    // 0 0 0 11110 ftype 1 Rm 0010 10 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b0010 << 12) |
        (0b10 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FSUB (floating-point subtract)
pub fn fsubRegRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
    // FSUB <Sd>, <Sn>, <Sm> or FSUB <Dd>, <Dn>, <Dm>
    // 0 0 0 11110 ftype 1 Rm 0011 10 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b0011 << 12) |
        (0b10 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FMUL (floating-point multiply)
pub fn fmulRegRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
    // FMUL <Sd>, <Sn>, <Sm> or FMUL <Dd>, <Dn>, <Dm>
    // 0 0 0 11110 ftype 1 Rm 0000 10 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b0000 << 12) |
        (0b10 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FDIV (floating-point divide)
pub fn fdivRegRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
    // FDIV <Sd>, <Sn>, <Sm> or FDIV <Dd>, <Dn>, <Dm>
    // 0 0 0 11110 ftype 1 Rm 0001 10 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (@as(u32, src2.enc()) << 16) |
        (0b0001 << 12) |
        (0b10 << 10) |
        (@as(u32, src1.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FSQRT (floating-point square root)
pub fn fsqrtRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
    // FSQRT <Sd>, <Sn> or FSQRT <Dd>, <Dn>
    // 0 0 0 11110 ftype 1 0000 11 10000 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b0000 << 17) |
        (0b11 << 15) |
        (0b10000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FNEG (floating-point negate)
pub fn fnegRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
    // FNEG <Sd>, <Sn> or FNEG <Dd>, <Dn>
    // 0 0 0 11110 ftype 1 0000 10 10000 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b0000 << 17) |
        (0b10 << 15) |
        (0b10000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FABS (floating-point absolute value)
pub fn fabsRegReg(self: *Emit, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
    // FABS <Sd>, <Sn> or FABS <Dd>, <Dn>
    // 0 0 0 11110 ftype 1 0000 01 10000 Rn Rd
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b0000 << 17) |
        (0b01 << 15) |
        (0b10000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FCMP (floating-point compare)
pub fn fcmpRegReg(self: *Emit, ftype: FloatType, lhs: FloatReg, rhs: FloatReg) !void {
    // FCMP <Sn>, <Sm> or FCMP <Dn>, <Dm>
    // 0 0 0 11110 ftype 1 Rm 00 1000 Rn 0 0 000
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (@as(u32, rhs.enc()) << 16) |
        (0b00 << 14) |
        (0b1000 << 10) |
        (@as(u32, lhs.enc()) << 5) |
        (0b00000 << 0);
    try self.emit32(inst);
}

/// FCMP with zero
pub fn fcmpRegZero(self: *Emit, ftype: FloatType, reg: FloatReg) !void {
    // FCMP <Sn>, #0.0 or FCMP <Dn>, #0.0
    // 0 0 0 11110 ftype 1 00000 00 1000 Rn 0 1 000
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b00000 << 16) |
        (0b00 << 14) |
        (0b1000 << 10) |
        (@as(u32, reg.enc()) << 5) |
        (0b01000 << 0);
    try self.emit32(inst);
}

/// SCVTF (signed integer to float)
pub fn scvtfFloatFromGen(self: *Emit, ftype: FloatType, dst: FloatReg, src: GeneralReg, src_width: RegisterWidth) !void {
    // SCVTF <Sd>, <Wn> or SCVTF <Dd>, <Xn> etc.
    const sf = src_width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b00 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b00 << 19) |
        (0b010 << 16) |
        (0b000000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FCVTZS (float to signed integer with truncation toward zero)
pub fn fcvtzsGenFromFloat(self: *Emit, ftype: FloatType, dst: GeneralReg, src: FloatReg, dst_width: RegisterWidth) !void {
    // FCVTZS <Wd>, <Sn> or FCVTZS <Xd>, <Dn> etc.
    const sf = dst_width.sf();
    const inst: u32 = (@as(u32, sf) << 31) |
        (0b00 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(ftype)) << 22) |
        (0b1 << 21) |
        (0b11 << 19) |
        (0b000 << 16) |
        (0b000000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// FCVT (float to float conversion)
pub fn fcvtFloatFloat(self: *Emit, dst_type: FloatType, dst: FloatReg, src_type: FloatType, src: FloatReg) !void {
    // FCVT <Sd>, <Dn> or FCVT <Dd>, <Sn>
    // 0 0 0 11110 src_type 1 0001 dst_type 10000 Rn Rd
    const opc: u2 = @intFromEnum(dst_type);
    const inst: u32 = (0b000 << 29) |
        (0b11110 << 24) |
        (@as(u32, @intFromEnum(src_type)) << 22) |
        (0b1 << 21) |
        (0b0001 << 17) |
        (@as(u32, opc) << 15) |
        (0b10000 << 10) |
        (@as(u32, src.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// LDR (load float register) with unsigned offset
pub fn fldrRegMemUoff(self: *Emit, ftype: FloatType, dst: FloatReg, base: GeneralReg, uoffset: u12) !void {
    // LDR <St>, [<Xn|SP>, #<pimm>] (single) or LDR <Dt>, [<Xn|SP>, #<pimm>] (double)
    // size 111 1 01 01 imm12 Rn Rt
    const size: u2 = if (ftype == .double) 0b11 else 0b10;
    const inst: u32 = (@as(u32, size) << 30) |
        (0b111101 << 24) |
        (0b01 << 22) |
        (@as(u32, uoffset) << 10) |
        (@as(u32, base.enc()) << 5) |
        dst.enc();
    try self.emit32(inst);
}

/// STR (store float register) with unsigned offset
pub fn fstrRegMemUoff(self: *Emit, ftype: FloatType, src: FloatReg, base: GeneralReg, uoffset: u12) !void {
    // STR <St>, [<Xn|SP>, #<pimm>] (single) or STR <Dt>, [<Xn|SP>, #<pimm>] (double)
    // size 111 1 01 00 imm12 Rn Rt
    const size: u2 = if (ftype == .double) 0b11 else 0b10;
    const inst: u32 = (@as(u32, size) << 30) |
        (0b111101 << 24) |
        (0b00 << 22) |
        (@as(u32, uoffset) << 10) |
        (@as(u32, base.enc()) << 5) |
        src.enc();
    try self.emit32(inst);
}

// Tests

test "mov reg, reg" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // mov x0, x1 should be encoded as: ORR X0, XZR, X1
    try asm_buf.movRegReg(.w64, .X0, .X1);
    // Expected: sf=1, opc=01, shift=00, N=0, Rm=1, imm6=0, Rn=31, Rd=0
    // 1 01 01010 00 0 00001 000000 11111 00000
    // = 0xAA0103E0
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xE0, 0x03, 0x01, 0xAA }, asm_buf.buf.items);
}

test "movz" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // movz x0, #0x1234
    try asm_buf.movz(.w64, .X0, 0x1234, 0);
    // Expected: sf=1, opc=10, hw=00, imm16=0x1234, Rd=0
    // = 0xD2824680
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x80, 0x46, 0x82, 0xD2 }, asm_buf.buf.items);
}

test "ret" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    try asm_buf.ret();
    // RET (return to LR) = 0xD65F03C0
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 }, asm_buf.buf.items);
}

test "add reg, reg, imm12" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // add x0, x1, #0x10 (0x91004020)
    try asm_buf.addRegRegImm12(.w64, .X0, .X1, 0x10);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x20, 0x40, 0x00, 0x91 }, asm_buf.buf.items);
}

test "cmp reg, reg" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // cmp x0, x1 is subs xzr, x0, x1 (0xEB01001F)
    try asm_buf.cmpRegReg(.w64, .X0, .X1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x1F, 0x00, 0x01, 0xEB }, asm_buf.buf.items);
}

test "conditional branch" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // b.eq +8 (0x54000040)
    try asm_buf.bcond(.eq, 8);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x40, 0x00, 0x00, 0x54 }, asm_buf.buf.items);
}

test "fadd" {
    var asm_buf = Emit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // fadd d0, d1, d2 (0x1E622820)
    try asm_buf.faddRegRegReg(.double, .V0, .V1, .V2);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x20, 0x28, 0x62, 0x1E }, asm_buf.buf.items);
}

test "condition invert" {
    try std.testing.expectEqual(Condition.ne, Condition.eq.invert());
    try std.testing.expectEqual(Condition.eq, Condition.ne.invert());
    try std.testing.expectEqual(Condition.lt, Condition.ge.invert());
    try std.testing.expectEqual(Condition.ge, Condition.lt.invert());
}
