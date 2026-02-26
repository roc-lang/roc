//! aarch64 (ARM64) instruction encoding.
//!
//! This module provides low-level aarch64 instruction encoding for the dev backend.
//! It generates machine code bytes that can be executed directly on aarch64 processors.
//!
//! Note: All aarch64 instructions are 32 bits (4 bytes) and must be 4-byte aligned.
//!
//! The Emit function is parameterized by RocTarget to enable full cross-compilation.
//! Each target variant is specialized at comptime with the correct calling convention.

const std = @import("std");
const RocTarget = @import("roc_target").RocTarget;
const Registers = @import("Registers.zig");
const RegisterWidth = Registers.RegisterWidth;

const Relocation = @import("../Relocation.zig").Relocation;

/// aarch64 instruction emitter for generating machine code.
/// Parameterized by target for cross-compilation support.
pub fn Emit(comptime target: RocTarget) type {
    // Validate this is an aarch64 target
    if (target.toCpuArch() != .aarch64 and target.toCpuArch() != .aarch64_be) {
        @compileError("aarch64.Emit requires an aarch64 target");
    }

    return struct {
        const Self = @This();

        // Re-export register types so CallBuilder can access them via EmitType
        pub const GeneralReg = Registers.GeneralReg;
        pub const FloatReg = Registers.FloatReg;

        /// The target this Emit was instantiated for
        pub const roc_target = target;

        /// Calling convention constants derived from target (AAPCS64)
        /// Note: aarch64 uses AAPCS64 for all targets, including Windows.
        pub const CC = struct {
            pub const PARAM_REGS = [_]Registers.GeneralReg{
                .X0, .X1, .X2, .X3, .X4, .X5, .X6, .X7,
            };

            pub const FLOAT_PARAM_REGS = [_]Registers.FloatReg{
                .V0, .V1, .V2, .V3, .V4, .V5, .V6, .V7,
            };

            pub const RETURN_REGS = [_]Registers.GeneralReg{ .X0, .X1 };

            pub const SHADOW_SPACE: u8 = 0; // AAPCS64 has no shadow space
            pub const RETURN_BY_PTR_THRESHOLD: usize = 16;
            pub const PASS_BY_PTR_THRESHOLD: usize = std.math.maxInt(usize); // AAPCS64: no pass-by-pointer

            pub const SCRATCH_REG = Registers.GeneralReg.X9;
            pub const BASE_PTR = Registers.GeneralReg.FP;
            pub const STACK_PTR = Registers.GeneralReg.ZRSP;
            pub const STACK_ALIGNMENT: u32 = 16;

            /// Check if a struct of the given size can be passed by value.
            /// AAPCS64: structs up to 16 bytes can be passed in registers.
            pub fn canPassStructByValue(size: usize) bool {
                return size <= 16;
            }

            /// Align a stack size to the platform's required alignment.
            pub fn alignStackSize(size: u32) u32 {
                return (size + STACK_ALIGNMENT - 1) & ~(STACK_ALIGNMENT - 1);
            }

            /// Check if return type needs to use pointer (implicit first arg)
            pub fn needsReturnByPointer(return_size: usize) bool {
                return return_size > RETURN_BY_PTR_THRESHOLD;
            }

            /// Check if a struct argument needs to be passed by pointer.
            /// AAPCS64: never uses pass-by-pointer.
            pub fn needsPassByPointer(arg_size: usize) bool {
                return arg_size > PASS_BY_PTR_THRESHOLD;
            }

            /// Returns true if i128 values must be passed by pointer
            /// AAPCS64: i128 passed in register pair, not by pointer
            pub fn passI128ByPointer() bool {
                return false;
            }

            /// Returns true if i128 return values use hidden pointer arg
            /// AAPCS64: i128 returned in register pair
            pub fn returnI128ByPointer() bool {
                return false;
            }
        };

        allocator: std.mem.Allocator,
        buf: std.ArrayList(u8),
        relocs: std.ArrayList(Relocation),

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .allocator = allocator,
                .buf = .{},
                .relocs = .{},
            };
        }

        pub fn deinit(self: *Self) void {
            self.buf.deinit(self.allocator);
            self.relocs.deinit(self.allocator);
        }

        /// Get the current code offset
        pub fn codeOffset(self: *const Self) u64 {
            return @intCast(self.buf.items.len);
        }

        /// Emit a 32-bit instruction (little-endian)
        fn emit32(self: *Self, inst: u32) !void {
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(inst)));
        }

        // Movement instructions

        /// MOV reg, reg (register to register)
        /// For normal registers: ORR Xd, XZR, Xm
        /// For SP as source: ADD Xd, SP, #0 (since ORR treats reg 31 as XZR, not SP)
        pub fn movRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
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
        pub fn movz(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: u16, shift: u6) !void {
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
        pub fn movk(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: u16, shift: u6) !void {
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

        /// MOVN - Move with NOT (load inverted immediate)
        /// Sets dst = ~(imm16 << shift), useful for loading negative values
        pub fn movn(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: u16, shift: u6) !void {
            // MOVN <Xd>, #<imm16>, LSL #<shift>
            // 31 30 29 28 27 26 25 24 23 22 21 20               5 4    0
            // sf  0  0  1  0  0  1  0  1  hw[1:0]  imm16[15:0]  Rd[4:0]
            const sf = width.sf();
            const hw: u2 = @truncate(shift >> 4);
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b00100101 << 23) |
                (@as(u32, hw) << 21) |
                (@as(u32, imm) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// Load a 64-bit immediate into a register
        /// Uses MOVZ + MOVK sequence as needed
        pub fn movRegImm64(self: *Self, dst: GeneralReg, imm: u64) !void {
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

        /// Load a 32-bit (or smaller) signed immediate into a register
        /// Uses MOVZ + MOVK sequence, treating the value as a bit pattern
        pub fn movRegImm32(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: i32) !void {
            // Treat as unsigned bit pattern (matches Rust implementation style)
            const val: u32 = @bitCast(imm);
            const low16: u16 = @truncate(val);
            const high16: u16 = @truncate(val >> 16);

            try self.movz(width, dst, low16, 0);
            if (high16 != 0) try self.movk(width, dst, high16, 16);
        }

        // Arithmetic instructions

        /// ADD reg, reg, reg
        pub fn addRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // When dst or src1 is SP, must use extended register encoding
            // (shifted register encoding treats reg 31 as XZR, not SP)
            if (dst == .ZRSP or src1 == .ZRSP) {
                // ADD (extended register): sf 0 0 01011 00 1 Rm option imm3 Rn Rd
                // option=011 (UXTX for 64-bit), imm3=000
                const sf = width.sf();
                const inst: u32 = (@as(u32, sf) << 31) |
                    (0b0001011 << 24) |
                    (0b00 << 22) |
                    (1 << 21) | // extended register flag
                    (@as(u32, src2.enc()) << 16) |
                    (0b011 << 13) | // option = UXTX
                    (0b000 << 10) | // imm3 = 0
                    (@as(u32, src1.enc()) << 5) |
                    dst.enc();
                try self.emit32(inst);
            } else {
                // ADD (shifted register): sf 0 0 01011 shift 0 Rm imm6 Rn Rd
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
        }

        /// SUB reg, reg, reg
        pub fn subRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // When dst or src1 is SP, must use extended register encoding
            // (shifted register encoding treats reg 31 as XZR, not SP)
            if (dst == .ZRSP or src1 == .ZRSP) {
                // SUB (extended register): sf 1 0 01011 00 1 Rm option imm3 Rn Rd
                const sf = width.sf();
                const inst: u32 = (@as(u32, sf) << 31) |
                    (0b1001011 << 24) |
                    (0b00 << 22) |
                    (1 << 21) | // extended register flag
                    (@as(u32, src2.enc()) << 16) |
                    (0b011 << 13) | // option = UXTX
                    (0b000 << 10) | // imm3 = 0
                    (@as(u32, src1.enc()) << 5) |
                    dst.enc();
                try self.emit32(inst);
            } else {
                // SUB (shifted register): sf 1 0 01011 shift 0 Rm imm6 Rn Rd
                const sf = width.sf();
                const inst: u32 = (@as(u32, sf) << 31) |
                    (0b1001011 << 24) |
                    (0b00 << 22) |
                    (0 << 21) |
                    (@as(u32, src2.enc()) << 16) |
                    (0b000000 << 10) |
                    (@as(u32, src1.enc()) << 5) |
                    dst.enc();
                try self.emit32(inst);
            }
        }

        /// ADDS reg, reg, reg (add and set flags)
        pub fn addsRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // ADDS <Xd>, <Xn>, <Xm> - same as ADD but with S bit set
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b0101011 << 24) | // ADD with S bit (bit 29)
                (0b00 << 22) |
                (0 << 21) |
                (@as(u32, src2.enc()) << 16) |
                (0b000000 << 10) |
                (@as(u32, src1.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// ADC reg, reg, reg (add with carry)
        pub fn adcRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // ADC <Xd>, <Xn>, <Xm>
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b0011010000 << 21) |
                (@as(u32, src2.enc()) << 16) |
                (0b000000 << 10) |
                (@as(u32, src1.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// SUBS reg, reg, reg (subtract and set flags)
        pub fn subsRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // SUBS <Xd>, <Xn>, <Xm> - same as SUB but with S bit set
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b1101011 << 24) | // SUB with S bit (bit 29)
                (0b00 << 22) |
                (0 << 21) |
                (@as(u32, src2.enc()) << 16) |
                (0b000000 << 10) |
                (@as(u32, src1.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// CMP reg, reg (compare two registers by subtracting and setting flags)
        /// This is an alias for SUBS with destination XZR (discard result)
        pub fn cmp(self: *Self, width: RegisterWidth, src1: GeneralReg, src2: GeneralReg) !void {
            try self.subsRegRegReg(width, .ZRSP, src1, src2);
        }

        /// SBC reg, reg, reg (subtract with carry/borrow)
        pub fn sbcRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // SBC <Xd>, <Xn>, <Xm>
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b1011010000 << 21) |
                (@as(u32, src2.enc()) << 16) |
                (0b000000 << 10) |
                (@as(u32, src1.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// MUL reg, reg, reg (alias for MADD with XZR as addend)
        pub fn mulRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
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

        /// UMULH reg, reg, reg (unsigned multiply high - gives high 64 bits of 64x64->128 multiply)
        pub fn umulhRegRegReg(self: *Self, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // UMULH <Xd>, <Xn>, <Xm>
            // 1 0 0 1 1 0 1 1 1 1 0 Rm 0 1 1 1 1 1 Rn Rd
            const inst: u32 = (0b10011011110 << 21) |
                (@as(u32, src2.enc()) << 16) |
                (0b011111 << 10) |
                (@as(u32, src1.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// SMULH reg, reg, reg (signed multiply high - gives high 64 bits of 64x64->128 signed multiply)
        pub fn smulhRegRegReg(self: *Self, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
            // SMULH <Xd>, <Xn>, <Xm>
            // 1 0 0 1 1 0 1 1 0 1 0 Rm 0 1 1 1 1 1 Rn Rd
            const inst: u32 = (0b10011011010 << 21) |
                (@as(u32, src2.enc()) << 16) |
                (0b011111 << 10) |
                (@as(u32, src1.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// SDIV reg, reg, reg (signed divide)
        pub fn sdivRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
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
        pub fn udivRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
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
        pub fn msubRegRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, rn: GeneralReg, rm: GeneralReg, ra: GeneralReg) !void {
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
        pub fn addRegRegImm12(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: u12) !void {
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
        pub fn subRegRegImm12(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: u12) !void {
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
        pub fn negRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            // NEG is SUB Xd, XZR, Xm (shifted register encoding).
            // Must NOT use subRegRegReg here because its SP detection would
            // emit extended register encoding, interpreting reg 31 as SP
            // instead of XZR. NEG needs the shifted register form where
            // reg 31 in Rn means XZR.
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b1001011 << 24) |
                (0b00 << 22) |
                (0 << 21) |
                (@as(u32, src.enc()) << 16) |
                (0b000000 << 10) |
                (@as(u32, GeneralReg.ZRSP.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// CMP reg, reg (compare - alias for SUBS with XZR destination)
        pub fn cmpRegReg(self: *Self, width: RegisterWidth, lhs: GeneralReg, rhs: GeneralReg) !void {
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
        pub fn cmpRegImm12(self: *Self, width: RegisterWidth, reg: GeneralReg, imm: u12) !void {
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
        pub fn andRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
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
        pub fn orrRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
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
        pub fn eorRegRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg) !void {
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

        /// EOR reg, reg, imm (exclusive OR with immediate)
        /// Uses IP0 as scratch register for the immediate
        pub fn eorRegRegImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: u64) !void {
            // Load immediate into IP0, then EOR
            try self.movRegImm64(.IP0, imm);
            try self.eorRegRegReg(width, dst, src, .IP0);
        }

        /// LSL reg, reg, imm (logical shift left by immediate)
        pub fn lslRegRegImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, shift: u6) !void {
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
        pub fn lsrRegRegImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, shift: u6) !void {
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
        pub fn asrRegRegImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, shift: u6) !void {
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

        /// LSLV dst, src, amount (shift left by register)
        pub fn lslRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) !void {
            // LSLV <Xd>, <Xn>, <Xm>: sf=1 | 0b0011010110 | Rm | 0b001000 | Rn | Rd
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b0011010110 << 21) |
                (@as(u32, amount.enc()) << 16) |
                (0b001000 << 10) |
                (@as(u32, src.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// LSRV dst, src, amount (logical shift right by register)
        pub fn lsrRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) !void {
            // LSRV <Xd>, <Xn>, <Xm>: sf=1 | 0b0011010110 | Rm | 0b001001 | Rn | Rd
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b0011010110 << 21) |
                (@as(u32, amount.enc()) << 16) |
                (0b001001 << 10) |
                (@as(u32, src.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// ASRV dst, src, amount (arithmetic shift right by register)
        pub fn asrRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, amount: GeneralReg) !void {
            // ASRV <Xd>, <Xn>, <Xm>: sf=1 | 0b0011010110 | Rm | 0b001010 | Rn | Rd
            const sf = width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b0011010110 << 21) |
                (@as(u32, amount.enc()) << 16) |
                (0b001010 << 10) |
                (@as(u32, src.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        // Control flow instructions

        /// RET (return to address in LR)
        pub fn ret(self: *Self) !void {
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

        /// UDF (permanently undefined instruction - generates exception)
        pub fn udf(self: *Self, imm16: u16) !void {
            // UDF #imm16
            // 0000 0000 0000 0000 imm16
            const inst: u32 = @as(u32, imm16);
            try self.emit32(inst);
        }

        /// ADR Xd, #imm — compute PC-relative address
        /// offset_bytes is a byte offset from the ADR instruction, range ±1 MB.
        pub fn adr(self: *Self, rd: GeneralReg, offset_bytes: i21) !void {
            // ADR: 0 immlo[1:0] 10000 immhi[18:0] Rd[4:0]
            const imm: u21 = @bitCast(offset_bytes);
            const immlo: u2 = @truncate(imm);
            const immhi: u19 = @truncate(imm >> 2);
            const inst: u32 = (0 << 31) |
                (@as(u32, immlo) << 29) |
                (0b10000 << 24) |
                (@as(u32, immhi) << 5) |
                @as(u32, rd.enc());
            try self.emit32(inst);
        }

        /// BLR Xn (branch with link to register - call to address in register)
        pub fn blrReg(self: *Self, reg: GeneralReg) !void {
            // BLR <Xn>
            // 1101011 0001 11111 000000 Rn[4:0] 00000
            const inst: u32 = (0b1101011 << 25) |
                (0b0001 << 21) |
                (0b11111 << 16) |
                (0b000000 << 10) |
                (@as(u32, reg.enc()) << 5) |
                0b00000;
            try self.emit32(inst);
        }

        /// BL (branch with link - function call)
        pub fn bl(self: *Self, offset_bytes: i32) !void {
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
        pub fn b(self: *Self, offset_bytes: i32) !void {
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
        pub fn bcond(self: *Self, cond: Condition, offset_bytes: i32) !void {
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
        pub fn cbz(self: *Self, width: RegisterWidth, reg: GeneralReg, offset_bytes: i32) !void {
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
        pub fn cbnz(self: *Self, width: RegisterWidth, reg: GeneralReg, offset_bytes: i32) !void {
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
        pub fn csel(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg, cond: Condition) !void {
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
        pub fn csinc(self: *Self, width: RegisterWidth, dst: GeneralReg, src1: GeneralReg, src2: GeneralReg, cond: Condition) !void {
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
        pub fn cset(self: *Self, width: RegisterWidth, dst: GeneralReg, cond: Condition) !void {
            // CSET <Xd>, <cond> is CSINC <Xd>, XZR, XZR, invert(cond)
            try self.csinc(width, dst, .ZRSP, .ZRSP, cond.invert());
        }

        /// BRK #1 - Breakpoint instruction (generates SIGTRAP, used as trap)
        pub fn brk(self: *Self) !void {
            // BRK #imm16: 1101_0100_001 imm16 00000
            // Using imm16=1 as a conventional trap value
            try self.emit32(0xD4200020);
        }

        // Memory instructions

        /// LDR (load register) with unsigned offset
        pub fn ldrRegMemUoff(self: *Self, width: RegisterWidth, dst: GeneralReg, base: GeneralReg, uoffset: u12) !void {
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
        pub fn strRegMemUoff(self: *Self, width: RegisterWidth, src: GeneralReg, base: GeneralReg, uoffset: u12) !void {
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
        pub fn ldurRegMem(self: *Self, width: RegisterWidth, dst: GeneralReg, base: GeneralReg, offset: i9) !void {
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
        pub fn sturRegMem(self: *Self, width: RegisterWidth, src: GeneralReg, base: GeneralReg, offset: i9) !void {
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

        /// STURB (store register byte unscaled) with signed offset
        pub fn sturbRegMem(self: *Self, src: GeneralReg, base: GeneralReg, offset: i9) !void {
            // STURB <Wt>, [<Xn|SP>, #<simm>]
            // 00 111 0 00 00 0 imm9 00 Rn Rt
            const imm9: u9 = @bitCast(offset);
            const inst: u32 = (0b00 << 30) | // size = 00 for byte
                (0b111000 << 24) |
                (0b00 << 22) |
                (0 << 21) |
                (@as(u32, imm9) << 12) |
                (0b00 << 10) |
                (@as(u32, base.enc()) << 5) |
                src.enc();
            try self.emit32(inst);
        }

        /// STURH (store register halfword unscaled) with signed offset
        pub fn sturhRegMem(self: *Self, src: GeneralReg, base: GeneralReg, offset: i9) !void {
            // STURH <Wt>, [<Xn|SP>, #<simm>]
            // 01 111 0 00 00 0 imm9 00 Rn Rt
            const imm9: u9 = @bitCast(offset);
            const inst: u32 = (0b01 << 30) | // size = 01 for halfword
                (0b111000 << 24) |
                (0b00 << 22) |
                (0 << 21) |
                (@as(u32, imm9) << 12) |
                (0b00 << 10) |
                (@as(u32, base.enc()) << 5) |
                src.enc();
            try self.emit32(inst);
        }

        /// LDURB (load register byte unscaled, zero-extend) with signed offset
        pub fn ldurbRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, offset: i9) !void {
            // LDURB <Wt>, [<Xn|SP>, #<simm>]
            // 00 111 0 00 01 0 imm9 00 Rn Rt
            const imm9: u9 = @bitCast(offset);
            const inst: u32 = (0b00 << 30) | // size = 00 for byte
                (0b111000 << 24) |
                (0b01 << 22) | // opc = 01 for load
                (0 << 21) |
                (@as(u32, imm9) << 12) |
                (0b00 << 10) |
                (@as(u32, base.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// LDURH (load register halfword unscaled, zero-extend) with signed offset
        pub fn ldurhRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, offset: i9) !void {
            // LDURH <Wt>, [<Xn|SP>, #<simm>]
            // 01 111 0 00 01 0 imm9 00 Rn Rt
            const imm9: u9 = @bitCast(offset);
            const inst: u32 = (0b01 << 30) | // size = 01 for halfword
                (0b111000 << 24) |
                (0b01 << 22) | // opc = 01 for load
                (0 << 21) |
                (@as(u32, imm9) << 12) |
                (0b00 << 10) |
                (@as(u32, base.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// LDRH (load register halfword, zero-extend) with unsigned offset
        pub fn ldrhRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, uoffset: u12) !void {
            // LDRH <Wt>, [<Xn|SP>, #<pimm>]
            // 01 111 0 01 01 imm12 Rn Rt
            const inst: u32 = (0b01 << 30) |
                (0b111001 << 24) |
                (0b01 << 22) |
                (@as(u32, uoffset) << 10) |
                (@as(u32, base.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// LDR with signed offset (i32)
        /// Handles arbitrary signed offsets by choosing appropriate encoding:
        /// - Small offsets (-256 to 255): use LDUR (unscaled)
        /// - Positive aligned offsets (0 to 32760 for 64-bit): use LDR with unsigned offset
        /// - Other offsets: compute address in IP0 then load with zero offset
        pub fn ldrRegMemSoff(self: *Self, width: RegisterWidth, dst: GeneralReg, base: GeneralReg, offset: i32) !void {
            // Try LDUR for small signed offsets
            if (offset >= -256 and offset <= 255) {
                try self.ldurRegMem(width, dst, base, @intCast(offset));
                return;
            }

            // Try unsigned offset for positive, aligned offsets
            if (offset >= 0) {
                const scale: u5 = if (width == .w64) 3 else 2; // 8 or 4 bytes
                const uoff: u32 = @intCast(offset);
                if ((uoff & ((@as(u32, 1) << scale) - 1)) == 0) { // Check alignment
                    const scaled = uoff >> scale;
                    if (scaled <= 4095) {
                        try self.ldrRegMemUoff(width, dst, base, @intCast(scaled));
                        return;
                    }
                }
            }

            // Handle offsets outside LDUR range [-256,255] and not suitable for unsigned encoding
            // Use movRegImm64 for correct sign extension of negative offsets
            try self.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
            try self.addRegRegReg(.w64, .IP0, base, .IP0);
            try self.ldurRegMem(width, dst, .IP0, 0);
        }

        /// STR with signed offset (i32)
        /// Handles arbitrary signed offsets by choosing appropriate encoding
        pub fn strRegMemSoff(self: *Self, width: RegisterWidth, src: GeneralReg, base: GeneralReg, offset: i32) !void {
            // Try STUR for small signed offsets
            if (offset >= -256 and offset <= 255) {
                try self.sturRegMem(width, src, base, @intCast(offset));
                return;
            }

            // Try unsigned offset for positive, aligned offsets
            if (offset >= 0) {
                const scale: u5 = if (width == .w64) 3 else 2; // 8 or 4 bytes
                const uoff: u32 = @intCast(offset);
                if ((uoff & ((@as(u32, 1) << scale) - 1)) == 0) { // Check alignment
                    const scaled = uoff >> scale;
                    if (scaled <= 4095) {
                        try self.strRegMemUoff(width, src, base, @intCast(scaled));
                        return;
                    }
                }
            }

            // Handle offsets outside STUR range [-256,255] and not suitable for unsigned encoding
            // Use movRegImm64 for correct sign extension of negative offsets
            try self.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
            try self.addRegRegReg(.w64, .IP0, base, .IP0);
            try self.sturRegMem(width, src, .IP0, 0);
        }

        /// LDRB (load register byte, zero-extend)
        pub fn ldrbRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, uoffset: u12) !void {
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
        pub fn strbRegMem(self: *Self, src: GeneralReg, base: GeneralReg, uoffset: u12) !void {
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

        /// STRH (store register halfword)
        pub fn strhRegMem(self: *Self, src: GeneralReg, base: GeneralReg, uoffset: u12) !void {
            // STRH <Wt>, [<Xn|SP>, #<pimm>]
            // 01 111 0 01 00 imm12 Rn Rt
            // Note: uoffset is scaled by 2 (halfword), caller must divide offset by 2
            const inst: u32 = (0b01 << 30) |
                (0b111001 << 24) |
                (0b00 << 22) |
                (@as(u32, uoffset) << 10) |
                (@as(u32, base.enc()) << 5) |
                src.enc();
            try self.emit32(inst);
        }

        // Stack instructions

        /// STP (store pair) - commonly used for pushing to stack
        pub fn stpPreIndex(self: *Self, width: RegisterWidth, reg1: GeneralReg, reg2: GeneralReg, base: GeneralReg, imm_offset: i7) !void {
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
        pub fn ldpPostIndex(self: *Self, width: RegisterWidth, reg1: GeneralReg, reg2: GeneralReg, base: GeneralReg, imm_offset: i7) !void {
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

        /// STP (store pair) with signed offset - no writeback
        /// Used for saving registers to pre-allocated frame slots
        pub fn stpSignedOffset(self: *Self, width: RegisterWidth, reg1: GeneralReg, reg2: GeneralReg, base: GeneralReg, imm_offset: i7) !void {
            // STP <Xt1>, <Xt2>, [<Xn|SP>, #<imm>]
            // ARM encoding: opc 101 V 010 L imm7 Rt2 Rn Rt
            // bits 31-30: opc (10 for 64-bit, 00 for 32-bit)
            // bits 29-27: 101
            // bit 26: V (0 for scalar)
            // bits 25-23: 010 (signed offset, no writeback)
            // bit 22: L (0 for store STP)
            // bits 21-15: imm7 (signed, scaled by register size)
            // bits 14-10: Rt2
            // bits 9-5: Rn (base register)
            // bits 4-0: Rt
            const opc: u2 = if (width == .w64) 0b10 else 0b00;
            const imm7: u7 = @bitCast(imm_offset);
            const inst: u32 = (@as(u32, opc) << 30) |
                (0b101 << 27) |
                (0b0 << 26) |
                (0b010 << 23) | // Signed offset mode: bits 25-23 = 010
                (0b0 << 22) | // L=0 for STP (store)
                (@as(u32, imm7) << 15) |
                (@as(u32, reg2.enc()) << 10) |
                (@as(u32, base.enc()) << 5) |
                reg1.enc();
            try self.emit32(inst);
        }

        /// LDP (load pair) with signed offset - no writeback
        /// Used for restoring registers from pre-allocated frame slots
        pub fn ldpSignedOffset(self: *Self, width: RegisterWidth, reg1: GeneralReg, reg2: GeneralReg, base: GeneralReg, imm_offset: i7) !void {
            // LDP <Xt1>, <Xt2>, [<Xn|SP>, #<imm>]
            // ARM encoding: opc 101 V 010 L imm7 Rt2 Rn Rt
            // bits 31-30: opc (10 for 64-bit, 00 for 32-bit)
            // bits 29-27: 101
            // bit 26: V (0 for scalar)
            // bits 25-23: 010 (signed offset, no writeback)
            // bit 22: L (1 for load LDP)
            // bits 21-15: imm7 (signed, scaled by register size)
            // bits 14-10: Rt2
            // bits 9-5: Rn (base register)
            // bits 4-0: Rt
            const opc: u2 = if (width == .w64) 0b10 else 0b00;
            const imm7: u7 = @bitCast(imm_offset);
            const inst: u32 = (@as(u32, opc) << 30) |
                (0b101 << 27) |
                (0b0 << 26) |
                (0b010 << 23) | // Signed offset mode: bits 25-23 = 010
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
        pub fn fmovRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
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
        pub fn fmovFloatFromGen(self: *Self, ftype: FloatType, dst: FloatReg, src: GeneralReg) !void {
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
        pub fn fmovGenFromFloat(self: *Self, ftype: FloatType, dst: GeneralReg, src: FloatReg) !void {
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
        pub fn faddRegRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
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
        pub fn fsubRegRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
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
        pub fn fmulRegRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
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
        pub fn fdivRegRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src1: FloatReg, src2: FloatReg) !void {
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
        pub fn fsqrtRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
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
        pub fn fnegRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
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
        pub fn fabsRegReg(self: *Self, ftype: FloatType, dst: FloatReg, src: FloatReg) !void {
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
        pub fn fcmpRegReg(self: *Self, ftype: FloatType, lhs: FloatReg, rhs: FloatReg) !void {
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
        pub fn fcmpRegZero(self: *Self, ftype: FloatType, reg: FloatReg) !void {
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
        pub fn scvtfFloatFromGen(self: *Self, ftype: FloatType, dst: FloatReg, src: GeneralReg, src_width: RegisterWidth) !void {
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

        /// UCVTF (unsigned integer to float)
        pub fn ucvtfFloatFromGen(self: *Self, ftype: FloatType, dst: FloatReg, src: GeneralReg, src_width: RegisterWidth) !void {
            // UCVTF <Sd>, <Wn> or UCVTF <Dd>, <Xn> etc.
            // Same as SCVTF but opcode = 011 instead of 010
            const sf = src_width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b00 << 29) |
                (0b11110 << 24) |
                (@as(u32, @intFromEnum(ftype)) << 22) |
                (0b1 << 21) |
                (0b00 << 19) |
                (0b011 << 16) |
                (0b000000 << 10) |
                (@as(u32, src.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// FCVTZS (float to signed integer with truncation toward zero)
        pub fn fcvtzsGenFromFloat(self: *Self, ftype: FloatType, dst: GeneralReg, src: FloatReg, dst_width: RegisterWidth) !void {
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

        /// FCVTZU (float to unsigned integer with truncation toward zero)
        pub fn fcvtzuGenFromFloat(self: *Self, ftype: FloatType, dst: GeneralReg, src: FloatReg, dst_width: RegisterWidth) !void {
            // FCVTZU <Wd>, <Sn> or FCVTZU <Xd>, <Dn> etc.
            // Same as FCVTZS but opcode = 001 instead of 000
            const sf = dst_width.sf();
            const inst: u32 = (@as(u32, sf) << 31) |
                (0b00 << 29) |
                (0b11110 << 24) |
                (@as(u32, @intFromEnum(ftype)) << 22) |
                (0b1 << 21) |
                (0b11 << 19) |
                (0b001 << 16) |
                (0b000000 << 10) |
                (@as(u32, src.enc()) << 5) |
                dst.enc();
            try self.emit32(inst);
        }

        /// FCVT (float to float conversion)
        pub fn fcvtFloatFloat(self: *Self, dst_type: FloatType, dst: FloatReg, src_type: FloatType, src: FloatReg) !void {
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
        pub fn fldrRegMemUoff(self: *Self, ftype: FloatType, dst: FloatReg, base: GeneralReg, uoffset: u12) !void {
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
        pub fn fstrRegMemUoff(self: *Self, ftype: FloatType, src: FloatReg, base: GeneralReg, uoffset: u12) !void {
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
    }; // end of struct returned by Emit
}

// Target-specific Emit types for cross-platform testing
const LinuxEmit = Emit(.arm64linux);
const WinEmit = Emit(.arm64win);
const MacEmit = Emit(.arm64mac);

// Tests

test "mov reg, reg" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // mov x0, x1 should be encoded as: ORR X0, XZR, X1
    try asm_buf.movRegReg(.w64, .X0, .X1);
    // Expected: sf=1, opc=01, shift=00, N=0, Rm=1, imm6=0, Rn=31, Rd=0
    // 1 01 01010 00 0 00001 000000 11111 00000
    // = 0xAA0103E0
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xE0, 0x03, 0x01, 0xAA }, asm_buf.buf.items);
}

test "movz" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // movz x0, #0x1234
    try asm_buf.movz(.w64, .X0, 0x1234, 0);
    // Expected: sf=1, opc=10, hw=00, imm16=0x1234, Rd=0
    // = 0xD2824680
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x80, 0x46, 0x82, 0xD2 }, asm_buf.buf.items);
}

test "ret" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    try asm_buf.ret();
    // RET (return to LR) = 0xD65F03C0
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 }, asm_buf.buf.items);
}

test "add reg, reg, imm12" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // add x0, x1, #0x10 (0x91004020)
    try asm_buf.addRegRegImm12(.w64, .X0, .X1, 0x10);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x20, 0x40, 0x00, 0x91 }, asm_buf.buf.items);
}

test "cmp reg, reg" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // cmp x0, x1 is subs xzr, x0, x1 (0xEB01001F)
    try asm_buf.cmpRegReg(.w64, .X0, .X1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x1F, 0x00, 0x01, 0xEB }, asm_buf.buf.items);
}

test "conditional branch" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // b.eq +8 (0x54000040)
    try asm_buf.bcond(.eq, 8);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x40, 0x00, 0x00, 0x54 }, asm_buf.buf.items);
}

test "fadd" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // fadd d0, d1, d2 (0x1E622820)
    try asm_buf.faddRegRegReg(.double, .V0, .V1, .V2);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x20, 0x28, 0x62, 0x1E }, asm_buf.buf.items);
}

test "condition invert" {
    try std.testing.expectEqual(LinuxEmit.Condition.ne, LinuxEmit.Condition.eq.invert());
    try std.testing.expectEqual(LinuxEmit.Condition.eq, LinuxEmit.Condition.ne.invert());
    try std.testing.expectEqual(LinuxEmit.Condition.lt, LinuxEmit.Condition.ge.invert());
    try std.testing.expectEqual(LinuxEmit.Condition.ge, LinuxEmit.Condition.lt.invert());
}

// Multi-target calling convention tests - verify AAPCS64 is uniform across all aarch64 targets

test "CC constants identical across all aarch64 targets" {
    // All aarch64 targets use AAPCS64 - 8 param regs
    try std.testing.expectEqual(@as(usize, 8), LinuxEmit.CC.PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), WinEmit.CC.PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), MacEmit.CC.PARAM_REGS.len);

    // 8 float param regs
    try std.testing.expectEqual(@as(usize, 8), LinuxEmit.CC.FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), WinEmit.CC.FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), MacEmit.CC.FLOAT_PARAM_REGS.len);

    // 2 return regs (X0, X1)
    try std.testing.expectEqual(@as(usize, 2), LinuxEmit.CC.RETURN_REGS.len);
    try std.testing.expectEqual(@as(usize, 2), WinEmit.CC.RETURN_REGS.len);
    try std.testing.expectEqual(@as(usize, 2), MacEmit.CC.RETURN_REGS.len);

    // No shadow space on any aarch64 target
    try std.testing.expectEqual(@as(u8, 0), LinuxEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(u8, 0), WinEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(u8, 0), MacEmit.CC.SHADOW_SPACE);

    // Return by pointer threshold is 16 bytes for all
    try std.testing.expectEqual(@as(usize, 16), LinuxEmit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(@as(usize, 16), WinEmit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(@as(usize, 16), MacEmit.CC.RETURN_BY_PTR_THRESHOLD);
}

test "CC.canPassStructByValue identical across aarch64 targets" {
    // AAPCS64: structs up to 16 bytes can be passed by value
    try std.testing.expect(LinuxEmit.CC.canPassStructByValue(1));
    try std.testing.expect(LinuxEmit.CC.canPassStructByValue(16));
    try std.testing.expect(!LinuxEmit.CC.canPassStructByValue(17));

    try std.testing.expect(WinEmit.CC.canPassStructByValue(16));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(17));

    try std.testing.expect(MacEmit.CC.canPassStructByValue(16));
    try std.testing.expect(!MacEmit.CC.canPassStructByValue(17));
}

test "CC.passI128ByPointer is false for all aarch64 targets" {
    // AAPCS64: i128 passed in register pair, not by pointer
    try std.testing.expect(!LinuxEmit.CC.passI128ByPointer());
    try std.testing.expect(!WinEmit.CC.passI128ByPointer());
    try std.testing.expect(!MacEmit.CC.passI128ByPointer());
}

test "CC.returnI128ByPointer is false for all aarch64 targets" {
    // AAPCS64: i128 returned in register pair
    try std.testing.expect(!LinuxEmit.CC.returnI128ByPointer());
    try std.testing.expect(!WinEmit.CC.returnI128ByPointer());
    try std.testing.expect(!MacEmit.CC.returnI128ByPointer());
}
