//! x86_64 instruction encoding.
//!
//! This module provides low-level x86_64 instruction encoding for the dev backend.
//! It generates machine code bytes that can be executed directly on x86_64 processors.
//!
//! The Emit function is parameterized by RocTarget to enable full cross-compilation.
//! Each target variant is specialized at comptime with the correct calling convention.

const std = @import("std");
const RocTarget = @import("roc_target").RocTarget;
const Registers = @import("Registers.zig");
const RegisterWidth = Registers.RegisterWidth;

const Relocation = @import("../Relocation.zig").Relocation;

/// x86_64 instruction emitter for generating machine code.
/// Parameterized by target for cross-compilation support.
pub fn Emit(comptime target: RocTarget) type {
    // Validate this is an x86_64 target
    if (target.toCpuArch() != .x86_64) {
        @compileError("x86_64.Emit requires an x86_64 target");
    }

    return struct {
        const Self = @This();

        // Re-export register types so CallBuilder can access them via EmitType
        pub const GeneralReg = Registers.GeneralReg;
        pub const FloatReg = Registers.FloatReg;

        /// The target this Emit was instantiated for
        pub const roc_target = target;

        /// Calling convention constants derived from target
        pub const CC = struct {
            pub const PARAM_REGS = if (target.isWindows())
                [_]Registers.GeneralReg{ .RCX, .RDX, .R8, .R9 }
            else
                [_]Registers.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 };

            pub const FLOAT_PARAM_REGS = if (target.isWindows())
                [_]Registers.FloatReg{ .XMM0, .XMM1, .XMM2, .XMM3 }
            else
                [_]Registers.FloatReg{ .XMM0, .XMM1, .XMM2, .XMM3, .XMM4, .XMM5, .XMM6, .XMM7 };

            pub const RETURN_REGS = if (target.isWindows())
                [_]Registers.GeneralReg{.RAX}
            else
                [_]Registers.GeneralReg{ .RAX, .RDX };

            pub const SHADOW_SPACE: u8 = if (target.isWindows()) 32 else 0;
            pub const RETURN_BY_PTR_THRESHOLD: usize = if (target.isWindows()) 8 else 16;
            pub const PASS_BY_PTR_THRESHOLD: usize = if (target.isWindows()) 8 else std.math.maxInt(usize);

            pub const SCRATCH_REG = Registers.GeneralReg.R11;
            pub const BASE_PTR = Registers.GeneralReg.RBP;
            pub const STACK_PTR = Registers.GeneralReg.RSP;
            pub const STACK_ALIGNMENT: u32 = 16;

            /// Check if a struct of the given size can be passed by value in a register.
            /// Windows x64 ABI: Only structs of size 1, 2, 4, or 8 bytes can be passed by value.
            pub fn canPassStructByValue(size: usize) bool {
                if (target.isWindows()) {
                    return size == 1 or size == 2 or size == 4 or size == 8;
                }
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
            pub fn needsPassByPointer(arg_size: usize) bool {
                if (target.isWindows()) {
                    return !(arg_size == 1 or arg_size == 2 or arg_size == 4 or arg_size == 8);
                }
                return arg_size > PASS_BY_PTR_THRESHOLD;
            }

            /// Returns true if i128 values must be passed by pointer (Windows x64)
            pub fn passI128ByPointer() bool {
                return target.isWindows();
            }

            /// Returns true if i128 return values use hidden pointer arg (Windows x64)
            pub fn returnI128ByPointer() bool {
                return target.isWindows();
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
        pub fn offset(self: *const Self) u64 {
            return @intCast(self.buf.items.len);
        }

        // REX prefix helpers

        /// REX prefix byte layout: 0100WRXB
        const REX_BASE: u8 = 0b0100_0000;

        /// Build a REX prefix byte
        fn rex(w: u1, r: u1, x: u1, b: u1) u8 {
            return REX_BASE | (@as(u8, w) << 3) | (@as(u8, r) << 2) | (@as(u8, x) << 1) | b;
        }

        /// Emit REX prefix if needed for 64-bit operation or extended registers
        fn emitRex(self: *Self, width: RegisterWidth, reg: ?GeneralReg, rm: ?GeneralReg) !void {
            const w: u1 = if (width.requiresRexW()) 1 else 0;
            const r: u1 = if (reg) |r_reg| r_reg.rexR() else 0;
            const b: u1 = if (rm) |rm_reg| rm_reg.rexB() else 0;

            // Always emit REX for 64-bit ops or extended registers
            if (w == 1 or r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(w, r, 0, b));
            }
        }

        // ModR/M byte helpers

        /// Build a ModR/M byte
        fn modRM(mod: u2, reg: u3, rm: u3) u8 {
            return (@as(u8, mod) << 6) | (@as(u8, reg) << 3) | rm;
        }

        // Movement instructions

        /// MOV reg, reg (64-bit)
        pub fn movRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66); // Operand size override
            }
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x89); // MOV r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// XCHG reg, reg (exchange two registers)
        pub fn xchgRegReg(self: *Self, width: RegisterWidth, a: GeneralReg, b: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, a, b);
            try self.buf.append(self.allocator, 0x87); // XCHG r/m, r
            try self.buf.append(self.allocator, modRM(0b11, a.enc(), b.enc()));
        }

        /// MOV reg, imm64 (movabs)
        pub fn movRegImm64(self: *Self, dst: GeneralReg, imm: i64) !void {
            try self.emitRex(.w64, null, dst);
            try self.buf.append(self.allocator, 0xB8 + @as(u8, dst.enc())); // MOV r64, imm64
            try self.buf.appendSlice(self.allocator, &@as([8]u8, @bitCast(imm)));
        }

        /// MOV reg, imm32 (sign-extended to 64-bit)
        pub fn movRegImm32(self: *Self, dst: GeneralReg, imm: i32) !void {
            try self.emitRex(.w64, null, dst);
            try self.buf.append(self.allocator, 0xC7); // MOV r/m64, imm32
            try self.buf.append(self.allocator, modRM(0b11, 0, dst.enc()));
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }

        // Arithmetic instructions

        /// ADD reg, reg
        pub fn addRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x01); // ADD r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// SUB reg, reg
        pub fn subRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x29); // SUB r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// ADC reg, reg (add with carry)
        pub fn adcRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x11); // ADC r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// SBB reg, reg (subtract with borrow)
        pub fn sbbRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x19); // SBB r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// IMUL reg, reg (signed multiply, result in first reg)
        pub fn imulRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            try self.emitRex(width, dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0xAF); // IMUL r, r/m
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// MUL r64 - unsigned widening multiply: RDX:RAX = RAX * src
        /// Result: low 64 bits in RAX, high 64 bits in RDX
        pub fn mulReg(self: *Self, width: RegisterWidth, src: GeneralReg) !void {
            try self.emitRex(width, null, src);
            try self.buf.append(self.allocator, 0xF7); // MUL r/m
            try self.buf.append(self.allocator, modRM(0b11, 4, src.enc())); // /4 = MUL
        }

        /// IMUL r64 - signed widening multiply: RDX:RAX = RAX * src (single operand form)
        /// Result: low 64 bits in RAX, high 64 bits in RDX
        pub fn imulRegWidening(self: *Self, width: RegisterWidth, src: GeneralReg) !void {
            try self.emitRex(width, null, src);
            try self.buf.append(self.allocator, 0xF7); // IMUL r/m
            try self.buf.append(self.allocator, modRM(0b11, 5, src.enc())); // /5 = IMUL
        }

        /// ADD reg, imm32 (sign-extended)
        pub fn addRegImm32(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: i32) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, dst);
            // Use short form for RAX
            if (dst == .RAX and !width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x05);
            } else {
                try self.buf.append(self.allocator, 0x81);
                try self.buf.append(self.allocator, modRM(0b11, 0, dst.enc()));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }

        /// ADD reg, imm (convenience wrapper using 64-bit width)
        pub fn addImm(self: *Self, dst: GeneralReg, imm: i32) !void {
            try self.addRegImm32(.w64, dst, imm);
        }

        /// ADD reg, imm (small immediate, alias for addRegImm32)
        pub fn addRegImm(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: i32) !void {
            try self.addRegImm32(width, dst, imm);
        }

        /// SUB reg, imm32 (sign-extended)
        pub fn subRegImm32(self: *Self, width: RegisterWidth, dst: GeneralReg, imm: i32) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, dst);
            // Use short form for RAX
            if (dst == .RAX and !width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x2D);
            } else {
                try self.buf.append(self.allocator, 0x81);
                try self.buf.append(self.allocator, modRM(0b11, 5, dst.enc()));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }

        /// CMP reg, reg
        pub fn cmpRegReg(self: *Self, width: RegisterWidth, a: GeneralReg, b: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, b, a);
            try self.buf.append(self.allocator, 0x39); // CMP r/m, r
            try self.buf.append(self.allocator, modRM(0b11, b.enc(), a.enc()));
        }

        /// CMP reg, imm32
        pub fn cmpRegImm32(self: *Self, width: RegisterWidth, reg: GeneralReg, imm: i32) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            // Use short form for RAX
            if (reg == .RAX) {
                try self.buf.append(self.allocator, 0x3D);
            } else {
                try self.buf.append(self.allocator, 0x81);
                try self.buf.append(self.allocator, modRM(0b11, 7, reg.enc()));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }

        /// TEST reg, reg (AND without storing result, sets flags)
        pub fn testRegReg(self: *Self, width: RegisterWidth, a: GeneralReg, b: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, b, a);
            try self.buf.append(self.allocator, 0x85); // TEST r/m, r
            try self.buf.append(self.allocator, modRM(0b11, b.enc(), a.enc()));
        }

        /// NEG reg (two's complement negation)
        pub fn negReg(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xF7);
            try self.buf.append(self.allocator, modRM(0b11, 3, reg.enc()));
        }

        /// NOT reg (one's complement)
        pub fn notReg(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xF7);
            try self.buf.append(self.allocator, modRM(0b11, 2, reg.enc()));
        }

        /// IDIV reg (signed divide RDX:RAX by reg, quotient in RAX, remainder in RDX)
        pub fn idivReg(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xF7);
            try self.buf.append(self.allocator, modRM(0b11, 7, reg.enc())); // /7 = IDIV
        }

        /// DIV reg (unsigned divide RDX:RAX by reg, quotient in RAX, remainder in RDX)
        pub fn divReg(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xF7);
            try self.buf.append(self.allocator, modRM(0b11, 6, reg.enc())); // /6 = DIV
        }

        /// CQO (sign-extend RAX into RDX:RAX for 64-bit division)
        pub fn cqo(self: *Self) !void {
            try self.buf.append(self.allocator, 0x48); // REX.W
            try self.buf.append(self.allocator, 0x99); // CQO
        }

        /// CDQ (sign-extend EAX into EDX:EAX for 32-bit division)
        pub fn cdq(self: *Self) !void {
            try self.buf.append(self.allocator, 0x99); // CDQ
        }

        /// AND reg, reg
        pub fn andRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x21); // AND r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// AND r64, imm8 - AND register with sign-extended immediate byte
        /// Used after SETCC to mask the result to just the lowest bit.
        pub fn andRegImm8(self: *Self, dst: GeneralReg, imm: i8) !void {
            // REX.W prefix (0x48) + REX.B if dst is R8-R15
            try self.buf.append(self.allocator, rex(1, 0, 0, dst.rexB()));
            try self.buf.append(self.allocator, 0x83); // AND r/m64, imm8
            // ModRM: mod=11 (register), reg=4 (/4 for AND), rm=dst
            try self.buf.append(self.allocator, modRM(0b11, 4, dst.enc()));
            try self.buf.append(self.allocator, @bitCast(imm));
        }

        /// AND r64, imm32 - AND register with 32-bit immediate
        pub fn andRegImm32(self: *Self, dst: GeneralReg, imm: i32) !void {
            // REX.W prefix (0x48) + REX.B if dst is R8-R15
            try self.buf.append(self.allocator, rex(1, 0, 0, dst.rexB()));
            try self.buf.append(self.allocator, 0x81); // AND r/m64, imm32
            // ModRM: mod=11 (register), reg=4 (/4 for AND), rm=dst
            try self.buf.append(self.allocator, modRM(0b11, 4, dst.enc()));
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }

        /// OR reg, reg
        pub fn orRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x09); // OR r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// XOR reg, reg
        pub fn xorRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, src, dst);
            try self.buf.append(self.allocator, 0x31); // XOR r/m, r
            try self.buf.append(self.allocator, modRM(0b11, src.enc(), dst.enc()));
        }

        /// XOR reg, imm8 (XOR with sign-extended 8-bit immediate)
        pub fn xorRegImm8(self: *Self, width: RegisterWidth, reg: GeneralReg, imm: i8) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0x83); // XOR r/m, imm8
            try self.buf.append(self.allocator, modRM(0b11, 6, reg.enc())); // /6 = XOR
            try self.buf.append(self.allocator, @bitCast(imm));
        }

        /// SHL reg, imm8 (shift left)
        pub fn shlRegImm8(self: *Self, width: RegisterWidth, reg: GeneralReg, imm: u8) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            if (imm == 1) {
                try self.buf.append(self.allocator, 0xD1);
                try self.buf.append(self.allocator, modRM(0b11, 4, reg.enc()));
            } else {
                try self.buf.append(self.allocator, 0xC1);
                try self.buf.append(self.allocator, modRM(0b11, 4, reg.enc()));
                try self.buf.append(self.allocator, imm);
            }
        }

        /// SHR reg, imm8 (logical shift right)
        pub fn shrRegImm8(self: *Self, width: RegisterWidth, reg: GeneralReg, imm: u8) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            if (imm == 1) {
                try self.buf.append(self.allocator, 0xD1);
                try self.buf.append(self.allocator, modRM(0b11, 5, reg.enc()));
            } else {
                try self.buf.append(self.allocator, 0xC1);
                try self.buf.append(self.allocator, modRM(0b11, 5, reg.enc()));
                try self.buf.append(self.allocator, imm);
            }
        }

        /// SAR reg, imm8 (arithmetic shift right)
        pub fn sarRegImm8(self: *Self, width: RegisterWidth, reg: GeneralReg, imm: u8) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            if (imm == 1) {
                try self.buf.append(self.allocator, 0xD1);
                try self.buf.append(self.allocator, modRM(0b11, 7, reg.enc()));
            } else {
                try self.buf.append(self.allocator, 0xC1);
                try self.buf.append(self.allocator, modRM(0b11, 7, reg.enc()));
                try self.buf.append(self.allocator, imm);
            }
        }

        /// SHL reg, CL (shift left by CL register)
        pub fn shlRegCl(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xD3);
            try self.buf.append(self.allocator, modRM(0b11, 4, reg.enc()));
        }

        /// SHR reg, CL (logical shift right by CL register)
        pub fn shrRegCl(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xD3);
            try self.buf.append(self.allocator, modRM(0b11, 5, reg.enc()));
        }

        /// SAR reg, CL (arithmetic shift right by CL register)
        pub fn sarRegCl(self: *Self, width: RegisterWidth, reg: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, reg);
            try self.buf.append(self.allocator, 0xD3);
            try self.buf.append(self.allocator, modRM(0b11, 7, reg.enc()));
        }

        // Control flow instructions

        /// RET (return from procedure)
        pub fn ret(self: *Self) !void {
            try self.buf.append(self.allocator, 0xC3);
        }

        /// PUSH r64 (push register onto stack)
        pub fn push(self: *Self, reg: GeneralReg) !void {
            // PUSH r64: 50+rd (or REX.B 50+rd for R8-R15)
            if (reg.requiresRex()) {
                try self.buf.append(self.allocator, 0x41); // REX.B
            }
            try self.buf.append(self.allocator, 0x50 + @as(u8, reg.enc()));
        }

        /// POP r64 (pop register from stack)
        pub fn pop(self: *Self, reg: GeneralReg) !void {
            // POP r64: 58+rd (or REX.B 58+rd for R8-R15)
            if (reg.requiresRex()) {
                try self.buf.append(self.allocator, 0x41); // REX.B
            }
            try self.buf.append(self.allocator, 0x58 + @as(u8, reg.enc()));
        }

        /// JMP rel32 (unconditional jump with 32-bit offset)
        /// This is an alias for jmpRel32 for code compatibility
        pub fn jmp(self: *Self, rel: i32) !void {
            try self.jmpRel32(rel);
        }

        /// CALL rel32 (relative call)
        pub fn callRel32(self: *Self, rel: i32) !void {
            try self.buf.append(self.allocator, 0xE8);
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(rel)));
        }

        /// CALL with relocation (address resolved at link time)
        pub fn callRelocated(self: *Self, name: []const u8) !void {
            const call_offset = self.offset();
            try self.buf.append(self.allocator, 0xE8);
            try self.buf.appendSlice(self.allocator, &[4]u8{ 0, 0, 0, 0 }); // Placeholder
            try self.relocs.append(self.allocator, .{
                .linked_function = .{
                    .offset = call_offset + 1, // Offset of the rel32 operand
                    .name = name,
                },
            });
        }

        /// CALL r64 (call to address in register)
        pub fn callReg(self: *Self, reg: GeneralReg) !void {
            // CALL r64: FF /2
            // REX prefix needed for R8-R15
            if (reg.requiresRex()) {
                try self.buf.append(self.allocator, 0x41); // REX.B
            }
            try self.buf.append(self.allocator, 0xFF);
            try self.buf.append(self.allocator, modRM(0b11, 2, reg.enc())); // /2 = CALL
        }

        /// CALL rel32 (relative call)
        pub fn call(self: *Self, rel: i32) !void {
            try self.callRel32(rel);
        }

        /// JMP rel32 (relative jump)
        pub fn jmpRel32(self: *Self, rel: i32) !void {
            try self.buf.append(self.allocator, 0xE9);
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(rel)));
        }

        /// JMP rel8 (short relative jump)
        pub fn jmpRel8(self: *Self, rel: i8) !void {
            try self.buf.append(self.allocator, 0xEB);
            try self.buf.append(self.allocator, @bitCast(rel));
        }

        /// Condition codes for conditional jumps and moves
        pub const Condition = enum(u4) {
            overflow = 0x0, // O
            not_overflow = 0x1, // NO
            below = 0x2, // B, NAE, C (unsigned <)
            above_or_equal = 0x3, // AE, NB, NC (unsigned >=)
            equal = 0x4, // E, Z
            not_equal = 0x5, // NE, NZ
            below_or_equal = 0x6, // BE, NA (unsigned <=)
            above = 0x7, // A, NBE (unsigned >)
            sign = 0x8, // S (negative)
            not_sign = 0x9, // NS (non-negative)
            parity_even = 0xA, // P, PE
            parity_odd = 0xB, // NP, PO
            less = 0xC, // L, NGE (signed <)
            greater_or_equal = 0xD, // GE, NL (signed >=)
            less_or_equal = 0xE, // LE, NG (signed <=)
            greater = 0xF, // G, NLE (signed >)

            pub fn invert(self: Condition) Condition {
                return @enumFromInt(@intFromEnum(self) ^ 1);
            }
        };

        /// Jcc rel32 (conditional jump near)
        pub fn jccRel32(self: *Self, cond: Condition, rel: i32) !void {
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x80 + @as(u8, @intFromEnum(cond)));
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(rel)));
        }

        /// Jcc rel8 (conditional jump short)
        pub fn jccRel8(self: *Self, cond: Condition, rel: i8) !void {
            try self.buf.append(self.allocator, 0x70 + @as(u8, @intFromEnum(cond)));
            try self.buf.append(self.allocator, @bitCast(rel));
        }

        /// JNE rel32 (jump if not equal)
        pub fn jne(self: *Self, rel: i32) !void {
            try self.jccRel32(.not_equal, rel);
        }

        /// JAE rel32 (jump if above or equal, unsigned >=)
        pub fn jae(self: *Self, rel: i32) !void {
            try self.jccRel32(.above_or_equal, rel);
        }

        /// CMOVcc reg, reg (conditional move)
        pub fn cmovcc(self: *Self, cond: Condition, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x40 + @as(u8, @intFromEnum(cond)));
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// SETcc reg (set byte based on condition)
        pub fn setcc(self: *Self, cond: Condition, reg: GeneralReg) !void {
            if (reg.requiresRex()) {
                try self.buf.append(self.allocator, rex(0, 0, 0, reg.rexB()));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x90 + @as(u8, @intFromEnum(cond)));
            try self.buf.append(self.allocator, modRM(0b11, 0, reg.enc()));
        }

        /// UD2 - Undefined instruction (generates SIGILL, used as trap)
        pub fn ud2(self: *Self) !void {
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x0B);
        }

        // Memory instructions

        /// MOV reg, [base + disp32] (load from memory)
        pub fn movRegMem(self: *Self, width: RegisterWidth, dst: GeneralReg, base: GeneralReg, disp: i32) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, dst, base);
            try self.buf.append(self.allocator, 0x8B); // MOV r, r/m

            // Handle special cases for RSP/R12 (need SIB byte) and RBP/R13 (need disp)
            const base_enc = base.enc();
            if (base_enc == 4) {
                // RSP/R12 - needs SIB byte
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24); // SIB: base=RSP, index=none, scale=1
            } else {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOV [base + disp32], reg (store to memory)
        pub fn movMemReg(self: *Self, width: RegisterWidth, base: GeneralReg, disp: i32, src: GeneralReg) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, src, base);
            try self.buf.append(self.allocator, 0x89); // MOV r/m, r

            const base_enc = base.enc();
            if (base_enc == 4) {
                // RSP/R12 - needs SIB byte
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24); // SIB
            } else {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOV [base + disp32], imm32 (store immediate to memory)
        pub fn movMemImm32(self: *Self, width: RegisterWidth, base: GeneralReg, disp: i32, imm: i32) !void {
            if (width.requiresSizeOverride()) {
                try self.buf.append(self.allocator, 0x66);
            }
            try self.emitRex(width, null, base);
            try self.buf.append(self.allocator, 0xC7); // MOV r/m, imm32

            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, 0, 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, 0, base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(imm)));
        }

        /// MOVZX r32, BYTE [base + disp32] (zero-extend byte to 64 bits)
        /// 32-bit result auto-zero-extends to 64 bits on x86_64; no REX.W needed.
        pub fn movzxBRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, disp: i32) !void {
            // REX prefix only for extended registers (R8-R15), never REX.W
            const r: u1 = dst.rexR();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0xB6); // MOVZX r32, r/m8

            const base_enc = base.enc();
            if (base_enc == 4) {
                // RSP/R12 - needs SIB byte
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOVZX r32, WORD [base + disp32] (zero-extend word to 64 bits)
        /// 32-bit result auto-zero-extends to 64 bits on x86_64; no REX.W needed.
        pub fn movzxWRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, disp: i32) !void {
            // REX prefix only for extended registers (R8-R15), never REX.W
            const r: u1 = dst.rexR();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0xB7); // MOVZX r32, r/m16

            const base_enc = base.enc();
            if (base_enc == 4) {
                // RSP/R12 - needs SIB byte
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// LEA reg, [base + disp32] (load effective address)
        /// LEA reg, [RIP + disp32] — compute PC-relative address
        /// disp is relative to the end of this instruction (7 bytes total).
        pub fn leaRegRipRel(self: *Self, dst: GeneralReg, disp: i32) !void {
            try self.emitRex(.w64, dst, .RBP); // REX.W prefix (RBP enc = 5, doesn't matter for mod=00 rm=101)
            try self.buf.append(self.allocator, 0x8D); // LEA
            // ModRM: mod=00, reg=dst, rm=101 (RIP-relative)
            try self.buf.append(self.allocator, modRM(0b00, dst.enc(), 0b101));
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        pub fn leaRegMem(self: *Self, dst: GeneralReg, base: GeneralReg, disp: i32) !void {
            try self.emitRex(.w64, dst, base);
            try self.buf.append(self.allocator, 0x8D); // LEA

            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        // Stack instructions

        /// PUSH reg
        pub fn pushReg(self: *Self, reg: GeneralReg) !void {
            if (reg.requiresRex()) {
                try self.buf.append(self.allocator, rex(0, 0, 0, reg.rexB()));
            }
            try self.buf.append(self.allocator, 0x50 + @as(u8, reg.enc()));
        }

        /// POP reg
        pub fn popReg(self: *Self, reg: GeneralReg) !void {
            if (reg.requiresRex()) {
                try self.buf.append(self.allocator, rex(0, 0, 0, reg.rexB()));
            }
            try self.buf.append(self.allocator, 0x58 + @as(u8, reg.enc()));
        }

        // Floating-point instructions (SSE/SSE2)

        /// Emit REX prefix for floating-point operations
        fn emitFloatRex(self: *Self, reg: ?FloatReg, rm: ?FloatReg) !void {
            const r: u1 = if (reg) |r_reg| r_reg.rexB() else 0;
            const b: u1 = if (rm) |rm_reg| rm_reg.rexB() else 0;
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
        }

        /// MOVSD xmm, xmm (move scalar double)
        pub fn movsdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2); // REPNE prefix for MOVSD
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x10); // MOVSD xmm, xmm/m64
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// MOVSS xmm, xmm (move scalar single)
        pub fn movssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3); // REPE prefix for MOVSS
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x10);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// MOVSD xmm, [base + disp32] (load scalar double from memory)
        pub fn movsdRegMem(self: *Self, dst: FloatReg, base: GeneralReg, disp: i32) !void {
            try self.buf.append(self.allocator, 0xF2);
            const r: u1 = dst.rexB();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x10);

            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOVSS xmm, [base + disp32] (load scalar single from memory)
        pub fn movssRegMem(self: *Self, dst: FloatReg, base: GeneralReg, disp: i32) !void {
            try self.buf.append(self.allocator, 0xF3);
            const r: u1 = dst.rexB();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x10);

            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, dst.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOVSD [base + disp32], xmm (store scalar double to memory)
        pub fn movsdMemReg(self: *Self, base: GeneralReg, disp: i32, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            const r: u1 = src.rexB();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x11); // MOVSD m64, xmm

            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOVSS [base + disp32], xmm (store scalar single to memory)
        pub fn movssMemReg(self: *Self, base: GeneralReg, disp: i32, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            const r: u1 = src.rexB();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x11); // MOVSS m32, xmm

            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// MOVDQU [base + disp32], xmm (store 128-bit unaligned from XMM to memory)
        /// Used for storing i128 return values from XMM0
        /// IMPORTANT: Uses F3 prefix (MOVDQU) not 66 prefix (MOVDQA) to avoid alignment faults
        pub fn movdquMemReg(self: *Self, base: GeneralReg, disp: i32, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3); // F3 prefix for MOVDQU (unaligned)
            const r: u1 = src.rexB();
            const b: u1 = base.rexB();
            if (r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(0, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x7F); // MOVDQU m128, xmm
            const base_enc = base.enc();
            if (base_enc == 4) {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), 0b100));
                try self.buf.append(self.allocator, 0x24);
            } else {
                try self.buf.append(self.allocator, modRM(0b10, src.enc(), base_enc));
            }
            try self.buf.appendSlice(self.allocator, &@as([4]u8, @bitCast(disp)));
        }

        /// ADDSD xmm, xmm (add scalar double)
        pub fn addsdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x58);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// ADDSS xmm, xmm (add scalar single)
        pub fn addssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x58);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// SUBSD xmm, xmm (subtract scalar double)
        pub fn subsdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x5C);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// SUBSS xmm, xmm (subtract scalar single)
        pub fn subssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x5C);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// MULSD xmm, xmm (multiply scalar double)
        pub fn mulsdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x59);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// MULSS xmm, xmm (multiply scalar single)
        pub fn mulssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x59);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// DIVSD xmm, xmm (divide scalar double)
        pub fn divsdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x5E);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// DIVSS xmm, xmm (divide scalar single)
        pub fn divssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x5E);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// SQRTSD xmm, xmm (square root scalar double)
        pub fn sqrtsdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x51);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// SQRTSS xmm, xmm (square root scalar single)
        pub fn sqrtssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x51);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// UCOMISD xmm, xmm (unordered compare scalar double)
        pub fn ucomisdRegReg(self: *Self, a: FloatReg, b: FloatReg) !void {
            try self.buf.append(self.allocator, 0x66); // Operand size prefix
            try self.emitFloatRex(a, b);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x2E);
            try self.buf.append(self.allocator, modRM(0b11, a.enc(), b.enc()));
        }

        /// UCOMISS xmm, xmm (unordered compare scalar single)
        pub fn ucomissRegReg(self: *Self, a: FloatReg, b: FloatReg) !void {
            try self.emitFloatRex(a, b);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x2E);
            try self.buf.append(self.allocator, modRM(0b11, a.enc(), b.enc()));
        }

        /// XORPD xmm, xmm (XOR packed double - used for zeroing)
        pub fn xorpdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0x66);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x57);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// XORPS xmm, xmm (XOR packed single - used for zeroing)
        pub fn xorpsRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x57);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// CVTSI2SD xmm, reg (convert integer to scalar double)
        pub fn cvtsi2sdRegReg(self: *Self, width: RegisterWidth, dst: FloatReg, src: GeneralReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            const w: u1 = if (width.requiresRexW()) 1 else 0;
            const r: u1 = dst.rexB();
            const b: u1 = src.rexB();
            if (w == 1 or r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(w, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x2A);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// CVTSI2SS xmm, reg (convert integer to scalar single)
        pub fn cvtsi2ssRegReg(self: *Self, width: RegisterWidth, dst: FloatReg, src: GeneralReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            const w: u1 = if (width.requiresRexW()) 1 else 0;
            const r: u1 = dst.rexB();
            const b: u1 = src.rexB();
            if (w == 1 or r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(w, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x2A);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// CVTTSD2SI reg, xmm (convert scalar double to integer with truncation)
        pub fn cvttsd2siRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            const w: u1 = if (width.requiresRexW()) 1 else 0;
            const r: u1 = dst.rexB();
            const b: u1 = src.rexB();
            if (w == 1 or r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(w, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x2C);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// CVTTSS2SI reg, xmm (convert scalar single to integer with truncation)
        pub fn cvttss2siRegReg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            const w: u1 = if (width.requiresRexW()) 1 else 0;
            const r: u1 = dst.rexB();
            const b: u1 = src.rexB();
            if (w == 1 or r == 1 or b == 1) {
                try self.buf.append(self.allocator, rex(w, r, 0, b));
            }
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x2C);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// CVTSS2SD xmm, xmm (convert scalar single to scalar double)
        pub fn cvtss2sdRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF3);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x5A);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }

        /// CVTSD2SS xmm, xmm (convert scalar double to scalar single)
        pub fn cvtsd2ssRegReg(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.buf.append(self.allocator, 0xF2);
            try self.emitFloatRex(dst, src);
            try self.buf.append(self.allocator, 0x0F);
            try self.buf.append(self.allocator, 0x5A);
            try self.buf.append(self.allocator, modRM(0b11, dst.enc(), src.enc()));
        }
    }; // end of struct returned by Emit
}

// Target-specific Emit types for cross-platform testing
const LinuxEmit = Emit(.x64linux);
const WinEmit = Emit(.x64win);
const MacEmit = Emit(.x64mac);

// Tests

test "mov reg, reg" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // mov rax, rbx (48 89 D8)
    try asm_buf.movRegReg(.w64, .RAX, .RBX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x89, 0xD8 }, asm_buf.buf.items);
}

test "mov r8, r9" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // mov r8, r9 (4D 89 C8)
    try asm_buf.movRegReg(.w64, .R8, .R9);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x4D, 0x89, 0xC8 }, asm_buf.buf.items);
}

test "mov rax, imm64" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // movabs rax, 0x123456789ABCDEF0
    try asm_buf.movRegImm64(.RAX, 0x123456789ABCDEF0);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x48, 0xB8, // REX.W + MOV rax, imm64
        0xF0, 0xDE, 0xBC, 0x9A, 0x78, 0x56, 0x34, 0x12, // Little-endian immediate
    }, asm_buf.buf.items);
}

test "ret" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    try asm_buf.ret();
    try std.testing.expectEqualSlices(u8, &[_]u8{0xC3}, asm_buf.buf.items);
}

test "push and pop" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    try asm_buf.pushReg(.RBP);
    try asm_buf.pushReg(.R12);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x55, 0x41, 0x54 }, asm_buf.buf.items);
}

test "add reg, imm32" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // add rax, 0x12345678 (special short form: 48 05 + imm32)
    try asm_buf.addRegImm32(.w64, .RAX, 0x12345678);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x05, 0x78, 0x56, 0x34, 0x12 }, asm_buf.buf.items);
}

test "cmp reg, reg" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // cmp rax, rbx (48 39 D8)
    try asm_buf.cmpRegReg(.w64, .RAX, .RBX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x39, 0xD8 }, asm_buf.buf.items);
}

test "conditional jump" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // je +0x10 (short form: 74 10)
    try asm_buf.jccRel8(.equal, 0x10);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x74, 0x10 }, asm_buf.buf.items);
}

test "addsd xmm, xmm" {
    var asm_buf = LinuxEmit.init(std.testing.allocator);
    defer asm_buf.deinit();

    // addsd xmm0, xmm1 (F2 0F 58 C1)
    try asm_buf.addsdRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xF2, 0x0F, 0x58, 0xC1 }, asm_buf.buf.items);
}

test "condition invert" {
    try std.testing.expectEqual(LinuxEmit.Condition.not_equal, LinuxEmit.Condition.equal.invert());
    try std.testing.expectEqual(LinuxEmit.Condition.equal, LinuxEmit.Condition.not_equal.invert());
    try std.testing.expectEqual(LinuxEmit.Condition.greater_or_equal, LinuxEmit.Condition.less.invert());
    try std.testing.expectEqual(LinuxEmit.Condition.less, LinuxEmit.Condition.greater_or_equal.invert());
}

const ALL_GENERAL_REGS = [_]LinuxEmit.GeneralReg{
    .RAX, .RCX, .RDX, .RBX, .RSP, .RBP, .RSI, .RDI,
    .R8,  .R9,  .R10, .R11, .R12, .R13, .R14, .R15,
};

const SAFE_GENERAL_REGS = [_]LinuxEmit.GeneralReg{
    .RAX, .RCX, .RDX, .RBX, .RSI, .RDI,
    .R8,  .R9,  .R10, .R11, .R12, .R13,
    .R14, .R15,
};

const ALL_FLOAT_REGS = [_]LinuxEmit.FloatReg{
    .XMM0, .XMM1, .XMM2,  .XMM3,  .XMM4,  .XMM5,  .XMM6,  .XMM7,
    .XMM8, .XMM9, .XMM10, .XMM11, .XMM12, .XMM13, .XMM14, .XMM15,
};

test "mov reg64, reg64 - all register combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (ALL_GENERAL_REGS) |dst| {
        for (ALL_GENERAL_REGS) |src| {
            emit.buf.clearRetainingCapacity();
            try emit.movRegReg(.w64, dst, src);
            try std.testing.expect(emit.buf.items.len >= 2);
        }
    }
}

test "add reg64, reg64 - all combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (SAFE_GENERAL_REGS) |dst| {
        for (SAFE_GENERAL_REGS) |src| {
            emit.buf.clearRetainingCapacity();
            try emit.addRegReg(.w64, dst, src);
            const opcode_idx: usize = if (emit.buf.items[0] >= 0x40 and emit.buf.items[0] <= 0x4F) 1 else 0;
            try std.testing.expectEqual(@as(u8, 0x01), emit.buf.items[opcode_idx]);
        }
    }
}

test "sub reg64, reg64 - all combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (SAFE_GENERAL_REGS) |dst| {
        for (SAFE_GENERAL_REGS) |src| {
            emit.buf.clearRetainingCapacity();
            try emit.subRegReg(.w64, dst, src);
            const opcode_idx: usize = if (emit.buf.items[0] >= 0x40 and emit.buf.items[0] <= 0x4F) 1 else 0;
            try std.testing.expectEqual(@as(u8, 0x29), emit.buf.items[opcode_idx]);
        }
    }
}

test "imul reg64, reg64 - all combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (SAFE_GENERAL_REGS) |dst| {
        for (SAFE_GENERAL_REGS) |src| {
            emit.buf.clearRetainingCapacity();
            try emit.imulRegReg(.w64, dst, src);
            var found_0f = false;
            for (emit.buf.items, 0..) |byte, i| {
                if (byte == 0x0F and i + 1 < emit.buf.items.len) {
                    try std.testing.expectEqual(@as(u8, 0xAF), emit.buf.items[i + 1]);
                    found_0f = true;
                    break;
                }
            }
            try std.testing.expect(found_0f);
        }
    }
}

test "cmp reg64, reg64 - all combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (SAFE_GENERAL_REGS) |a| {
        for (SAFE_GENERAL_REGS) |b| {
            emit.buf.clearRetainingCapacity();
            try emit.cmpRegReg(.w64, a, b);
            const opcode_idx: usize = if (emit.buf.items[0] >= 0x40 and emit.buf.items[0] <= 0x4F) 1 else 0;
            try std.testing.expectEqual(@as(u8, 0x39), emit.buf.items[opcode_idx]);
        }
    }
}

test "movabs reg64, imm64 - all registers" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    const test_imm: i64 = 0x123456789ABCDEF0;
    for (ALL_GENERAL_REGS) |reg| {
        emit.buf.clearRetainingCapacity();
        try emit.movRegImm64(reg, test_imm);
        try std.testing.expectEqual(@as(usize, 10), emit.buf.items.len);
    }
}

test "push/pop - all registers" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (ALL_GENERAL_REGS) |reg| {
        emit.buf.clearRetainingCapacity();
        try emit.pushReg(reg);
        if (reg.requiresRex()) {
            try std.testing.expect(emit.buf.items.len == 2);
        } else {
            try std.testing.expect(emit.buf.items.len == 1);
        }
    }

    for (ALL_GENERAL_REGS) |reg| {
        emit.buf.clearRetainingCapacity();
        try emit.popReg(reg);
        if (reg.requiresRex()) {
            try std.testing.expect(emit.buf.items.len == 2);
        } else {
            try std.testing.expect(emit.buf.items.len == 1);
        }
    }
}

test "jcc rel8 - all conditions" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    const conditions = [_]LinuxEmit.Condition{
        .overflow, .not_overflow,     .below,          .above_or_equal,
        .equal,    .not_equal,        .below_or_equal, .above,
        .sign,     .not_sign,         .parity_even,    .parity_odd,
        .less,     .greater_or_equal, .less_or_equal,  .greater,
    };

    for (conditions, 0..) |cond, i| {
        emit.buf.clearRetainingCapacity();
        try emit.jccRel8(cond, 0x10);
        try std.testing.expectEqual(@as(u8, 0x70 + @as(u8, @intCast(i))), emit.buf.items[0]);
        try std.testing.expectEqual(@as(u8, 0x10), emit.buf.items[1]);
    }
}

test "jcc rel32 - all conditions" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    const conditions = [_]LinuxEmit.Condition{
        .overflow, .not_overflow,     .below,          .above_or_equal,
        .equal,    .not_equal,        .below_or_equal, .above,
        .sign,     .not_sign,         .parity_even,    .parity_odd,
        .less,     .greater_or_equal, .less_or_equal,  .greater,
    };

    for (conditions, 0..) |cond, i| {
        emit.buf.clearRetainingCapacity();
        try emit.jccRel32(cond, 0x12345678);
        try std.testing.expectEqual(@as(u8, 0x0F), emit.buf.items[0]);
        try std.testing.expectEqual(@as(u8, 0x80 + @as(u8, @intCast(i))), emit.buf.items[1]);
    }
}

test "movsd xmm, xmm - all combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (ALL_FLOAT_REGS) |dst| {
        for (ALL_FLOAT_REGS) |src| {
            emit.buf.clearRetainingCapacity();
            try emit.movsdRegReg(dst, src);
            try std.testing.expectEqual(@as(u8, 0xF2), emit.buf.items[0]);
        }
    }
}

test "addsd xmm, xmm - all combinations" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    for (ALL_FLOAT_REGS) |dst| {
        for (ALL_FLOAT_REGS) |src| {
            emit.buf.clearRetainingCapacity();
            try emit.addsdRegReg(dst, src);
            try std.testing.expectEqual(@as(u8, 0xF2), emit.buf.items[0]);
        }
    }
}

test "subsd/mulsd/divsd xmm, xmm encoding" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.subsdRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xF2, 0x0F, 0x5C, 0xC1 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.mulsdRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xF2, 0x0F, 0x59, 0xC1 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.divsdRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xF2, 0x0F, 0x5E, 0xC1 }, emit.buf.items);
}

test "bitwise ops - and/or/xor" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.andRegReg(.w64, .RAX, .RBX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x21, 0xD8 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.orRegReg(.w64, .RAX, .RBX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x09, 0xD8 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.xorRegReg(.w64, .RAX, .RBX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x31, 0xD8 }, emit.buf.items);
}

test "shift ops - shl/shr/sar" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.shlRegImm8(.w64, .RAX, 1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0xD1, 0xE0 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.shlRegImm8(.w64, .RAX, 4);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0xC1, 0xE0, 0x04 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.shrRegImm8(.w64, .RAX, 4);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0xC1, 0xE8, 0x04 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.sarRegImm8(.w64, .RAX, 4);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0xC1, 0xF8, 0x04 }, emit.buf.items);
}

test "neg and not" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.negReg(.w64, .RAX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0xF7, 0xD8 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.notReg(.w64, .RAX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0xF7, 0xD0 }, emit.buf.items);
}

test "xor zeroing idiom" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.xorRegReg(.w32, .RAX, .RAX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x31, 0xC0 }, emit.buf.items);
}

test "extended registers require REX" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    const extended_regs = [_]LinuxEmit.GeneralReg{ .R8, .R9, .R10, .R11, .R12, .R13, .R14, .R15 };
    for (extended_regs) |reg| {
        emit.buf.clearRetainingCapacity();
        try emit.movRegReg(.w64, reg, .RAX);
        try std.testing.expect(emit.buf.items[0] >= 0x40 and emit.buf.items[0] <= 0x4F);
    }
}

test "float conversion ops" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.cvtss2sdRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xF3, 0x0F, 0x5A, 0xC1 }, emit.buf.items);

    emit.buf.clearRetainingCapacity();
    try emit.cvtsd2ssRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xF2, 0x0F, 0x5A, 0xC1 }, emit.buf.items);
}

test "ucomisd comparison" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.ucomisdRegReg(.XMM0, .XMM1);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x66, 0x0F, 0x2E, 0xC1 }, emit.buf.items);
}

test "xorpd zeroing" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.xorpdRegReg(.XMM0, .XMM0);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x66, 0x0F, 0x57, 0xC0 }, emit.buf.items);
}

test "function prologue sequence" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    try emit.pushReg(.RBP);
    try emit.movRegReg(.w64, .RBP, .RSP);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x55, 0x48, 0x89, 0xE5 }, emit.buf.items);
}

test "movRegMem - load from [rbp-144]" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov rax, [rbp-144]
    // REX.W (48) + opcode (8B) + ModRM (85 = mod:10 reg:000 rm:101) + disp32 (-144 = 0xFFFFFF70)
    try emit.movRegMem(.w64, .RAX, .RBP, -144);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x8B, 0x85, 0x70, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "movRegMem - load from [rsp+32] requires SIB byte" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov rcx, [rsp+32]
    // RSP as base requires SIB byte (0x24 = scale:00 index:100 base:100)
    // REX.W (48) + opcode (8B) + ModRM (8C = mod:10 reg:001 rm:100) + SIB (24) + disp32 (32)
    try emit.movRegMem(.w64, .RCX, .RSP, 32);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x8B, 0x8C, 0x24, 0x20, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "movRegMem - load from [r12+offset] requires SIB byte" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov rax, [r12+16]
    // R12 has same encoding as RSP (100), needs SIB byte
    // REX.WB (49) + opcode (8B) + ModRM (84 = mod:10 reg:000 rm:100) + SIB (24) + disp32 (16)
    try emit.movRegMem(.w64, .RAX, .R12, 16);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x49, 0x8B, 0x84, 0x24, 0x10, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "movMemReg - store to [rbp-8]" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov [rbp-8], rax
    // REX.W (48) + opcode (89) + ModRM (85 = mod:10 reg:000 rm:101) + disp32 (-8)
    try emit.movMemReg(.w64, .RBP, -8, .RAX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x89, 0x85, 0xF8, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "movMemReg - store to [rsp+32] requires SIB byte" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov [rsp+32], r11
    // REX.WR (4C) + opcode (89) + ModRM (9C = mod:10 reg:011 rm:100) + SIB (24) + disp32 (32)
    try emit.movMemReg(.w64, .RSP, 32, .R11);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x4C, 0x89, 0x9C, 0x24, 0x20, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "leaRegMem - lea rax, [rbp-32]" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // lea rax, [rbp-32]
    // REX.W (48) + opcode (8D) + ModRM (85 = mod:10 reg:000 rm:101) + disp32 (-32)
    try emit.leaRegMem(.RAX, .RBP, -32);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x8D, 0x85, 0xE0, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "leaRegMem - lea rcx, [rsp+64] requires SIB" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // lea rcx, [rsp+64]
    // REX.W (48) + opcode (8D) + ModRM (8C = mod:10 reg:001 rm:100) + SIB (24) + disp32 (64)
    try emit.leaRegMem(.RCX, .RSP, 64);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x8D, 0x8C, 0x24, 0x40, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "subRegImm32 - sub rsp, 80 (stack allocation)" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // sub rsp, 80
    // REX.W (48) + opcode (81) + ModRM (EC = mod:11 reg:101 rm:100) + imm32 (80)
    try emit.subRegImm32(.w64, .RSP, 80);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x81, 0xEC, 0x50, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "addRegImm32 - add rsp, 80 (stack deallocation)" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // add rsp, 80
    // REX.W (48) + opcode (81) + ModRM (C4 = mod:11 reg:000 rm:100) + imm32 (80)
    try emit.addRegImm32(.w64, .RSP, 80);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x81, 0xC4, 0x50, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "callReg - call r11" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // call r11
    // REX.B (41) + opcode (FF) + ModRM (D3 = mod:11 reg:010 rm:011)
    try emit.callReg(.R11);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x41, 0xFF, 0xD3 }, emit.buf.items);
}

test "callReg - call rax (no REX needed)" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // call rax
    // opcode (FF) + ModRM (D0 = mod:11 reg:010 rm:000)
    try emit.callReg(.RAX);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xFF, 0xD0 }, emit.buf.items);
}

test "pushReg - push r12" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // push r12
    // REX.B (41) + opcode (54 = 50 + 4)
    try emit.pushReg(.R12);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x41, 0x54 }, emit.buf.items);
}

test "popReg - pop r12" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // pop r12
    // REX.B (41) + opcode (5C = 58 + 4)
    try emit.popReg(.R12);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x41, 0x5C }, emit.buf.items);
}

test "movRegImm64 - movabs r11, address" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // movabs r11, 0x00007FFF12345678
    // REX.WB (49) + opcode (BB = B8 + 3) + imm64
    try emit.movRegImm64(.R11, 0x00007FFF12345678);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x49, 0xBB, // REX.WB + MOV r11, imm64
        0x78, 0x56, 0x34, 0x12, 0xFF, 0x7F, 0x00, 0x00, // Little-endian immediate
    }, emit.buf.items);
}

test "Windows x64 call sequence - shadow space + call" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // Typical Windows x64 call setup:
    // sub rsp, 32      ; allocate shadow space
    // mov rcx, rdi     ; first arg (example)
    // call r11
    // add rsp, 32      ; deallocate shadow space

    try emit.subRegImm32(.w64, .RSP, 32);
    try emit.movRegReg(.w64, .RCX, .RDI);
    try emit.callReg(.R11);
    try emit.addRegImm32(.w64, .RSP, 32);

    // Verify total sequence length
    // sub rsp, 32: 7 bytes
    // mov rcx, rdi: 3 bytes
    // call r11: 3 bytes
    // add rsp, 32: 7 bytes
    // Total: 20 bytes
    try std.testing.expectEqual(@as(usize, 20), emit.buf.items.len);
}

test "movMemReg 32-bit width" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov [rbp-16], eax (32-bit store, no REX.W)
    try emit.movMemReg(.w32, .RBP, -16, .RAX);
    // opcode (89) + ModRM (85) + disp32 (-16)
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x89, 0x85, 0xF0, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "movRegMem 32-bit width" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // mov eax, [rbp-16] (32-bit load, no REX.W)
    try emit.movRegMem(.w32, .RAX, .RBP, -16);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x8B, 0x85, 0xF0, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "jmpRel32 encoding" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // jmp +0x12345678
    try emit.jmpRel32(0x12345678);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xE9, 0x78, 0x56, 0x34, 0x12 }, emit.buf.items);
}

test "callRel32 encoding" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // call +0x100
    try emit.callRel32(0x100);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xE8, 0x00, 0x01, 0x00, 0x00 }, emit.buf.items);
}

// Multi-target calling convention tests

test "CC constants differ between Windows and System V" {
    // Windows uses 4 param regs, System V uses 6
    try std.testing.expectEqual(@as(usize, 4), WinEmit.CC.PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 6), LinuxEmit.CC.PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 6), MacEmit.CC.PARAM_REGS.len);

    // Windows has 32-byte shadow space, System V has none
    try std.testing.expectEqual(@as(u8, 32), WinEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(u8, 0), LinuxEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(u8, 0), MacEmit.CC.SHADOW_SPACE);

    // Windows first param is RCX, System V is RDI
    try std.testing.expectEqual(Registers.GeneralReg.RCX, WinEmit.CC.PARAM_REGS[0]);
    try std.testing.expectEqual(Registers.GeneralReg.RDI, LinuxEmit.CC.PARAM_REGS[0]);
    try std.testing.expectEqual(Registers.GeneralReg.RDI, MacEmit.CC.PARAM_REGS[0]);

    // Windows uses 4 float param regs, System V uses 8
    try std.testing.expectEqual(@as(usize, 4), WinEmit.CC.FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), LinuxEmit.CC.FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), MacEmit.CC.FLOAT_PARAM_REGS.len);

    // Windows returns in RAX only, System V can use RAX+RDX
    try std.testing.expectEqual(@as(usize, 1), WinEmit.CC.RETURN_REGS.len);
    try std.testing.expectEqual(@as(usize, 2), LinuxEmit.CC.RETURN_REGS.len);
    try std.testing.expectEqual(@as(usize, 2), MacEmit.CC.RETURN_REGS.len);
}

test "CC.canPassStructByValue differs between Windows and System V" {
    // Windows: only power-of-2 sizes (1, 2, 4, 8)
    try std.testing.expect(WinEmit.CC.canPassStructByValue(1));
    try std.testing.expect(WinEmit.CC.canPassStructByValue(2));
    try std.testing.expect(WinEmit.CC.canPassStructByValue(4));
    try std.testing.expect(WinEmit.CC.canPassStructByValue(8));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(3));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(9));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(16));

    // System V: any size up to 16
    try std.testing.expect(LinuxEmit.CC.canPassStructByValue(1));
    try std.testing.expect(LinuxEmit.CC.canPassStructByValue(9));
    try std.testing.expect(LinuxEmit.CC.canPassStructByValue(16));
    try std.testing.expect(!LinuxEmit.CC.canPassStructByValue(17));
}

test "CC.passI128ByPointer differs between Windows and System V" {
    // Windows requires i128 pass-by-pointer
    try std.testing.expect(WinEmit.CC.passI128ByPointer());
    // System V passes i128 in registers (RAX+RDX)
    try std.testing.expect(!LinuxEmit.CC.passI128ByPointer());
    try std.testing.expect(!MacEmit.CC.passI128ByPointer());
}

test "CC.returnI128ByPointer differs between Windows and System V" {
    // Windows uses hidden pointer for i128 returns
    try std.testing.expect(WinEmit.CC.returnI128ByPointer());
    // System V returns i128 in RAX+RDX
    try std.testing.expect(!LinuxEmit.CC.returnI128ByPointer());
    try std.testing.expect(!MacEmit.CC.returnI128ByPointer());
}

test "movzxBRegMem - zero-extend byte from [rbp-8]" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // movzx eax, BYTE [rbp-8]
    // No REX prefix (no extended regs, no .W)
    // 0F B6 85 F8FFFFFF
    try emit.movzxBRegMem(.RAX, .RBP, -8);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x0F, 0xB6, 0x85, 0xF8, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "movzxWRegMem - zero-extend word from [rbp-16]" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // movzx eax, WORD [rbp-16]
    // 0F B7 85 F0FFFFFF
    try emit.movzxWRegMem(.RAX, .RBP, -16);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x0F, 0xB7, 0x85, 0xF0, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}

test "movzxBRegMem - [rsp+offset] requires SIB" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // movzx ecx, BYTE [rsp+32]
    // No REX (RCX and RSP are not extended)
    // 0F B6 8C 24 20000000
    try emit.movzxBRegMem(.RCX, .RSP, 32);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x0F, 0xB6, 0x8C, 0x24, 0x20, 0x00, 0x00, 0x00 }, emit.buf.items);
}

test "movzxBRegMem - extended register R8 from [rbp-4]" {
    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // movzx r8d, BYTE [rbp-4]
    // REX.R (44) + 0F B6 85 FCFFFFFF
    try emit.movzxBRegMem(.R8, .RBP, -4);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x44, 0x0F, 0xB6, 0x85, 0xFC, 0xFF, 0xFF, 0xFF }, emit.buf.items);
}
