//! Mono IR Code Generator
//!
//! This module generates native machine code from Mono IR expressions.
//! It uses the Emit.zig infrastructure for instruction encoding and
//! CodeGen.zig for register allocation.
//!
//! Pipeline position:
//! ```
//! CIR -> Mono IR Lowering -> MonoExprCodeGen -> Machine Code
//! ```
//!
//! Key properties:
//! - Uses real machine instructions via Emit.zig
//! - Proper register allocation with spilling support
//! - Handles System V ABI (x86_64/aarch64) calling convention
//! - Generates position-independent code with relocations
//! - Supports x86_64 and aarch64 architectures

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const mono = @import("mono");
const builtins = @import("builtins");

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

// Num builtin functions for 128-bit integer operations
const num_divTruncI128 = builtins.num.divTruncI128;
const num_divTruncU128 = builtins.num.divTruncU128;
const num_remTruncI128 = builtins.num.remTruncI128;
const num_remTruncU128 = builtins.num.remTruncU128;

const Relocation = @import("Relocation.zig").Relocation;
const StaticDataInterner = @import("StaticDataInterner.zig");

const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoExpr;
const MonoExprId = mono.MonoExprId;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;
const SelfRecursive = mono.SelfRecursive;
const JoinPointId = mono.JoinPointId;
const LambdaSetMember = mono.LambdaSetMember;
const LambdaSetMemberSpan = mono.LambdaSetMemberSpan;

// Control flow statement types (for two-pass compilation)
const CFStmtId = mono.CFStmtId;
const LayoutIdxSpan = mono.LayoutIdxSpan;
const MonoProc = mono.MonoProc;

const Allocator = std.mem.Allocator;

/// Code generator for Mono IR expressions
/// Generic over the architecture-specific code generator
pub fn MonoExprCodeGenFor(comptime CodeGen: type, comptime GeneralReg: type, comptime FloatReg: type, comptime Condition: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,

        /// Architecture-specific code generator with register allocation
        codegen: CodeGen,

        /// The Mono IR store containing expressions to compile
        store: *const MonoExprStore,

        /// Static data interner for string literals
        static_interner: ?*StaticDataInterner,

        /// Map from MonoSymbol to value location (register or stack slot)
        symbol_locations: std.AutoHashMap(u48, ValueLocation),

        /// Map from MonoSymbol to lambda/closure expression ID (for callable bindings)
        lambda_bindings: std.AutoHashMap(u48, MonoExprId),

        /// Map from JoinPointId to code offset (for recursive closure jumps)
        join_points: std.AutoHashMap(u32, usize),

        /// Current recursive context (for detecting recursive calls)
        /// When set, lookups of this symbol should jump to the join point instead of re-entering
        current_recursive_symbol: ?MonoSymbol,
        current_recursive_join_point: ?JoinPointId,

        /// The symbol currently being bound (during let statement processing).
        current_binding_symbol: ?MonoSymbol,

        /// Registry of compiled procedures (symbol -> CompiledProc)
        /// Used to find call targets during second pass
        proc_registry: std.AutoHashMap(u48, CompiledProc),

        /// Pending calls that need to be patched after all procedures are compiled
        pending_calls: std.ArrayList(PendingCall),

        /// Map from JoinPointId to list of jumps that target it (for patching)
        join_point_jumps: std.AutoHashMap(u32, std.ArrayList(JumpRecord)),

        /// Register where RocOps pointer is saved (for calling builtins that need it)
        roc_ops_reg: ?GeneralReg = null,

        /// Compiled procedure information for two-pass compilation.
        /// After a procedure is fully compiled (including RET), it's registered here.
        pub const CompiledProc = struct {
            /// Offset into the code buffer where this procedure starts
            code_start: usize,
            /// Offset where this procedure ends
            code_end: usize,
            /// The symbol this procedure is bound to
            name: MonoSymbol,
        };

        /// A pending call that needs to be patched after all procedures are compiled.
        pub const PendingCall = struct {
            /// Offset where the call instruction is (needs patching)
            call_site: usize,
            /// The function being called
            target_symbol: MonoSymbol,
        };

        /// Record of a jump instruction that needs patching to a join point.
        pub const JumpRecord = struct {
            /// Offset of the jump instruction
            location: usize,
        };

        /// Where a value is stored
        pub const ValueLocation = union(enum) {
            /// Value is in a general-purpose register
            general_reg: GeneralReg,
            /// Value is in a float register
            float_reg: FloatReg,
            /// Value is on the stack at given offset from frame pointer
            stack: i32,
            /// 128-bit value on the stack (16 bytes: low at offset, high at offset+8)
            stack_i128: i32,
            /// Immediate value known at compile time
            immediate_i64: i64,
            /// Immediate float value
            immediate_f64: f64,
            /// Immediate 128-bit value
            immediate_i128: i128,
        };

        /// Result of code generation
        pub const CodeResult = struct {
            /// Generated machine code
            code: []const u8,
            /// Relocations for external references
            relocations: []const Relocation,
            /// Layout of the result
            result_layout: layout.Idx,
            /// Offset from start of code where execution should begin
            /// (procedures may be compiled before the main expression)
            entry_offset: usize = 0,
        };

        /// Errors that can occur during code generation
        pub const Error = error{
            OutOfMemory,
            UnsupportedExpression,
            NoRegisterToSpill,
            InvalidLocalLocation,
            LocalNotFound,
        };

        /// Initialize the code generator
        pub fn init(
            allocator: Allocator,
            store: *const MonoExprStore,
            static_interner: ?*StaticDataInterner,
        ) Self {
            return .{
                .allocator = allocator,
                .codegen = CodeGen.init(allocator),
                .store = store,
                .static_interner = static_interner,
                .symbol_locations = std.AutoHashMap(u48, ValueLocation).init(allocator),
                .lambda_bindings = std.AutoHashMap(u48, MonoExprId).init(allocator),
                .join_points = std.AutoHashMap(u32, usize).init(allocator),
                .current_recursive_symbol = null,
                .current_recursive_join_point = null,
                .current_binding_symbol = null,
                .proc_registry = std.AutoHashMap(u48, CompiledProc).init(allocator),
                .pending_calls = std.ArrayList(PendingCall).empty,
                .join_point_jumps = std.AutoHashMap(u32, std.ArrayList(JumpRecord)).init(allocator),
            };
        }

        /// Clean up resources
        pub fn deinit(self: *Self) void {
            self.codegen.deinit();
            self.symbol_locations.deinit();
            self.lambda_bindings.deinit();
            self.join_points.deinit();
            self.proc_registry.deinit();
            self.pending_calls.deinit(self.allocator);
            // Clean up the nested ArrayLists in join_point_jumps
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.deinit(self.allocator);
            }
            self.join_point_jumps.deinit();
        }

        /// Reset the code generator for generating a new expression
        pub fn reset(self: *Self) void {
            self.codegen.reset();
            self.symbol_locations.clearRetainingCapacity();
            self.lambda_bindings.clearRetainingCapacity();
            self.join_points.clearRetainingCapacity();
            self.current_recursive_symbol = null;
            self.current_recursive_join_point = null;
            self.current_binding_symbol = null;
            self.proc_registry.clearRetainingCapacity();
            self.pending_calls.clearRetainingCapacity();
            // Clear nested ArrayLists
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.clearRetainingCapacity();
            }
            self.join_point_jumps.clearRetainingCapacity();
        }

        /// Generate code for a Mono IR expression
        ///
        /// The generated code follows the calling convention:
        /// - First arg (RDI/X0) contains the pointer to the result buffer
        /// - Second arg (RSI/X1) contains the pointer to RocOps
        /// - The function writes the result to the result buffer and returns
        pub fn generateCode(
            self: *Self,
            expr_id: MonoExprId,
            result_layout: layout.Idx,
        ) Error!CodeResult {
            // Track where the main expression code starts
            // (procedures may have been compiled before this, at the start of the buffer)
            const main_code_start = self.codegen.currentOffset();

            // Reserve argument registers so they don't get allocated for temporaries
            // X0/RDI = result pointer, X1/RSI = RocOps pointer
            self.reserveArgumentRegisters();

            // Emit prologue to save callee-saved registers we'll use (X19 for result ptr)
            try self.emitMainPrologue();

            // IMPORTANT: Save the result pointer and RocOps pointer to callee-saved registers
            // before generating code that might call procedures (which would clobber them).
            // On aarch64: save X0 to X19, X1 to X20 (callee-saved)
            // On x86_64: save RDI to RBX, RSI to R12 (callee-saved)
            const result_ptr_save_reg = if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X19
            else
                x86_64.GeneralReg.RBX;

            const roc_ops_save_reg = if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X20
            else
                x86_64.GeneralReg.R12;

            try self.emitMovRegReg(result_ptr_save_reg, if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X0
            else
                x86_64.GeneralReg.RDI);

            try self.emitMovRegReg(roc_ops_save_reg, if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X1
            else
                x86_64.GeneralReg.RSI);

            // Store RocOps save reg for use by Dec operations
            self.roc_ops_reg = roc_ops_save_reg;

            // Generate code for the expression - result ends up in a register
            const result_loc = try self.generateExpr(expr_id);

            // Store result to the saved result pointer
            try self.storeResultToSavedPtr(result_loc, result_layout, result_ptr_save_reg);

            // Emit epilogue to restore callee-saved registers and return
            try self.emitMainEpilogue();

            // Get ALL the generated code (including procedures at the start)
            // Execution will start at main_code_start via entry_offset
            const all_code = self.codegen.getCode();

            // Make a copy of the code since codegen buffer may be reused
            const code_copy = self.allocator.dupe(u8, all_code) catch return Error.OutOfMemory;

            return CodeResult{
                .code = code_copy,
                .relocations = self.codegen.relocations.items,
                .result_layout = result_layout,
                .entry_offset = main_code_start,
            };
        }

        /// Reserve argument registers so they don't get allocated for temporaries
        fn reserveArgumentRegisters(self: *Self) void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // Clear X0 and X1 from the free register mask
                // X0 = bit 0, X1 = bit 1
                self.codegen.free_general &= ~@as(u32, 0b11);
                // Reserve X19 (result pointer) and X20 (RocOps pointer)
                const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                self.codegen.callee_saved_available &= ~(x19_bit | x20_bit);
            } else {
                // Clear RDI and RSI from the free register mask
                // RDI = 7, RSI = 6
                const rdi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RDI);
                const rsi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RSI);
                self.codegen.free_general &= ~(rdi_bit | rsi_bit);
                // Reserve RBX (result pointer) and R12 (RocOps pointer)
                const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                self.codegen.callee_saved_available &= ~(rbx_bit | r12_bit);
            }
        }

        /// Generate code for an expression and return where the result is stored
        fn generateExpr(self: *Self, expr_id: MonoExprId) Error!ValueLocation {
            const expr = self.store.getExpr(expr_id);

            return switch (expr) {
                // Literals
                .i64_literal => |val| .{ .immediate_i64 = val },
                .i128_literal => |val| try self.generateI128Literal(val),
                .f64_literal => |val| .{ .immediate_f64 = val },
                .f32_literal => |val| .{ .immediate_f64 = @floatCast(val) },
                .bool_literal => |val| .{ .immediate_i64 = if (val) 1 else 0 },
                .dec_literal => |val| try self.generateI128Literal(val),

                // Lookups
                .lookup => |lookup| try self.generateLookup(lookup.symbol, lookup.layout_idx),

                // Binary operations
                .binop => |binop| try self.generateBinop(binop),

                // Unary operations
                .unary_minus => |unary| try self.generateUnaryMinus(unary),
                .unary_not => |unary| try self.generateUnaryNot(unary),

                // Control flow
                .if_then_else => |ite| try self.generateIfThenElse(ite),
                .when => |when_expr| try self.generateWhen(when_expr),

                // Blocks
                .block => |block| try self.generateBlock(block),

                // Function calls and lambdas
                .call => |call| try self.generateCall(call),
                .lambda => |lambda| try self.generateLambda(lambda),
                .closure => |closure| try self.generateClosure(closure),

                // TODO: Implement remaining expression types
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for an i128 literal
        fn generateI128Literal(_: *Self, val: i128) Error!ValueLocation {
            // Return as immediate - will be materialized when needed
            return .{ .immediate_i128 = val };
        }

        /// Generate code for a symbol lookup
        fn generateLookup(self: *Self, symbol: MonoSymbol, _: layout.Idx) Error!ValueLocation {

            // Check if we have a location for this symbol
            const symbol_key: u48 = @bitCast(symbol);
            if (self.symbol_locations.get(symbol_key)) |loc| {
                return loc;
            }

            // Symbol not found - it might be a top-level definition
            if (self.store.getSymbolDef(symbol)) |def_expr_id| {
                // Generate code for the definition
                const loc = try self.generateExpr(def_expr_id);
                // Cache the location
                try self.symbol_locations.put(symbol_key, loc);
                return loc;
            }

            return Error.LocalNotFound;
        }

        /// Generate code for a binary operation
        fn generateBinop(self: *Self, binop: anytype) Error!ValueLocation {
            // Generate code for LHS first
            var lhs_loc = try self.generateExpr(binop.lhs);

            // If LHS is in a register and RHS might involve a call (which clobbers registers),
            // we need to save LHS to the stack first. This handles cases like `n * factorial(n-1)`
            // where evaluating the RHS call would clobber the register containing n.
            if (lhs_loc == .general_reg) {
                // Check if RHS might involve a call by looking at the expression
                const rhs_expr = self.store.getExpr(binop.rhs);
                if (self.exprMightInvolveCall(rhs_expr)) {
                    // Spill LHS to stack before evaluating RHS
                    const stack_offset = try self.codegen.spillToStack(lhs_loc.general_reg);
                    lhs_loc = .{ .stack = stack_offset };

                    // If LHS is a lookup, update the symbol's location so that any
                    // subsequent lookups of the same symbol (in the RHS) will find
                    // the spilled value on the stack instead of the stale register.
                    const lhs_expr = self.store.getExpr(binop.lhs);
                    if (lhs_expr == .lookup) {
                        const symbol_key: u48 = @bitCast(lhs_expr.lookup.symbol);
                        try self.symbol_locations.put(symbol_key, .{ .stack = stack_offset });
                    }
                }
            }

            // Now evaluate RHS (safe even if it involves calls)
            const rhs_loc = try self.generateExpr(binop.rhs);

            // Determine if this is an integer or float operation
            const is_float = switch (binop.result_layout) {
                .f32, .f64 => true,
                else => false,
            };

            if (is_float) {
                return self.generateFloatBinop(binop.op, lhs_loc, rhs_loc);
            } else if (binop.result_layout == .i128 or binop.result_layout == .u128 or binop.result_layout == .dec) {
                return self.generateI128Binop(binop.op, lhs_loc, rhs_loc, binop.result_layout);
            } else {
                return self.generateIntBinop(binop.op, lhs_loc, rhs_loc, binop.result_layout);
            }
        }

        /// Check if an expression might involve a function call
        fn exprMightInvolveCall(self: *Self, expr: MonoExpr) bool {
            return switch (expr) {
                .call => true,
                .binop => |b| {
                    // Recursively check both operands
                    const lhs_expr = self.store.getExpr(b.lhs);
                    const rhs_expr = self.store.getExpr(b.rhs);
                    return self.exprMightInvolveCall(lhs_expr) or self.exprMightInvolveCall(rhs_expr);
                },
                else => false,
            };
        }

        /// Generate integer binary operation
        fn generateIntBinop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            result_layout: layout.Idx,
        ) Error!ValueLocation {
            // IMPORTANT: Load RHS first to protect its register
            // If rhs is in a register (e.g., X0 from a function call result)
            // and lhs is on the stack, loading lhs might allocate X0 and
            // clobber the rhs value. By loading rhs first, we mark its register
            // as in use so the allocator won't reuse it.
            const rhs_reg = try self.ensureInGeneralReg(rhs_loc);

            // Now load LHS into a register (safe because rhs_reg is protected)
            const lhs_reg = try self.ensureInGeneralReg(lhs_loc);

            // Allocate result register
            const result_reg = try self.codegen.allocGeneralFor(0);

            // Determine if this is an unsigned type (for division/modulo)
            const is_unsigned = switch (result_layout) {
                layout.Idx.u8, layout.Idx.u16, layout.Idx.u32, layout.Idx.u64, layout.Idx.u128 => true,
                else => false,
            };

            switch (op) {
                .add => try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSub(.w64, result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg),
                .div => {
                    if (is_unsigned) {
                        try self.codegen.emitUDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                .mod => {
                    if (is_unsigned) {
                        try self.codegen.emitUMod(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSMod(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                // Comparison operations
                .eq => {
                    try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condEqual());
                },
                .neq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condNotEqual()),
                .lt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condLess()),
                .lte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condLessOrEqual()),
                .gt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condGreater()),
                .gte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condGreaterOrEqual()),
                // Boolean operations - AND/OR two values
                // Boolean values in Roc are represented as 0 (false) or 1 (true).
                // Bitwise AND/OR work correctly for single-bit boolean values.
                .@"and" => try self.codegen.emitAnd(.w64, result_reg, lhs_reg, rhs_reg),
                .@"or" => try self.codegen.emitOr(.w64, result_reg, lhs_reg, rhs_reg),
            }

            // Free operand registers if they were temporary
            self.codegen.freeGeneral(lhs_reg);
            self.codegen.freeGeneral(rhs_reg);

            return .{ .general_reg = result_reg };
        }

        // Condition code helpers for cross-architecture support
        fn condEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .eq else .equal;
        }

        fn condNotEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .ne else .not_equal;
        }

        fn condLess() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .lt else .less;
        }

        fn condLessOrEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .le else .less_or_equal;
        }

        fn condGreater() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .gt else .greater;
        }

        fn condGreaterOrEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .ge else .greater_or_equal;
        }

        /// Generate 128-bit integer binary operation
        fn generateI128Binop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            result_layout: layout.Idx,
        ) Error!ValueLocation {
            // For 128-bit operations, we work with the values as pairs of 64-bit words
            // Low word at offset 0, high word at offset 8

            // Get low and high parts of both operands
            const lhs_parts = try self.getI128Parts(lhs_loc);
            const rhs_parts = try self.getI128Parts(rhs_loc);

            // Allocate registers for result
            const result_low = try self.codegen.allocGeneralFor(0);
            const result_high = try self.codegen.allocGeneralFor(1);

            const is_unsigned = result_layout == .u128;

            switch (op) {
                .add => {
                    // 128-bit add: low = lhs_low + rhs_low, high = lhs_high + rhs_high + carry
                    if (comptime builtin.cpu.arch == .aarch64) {
                        // ADDS sets carry flag, ADC adds with carry
                        try self.codegen.emit.addsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.adcRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: ADD sets carry, ADC uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.addRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.adcRegReg(.w64, result_high, rhs_parts.high);
                    }
                },
                .sub => {
                    // 128-bit sub: low = lhs_low - rhs_low, high = lhs_high - rhs_high - borrow
                    if (comptime builtin.cpu.arch == .aarch64) {
                        // SUBS sets borrow flag, SBC subtracts with borrow
                        try self.codegen.emit.subsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.sbcRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: SUB sets borrow, SBB uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.subRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.sbbRegReg(.w64, result_high, rhs_parts.high);
                    }
                },
                .mul => {
                    if (result_layout == .dec) {
                        // Dec multiplication: call builtin function
                        // mulSaturatedC(RocDec, RocDec) -> RocDec
                        // RocDec is extern struct { num: i128 }
                        try self.callDecMul(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit multiply: (a_lo, a_hi) * (b_lo, b_hi)
                        // result_lo = low64(a_lo * b_lo)
                        // result_hi = high64(a_lo * b_lo) + low64(a_lo * b_hi) + low64(a_hi * b_lo)

                        if (comptime builtin.cpu.arch == .aarch64) {
                            // aarch64: Use MUL for low and UMULH for high
                            // 1. a_lo * b_lo -> result_low (low), temp (high via UMULH)
                            try self.codegen.emit.mulRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                            try self.codegen.emit.umulhRegRegReg(result_high, lhs_parts.low, rhs_parts.low);

                            // 2. a_lo * b_hi -> temp1 (low part only, add to result_high)
                            const temp1 = try self.codegen.allocGeneralFor(2);
                            try self.codegen.emit.mulRegRegReg(.w64, temp1, lhs_parts.low, rhs_parts.high);
                            try self.codegen.emit.addRegRegReg(.w64, result_high, result_high, temp1);
                            self.codegen.freeGeneral(temp1);

                            // 3. a_hi * b_lo -> temp2 (low part only, add to result_high)
                            const temp2 = try self.codegen.allocGeneralFor(2);
                            try self.codegen.emit.mulRegRegReg(.w64, temp2, rhs_parts.low, lhs_parts.high);
                            try self.codegen.emit.addRegRegReg(.w64, result_high, result_high, temp2);
                            self.codegen.freeGeneral(temp2);
                        } else {
                            // x86_64: Use MUL which gives RDX:RAX = RAX * src
                            // IMPORTANT: MUL clobbers both RAX and RDX. We do 3 MUL operations.
                            // All input/output registers that are RAX or RDX will be clobbered!
                            //
                            // Usage pattern:
                            // Step 1: lhs_parts.low, rhs_parts.low -> clobbers RAX, RDX
                            // Step 2: lhs_parts.low, rhs_parts.high -> clobbers RAX, RDX
                            // Step 3: lhs_parts.high, rhs_parts.low -> clobbers RAX, RDX
                            //
                            // We must save any input in RAX/RDX before they get clobbered.

                            // Mark RAX and RDX as in-use so allocGeneralFor won't return them
                            self.codegen.markRegisterInUse(.RAX);
                            self.codegen.markRegisterInUse(.RDX);

                            // Allocate temp registers for accumulation (guaranteed not RAX/RDX)
                            const temp_low = try self.codegen.allocGeneralFor(0xFFFE);
                            const temp_high = try self.codegen.allocGeneralFor(0xFFFF);

                            // Helper to save a register if it's RAX or RDX
                            const SavedReg = struct {
                                reg: GeneralReg,
                                needs_free: bool,
                            };

                            const saveIfClobbered = struct {
                                fn f(s: *Self, reg: GeneralReg) !SavedReg {
                                    if (reg == .RAX or reg == .RDX) {
                                        const saved = try s.codegen.allocGeneralFor(0xFFFC);
                                        try s.codegen.emit.movRegReg(.w64, saved, reg);
                                        return .{ .reg = saved, .needs_free = true };
                                    }
                                    return .{ .reg = reg, .needs_free = false };
                                }
                            }.f;

                            // Save all inputs that are in RAX/RDX
                            // lhs_parts.low: used in steps 1, 2
                            const lhs_low = try saveIfClobbered(self, lhs_parts.low);
                            // lhs_parts.high: used in step 3
                            const lhs_high = try saveIfClobbered(self, lhs_parts.high);
                            // rhs_parts.low: used in steps 1, 3
                            const rhs_low = try saveIfClobbered(self, rhs_parts.low);
                            // rhs_parts.high: used in step 2
                            const rhs_high = try saveIfClobbered(self, rhs_parts.high);

                            // Restore RAX/RDX to free pool (MUL will use them)
                            self.codegen.freeGeneral(.RAX);
                            self.codegen.freeGeneral(.RDX);

                            // 1. a_lo * b_lo -> RAX (low), RDX (high)
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.movRegReg(.w64, temp_low, .RAX);
                            try self.codegen.emit.movRegReg(.w64, temp_high, .RDX);

                            // 2. a_lo * b_hi -> add low part to temp_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_high.reg);
                            try self.codegen.emit.addRegReg(.w64, temp_high, .RAX);

                            // 3. a_hi * b_lo -> add low part to temp_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_high.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.addRegReg(.w64, temp_high, .RAX);

                            // Move results to actual output registers
                            try self.codegen.emit.movRegReg(.w64, result_low, temp_low);
                            try self.codegen.emit.movRegReg(.w64, result_high, temp_high);

                            // Cleanup temp registers
                            self.codegen.freeGeneral(temp_low);
                            self.codegen.freeGeneral(temp_high);
                            if (lhs_low.needs_free) self.codegen.freeGeneral(lhs_low.reg);
                            if (lhs_high.needs_free) self.codegen.freeGeneral(lhs_high.reg);
                            if (rhs_low.needs_free) self.codegen.freeGeneral(rhs_low.reg);
                            if (rhs_high.needs_free) self.codegen.freeGeneral(rhs_high.reg);
                        }
                    }
                },
                .div => {
                    if (result_layout == .dec) {
                        // Dec division: call builtin function
                        // divC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDiv(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer division: call builtin function
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .mod => {
                    // 128-bit integer remainder: call builtin function
                    try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, true);
                },
                else => {
                    // Comparisons and boolean ops - just use low 64 bits for now
                    return self.generateIntBinop(op, lhs_loc, rhs_loc, .i64);
                },
            }

            // Free the part registers we loaded
            self.codegen.freeGeneral(lhs_parts.low);
            self.codegen.freeGeneral(lhs_parts.high);
            self.codegen.freeGeneral(rhs_parts.low);
            self.codegen.freeGeneral(rhs_parts.high);

            // Store result to stack and return stack location
            const stack_offset = self.codegen.allocStackSlot(16);
            try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
            try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

            self.codegen.freeGeneral(result_low);
            self.codegen.freeGeneral(result_high);

            return .{ .stack_i128 = stack_offset };
        }

        /// Call Dec multiplication builtin: mulSaturatedC(RocDec, RocDec) -> RocDec
        /// RocDec is extern struct { num: i128 }, so passed/returned as i128
        fn callDecMul(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Error!void {
            // Get the address of the Dec multiply function
            const fn_addr = @intFromPtr(&builtins.dec.mulSaturatedC);

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for i128:
                // arg1: X0 (low), X1 (high)
                // arg2: X2 (low), X3 (high)
                // return: X0 (low), X1 (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);

                // Load function address and call
                const addr_reg = try self.codegen.allocGeneralFor(4);
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for i128:
                // arg1: RDI (low), RSI (high)
                // arg2: RDX (low), RCX (high)
                // return: RAX (low), RDX (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .RDI, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .RSI, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .RDX, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .RCX, rhs_parts.high);

                // Load function address into a register and call
                const addr_reg = try self.codegen.allocGeneralFor(0);
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.callReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from RAX, RDX
                try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
            }
        }

        /// Call Dec division builtin: divC(RocDec, RocDec, *RocOps) -> i128
        fn callDecDiv(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Error!void {
            // Get the address of the Dec divide function
            const fn_addr = @intFromPtr(&builtins.dec.divC);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for divC(RocDec, RocDec, *RocOps) -> i128:
                // arg1 (RocDec): X0 (low), X1 (high)
                // arg2 (RocDec): X2 (low), X3 (high)
                // arg3 (*RocOps): X4
                // return: X0 (low), X1 (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X4, roc_ops_reg);

                // Load function address and call
                const addr_reg = try self.codegen.allocGeneralFor(5);
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for divC(RocDec, RocDec, *RocOps) -> i128:
                // arg1 (RocDec): RDI (low), RSI (high)
                // arg2 (RocDec): RDX (low), RCX (high)
                // arg3 (*RocOps): R8
                // return: RAX (low), RDX (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .RDI, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .RSI, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .RDX, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .RCX, rhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .R8, roc_ops_reg);

                // Load function address into a register and call
                const addr_reg = try self.codegen.allocGeneralFor(0);
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.callReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from RAX, RDX
                try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
            }
        }

        /// Call i128/u128 division or remainder builtin
        /// Signature: (i128/u128, i128/u128, *RocOps) -> i128/u128
        fn callI128DivRem(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_low: GeneralReg,
            result_high: GeneralReg,
            is_unsigned: bool,
            is_rem: bool,
        ) Error!void {
            // Get the address of the appropriate builtin function
            const fn_addr: usize = if (is_unsigned)
                if (is_rem) @intFromPtr(&num_remTruncU128) else @intFromPtr(&num_divTruncU128)
            else if (is_rem) @intFromPtr(&num_remTruncI128) else @intFromPtr(&num_divTruncI128);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for (i128, i128, *RocOps) -> i128:
                // arg1: X0 (low), X1 (high)
                // arg2: X2 (low), X3 (high)
                // arg3: X4
                // return: X0 (low), X1 (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X4, roc_ops_reg);

                // Load function address and call
                const addr_reg = try self.codegen.allocGeneralFor(5);
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for (i128, i128, *RocOps) -> i128:
                // arg1: RDI (low), RSI (high)
                // arg2: RDX (low), RCX (high)
                // arg3: R8
                // return: RAX (low), RDX (high)
                //
                // IMPORTANT: Must handle register conflicts when moving to arg registers.
                // The source registers (from allocGeneralFor) could be any caller-saved
                // register including the argument registers themselves. If a source is
                // an arg register that gets written before the source is read, we'd get
                // wrong values. We save conflicting sources to R9/R10 first.

                // Load function address into R11 (caller-saved, not an arg register)
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                const arg_regs = [_]GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8 };
                const sources = [_]GeneralReg{ lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, roc_ops_reg };

                // For each source, check if it would be clobbered before use.
                // Source i is clobbered if it equals any of arg_regs[0..i].
                // We save such sources to R9 or R10.
                var saved: [5]?GeneralReg = .{ null, null, null, null, null };
                var next_temp: GeneralReg = .R9;

                for (0..5) |i| {
                    const src = sources[i];
                    for (0..i) |j| {
                        if (src == arg_regs[j]) {
                            // Check if we already saved this one
                            var found_saved: ?GeneralReg = null;
                            for (0..i) |k| {
                                if (sources[k] == src and saved[k] != null) {
                                    found_saved = saved[k];
                                    break;
                                }
                            }
                            if (found_saved) |s| {
                                saved[i] = s;
                            } else {
                                // Save to temp
                                try self.codegen.emit.movRegReg(.w64, next_temp, src);
                                saved[i] = next_temp;
                                next_temp = if (next_temp == .R9) .R10 else .RAX;
                            }
                            break;
                        }
                    }
                }

                // Now move to argument registers
                for (0..5) |i| {
                    const src = saved[i] orelse sources[i];
                    try self.codegen.emit.movRegReg(.w64, arg_regs[i], src);
                }

                // Call through R11
                try self.codegen.emit.callReg(.R11);

                // Get result from RAX, RDX
                // Handle conflict: if result_low is RDX, we'd clobber return high
                if (result_low == .RDX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .R9);
                } else if (result_high == .RAX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .R9);
                } else {
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                }
            }
        }

        /// Get low and high 64-bit parts of a 128-bit value
        const I128Parts = struct {
            low: GeneralReg,
            high: GeneralReg,
        };

        fn getI128Parts(self: *Self, loc: ValueLocation) Error!I128Parts {
            const low_reg = try self.codegen.allocGeneralFor(0);
            const high_reg = try self.codegen.allocGeneralFor(1);

            switch (loc) {
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    try self.codegen.emitLoadImm(low_reg, @bitCast(low));
                    try self.codegen.emitLoadImm(high_reg, @bitCast(high));
                },
                .stack_i128 => |offset| {
                    try self.codegen.emitLoadStack(.w64, low_reg, offset);
                    try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                },
                .immediate_i64 => |val| {
                    // Sign-extend to 128 bits
                    try self.codegen.emitLoadImm(low_reg, val);
                    if (val < 0) {
                        try self.codegen.emitLoadImm(high_reg, -1); // All 1s for sign extension
                    } else {
                        try self.codegen.emitLoadImm(high_reg, 0);
                    }
                },
                .general_reg => |reg| {
                    try self.emitMovRegReg(low_reg, reg);
                    try self.codegen.emitLoadImm(high_reg, 0);
                },
                .stack => |offset| {
                    try self.codegen.emitLoadStack(.w64, low_reg, offset);
                    try self.codegen.emitLoadImm(high_reg, 0);
                },
                else => {
                    return Error.InvalidLocalLocation;
                },
            }

            return .{ .low = low_reg, .high = high_reg };
        }

        /// Generate floating-point binary operation
        fn generateFloatBinop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
        ) Error!ValueLocation {
            // Load LHS into a float register
            const lhs_reg = try self.ensureInFloatReg(lhs_loc);

            // Load RHS into a float register
            const rhs_reg = try self.ensureInFloatReg(rhs_loc);

            // Allocate result register
            const result_reg = try self.codegen.allocFloatFor(0);

            switch (op) {
                .add => try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSubF64(result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMulF64(result_reg, lhs_reg, rhs_reg),
                .div => try self.codegen.emitDivF64(result_reg, lhs_reg, rhs_reg),
                else => {
                    // TODO: PLACEHOLDER - Uses ADD for float comparisons/boolean ops
                    // WHY: Float comparisons require:
                    //   - AArch64: FCMP followed by CSET to materialize condition
                    //   - x86: UCOMISD followed by SETcc to materialize condition
                    // The result should be an integer (0 or 1), not a float!
                    // IMPACT: Float comparisons return garbage (sum of operands as float)
                    // PROPER FIX:
                    //   1. Emit FCMP/UCOMISD to set condition flags
                    //   2. Use CSET (AArch64) or SETcc (x86) to get 0/1 into integer reg
                    //   3. Return that integer register, not a float register
                    try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg);
                },
            }

            // Free operand registers
            self.codegen.freeFloat(lhs_reg);
            self.codegen.freeFloat(rhs_reg);

            return .{ .float_reg = result_reg };
        }

        /// Generate code for unary minus
        fn generateUnaryMinus(self: *Self, unary: anytype) Error!ValueLocation {
            const inner_loc = try self.generateExpr(unary.expr);

            // Check if float
            const is_float = switch (unary.result_layout) {
                .f32, .f64 => true,
                else => false,
            };

            // Check if 128-bit type
            const is_i128 = switch (unary.result_layout) {
                .i128, .u128, .dec => true,
                else => false,
            };

            if (is_float) {
                const src_reg = try self.ensureInFloatReg(inner_loc);
                const result_reg = try self.codegen.allocFloatFor(0);
                try self.codegen.emitNegF64(result_reg, src_reg);
                self.codegen.freeFloat(src_reg);
                return .{ .float_reg = result_reg };
            } else if (is_i128) {
                // 128-bit negation: result = 0 - value (using SUBS/SBC or SUB/SBB)
                const parts = try self.getI128Parts(inner_loc);

                const result_low = try self.codegen.allocGeneralFor(0);
                const result_high = try self.codegen.allocGeneralFor(1);

                if (comptime builtin.cpu.arch == .aarch64) {
                    // Negate using NEGS (NEG with flags) and NGC (negate with carry)
                    // NEGS is actually SUBS with XZR as first operand
                    try self.codegen.emit.subsRegRegReg(.w64, result_low, .ZRSP, parts.low);
                    // NGC is SBC with XZR as first operand
                    try self.codegen.emit.sbcRegRegReg(.w64, result_high, .ZRSP, parts.high);
                } else {
                    // x86_64: Load 0, then subtract
                    try self.codegen.emitLoadImm(result_low, 0);
                    try self.codegen.emit.subRegReg(.w64, result_low, parts.low);
                    try self.codegen.emitLoadImm(result_high, 0);
                    try self.codegen.emit.sbbRegReg(.w64, result_high, parts.high);
                }

                self.codegen.freeGeneral(parts.low);
                self.codegen.freeGeneral(parts.high);

                // Store result to stack
                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

                self.codegen.freeGeneral(result_low);
                self.codegen.freeGeneral(result_high);

                return .{ .stack_i128 = stack_offset };
            } else {
                // For 64-bit integers, use NEG
                const reg = try self.ensureInGeneralReg(inner_loc);
                const result_reg = try self.codegen.allocGeneralFor(0);
                try self.codegen.emitNeg(.w64, result_reg, reg);
                self.codegen.freeGeneral(reg);
                return .{ .general_reg = result_reg };
            }
        }

        /// Generate code for unary not
        fn generateUnaryNot(self: *Self, unary: anytype) Error!ValueLocation {
            const inner_loc = try self.generateExpr(unary.expr);

            const reg = try self.ensureInGeneralReg(inner_loc);
            const result_reg = try self.codegen.allocGeneralFor(0);

            // TODO: PLACEHOLDER - Uses NEG instead of proper boolean NOT
            // WHY: Boolean NOT should flip 0↔1, but NEG gives: NEG(0)=0, NEG(1)=-1
            // IMPACT: `not False` = 0 (correct), `not True` = -1 (wrong, should be 0)
            // PROPER FIX: Use XOR with 1:
            //   - AArch64: EOR Rd, Rn, #1
            //   - x86: XOR reg, 1
            // This correctly flips: 0 XOR 1 = 1, 1 XOR 1 = 0
            try self.codegen.emitNeg(.w64, result_reg, reg);

            self.codegen.freeGeneral(reg);
            return .{ .general_reg = result_reg };
        }

        /// Generate code for if-then-else
        fn generateIfThenElse(self: *Self, ite: anytype) Error!ValueLocation {
            const branches = self.store.getIfBranches(ite.branches);

            // Allocate result register
            const result_reg = try self.codegen.allocGeneralFor(0);

            // Collect jump targets for patching
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            // Generate each branch
            for (branches) |branch| {
                // Generate condition
                const cond_loc = try self.generateExpr(branch.cond);
                const cond_reg = try self.ensureInGeneralReg(cond_loc);

                // Compare with zero and branch if equal (condition is false)
                const else_patch = try self.emitCmpZeroAndJump(cond_reg);

                self.codegen.freeGeneral(cond_reg);

                // Generate body (true case)
                const body_loc = try self.generateExpr(branch.body);
                const body_reg = try self.ensureInGeneralReg(body_loc);
                try self.emitMovRegReg(result_reg, body_reg);
                self.codegen.freeGeneral(body_reg);

                // Jump to end (skip the else branch)
                const end_patch = try self.codegen.emitJump();
                try end_patches.append(self.allocator, end_patch);

                // Patch the else jump to here (start of else/next branch)
                const current_offset = self.codegen.currentOffset();
                self.codegen.patchJump(else_patch, current_offset);
            }

            // Generate final else
            const else_loc = try self.generateExpr(ite.final_else);
            const else_reg = try self.ensureInGeneralReg(else_loc);
            try self.emitMovRegReg(result_reg, else_reg);
            self.codegen.freeGeneral(else_reg);

            // Patch all end jumps to here
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            return .{ .general_reg = result_reg };
        }

        /// Compare register with zero and jump if equal (condition is false)
        /// Returns the patch location for the jump
        fn emitCmpZeroAndJump(self: *Self, reg: GeneralReg) !usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // cbz reg, 0 (branch if zero, offset will be patched)
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.cbz(.w64, reg, 0);
                return patch_loc;
            } else {
                // cmp reg, 0; je (will be patched)
                try self.codegen.emit.cmpRegImm32(.w64, reg, 0);
                return try self.codegen.emitCondJump(.equal);
            }
        }

        /// Move register to register (architecture-specific)
        fn emitMovRegReg(self: *Self, dst: GeneralReg, src: GeneralReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, dst, src);
            } else {
                try self.codegen.emit.movRegReg(.w64, dst, src);
            }
        }

        /// Generate code for when/match expression
        fn generateWhen(_: *Self, _: anytype) Error!ValueLocation {
            // TODO: NOT IMPLEMENTED - Pattern matching (when/match expressions)
            // WHY: Pattern matching requires:
            //   1. Tag extraction: Load the tag byte from the scrutinee
            //   2. Tag comparison: Generate switch on tag values
            //   3. Payload extraction: Load bound variables from union payload
            //   4. Guard evaluation: If patterns have guards, evaluate them
            //   5. Branch generation: Like if-then-else but with multiple arms
            // RELATED CODE:
            //   - generateIfThenElse shows similar branch/patch logic
            //   - generateU8DispatchCall shows switch-like code generation
            //   - Lower.zig pattern handling shows what patterns look like
            // IMPACT: Any code using `when expr is ...` fails with UnsupportedExpression
            return Error.UnsupportedExpression;
        }

        /// Generate code for a block
        fn generateBlock(self: *Self, block: anytype) Error!ValueLocation {
            const stmts = self.store.getStmts(block.stmts);

            // Process each statement
            for (stmts) |stmt| {
                // Check if this is a lambda/closure binding
                const stmt_expr = self.store.getExpr(stmt.expr);

                switch (stmt_expr) {
                    .lambda, .closure => {
                        // Store the expression ID for later invocation
                        try self.bindLambdaPattern(stmt.pattern, stmt.expr);

                        // If this is a recursive closure, track the symbol for recursive call detection
                        if (stmt_expr == .closure) {
                            const closure = stmt_expr.closure;
                            if (closure.self_recursive == .self_recursive) {
                                const pattern = self.store.getPattern(stmt.pattern);
                                if (pattern == .bind) {
                                    // Track this as a potential recursive symbol
                                    // The actual recursive context is set up when calling the closure
                                }
                            }
                        }
                    },
                    else => {
                        // Generate code for the expression
                        const expr_loc = try self.generateExpr(stmt.expr);
                        // Bind the result to the pattern
                        try self.bindPattern(stmt.pattern, expr_loc);
                    },
                }
            }

            // Generate the final expression
            return self.generateExpr(block.final_expr);
        }

        /// Bind a lambda/closure expression ID to a pattern (for later invocation)
        fn bindLambdaPattern(self: *Self, pattern_id: MonoPatternId, expr_id: MonoExprId) Error!void {
            const pattern = self.store.getPattern(pattern_id);

            switch (pattern) {
                .bind => |bind| {
                    const symbol_key: u48 = @bitCast(bind.symbol);
                    try self.lambda_bindings.put(symbol_key, expr_id);
                },
                else => {
                    // Non-simple bindings for lambdas not supported
                },
            }
        }

        /// Bind a value to a pattern
        fn bindPattern(self: *Self, pattern_id: MonoPatternId, value_loc: ValueLocation) Error!void {
            const pattern = self.store.getPattern(pattern_id);

            switch (pattern) {
                .bind => |bind| {
                    // Simple binding - just record the location
                    const symbol_key: u48 = @bitCast(bind.symbol);
                    try self.symbol_locations.put(symbol_key, value_loc);
                },
                .wildcard => {
                    // Ignore the value
                },
                else => {
                    // TODO: NOT IMPLEMENTED - Destructuring patterns
                    // Examples that don't work:
                    //   { a, b } = some_record       -- record destructuring
                    //   (x, y) = some_tuple          -- tuple destructuring
                    //   Cons(head, tail) = some_list -- tag destructuring
                    // WHAT'S NEEDED:
                    //   1. For records/tuples: load each field from struct offset, bind to symbol
                    //   2. For tags: check tag matches, load payload, recursively bind sub-pattern
                    // IMPACT: Code using destructuring patterns silently fails (no binding happens)
                },
            }
        }

        /// Bind all captured values for a closure
        fn bindClosureCaptures(self: *Self, closure: anytype) Error!void {
            const captures = self.store.getCaptures(closure.captures);
            for (captures) |cap| {
                const symbol_key: u48 = @bitCast(cap.symbol);
                if (self.symbol_locations.get(symbol_key) == null) {
                    // Try to look up the captured value from definitions
                    if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                        const loc = try self.generateExpr(def_expr_id);
                        try self.symbol_locations.put(symbol_key, loc);
                    }
                }
            }
        }

        /// Generate code for a lambda expression (unevaluated function)
        /// Lambdas are not executed immediately - they're stored for later invocation
        fn generateLambda(_: *Self, _: anytype) Error!ValueLocation {
            // TODO: PLACEHOLDER - Standalone lambda returns 0 instead of closure value
            //
            // CURRENT BEHAVIOR:
            // Returns 0 because our current calling convention inlines lambdas at call
            // sites. When `(|x| x + 1)(5)` is evaluated, generateCall detects the lambda
            // and calls generateLambdaCall directly with the body - no closure value needed.
            //
            // WHEN THIS MATTERS:
            // This placeholder breaks when lambdas aren't immediately called:
            //   f = |x| x + 1    -- f gets 0, not a callable value
            //   f(5)             -- lookup finds 0, not the lambda body
            //
            // PROPER IMPLEMENTATION:
            // For lambdas that escape their immediate context, we need to:
            // 1. Allocate a closure struct (even for non-capturing lambdas, for uniformity)
            // 2. Store a function pointer or lambda index
            // 3. Return the closure struct address
            //
            // This is handled by closure expressions with direct_call representation,
            // so most lambdas should be wrapped in closures by Lower.zig before we
            // reach this point. This function is only called for rare edge cases.
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for a closure (lambda with captured environment)
        /// This creates the actual runtime closure value based on the representation.
        ///
        /// Generate code for a closure expression.
        /// Binds captures and returns closure data for later call.
        ///
        /// NOTE: Recursive closures should be compiled as procedures via compileAllProcs()
        /// BEFORE generating the main expression. This inline path only works for
        /// non-recursive closures.
        fn generateClosure(self: *Self, closure: anytype) Error!ValueLocation {
            // Bind captures and return closure data for inline evaluation
            switch (closure.representation) {
                .direct_call => {
                    // No captures - no runtime representation needed
                    // The closure is just a reference to the lambda body
                    return .{ .immediate_i64 = 0 };
                },
                .unwrapped_capture => |_| {
                    // Single capture - the closure IS the captured value
                    // Zero overhead representation
                    const captures = self.store.getCaptures(closure.captures);
                    if (captures.len > 0) {
                        const cap = captures[0];
                        const symbol_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(symbol_key)) |loc| {
                            return loc;
                        }
                        // Try to look up the captured value
                        if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                            return try self.generateExpr(def_expr_id);
                        }
                    }
                    return .{ .immediate_i64 = 0 };
                },
                .struct_captures => |repr| {
                    // Multiple captures - bind all captures for inline evaluation
                    const captures = self.store.getCaptures(repr.captures);

                    // Evaluate all captured values and store them in symbol_locations
                    // This allows the inlined body to access all captures
                    var first_loc: ?ValueLocation = null;
                    for (captures) |cap| {
                        const symbol_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(symbol_key)) |loc| {
                            if (first_loc == null) first_loc = loc;
                        } else if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                            const loc = try self.generateExpr(def_expr_id);
                            try self.symbol_locations.put(symbol_key, loc);
                            if (first_loc == null) first_loc = loc;
                        }
                    }

                    return first_loc orelse .{ .immediate_i64 = 0 };
                },
                .enum_dispatch => |repr| {
                    // Multiple functions, no captures - just return the tag
                    return .{ .immediate_i64 = repr.tag };
                },
                .union_repr => |repr| {
                    // Multiple functions with captures - bind captures for inline evaluation
                    // The tag and captures will be used for dispatch at call site
                    const captures = self.store.getCaptures(repr.captures);
                    for (captures) |cap| {
                        const symbol_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(symbol_key) == null) {
                            if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                                const loc = try self.generateExpr(def_expr_id);
                                try self.symbol_locations.put(symbol_key, loc);
                            }
                        }
                    }
                    return .{ .immediate_i64 = repr.tag };
                },
            }
        }

        /// Get the register used for argument N in the calling convention
        fn getArgumentRegister(_: *Self, index: u8) GeneralReg {
            if (comptime builtin.cpu.arch == .aarch64) {
                // AArch64: X0-X7 for arguments
                return @enumFromInt(index);
            } else {
                // x86_64 System V: RDI, RSI, RDX, RCX, R8, R9
                const arg_regs = [_]x86_64.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 };
                return arg_regs[@min(index, arg_regs.len - 1)];
            }
        }

        /// Get the register used for return values
        fn getReturnRegister(_: *Self) GeneralReg {
            if (comptime builtin.cpu.arch == .aarch64) {
                return .X0;
            } else {
                return .RAX;
            }
        }

        /// Emit a call instruction to a specific code offset
        fn emitCallToOffset(self: *Self, target_offset: usize) !void {
            const current = self.codegen.currentOffset();
            // Calculate relative byte offset (can be negative for backward call)
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)));

            if (comptime builtin.cpu.arch == .aarch64) {
                // BL instruction expects byte offset (it divides by 4 internally)
                try self.codegen.emit.bl(rel_offset);
            } else {
                // x86_64: CALL rel32
                // Offset is relative to instruction after the call (current + 5)
                const call_rel = rel_offset - 5;
                try self.codegen.emit.call(@bitCast(call_rel));
            }
        }

        /// Generate code for a function call
        fn generateCall(self: *Self, call: anytype) Error!ValueLocation {
            // Get the function expression
            const fn_expr = self.store.getExpr(call.fn_expr);

            return switch (fn_expr) {
                // Direct lambda call: (|x| x + 1)(5)
                .lambda => |lambda| try self.generateLambdaCall(lambda, call.args, call.ret_layout),

                // Direct closure call
                .closure => |closure| try self.generateClosureCall(closure, call.args, call.ret_layout),

                // Chained/curried call: the result of a previous call
                .call => |inner_call| try self.generateChainedCall(inner_call, call.args, call.ret_layout),

                // Block that returns a lambda
                .block => |block| try self.generateBlockCall(block, call.args, call.ret_layout),

                // Lookup a function and call it
                .lookup => |lookup| try self.generateLookupCall(lookup, call.args, call.ret_layout),

                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for calling a lambda directly: (|x| x + 1)(5)
        fn generateLambdaCall(self: *Self, lambda: anytype, args_span: anytype, _: layout.Idx) Error!ValueLocation {
            // Get the parameter patterns
            const params = self.store.getPatternSpan(lambda.params);
            const args = self.store.getExprSpan(args_span);

            // Evaluate each argument and bind to the corresponding parameter
            for (params, args) |param_id, arg_id| {
                // Check if the argument is a closure/lambda - if so, bind it for later invocation
                const arg_expr = self.store.getExpr(arg_id);
                switch (arg_expr) {
                    .lambda, .closure => {
                        // This is a closure being passed as an argument
                        // Bind the expression ID so it can be called later
                        try self.bindLambdaPattern(param_id, arg_id);
                        // Also evaluate and bind the value (for captures, etc.)
                        const arg_loc = try self.generateExpr(arg_id);
                        try self.bindPattern(param_id, arg_loc);
                    },
                    else => {
                        // Normal argument - just evaluate and bind
                        const arg_loc = try self.generateExpr(arg_id);
                        try self.bindPattern(param_id, arg_loc);
                    },
                }
            }

            // Now generate code for the lambda body with arguments bound
            return self.generateExpr(lambda.body);
        }

        /// Generate code for calling a closure (lambda with captures)
        /// This handles the representation-based dispatch and capture unpacking.
        ///
        /// INLINING DECISION:
        /// - If shouldInlineClosure() returns true: inline the lambda body
        /// - If shouldInlineClosure() returns false: call the emitted function
        fn generateClosureCall(self: *Self, closure: anytype, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            // Inline path: bind captures and inline the lambda body
            // NOTE: For recursive closures, this will fail. They need to be compiled
            // as procedures first via compileAllProcs().
            // Bind the captured values based on representation
            switch (closure.representation) {
                .direct_call => {
                    // No captures to bind
                },
                .unwrapped_capture => {
                    // Single capture - the closure value IS the captured value
                    const captures = self.store.getCaptures(closure.captures);
                    if (captures.len > 0) {
                        const cap = captures[0];
                        const symbol_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(symbol_key) == null) {
                            // Try to look up the captured value from outer scope
                            if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                                const loc = try self.generateExpr(def_expr_id);
                                try self.symbol_locations.put(symbol_key, loc);
                            }
                        }
                    }
                },
                .struct_captures => |repr| {
                    // Multiple captures - unpack from struct
                    const captures = self.store.getCaptures(repr.captures);
                    for (captures) |cap| {
                        const symbol_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(symbol_key) == null) {
                            if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                                const loc = try self.generateExpr(def_expr_id);
                                try self.symbol_locations.put(symbol_key, loc);
                            }
                        }
                    }
                },
                .enum_dispatch, .union_repr => {
                    // Bind captures from scope for inline evaluation
                    const captures = self.store.getCaptures(closure.captures);
                    for (captures) |cap| {
                        const symbol_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(symbol_key) == null) {
                            if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                                const loc = try self.generateExpr(def_expr_id);
                                try self.symbol_locations.put(symbol_key, loc);
                            }
                        }
                    }
                },
            }

            // Get the lambda from the closure and call it (inlined)
            const lambda_expr = self.store.getExpr(closure.lambda);

            return switch (lambda_expr) {
                .lambda => |lambda| try self.generateRecursiveLambdaCall(lambda, args_span, ret_layout, closure.self_recursive),
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate a closure call via an emitted function (for non-inlined closures).
        /// This is used when the closure is bound to a variable or recursive.
        /// Generate code for calling a closure with symbol tracking for recursion
        fn generateClosureCallWithSymbol(
            self: *Self,
            closure: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
            symbol: MonoSymbol,
        ) Error!ValueLocation {
            // Inline path: bind captures and inline the lambda body
            // NOTE: For recursive closures, this will fail. They need to be compiled
            // as procedures first via compileAllProcs().

            // Check for lambda set dispatch (enum_dispatch or union_repr)
            switch (closure.representation) {
                .enum_dispatch => |repr| {
                    // Multiple functions with no captures - generate switch dispatch
                    return try self.generateEnumDispatchCall(repr.lambda_set, symbol, args_span, ret_layout);
                },
                .union_repr => |repr| {
                    // Multiple functions with captures - generate switch dispatch with captures
                    return try self.generateUnionReprCall(repr.lambda_set, symbol, args_span, ret_layout);
                },
                .direct_call => {},
                .unwrapped_capture => {
                    const captures = self.store.getCaptures(closure.captures);
                    if (captures.len > 0) {
                        const cap = captures[0];
                        const sym_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(sym_key) == null) {
                            if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                                const loc = try self.generateExpr(def_expr_id);
                                try self.symbol_locations.put(sym_key, loc);
                            }
                        }
                    }
                },
                .struct_captures => |repr| {
                    const captures = self.store.getCaptures(repr.captures);
                    for (captures) |cap| {
                        const sym_key: u48 = @bitCast(cap.symbol);
                        if (self.symbol_locations.get(sym_key) == null) {
                            if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                                const loc = try self.generateExpr(def_expr_id);
                                try self.symbol_locations.put(sym_key, loc);
                            }
                        }
                    }
                },
            }

            // Get the lambda from the closure and call it with symbol tracking
            const lambda_expr = self.store.getExpr(closure.lambda);

            return switch (lambda_expr) {
                .lambda => |lambda| try self.generateRecursiveLambdaCallWithSymbol(
                    lambda,
                    args_span,
                    ret_layout,
                    closure.self_recursive,
                    symbol,
                ),
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for enum dispatch call (multiple non-capturing functions)
        /// Generates a switch on the runtime tag value to call the correct lambda.
        fn generateEnumDispatchCall(
            self: *Self,
            lambda_set: mono.LambdaSetMemberSpan,
            symbol: MonoSymbol,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            const members = self.store.getLambdaSetMembers(lambda_set);
            if (members.len == 0) {
                return Error.UnsupportedExpression;
            }

            // Get the runtime tag value from the symbol's location
            const symbol_key: u48 = @bitCast(symbol);
            const tag_loc = self.symbol_locations.get(symbol_key) orelse return Error.LocalNotFound;

            // For 2 functions, use a simple conditional branch (Bool dispatch)
            if (members.len == 2) {
                return try self.generateBoolDispatchCall(members, tag_loc, args_span, ret_layout);
            }

            // For 3+ functions, generate a switch (U8 dispatch)
            return try self.generateU8DispatchCall(members, tag_loc, args_span, ret_layout);
        }

        /// Generate code for union_repr call (multiple functions with captures)
        ///
        /// TODO: PLACEHOLDER - Delegates to enum_dispatch, ignores capture extraction
        ///
        /// DIFFERENCE FROM enum_dispatch:
        /// enum_dispatch: closures have NO captures, so the tag alone identifies the function
        /// union_repr: closures HAVE captures, stored in a tagged union alongside the tag
        ///
        /// CURRENT BEHAVIOR:
        /// Delegates to generateEnumDispatchCall, which only handles the tag-based dispatch.
        /// This works when captures are still bound from the enclosing scope (immediate calls),
        /// but breaks when the closure escapes and captures need to be loaded from the union.
        ///
        /// PROPER IMPLEMENTATION:
        /// 1. Load the closure pointer from symbol_locations (points to tagged union)
        /// 2. Load the tag: ldr tag_reg, [closure_ptr]
        /// 3. Switch on tag (like enum_dispatch)
        /// 4. FOR EACH BRANCH, before calling the lambda:
        ///    a. Get that lambda's capture layout from LambdaSetMember.captures
        ///    b. Load each capture from the union payload:
        ///       ldr cap_reg, [closure_ptr, #offset]  // offset varies by capture index
        ///    c. Bind captures to symbol_locations
        ///    d. Then call the lambda body
        ///
        /// This is more complex than enum_dispatch because each lambda in the set may have
        /// DIFFERENT captures with DIFFERENT layouts. The union payload must be large enough
        /// to hold the largest capture set, with each lambda knowing its own offsets.
        fn generateUnionReprCall(
            self: *Self,
            lambda_set: mono.LambdaSetMemberSpan,
            symbol: MonoSymbol,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            // Delegate to enum_dispatch for now - captures are bound from enclosing scope
            return try self.generateEnumDispatchCall(lambda_set, symbol, args_span, ret_layout);
        }

        /// Generate Bool dispatch (2 functions in lambda set)
        fn generateBoolDispatchCall(
            self: *Self,
            members: []const mono.LambdaSetMember,
            tag_loc: ValueLocation,
            args_span: anytype,
            _: layout.Idx,
        ) Error!ValueLocation {
            // Allocate result register
            const result_reg = try self.codegen.allocGeneralFor(0);

            // Get tag into a register
            const tag_reg = try self.ensureInGeneralReg(tag_loc);

            // Compare tag with zero and branch
            const else_patch = try self.emitCmpZeroAndJump(tag_reg);
            self.codegen.freeGeneral(tag_reg);

            // Tag == 0: Call first lambda (members[0])
            const first_member = members[0];
            const first_lambda_expr = self.store.getExpr(first_member.lambda_body);
            if (first_lambda_expr == .lambda) {
                const result0 = try self.generateLambdaCall(first_lambda_expr.lambda, args_span, .i64);
                const result0_reg = try self.ensureInGeneralReg(result0);
                try self.emitMovRegReg(result_reg, result0_reg);
                self.codegen.freeGeneral(result0_reg);
            }

            // Jump to end
            const end_patch = try self.codegen.emitJump();

            // Patch else jump
            const else_offset = self.codegen.currentOffset();
            self.codegen.patchJump(else_patch, else_offset);

            // Tag != 0: Call second lambda (members[1])
            if (members.len > 1) {
                const second_member = members[1];
                const second_lambda_expr = self.store.getExpr(second_member.lambda_body);
                if (second_lambda_expr == .lambda) {
                    const result1 = try self.generateLambdaCall(second_lambda_expr.lambda, args_span, .i64);
                    const result1_reg = try self.ensureInGeneralReg(result1);
                    try self.emitMovRegReg(result_reg, result1_reg);
                    self.codegen.freeGeneral(result1_reg);
                }
            }

            // Patch end jump
            const end_offset = self.codegen.currentOffset();
            self.codegen.patchJump(end_patch, end_offset);

            return .{ .general_reg = result_reg };
        }

        /// Generate U8 dispatch (3+ functions in lambda set)
        fn generateU8DispatchCall(
            self: *Self,
            members: []const mono.LambdaSetMember,
            tag_loc: ValueLocation,
            args_span: anytype,
            _: layout.Idx,
        ) Error!ValueLocation {
            // Allocate result register
            const result_reg = try self.codegen.allocGeneralFor(0);

            // Get tag into a register
            const tag_reg = try self.ensureInGeneralReg(tag_loc);

            // Generate cascading comparisons for each member
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            for (members, 0..) |member, i| {
                // Compare tag with member's tag value
                if (i < members.len - 1) {
                    // Not the last - need comparison and jump
                    try self.emitCmpImm(tag_reg, @intCast(member.tag));
                    const skip_patch = try self.emitJumpIfNotEqual();

                    // Tag matches - call this lambda
                    const lambda_expr = self.store.getExpr(member.lambda_body);
                    if (lambda_expr == .lambda) {
                        const result = try self.generateLambdaCall(lambda_expr.lambda, args_span, .i64);
                        const res_reg = try self.ensureInGeneralReg(result);
                        try self.emitMovRegReg(result_reg, res_reg);
                        self.codegen.freeGeneral(res_reg);
                    }

                    // Jump to end
                    const end_patch = try self.codegen.emitJump();
                    try end_patches.append(self.allocator, end_patch);

                    // Patch skip to here
                    const skip_offset = self.codegen.currentOffset();
                    self.codegen.patchJump(skip_patch, skip_offset);
                } else {
                    // Last member - default case, no comparison needed
                    const lambda_expr = self.store.getExpr(member.lambda_body);
                    if (lambda_expr == .lambda) {
                        const result = try self.generateLambdaCall(lambda_expr.lambda, args_span, .i64);
                        const res_reg = try self.ensureInGeneralReg(result);
                        try self.emitMovRegReg(result_reg, res_reg);
                        self.codegen.freeGeneral(res_reg);
                    }
                }
            }

            // Patch all end jumps
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            self.codegen.freeGeneral(tag_reg);

            return .{ .general_reg = result_reg };
        }

        /// Emit compare immediate instruction
        fn emitCmpImm(self: *Self, reg: GeneralReg, value: i64) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // CMP reg, #imm12
                try self.codegen.emit.cmpRegImm12(.w64, reg, @intCast(value));
            } else {
                // x86_64: CMP reg, imm32
                // Load immediate into temporary register and compare
                const temp = try self.codegen.allocGeneralFor(0);
                try self.codegen.emitLoadImm(temp, value);
                try self.codegen.emit.cmpRegReg(.w64, reg, temp);
                self.codegen.freeGeneral(temp);
            }
        }

        /// Emit jump if not equal (after comparison)
        ///
        /// BRANCH PATCHING MECHANISM:
        /// When generating switch dispatch, we don't know the jump target offset until
        /// we've generated the code for the branch body. So we:
        /// 1. Emit the branch instruction with offset=0 (placeholder)
        /// 2. Record the instruction's location (patch_loc)
        /// 3. Generate the branch body code
        /// 4. Calculate the actual offset: current_offset - patch_loc
        /// 5. Patch the instruction at patch_loc with the real offset
        ///
        /// WHY OFFSET 0 IS SAFE:
        /// Offset 0 means "jump to the next instruction" which is harmless if we
        /// somehow fail to patch. But in normal operation, codegen.patchJump()
        /// overwrites the placeholder before execution.
        ///
        /// RETURNS: The patch location (where the displacement bytes are) for later patching.
        fn emitJumpIfNotEqual(self: *Self) !usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B.NE (branch if not equal) with placeholder offset
                // On aarch64, the entire 4-byte instruction encodes the offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.ne, 0);
                return patch_loc;
            } else {
                // JNE (jump if not equal) with placeholder offset
                // x86_64: JNE rel32 is 0F 85 xx xx xx xx (6 bytes)
                // The displacement starts at offset +2, so patch_loc = currentOffset + 2
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jne(@bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Generate code for calling a lambda, handling recursive closures with join points
        fn generateRecursiveLambdaCall(
            self: *Self,
            lambda: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
            self_recursive: SelfRecursive,
        ) Error!ValueLocation {
            // Call without symbol tracking (for non-lookup calls)
            return self.generateRecursiveLambdaCallWithSymbol(lambda, args_span, ret_layout, self_recursive, null);
        }

        /// Generate code for calling a lambda with full recursion support
        fn generateRecursiveLambdaCallWithSymbol(
            self: *Self,
            lambda: anytype,
            args_span: anytype,
            _: layout.Idx,
            self_recursive: SelfRecursive,
            symbol: ?MonoSymbol,
        ) Error!ValueLocation {
            // Get the parameter patterns and arguments
            const params = self.store.getPatternSpan(lambda.params);
            const args = self.store.getExprSpan(args_span);

            // Evaluate each argument and bind to the corresponding parameter
            for (params, args) |param_id, arg_id| {
                // Check if the argument is a closure/lambda - if so, bind it for later invocation
                const arg_expr = self.store.getExpr(arg_id);
                switch (arg_expr) {
                    .lambda, .closure => {
                        // This is a closure being passed as an argument
                        // Bind the expression ID so it can be called later
                        try self.bindLambdaPattern(param_id, arg_id);
                        // Also evaluate and bind the value (for captures, etc.)
                        const arg_loc = try self.generateExpr(arg_id);
                        try self.bindPattern(param_id, arg_loc);
                    },
                    else => {
                        // Normal argument - just evaluate and bind
                        const arg_loc = try self.generateExpr(arg_id);
                        try self.bindPattern(param_id, arg_loc);
                    },
                }
            }

            // Check if this is a recursive closure
            switch (self_recursive) {
                .not_self_recursive => {
                    // Non-recursive - just generate the body normally
                    return self.generateExpr(lambda.body);
                },
                .self_recursive => |join_point_id| {
                    // Recursive closure - set up recursive context
                    const old_recursive_symbol = self.current_recursive_symbol;
                    const old_recursive_join_point = self.current_recursive_join_point;

                    // Set up recursive context if we have a symbol
                    if (symbol) |sym| {
                        self.current_recursive_symbol = sym;
                        self.current_recursive_join_point = join_point_id;
                    }

                    // Record the current code offset as the join point (start of body)
                    const join_point_offset = self.codegen.currentOffset();
                    try self.join_points.put(@intFromEnum(join_point_id), join_point_offset);

                    // Generate the body
                    // NOTE: This inline approach does NOT work for non-tail recursive calls
                    // like `n * factorial(n-1)`. Proper recursive support requires compiling
                    // the closure as a procedure first via compileAllProcs().
                    const result = try self.generateExpr(lambda.body);

                    // Restore old recursive context
                    self.current_recursive_symbol = old_recursive_symbol;
                    self.current_recursive_join_point = old_recursive_join_point;

                    return result;
                },
            }
        }

        /// Generate code for a chained/curried call: ((|a| |b| a * b)(5))(10)
        fn generateChainedCall(self: *Self, inner_call: anytype, outer_args: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            // First, execute the inner call to get the resulting lambda/closure
            const inner_fn_expr = self.store.getExpr(inner_call.fn_expr);

            return switch (inner_fn_expr) {
                .lambda => |lambda| {
                    // Get inner call arguments
                    const inner_args = self.store.getExprSpan(inner_call.args);
                    const params = self.store.getPatternSpan(lambda.params);

                    // Bind inner arguments to parameters
                    for (params, inner_args) |param_id, arg_id| {
                        const arg_loc = try self.generateExpr(arg_id);
                        try self.bindPattern(param_id, arg_loc);
                    }

                    // The lambda body should return another lambda
                    const body_expr = self.store.getExpr(lambda.body);

                    return switch (body_expr) {
                        .lambda => |inner_lambda| {
                            // Now call the returned lambda with outer args
                            return try self.generateLambdaCall(inner_lambda, outer_args, ret_layout);
                        },
                        .closure => |inner_closure| {
                            return try self.generateClosureCall(inner_closure, outer_args, ret_layout);
                        },
                        .block => |block| {
                            // Block that returns a lambda
                            return try self.generateBlockCall(block, outer_args, ret_layout);
                        },
                        else => return Error.UnsupportedExpression,
                    };
                },
                .call => |nested_call| {
                    // Triple-nested call: (((fn)(arg1))(arg2))(arg3)
                    // We need to recursively process the inner call first
                    // The result will be a lambda/closure that we then call with inner_call.args
                    // and finally with outer_args

                    // Process the innermost call to get the first returned lambda
                    const nested_fn_expr = self.store.getExpr(nested_call.fn_expr);

                    switch (nested_fn_expr) {
                        .lambda => |nested_lambda| {
                            // Bind the nested call's arguments to the nested lambda's parameters
                            const nested_args = self.store.getExprSpan(nested_call.args);
                            const nested_params = self.store.getPatternSpan(nested_lambda.params);

                            for (nested_params, nested_args) |param_id, arg_id| {
                                const arg_loc = try self.generateExpr(arg_id);
                                try self.bindPattern(param_id, arg_loc);
                            }

                            // The nested lambda body should return another lambda
                            const nested_body_expr = self.store.getExpr(nested_lambda.body);

                            switch (nested_body_expr) {
                                .lambda => |level2_lambda| {
                                    // Bind inner_call.args to level2 lambda params
                                    const level2_args = self.store.getExprSpan(inner_call.args);
                                    const level2_params = self.store.getPatternSpan(level2_lambda.params);

                                    for (level2_params, level2_args) |param_id, arg_id| {
                                        const arg_loc = try self.generateExpr(arg_id);
                                        try self.bindPattern(param_id, arg_loc);
                                    }

                                    // The level2 lambda body should return another lambda
                                    const level2_body_expr = self.store.getExpr(level2_lambda.body);

                                    switch (level2_body_expr) {
                                        .lambda => |level3_lambda| {
                                            // Finally call with outer_args
                                            return try self.generateLambdaCall(level3_lambda, outer_args, ret_layout);
                                        },
                                        .closure => |level3_closure| {
                                            return try self.generateClosureCall(level3_closure, outer_args, ret_layout);
                                        },
                                        else => return Error.UnsupportedExpression,
                                    }
                                },
                                .closure => |level2_closure| {
                                    // Handle closure that returns another lambda/closure
                                    // First bind the captures
                                    try self.bindClosureCaptures(level2_closure);

                                    // Bind inner_call.args to the closure's lambda params
                                    const lambda_expr = self.store.getExpr(level2_closure.lambda);
                                    switch (lambda_expr) {
                                        .lambda => |inner_lambda| {
                                            const level2_args = self.store.getExprSpan(inner_call.args);
                                            const level2_params = self.store.getPatternSpan(inner_lambda.params);

                                            for (level2_params, level2_args) |param_id, arg_id| {
                                                const arg_loc = try self.generateExpr(arg_id);
                                                try self.bindPattern(param_id, arg_loc);
                                            }

                                            // The inner lambda body should return another lambda
                                            const level2_body_expr = self.store.getExpr(inner_lambda.body);

                                            switch (level2_body_expr) {
                                                .lambda => |level3_lambda| {
                                                    return try self.generateLambdaCall(level3_lambda, outer_args, ret_layout);
                                                },
                                                .closure => |level3_closure| {
                                                    return try self.generateClosureCall(level3_closure, outer_args, ret_layout);
                                                },
                                                else => return Error.UnsupportedExpression,
                                            }
                                        },
                                        else => return Error.UnsupportedExpression,
                                    }
                                },
                                else => return Error.UnsupportedExpression,
                            }
                        },
                        else => return Error.UnsupportedExpression,
                    }
                },
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for calling the result of a block expression
        fn generateBlockCall(self: *Self, block: anytype, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            // Execute the block statements
            const stmts = self.store.getStmts(block.stmts);
            for (stmts) |stmt| {
                const expr_loc = try self.generateExpr(stmt.expr);
                try self.bindPattern(stmt.pattern, expr_loc);
            }

            // The final expression should be a lambda/closure
            const final_expr = self.store.getExpr(block.final_expr);

            return switch (final_expr) {
                .lambda => |lambda| try self.generateLambdaCall(lambda, args_span, ret_layout),
                .closure => |closure| try self.generateClosureCall(closure, args_span, ret_layout),
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for calling a looked-up function
        fn generateLookupCall(self: *Self, lookup: anytype, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            const symbol_key: u48 = @bitCast(lookup.symbol);

            // FIRST: Check if the function was compiled as a procedure (for recursive functions)
            // This takes priority over inlining because recursive functions MUST be called
            // as proper procedures with stack frames.
            if (self.proc_registry.get(symbol_key)) |proc| {
                return try self.generateCallToCompiledProc(proc, args_span, ret_layout);
            }

            // Fall back to inline path for non-recursive closures

            // Check if the symbol is bound to a lambda/closure in local scope
            if (self.lambda_bindings.get(symbol_key)) |lambda_expr_id| {
                const lambda_expr = self.store.getExpr(lambda_expr_id);

                return switch (lambda_expr) {
                    .lambda => |lambda| try self.generateLambdaCall(lambda, args_span, ret_layout),
                    .closure => |closure| try self.generateClosureCallWithSymbol(closure, args_span, ret_layout, lookup.symbol),
                    else => return Error.UnsupportedExpression,
                };
            }

            // Look up the function in top-level definitions
            if (self.store.getSymbolDef(lookup.symbol)) |def_expr_id| {
                const def_expr = self.store.getExpr(def_expr_id);

                return switch (def_expr) {
                    .lambda => |lambda| try self.generateLambdaCall(lambda, args_span, ret_layout),
                    .closure => |closure| try self.generateClosureCallWithSymbol(closure, args_span, ret_layout, lookup.symbol),
                    else => return Error.UnsupportedExpression,
                };
            }

            return Error.LocalNotFound;
        }

        /// Generate a call to an already-compiled procedure.
        /// This is used for recursive functions that were compiled via compileAllProcs.
        fn generateCallToCompiledProc(self: *Self, proc: CompiledProc, args_span: anytype, _: layout.Idx) Error!ValueLocation {
            // Evaluate arguments and place them in argument registers
            const args = self.store.getExprSpan(args_span);
            for (args, 0..) |arg_id, i| {
                const arg_loc = try self.generateExpr(arg_id);
                const arg_reg = self.getArgumentRegister(@intCast(i));
                try self.moveToReg(arg_loc, arg_reg);
            }

            // Emit the call instruction
            try self.emitCallToOffset(proc.code_start);

            // Result is in the return register - mark it as allocated so it won't
            // be reused before we're done with it
            const ret_reg = self.getReturnRegister();
            self.codegen.markRegisterInUse(ret_reg);
            return .{ .general_reg = ret_reg };
        }

        /// Move a value to a specific register
        fn moveToReg(self: *Self, loc: ValueLocation, target_reg: GeneralReg) Error!void {
            switch (loc) {
                .general_reg => |src_reg| {
                    if (src_reg != target_reg) {
                        try self.emitMovRegReg(target_reg, src_reg);
                    }
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(target_reg, val);
                },
                .immediate_i128 => |val| {
                    // Only load low 64 bits into single register
                    const low: i64 = @truncate(val);
                    try self.codegen.emitLoadImm(target_reg, low);
                },
                .stack => |offset| {
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .stack_i128 => |offset| {
                    // Only load low 64 bits
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .float_reg, .immediate_f64 => {
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Ensure a value is in a general-purpose register
        fn ensureInGeneralReg(self: *Self, loc: ValueLocation) Error!GeneralReg {
            switch (loc) {
                .general_reg => |reg| return reg,
                .immediate_i64 => |val| {
                    const reg = try self.codegen.allocGeneralFor(0);
                    try self.codegen.emitLoadImm(reg, val);
                    return reg;
                },
                .immediate_i128 => |val| {
                    // Only load low 64 bits
                    const reg = try self.codegen.allocGeneralFor(0);
                    const low: i64 = @truncate(val);
                    try self.codegen.emitLoadImm(reg, low);
                    return reg;
                },
                .stack => |offset| {
                    const reg = try self.codegen.allocGeneralFor(0);
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .stack_i128 => |offset| {
                    // Only load low 64 bits
                    const reg = try self.codegen.allocGeneralFor(0);
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .float_reg, .immediate_f64 => {
                    // Convert float to int - this shouldn't happen in normal code
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Ensure a value is in a floating-point register
        fn ensureInFloatReg(self: *Self, loc: ValueLocation) Error!FloatReg {
            switch (loc) {
                .float_reg => |reg| return reg,
                .immediate_f64 => |val| {
                    const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                    const bits: u64 = @bitCast(val);

                    if (bits == 0) {
                        // Special case: 0.0 can be loaded efficiently
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.fmovFloatFromGen(.double, reg, .ZRSP);
                        } else {
                            try self.codegen.emit.xorpdRegReg(reg, reg);
                        }
                    } else {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            // Load bits into scratch register, then FMOV to float register
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(bits));
                            try self.codegen.emit.fmovFloatFromGen(.double, reg, .IP0);
                        } else {
                            // x86_64: Store bits to stack, then load into float register
                            // Use a temporary stack slot
                            const stack_offset: i32 = -16; // Below any local variables
                            try self.codegen.emit.movRegImm64(.R11, @bitCast(bits));
                            try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .R11);
                            try self.codegen.emit.movsdRegMem(reg, .RBP, stack_offset);
                        }
                    }
                    return reg;
                },
                .stack => |offset| {
                    const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                    try self.codegen.emitLoadStackF64(reg, offset);
                    return reg;
                },
                .general_reg, .immediate_i64, .immediate_i128, .stack_i128 => {
                    // Convert int to float - not supported
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Store the result to the output buffer pointed to by a saved register
        /// This is used when the original result pointer (X0/RDI) may have been clobbered
        fn storeResultToSavedPtr(self: *Self, loc: ValueLocation, result_layout: layout.Idx, saved_ptr_reg: GeneralReg) Error!void {
            switch (result_layout) {
                .i64, .i32, .i16, .i8, .u64, .u32, .u16, .u8, .bool => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(saved_ptr_reg, reg);
                },
                .f64 => {
                    switch (loc) {
                        .float_reg => |reg| {
                            try self.emitStoreFloatToMem(saved_ptr_reg, reg);
                        },
                        .immediate_f64 => |val| {
                            const bits: i64 = @bitCast(val);
                            const reg = try self.codegen.allocGeneralFor(0);
                            try self.codegen.emitLoadImm(reg, bits);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                            self.codegen.freeGeneral(reg);
                        },
                        else => {
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                        },
                    }
                },
                .f32 => {
                    // F32: Convert from F64 and store 4 bytes
                    switch (loc) {
                        .float_reg => |reg| {
                            // Convert F64 to F32, then store 4 bytes
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.fcvtFloatFloat(.single, reg, .double, reg);
                                try self.codegen.emit.fstrRegMemUoff(.single, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.cvtsd2ssRegReg(reg, reg);
                                try self.codegen.emit.movssMemReg(saved_ptr_reg, 0, reg);
                            }
                        },
                        .immediate_f64 => |val| {
                            // Convert to f32 bits and store 4 bytes
                            const f32_val: f32 = @floatCast(val);
                            const bits: u32 = @bitCast(f32_val);
                            const reg = try self.codegen.allocGeneralFor(0);
                            try self.codegen.emitLoadImm(reg, @as(i64, bits));
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.strRegMemUoff(.w32, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.movMemReg(.w32, saved_ptr_reg, 0, reg);
                            }
                            self.codegen.freeGeneral(reg);
                        },
                        else => {
                            // Store 4 bytes from general register
                            const reg = try self.ensureInGeneralReg(loc);
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.strRegMemUoff(.w32, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.movMemReg(.w32, saved_ptr_reg, 0, reg);
                            }
                        },
                    }
                },
                .i128, .u128, .dec => {
                    try self.storeI128ToMem(saved_ptr_reg, loc);
                },
                else => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(saved_ptr_reg, reg);
                },
            }
        }

        /// Store 128-bit value to memory at [ptr_reg]
        fn storeI128ToMem(self: *Self, ptr_reg: GeneralReg, loc: ValueLocation) Error!void {
            switch (loc) {
                .immediate_i128 => |val| {
                    // Store low 64 bits, then high 64 bits
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);

                    const reg = try self.codegen.allocGeneralFor(0);

                    // Store low 64 bits at [ptr]
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                    }

                    // Store high 64 bits at [ptr + 8]
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 1); // offset 1 = 8 bytes for u64
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, reg);
                    }

                    self.codegen.freeGeneral(reg);
                },
                .stack_i128 => |offset| {
                    // Copy 16 bytes from stack to destination
                    const reg = try self.codegen.allocGeneralFor(0);

                    // Load low 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                    }

                    // Load high 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset + 8);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 1);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, reg);
                    }

                    self.codegen.freeGeneral(reg);
                },
                else => {
                    // Fallback: store low 64 bits and zero high 64 bits
                    const reg = try self.ensureInGeneralReg(loc);

                    // Store low 64 bits
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                        // Store zero for high 64 bits
                        try self.codegen.emit.strRegMemUoff(.w64, .ZRSP, ptr_reg, 1);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                        // Store zero for high 64 bits
                        try self.codegen.emitLoadImm(reg, 0);
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, reg);
                    }
                },
            }
        }

        /// Store general register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreToMem(self: *Self, ptr_reg: anytype, src_reg: GeneralReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.strRegMemUoff(.w64, src_reg, ptr_reg, 0);
            } else {
                try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, src_reg);
            }
        }

        /// Store float register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreFloatToMem(self: *Self, ptr_reg: anytype, src_reg: FloatReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.fstrRegMemUoff(.double, src_reg, ptr_reg, 0);
            } else {
                try self.codegen.emit.movsdMemReg(ptr_reg, 0, src_reg);
            }
        }

        /// Compile all procedures first, before generating any calls.
        /// This ensures all call targets are known before we need to patch calls.
        pub fn compileAllProcs(self: *Self, procs: []const MonoProc) Error!void {
            for (procs) |proc| {
                try self.compileProc(proc);
            }
        }

        /// Compile a single procedure as a complete unit.
        /// Generates prologue, body, and epilogue (including RET).
        fn compileProc(self: *Self, proc: MonoProc) Error!void {
            const code_start = self.codegen.currentOffset();

            // CRITICAL: Register the procedure BEFORE generating the body
            // so that recursive calls within the body can find this procedure.
            // We'll update code_end after generation is complete.
            const key: u48 = @bitCast(proc.name);
            try self.proc_registry.put(key, .{
                .code_start = code_start,
                .code_end = 0, // Placeholder, updated below
                .name = proc.name,
            });

            // Generate function prologue (save frame, allocate stack)
            try self.emitPrologue();

            // Set up recursive context
            const old_recursive_symbol = self.current_recursive_symbol;
            const old_recursive_join_point = self.current_recursive_join_point;

            switch (proc.is_self_recursive) {
                .self_recursive => |join_point_id| {
                    self.current_recursive_symbol = proc.name;
                    self.current_recursive_join_point = join_point_id;
                },
                .not_self_recursive => {},
            }

            // Bind parameters to argument registers
            try self.bindProcParams(proc.args, proc.arg_layouts);

            // Generate the body (control flow statements)
            // Note: .ret statements in the body will emit epilogue+ret
            try self.generateStmt(proc.body);

            // Restore recursive context
            self.current_recursive_symbol = old_recursive_symbol;
            self.current_recursive_join_point = old_recursive_join_point;

            // Update the code_end now that generation is complete
            const code_end = self.codegen.currentOffset();
            if (self.proc_registry.getPtr(key)) |entry| {
                entry.code_end = code_end;
            }
        }

        /// Fixed stack frame size for procedures (includes space for spills)
        /// Note: On aarch64, stp/ldp use 7-bit signed scaled offsets.
        /// Max frame size is 63 * 8 = 504 bytes. We use 48 bytes for locals.
        const PROC_STACK_SIZE: i32 = 48;

        /// Emit function prologue (architecture-specific).
        /// Sets up the stack frame for the function, including space for local variables.
        fn emitPrologue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // AArch64 prologue:
                // stp x29, x30, [sp, #-(16+STACK_SIZE)]!  ; Save FP/LR and allocate stack
                // mov x29, sp                             ; Set up new frame pointer
                // Total frame = 16 (FP/LR) + PROC_STACK_SIZE (locals)
                // stp offset is in units of 8 bytes (scaled)
                const total_frame = 16 + PROC_STACK_SIZE;
                const scaled_offset: i7 = @intCast(@divExact(-total_frame, 8));
                try self.codegen.emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, scaled_offset);
                try self.codegen.emit.movRegReg(.w64, .FP, .ZRSP);
                // Reset stack_offset to account for the pre-allocated space
                // Stack slots start at FP+16 (above saved FP/LR) and go up
                self.codegen.stack_offset = 16; // First slot at [FP+16]
            } else {
                // x86_64 prologue:
                // push rbp                    ; Save frame pointer
                // mov rbp, rsp                ; Set up new frame pointer
                // sub rsp, PROC_STACK_SIZE   ; Allocate stack space
                try self.codegen.emit.push(.RBP);
                try self.codegen.emit.movRegReg(.w64, .RBP, .RSP);
                try self.codegen.emit.subRegImm32(.w64, .RSP, PROC_STACK_SIZE);
                // Stack slots are at negative offsets from RBP
                self.codegen.stack_offset = 0; // Will go negative
            }
        }

        /// Emit function epilogue (architecture-specific).
        /// Tears down the stack frame and returns.
        fn emitEpilogue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // AArch64 epilogue:
                // ldp x29, x30, [sp], #(16+STACK_SIZE)  ; Restore FP/LR and deallocate
                // ret                                   ; Return to caller
                // ldp offset is in units of 8 bytes (scaled)
                const total_frame = 16 + PROC_STACK_SIZE;
                const scaled_offset: i7 = @intCast(@divExact(total_frame, 8));
                try self.codegen.emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, scaled_offset);
                try self.codegen.emit.ret();
            } else {
                // x86_64 epilogue:
                // mov rsp, rbp                ; Restore stack pointer (deallocates locals)
                // pop rbp                     ; Restore frame pointer
                // ret                          ; Return to caller
                try self.codegen.emit.movRegReg(.w64, .RSP, .RBP);
                try self.codegen.emit.pop(.RBP);
                try self.codegen.emit.ret();
            }
        }

        /// Emit prologue for main expression code.
        /// Sets up frame pointer and saves callee-saved registers.
        /// The frame pointer is REQUIRED because emitStoreStack/emitLoadStack use FP-relative addressing.
        fn emitMainPrologue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // First, save FP and LR and establish frame pointer.
                // This is REQUIRED because stack slot accesses use FP-relative addressing.
                // stp x29, x30, [sp, #-16]!  (push FP and LR)
                try self.codegen.emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, -2);
                // mov x29, sp  (establish frame pointer)
                try self.codegen.emit.movRegReg(.w64, .FP, .ZRSP);

                // Now save X19 and X20 (callee-saved) which we use for result ptr and RocOps ptr.
                // stp x19, x20, [sp, #-16]!
                try self.codegen.emit.stpPreIndex(.w64, .X19, .X20, .ZRSP, -2);

                // CRITICAL: Initialize stack_offset to account for saved X19/X20 at [FP-16].
                // allocStackSlot decrements stack_offset and returns the new value.
                // With stack_offset = -16, first allocation of 16 bytes returns -32,
                // which is below the saved registers and won't corrupt them.
                self.codegen.stack_offset = -16;
            } else {
                // First, set up frame pointer.
                // This is REQUIRED because stack slot accesses use RBP-relative addressing.
                try self.codegen.emit.push(.RBP);
                try self.codegen.emit.movRegReg(.w64, .RBP, .RSP);

                // Save RBX and R12 (callee-saved) which we use for result ptr and RocOps ptr
                try self.codegen.emit.push(.RBX);
                try self.codegen.emit.push(.R12);

                // CRITICAL: Initialize stack_offset to account for saved RBX at [RBP-8]
                // and R12 at [RBP-16]. With stack_offset = -16, first allocation returns -32.
                self.codegen.stack_offset = -16;
            }
        }

        /// Emit epilogue for main expression code.
        /// Restores callee-saved registers and frame pointer, then returns.
        fn emitMainEpilogue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // Restore X19 and X20
                // ldp x19, x20, [sp], #16
                try self.codegen.emit.ldpPostIndex(.w64, .X19, .X20, .ZRSP, 2);
                // Restore FP and LR
                // ldp x29, x30, [sp], #16
                try self.codegen.emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, 2);
                try self.codegen.emit.ret();
            } else {
                // Restore R12 and RBX (in reverse order of push)
                try self.codegen.emit.pop(.R12);
                try self.codegen.emit.pop(.RBX);
                // Restore frame pointer
                try self.codegen.emit.pop(.RBP);
                try self.codegen.emit.ret();
            }
        }

        /// Bind procedure parameters to argument registers
        fn bindProcParams(self: *Self, params: mono.MonoPatternSpan, param_layouts: LayoutIdxSpan) Error!void {
            const pattern_ids = self.store.getPatternSpan(params);
            _ = param_layouts; // Layout info for proper ABI handling

            for (pattern_ids, 0..) |pattern_id, i| {
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        const arg_reg = self.getArgumentRegister(@intCast(i));
                        const symbol_key: u48 = @bitCast(bind.symbol);
                        try self.symbol_locations.put(symbol_key, .{ .general_reg = arg_reg });
                        // IMPORTANT: Mark the register as in use so the allocator won't
                        // reuse it for other values. Without this, loading a literal
                        // might clobber the parameter value.
                        self.codegen.markRegisterInUse(arg_reg);
                    },
                    else => {
                        // Complex parameter patterns not yet supported
                    },
                }
            }
        }

        /// Generate code for a control flow statement
        fn generateStmt(self: *Self, stmt_id: CFStmtId) Error!void {
            const stmt = self.store.getCFStmt(stmt_id);

            switch (stmt) {
                .let_stmt => |let_s| {
                    // Evaluate the value
                    const value_loc = try self.generateExpr(let_s.value);
                    // Bind to pattern
                    try self.bindPattern(let_s.pattern, value_loc);
                    // Continue with next statement
                    try self.generateStmt(let_s.next);
                },

                .join => |j| {
                    // Set up storage for join point parameters (they'll be rebound on each jump)
                    try self.setupJoinPointParams(j.id, j.params);

                    // Initialize jump record list for this join point
                    const jp_key = @intFromEnum(j.id);
                    if (!self.join_point_jumps.contains(jp_key)) {
                        try self.join_point_jumps.put(jp_key, std.ArrayList(JumpRecord).empty);
                    }

                    // Generate REMAINDER first (code that eventually jumps TO the join point)
                    try self.generateStmt(j.remainder);

                    // Record where join point body starts (this is where jumps will target)
                    const join_location = self.codegen.currentOffset();
                    try self.join_points.put(jp_key, join_location);

                    // Generate BODY (what happens when jumped to)
                    try self.generateStmt(j.body);

                    // Patch all jumps to this join point
                    if (self.join_point_jumps.get(jp_key)) |jumps| {
                        for (jumps.items) |jump_record| {
                            self.codegen.patchJump(jump_record.location, join_location);
                        }
                    }
                },

                .jump => |jmp| {
                    // Evaluate all arguments first (before rebinding, in case args reference params)
                    const args = self.store.getExprSpan(jmp.args);
                    var arg_locs: std.ArrayListUnmanaged(ValueLocation) = .empty;
                    defer arg_locs.deinit(self.allocator);

                    for (args) |arg_id| {
                        const loc = try self.generateExpr(arg_id);
                        try arg_locs.append(self.allocator, loc);
                    }

                    // Rebind join point parameters to new argument values
                    try self.rebindJoinPointParams(jmp.target, arg_locs.items);

                    // Emit jump instruction with placeholder offset
                    const jump_location = self.codegen.currentOffset();
                    try self.emitJumpPlaceholder();

                    // Record for patching
                    const jp_key = @intFromEnum(jmp.target);
                    if (self.join_point_jumps.getPtr(jp_key)) |jumps| {
                        try jumps.append(self.allocator, .{ .location = jump_location });
                    }
                },

                .ret => |r| {
                    // Evaluate the return value
                    const value_loc = try self.generateExpr(r.value);
                    // Move to return register
                    const return_reg = self.getReturnRegister();
                    const value_reg = try self.ensureInGeneralReg(value_loc);
                    if (value_reg != return_reg) {
                        try self.emitMovRegReg(return_reg, value_reg);
                    }
                    // Emit epilogue (restores frame and returns)
                    try self.emitEpilogue();
                },

                .expr_stmt => |e| {
                    // Evaluate expression for side effects
                    _ = try self.generateExpr(e.value);
                    // Continue with next
                    try self.generateStmt(e.next);
                },

                .switch_stmt => |sw| {
                    try self.generateSwitchStmt(sw);
                },
            }
        }

        /// Set up storage locations for join point parameters
        fn setupJoinPointParams(self: *Self, _: JoinPointId, params: mono.MonoPatternSpan) Error!void {
            const pattern_ids = self.store.getPatternSpan(params);

            // For each parameter, allocate a register or stack slot
            for (pattern_ids, 0..) |pattern_id, i| {
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        // Use argument registers for parameters
                        const reg = self.getArgumentRegister(@intCast(i));
                        const symbol_key: u48 = @bitCast(bind.symbol);
                        try self.symbol_locations.put(symbol_key, .{ .general_reg = reg });
                    },
                    else => {},
                }
            }
        }

        /// Rebind join point parameters to new argument values (for jump)
        fn rebindJoinPointParams(self: *Self, _: JoinPointId, arg_locs: []const ValueLocation) Error!void {
            // Move argument values to their parameter registers
            // This needs to be done carefully to avoid clobbering values we still need
            // For now, simple sequential assignment (works when args don't alias params)
            for (arg_locs, 0..) |loc, i| {
                const dst_reg = self.getArgumentRegister(@intCast(i));
                const src_reg = try self.ensureInGeneralReg(loc);
                if (src_reg != dst_reg) {
                    try self.emitMovRegReg(dst_reg, src_reg);
                }
            }
        }

        /// Emit a jump placeholder (will be patched later)
        fn emitJumpPlaceholder(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B instruction with offset 0 (will be patched)
                try self.codegen.emit.b(0);
            } else {
                // JMP rel32 with offset 0 (will be patched)
                try self.codegen.emit.jmp(0);
            }
        }

        /// Generate code for a switch statement
        fn generateSwitchStmt(self: *Self, sw: anytype) Error!void {
            // Evaluate condition
            const cond_loc = try self.generateExpr(sw.cond);
            const cond_reg = try self.ensureInGeneralReg(cond_loc);

            const branches = self.store.getCFSwitchBranches(sw.branches);

            // For single branch (bool switch): compare and branch
            if (branches.len == 1) {
                const branch = branches[0];

                // Compare with branch value and jump if NOT equal (to default)
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cond_reg, @intCast(branch.value));
                } else {
                    try self.codegen.emit.cmpRegImm32(.w64, cond_reg, @intCast(branch.value));
                }

                // Jump to default if not equal
                const else_patch = try self.emitJumpIfNotEqual();

                self.codegen.freeGeneral(cond_reg);

                // Generate branch body (recursively generates statements)
                try self.generateStmt(branch.body);

                // Patch else jump to here
                const else_offset = self.codegen.currentOffset();
                self.codegen.patchJump(else_patch, else_offset);

                // Generate default branch
                try self.generateStmt(sw.default_branch);
            } else {
                // Multiple branches - generate cascading comparisons
                var end_patches = std.ArrayList(usize).empty;
                defer end_patches.deinit(self.allocator);

                for (branches, 0..) |branch, i| {
                    if (i < branches.len - 1) {
                        // Compare and skip if not match
                        try self.emitCmpImm(cond_reg, @intCast(branch.value));
                        const skip_patch = try self.emitJumpIfNotEqual();

                        // Generate branch body
                        try self.generateStmt(branch.body);

                        // Jump to end
                        const end_patch = try self.codegen.emitJump();
                        try end_patches.append(self.allocator, end_patch);

                        // Patch skip
                        const skip_offset = self.codegen.currentOffset();
                        self.codegen.patchJump(skip_patch, skip_offset);
                    } else {
                        // Last branch before default
                        try self.emitCmpImm(cond_reg, @intCast(branch.value));
                        const skip_patch = try self.emitJumpIfNotEqual();

                        try self.generateStmt(branch.body);

                        const end_patch = try self.codegen.emitJump();
                        try end_patches.append(self.allocator, end_patch);

                        self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
                    }
                }

                self.codegen.freeGeneral(cond_reg);

                // Generate default branch
                try self.generateStmt(sw.default_branch);

                // Patch all end jumps
                const end_offset = self.codegen.currentOffset();
                for (end_patches.items) |patch| {
                    self.codegen.patchJump(patch, end_offset);
                }
            }
        }

        /// Patch all pending calls after all procedures are compiled
        pub fn patchPendingCalls(self: *Self) Error!void {
            for (self.pending_calls.items) |pending| {
                const key: u48 = @bitCast(pending.target_symbol);
                const proc = self.proc_registry.get(key) orelse {
                    return Error.LocalNotFound; // Function not found
                };
                self.patchCallTarget(pending.call_site, proc.code_start);
            }
        }

        /// Patch a call instruction to target a specific offset
        fn patchCallTarget(self: *Self, call_site: usize, target_offset: usize) void {
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(call_site)));

            if (comptime builtin.cpu.arch == .aarch64) {
                // BL instruction: patch the immediate offset
                // BL uses 26-bit signed offset in instructions (multiply by 4)
                const instr_offset = @divTrunc(rel_offset, 4);
                self.codegen.patchBL(call_site, instr_offset);
            } else {
                // CALL rel32: patch the 32-bit relative offset
                // Offset is relative to instruction after CALL (call_site + 5)
                const call_rel = rel_offset - 5;
                self.codegen.patchCall(call_site, call_rel);
            }
        }
    };
}

/// Select the appropriate code generator based on target architecture
pub const MonoExprCodeGen = if (builtin.cpu.arch == .aarch64)
    MonoExprCodeGenFor(aarch64.CodeGen.AArch64CodeGen, aarch64.GeneralReg, aarch64.FloatReg, aarch64.Emit.Condition)
else if (builtin.cpu.arch == .x86_64)
    MonoExprCodeGenFor(x86_64.CodeGen.SystemVCodeGen, x86_64.GeneralReg, x86_64.FloatReg, x86_64.Emit.Condition)
else
    UnsupportedArchCodeGen;

/// Stub code generator for unsupported architectures.
/// This allows the code to compile for cross-compilation targets like 32-bit ARM/x86,
/// but will error at runtime if actually used.
pub const UnsupportedArchCodeGen = struct {
    const Self = @This();

    pub const Error = error{
        UnsupportedArchitecture,
        UnsupportedExpression,
        OutOfMemory,
    };

    pub const CodeResult = struct {
        code: []const u8,
        entry_offset: usize,
    };

    allocator: Allocator,

    pub fn init(
        allocator: Allocator,
        _: *const MonoExprStore,
        _: ?*StaticDataInterner,
    ) Self {
        return .{ .allocator = allocator };
    }

    pub fn deinit(_: *Self) void {}

    pub fn compileAllProcs(_: *Self, _: anytype) Error!void {
        return error.UnsupportedArchitecture;
    }

    pub fn generateCode(_: *Self, _: anytype, _: anytype) Error!CodeResult {
        return error.UnsupportedArchitecture;
    }

    pub fn generateExpr(_: *Self, _: anytype) Error!void {
        return error.UnsupportedArchitecture;
    }

    pub fn generateProc(_: *Self, _: anytype) Error!void {
        return error.UnsupportedArchitecture;
    }

    pub fn finalize(_: *Self) Error![]const u8 {
        return error.UnsupportedArchitecture;
    }

    pub fn getCode(_: *const Self) []const u8 {
        return &[_]u8{};
    }
};

// Tests

test "code generator initialization" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    var codegen = MonoExprCodeGen.init(allocator, &store, null);
    defer codegen.deinit();
}

test "generate i64 literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Add an i64 literal
    const expr_id = try store.addExpr(.{ .i64_literal = 42 }, base.Region.zero());

    var codegen = MonoExprCodeGen.init(allocator, &store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64);
    defer allocator.free(result.code);

    // Should have generated some code
    try std.testing.expect(result.code.len > 0);
}

test "generate bool literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    const expr_id = try store.addExpr(.{ .bool_literal = true }, base.Region.zero());

    var codegen = MonoExprCodeGen.init(allocator, &store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .bool);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate addition" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create: 1 + 2
    const lhs_id = try store.addExpr(.{ .i64_literal = 1 }, base.Region.zero());
    const rhs_id = try store.addExpr(.{ .i64_literal = 2 }, base.Region.zero());
    const add_id = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = lhs_id,
        .rhs = rhs_id,
        .result_layout = .i64,
    } }, base.Region.zero());

    var codegen = MonoExprCodeGen.init(allocator, &store, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(add_id, .i64);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}
