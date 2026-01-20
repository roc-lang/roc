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

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

const Relocation = @import("Relocation.zig").Relocation;
const StaticDataInterner = @import("StaticDataInterner.zig");

const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoExpr;
const MonoPattern = mono.MonoPattern;
const MonoExprId = mono.MonoExprId;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;
const ClosureRepresentation = mono.ClosureRepresentation;
const MonoCapture = mono.MonoCapture;
const Recursive = mono.Recursive;
const SelfRecursive = mono.SelfRecursive;
const JoinPointId = mono.JoinPointId;

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

        /// Where a value is stored
        pub const ValueLocation = union(enum) {
            /// Value is in a general-purpose register
            general_reg: GeneralReg,
            /// Value is in a float register
            float_reg: FloatReg,
            /// Value is on the stack at given offset from frame pointer
            stack: i32,
            /// Immediate value known at compile time
            immediate_i64: i64,
            /// Immediate float value
            immediate_f64: f64,
        };

        /// Result of code generation
        pub const CodeResult = struct {
            /// Generated machine code
            code: []const u8,
            /// Relocations for external references
            relocations: []const Relocation,
            /// Layout of the result
            result_layout: layout.Idx,
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
            };
        }

        /// Clean up resources
        pub fn deinit(self: *Self) void {
            self.codegen.deinit();
            self.symbol_locations.deinit();
            self.lambda_bindings.deinit();
            self.join_points.deinit();
        }

        /// Reset the code generator for generating a new expression
        pub fn reset(self: *Self) void {
            self.codegen.reset();
            self.symbol_locations.clearRetainingCapacity();
            self.lambda_bindings.clearRetainingCapacity();
            self.join_points.clearRetainingCapacity();
            self.current_recursive_symbol = null;
            self.current_recursive_join_point = null;
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
            // Reserve argument registers so they don't get allocated for temporaries
            // X0/RDI = result pointer, X1/RSI = RocOps pointer
            self.reserveArgumentRegisters();

            // Generate code for the expression - result ends up in a register
            const result_loc = try self.generateExpr(expr_id);

            // Store result to the result pointer (first argument register)
            try self.storeResult(result_loc, result_layout);

            // Emit return
            try self.emitRet();

            // Get the generated code
            const code = self.codegen.getCode();

            // Make a copy of the code since codegen buffer may be reused
            const code_copy = self.allocator.dupe(u8, code) catch return Error.OutOfMemory;

            return CodeResult{
                .code = code_copy,
                .relocations = self.codegen.relocations.items,
                .result_layout = result_layout,
            };
        }

        /// Reserve argument registers so they don't get allocated for temporaries
        fn reserveArgumentRegisters(self: *Self) void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // Clear X0 and X1 from the free register mask
                // X0 = bit 0, X1 = bit 1
                self.codegen.free_general &= ~@as(u32, 0b11);
            } else {
                // Clear RDI and RSI from the free register mask
                // RDI = 7, RSI = 6
                const rdi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RDI);
                const rsi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RSI);
                self.codegen.free_general &= ~(rdi_bit | rsi_bit);
            }
        }

        /// Emit return instruction (architecture-specific)
        fn emitRet(self: *Self) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ret();
            } else {
                try self.codegen.emit.ret();
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
        fn generateI128Literal(self: *Self, val: i128) Error!ValueLocation {
            // For i128, we need two registers (low and high 64 bits)
            // For now, just handle the low 64 bits
            // TODO: Full i128 support
            const low: i64 = @truncate(val);
            const reg = try self.codegen.allocGeneralFor(0);
            try self.codegen.emitLoadImm(reg, low);
            return .{ .general_reg = reg };
        }

        /// Generate code for a symbol lookup
        fn generateLookup(self: *Self, symbol: MonoSymbol, layout_idx: layout.Idx) Error!ValueLocation {
            _ = layout_idx;

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
            // Generate code for both operands
            const lhs_loc = try self.generateExpr(binop.lhs);
            const rhs_loc = try self.generateExpr(binop.rhs);

            // Determine if this is an integer or float operation
            const is_float = switch (binop.result_layout) {
                .f32, .f64 => true,
                else => false,
            };

            if (is_float) {
                return self.generateFloatBinop(binop.op, lhs_loc, rhs_loc);
            } else {
                return self.generateIntBinop(binop.op, lhs_loc, rhs_loc);
            }
        }

        /// Generate integer binary operation
        fn generateIntBinop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
        ) Error!ValueLocation {
            // Load LHS into a register
            const lhs_reg = try self.ensureInGeneralReg(lhs_loc);

            // Load RHS into a register
            const rhs_reg = try self.ensureInGeneralReg(rhs_loc);

            // Allocate result register
            const result_reg = try self.codegen.allocGeneralFor(0);

            switch (op) {
                .add => try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSub(.w64, result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg),
                .div => {
                    // Integer division - use mul as placeholder for now
                    // TODO: Implement proper idiv
                    try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg);
                },
                .mod => {
                    // Modulo - use mul as placeholder for now
                    // TODO: Implement proper idiv remainder
                    try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg);
                },
                // Comparison operations
                .eq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condEqual()),
                .neq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condNotEqual()),
                .lt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condLess()),
                .lte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condLessOrEqual()),
                .gt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condGreater()),
                .gte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condGreaterOrEqual()),
                // Boolean operations - AND/OR two values
                .@"and", .@"or" => {
                    // Just use the result of lhs for now
                    // TODO: Proper boolean AND/OR
                    try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg);
                },
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
                    // Comparison and boolean ops for floats
                    // TODO: Implement float comparisons
                    try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg); // placeholder
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

            if (is_float) {
                // For floats, XOR with sign bit
                // TODO: Implement float negation properly
                return inner_loc;
            } else {
                // For integers, use NEG
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

            // Boolean NOT: XOR with 1
            const reg = try self.ensureInGeneralReg(inner_loc);
            const result_reg = try self.codegen.allocGeneralFor(0);

            // Load 1 and XOR to flip the bit
            // For simplicity, just use NEG and add 1
            try self.codegen.emitNeg(.w64, result_reg, reg);
            // TODO: Proper boolean NOT implementation

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
        fn generateWhen(self: *Self, when_expr: anytype) Error!ValueLocation {
            _ = self;
            _ = when_expr;
            // TODO: Implement pattern matching
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
                    // TODO: Handle destructuring patterns
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
        fn generateLambda(self: *Self, lambda: anytype) Error!ValueLocation {
            // A lambda by itself doesn't produce a value - it's a function definition
            // When called, we'll inline it. For now, store a reference to the expression.
            _ = self;
            _ = lambda;
            // Lambdas that aren't immediately called should be stored as closure data
            // For now, return a placeholder
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for a closure (lambda with captured environment)
        /// This creates the actual runtime closure value based on the representation.
        fn generateClosure(self: *Self, closure: anytype) Error!ValueLocation {
            switch (closure.representation) {
                .direct_call => {
                    // No captures - no runtime representation needed
                    // The closure is just a reference to the lambda body
                    return .{ .immediate_i64 = 0 };
                },
                .unwrapped_capture => |repr| {
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
                    _ = repr;
                    return .{ .immediate_i64 = 0 };
                },
                .struct_captures => |repr| {
                    // Multiple captures - allocate struct on stack and copy captures
                    const captures = self.store.getCaptures(repr.captures);

                    // For now, just evaluate all captures and return the first one
                    // TODO: Properly allocate stack space and store all captures
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
                    // Multiple functions with captures - create tagged union
                    // For now, just return the tag
                    _ = repr;
                    return .{ .immediate_i64 = 0 };
                },
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
        fn generateLambdaCall(self: *Self, lambda: anytype, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            _ = ret_layout;

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
        fn generateClosureCall(self: *Self, closure: anytype, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
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
                    // Multiple functions with same signature - need lambda set dispatch
                    // For now, just bind captures the traditional way
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

            // Get the lambda from the closure and call it
            const lambda_expr = self.store.getExpr(closure.lambda);

            return switch (lambda_expr) {
                .lambda => |lambda| try self.generateRecursiveLambdaCall(lambda, args_span, ret_layout, closure.self_recursive),
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for calling a closure with symbol tracking for recursion
        fn generateClosureCallWithSymbol(
            self: *Self,
            closure: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
            symbol: MonoSymbol,
        ) Error!ValueLocation {
            // Bind the captured values based on representation (same as generateClosureCall)
            switch (closure.representation) {
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
                .enum_dispatch, .union_repr => {
                    const captures = self.store.getCaptures(closure.captures);
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
            ret_layout: layout.Idx,
            self_recursive: SelfRecursive,
            symbol: ?MonoSymbol,
        ) Error!ValueLocation {
            _ = ret_layout;

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

                    // Store the parameter patterns for recursive jump to re-bind
                    // (stored in a field we'll add)

                    // Generate the body
                    const result = try self.generateExpr(lambda.body);

                    // Restore old recursive context
                    self.current_recursive_symbol = old_recursive_symbol;
                    self.current_recursive_join_point = old_recursive_join_point;

                    return result;
                },
            }
        }

        /// Generate code for a recursive jump (tail call to current recursive closure)
        fn generateRecursiveJump(self: *Self, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            _ = ret_layout;

            // Get the join point for the current recursive closure
            const join_point_id = self.current_recursive_join_point orelse return Error.LocalNotFound;
            const join_point_offset = self.join_points.get(@intFromEnum(join_point_id)) orelse return Error.LocalNotFound;

            // Get the arguments
            const args = self.store.getExprSpan(args_span);

            // For a proper recursive jump, we need to:
            // 1. Evaluate the new arguments
            // 2. Update the parameter bindings
            // 3. Jump back to the join point
            //
            // However, this requires knowing the parameter patterns, which we don't
            // have easily accessible here. For now, we evaluate arguments and
            // use a simpler approach: just emit a jump back to the join point.
            //
            // TODO: Store parameter patterns when entering recursive closure
            // and use them here to properly update bindings.

            // Evaluate arguments (for side effects and to get values)
            var arg_locations = std.ArrayList(ValueLocation).empty;
            defer arg_locations.deinit(self.allocator);

            for (args) |arg_id| {
                const arg_loc = try self.generateExpr(arg_id);
                try arg_locations.append(self.allocator, arg_loc);
            }

            // Emit unconditional jump back to join point
            // For now, this is a simple backward jump
            try self.emitJumpToOffset(join_point_offset);

            // Return a placeholder - this code path shouldn't be reached at runtime
            // because we jumped away
            return .{ .immediate_i64 = 0 };
        }

        /// Emit an unconditional jump to a specific code offset
        fn emitJumpToOffset(self: *Self, target_offset: usize) !void {
            const current = self.codegen.currentOffset();
            // Calculate relative offset (negative for backward jump)
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)));

            if (comptime builtin.cpu.arch == .aarch64) {
                // B instruction with relative offset
                // Note: AArch64 branch offsets are in instructions (4 bytes each)
                const instr_offset = @divTrunc(rel_offset - 4, 4); // -4 because PC is ahead
                try self.codegen.emit.b(@bitCast(instr_offset));
            } else {
                // JMP rel32
                // x86_64 jmp instruction: 0xE9 followed by 32-bit relative offset
                // The offset is relative to the instruction after the jmp (current + 5)
                const jmp_rel = rel_offset - 5;
                try self.codegen.emit.jmp(@bitCast(jmp_rel));
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

            // Check if this is a recursive call to the current closure
            if (self.current_recursive_symbol) |recursive_sym| {
                if (recursive_sym.eql(lookup.symbol)) {
                    // This is a recursive call - generate jump to join point instead of re-entering
                    return try self.generateRecursiveJump(args_span, ret_layout);
                }
            }

            // First check if the symbol is bound to a lambda/closure in local scope
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

        /// Ensure a value is in a general-purpose register
        fn ensureInGeneralReg(self: *Self, loc: ValueLocation) Error!GeneralReg {
            switch (loc) {
                .general_reg => |reg| return reg,
                .immediate_i64 => |val| {
                    const reg = try self.codegen.allocGeneralFor(0);
                    try self.codegen.emitLoadImm(reg, val);
                    return reg;
                },
                .stack => |offset| {
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
                    // Load float immediate via memory
                    // For now, zero the register
                    const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                    _ = val;
                    // TODO: Proper float literal loading via static data
                    // For now just zero the register
                    if (comptime builtin.cpu.arch == .aarch64) {
                        // Load zero for now
                        try self.codegen.emit.fmovFloatFromGen(.double, reg, .ZRSP);
                    } else {
                        try self.codegen.emit.xorpdRegReg(reg, reg);
                    }
                    return reg;
                },
                .stack => |offset| {
                    const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                    try self.codegen.emitLoadStackF64(reg, offset);
                    return reg;
                },
                .general_reg, .immediate_i64 => {
                    // Convert int to float
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Store the result to the output buffer pointed to by the first argument register
        fn storeResult(self: *Self, loc: ValueLocation, result_layout: layout.Idx) Error!void {
            // First argument register: X0 on aarch64, RDI on x86_64
            const result_ptr_reg = if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X0
            else
                x86_64.GeneralReg.RDI;

            switch (result_layout) {
                .i64, .i32, .i16, .i8, .u64, .u32, .u16, .u8, .bool => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(result_ptr_reg, reg);
                },
                .f64, .f32 => {
                    switch (loc) {
                        .float_reg => |reg| {
                            try self.emitStoreFloatToMem(result_ptr_reg, reg);
                        },
                        .immediate_f64 => |val| {
                            // Store via integer register
                            const bits: i64 = @bitCast(val);
                            const reg = try self.codegen.allocGeneralFor(0);
                            try self.codegen.emitLoadImm(reg, bits);
                            try self.emitStoreToMem(result_ptr_reg, reg);
                            self.codegen.freeGeneral(reg);
                        },
                        else => {
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(result_ptr_reg, reg);
                        },
                    }
                },
                .i128, .u128, .dec => {
                    // 128-bit values need two 64-bit stores
                    const reg = try self.ensureInGeneralReg(loc);
                    // Store low 64 bits
                    try self.emitStoreToMem(result_ptr_reg, reg);
                    // TODO: Store high 64 bits
                },
                else => {
                    // For other types, just do a basic store
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(result_ptr_reg, reg);
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
    };
}

/// Select the appropriate code generator based on target architecture
pub const MonoExprCodeGen = if (builtin.cpu.arch == .aarch64)
    MonoExprCodeGenFor(aarch64.CodeGen.AArch64CodeGen, aarch64.GeneralReg, aarch64.FloatReg, aarch64.Emit.Condition)
else if (builtin.cpu.arch == .x86_64)
    MonoExprCodeGenFor(x86_64.CodeGen.SystemVCodeGen, x86_64.GeneralReg, x86_64.FloatReg, x86_64.Emit.Condition)
else
    @compileError("Unsupported architecture for MonoExprCodeGen");

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
