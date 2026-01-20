//! Mono IR Code Generator for x86_64
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
//! - Uses real x86_64 instructions via Emit.zig
//! - Proper register allocation with spilling support
//! - Handles System V ABI calling convention
//! - Generates position-independent code with relocations

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const mono = @import("mono");

const x86_64 = @import("x86_64/mod.zig");
const Emit = x86_64.Emit;
const CodeGen = x86_64.CodeGen;
const GeneralReg = x86_64.GeneralReg;
const FloatReg = x86_64.FloatReg;
const RegisterWidth = x86_64.RegisterWidth;

/// Check if we're on a supported architecture for code generation
fn isSupportedArch() bool {
    return builtin.cpu.arch == .x86_64;
}

const Relocation = @import("Relocation.zig").Relocation;
const StaticDataInterner = @import("StaticDataInterner.zig");

const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoExpr;
const MonoPattern = mono.MonoPattern;
const MonoExprId = mono.MonoExprId;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;

const Allocator = std.mem.Allocator;

/// Code generator for Mono IR expressions
pub const MonoExprCodeGen = struct {
    const Self = @This();

    allocator: Allocator,

    /// x86_64 code generator with register allocation
    codegen: CodeGen.SystemVCodeGen,

    /// The Mono IR store containing expressions to compile
    store: *const MonoExprStore,

    /// Static data interner for string literals
    static_interner: ?*StaticDataInterner,

    /// Map from MonoSymbol to value location (register or stack slot)
    symbol_locations: std.AutoHashMap(u48, ValueLocation),

    /// Where a value is stored
    const ValueLocation = union(enum) {
        /// Value is in a general-purpose register
        general_reg: GeneralReg,
        /// Value is in a float register
        float_reg: FloatReg,
        /// Value is on the stack at given offset from RBP
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
            .codegen = CodeGen.SystemVCodeGen.init(allocator),
            .store = store,
            .static_interner = static_interner,
            .symbol_locations = std.AutoHashMap(u48, ValueLocation).init(allocator),
        };
    }

    /// Clean up resources
    pub fn deinit(self: *Self) void {
        self.codegen.deinit();
        self.symbol_locations.deinit();
    }

    /// Reset the code generator for generating a new expression
    pub fn reset(self: *Self) void {
        self.codegen.reset();
        self.symbol_locations.clearRetainingCapacity();
    }

    /// Generate code for a Mono IR expression
    ///
    /// The generated code follows the System V ABI:
    /// - RDI contains the pointer to the result buffer
    /// - RSI contains the pointer to RocOps
    /// - The function writes the result to [RDI] and returns
    pub fn generateCode(
        self: *Self,
        expr_id: MonoExprId,
        result_layout: layout.Idx,
    ) Error!CodeResult {
        // Check architecture support
        if (!isSupportedArch()) {
            return Error.UnsupportedExpression;
        }

        // Reserve RDI for result pointer and RSI for RocOps
        // These are passed as arguments and should not be used for temps
        // (The codegen automatically handles this through caller-saved regs)

        // Generate code for the expression - result ends up in a register
        const result_loc = try self.generateExpr(expr_id);

        // Store result to [RDI] (the result pointer passed by caller)
        try self.storeResult(result_loc, result_layout);

        // Emit function epilogue and return
        try self.codegen.emit.ret();

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
            .when => |when| try self.generateWhen(when),

            // Blocks
            .block => |block| try self.generateBlock(block),

            // Function calls
            .call => |call| try self.generateCall(call),

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
                // Integer division requires special handling (uses RAX and RDX)
                // TODO: Implement proper idiv
                try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg); // placeholder
            },
            .mod => {
                // Modulo also uses idiv
                // TODO: Implement proper idiv remainder
                try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg); // placeholder
            },
            // Comparison operations
            .eq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, .equal),
            .neq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, .not_equal),
            .lt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, .less),
            .lte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, .less_or_equal),
            .gt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, .greater),
            .gte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, .greater_or_equal),
            // Boolean operations
            .@"and" => try self.codegen.emit.andRegReg(.w64, result_reg, rhs_reg),
            .@"or" => try self.codegen.emit.orRegReg(.w64, result_reg, rhs_reg),
        }

        // Free operand registers if they were temporary
        self.codegen.freeGeneral(lhs_reg);
        self.codegen.freeGeneral(rhs_reg);

        return .{ .general_reg = result_reg };
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

        // XOR reg, 1 to flip the bit
        try self.codegen.emit.movRegReg(.w64, result_reg, reg);
        try self.codegen.emit.xorRegReg(.w64, result_reg, reg);
        // Actually we need XOR with 1 constant - simplified for now
        // Just use NOT for boolean
        try self.codegen.emit.notReg(.w64, result_reg);
        // Then AND with 1 to ensure only lowest bit
        try self.codegen.emit.andRegReg(.w64, result_reg, result_reg); // placeholder

        self.codegen.freeGeneral(reg);
        return .{ .general_reg = result_reg };
    }

    /// Generate code for if-then-else
    fn generateIfThenElse(self: *Self, ite: anytype) Error!ValueLocation {
        const branches = self.store.getIfBranches(ite.branches);

        // Allocate result register
        const result_reg = try self.codegen.allocGeneralFor(0);

        // Collect jump targets for patching
        var else_patches = std.ArrayList(usize).empty;
        defer else_patches.deinit(self.allocator);

        var end_patches = std.ArrayList(usize).empty;
        defer end_patches.deinit(self.allocator);

        // Generate each branch
        for (branches) |branch| {
            // Generate condition
            const cond_loc = try self.generateExpr(branch.cond);
            const cond_reg = try self.ensureInGeneralReg(cond_loc);

            // Test condition: cmp reg, 0; je else_branch
            try self.codegen.emit.cmpRegImm32(.w64, cond_reg, 0);
            const else_patch = try self.codegen.emitCondJump(.equal);
            try else_patches.append(self.allocator, else_patch);

            self.codegen.freeGeneral(cond_reg);

            // Generate body
            const body_loc = try self.generateExpr(branch.body);
            const body_reg = try self.ensureInGeneralReg(body_loc);
            try self.codegen.emit.movRegReg(.w64, result_reg, body_reg);
            self.codegen.freeGeneral(body_reg);

            // Jump to end
            const end_patch = try self.codegen.emitJump();
            try end_patches.append(self.allocator, end_patch);

            // Patch the else jump to here
            const current_offset = self.codegen.currentOffset();
            self.codegen.patchJump(else_patches.items[else_patches.items.len - 1], current_offset);
        }

        // Generate final else
        const else_loc = try self.generateExpr(ite.final_else);
        const else_reg = try self.ensureInGeneralReg(else_loc);
        try self.codegen.emit.movRegReg(.w64, result_reg, else_reg);
        self.codegen.freeGeneral(else_reg);

        // Patch all end jumps to here
        const end_offset = self.codegen.currentOffset();
        for (end_patches.items) |patch| {
            self.codegen.patchJump(patch, end_offset);
        }

        return .{ .general_reg = result_reg };
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
            // Generate code for the expression
            const expr_loc = try self.generateExpr(stmt.expr);

            // Bind the result to the pattern
            try self.bindPattern(stmt.pattern, expr_loc);
        }

        // Generate the final expression
        return self.generateExpr(block.final_expr);
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

    /// Generate code for a function call
    fn generateCall(self: *Self, call: anytype) Error!ValueLocation {
        _ = self;
        _ = call;
        // TODO: Implement function calls with proper ABI
        return Error.UnsupportedExpression;
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
                // For now, use a simpler approach: load via integer and convert
                const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                // TODO: Proper float literal loading via static data
                _ = val;
                try self.codegen.emit.xorpdRegReg(reg, reg); // zero for now
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

    /// Store the result to the output buffer pointed to by RDI
    fn storeResult(self: *Self, loc: ValueLocation, result_layout: layout.Idx) Error!void {
        switch (result_layout) {
            .i64, .i32, .i16, .i8, .u64, .u32, .u16, .u8, .bool => {
                const reg = try self.ensureInGeneralReg(loc);
                // mov [rdi], reg
                try self.codegen.emit.movMemReg(.w64, .RDI, 0, reg);
            },
            .f64, .f32 => {
                switch (loc) {
                    .float_reg => |reg| {
                        // movsd [rdi], xmm
                        try self.codegen.emit.movsdMemReg(.RDI, 0, reg);
                    },
                    .immediate_f64 => |val| {
                        // Store via integer register
                        const bits: i64 = @bitCast(val);
                        const reg = try self.codegen.allocGeneralFor(0);
                        try self.codegen.emitLoadImm(reg, bits);
                        try self.codegen.emit.movMemReg(.w64, .RDI, 0, reg);
                        self.codegen.freeGeneral(reg);
                    },
                    else => {
                        const reg = try self.ensureInGeneralReg(loc);
                        try self.codegen.emit.movMemReg(.w64, .RDI, 0, reg);
                    },
                }
            },
            .i128, .u128, .dec => {
                // 128-bit values need two 64-bit stores
                const reg = try self.ensureInGeneralReg(loc);
                // Store low 64 bits
                try self.codegen.emit.movMemReg(.w64, .RDI, 0, reg);
                // TODO: Store high 64 bits
                // For now, zero the high bits
                try self.codegen.emit.movMemImm32(.w64, .RDI, 8, 0);
            },
            else => {
                // For other types, just do a basic store
                const reg = try self.ensureInGeneralReg(loc);
                try self.codegen.emit.movMemReg(.w64, .RDI, 0, reg);
            },
        }
    }
};

// Tests

test "code generator initialization" {
    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    var codegen = MonoExprCodeGen.init(allocator, &store, null);
    defer codegen.deinit();
}

test "generate i64 literal" {
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

    // Should end with ret (0xC3)
    try std.testing.expectEqual(@as(u8, 0xC3), result.code[result.code.len - 1]);
}

test "generate bool literal" {
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
