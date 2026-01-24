//! Mono IR to LLVM Code Generator
//!
//! This module generates LLVM IR from Mono IR expressions.
//! It uses Zig's std.zig.llvm.Builder for IR generation.
//!
//! Pipeline position:
//! ```
//! CIR -> Mono IR Lowering -> MonoLlvmCodeGen -> LLVM Bitcode -> Native Code
//! ```
//!
//! Key properties:
//! - Consumes the same Mono IR as the dev backend
//! - Generates LLVM IR via Zig's llvm.Builder
//! - Produces bitcode that can be compiled to native code via LLVM

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const mono = @import("mono");
const builtins = @import("builtins");

const LlvmBuilder = std.zig.llvm.Builder;

const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoExpr;
const MonoExprId = mono.MonoExprId;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;
const MonoProc = mono.MonoProc;

const Allocator = std.mem.Allocator;

/// Get the LLVM target triple for the current platform.
fn getLlvmTriple() []const u8 {
    const arch = switch (builtin.cpu.arch) {
        .x86_64 => "x86_64",
        .aarch64 => "aarch64",
        .x86 => "i686",
        .arm, .armeb => "arm",
        .thumb, .thumbeb => "thumb",
        .wasm32 => "wasm32",
        .wasm64 => "wasm64",
        .riscv32 => "riscv32",
        .riscv64 => "riscv64",
        else => "unknown",
    };

    const vendor_os = switch (builtin.os.tag) {
        .windows => "-w64-windows",
        .macos => "-apple-darwin",
        .ios => "-apple-ios",
        .linux => "-unknown-linux",
        .freebsd => "-unknown-freebsd",
        .openbsd => "-unknown-openbsd",
        .netbsd => "-unknown-netbsd",
        .freestanding => "-unknown-unknown",
        .wasi => "-wasi",
        else => "-unknown-unknown",
    };

    const abi = switch (builtin.os.tag) {
        .windows => "-gnu",
        .linux => switch (builtin.abi) {
            .musleabihf => "-musleabihf",
            .gnueabihf => "-gnueabihf",
            .musleabi => "-musleabi",
            .gnueabi => "-gnueabi",
            .musl => "-musl",
            .gnu => "-gnu",
            .android => "-android",
            else => "-gnu",
        },
        .freestanding, .wasi => "",
        else => "",
    };

    return arch ++ vendor_os ++ abi;
}

/// LLVM code generator for Mono IR expressions
pub const MonoLlvmCodeGen = struct {
    allocator: Allocator,

    /// The Mono IR store containing expressions to compile
    store: *const MonoExprStore,

    /// Map from MonoSymbol to LLVM value
    symbol_values: std.AutoHashMap(u48, LlvmBuilder.Value),

    /// Map from MonoSymbol to lambda expression ID (for callable bindings)
    lambda_bindings: std.AutoHashMap(u48, MonoExprId),

    /// Registry of compiled procedures (symbol -> function)
    proc_registry: std.AutoHashMap(u48, LlvmBuilder.Function.Index),

    /// Current LLVM builder (set during code generation)
    builder: ?*LlvmBuilder = null,

    /// Current WIP function (set during code generation)
    wip: ?*LlvmBuilder.WipFunction = null,

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        result_layout: layout.Idx,
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    /// Errors that can occur during code generation
    pub const Error = error{
        OutOfMemory,
        UnsupportedExpression,
        CompilationFailed,
    };

    /// Initialize the code generator
    pub fn init(
        allocator: Allocator,
        store: *const MonoExprStore,
    ) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .store = store,
            .symbol_values = std.AutoHashMap(u48, LlvmBuilder.Value).init(allocator),
            .lambda_bindings = std.AutoHashMap(u48, MonoExprId).init(allocator),
            .proc_registry = std.AutoHashMap(u48, LlvmBuilder.Function.Index).init(allocator),
        };
    }

    /// Clean up resources
    pub fn deinit(self: *MonoLlvmCodeGen) void {
        self.symbol_values.deinit();
        self.lambda_bindings.deinit();
        self.proc_registry.deinit();
    }

    /// Reset the code generator for a new expression
    pub fn reset(self: *MonoLlvmCodeGen) void {
        self.symbol_values.clearRetainingCapacity();
        self.lambda_bindings.clearRetainingCapacity();
        self.proc_registry.clearRetainingCapacity();
    }

    /// Generate LLVM bitcode for a Mono IR expression
    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        expr_id: MonoExprId,
        result_layout: layout.Idx,
    ) Error!BitcodeResult {
        // Create LLVM Builder
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_mono_eval",
            .target = &builtin.target,
            .triple = getLlvmTriple(),
        }) catch return error.OutOfMemory;
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        // Determine the LLVM type for the result
        const value_type = layoutToLlvmType(result_layout);

        // Create the eval function: void roc_eval(<type>* out_ptr)
        const ptr_type = builder.ptrType(.default) catch return error.OutOfMemory;
        const eval_fn_type = builder.fnType(.void, &.{ptr_type}, .normal) catch return error.OutOfMemory;
        const eval_name = if (builtin.os.tag == .macos)
            builder.strtabString("\x01_roc_eval") catch return error.OutOfMemory
        else
            builder.strtabString("roc_eval") catch return error.OutOfMemory;
        const eval_fn = builder.addFunction(eval_fn_type, eval_name, .default) catch return error.OutOfMemory;
        eval_fn.setLinkage(.external, &builder);

        // Set calling convention for x86_64
        if (builtin.cpu.arch == .x86_64) {
            if (builtin.os.tag == .windows) {
                eval_fn.setCallConv(.win64cc, &builder);
            } else {
                eval_fn.setCallConv(.x86_64_sysvcc, &builder);
            }
        }

        // Build function body
        var wip = LlvmBuilder.WipFunction.init(&builder, .{
            .function = eval_fn,
            .strip = false,
        }) catch return error.OutOfMemory;
        defer wip.deinit();

        self.wip = &wip;
        defer self.wip = null;

        const entry_block = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry_block };

        // Get the output pointer
        const out_ptr = wip.arg(0);

        // Generate LLVM IR for the expression
        const value = self.generateExpr(expr_id) catch return error.UnsupportedExpression;

        // Determine the final type to store based on output layout
        const final_type: LlvmBuilder.Type = switch (result_layout) {
            .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
            .i128, .u128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            else => return error.UnsupportedExpression,
        };

        // Determine signedness for integer extension
        const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (result_layout) {
            .i8, .i16, .i32, .i64, .i128 => .signed,
            .bool, .u8, .u16, .u32, .u64, .u128 => .unsigned,
            .f32, .f64, .dec => .unneeded,
            else => .unneeded,
        };

        // Convert value if needed and store
        const store_value = if (value_type == final_type)
            value
        else
            wip.conv(signedness, value, final_type, "") catch return error.CompilationFailed;

        // Use natural alignment for the stored type
        const alignment = LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
            .i64 => 8,
            .i128 => 16,
            .float => 4,
            .double => 8,
            else => 0,
        });
        _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;

        _ = wip.retVoid() catch return error.CompilationFailed;
        wip.finish() catch return error.CompilationFailed;

        // Serialize to bitcode
        const producer = LlvmBuilder.Producer{
            .name = "Roc Mono LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

        const bitcode = builder.toBitcode(self.allocator, producer) catch return error.CompilationFailed;

        return BitcodeResult{
            .bitcode = bitcode,
            .result_layout = result_layout,
            .allocator = self.allocator,
        };
    }

    /// Compile all procedures (for recursive functions)
    pub fn compileAllProcs(self: *MonoLlvmCodeGen, procs: []const MonoProc) Error!void {
        // For now, just stub out procedures - they'll be compiled lazily at call sites
        _ = self;
        _ = procs;
    }

    /// Convert layout to LLVM type
    fn layoutToLlvmType(result_layout: layout.Idx) LlvmBuilder.Type {
        return switch (result_layout) {
            .bool => .i1,
            .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            else => .i64,
        };
    }

    /// Generate LLVM IR for an expression
    fn generateExpr(self: *MonoLlvmCodeGen, expr_id: MonoExprId) Error!LlvmBuilder.Value {
        const expr = self.store.getExpr(expr_id);

        return switch (expr) {
            // Literals
            .i64_literal => |val| self.emitI64(val),
            .i128_literal => |val| self.emitI128(val),
            .f64_literal => |val| self.emitF64(val),
            .f32_literal => |val| self.emitF32(val),
            .dec_literal => |val| self.emitI128(val),
            .bool_literal => |val| self.emitBool(val),

            // Lookups
            .lookup => |lookup| self.generateLookup(lookup.symbol, lookup.layout_idx),

            // Binary operations
            .binop => |binop| self.generateBinop(binop),

            // Unary operations
            .unary_minus => |unary| self.generateUnaryMinus(unary),
            .unary_not => |unary| self.generateUnaryNot(unary),

            // Control flow
            .if_then_else => |ite| self.generateIfThenElse(ite),

            // Blocks
            .block => |block| self.generateBlock(block),

            // Function calls and lambdas
            .call => |call| self.generateCall(call),
            .lambda => |lambda| self.generateLambda(lambda),
            .closure => |closure| self.generateClosure(closure),

            // Nominal wrappers are transparent
            .nominal => |nom| self.generateExpr(nom.backing_expr),

            else => error.UnsupportedExpression,
        };
    }

    fn emitI64(self: *MonoLlvmCodeGen, val: i64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i64, @bitCast(val)) catch return error.OutOfMemory).toValue();
    }

    fn emitI128(self: *MonoLlvmCodeGen, val: i128) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i128, val) catch return error.OutOfMemory).toValue();
    }

    fn emitF64(self: *MonoLlvmCodeGen, val: f64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.doubleConst(val) catch return error.OutOfMemory).toValue();
    }

    fn emitF32(self: *MonoLlvmCodeGen, val: f32) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.floatConst(val) catch return error.OutOfMemory).toValue();
    }

    fn emitBool(self: *MonoLlvmCodeGen, val: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i1, if (val) 1 else 0) catch return error.OutOfMemory).toValue();
    }

    fn generateLookup(self: *MonoLlvmCodeGen, symbol: MonoSymbol, _: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u48 = @bitCast(symbol);

        // Check if we have a value for this symbol
        if (self.symbol_values.get(symbol_key)) |val| {
            return val;
        }

        // Check if it's a lambda binding that needs to be evaluated
        if (self.lambda_bindings.get(symbol_key)) |lambda_expr_id| {
            const val = try self.generateExpr(lambda_expr_id);
            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
            return val;
        }

        // Check if it's a top-level definition
        if (self.store.getSymbolDef(symbol)) |def_expr_id| {
            const val = try self.generateExpr(def_expr_id);
            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
            return val;
        }

        return error.UnsupportedExpression;
    }

    fn generateBinop(self: *MonoLlvmCodeGen, binop: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        const lhs = try self.generateExpr(binop.lhs);
        const rhs = try self.generateExpr(binop.rhs);

        const is_float = binop.result_layout == .f32 or binop.result_layout == .f64;

        return switch (binop.op) {
            .add => if (is_float)
                wip.bin(.fadd, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed,

            .sub => if (is_float)
                wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed,

            .mul => if (is_float)
                wip.bin(.fmul, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed,

            .div => if (is_float)
                wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,

            .mod => if (is_float)
                wip.bin(.frem, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed,

            .eq => if (is_float)
                wip.fcmp(.oeq, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.eq, lhs, rhs, "") catch return error.CompilationFailed,

            .neq => if (is_float)
                wip.fcmp(.one, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ne, lhs, rhs, "") catch return error.CompilationFailed,

            .lt => if (is_float)
                wip.fcmp(.olt, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.icmp(.slt, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ult, lhs, rhs, "") catch return error.CompilationFailed,

            .lte => if (is_float)
                wip.fcmp(.ole, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.icmp(.sle, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ule, lhs, rhs, "") catch return error.CompilationFailed,

            .gt => if (is_float)
                wip.fcmp(.ogt, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.icmp(.sgt, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ugt, lhs, rhs, "") catch return error.CompilationFailed,

            .gte => if (is_float)
                wip.fcmp(.oge, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.icmp(.sge, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.uge, lhs, rhs, "") catch return error.CompilationFailed,

            .@"and" => wip.bin(.@"and", lhs, rhs, "") catch return error.CompilationFailed,
            .@"or" => wip.bin(.@"or", lhs, rhs, "") catch return error.CompilationFailed,
        };
    }

    fn isSigned(result_layout: layout.Idx) bool {
        return switch (result_layout) {
            .i8, .i16, .i32, .i64, .i128, .dec => true,
            else => false,
        };
    }

    fn generateUnaryMinus(self: *MonoLlvmCodeGen, unary: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const val = try self.generateExpr(unary.expr);
        const is_float = unary.result_layout == .f32 or unary.result_layout == .f64;

        if (is_float) {
            return wip.un(.fneg, val, "") catch return error.CompilationFailed;
        } else {
            // For integers, subtract from 0
            const zero = (builder.intConst(layoutToLlvmType(unary.result_layout), 0) catch return error.OutOfMemory).toValue();
            return wip.bin(.sub, zero, val, "") catch return error.CompilationFailed;
        }
    }

    fn generateUnaryNot(self: *MonoLlvmCodeGen, unary: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const val = try self.generateExpr(unary.expr);
        // XOR with 1 to flip the boolean
        const one = (builder.intConst(.i1, 1) catch return error.OutOfMemory).toValue();
        return wip.bin(.xor, val, one, "") catch return error.CompilationFailed;
    }

    fn generateIfThenElse(self: *MonoLlvmCodeGen, ite: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        // Get the branches
        const branches = self.store.getIfBranches(ite.branches);

        if (branches.len == 0) {
            // No branches, just generate the else
            return self.generateExpr(ite.final_else);
        }

        // For simplicity, handle single branch if-then-else
        const first_branch = branches[0];
        const cond_val = try self.generateExpr(first_branch.cond);

        // Create basic blocks
        const then_block = wip.block(0, "then") catch return error.CompilationFailed;
        const else_block = wip.block(0, "else") catch return error.CompilationFailed;
        const merge_block = wip.block(0, "merge") catch return error.CompilationFailed;

        // Conditional branch
        _ = wip.brCond(cond_val, then_block, else_block) catch return error.CompilationFailed;

        // Then block
        wip.cursor = .{ .block = then_block };
        const then_val = try self.generateExpr(first_branch.body);
        _ = wip.br(merge_block) catch return error.CompilationFailed;
        const then_exit_block = wip.cursor.block;

        // Else block
        wip.cursor = .{ .block = else_block };
        const else_val = try self.generateExpr(ite.final_else);
        _ = wip.br(merge_block) catch return error.CompilationFailed;
        const else_exit_block = wip.cursor.block;

        // Merge block with phi
        wip.cursor = .{ .block = merge_block };
        const result_type = layoutToLlvmType(ite.result_layout);
        const phi = wip.phi(result_type, "") catch return error.CompilationFailed;
        wip.addPhiArg(phi, then_exit_block, then_val);
        wip.addPhiArg(phi, else_exit_block, else_val);

        return phi;
    }

    fn generateBlock(self: *MonoLlvmCodeGen, block: anytype) Error!LlvmBuilder.Value {
        // Process all statements (let bindings)
        const stmts = self.store.getStmts(block.stmts);
        for (stmts) |stmt| {
            // Generate the expression
            const val = try self.generateExpr(stmt.expr);

            // Bind the pattern
            const pattern = self.store.getPattern(stmt.pattern);
            switch (pattern) {
                .bind => |bind| {
                    const symbol_key: u48 = @bitCast(bind.symbol);
                    self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                },
                else => {
                    // For now, only support simple bindings
                },
            }
        }

        // Generate and return the final expression
        return self.generateExpr(block.final_expr);
    }

    fn generateCall(self: *MonoLlvmCodeGen, call: anytype) Error!LlvmBuilder.Value {
        // Check if the function is a lambda or closure we can inline
        const fn_expr = self.store.getExpr(call.fn_expr);

        switch (fn_expr) {
            .lambda => |lambda| {
                return self.inlineLambdaCall(lambda, call);
            },
            .closure => |closure| {
                // Get the underlying lambda
                const lambda_expr = self.store.getExpr(closure.lambda);
                switch (lambda_expr) {
                    .lambda => |lambda| {
                        return self.inlineClosureCall(lambda, closure, call);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .lookup => |lookup| {
                // Look up the symbol to find the lambda/closure
                const symbol_key: u48 = @bitCast(lookup.symbol);
                if (self.lambda_bindings.get(symbol_key)) |lambda_id| {
                    const bound_expr = self.store.getExpr(lambda_id);
                    switch (bound_expr) {
                        .lambda => |lambda| {
                            return self.inlineLambdaCall(lambda, call);
                        },
                        .closure => |closure| {
                            const lambda_expr2 = self.store.getExpr(closure.lambda);
                            switch (lambda_expr2) {
                                .lambda => |lambda| {
                                    return self.inlineClosureCall(lambda, closure, call);
                                },
                                else => return error.UnsupportedExpression,
                            }
                        },
                        else => return error.UnsupportedExpression,
                    }
                }
                return error.UnsupportedExpression;
            },
            else => return error.UnsupportedExpression,
        }
    }

    fn inlineLambdaCall(self: *MonoLlvmCodeGen, lambda: anytype, call: anytype) Error!LlvmBuilder.Value {
        // Bind arguments to parameters
        const params = self.store.getPatternSpan(lambda.params);
        const args = self.store.getExprSpan(call.args);

        for (params, args) |param_id, arg_id| {
            const arg_val = try self.generateExpr(arg_id);
            const pattern = self.store.getPattern(param_id);
            switch (pattern) {
                .bind => |bind| {
                    const symbol_key: u48 = @bitCast(bind.symbol);
                    self.symbol_values.put(symbol_key, arg_val) catch return error.OutOfMemory;
                },
                else => {},
            }
        }

        // Generate the body
        return self.generateExpr(lambda.body);
    }

    fn inlineClosureCall(self: *MonoLlvmCodeGen, lambda: anytype, closure: anytype, call: anytype) Error!LlvmBuilder.Value {
        // First, bind the captured values
        const captures = self.store.getCaptures(closure.captures);
        for (captures) |capture| {
            // The capture's symbol should already have a value from the enclosing scope
            const symbol_key: u48 = @bitCast(capture.symbol);
            if (self.symbol_values.get(symbol_key)) |_| {
                // Already bound, nothing to do
            } else {
                // Try to look it up
                if (self.store.getSymbolDef(capture.symbol)) |def_id| {
                    const val = try self.generateExpr(def_id);
                    self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                }
            }
        }

        // Then bind arguments and generate body like a lambda
        return self.inlineLambdaCall(lambda, call);
    }

    fn generateLambda(self: *MonoLlvmCodeGen, lambda: anytype) Error!LlvmBuilder.Value {
        // Standalone lambda - just return 0 as placeholder
        // Actual evaluation happens at call site
        _ = lambda;
        return self.emitI64(0);
    }

    fn generateClosure(self: *MonoLlvmCodeGen, closure: anytype) Error!LlvmBuilder.Value {
        // Standalone closure - just return 0 as placeholder
        // Actual evaluation happens at call site
        _ = closure;
        return self.emitI64(0);
    }
};
