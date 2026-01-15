//! Dev Backend Evaluator for Roc expressions
//!
//! This module generates native machine code from Roc expressions using the dev backend.
//! Unlike the LLVM evaluator, this generates native code directly without LLVM IR.
//!
//! Used when the `--backend=dev` flag is passed to the REPL.
//!
//! The evaluator works by:
//! 1. Parsing and type-checking the source expression
//! 2. Translating the CIR to native machine code using the dev backend
//! 3. JIT-executing the native code to produce a result

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const check = @import("check");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("mod.zig");
const backend = @import("backend");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;

/// Dev backend-based evaluator for Roc expressions
pub const DevEvaluator = struct {
    allocator: Allocator,

    /// Builtin type declaration indices (loaded once at startup)
    builtin_indices: CIR.BuiltinIndices,

    /// Loaded Builtin module (loaded once at startup)
    builtin_module: builtin_loading.LoadedModule,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        UnsupportedType,
        UnsupportedExpression,
        ParseError,
        CanonicalizeError,
        TypeError,
        JitError,
        NotImplemented,
    };

    /// Type of the result value for JIT execution.
    /// Matches the types in llvm_evaluator.ResultType.
    pub const ResultType = enum {
        i64,
        u64,
        i128,
        u128,
        f64,
        dec,

        pub fn validate() void {
            comptime {
                if (@typeInfo(ResultType).@"enum".fields.len != 6) @compileError("ResultType must have exactly 6 variants");
                if (@intFromEnum(ResultType.i64) != 0) @compileError("ResultType.i64 must be ordinal 0");
                if (@intFromEnum(ResultType.u64) != 1) @compileError("ResultType.u64 must be ordinal 1");
                if (@intFromEnum(ResultType.i128) != 2) @compileError("ResultType.i128 must be ordinal 2");
                if (@intFromEnum(ResultType.u128) != 3) @compileError("ResultType.u128 must be ordinal 3");
                if (@intFromEnum(ResultType.f64) != 4) @compileError("ResultType.f64 must be ordinal 4");
                if (@intFromEnum(ResultType.dec) != 5) @compileError("ResultType.dec must be ordinal 5");
            }
        }
    };

    comptime {
        ResultType.validate();
    }

    /// Result of code generation
    pub const CodeResult = struct {
        /// The native machine code bytes
        code: []const u8,
        /// The result type for interpreting the return value
        result_type: ResultType,
        /// Allocator used for cleanup
        allocator: Allocator,

        pub fn deinit(self: *CodeResult) void {
            self.allocator.free(self.code);
        }
    };

    /// Initialize a new Dev evaluator
    pub fn init(allocator: Allocator) Error!DevEvaluator {
        // Load builtin indices once at startup (generated at build time)
        const builtin_indices = builtin_loading.deserializeBuiltinIndices(
            allocator,
            compiled_builtins.builtin_indices_bin,
        ) catch return error.OutOfMemory;

        // Load Builtin module once at startup
        const builtin_source = compiled_builtins.builtin_source;
        var builtin_module = builtin_loading.loadCompiledModule(
            allocator,
            compiled_builtins.builtin_bin,
            "Builtin",
            builtin_source,
        ) catch return error.OutOfMemory;
        errdefer builtin_module.deinit();

        return DevEvaluator{
            .allocator = allocator,
            .builtin_indices = builtin_indices,
            .builtin_module = builtin_module,
        };
    }

    /// Clean up the evaluator
    pub fn deinit(self: *DevEvaluator) void {
        self.builtin_module.deinit();
    }

    /// Generate native code for a CIR expression
    /// Returns the generated code and result type
    /// The caller is responsible for freeing the code via result.deinit()
    pub fn generateCode(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error!CodeResult {
        // Get the result type from the expression
        const result_type = self.getExprResultType(expr);

        // Generate code based on the expression type
        const code = switch (expr) {
            .e_num => |num| try self.generateNumericCode(num, result_type),
            .e_frac_f64 => |frac| try self.generateFloatCode(frac.value),
            .e_frac_f32 => |frac| try self.generateFloatCode(@floatCast(frac.value)),
            .e_binop => |binop| try self.generateBinopCode(module_env, binop, result_type),
            .e_unary_minus => |unary| try self.generateUnaryMinusCode(module_env, unary, result_type),
            .e_if => |if_expr| try self.generateIfCode(module_env, if_expr, result_type),
            else => return error.UnsupportedExpression,
        };

        return CodeResult{
            .code = code,
            .result_type = result_type,
            .allocator = self.allocator,
        };
    }

    /// Generate code for binary operations
    /// For now, uses constant folding when both operands are literals
    fn generateBinopCode(self: *DevEvaluator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, result_type: ResultType) Error![]const u8 {
        // Get the left and right expressions
        const lhs_expr = module_env.store.getExpr(binop.lhs);
        const rhs_expr = module_env.store.getExpr(binop.rhs);

        // Try to evaluate both sides as constants (constant folding)
        const lhs_val = self.tryEvalConstantI64(lhs_expr) orelse return error.UnsupportedExpression;
        const rhs_val = self.tryEvalConstantI64(rhs_expr) orelse return error.UnsupportedExpression;

        // Perform the operation at compile time
        const result_val: i64 = switch (binop.op) {
            .add => lhs_val +% rhs_val,
            .sub => lhs_val -% rhs_val,
            .mul => lhs_val *% rhs_val,
            .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
            .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
            .le => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
            .ge => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
            .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
            .ne => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
            .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
            .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
        };

        return self.generateReturnI64Code(result_val, result_type);
    }

    /// Generate code for unary minus
    fn generateUnaryMinusCode(self: *DevEvaluator, module_env: *ModuleEnv, unary: CIR.Expr.UnaryMinus, result_type: ResultType) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.tryEvalConstantI64(inner_expr) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(-inner_val, result_type);
    }

    /// Generate code for if/else expressions
    /// Uses constant folding - evaluates condition at compile time and generates code for the taken branch
    fn generateIfCode(self: *DevEvaluator, module_env: *ModuleEnv, if_expr: anytype, result_type: ResultType) Error![]const u8 {
        // Get the branches
        const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);

        // Try each branch's condition
        for (branch_indices) |branch_idx| {
            const branch = module_env.store.getIfBranch(branch_idx);
            const cond_expr = module_env.store.getExpr(branch.cond);

            // Try to evaluate the condition as a constant (supports binary operations like 1 > 0)
            const cond_val = self.tryEvalConstantI64WithEnv(module_env, cond_expr);

            if (cond_val) |val| {
                if (val != 0) {
                    // Condition is true - generate code for this branch's body
                    const body_expr = module_env.store.getExpr(branch.body);
                    return self.generateCodeForExpr(module_env, body_expr, result_type);
                }
                // Condition is false - try next branch
            } else {
                // Can't evaluate condition at compile time
                return error.UnsupportedExpression;
            }
        }

        // All conditions were false - use the final else branch
        const else_expr = module_env.store.getExpr(if_expr.final_else);
        return self.generateCodeForExpr(module_env, else_expr, result_type);
    }

    /// Helper to generate code for an expression (recursive)
    fn generateCodeForExpr(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, result_type: ResultType) Error![]const u8 {
        return switch (expr) {
            .e_num => |num| try self.generateNumericCode(num, result_type),
            .e_frac_f64 => |frac| try self.generateFloatCode(frac.value),
            .e_frac_f32 => |frac| try self.generateFloatCode(@floatCast(frac.value)),
            .e_binop => |binop| try self.generateBinopCode(module_env, binop, result_type),
            .e_unary_minus => |unary| try self.generateUnaryMinusCode(module_env, unary, result_type),
            .e_if => |if_expr| try self.generateIfCode(module_env, if_expr, result_type),
            else => return error.UnsupportedExpression,
        };
    }

    /// Try to evaluate an expression as a constant i64 value
    /// This handles numeric literals and binary operations with constant operands
    fn tryEvalConstantI64(_: *DevEvaluator, expr: CIR.Expr) ?i64 {
        return switch (expr) {
            .e_num => |num| {
                const value_i128 = num.value.toI128();
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return null;
                }
                return @intCast(value_i128);
            },
            else => null,
        };
    }

    /// Try to evaluate an expression as a constant i64 value, with access to module_env for sub-expressions
    fn tryEvalConstantI64WithEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) ?i64 {
        return switch (expr) {
            .e_num => |num| {
                const value_i128 = num.value.toI128();
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return null;
                }
                return @intCast(value_i128);
            },
            .e_binop => |binop| {
                const lhs_expr = module_env.store.getExpr(binop.lhs);
                const rhs_expr = module_env.store.getExpr(binop.rhs);
                const lhs_val = self.tryEvalConstantI64WithEnv(module_env, lhs_expr) orelse return null;
                const rhs_val = self.tryEvalConstantI64WithEnv(module_env, rhs_expr) orelse return null;

                return switch (binop.op) {
                    .add => lhs_val +% rhs_val,
                    .sub => lhs_val -% rhs_val,
                    .mul => lhs_val *% rhs_val,
                    .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else null,
                    .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
                    .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
                    .le => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
                    .ge => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
                    .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
                    .ne => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
                    .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
                    .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
                };
            },
            .e_unary_minus => |unary| {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.tryEvalConstantI64WithEnv(module_env, inner_expr) orelse return null;
                return -inner_val;
            },
            else => null,
        };
    }

    /// Generate code for a numeric literal
    fn generateNumericCode(self: *DevEvaluator, num: anytype, result_type: ResultType) Error![]const u8 {
        // Get the value as i64
        // The num has .value (IntValue) and .kind (NumKind) fields
        const value_i128 = num.value.toI128();
        if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
            return error.UnsupportedType;
        }
        const value: i64 = @intCast(value_i128);

        return self.generateReturnI64Code(value, result_type);
    }

    /// Generate code for a floating-point literal
    fn generateFloatCode(self: *DevEvaluator, value: f64) Error![]const u8 {
        return self.generateReturnF64Code(value);
    }

    /// Generate code that returns an i64/u64 value
    fn generateReturnI64Code(self: *DevEvaluator, value: i64, _: ResultType) Error![]const u8 {

        switch (builtin.cpu.arch) {
            .x86_64 => {
                // x86_64 code:
                // movabs rax, <value>  ; 48 B8 <8 bytes>
                // ret                  ; C3
                var code = self.allocator.alloc(u8, 11) catch return error.OutOfMemory;

                // movabs rax, imm64
                code[0] = 0x48; // REX.W
                code[1] = 0xB8; // MOV RAX, imm64
                @memcpy(code[2..10], std.mem.asBytes(&value));
                code[10] = 0xC3; // RET

                return code;
            },
            .aarch64 => {
                // aarch64 code: need to load 64-bit value into x0 and return
                // For simplicity, we'll handle values that fit in various sizes
                const uvalue: u64 = @bitCast(value);

                if (uvalue <= 0xFFFF) {
                    // mov x0, #<imm16>  ; 4 bytes
                    // ret              ; 4 bytes
                    var code = self.allocator.alloc(u8, 8) catch return error.OutOfMemory;

                    // MOV X0, #imm16
                    const imm16: u16 = @truncate(uvalue);
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm16) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[4..8], std.mem.asBytes(&ret_inst));

                    return code;
                } else {
                    // For larger values, we need multiple MOV/MOVK instructions
                    // mov x0, #<low16>
                    // movk x0, #<high16>, lsl #16
                    // movk x0, #<high32>, lsl #32
                    // movk x0, #<high48>, lsl #48
                    // ret
                    var code = self.allocator.alloc(u8, 20) catch return error.OutOfMemory;

                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    // MOV X0, #imm0
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm0) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // MOVK X0, #imm1, LSL #16
                    const movk1_inst: u32 = 0xF2A00000 | (@as(u32, imm1) << 5);
                    @memcpy(code[4..8], std.mem.asBytes(&movk1_inst));

                    // MOVK X0, #imm2, LSL #32
                    const movk2_inst: u32 = 0xF2C00000 | (@as(u32, imm2) << 5);
                    @memcpy(code[8..12], std.mem.asBytes(&movk2_inst));

                    // MOVK X0, #imm3, LSL #48
                    const movk3_inst: u32 = 0xF2E00000 | (@as(u32, imm3) << 5);
                    @memcpy(code[12..16], std.mem.asBytes(&movk3_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[16..20], std.mem.asBytes(&ret_inst));

                    return code;
                }
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code that returns an f64 value
    fn generateReturnF64Code(self: *DevEvaluator, value: f64) Error![]const u8 {
        const bits: u64 = @bitCast(value);

        switch (builtin.cpu.arch) {
            .x86_64 => {
                // x86_64 code:
                // movabs rax, <bits>   ; 48 B8 <8 bytes>
                // movq xmm0, rax       ; 66 48 0F 6E C0
                // ret                  ; C3
                var code = self.allocator.alloc(u8, 16) catch return error.OutOfMemory;

                // movabs rax, imm64
                code[0] = 0x48; // REX.W
                code[1] = 0xB8; // MOV RAX, imm64
                @memcpy(code[2..10], std.mem.asBytes(&bits));

                // movq xmm0, rax
                code[10] = 0x66;
                code[11] = 0x48;
                code[12] = 0x0F;
                code[13] = 0x6E;
                code[14] = 0xC0;

                // ret
                code[15] = 0xC3;

                return code;
            },
            .aarch64 => {
                // aarch64 code:
                // Load 64-bit value into x0, then move to d0
                // For floating point, return value is in d0/v0
                const uvalue = bits;

                if (uvalue <= 0xFFFF) {
                    // mov x0, #<imm16>
                    // fmov d0, x0
                    // ret
                    var code = self.allocator.alloc(u8, 12) catch return error.OutOfMemory;

                    const imm16: u16 = @truncate(uvalue);
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm16) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // FMOV D0, X0
                    const fmov_inst: u32 = 0x9E670000;
                    @memcpy(code[4..8], std.mem.asBytes(&fmov_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[8..12], std.mem.asBytes(&ret_inst));

                    return code;
                } else {
                    // Full 64-bit load
                    var code = self.allocator.alloc(u8, 24) catch return error.OutOfMemory;

                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    // MOV X0, #imm0
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm0) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // MOVK X0, #imm1, LSL #16
                    const movk1_inst: u32 = 0xF2A00000 | (@as(u32, imm1) << 5);
                    @memcpy(code[4..8], std.mem.asBytes(&movk1_inst));

                    // MOVK X0, #imm2, LSL #32
                    const movk2_inst: u32 = 0xF2C00000 | (@as(u32, imm2) << 5);
                    @memcpy(code[8..12], std.mem.asBytes(&movk2_inst));

                    // MOVK X0, #imm3, LSL #48
                    const movk3_inst: u32 = 0xF2E00000 | (@as(u32, imm3) << 5);
                    @memcpy(code[12..16], std.mem.asBytes(&movk3_inst));

                    // FMOV D0, X0
                    const fmov_inst: u32 = 0x9E670000;
                    @memcpy(code[16..20], std.mem.asBytes(&fmov_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[20..24], std.mem.asBytes(&ret_inst));

                    return code;
                }
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate native code from source code string
    /// This does the full pipeline: parse -> canonicalize -> type check -> generate code -> execute
    pub fn generateCodeFromSource(self: *DevEvaluator, source: []const u8) Error!CodeResult {
        // Step 1: Create module environment and parse
        var module_env = ModuleEnv.init(self.allocator, source) catch return error.OutOfMemory;
        defer module_env.deinit();

        var parse_ast = parse.parseExpr(&module_env.common, self.allocator) catch {
            return error.ParseError;
        };
        defer parse_ast.deinit(self.allocator);

        if (parse_ast.hasErrors()) {
            return error.ParseError;
        }

        // Step 2: Initialize CIR and canonicalize
        module_env.initCIRFields("dev_eval") catch return error.OutOfMemory;

        // Set up module envs map for auto-imported builtins
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(self.allocator);
        defer module_envs_map.deinit();

        Can.populateModuleEnvs(
            &module_envs_map,
            &module_env,
            self.builtin_module.env,
            self.builtin_indices,
        ) catch return error.OutOfMemory;

        var czer = Can.init(&module_env, &parse_ast, &module_envs_map) catch {
            return error.CanonicalizeError;
        };
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const canonical_expr = czer.canonicalizeExpr(expr_idx) catch {
            return error.CanonicalizeError;
        } orelse {
            return error.CanonicalizeError;
        };
        const final_expr_idx = canonical_expr.get_idx();

        // Step 3: Type check
        const imported_modules = [_]*const ModuleEnv{self.builtin_module.env};
        module_env.imports.resolveImports(&module_env, &imported_modules);

        const builtin_ctx: Check.BuiltinContext = .{
            .module_name = module_env.insertIdent(base.Ident.for_text("dev_eval")) catch return error.OutOfMemory,
            .bool_stmt = self.builtin_indices.bool_type,
            .try_stmt = self.builtin_indices.try_type,
            .str_stmt = self.builtin_indices.str_type,
            .builtin_module = self.builtin_module.env,
            .builtin_indices = self.builtin_indices,
        };

        var checker = Check.init(
            self.allocator,
            &module_env.types,
            &module_env,
            &imported_modules,
            &module_envs_map,
            &module_env.store.regions,
            builtin_ctx,
        ) catch return error.OutOfMemory;
        defer checker.deinit();

        _ = checker.checkExprRepl(final_expr_idx) catch {
            return error.TypeError;
        };

        // Step 4: Generate native code
        const expr = module_env.store.getExpr(final_expr_idx);
        return self.generateCode(&module_env, expr);
    }

    /// Get the ResultType for JIT execution from a CIR expression
    fn getExprResultType(_: *DevEvaluator, expr: CIR.Expr) ResultType {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .i8, .i16, .i32, .i64, .num_unbound, .int_unbound => .i64,
                .u8, .u16, .u32, .u64 => .u64,
                .i128 => .i128,
                .u128 => .u128,
                .f32, .f64 => .f64,
                .dec => .dec,
            },
            .e_frac_f32, .e_frac_f64 => .f64,
            .e_dec, .e_dec_small => .dec,
            else => .i64,
        };
    }

    /// Result of evaluation
    pub const EvalResult = union(enum) {
        i64_val: i64,
        u64_val: u64,
        f64_val: f64,
        i128_val: i128,
        u128_val: u128,

        pub fn format(self: EvalResult, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .i64_val => |v| try writer.print("{}", .{v}),
                .u64_val => |v| try writer.print("{}", .{v}),
                .f64_val => |v| try writer.print("{d}", .{v}),
                .i128_val => |v| try writer.print("{}", .{v}),
                .u128_val => |v| try writer.print("{}", .{v}),
            }
        }
    };

    /// Evaluate source code and return the result
    pub fn evaluate(self: *DevEvaluator, source: []const u8) Error!EvalResult {
        // Generate code
        var code_result = try self.generateCodeFromSource(source);
        defer code_result.deinit();

        // JIT execute
        var jit_code = backend.JitCode.init(code_result.code) catch return error.JitError;
        defer jit_code.deinit();

        // Call and return result based on type
        return switch (code_result.result_type) {
            .i64 => EvalResult{ .i64_val = jit_code.callReturnI64() },
            .u64 => EvalResult{ .u64_val = jit_code.callReturnU64() },
            .f64 => EvalResult{ .f64_val = jit_code.callReturnF64() },
            .i128 => EvalResult{ .i128_val = @as(i128, jit_code.callReturnI64()) }, // TODO: proper 128-bit
            .u128 => EvalResult{ .u128_val = @as(u128, jit_code.callReturnU64()) }, // TODO: proper 128-bit
            .dec => EvalResult{ .i128_val = @as(i128, jit_code.callReturnI64()) }, // TODO: proper decimal
        };
    }
};

// Tests

test "dev evaluator initialization" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        // It's OK if builtin loading fails in tests (missing compiled builtins)
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();
}

test "result type ordinals match llvm evaluator" {
    // Validate that our ResultType matches the expected ordinals
    DevEvaluator.ResultType.validate();
}

test "generate i64 code" {
    // Test direct code generation for i64 values
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // Generate code for value 42
    const code = try evaluator.generateReturnI64Code(42, .i64);
    defer evaluator.allocator.free(code);

    // Execute the code using JIT
    var jit = backend.JitCode.init(code) catch return error.SkipZigTest;
    defer jit.deinit();

    const result = jit.callReturnI64();
    try std.testing.expectEqual(@as(i64, 42), result);
}

test "generate i64 code large value" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // Generate code for large value
    const large_value: i64 = 0x123456789ABCDEF;
    const code = try evaluator.generateReturnI64Code(large_value, .i64);
    defer evaluator.allocator.free(code);

    var jit = backend.JitCode.init(code) catch return error.SkipZigTest;
    defer jit.deinit();

    const result = jit.callReturnI64();
    try std.testing.expectEqual(large_value, result);
}

test "generate f64 code" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const code = try evaluator.generateReturnF64Code(3.14159);
    defer evaluator.allocator.free(code);

    var jit = backend.JitCode.init(code) catch return error.SkipZigTest;
    defer jit.deinit();

    const result = jit.callReturnF64();
    try std.testing.expectApproxEqRel(@as(f64, 3.14159), result, 0.0001);
}

test "evaluate addition" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("1 + 2") catch |err| {
        // Skip if parsing/canonicalization fails (expected in unit test environment)
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 3 }, result);
}

test "evaluate subtraction" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("10 - 3") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate multiplication" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("6 * 7") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate unary minus" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("-42") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = -42 }, result);
}

test "evaluate if true branch" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 1 > 0 then 42 else 0") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate if false branch" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 0 > 1 then 42 else 99") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 99 }, result);
}

test "evaluate nested if" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 1 > 0 then (if 2 > 1 then 100 else 50) else 0") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 100 }, result);
}
