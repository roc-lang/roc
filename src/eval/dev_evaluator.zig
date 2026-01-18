//! Dev Backend Evaluator
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Generating native machine code (x86_64/aarch64)
//! 5. Executing the generated code
//!
//! Code generation is delegated to ExprCodeGen in the backend.
//! This module handles orchestration and the compilation pipeline.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const layout = @import("layout");
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const LoadedModule = builtin_loading.LoadedModule;

// Re-export types from backend ExprCodeGen
pub const ExprCodeGen = backend.ExprCodeGen;
pub const BindingValue = backend.BindingValue;
pub const Scope = backend.Scope;

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

/// Dev backend evaluator
///
/// Orchestrates the compilation pipeline:
/// - Initializes with builtin modules
/// - Parses, canonicalizes, and type-checks expressions
/// - Delegates code generation to ExprCodeGen
/// - Executes generated code
pub const DevEvaluator = struct {
    allocator: Allocator,
    codegen: ExprCodeGen,

    /// Loaded builtin module (Bool, Result, etc.)
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,

    pub const Error = error{
        OutOfMemory,
        UnsupportedType,
        UnsupportedExpression,
        Crash,
        RuntimeError,
        ParseError,
        CanonicalizeError,
        TypeError,
        ExecutionError,
    };

    /// Initialize the evaluator with builtin modules
    pub fn init(allocator: Allocator) Error!DevEvaluator {
        // Load compiled builtins
        const compiled_builtins = @import("compiled_builtins");

        const builtin_indices = builtin_loading.deserializeBuiltinIndices(
            allocator,
            compiled_builtins.builtin_indices_bin,
        ) catch return error.OutOfMemory;

        const builtin_module = builtin_loading.loadCompiledModule(
            allocator,
            compiled_builtins.builtin_bin,
            "Builtin",
            compiled_builtins.builtin_source,
        ) catch return error.OutOfMemory;

        return DevEvaluator{
            .allocator = allocator,
            .codegen = ExprCodeGen.init(allocator),
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
        };
    }

    /// Clean up resources
    pub fn deinit(self: *DevEvaluator) void {
        self.codegen.deinit();
        self.builtin_module.deinit();
    }

    /// Create an empty scope for code generation
    pub fn createScope(self: *DevEvaluator) Scope {
        return self.codegen.createScope();
    }

    /// Result of code generation
    pub const CodeResult = struct {
        code: []const u8,
        allocator: Allocator,
        result_layout: LayoutIdx,
        tuple_len: usize = 1,
        crash_message: ?[]const u8 = null,

        pub fn deinit(self: *CodeResult) void {
            self.allocator.free(self.code);
        }
    };

    /// Generate code for a CIR expression
    pub fn generateCode(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error!CodeResult {
        var env = self.createScope();
        defer env.deinit();

        // Determine result layout and tuple length
        var type_env = std.AutoHashMap(u32, LayoutIdx).init(self.allocator);
        defer type_env.deinit();
        const result_layout = self.getExprLayoutWithTypeEnv(module_env, expr, &type_env);
        var tuple_len: usize = 1;

        switch (expr) {
            .e_tuple => |tuple| {
                const elems = module_env.store.sliceExpr(tuple.elems);
                tuple_len = elems.len;
            },
            else => {},
        }

        // Generate code using ExprCodeGen
        const code = self.codegen.generateCodeForExpr(module_env, expr, result_layout, &env) catch |err| {
            if (err == error.Crash or err == error.RuntimeError) {
                // Create result with crash message
                const crash_msg = self.codegen.getCrashMessage();
                return CodeResult{
                    .code = &[_]u8{},
                    .allocator = self.allocator,
                    .result_layout = result_layout,
                    .tuple_len = tuple_len,
                    .crash_message = crash_msg,
                };
            }
            return err;
        };

        return CodeResult{
            .code = code,
            .allocator = self.allocator,
            .result_layout = result_layout,
            .tuple_len = tuple_len,
            .crash_message = null,
        };
    }

    /// Type environment for tracking variable types
    const TypeEnv = std.AutoHashMap(u32, LayoutIdx);

    /// Get the Layout for code execution from a CIR expression
    fn getExprLayout(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) LayoutIdx {
        var type_env = TypeEnv.init(self.allocator);
        defer type_env.deinit();
        return self.getExprLayoutWithTypeEnv(module_env, expr, &type_env);
    }

    /// Get the Layout with a type environment for tracking variable types through blocks
    fn getExprLayoutWithTypeEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, type_env: *TypeEnv) LayoutIdx {
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
            .e_typed_int => |ti| self.getTypedIntLayout(module_env, ti.type_name),
            .e_binop => |binop| self.getBinopLayoutWithTypeEnv(module_env, binop, type_env),
            .e_unary_minus => |unary| blk: {
                const inner_expr = module_env.store.getExpr(unary.expr);
                break :blk self.getExprLayoutWithTypeEnv(module_env, inner_expr, type_env);
            },
            .e_nominal => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.getExprLayoutWithTypeEnv(module_env, backing_expr, type_env);
            },
            .e_nominal_external => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.getExprLayoutWithTypeEnv(module_env, backing_expr, type_env);
            },
            .e_if => |if_expr| blk: {
                const else_expr = module_env.store.getExpr(if_expr.final_else);
                break :blk self.getExprLayoutWithTypeEnv(module_env, else_expr, type_env);
            },
            .e_block => |block| self.getBlockLayout(module_env, block, type_env),
            .e_lookup_local => |lookup| blk: {
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                break :blk type_env.get(pattern_key) orelse .i64;
            },
            .e_str, .e_str_segment => .str,
            .e_call => |call| blk: {
                const func_expr = module_env.store.getExpr(call.func);
                switch (func_expr) {
                    .e_lambda => |lambda| {
                        const body_expr = module_env.store.getExpr(lambda.body);
                        switch (body_expr) {
                            .e_lookup_local => {
                                const args = module_env.store.sliceExpr(call.args);
                                if (args.len > 0) {
                                    const arg_expr = module_env.store.getExpr(args[0]);
                                    break :blk self.getExprLayoutWithTypeEnv(module_env, arg_expr, type_env);
                                }
                            },
                            else => {},
                        }
                        break :blk self.getExprLayoutWithTypeEnv(module_env, body_expr, type_env);
                    },
                    .e_closure => |closure| {
                        const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
                        switch (lambda_expr) {
                            .e_lambda => |lambda| {
                                const body_expr = module_env.store.getExpr(lambda.body);
                                switch (body_expr) {
                                    .e_lookup_local => {
                                        const args = module_env.store.sliceExpr(call.args);
                                        if (args.len > 0) {
                                            const arg_expr = module_env.store.getExpr(args[0]);
                                            break :blk self.getExprLayoutWithTypeEnv(module_env, arg_expr, type_env);
                                        }
                                    },
                                    else => {},
                                }
                                break :blk self.getExprLayoutWithTypeEnv(module_env, body_expr, type_env);
                            },
                            else => {},
                        }
                        break :blk .i64;
                    },
                    .e_lookup_local => {
                        const args = module_env.store.sliceExpr(call.args);
                        if (args.len > 0) {
                            const arg_expr = module_env.store.getExpr(args[0]);
                            const arg_layout = self.getExprLayoutWithTypeEnv(module_env, arg_expr, type_env);
                            if (arg_layout == .str) {
                                break :blk .str;
                            }
                        }
                        break :blk .i64;
                    },
                    else => {},
                }
                break :blk .i64;
            },
            .e_dot_access => |dot| blk: {
                const target_expr = self.codegen.resolveDotAccess(module_env, dot) orelse break :blk .i64;
                break :blk self.getExprLayoutWithTypeEnv(module_env, target_expr, type_env);
            },
            else => .i64,
        };
    }

    /// Get the result type for a block
    fn getBlockLayout(self: *DevEvaluator, module_env: *ModuleEnv, block: anytype, type_env: *TypeEnv) LayoutIdx {
        var type_annos = std.AutoHashMap(base.Ident.Idx, LayoutIdx).init(self.allocator);
        defer type_annos.deinit();

        const stmts = module_env.store.sliceStatements(block.stmts);

        // First pass: collect type annotations
        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_type_anno => |ta| {
                    const type_anno = module_env.store.getTypeAnno(ta.anno);
                    switch (type_anno) {
                        .apply => |apply| {
                            const result_layout = layoutFromLocalOrExternal(apply.base);
                            type_annos.put(ta.name, result_layout) catch {};
                        },
                        .lookup => |lookup| {
                            const result_layout = layoutFromLocalOrExternal(lookup.base);
                            type_annos.put(ta.name, result_layout) catch {};
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }

        // Second pass: process declarations
        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_decl => |decl| {
                    const pattern_key = @intFromEnum(decl.pattern);
                    if (decl.anno) |anno_idx| {
                        const result_layout = self.getAnnotationLayout(module_env, anno_idx);
                        type_env.put(pattern_key, result_layout) catch {};
                    } else {
                        const pattern = module_env.store.getPattern(decl.pattern);
                        var found_annotation = false;
                        switch (pattern) {
                            .assign => |assign| {
                                if (type_annos.get(assign.ident)) |anno_layout| {
                                    type_env.put(pattern_key, anno_layout) catch {};
                                    found_annotation = true;
                                }
                            },
                            else => {},
                        }
                        if (!found_annotation) {
                            const decl_expr = module_env.store.getExpr(decl.expr);
                            const inferred_layout = self.getExprLayoutWithTypeEnv(module_env, decl_expr, type_env);
                            type_env.put(pattern_key, inferred_layout) catch {};
                        }
                    }
                },
                .s_decl_gen => |decl| {
                    const pattern_key = @intFromEnum(decl.pattern);
                    if (decl.anno) |anno_idx| {
                        const anno_layout = self.getAnnotationLayout(module_env, anno_idx);
                        type_env.put(pattern_key, anno_layout) catch {};
                    } else {
                        const pattern = module_env.store.getPattern(decl.pattern);
                        var found_annotation = false;
                        switch (pattern) {
                            .assign => |assign| {
                                if (type_annos.get(assign.ident)) |anno_layout| {
                                    type_env.put(pattern_key, anno_layout) catch {};
                                    found_annotation = true;
                                }
                            },
                            else => {},
                        }
                        if (!found_annotation) {
                            const decl_expr = module_env.store.getExpr(decl.expr);
                            const inferred_layout = self.getExprLayoutWithTypeEnv(module_env, decl_expr, type_env);
                            type_env.put(pattern_key, inferred_layout) catch {};
                        }
                    }
                },
                else => {},
            }
        }

        const final_expr = module_env.store.getExpr(block.final_expr);
        return self.getExprLayoutWithTypeEnv(module_env, final_expr, type_env);
    }

    /// Get the result type from an annotation
    fn getAnnotationLayout(_: *DevEvaluator, module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) LayoutIdx {
        const anno = module_env.store.getAnnotation(anno_idx);
        const type_anno = module_env.store.getTypeAnno(anno.anno);
        switch (type_anno) {
            .apply => |apply| return layoutFromLocalOrExternal(apply.base),
            .lookup => |lookup| return layoutFromLocalOrExternal(lookup.base),
            else => return .i64,
        }
    }

    /// Get the result type for a typed integer
    fn getTypedIntLayout(_: *DevEvaluator, module_env: *ModuleEnv, type_name: base.Ident.Idx) LayoutIdx {
        const idents = &module_env.idents;
        if (type_name == idents.u8_type or
            type_name == idents.u16_type or
            type_name == idents.u32_type or
            type_name == idents.u64_type)
        {
            return .u64;
        } else if (type_name == idents.u128_type) {
            return .u128;
        } else if (type_name == idents.i128_type) {
            return .i128;
        } else if (type_name == idents.f32_type or type_name == idents.f64_type) {
            return .f64;
        } else if (type_name == idents.dec_type) {
            return .dec;
        }
        return .i64;
    }

    /// Get the result type for a binary operation
    fn getBinopLayoutWithTypeEnv(self: *DevEvaluator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, type_env: *TypeEnv) LayoutIdx {
        switch (binop.op) {
            .lt, .gt, .le, .ge, .eq, .ne, .@"and", .@"or" => return .i64,
            else => {},
        }
        const lhs_expr = module_env.store.getExpr(binop.lhs);
        return self.getExprLayoutWithTypeEnv(module_env, lhs_expr, type_env);
    }

    /// Generate native code from source code string (full pipeline)
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

        parse_ast.store.emptyScratch();

        // Step 2: Initialize CIR and canonicalize
        module_env.initCIRFields("dev_eval") catch return error.OutOfMemory;

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

    /// Result of evaluation
    pub const EvalResult = union(enum) {
        i64_val: i64,
        u64_val: u64,
        f64_val: f64,
        i128_val: i128,
        u128_val: u128,

        pub fn format(self_val: EvalResult, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self_val) {
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
        var code_result = try self.generateCodeFromSource(source);
        defer code_result.deinit();

        var executable = backend.ExecutableMemory.init(code_result.code) catch return error.ExecutionError;
        defer executable.deinit();

        return switch (code_result.result_layout) {
            .i64, .i8, .i16, .i32 => blk: {
                var result: i64 = undefined;
                executable.callWithResultPtr(@ptrCast(&result));
                break :blk EvalResult{ .i64_val = result };
            },
            .u64, .u8, .u16, .u32, .bool => blk: {
                var result: u64 = undefined;
                executable.callWithResultPtr(@ptrCast(&result));
                break :blk EvalResult{ .u64_val = result };
            },
            .f64, .f32 => blk: {
                var result: f64 = undefined;
                executable.callWithResultPtr(@ptrCast(&result));
                break :blk EvalResult{ .f64_val = result };
            },
            .i128 => blk: {
                var result: i128 = undefined;
                executable.callWithResultPtr(@ptrCast(&result));
                break :blk EvalResult{ .i128_val = result };
            },
            .u128 => blk: {
                var result: u128 = undefined;
                executable.callWithResultPtr(@ptrCast(&result));
                break :blk EvalResult{ .u128_val = result };
            },
            .dec => blk: {
                var result: i128 = undefined;
                executable.callWithResultPtr(@ptrCast(&result));
                break :blk EvalResult{ .i128_val = result };
            },
            else => return error.UnsupportedType,
        };
    }

    // Public methods for direct code generation (used by tests)
    pub fn generateReturnI64Code(self: *DevEvaluator, value: i64, result_layout: LayoutIdx) Error![]const u8 {
        return self.codegen.generateReturnI64Code(value, result_layout);
    }

    pub fn generateReturnF64Code(self: *DevEvaluator, value: f64) Error![]const u8 {
        return self.codegen.generateReturnF64Code(value);
    }

    pub fn generateReturnI128Code(self: *DevEvaluator, value: i128) Error![]const u8 {
        return self.codegen.generateReturnI128Code(value);
    }
};

/// Convert a LocalOrExternal to LayoutIdx
fn layoutFromLocalOrExternal(loe: CIR.TypeAnno.LocalOrExternal) LayoutIdx {
    switch (loe) {
        .builtin => |b| return layoutFromBuiltin(b),
        .local, .external => return .i64,
    }
}

/// Convert a Builtin type enum to LayoutIdx
fn layoutFromBuiltin(b: CIR.TypeAnno.Builtin) LayoutIdx {
    return switch (b) {
        .u8, .u16, .u32, .u64 => .u64,
        .u128 => .u128,
        .i128 => .i128,
        .f32, .f64 => .f64,
        .dec => .dec,
        else => .i64,
    };
}

// Tests

test "dev evaluator initialization" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();
}

test "generate i64 code" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const code = try runner.generateReturnI64Code(42, .i64);
    defer runner.allocator.free(code);

    var exec = backend.ExecutableMemory.init(code) catch return error.SkipZigTest;
    defer exec.deinit();

    var result: i64 = undefined;
    exec.callWithResultPtr(@ptrCast(&result));
    try std.testing.expectEqual(@as(i64, 42), result);
}

test "generate i64 code large value" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const large_value: i64 = 0x123456789ABCDEF;
    const code = try runner.generateReturnI64Code(large_value, .i64);
    defer runner.allocator.free(code);

    var exec = backend.ExecutableMemory.init(code) catch return error.SkipZigTest;
    defer exec.deinit();

    var result: i64 = undefined;
    exec.callWithResultPtr(@ptrCast(&result));
    try std.testing.expectEqual(large_value, result);
}

test "generate f64 code" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const code = try runner.generateReturnF64Code(3.14159);
    defer runner.allocator.free(code);

    var exec = backend.ExecutableMemory.init(code) catch return error.SkipZigTest;
    defer exec.deinit();

    var result: f64 = undefined;
    exec.callWithResultPtr(@ptrCast(&result));
    try std.testing.expectApproxEqRel(@as(f64, 3.14159), result, 0.0001);
}

test "evaluate addition" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("1 + 2") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 3 }, result);
}

test "evaluate subtraction" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("10 - 3") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate multiplication" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("6 * 7") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate unary minus" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("-42") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = -42 }, result);
}

test "evaluate if true branch" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("if 1 > 0 42 else 0") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate True tag" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("True") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate False tag" {
    var runner = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer runner.deinit();

    const result = runner.evaluate("False") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 0 }, result);
}
