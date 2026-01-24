//! LLVM-based Evaluator for Roc expressions
//!
//! This module evaluates Roc expressions by:
//! 1. Parsing source code
//! 2. Canonicalizing to CIR
//! 3. Type checking
//! 4. Lowering to Mono IR (globally unique symbols)
//! 5. Generating LLVM bitcode via MonoLlvmCodeGen
//! 6. Compiling to native code via LLVM
//! 7. Executing the generated code
//!
//! This mirrors the dev backend pipeline, except the final code generation
//! produces LLVM IR instead of direct machine code.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mono = @import("mono");
const backend = @import("backend");
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LoadedModule = builtin_loading.LoadedModule;

// Mono IR types
const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;

// LLVM code generation
const MonoLlvmCodeGen = backend.llvm.MonoLlvmCodeGen;

/// Layout index for result types
pub const LayoutIdx = layout.Idx;

/// LLVM-based evaluator for Roc expressions
///
/// Orchestrates the compilation pipeline:
/// - Initializes with builtin modules
/// - Parses, canonicalizes, and type-checks expressions
/// - Lowers to Mono IR
/// - Generates LLVM bitcode
pub const LlvmEvaluator = struct {
    allocator: Allocator,

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
        CompilationFailed,
        UnsupportedLayout,
    };

    /// Initialize the evaluator with builtin modules
    pub fn init(allocator: Allocator) Error!LlvmEvaluator {
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

        return LlvmEvaluator{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
        };
    }

    /// Clean up resources
    pub fn deinit(self: *LlvmEvaluator) void {
        self.builtin_module.deinit();
    }

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        output_layout: layout.Idx,
        is_list: bool,
        is_record: bool,
        record_field_names: ?[]const u8,
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
            if (self.record_field_names) |names| {
                self.allocator.free(names);
            }
        }
    };

    /// Result of code generation
    pub const CodeResult = struct {
        code: []const u8,
        allocator: Allocator,
        result_layout: LayoutIdx,
        entry_offset: usize = 0,

        pub fn deinit(self: *CodeResult) void {
            if (self.code.len > 0) {
                self.allocator.free(self.code);
            }
        }
    };

    /// Generate LLVM bitcode for a CIR expression using the Mono IR pipeline
    ///
    /// This lowers CIR to Mono IR and then generates LLVM bitcode.
    pub fn generateBitcode(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
    ) Error!BitcodeResult {
        // Create module envs array - just the single module and builtin
        const builtin_env = self.builtin_module.env;
        const all_module_envs = [_]*ModuleEnv{ module_env, @constCast(builtin_env) };

        // Create a Mono IR store for lowered expressions
        var mono_store = MonoExprStore.init(self.allocator);
        defer mono_store.deinit();

        // Find the module index for this module
        var module_idx: u16 = 0;
        for (&all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                module_idx = @intCast(i);
                break;
            }
        }

        // Create the lowerer
        var lowerer = MonoLower.init(self.allocator, &mono_store, &all_module_envs, null, null);
        defer lowerer.deinit();

        // Lower the CIR expression to Mono IR
        const mono_expr_id = lowerer.lowerExpr(module_idx, expr_idx) catch {
            return error.UnsupportedExpression;
        };

        // Determine the result layout
        var type_env = std.AutoHashMap(u32, LayoutIdx).init(self.allocator);
        defer type_env.deinit();
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = getExprLayoutWithTypeEnv(self.allocator, module_env, cir_expr, &type_env);

        // Create the LLVM code generator
        var codegen = MonoLlvmCodeGen.init(
            self.allocator,
            &mono_store,
        );
        defer codegen.deinit();

        // Compile all procedures first (for recursive functions)
        const procs = mono_store.getProcs();
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch {
                return error.UnsupportedExpression;
            };
        }

        // Generate LLVM bitcode for the expression
        var gen_result = codegen.generateCode(mono_expr_id, result_layout) catch {
            return error.UnsupportedExpression;
        };
        defer gen_result.deinit();

        // Copy the bitcode since gen_result owns it
        const bitcode_copy = self.allocator.dupe(u32, gen_result.bitcode) catch return error.OutOfMemory;

        // Detect list and record expressions
        const is_list = switch (cir_expr) {
            .e_empty_list, .e_list => true,
            else => false,
        };
        const is_record = switch (cir_expr) {
            .e_record, .e_empty_record => true,
            else => false,
        };

        return BitcodeResult{
            .bitcode = bitcode_copy,
            .output_layout = result_layout,
            .is_list = is_list,
            .is_record = is_record,
            .record_field_names = null,
            .allocator = self.allocator,
        };
    }

    /// Generate code for a CIR expression (full pipeline with lowering)
    ///
    /// This lowers CIR to Mono IR and then generates LLVM bitcode.
    pub fn generateCode(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
    ) Error!CodeResult {
        // Create a Mono IR store for lowered expressions
        var mono_store = MonoExprStore.init(self.allocator);
        defer mono_store.deinit();

        // Find the module index for this module
        var module_idx: u16 = 0;
        for (all_module_envs, 0..) |env, i| {
            if (env == module_env) {
                module_idx = @intCast(i);
                break;
            }
        }

        // Create the lowerer
        var lowerer = MonoLower.init(self.allocator, &mono_store, all_module_envs, null, null);
        defer lowerer.deinit();

        // Lower the CIR expression to Mono IR
        const mono_expr_id = lowerer.lowerExpr(module_idx, expr_idx) catch {
            return error.UnsupportedExpression;
        };

        // Determine the result layout
        var type_env = std.AutoHashMap(u32, LayoutIdx).init(self.allocator);
        defer type_env.deinit();
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = getExprLayoutWithTypeEnv(self.allocator, module_env, cir_expr, &type_env);

        // Create the LLVM code generator
        var codegen = MonoLlvmCodeGen.init(
            self.allocator,
            &mono_store,
        );
        defer codegen.deinit();

        // Compile all procedures first (for recursive functions)
        const procs = mono_store.getProcs();
        if (procs.len > 0) {
            codegen.compileAllProcs(procs) catch {
                return error.UnsupportedExpression;
            };
        }

        // Generate LLVM bitcode
        var gen_result = codegen.generateCode(mono_expr_id, result_layout) catch {
            return error.UnsupportedExpression;
        };
        defer gen_result.deinit();

        // Convert bitcode to bytes for the result
        const bitcode_bytes = std.mem.sliceAsBytes(gen_result.bitcode);
        const code_copy = self.allocator.dupe(u8, bitcode_bytes) catch return error.OutOfMemory;

        return CodeResult{
            .code = code_copy,
            .allocator = self.allocator,
            .result_layout = result_layout,
        };
    }
};

/// Get the Layout for a CIR expression
fn getExprLayoutWithTypeEnv(allocator: Allocator, module_env: *ModuleEnv, expr: CIR.Expr, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    return switch (expr) {
        .e_num => |num| switch (num.kind) {
            .i8, .i16, .i32, .i64, .num_unbound, .int_unbound => .i64,
            .u8, .u16, .u32, .u64 => .u64,
            .i128 => .i128,
            .u128 => .u128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
        },
        .e_frac_f32 => .f32,
        .e_frac_f64 => .f64,
        .e_dec, .e_dec_small => .dec,
        .e_typed_int => |ti| getTypedIntLayout(module_env, ti.type_name),
        .e_binop => |binop| getBinopLayout(allocator, module_env, binop, type_env),
        .e_unary_minus => |unary| blk: {
            const inner_expr = module_env.store.getExpr(unary.expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, inner_expr, type_env);
        },
        .e_nominal => |nom| blk: {
            const backing_expr = module_env.store.getExpr(nom.backing_expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, backing_expr, type_env);
        },
        .e_nominal_external => |nom| blk: {
            const backing_expr = module_env.store.getExpr(nom.backing_expr);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, backing_expr, type_env);
        },
        .e_if => |if_expr| blk: {
            const else_expr = module_env.store.getExpr(if_expr.final_else);
            break :blk getExprLayoutWithTypeEnv(allocator, module_env, else_expr, type_env);
        },
        .e_block => |block| getBlockLayout(allocator, module_env, block, type_env),
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
                                break :blk getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                            }
                        },
                        else => {},
                    }
                    break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
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
                                        break :blk getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
                                    }
                                },
                                else => {},
                            }
                            break :blk getExprLayoutWithTypeEnv(allocator, module_env, body_expr, type_env);
                        },
                        else => {},
                    }
                    break :blk .i64;
                },
                .e_lookup_local => {
                    const args = module_env.store.sliceExpr(call.args);
                    if (args.len > 0) {
                        const arg_expr = module_env.store.getExpr(args[0]);
                        const arg_layout = getExprLayoutWithTypeEnv(allocator, module_env, arg_expr, type_env);
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
            const receiver_expr = module_env.store.getExpr(dot.receiver);
            switch (receiver_expr) {
                .e_record => |rec| {
                    const fields = module_env.store.sliceRecordFields(rec.fields);
                    for (fields) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        if (@as(u32, @bitCast(field.name)) == @as(u32, @bitCast(dot.field_name))) {
                            const field_expr = module_env.store.getExpr(field.value);
                            break :blk getExprLayoutWithTypeEnv(allocator, module_env, field_expr, type_env);
                        }
                    }
                },
                else => {},
            }
            break :blk .i64;
        },
        else => .i64,
    };
}

/// Get the result type for a block
fn getBlockLayout(allocator: Allocator, module_env: *ModuleEnv, block: anytype, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    var type_annos = std.AutoHashMap(base.Ident.Idx, LayoutIdx).init(allocator);
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
                    const result_layout = getAnnotationLayout(module_env, anno_idx);
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
                        const inferred_layout = getExprLayoutWithTypeEnv(allocator, module_env, decl_expr, type_env);
                        type_env.put(pattern_key, inferred_layout) catch {};
                    }
                }
            },
            else => {},
        }
    }

    const final_expr = module_env.store.getExpr(block.final_expr);
    return getExprLayoutWithTypeEnv(allocator, module_env, final_expr, type_env);
}

/// Get the result type from an annotation
fn getAnnotationLayout(module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) LayoutIdx {
    const anno = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(anno.anno);
    switch (type_anno) {
        .apply => |apply| return layoutFromLocalOrExternal(apply.base),
        .lookup => |lookup| return layoutFromLocalOrExternal(lookup.base),
        else => return .i64,
    }
}

/// Get the result type for a typed integer
fn getTypedIntLayout(module_env: *ModuleEnv, type_name: base.Ident.Idx) LayoutIdx {
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
    } else if (type_name == idents.f32_type) {
        return .f32;
    } else if (type_name == idents.f64_type) {
        return .f64;
    } else if (type_name == idents.dec_type) {
        return .dec;
    }
    return .i64;
}

/// Get the result type for a binary operation
fn getBinopLayout(allocator: Allocator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, type_env: *std.AutoHashMap(u32, LayoutIdx)) LayoutIdx {
    switch (binop.op) {
        .lt, .gt, .le, .ge, .eq, .ne, .@"and", .@"or" => return .i64,
        else => {},
    }
    const lhs_expr = module_env.store.getExpr(binop.lhs);
    return getExprLayoutWithTypeEnv(allocator, module_env, lhs_expr, type_env);
}

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
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        else => .i64,
    };
}

// Tests

test "llvm evaluator initialization" {
    var evaluator = LlvmEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();
}
