//! LLVM-based Evaluator for Roc expressions
//!
//! This module provides an alternative to the interpreter that uses LLVM
//! to compile and execute Roc expressions. It's used when the `--optimize`
//! flag is passed to the REPL.
//!
//! The evaluator works by:
//! 1. Taking a CIR expression
//! 2. Translating it to LLVM IR
//! 3. Compiling to object code
//! 4. Linking with a minimal runtime
//! 5. Executing and capturing the result
//!
//! Note: For the initial implementation, we use ahead-of-time compilation.
//! JIT compilation can be added later for better performance.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const parse = @import("parse");
const check = @import("check");
const layout = @import("layout");
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("mod.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;

/// LLVM-based evaluator for Roc expressions
pub const LlvmEvaluator = struct {
    allocator: Allocator,

    /// Temporary directory for object files
    temp_dir: ?[]const u8,

    /// Counter for unique file names
    counter: u64,

    /// Builtin type declaration indices (loaded once at startup)
    builtin_indices: CIR.BuiltinIndices,

    /// Loaded Builtin module (loaded once at startup)
    builtin_module: builtin_loading.LoadedModule,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        LinkingFailed,
        ExecutionFailed,
        UnsupportedType,
        NotImplemented,
        ParseError,
        CanonicalizeError,
        TypeError,
    };

    /// Initialize a new LLVM evaluator
    pub fn init(allocator: Allocator) Error!LlvmEvaluator {
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

        return LlvmEvaluator{
            .allocator = allocator,
            .temp_dir = null,
            .counter = 0,
            .builtin_indices = builtin_indices,
            .builtin_module = builtin_module,
        };
    }

    /// Clean up the evaluator
    pub fn deinit(self: *LlvmEvaluator) void {
        if (self.temp_dir) |dir| {
            self.allocator.free(dir);
        }
        self.builtin_module.deinit();
    }

    /// Evaluate a CIR expression and return its string representation
    /// This is the main entry point for REPL evaluation with LLVM
    pub fn evalToString(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        type_var: types.Var,
    ) Error![]const u8 {
        _ = type_var;
        _ = expr_idx;
        _ = module_env;

        // TODO: Implement the full pipeline:
        // 1. Run monomorphization on the expression
        // 2. Run closure transformation
        // 3. Translate CIR to LLVM IR using emit.zig
        // 4. Compile to object code using codegen.zig
        // 5. Link with runtime
        // 6. Execute and capture output
        // 7. Parse output and format as Roc value

        // For now, return a placeholder indicating LLVM backend is not yet implemented
        return try self.allocator.dupe(u8, "<LLVM backend: not yet implemented>");
    }

    /// Evaluate a simple numeric expression (for testing)
    /// This provides a simpler path for testing the LLVM pipeline
    pub fn evalNumericExpr(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
    ) Error!i64 {
        _ = self;
        _ = module_env;
        _ = expr_idx;

        // TODO: Implement numeric expression evaluation
        // This is a stepping stone before full expression support
        return error.NotImplemented;
    }

    /// Evaluate source code directly (for snapshot testing)
    /// This does the full pipeline: parse → canonicalize → type check → evaluate
    pub fn evalSourceToString(self: *LlvmEvaluator, source: []const u8) Error![]const u8 {
        // Step 1: Create module environment and parse
        var module_env = ModuleEnv.init(self.allocator, source) catch return error.OutOfMemory;
        defer module_env.deinit();

        var parse_ast = parse.parseExpr(&module_env.common, self.allocator) catch {
            // Parse failed - return placeholder so comparison is skipped
            // TODO: Match interpreter's error message format
            return self.allocator.dupe(u8, "<LLVM backend: parse error>") catch return error.OutOfMemory;
        };
        defer parse_ast.deinit(self.allocator);

        if (parse_ast.hasErrors()) {
            // Parse had errors - return placeholder so comparison is skipped
            // TODO: Match interpreter's error message format
            return self.allocator.dupe(u8, "<LLVM backend: parse error>") catch return error.OutOfMemory;
        }

        // Step 2: Initialize CIR and canonicalize
        module_env.initCIRFields("llvm_eval") catch return error.OutOfMemory;

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
            .module_name = module_env.insertIdent(base.Ident.for_text("llvm_eval")) catch return error.OutOfMemory,
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

        // Step 4: Evaluate the expression (constant folding for now)
        // For simple numeric expressions, we can evaluate at compile time
        const expr = module_env.store.getExpr(final_expr_idx);
        return self.evalExprToString(&module_env, expr);
    }

    /// Evaluate a CIR expression and format as string
    /// Currently supports numeric literals - will be expanded
    fn evalExprToString(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error![]const u8 {
        switch (expr) {
            .e_num => |num| {
                // Format integer - match interpreter behavior (no type annotations)
                const int_value = num.value.toI128();
                return switch (num.kind) {
                    .u8, .u16, .u32, .u64, .u128 => blk: {
                        // For unsigned types, display as unsigned
                        const unsigned: u128 = @bitCast(int_value);
                        break :blk std.fmt.allocPrint(self.allocator, "{d}", .{unsigned}) catch return error.OutOfMemory;
                    },
                    .i8, .i16, .i32, .i64, .i128, .num_unbound, .int_unbound => blk: {
                        // For signed types and unbound, display as signed
                        break :blk std.fmt.allocPrint(self.allocator, "{d}", .{int_value}) catch return error.OutOfMemory;
                    },
                    else => return error.UnsupportedType,
                };
            },
            .e_frac_f32 => |frac| {
                return std.fmt.allocPrint(self.allocator, "{d}", .{frac.value}) catch return error.OutOfMemory;
            },
            .e_frac_f64 => |frac| {
                return std.fmt.allocPrint(self.allocator, "{d}", .{frac.value}) catch return error.OutOfMemory;
            },
            .e_dec => |dec| {
                // Dec values - format without trailing zeros for integers
                const value = dec.value;
                // RocDec stores value * 10^18, so divide to get actual value
                const scaled: f64 = @as(f64, @floatFromInt(value.num)) / 1e18;
                // Check if it's a whole number
                if (@floor(scaled) == scaled) {
                    return std.fmt.allocPrint(self.allocator, "{d}", .{@as(i64, @intFromFloat(scaled))}) catch return error.OutOfMemory;
                } else {
                    return std.fmt.allocPrint(self.allocator, "{d}", .{scaled}) catch return error.OutOfMemory;
                }
            },
            .e_dec_small => |dec| {
                // Small decimal stored as rational number
                const numerator: f64 = @floatFromInt(dec.value.numerator);
                const divisor: f64 = std.math.pow(f64, 10.0, @floatFromInt(dec.value.denominator_power_of_ten));
                const value = numerator / divisor;
                // Check if it's a whole number
                if (@floor(value) == value and value >= -9007199254740992 and value <= 9007199254740992) {
                    return std.fmt.allocPrint(self.allocator, "{d}", .{@as(i64, @intFromFloat(value))}) catch return error.OutOfMemory;
                } else {
                    return std.fmt.allocPrint(self.allocator, "{d}", .{value}) catch return error.OutOfMemory;
                }
            },
            .e_str_segment => |seg| {
                const literal = module_env.getString(seg.literal);
                return std.fmt.allocPrint(self.allocator, "\"{s}\"", .{literal}) catch return error.OutOfMemory;
            },
            .e_empty_list => {
                return self.allocator.dupe(u8, "[]") catch return error.OutOfMemory;
            },
            .e_empty_record => {
                return self.allocator.dupe(u8, "{}") catch return error.OutOfMemory;
            },
            .e_zero_argument_tag => |tag| {
                const name = module_env.getIdent(tag.name);
                return self.allocator.dupe(u8, name) catch return error.OutOfMemory;
            },
            else => {
                // For unsupported expressions, return placeholder
                return self.allocator.dupe(u8, "<LLVM backend: not yet implemented>") catch return error.OutOfMemory;
            },
        }
    }

    /// Get the target triple for the current host
    fn getHostTriple(self: *LlvmEvaluator) []const u8 {
        _ = self;
        return switch (builtin.os.tag) {
            .linux => switch (builtin.cpu.arch) {
                .x86_64 => "x86_64-linux-gnu",
                .aarch64 => "aarch64-linux-gnu",
                else => "unknown-linux-gnu",
            },
            .macos => switch (builtin.cpu.arch) {
                .x86_64 => "x86_64-macos-none",
                .aarch64 => "aarch64-macos-none",
                else => "unknown-macos-none",
            },
            .windows => "x86_64-windows-msvc",
            else => "unknown-unknown-unknown",
        };
    }

    /// Generate a unique temporary file path
    fn getTempPath(self: *LlvmEvaluator, extension: []const u8) Error![]const u8 {
        self.counter += 1;
        return std.fmt.allocPrint(
            self.allocator,
            "/tmp/roc_llvm_{d}{s}",
            .{ self.counter, extension },
        ) catch return error.OutOfMemory;
    }

    /// Execute a compiled program and capture its output
    fn executeAndCapture(self: *LlvmEvaluator, exe_path: []const u8) Error![]const u8 {
        var child = std.process.Child.init(
            &.{exe_path},
            self.allocator,
        );
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        child.spawn() catch return error.ExecutionFailed;

        const stdout = child.stdout orelse return error.ExecutionFailed;
        const output = stdout.reader().readAllAlloc(self.allocator, 1024 * 1024) catch return error.ExecutionFailed;

        const term = child.wait() catch return error.ExecutionFailed;

        if (term.Exited != 0) {
            self.allocator.free(output);
            return error.ExecutionFailed;
        }

        return output;
    }
};

/// LLVM emit NumKind enum (duplicated here since we can't import from backend)
/// This must be kept in sync with src/backend/llvm/emit.zig
pub const LlvmNumKind = enum {
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    numeral,
};

/// Translate a CIR NumKind to the LLVM emit NumKind
/// Returns null for unbound numeric types that need type inference first
pub fn cirNumKindToLlvmNumKind(cir_kind: CIR.NumKind) ?LlvmNumKind {
    return switch (cir_kind) {
        // Unbound types need to be resolved through type inference first
        .num_unbound, .int_unbound => null,
        // Concrete numeric types
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
    };
}

/// Convert CIR NumKind to display name matching interpreter output
fn numKindToTypeName(kind: CIR.NumKind) []const u8 {
    return switch (kind) {
        .u8 => "U8",
        .i8 => "I8",
        .u16 => "U16",
        .i16 => "I16",
        .u32 => "U32",
        .i32 => "I32",
        .u64 => "U64",
        .i64 => "I64",
        .u128 => "U128",
        .i128 => "I128",
        .f32 => "F32",
        .f64 => "F64",
        .dec => "Dec",
        .num_unbound, .int_unbound => "",
    };
}

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works
    try std.testing.expect(evaluator.counter == 0);
}
