//! LLVM-based Evaluator for Roc expressions
//!
//! This module provides an alternative to the interpreter that uses LLVM
//! to compile and execute Roc expressions. It's used when the `--optimize`
//! flag is passed to the REPL.
//!
//! The evaluator works by:
//! 1. Taking a CIR expression
//! 2. Translating it to LLVM IR using the Builder
//! 3. Compiling bitcode to object code using LLVM bindings
//! 4. Linking with LLD (for cross-compilation support)
//! 5. Executing and capturing the result
//!
//! Note: Currently uses direct evaluation for numeric expressions.
//! Full LLVM compilation/linking is available when integrated with the CLI.

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

// LLVM Builder from Zig's standard library (for IR generation)
const llvm = @import("std").zig.llvm;
const LlvmBuilder = llvm.Builder;

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
        _: *ModuleEnv,
        _: CIR.Expr.Idx,
        _: types.Var,
    ) Error![]const u8 {
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
        _: *LlvmEvaluator,
        _: *ModuleEnv,
        _: CIR.Expr.Idx,
    ) Error!i64 {
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

        // Step 4: Generate LLVM IR, compile, and execute
        const expr = module_env.store.getExpr(final_expr_idx);
        return self.compileAndExecuteExpr(&module_env, expr) catch |err| {
            // For unsupported or compilation errors, return placeholder so tests can continue
            return switch (err) {
                error.UnsupportedType => self.allocator.dupe(u8, "<LLVM backend: unsupported expression type>") catch return error.OutOfMemory,
                error.CompilationFailed => self.allocator.dupe(u8, "<LLVM backend: compilation failed>") catch return error.OutOfMemory,
                error.LinkingFailed => self.allocator.dupe(u8, "<LLVM backend: linking failed>") catch return error.OutOfMemory,
                error.ExecutionFailed => self.allocator.dupe(u8, "<LLVM backend: execution failed>") catch return error.OutOfMemory,
                else => err,
            };
        };
    }

    /// Compile a CIR expression to LLVM IR, build executable, run it, and return output
    /// Currently uses direct evaluation. Full LLVM compilation/linking is available
    /// when integrated with the CLI which has access to LLVM bindings and LLD.
    fn compileAndExecuteExpr(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error![]const u8 {
        // Use direct evaluation for now
        // Full LLVM compile/link pipeline requires CLI integration for LLVM bindings
        return self.evalExprDirectly(module_env, expr);
    }

    /// Type of the result value for JIT execution
    pub const ResultType = enum {
        i64,
        i128,
        f64,
    };

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        result_type: ResultType,
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    /// Generate LLVM bitcode for a CIR expression
    /// Returns the bitcode and whether it's a float type (for printf formatting)
    /// The caller is responsible for freeing the bitcode via result.deinit()
    pub fn generateBitcode(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error!BitcodeResult {
        // Create LLVM Builder
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_repl_eval",
            .target = &builtin.target,
        }) catch return error.OutOfMemory;
        defer builder.deinit();

        // Generate LLVM IR for the expression
        const value_type = try self.getExprLlvmType(&builder, expr);
        const value = try self.emitExprValue(&builder, module_env, expr);

        // Determine the result type for JIT execution
        const result_type: ResultType = switch (value_type) {
            .float, .double => .f64,
            .i128 => .i128,
            else => .i64,
        };

        // Generate a main function that prints the result
        try self.emitMainWithPrint(&builder, value_type, value);

        // Serialize to bitcode
        const producer = LlvmBuilder.Producer{
            .name = "Roc LLVM Evaluator",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

        const bitcode = builder.toBitcode(self.allocator, producer) catch return error.CompilationFailed;

        return BitcodeResult{
            .bitcode = bitcode,
            .result_type = result_type,
            .allocator = self.allocator,
        };
    }

    /// Generate bitcode from source code string
    /// This does the full pipeline: parse → canonicalize → type check → generate bitcode
    /// The caller is responsible for compiling and executing the bitcode using llvm_compile.
    pub fn generateBitcodeFromSource(self: *LlvmEvaluator, source: []const u8) Error!BitcodeResult {
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

        // Step 4: Generate bitcode
        const expr = module_env.store.getExpr(final_expr_idx);
        return self.generateBitcode(&module_env, expr);
    }

    /// Verify that we can generate LLVM IR for an expression (for testing purposes)
    fn canGenerateLlvmIr(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) bool {
        // Create LLVM Builder
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_repl_eval",
            .target = &builtin.target,
        }) catch return false;
        defer builder.deinit();

        // Try to generate LLVM type and value for the expression
        _ = self.getExprLlvmType(&builder, expr) catch return false;
        _ = self.emitExprValue(&builder, module_env, expr) catch return false;

        return true;
    }

    /// Get the LLVM type for a CIR expression
    fn getExprLlvmType(_: *LlvmEvaluator, builder: *LlvmBuilder, expr: CIR.Expr) !LlvmBuilder.Type {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .u8, .i8 => .i8,
                .u16, .i16 => .i16,
                .u32, .i32 => .i32,
                .u64, .i64, .num_unbound, .int_unbound => .i64,
                .u128, .i128 => .i128,
                else => error.UnsupportedType,
            },
            .e_frac_f32 => .float,
            .e_frac_f64 => .double,
            .e_dec, .e_dec_small => .double, // Represent Dec as double for printing
            else => try builder.ptrType(.default), // For complex types, use pointer
        };
    }

    /// Emit LLVM value for a CIR expression
    fn emitExprValue(self: *LlvmEvaluator, builder: *LlvmBuilder, _: *ModuleEnv, expr: CIR.Expr) !LlvmBuilder.Constant {
        return switch (expr) {
            .e_num => |num| {
                const int_value = num.value.toI128();
                const llvm_type = try self.getExprLlvmType(builder, expr);
                return builder.intConst(llvm_type, int_value) catch return error.CompilationFailed;
            },
            .e_frac_f32 => |frac| {
                return builder.floatConst(frac.value) catch return error.CompilationFailed;
            },
            .e_frac_f64 => |frac| {
                return builder.doubleConst(frac.value) catch return error.CompilationFailed;
            },
            .e_dec => |dec| {
                const scaled: f64 = @as(f64, @floatFromInt(dec.value.num)) / 1e18;
                return builder.doubleConst(scaled) catch return error.CompilationFailed;
            },
            .e_dec_small => |dec| {
                const numerator: f64 = @floatFromInt(dec.value.numerator);
                const divisor: f64 = std.math.pow(f64, 10.0, @floatFromInt(dec.value.denominator_power_of_ten));
                return builder.doubleConst(numerator / divisor) catch return error.CompilationFailed;
            },
            else => return error.UnsupportedType,
        };
    }

    /// Emit an eval function that returns the computed value directly.
    /// For JIT execution, this avoids printf complexity and vararg ABI issues.
    /// Returns the value in its native LLVM type.
    fn emitMainWithPrint(_: *LlvmEvaluator, builder: *LlvmBuilder, value_type: LlvmBuilder.Type, value: LlvmBuilder.Constant) !void {
        // Use the actual value type as the return type (no conversion needed)
        const return_type = value_type;

        // Create eval function
        const eval_type = try builder.fnType(return_type, &.{}, .normal);
        const eval_name = if (builtin.os.tag == .macos)
            try builder.strtabString("\x01_roc_eval") // \x01 prefix tells LLVM to use name verbatim
        else
            try builder.strtabString("roc_eval");
        const eval_fn = try builder.addFunction(eval_type, eval_name, .default);
        eval_fn.setLinkage(.external, builder);

        // Build eval function body
        var wip = try LlvmBuilder.WipFunction.init(builder, .{
            .function = eval_fn,
            .strip = false,
        });
        defer wip.deinit();

        const entry_block = try wip.block(0, "entry");
        wip.cursor = .{ .block = entry_block };

        // Return the value directly - types match exactly
        _ = try wip.ret(value.toValue());
        try wip.finish();
    }

    /// Fallback: evaluate expression directly without LLVM (for unsupported types)
    fn evalExprDirectly(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error![]const u8 {
        switch (expr) {
            // Numeric expressions - format as strings
            .e_num => |num| {
                const int_value = num.value.toI128();
                return std.fmt.allocPrint(self.allocator, "{d}", .{int_value}) catch return error.OutOfMemory;
            },
            .e_frac_f32 => |frac| {
                return std.fmt.allocPrint(self.allocator, "{d}", .{frac.value}) catch return error.OutOfMemory;
            },
            .e_frac_f64 => |frac| {
                return std.fmt.allocPrint(self.allocator, "{d}", .{frac.value}) catch return error.OutOfMemory;
            },
            .e_dec => |dec| {
                // Dec is stored as an integer scaled by 10^18
                const scaled: f64 = @as(f64, @floatFromInt(dec.value.num)) / 1e18;
                return std.fmt.allocPrint(self.allocator, "{d}", .{scaled}) catch return error.OutOfMemory;
            },
            .e_dec_small => |dec| {
                const numerator: f64 = @floatFromInt(dec.value.numerator);
                const divisor: f64 = std.math.pow(f64, 10.0, @floatFromInt(dec.value.denominator_power_of_ten));
                return std.fmt.allocPrint(self.allocator, "{d}", .{numerator / divisor}) catch return error.OutOfMemory;
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
                return error.UnsupportedType;
            },
        }
    }

    /// Get the target triple for the current host
    fn getHostTriple(_: *LlvmEvaluator) []const u8 {
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

// ============================================================================
// Platform-Specific i128 ABI Handling
// ============================================================================
//
// Different platforms represent i128 values differently when passing them
// to/from functions compiled by Zig:
//
// - Windows: Uses <2 x i64> vector type for i128 values
// - macOS ARM64: Uses [2 x i64] array type for i128 values
// - Other platforms (x86-64 Linux/macOS): Use native i128 type
//
// This matters when calling Zig-compiled builtin functions (like Dec operations)
// that take or return i128. The Roc builtins are compiled from Zig to LLVM
// bitcode, and Zig's code generation for i128 varies by platform.
//
// When generating LLVM IR that calls into these builtins, we need to:
// 1. Convert our native i128 values to the platform-specific format before calling
// 2. Convert the return value back to native i128 after the call
//
// Currently, the LLVM evaluator only handles numeric literals without calling
// any builtins, so this ABI handling is not yet active. When builtin function
// call support is added, use the helpers below.

/// Platform-specific representation used for i128 values in function calls
pub const I128Repr = enum {
    /// Native i128 type - used on x86-64 Linux and macOS x86-64
    native,
    /// <2 x i64> vector type - used on Windows
    vector_2xi64,
    /// [2 x i64] array type - used on macOS ARM64
    array_2xi64,
};

/// Detect the i128 representation needed for the current target platform.
/// This should be used when generating calls to Zig-compiled builtin functions.
pub fn getI128Repr() I128Repr {
    if (builtin.os.tag == .windows) {
        return .vector_2xi64;
    } else if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        return .array_2xi64;
    } else {
        return .native;
    }
}

/// Check if i128 values need ABI conversion for the current platform.
/// Returns true if we're on Windows or macOS ARM64.
pub fn needsI128Conversion() bool {
    return getI128Repr() != .native;
}

// TODO: When adding builtin function call support, implement these helpers:
//
// /// Convert an i128 LLVM value to the platform-specific representation.
// /// Use this before passing i128 arguments to Zig-compiled builtins.
// pub fn prepareI128Arg(builder: *LlvmBuilder, value: LlvmBuilder.Value) !LlvmBuilder.Value {
//     return switch (getI128Repr()) {
//         .native => value,
//         .vector_2xi64 => // bitcast i128 to <2 x i64>
//         .array_2xi64 => // bitcast i128 to [2 x i64]
//     };
// }
//
// /// Convert a platform-specific i128 representation back to native i128.
// /// Use this after receiving i128 return values from Zig-compiled builtins.
// pub fn normalizeI128Return(builder: *LlvmBuilder, value: LlvmBuilder.Value) !LlvmBuilder.Value {
//     return switch (getI128Repr()) {
//         .native => value,
//         .vector_2xi64 => // bitcast <2 x i64> to i128
//         .array_2xi64 => // bitcast [2 x i64] to i128
//     };
// }

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works
    try std.testing.expect(evaluator.counter == 0);
}

test "i128 platform detection" {
    const repr = getI128Repr();

    // Verify the platform detection returns a valid value
    switch (repr) {
        .native => {
            // On native platforms, no conversion is needed
            try std.testing.expect(!needsI128Conversion());
        },
        .vector_2xi64 => {
            // Windows uses vector representation
            try std.testing.expect(builtin.os.tag == .windows);
            try std.testing.expect(needsI128Conversion());
        },
        .array_2xi64 => {
            // macOS ARM64 uses array representation
            try std.testing.expect(builtin.os.tag == .macos);
            try std.testing.expect(builtin.cpu.arch == .aarch64);
            try std.testing.expect(needsI128Conversion());
        },
    }
}
