//! LLVM-based Evaluator for Roc expressions
//!
//! This module provides an alternative to the interpreter that uses LLVM
//! to compile and execute Roc expressions. It's used when the `--opt=size`
//! or `--opt=speed` flags are passed to the REPL.
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
const target = @import("target");

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

    /// Type of the result value for JIT execution.
    /// This enum must be kept in sync with llvm_compile.ResultType.
    /// The snapshot_tool depends on both having identical variants in the same order.
    /// See llvm_compile/compile.zig for the canonical definition.
    pub const ResultType = enum {
        i64,
        u64,
        i128,
        u128,
        f64,
        dec,

        /// Compile-time validation that this enum matches the expected structure.
        /// This helps catch accidental divergence from llvm_compile.ResultType.
        /// Using ordinal comparison instead of string comparison to comply with lint rules.
        pub fn validate() void {
            comptime {
                // Validate there are exactly 6 variants by checking that ordinal 5 is the max
                if (@typeInfo(ResultType).@"enum".fields.len != 6) @compileError("ResultType must have exactly 6 variants");
                // Validate ordinals match expected order
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
        // We need to look at the original expression to get signedness,
        // since LLVM types don't distinguish signed from unsigned
        const result_type: ResultType = self.getExprResultType(expr);

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

    /// Get the ResultType for JIT execution from a CIR expression
    /// This captures signedness which LLVM types don't distinguish
    fn getExprResultType(_: *LlvmEvaluator, expr: CIR.Expr) ResultType {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                // Signed types that fit in i64
                .i8, .i16, .i32, .i64, .num_unbound, .int_unbound => .i64,
                // Unsigned types that fit in u64
                .u8, .u16, .u32, .u64 => .u64,
                // 128-bit signed
                .i128 => .i128,
                // 128-bit unsigned
                .u128 => .u128,
                // Float types
                .f32, .f64 => .f64,
                // Dec type (fixed-point decimal stored as i128)
                .dec => .dec,
            },
            .e_frac_f32, .e_frac_f64 => .f64,
            .e_dec, .e_dec_small => .dec,
            else => .i64, // Default for other expression types
        };
    }

    /// Get the LLVM type for a CIR expression
    fn getExprLlvmType(_: *LlvmEvaluator, builder: *LlvmBuilder, expr: CIR.Expr) !LlvmBuilder.Type {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .u8, .i8 => .i8,
                .u16, .i16 => .i16,
                .u32, .i32 => .i32,
                .u64, .i64, .num_unbound, .int_unbound => .i64,
                .u128, .i128, .dec => .i128, // Dec is stored as i128 (scaled by 10^18)
                .f32 => .float,
                .f64 => .double,
            },
            .e_frac_f32 => .float,
            .e_frac_f64 => .double,
            .e_dec, .e_dec_small => .i128, // Dec is stored as i128 (scaled by 10^18)
            else => try builder.ptrType(.default), // For complex types, use pointer
        };
    }

    /// Emit LLVM value for a CIR expression
    ///
    /// Note on u128 handling: We use toI128() for all integer values, including u128.
    /// For u128 values larger than i128 max, this reinterprets the bits as a negative
    /// i128. This works correctly because LLVM's i128 type is just 128 bits - signedness
    /// is a matter of interpretation. The JIT execution in compile.zig interprets the
    /// return value based on ResultType (u128 vs i128) to display the correct value.
    fn emitExprValue(self: *LlvmEvaluator, builder: *LlvmBuilder, _: *ModuleEnv, expr: CIR.Expr) !LlvmBuilder.Constant {
        return switch (expr) {
            .e_num => |num| {
                const int_value = num.value.toI128();
                // Handle float suffixes (e.g., 42f32, 42f64)
                return switch (num.kind) {
                    .f32 => builder.floatConst(@floatFromInt(int_value)) catch return error.CompilationFailed,
                    .f64 => builder.doubleConst(@floatFromInt(int_value)) catch return error.CompilationFailed,
                    else => blk: {
                        const llvm_type = try self.getExprLlvmType(builder, expr);
                        break :blk builder.intConst(llvm_type, int_value) catch return error.CompilationFailed;
                    },
                };
            },
            .e_frac_f32 => |frac| {
                return builder.floatConst(frac.value) catch return error.CompilationFailed;
            },
            .e_frac_f64 => |frac| {
                return builder.doubleConst(frac.value) catch return error.CompilationFailed;
            },
            .e_dec => |dec| {
                // Dec is stored as i128 internally (scaled by 10^18)
                return builder.intConst(.i128, dec.value.num) catch return error.CompilationFailed;
            },
            .e_dec_small => |dec| {
                // Convert small Dec representation to full i128
                // RocDec.decimal_places is 18
                const decimal_places: u5 = 18;
                const scale_factor = std.math.pow(i128, 10, decimal_places - dec.value.denominator_power_of_ten);
                const scaled_value = @as(i128, dec.value.numerator) * scale_factor;
                return builder.intConst(.i128, scaled_value) catch return error.CompilationFailed;
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
                // Dec is stored as i128 internally (scaled by 10^18)
                return formatDecValue(self.allocator, dec.value.num) catch return error.OutOfMemory;
            },
            .e_dec_small => |dec| {
                // Convert small Dec representation to full i128
                const decimal_places: u5 = 18;
                const scale_factor = std.math.pow(i128, 10, decimal_places - dec.value.denominator_power_of_ten);
                const scaled_value = @as(i128, dec.value.numerator) * scale_factor;
                return formatDecValue(self.allocator, scaled_value) catch return error.OutOfMemory;
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
        return target.RocTarget.detectNative().toTriple();
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

/// Dec (fixed-point decimal) has 18 decimal places.
/// The internal representation is i128 scaled by 10^18.
const dec_decimal_places: u5 = 18;
const dec_scale_factor: i128 = blk: {
    var result: i128 = 1;
    for (0..dec_decimal_places) |_| {
        result *= 10;
    }
    break :blk result;
};

/// Format a Dec value (i128 with 18 decimal places) as a string.
/// This preserves full precision unlike f64 conversion.
fn formatDecValue(allocator: std.mem.Allocator, num: i128) std.mem.Allocator.Error![]u8 {
    if (num == 0) {
        return try allocator.dupe(u8, "0");
    }

    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();

    const is_negative = num < 0;
    // Use @abs which handles i128 min correctly by returning u128
    const abs_value: u128 = @abs(num);

    if (is_negative) {
        try out.append('-');
    }

    const integer_part = @divTrunc(abs_value, @as(u128, @intCast(dec_scale_factor)));
    const fractional_part = @rem(abs_value, @as(u128, @intCast(dec_scale_factor)));

    // Format integer part
    var int_buf: [40]u8 = undefined;
    const int_str = std.fmt.bufPrint(&int_buf, "{d}", .{integer_part}) catch unreachable;
    try out.appendSlice(int_str);

    if (fractional_part == 0) {
        return try out.toOwnedSlice();
    }

    try out.append('.');

    // Format fractional part with leading zeros preserved
    var digits: [dec_decimal_places]u8 = undefined;
    @memset(&digits, '0');
    var remaining = fractional_part;
    var idx: usize = dec_decimal_places;
    while (idx > 0) : (idx -= 1) {
        const digit: u8 = @intCast(@mod(remaining, 10));
        digits[idx - 1] = digit + '0';
        remaining = @divTrunc(remaining, 10);
    }

    // Trim trailing zeros
    var end: usize = dec_decimal_places;
    while (end > 1 and digits[end - 1] == '0') {
        end -= 1;
    }

    try out.appendSlice(digits[0..end]);
    return try out.toOwnedSlice();
}

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works
    try std.testing.expect(evaluator.counter == 0);
}

test "formatDecValue preserves full precision" {
    const allocator = std.testing.allocator;

    // Test: 1.123456789012345678 = 1123456789012345678 (scaled by 10^18)
    {
        const result = try formatDecValue(allocator, 1123456789012345678);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("1.123456789012345678", result);
    }

    // Test: 0.000000000000000001 = 1 (smallest positive Dec value)
    {
        const result = try formatDecValue(allocator, 1);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("0.000000000000000001", result);
    }

    // Test: 0 should render as "0"
    {
        const result = try formatDecValue(allocator, 0);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("0", result);
    }

    // Test: -1.5 = -1500000000000000000 (scaled by 10^18)
    {
        const result = try formatDecValue(allocator, -1500000000000000000);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("-1.5", result);
    }

    // Test: trailing zeros are trimmed (0.125 = 125000000000000000)
    {
        const result = try formatDecValue(allocator, 125000000000000000);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("0.125", result);
    }

    // Test: whole numbers without fractional part (42.0 = 42000000000000000000)
    {
        const result = try formatDecValue(allocator, 42000000000000000000);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("42", result);
    }
}
