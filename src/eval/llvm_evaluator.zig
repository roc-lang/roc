//! LLVM-based Evaluator for Roc expressions
//!
//! This module generates LLVM bitcode from Roc expressions. The bitcode can then
//! be JIT compiled and executed by llvm_compile.compileAndExecute().
//!
//! Used when the `--opt=size` or `--opt=speed` flags are passed to the REPL.
//!
//! The evaluator works by:
//! 1. Parsing and type-checking the source expression
//! 2. Translating the CIR to LLVM IR using Zig's LLVM Builder
//! 3. Serializing to LLVM bitcode for JIT compilation

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const check = @import("check");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("mod.zig");
const result_type_mod = @import("result_type");

// LLVM Builder from Zig's standard library (for IR generation)
const LlvmBuilder = std.zig.llvm.Builder;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;

/// LLVM-based evaluator for Roc expressions
pub const LlvmEvaluator = struct {
    allocator: Allocator,

    /// Builtin type declaration indices (loaded once at startup)
    builtin_indices: CIR.BuiltinIndices,

    /// Loaded Builtin module (loaded once at startup)
    builtin_module: builtin_loading.LoadedModule,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        UnsupportedType,
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
            .builtin_indices = builtin_indices,
            .builtin_module = builtin_module,
        };
    }

    /// Clean up the evaluator
    pub fn deinit(self: *LlvmEvaluator) void {
        self.builtin_module.deinit();
    }

    /// Type of the result value for JIT execution.
    /// Shared with llvm_compile module via result_type module.
    pub const ResultType = result_type_mod.ResultType;

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
};

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works - the evaluator should be usable
    try std.testing.expect(evaluator.allocator.ptr == allocator.ptr);
}
