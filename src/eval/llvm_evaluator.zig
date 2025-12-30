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

// LLVM Builder from Zig's standard library (for IR generation)
const LlvmBuilder = std.zig.llvm.Builder;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;

/// Get the LLVM target triple for the current platform.
/// This is needed so LLVM knows to use the correct calling conventions
/// (e.g., Windows x64 ABI for i128/f64 returns).
///
/// Reference triples from target/mod.zig:
/// - x86_64-apple-darwin, aarch64-apple-darwin
/// - x86_64-pc-windows-msvc, aarch64-pc-windows-msvc
/// - x86_64-unknown-linux-gnu, aarch64-unknown-linux-gnu
/// - x86_64-unknown-linux-musl, aarch64-unknown-linux-musl
/// - arm-unknown-linux-gnueabihf, arm-unknown-linux-musleabihf
/// - x86_64-unknown-freebsd
/// - wasm32-unknown-unknown
fn getLlvmTriple() []const u8 {
    // Construct LLVM triple from Zig builtin values.
    // Format: <arch>-<vendor>-<os>-<abi>
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
        .windows => "-pc-windows",
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

    // ABI suffix - must match exactly what LLVM expects
    const abi = switch (builtin.os.tag) {
        .windows => switch (builtin.abi) {
            .gnu => "-gnu", // MinGW
            else => "-msvc",
        },
        .linux => switch (builtin.abi) {
            // ARM hard float variants need the "hf" suffix
            .musleabihf => "-musleabihf",
            .gnueabihf => "-gnueabihf",
            // ARM soft float variants
            .musleabi => "-musleabi",
            .gnueabi => "-gnueabi",
            // Standard variants
            .musl => "-musl",
            .gnu => "-gnu",
            // Android
            .android => "-android",
            else => "-gnu",
        },
        .freestanding, .wasi => "",
        else => "", // macOS, iOS, BSDs don't need ABI suffix
    };

    return arch ++ vendor_os ++ abi;
}

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
        // Create LLVM Builder with target triple so LLVM uses correct calling conventions.
        // Without the triple, LLVM may not use Windows x64 ABI for i128/f64 returns.
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_repl_eval",
            .target = &builtin.target,
            .triple = getLlvmTriple(),
        }) catch return error.OutOfMemory;
        defer builder.deinit();

        // Generate LLVM IR for the expression
        const value_type = try self.getExprLlvmType(&builder, expr);
        const value = try self.emitExprValue(&builder, module_env, expr);

        // Determine the result type for JIT execution
        // We need to look at the original expression to get signedness,
        // since LLVM types don't distinguish signed from unsigned
        const result_type: ResultType = self.getExprResultType(expr);

        // Generate a main function that returns the result
        // The return type must match what compile.zig expects based on result_type
        try self.emitMainWithPrint(&builder, value_type, value, result_type);

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

    /// Emit an eval function that returns the computed value.
    /// For JIT execution, this avoids printf complexity and vararg ABI issues.
    ///
    /// The function signature depends on the platform and type:
    /// - For i128/u128/dec on Windows: void roc_eval(i128* out_ptr)
    ///   Windows x64 ABI has complex sret handling for i128 that causes
    ///   mismatches between LLVM's sret and Zig's sret. Using explicit
    ///   pointer output avoids this issue.
    /// - For other types/platforms: <type> roc_eval()
    ///   Direct return works fine for i64/u64/f64 and for i128 on non-Windows.
    fn emitMainWithPrint(_: *LlvmEvaluator, builder: *LlvmBuilder, value_type: LlvmBuilder.Type, value: LlvmBuilder.Constant, result_type: ResultType) !void {
        // On Windows x64, i128 returns have complex sret ABI that causes mismatches.
        // Use explicit pointer output to avoid this.
        const use_out_ptr = builtin.os.tag == .windows and builtin.cpu.arch == .x86_64 and
            (result_type == .i128 or result_type == .u128 or result_type == .dec);

        if (use_out_ptr) {
            // Windows i128: void roc_eval(i128* out_ptr)
            const ptr_type = try builder.ptrType(.default);
            const eval_type = try builder.fnType(.void, &.{ptr_type}, .normal);
            const eval_name = try builder.strtabString("roc_eval");
            const eval_fn = try builder.addFunction(eval_type, eval_name, .default);
            eval_fn.setLinkage(.external, builder);

            // Build function body
            var wip = try LlvmBuilder.WipFunction.init(builder, .{
                .function = eval_fn,
                .strip = false,
            });
            defer wip.deinit();

            const entry_block = try wip.block(1, "entry"); // 1 arg
            wip.cursor = .{ .block = entry_block };

            // Get the pointer argument
            const out_ptr = wip.arg(0);

            // Convert value if needed and store through pointer
            const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (result_type) {
                .i128 => .signed,
                .u128, .dec => .unsigned,
                else => unreachable,
            };
            const store_value = if (value_type == .i128)
                value.toValue()
            else
                try wip.conv(signedness, value.toValue(), .i128, "");

            _ = try wip.store(.default, store_value, out_ptr);
            _ = try wip.retVoid();
            try wip.finish();
        } else {
            // Standard case: direct return
            const return_type: LlvmBuilder.Type = switch (result_type) {
                .i64, .u64 => .i64,
                .i128, .u128, .dec => .i128,
                .f64 => .double,
            };

            // Determine signedness for integer extension
            const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (result_type) {
                .i64, .i128 => .signed,
                .u64, .u128 => .unsigned,
                .f64, .dec => .unneeded, // f64 uses fpext, dec is already i128
            };

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

            // Convert the value to the return type if needed
            const return_value = if (value_type == return_type)
                value.toValue()
            else
                try wip.conv(signedness, value.toValue(), return_type, "");

            _ = try wip.ret(return_value);
            try wip.finish();
        }
    }
};

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works - the evaluator should be usable
    try std.testing.expect(evaluator.allocator.ptr == allocator.ptr);
}
