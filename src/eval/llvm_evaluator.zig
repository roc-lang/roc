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
//!
//! ## Supported Expression Types
//!
//! - Numeric literals: integers, floats, Dec
//! - Binary operations: +, -, *, /, %, <, >, <=, >=, ==, !=, and, or
//! - Unary operations: -, !
//! - String literals (small strings only, stored inline)
//! - Boolean values (True, False)
//! - If expressions
//! - Blocks with local bindings

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const check = @import("check");
const layout = @import("layout");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("mod.zig");
const builtins = @import("builtins");

// LLVM Builder from Zig's standard library (for IR generation)
const LlvmBuilder = std.zig.llvm.Builder;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;
const RocStr = builtins.str.RocStr;

/// Get the LLVM target triple for the current platform.
/// This must match the triple that LLVM's GetDefaultTargetTriple() returns,
/// or there will be calling convention mismatches when JIT-compiling.
///
/// Note: Zig on Windows uses GNU ABI (mingw), not MSVC!
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
        // Windows 64-bit uses w64 (mingw-w64) vendor, not pc
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

    // ABI suffix - Zig's LLVM on Windows uses GNU (mingw), not MSVC!
    const abi = switch (builtin.os.tag) {
        .windows => "-gnu", // Zig uses mingw/GNU toolchain on Windows
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
        UnsupportedLayout,
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

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        output_layout: layout.Idx,
        is_list: bool,
        is_record: bool,
        record_field_names: ?[]const u8, // Comma-separated field names for record output
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
            if (self.record_field_names) |names| {
                self.allocator.free(names);
            }
        }
    };

    /// Generate LLVM bitcode for a CIR expression
    /// Returns the bitcode and the output layout for JIT execution
    /// The caller is responsible for freeing the bitcode via result.deinit()
    pub fn generateBitcode(self: *LlvmEvaluator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) Error!BitcodeResult {
        // Create LLVM Builder with target triple so LLVM uses correct calling conventions.
        // The triple must match what LLVM's GetDefaultTargetTriple() returns on the host,
        // otherwise calling convention mismatches will cause segfaults on Windows.
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_repl_eval",
            .target = &builtin.target,
            .triple = getLlvmTriple(),
        }) catch return error.OutOfMemory;
        defer builder.deinit();

        // Check for list and record expressions - they need special handling
        const top_expr = module_env.store.getExpr(expr_idx);
        const is_list_expr = (top_expr == .e_empty_list or top_expr == .e_list);
        const is_record_expr = (top_expr == .e_record or top_expr == .e_empty_record);

        // Record result detection is complex when records are inside lambda bodies
        // For now, only handle direct record expressions
        const is_record_result = false;

        // Determine the output layout for JIT execution
        // We need to look at the original expression to get signedness,
        // since LLVM types don't distinguish signed from unsigned
        const output_layout: layout.Idx = if (is_list_expr or is_record_expr or is_record_result) .i64 else self.getExprOutputLayout(module_env, expr_idx);

        // Determine the value type based on output layout
        const value_type: LlvmBuilder.Type = switch (output_layout) {
            .bool, .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            .str => .i64, // Placeholder - str layout needs special handling
            else => return error.UnsupportedLayout,
        };

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

        const entry_block = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry_block };

        // Create expression generation context
        var ctx = ExprContext{
            .evaluator = self,
            .builder = &builder,
            .wip = &wip,
            .module_env = module_env,
            .locals = std.AutoHashMap(CIR.Pattern.Idx, LlvmBuilder.Value).init(self.allocator),
        };
        defer ctx.locals.deinit();

        // Get the output pointer
        const out_ptr = wip.arg(0);

        // Special handling for list output - write RocList struct directly
        var record_field_names: ?[]const u8 = null;
        if (is_list_expr) {
            try ctx.emitListToPtr(top_expr, out_ptr);
        } else if (is_record_expr or is_record_result) {
            // Handle record output - emit field values to output buffer
            const record_expr = if (is_record_result) @as(CIR.Expr, undefined) else top_expr;
            _ = record_expr;
            record_field_names = try ctx.emitRecordToPtr(top_expr, out_ptr, self.allocator);
        } else if (output_layout == .str) {
            // Special handling for string output - write RocStr struct directly
            const expr = module_env.store.getExpr(expr_idx);
            try ctx.emitStringToPtr(expr, out_ptr);
        } else {
            // Generate LLVM IR for the expression
            const value = ctx.emitExpr(expr_idx) catch |err| {
                // Map internal errors to public error type
                return switch (err) {
                    error.OutOfMemory => error.OutOfMemory,
                    error.UnsupportedType => error.UnsupportedType,
                    error.UnsupportedLayout => error.UnsupportedLayout,
                    error.CompilationFailed => error.CompilationFailed,
                };
            };

            // Determine the final type to store based on output layout
            const final_type: LlvmBuilder.Type = switch (output_layout) {
                .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
                .i128, .u128, .dec => .i128,
                .f32, .f64 => .double,
                else => return error.UnsupportedLayout,
            };

            // Determine signedness for integer extension
            const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (output_layout) {
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
                .double => 8,
                else => 0,
            });
            _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
        }
        _ = wip.retVoid() catch return error.CompilationFailed;
        wip.finish() catch return error.CompilationFailed;

        // Serialize to bitcode
        const producer = LlvmBuilder.Producer{
            .name = "Roc LLVM Evaluator",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

        const bitcode = builder.toBitcode(self.allocator, producer) catch return error.CompilationFailed;

        return BitcodeResult{
            .bitcode = bitcode,
            .output_layout = output_layout,
            .is_list = is_list_expr,
            .is_record = is_record_expr or is_record_result,
            .record_field_names = record_field_names,
            .allocator = self.allocator,
        };
    }

    /// Context for expression code generation
    const ExprContext = struct {
        evaluator: *LlvmEvaluator,
        builder: *LlvmBuilder,
        wip: *LlvmBuilder.WipFunction,
        module_env: *ModuleEnv,
        locals: std.AutoHashMap(CIR.Pattern.Idx, LlvmBuilder.Value),

        const ExprError = error{
            OutOfMemory,
            UnsupportedType,
            UnsupportedLayout,
            CompilationFailed,
        };

        /// Emit LLVM IR for an expression, returning a runtime value
        fn emitExpr(ctx: *ExprContext, expr_idx: CIR.Expr.Idx) ExprError!LlvmBuilder.Value {
            const expr = ctx.module_env.store.getExpr(expr_idx);
            return switch (expr) {
                .e_num => |num| ctx.emitNum(num, expr),
                .e_frac_f32 => |frac| (ctx.builder.floatConst(frac.value) catch return error.CompilationFailed).toValue(),
                .e_frac_f64 => |frac| (ctx.builder.doubleConst(frac.value) catch return error.CompilationFailed).toValue(),
                .e_dec => |dec| (ctx.builder.intConst(.i128, dec.value.num) catch return error.CompilationFailed).toValue(),
                .e_dec_small => |dec| blk: {
                    const decimal_places: u5 = 18;
                    const scale_factor = std.math.pow(i128, 10, decimal_places - dec.value.denominator_power_of_ten);
                    const scaled_value = @as(i128, dec.value.numerator) * scale_factor;
                    break :blk (ctx.builder.intConst(.i128, scaled_value) catch return error.CompilationFailed).toValue();
                },
                .e_typed_int => |typed_int| ctx.emitTypedInt(typed_int),
                .e_typed_frac => |typed_frac| ctx.emitTypedFrac(typed_frac),
                .e_binop => |binop| ctx.emitBinop(binop),
                .e_unary_minus => |unary| ctx.emitUnaryMinus(unary),
                .e_unary_not => |unary| ctx.emitUnaryNot(unary),
                .e_if => |if_expr| ctx.emitIf(if_expr, expr_idx),
                .e_zero_argument_tag => |tag| ctx.emitZeroArgTag(tag),
                .e_tag => |tag| ctx.emitTag(tag),
                .e_lookup_local => |lookup| ctx.emitLookupLocal(lookup),
                .e_block => |blk| ctx.emitBlock(blk),
                .e_call => |call| ctx.emitCall(call),
                .e_str_segment => |str_seg| ctx.emitStrSegment(str_seg),
                .e_str => |str_expr| ctx.emitStr(str_expr),
                .e_runtime_error => {
                    // Runtime errors can't be compiled - they represent errors
                    // that the interpreter would produce
                    return error.UnsupportedType;
                },
                .e_list => |list| ctx.emitList(list),
                .e_empty_list => ctx.emitEmptyList(),
                .e_empty_record => ctx.emitEmptyRecord(),
                .e_record => |record| ctx.emitRecord(record),
                .e_dot_access => |dot| ctx.emitDotAccess(dot),
                .e_lambda => {
                    // Lambda expressions require closure handling
                    return error.UnsupportedType;
                },
                .e_closure => {
                    // Closures require captured variable handling
                    return error.UnsupportedType;
                },
                .e_for => {
                    // For loops require list iteration
                    return error.UnsupportedType;
                },
                .e_match => {
                    // Pattern matching requires complex control flow
                    return error.UnsupportedType;
                },
                .e_nominal_external => |nominal| {
                    // Emit the backing expression (e.g., for Bool.True, emit the True tag)
                    return ctx.emitExpr(nominal.backing_expr);
                },
                .e_nominal => {
                    // Local nominal types require special handling
                    return error.UnsupportedType;
                },
                .e_lookup_external => {
                    // External lookups are for function references - can't evaluate standalone
                    return error.UnsupportedType;
                },
                .e_low_level_lambda => {
                    // Low-level lambdas are function definitions, not values
                    return error.UnsupportedType;
                },
                else => {
                    std.debug.print("LLVM: Unsupported expression type: {s}\n", .{@tagName(expr)});
                    return error.UnsupportedType;
                },
            };
        }

        /// Emit a numeric literal
        fn emitNum(ctx: *ExprContext, num: anytype, expr: CIR.Expr) ExprError!LlvmBuilder.Value {
            const int_value = num.value.toI128();
            return switch (num.kind) {
                .f32 => (ctx.builder.floatConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue(),
                .f64 => (ctx.builder.doubleConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue(),
                else => blk: {
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, expr) catch return error.CompilationFailed;
                    break :blk (ctx.builder.intConst(llvm_type, int_value) catch return error.CompilationFailed).toValue();
                },
            };
        }

        /// Emit a typed integer literal (e.g., 42.U32, 100.I64)
        fn emitTypedInt(ctx: *ExprContext, typed_int: anytype) ExprError!LlvmBuilder.Value {
            const int_value = typed_int.value.toI128();
            const type_name = ctx.module_env.getIdentText(typed_int.type_name);

            // Handle Dec type - scale integer to Dec representation
            if (std.mem.eql(u8, type_name, "Dec")) {
                const dec_value = int_value * 1000000000000000000; // * 10^18
                return (ctx.builder.intConst(.i128, dec_value) catch return error.CompilationFailed).toValue();
            }

            // Handle float types
            if (std.mem.eql(u8, type_name, "F32")) {
                return (ctx.builder.floatConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue();
            }
            if (std.mem.eql(u8, type_name, "F64")) {
                return (ctx.builder.doubleConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue();
            }

            // Determine LLVM type from the type name
            const llvm_type: LlvmBuilder.Type = if (std.mem.eql(u8, type_name, "I8") or std.mem.eql(u8, type_name, "U8"))
                .i8
            else if (std.mem.eql(u8, type_name, "I16") or std.mem.eql(u8, type_name, "U16"))
                .i16
            else if (std.mem.eql(u8, type_name, "I32") or std.mem.eql(u8, type_name, "U32"))
                .i32
            else if (std.mem.eql(u8, type_name, "I64") or std.mem.eql(u8, type_name, "U64"))
                .i64
            else if (std.mem.eql(u8, type_name, "I128") or std.mem.eql(u8, type_name, "U128"))
                .i128
            else
                return error.UnsupportedType;

            return (ctx.builder.intConst(llvm_type, int_value) catch return error.CompilationFailed).toValue();
        }

        /// Emit a typed fractional literal (e.g., 3.14.Dec, 2.0.F64)
        fn emitTypedFrac(ctx: *ExprContext, typed_frac: anytype) ExprError!LlvmBuilder.Value {
            const type_name = ctx.module_env.getIdentText(typed_frac.type_name);
            const int_value = typed_frac.value.toI128();

            if (std.mem.eql(u8, type_name, "Dec")) {
                // Dec values are stored as scaled i128 (like e_dec)
                return (ctx.builder.intConst(.i128, int_value) catch return error.CompilationFailed).toValue();
            } else if (std.mem.eql(u8, type_name, "F32")) {
                // The value is stored as Dec-scaled (int * 10^18), need to convert to float
                const float_val: f32 = @as(f32, @floatFromInt(int_value)) / 1e18;
                return (ctx.builder.floatConst(float_val) catch return error.CompilationFailed).toValue();
            } else if (std.mem.eql(u8, type_name, "F64")) {
                // The value is stored as Dec-scaled (int * 10^18), need to convert to float
                const float_val: f64 = @as(f64, @floatFromInt(int_value)) / 1e18;
                return (ctx.builder.doubleConst(float_val) catch return error.CompilationFailed).toValue();
            } else {
                return error.UnsupportedType;
            }
        }

        /// Emit a binary operation
        fn emitBinop(ctx: *ExprContext, binop: CIR.Expr.Binop) ExprError!LlvmBuilder.Value {
            // Handle short-circuit operators specially
            switch (binop.op) {
                .@"and" => return ctx.emitShortCircuitAnd(binop),
                .@"or" => return ctx.emitShortCircuitOr(binop),
                else => {},
            }

            // Check for string comparison - needs special handling
            const lhs_expr = ctx.module_env.store.getExpr(binop.lhs);
            const rhs_expr = ctx.module_env.store.getExpr(binop.rhs);
            if (ctx.isStringExpr(lhs_expr) and ctx.isStringExpr(rhs_expr)) {
                return ctx.emitStringComparison(binop, lhs_expr, rhs_expr);
            }

            // Evaluate both sides
            const lhs = try ctx.emitExpr(binop.lhs);
            const rhs = try ctx.emitExpr(binop.rhs);

            // Determine if this is an integer or float operation based on LHS expression
            const is_float = ctx.isFloatExpr(lhs_expr);
            const is_dec = ctx.isDecExpr(lhs_expr);
            const is_signed = ctx.isSignedExpr(lhs_expr);

            // Check if types match - if not, we can't do the operation
            // This can happen with polymorphic expressions like lookups
            const lhs_type = lhs.typeOfWip(ctx.wip);
            const rhs_type = rhs.typeOfWip(ctx.wip);
            if (lhs_type != rhs_type) {
                // Types don't match - we'd need runtime type info to handle this
                return error.UnsupportedType;
            }

            return switch (binop.op) {
                .add => if (is_float)
                    ctx.wip.bin(.fadd, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed,
                .sub => if (is_float)
                    ctx.wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed,
                .mul => if (is_float)
                    ctx.wip.bin(.fmul, lhs, rhs, "") catch return error.CompilationFailed
                else if (is_dec)
                    // Dec multiplication needs to divide by scale factor after multiply
                    // For now, just do regular multiply (will be wrong for Dec)
                    ctx.wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed,
                .div => if (is_float)
                    ctx.wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
                else if (is_signed)
                    ctx.wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,
                .div_trunc => if (is_signed)
                    ctx.wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,
                .rem => if (is_signed)
                    ctx.wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed,
                .lt => if (is_float)
                    ctx.wip.fcmp(.normal, .olt, lhs, rhs, "") catch return error.CompilationFailed
                else if (is_signed)
                    ctx.wip.icmp(.slt, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.icmp(.ult, lhs, rhs, "") catch return error.CompilationFailed,
                .le => if (is_float)
                    ctx.wip.fcmp(.normal, .ole, lhs, rhs, "") catch return error.CompilationFailed
                else if (is_signed)
                    ctx.wip.icmp(.sle, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.icmp(.ule, lhs, rhs, "") catch return error.CompilationFailed,
                .gt => if (is_float)
                    ctx.wip.fcmp(.normal, .ogt, lhs, rhs, "") catch return error.CompilationFailed
                else if (is_signed)
                    ctx.wip.icmp(.sgt, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.icmp(.ugt, lhs, rhs, "") catch return error.CompilationFailed,
                .ge => if (is_float)
                    ctx.wip.fcmp(.normal, .oge, lhs, rhs, "") catch return error.CompilationFailed
                else if (is_signed)
                    ctx.wip.icmp(.sge, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.icmp(.uge, lhs, rhs, "") catch return error.CompilationFailed,
                .eq => if (is_float)
                    ctx.wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.icmp(.eq, lhs, rhs, "") catch return error.CompilationFailed,
                .ne => if (is_float)
                    ctx.wip.fcmp(.normal, .one, lhs, rhs, "") catch return error.CompilationFailed
                else
                    ctx.wip.icmp(.ne, lhs, rhs, "") catch return error.CompilationFailed,
                .@"and", .@"or" => unreachable, // Handled above
            };
        }

        /// Emit short-circuit AND operation using select
        /// For simple boolean and/or, we use select which is simpler than control flow
        fn emitShortCircuitAnd(ctx: *ExprContext, binop: CIR.Expr.Binop) ExprError!LlvmBuilder.Value {
            // Evaluate both sides (select evaluates both, but that's ok for simple expressions)
            const lhs = try ctx.emitExpr(binop.lhs);
            const rhs = try ctx.emitExpr(binop.rhs);

            // AND: if lhs is true, result is rhs; otherwise result is false
            // Convert i8 to i1 for condition by comparing with zero
            const zero = (ctx.builder.intConst(.i8, 0) catch return error.CompilationFailed).toValue();
            const cond = ctx.wip.icmp(.ne, lhs, zero, "") catch return error.CompilationFailed;
            const false_const = (ctx.builder.intConst(.i8, 0) catch return error.CompilationFailed).toValue();
            return ctx.wip.select(.normal, cond, rhs, false_const, "") catch return error.CompilationFailed;
        }

        /// Emit short-circuit OR operation using select
        fn emitShortCircuitOr(ctx: *ExprContext, binop: CIR.Expr.Binop) ExprError!LlvmBuilder.Value {
            // Evaluate both sides
            const lhs = try ctx.emitExpr(binop.lhs);
            const rhs = try ctx.emitExpr(binop.rhs);

            // OR: if lhs is true, result is true; otherwise result is rhs
            // Convert i8 to i1 for condition by comparing with zero
            const zero = (ctx.builder.intConst(.i8, 0) catch return error.CompilationFailed).toValue();
            const cond = ctx.wip.icmp(.ne, lhs, zero, "") catch return error.CompilationFailed;
            const true_const = (ctx.builder.intConst(.i8, 1) catch return error.CompilationFailed).toValue();
            return ctx.wip.select(.normal, cond, true_const, rhs, "") catch return error.CompilationFailed;
        }

        /// Emit unary minus operation
        fn emitUnaryMinus(ctx: *ExprContext, unary: CIR.Expr.UnaryMinus) ExprError!LlvmBuilder.Value {
            const operand = try ctx.emitExpr(unary.expr);
            const operand_expr = ctx.module_env.store.getExpr(unary.expr);
            const is_float = ctx.isFloatExpr(operand_expr);

            if (is_float) {
                return ctx.wip.un(.fneg, operand, "") catch return error.CompilationFailed;
            } else {
                // For integers, subtract from zero
                const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, operand_expr) catch return error.CompilationFailed;
                const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                return ctx.wip.bin(.sub, zero, operand, "") catch return error.CompilationFailed;
            }
        }

        /// Emit unary not operation
        fn emitUnaryNot(ctx: *ExprContext, unary: CIR.Expr.UnaryNot) ExprError!LlvmBuilder.Value {
            const operand = try ctx.emitExpr(unary.expr);
            // XOR with 1 to flip the boolean (use i8 since Bool is stored as i8)
            const one = (ctx.builder.intConst(.i8, 1) catch return error.CompilationFailed).toValue();
            return ctx.wip.bin(.xor, operand, one, "") catch return error.CompilationFailed;
        }

        /// Emit if expression using select
        /// For simple expressions, select is sufficient and avoids complex control flow
        fn emitIf(ctx: *ExprContext, if_expr: anytype, _: CIR.Expr.Idx) ExprError!LlvmBuilder.Value {
            const branches = ctx.module_env.store.sliceIfBranches(if_expr.branches);
            if (branches.len == 0) {
                // No branches, just evaluate final else
                return ctx.emitExpr(if_expr.final_else);
            }

            // For now, handle only single-branch if-else
            if (branches.len > 1) {
                return error.UnsupportedType; // Multi-branch if not yet supported
            }

            const branch = ctx.module_env.store.getIfBranch(branches[0]);

            // Evaluate condition
            const cond = try ctx.emitExpr(branch.cond);

            // Evaluate both branches (select evaluates both)
            const then_val = try ctx.emitExpr(branch.body);
            const else_val = try ctx.emitExpr(if_expr.final_else);

            // Use select to choose between values
            return ctx.wip.select(.normal, cond, then_val, else_val, "") catch return error.CompilationFailed;
        }

        /// Emit a zero-argument tag (like True, False)
        fn emitZeroArgTag(ctx: *ExprContext, tag: anytype) ExprError!LlvmBuilder.Value {
            // Look up the tag name to determine the value
            const tag_name = ctx.module_env.getIdentText(tag.name);

            // For Bool tags, True is 1 and False is 0
            const value: i128 = if (std.mem.eql(u8, tag_name, "True")) 1 else 0;
            return (ctx.builder.intConst(.i8, value) catch return error.CompilationFailed).toValue();
        }

        /// Emit a tag expression (e.g., True, False, Ok(value), Err(msg))
        fn emitTag(ctx: *ExprContext, tag: anytype) ExprError!LlvmBuilder.Value {
            // Check if this is a zero-argument tag (like True, False)
            const args = ctx.module_env.store.sliceExpr(tag.args);
            if (args.len == 0) {
                // Zero-argument tag - handle like e_zero_argument_tag
                const tag_name = ctx.module_env.getIdentText(tag.name);

                // For Bool tags, True is 1 and False is 0
                const value: i128 = if (std.mem.eql(u8, tag_name, "True")) 1 else 0;
                return (ctx.builder.intConst(.i8, value) catch return error.CompilationFailed).toValue();
            }

            // Tags with arguments require more complex handling
            return error.UnsupportedType;
        }

        /// Emit a local variable lookup
        fn emitLookupLocal(ctx: *ExprContext, lookup: anytype) ExprError!LlvmBuilder.Value {
            return ctx.locals.get(lookup.pattern_idx) orelse error.UnsupportedType;
        }

        /// Emit a block expression
        fn emitBlock(ctx: *ExprContext, blk: anytype) ExprError!LlvmBuilder.Value {
            // Process statements
            const stmts = ctx.module_env.store.sliceStatements(blk.stmts);
            for (stmts) |stmt_idx| {
                const stmt = ctx.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        // Evaluate the expression
                        const val = try ctx.emitExpr(decl.expr);
                        // Bind to pattern (for now, only support simple identifier patterns)
                        try ctx.locals.put(decl.pattern, val);
                    },
                    .s_var => |var_stmt| {
                        const val = try ctx.emitExpr(var_stmt.expr);
                        try ctx.locals.put(var_stmt.pattern_idx, val);
                    },
                    else => {}, // Ignore other statement types for now
                }
            }

            // Evaluate and return the final expression
            return ctx.emitExpr(blk.final_expr);
        }

        /// Emit a function call
        /// Currently supports specific builtin functions as inline implementations
        fn emitCall(ctx: *ExprContext, call: anytype) ExprError!LlvmBuilder.Value {
            // Check if this is a call to a known builtin function
            const func_expr = ctx.module_env.store.getExpr(call.func);

            // Handle e_lookup_external for builtin functions
            if (func_expr == .e_lookup_external) {
                const lookup = func_expr.e_lookup_external;
                const func_name = ctx.module_env.getIdentText(lookup.ident_idx);
                const args = ctx.module_env.store.sliceExpr(call.args);

                // Handle Bool.not specially - just emit a not operation
                if (std.mem.eql(u8, func_name, "not") and args.len == 1) {
                    const arg_val = try ctx.emitExpr(args[0]);
                    // XOR with 1 to flip the boolean (use i8 since Bool is stored as i8)
                    const one = (ctx.builder.intConst(.i8, 1) catch return error.CompilationFailed).toValue();
                    return ctx.wip.bin(.xor, arg_val, one, "") catch return error.CompilationFailed;
                }

                // Handle Num.abs - absolute value
                if (std.mem.eql(u8, func_name, "abs") and args.len == 1) {
                    const arg_val = try ctx.emitExpr(args[0]);
                    const arg_expr = ctx.module_env.store.getExpr(args[0]);

                    // Get the type info to determine signedness and type
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, arg_expr) catch return error.CompilationFailed;

                    // For floats, use fneg and select
                    if (ctx.isFloatExpr(arg_expr)) {
                        const neg_val = ctx.wip.un(.fneg, arg_val, "") catch return error.CompilationFailed;
                        const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                        const is_neg = ctx.wip.fcmp(.normal, .olt, arg_val, zero, "") catch return error.CompilationFailed;
                        return ctx.wip.select(.normal, is_neg, neg_val, arg_val, "") catch return error.CompilationFailed;
                    }

                    // For integers, use sub and select
                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                    const neg_val = ctx.wip.bin(.sub, zero, arg_val, "") catch return error.CompilationFailed;
                    const is_neg = ctx.wip.icmp(.slt, arg_val, zero, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, is_neg, neg_val, arg_val, "") catch return error.CompilationFailed;
                }

                // Handle Num.neg - negation (same as unary minus)
                if (std.mem.eql(u8, func_name, "neg") and args.len == 1) {
                    const arg_val = try ctx.emitExpr(args[0]);
                    const arg_expr = ctx.module_env.store.getExpr(args[0]);

                    if (ctx.isFloatExpr(arg_expr)) {
                        return ctx.wip.un(.fneg, arg_val, "") catch return error.CompilationFailed;
                    }

                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, arg_expr) catch return error.CompilationFailed;
                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                    return ctx.wip.bin(.sub, zero, arg_val, "") catch return error.CompilationFailed;
                }

                // Handle Num.mod_by - modulo operation (Roc semantics)
                // Roc's mod_by returns a result with the same sign as the divisor
                // This differs from C's % which uses truncated division
                if (std.mem.eql(u8, func_name, "mod_by") and args.len == 2) {
                    const lhs_val = try ctx.emitExpr(args[0]);
                    const rhs_val = try ctx.emitExpr(args[1]);
                    const lhs_expr = ctx.module_env.store.getExpr(args[0]);
                    const is_signed = ctx.isSignedExpr(lhs_expr);

                    if (!is_signed) {
                        // Unsigned: simple remainder
                        return ctx.wip.bin(.urem, lhs_val, rhs_val, "") catch return error.CompilationFailed;
                    }

                    // Signed: implement Roc semantics (floored division modulo)
                    // result = lhs - (floor(lhs/rhs) * rhs)
                    // If remainder and divisor have different signs, adjust by adding divisor
                    const rem = ctx.wip.bin(.srem, lhs_val, rhs_val, "") catch return error.CompilationFailed;
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, lhs_expr) catch return error.CompilationFailed;
                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();

                    // Check if remainder != 0 and (remainder ^ divisor) < 0 (different signs)
                    const rem_ne_zero = ctx.wip.icmp(.ne, rem, zero, "") catch return error.CompilationFailed;
                    const xor_result = ctx.wip.bin(.xor, rem, rhs_val, "") catch return error.CompilationFailed;
                    const different_signs = ctx.wip.icmp(.slt, xor_result, zero, "") catch return error.CompilationFailed;
                    const need_adjust = ctx.wip.bin(.@"and", rem_ne_zero, different_signs, "") catch return error.CompilationFailed;

                    // If need_adjust, add divisor to remainder
                    const adjusted = ctx.wip.bin(.add, rem, rhs_val, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, need_adjust, adjusted, rem, "") catch return error.CompilationFailed;
                }

                // Handle Num.abs_diff - absolute difference |a - b|
                if (std.mem.eql(u8, func_name, "abs_diff") and args.len == 2) {
                    const lhs_val = try ctx.emitExpr(args[0]);
                    const rhs_val = try ctx.emitExpr(args[1]);
                    const lhs_expr = ctx.module_env.store.getExpr(args[0]);

                    // Compute a - b
                    const diff = if (ctx.isFloatExpr(lhs_expr))
                        ctx.wip.bin(.fsub, lhs_val, rhs_val, "") catch return error.CompilationFailed
                    else
                        ctx.wip.bin(.sub, lhs_val, rhs_val, "") catch return error.CompilationFailed;

                    // Then take abs of the result
                    if (ctx.isFloatExpr(lhs_expr)) {
                        const neg_diff = ctx.wip.un(.fneg, diff, "") catch return error.CompilationFailed;
                        const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                        const is_neg = ctx.wip.fcmp(.normal, .olt, diff, zero, "") catch return error.CompilationFailed;
                        return ctx.wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                    }

                    // For integers: abs(diff)
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, lhs_expr) catch return error.CompilationFailed;
                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                    const neg_diff = ctx.wip.bin(.sub, zero, diff, "") catch return error.CompilationFailed;
                    const is_signed = ctx.isSignedExpr(lhs_expr);
                    const is_neg = if (is_signed)
                        ctx.wip.icmp(.slt, diff, zero, "") catch return error.CompilationFailed
                    else
                        // For unsigned, diff is negative if lhs < rhs (would wrap)
                        ctx.wip.icmp(.ult, lhs_val, rhs_val, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                }
            }

            // Handle method calls on values (e.g., (3.5).abs_diff(1.2))
            if (func_expr == .e_dot_access) {
                const dot = func_expr.e_dot_access;
                const method_name = ctx.module_env.getIdentText(dot.field_name);
                const args = ctx.module_env.store.sliceExpr(call.args);

                if (std.mem.eql(u8, method_name, "abs_diff") and args.len == 1) {
                    // abs_diff(other) computes |self - other|
                    const lhs_expr = ctx.module_env.store.getExpr(dot.receiver);
                    const rhs_expr = ctx.module_env.store.getExpr(args[0]);

                    // For Dec literals, we can compute at compile time
                    if (ctx.isDecExpr(lhs_expr) and ctx.isDecExpr(rhs_expr)) {
                        // Get Dec values and compute difference
                        const lhs_dec = ctx.getDecValue(lhs_expr) orelse return error.UnsupportedType;
                        const rhs_dec = ctx.getDecValue(rhs_expr) orelse return error.UnsupportedType;
                        const diff = if (lhs_dec >= rhs_dec) lhs_dec - rhs_dec else rhs_dec - lhs_dec;
                        return (ctx.builder.intConst(.i128, @as(i128, @intCast(diff))) catch return error.CompilationFailed).toValue();
                    }

                    const lhs_val = try ctx.emitExpr(dot.receiver);
                    const rhs_val = try ctx.emitExpr(args[0]);

                    // Compute a - b
                    const diff = if (ctx.isFloatExpr(lhs_expr))
                        ctx.wip.bin(.fsub, lhs_val, rhs_val, "") catch return error.CompilationFailed
                    else
                        ctx.wip.bin(.sub, lhs_val, rhs_val, "") catch return error.CompilationFailed;

                    // Then take abs of the result
                    if (ctx.isFloatExpr(lhs_expr)) {
                        const neg_diff = ctx.wip.un(.fneg, diff, "") catch return error.CompilationFailed;
                        const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                        const is_neg = ctx.wip.fcmp(.normal, .olt, diff, zero, "") catch return error.CompilationFailed;
                        return ctx.wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                    }

                    // For integers: abs(diff)
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, lhs_expr) catch return error.CompilationFailed;
                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                    const neg_diff = ctx.wip.bin(.sub, zero, diff, "") catch return error.CompilationFailed;
                    const is_signed = ctx.isSignedExpr(lhs_expr);
                    const is_neg = if (is_signed)
                        ctx.wip.icmp(.slt, diff, zero, "") catch return error.CompilationFailed
                    else
                        ctx.wip.icmp(.ult, lhs_val, rhs_val, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                }
            }

            // Handle low-level lambda calls (builtins like list_len)
            if (func_expr == .e_low_level_lambda) {
                const low_level = func_expr.e_low_level_lambda;
                const args = ctx.module_env.store.sliceExpr(call.args);

                switch (low_level.op) {
                    .list_len => {
                        // For list literals, we can compute length at compile time
                        if (args.len == 1) {
                            const arg_expr = ctx.module_env.store.getExpr(args[0]);
                            if (arg_expr == .e_empty_list) {
                                return (ctx.builder.intConst(.i64, 0) catch return error.CompilationFailed).toValue();
                            }
                            if (arg_expr == .e_list) {
                                const list = arg_expr.e_list;
                                const elems = ctx.module_env.store.sliceExpr(list.elems);
                                return (ctx.builder.intConst(.i64, @as(i128, @intCast(elems.len))) catch return error.CompilationFailed).toValue();
                            }
                        }
                        return error.UnsupportedType;
                    },
                    .list_is_empty => {
                        // For list literals, we can compute is_empty at compile time
                        if (args.len == 1) {
                            const arg_expr = ctx.module_env.store.getExpr(args[0]);
                            if (arg_expr == .e_empty_list) {
                                // Empty list - return True (1)
                                return (ctx.builder.intConst(.i8, 1) catch return error.CompilationFailed).toValue();
                            }
                            if (arg_expr == .e_list) {
                                const list = arg_expr.e_list;
                                const elems = ctx.module_env.store.sliceExpr(list.elems);
                                const is_empty: i128 = if (elems.len == 0) 1 else 0;
                                return (ctx.builder.intConst(.i8, is_empty) catch return error.CompilationFailed).toValue();
                            }
                        }
                        return error.UnsupportedType;
                    },
                    else => return error.UnsupportedType,
                }
            }

            // Handle lambda calls: (|x| body)(arg)
            // Also handles curried functions: (((|a| |b| |c| body)(x))(y))(z)
            return ctx.emitLambdaCall(call.func, call.args);
        }

        /// Handle lambda calls, including curried functions.
        /// For curried functions like (((|a| |b| |c| body)(x))(y))(z):
        /// 1. Collect all arguments [x, y, z] from the call chain
        /// 2. Find the base lambda and all nested lambdas
        /// 3. Bind each parameter to its argument
        /// 4. Evaluate the innermost body with all bindings in scope
        fn emitLambdaCall(ctx: *ExprContext, func_idx: CIR.Expr.Idx, outer_args: CIR.Expr.Span) ExprError!LlvmBuilder.Value {
            // Collect all arguments from the call chain (outermost first)
            var all_args = std.ArrayListUnmanaged(CIR.Expr.Idx){};
            defer all_args.deinit(ctx.evaluator.allocator);

            // Add outer args first
            for (ctx.module_env.store.sliceExpr(outer_args)) |arg| {
                all_args.append(ctx.evaluator.allocator, arg) catch return error.OutOfMemory;
            }

            // Walk through nested calls and collect their arguments
            var current_func = func_idx;
            while (true) {
                const expr = ctx.module_env.store.getExpr(current_func);
                if (expr == .e_call) {
                    const inner_call = expr.e_call;
                    // Insert inner args at the beginning (they come first in application order)
                    const inner_args = ctx.module_env.store.sliceExpr(inner_call.args);
                    for (0..inner_args.len) |i| {
                        all_args.insert(ctx.evaluator.allocator, i, inner_args[i]) catch return error.OutOfMemory;
                    }
                    current_func = inner_call.func;
                } else {
                    break;
                }
            }

            // Now current_func should be the base lambda
            const base_expr = ctx.module_env.store.getExpr(current_func);
            if (base_expr != .e_lambda) {
                return error.UnsupportedType;
            }

            // Collect all nested lambdas and their parameters
            var all_params = std.ArrayListUnmanaged(CIR.Pattern.Idx){};
            defer all_params.deinit(ctx.evaluator.allocator);

            var current_lambda = base_expr.e_lambda;
            var innermost_body: CIR.Expr.Idx = undefined;

            while (true) {
                // Collect params from this lambda
                const params = ctx.module_env.store.slicePatterns(current_lambda.args);
                for (params) |param| {
                    all_params.append(ctx.evaluator.allocator, param) catch return error.OutOfMemory;
                }

                // Check if body is another lambda or closure
                const body_expr = ctx.module_env.store.getExpr(current_lambda.body);
                if (body_expr == .e_lambda) {
                    current_lambda = body_expr.e_lambda;
                } else if (body_expr == .e_closure) {
                    // Closures wrap lambdas - get the inner lambda
                    const closure = body_expr.e_closure;
                    const inner_lambda_expr = ctx.module_env.store.getExpr(closure.lambda_idx);
                    if (inner_lambda_expr != .e_lambda) {
                        return error.UnsupportedType;
                    }
                    current_lambda = inner_lambda_expr.e_lambda;
                } else {
                    innermost_body = current_lambda.body;
                    break;
                }
            }

            // Check that we have the right number of arguments
            if (all_args.items.len != all_params.items.len) {
                return error.UnsupportedType;
            }

            // Bind each parameter to its argument
            for (all_params.items, all_args.items) |param_idx, arg_idx| {
                const pattern = ctx.module_env.store.getPattern(param_idx);
                if (pattern != .assign) {
                    return error.UnsupportedType;
                }

                const arg_val = try ctx.emitExpr(arg_idx);
                ctx.locals.put(param_idx, arg_val) catch return error.CompilationFailed;
            }

            // Evaluate the innermost body with all bindings in scope
            return ctx.emitExpr(innermost_body);
        }

        /// Emit a string segment (single string literal)
        /// Note: For top-level string expressions, use emitStringToPtr instead
        fn emitStrSegment(_: *ExprContext, _: anytype) ExprError!LlvmBuilder.Value {
            // String literals in non-top-level positions require special handling
            return error.UnsupportedType;
        }

        /// Emit a string expression (potentially with interpolations)
        fn emitStr(_: *ExprContext, _: anytype) ExprError!LlvmBuilder.Value {
            // String expressions with interpolations are complex
            return error.UnsupportedType;
        }

        /// Emit a string expression directly to an output pointer as RocStr
        /// Used for top-level string expressions
        fn emitStringToPtr(ctx: *ExprContext, expr: CIR.Expr, out_ptr: LlvmBuilder.Value) !void {
            // Get the string content
            const str_content: []const u8 = switch (expr) {
                .e_str_segment => |seg| ctx.module_env.getString(seg.literal),
                .e_str => |str_expr| blk: {
                    // For now, only handle single-segment strings
                    const segments = ctx.module_env.store.sliceExpr(str_expr.span);
                    if (segments.len != 1) return error.UnsupportedType;
                    const seg_expr = ctx.module_env.store.getExpr(segments[0]);
                    if (seg_expr != .e_str_segment) return error.UnsupportedType;
                    break :blk ctx.module_env.getString(seg_expr.e_str_segment.literal);
                },
                .e_dot_access => |dot| blk: {
                    // Handle record field access that returns a string
                    const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);
                    if (receiver_expr != .e_record) return error.UnsupportedType;
                    const record = receiver_expr.e_record;
                    const field_name = ctx.module_env.getIdentText(dot.field_name);
                    const field_indices = ctx.module_env.store.sliceRecordFields(record.fields);
                    for (field_indices) |field_idx| {
                        const field = ctx.module_env.store.getRecordField(field_idx);
                        const this_field_name = ctx.module_env.getIdentText(field.name);
                        if (std.mem.eql(u8, this_field_name, field_name)) {
                            // Found the field - get its string content
                            const field_expr = ctx.module_env.store.getExpr(field.value);
                            break :blk ctx.getStringContent(field_expr) orelse return error.UnsupportedType;
                        }
                    }
                    return error.UnsupportedType;
                },
                .e_call => |call| blk: {
                    // Handle lambda calls that return strings
                    // For identity lambdas (|s| s)("str"), get the string from the argument
                    const func_expr = ctx.module_env.store.getExpr(call.func);
                    if (func_expr != .e_lambda) return error.UnsupportedType;
                    const lambda = func_expr.e_lambda;
                    const body_expr = ctx.module_env.store.getExpr(lambda.body);
                    if (body_expr != .e_lookup_local) return error.UnsupportedType;
                    const lookup = body_expr.e_lookup_local;
                    // Find which parameter this lookup refers to
                    const params = ctx.module_env.store.slicePatterns(lambda.args);
                    const args = ctx.module_env.store.sliceExpr(call.args);
                    for (params, 0..) |param_idx, i| {
                        if (param_idx == lookup.pattern_idx and i < args.len) {
                            const arg_expr = ctx.module_env.store.getExpr(args[i]);
                            break :blk ctx.getStringContent(arg_expr) orelse return error.UnsupportedType;
                        }
                    }
                    return error.UnsupportedType;
                },
                else => return error.UnsupportedType,
            };

            // Check if it fits in a small string (< 24 bytes on 64-bit)
            const SMALL_STRING_SIZE = 24;
            if (str_content.len >= SMALL_STRING_SIZE) {
                // Big strings need heap allocation - not supported yet
                return error.UnsupportedType;
            }

            // Build the RocStr as a 24-byte array
            // For small strings: bytes 0-22 are content, byte 23 is length | 0x80
            var roc_str_bytes: [SMALL_STRING_SIZE]u8 = [_]u8{0} ** SMALL_STRING_SIZE;
            @memcpy(roc_str_bytes[0..str_content.len], str_content);
            roc_str_bytes[SMALL_STRING_SIZE - 1] = @as(u8, @intCast(str_content.len)) | 0x80;

            // Store each byte to the output pointer
            // We need to use GEP and store for each byte since LLVM Builder
            // doesn't have a convenient memcpy-like operation for constants
            const i8_type = LlvmBuilder.Type.i8;
            for (roc_str_bytes, 0..) |byte_val, i| {
                const byte_const = ctx.builder.intConst(i8_type, byte_val) catch return error.CompilationFailed;
                const idx_const = ctx.builder.intConst(.i64, @as(i128, @intCast(i))) catch return error.CompilationFailed;
                const byte_ptr = ctx.wip.gep(.inbounds, i8_type, out_ptr, &.{idx_const.toValue()}, "") catch return error.CompilationFailed;
                _ = ctx.wip.store(.normal, byte_const.toValue(), byte_ptr, LlvmBuilder.Alignment.fromByteUnits(1)) catch return error.CompilationFailed;
            }
        }

        /// Emit a list to output pointer
        /// Format: [length: i64][elem0: i64][elem1: i64]...
        /// This inline format avoids pointer allocation issues in JIT
        fn emitListToPtr(ctx: *ExprContext, expr: CIR.Expr, out_ptr: LlvmBuilder.Value) !void {
            switch (expr) {
                .e_empty_list => {
                    // Empty list: just store length = 0
                    const zero = ctx.builder.intConst(.i64, 0) catch return error.CompilationFailed;
                    _ = ctx.wip.store(.normal, zero.toValue(), out_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.CompilationFailed;
                },
                .e_list => |list| {
                    const elems = ctx.module_env.store.sliceExpr(list.elems);

                    // Store length first
                    const length = ctx.builder.intConst(.i64, @as(i128, @intCast(elems.len))) catch return error.CompilationFailed;
                    _ = ctx.wip.store(.normal, length.toValue(), out_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.CompilationFailed;

                    // Store each element (as i64)
                    for (elems, 0..) |elem_idx, i| {
                        const elem_value = try ctx.emitExpr(elem_idx);

                        // Compute offset: 8 bytes for length + i*8 for elements
                        const offset = (i + 1) * 8;
                        const offset_const = ctx.builder.intConst(.i64, @as(i128, @intCast(offset))) catch return error.CompilationFailed;
                        const elem_ptr = ctx.wip.gep(.inbounds, .i8, out_ptr, &.{offset_const.toValue()}, "") catch return error.CompilationFailed;

                        // Extend to i64 and store
                        const extended = ctx.wip.conv(.signed, elem_value, .i64, "") catch return error.CompilationFailed;
                        _ = ctx.wip.store(.normal, extended, elem_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.CompilationFailed;
                    }
                },
                else => return error.UnsupportedType,
            }
        }

        /// Emit a record to output pointer
        /// Returns comma-separated field names for formatting
        fn emitRecordToPtr(ctx: *ExprContext, expr: CIR.Expr, out_ptr: LlvmBuilder.Value, allocator: Allocator) !?[]const u8 {
            switch (expr) {
                .e_empty_record => {
                    // Empty record: just return empty field names
                    return allocator.dupe(u8, "") catch return error.OutOfMemory;
                },
                .e_record => |record| {
                    const field_indices = ctx.module_env.store.sliceRecordFields(record.fields);
                    if (field_indices.len == 0) {
                        return allocator.dupe(u8, "") catch return error.OutOfMemory;
                    }

                    // Collect field names and evaluate values
                    var names_list = std.ArrayListUnmanaged(u8){};
                    defer names_list.deinit(allocator);

                    var offset: usize = 0;
                    for (field_indices, 0..) |field_idx, i| {
                        const field = ctx.module_env.store.getRecordField(field_idx);
                        const field_name = ctx.module_env.getIdentText(field.name);

                        // Add field name to list (comma-separated)
                        if (i > 0) {
                            names_list.append(allocator, ',') catch return error.OutOfMemory;
                        }
                        names_list.appendSlice(allocator, field_name) catch return error.OutOfMemory;

                        // Evaluate the field value
                        const field_value = try ctx.emitExpr(field.value);

                        // Store field value at offset (assume i64 for now)
                        const idx_const = ctx.builder.intConst(.i64, @as(i128, @intCast(offset))) catch return error.CompilationFailed;
                        const field_ptr = ctx.wip.gep(.inbounds, .i8, out_ptr, &.{idx_const.toValue()}, "") catch return error.CompilationFailed;

                        // Extend to i64 and store
                        const extended = ctx.wip.conv(.signed, field_value, .i64, "") catch return error.CompilationFailed;
                        _ = ctx.wip.store(.normal, extended, field_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.CompilationFailed;

                        offset += 8; // Each field is 8 bytes (i64)
                    }

                    return allocator.dupe(u8, names_list.items) catch return error.OutOfMemory;
                },
                else => return error.UnsupportedType,
            }
        }

        /// Emit a list literal
        fn emitList(_: *ExprContext, _: anytype) ExprError!LlvmBuilder.Value {
            // List literals require RocList struct handling
            return error.UnsupportedType;
        }

        /// Emit an empty list
        fn emitEmptyList(_: *ExprContext) ExprError!LlvmBuilder.Value {
            // Empty lists still need RocList struct
            return error.UnsupportedType;
        }

        /// Emit an empty record
        fn emitEmptyRecord(ctx: *ExprContext) ExprError!LlvmBuilder.Value {
            // Empty record {} is represented as unit type
            // For LLVM, we represent it as i64 zero (placeholder)
            return (ctx.builder.intConst(.i64, 0) catch return error.CompilationFailed).toValue();
        }

        /// Emit a record literal
        fn emitRecord(_: *ExprContext, _: anytype) ExprError!LlvmBuilder.Value {
            // Record literals require struct handling
            return error.UnsupportedType;
        }

        /// Emit a record field access (e.g., record.field)
        fn emitDotAccess(ctx: *ExprContext, dot: anytype) ExprError!LlvmBuilder.Value {
            // Get the field/method name
            const field_name = ctx.module_env.getIdentText(dot.field_name);

            // Handle instance method calls like (-3.14).abs()
            if (std.mem.eql(u8, field_name, "abs")) {
                // abs() takes no extra args - the receiver is the value
                const receiver_val = try ctx.emitExpr(dot.receiver);
                const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);

                if (ctx.isFloatExpr(receiver_expr)) {
                    const neg_val = ctx.wip.un(.fneg, receiver_val, "") catch return error.CompilationFailed;
                    const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                    const is_neg = ctx.wip.fcmp(.normal, .olt, receiver_val, zero, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, is_neg, neg_val, receiver_val, "") catch return error.CompilationFailed;
                }

                // For integers
                const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, receiver_expr) catch return error.CompilationFailed;
                const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                const neg_val = ctx.wip.bin(.sub, zero, receiver_val, "") catch return error.CompilationFailed;
                const is_neg = ctx.wip.icmp(.slt, receiver_val, zero, "") catch return error.CompilationFailed;
                return ctx.wip.select(.normal, is_neg, neg_val, receiver_val, "") catch return error.CompilationFailed;
            }

            if (std.mem.eql(u8, field_name, "neg")) {
                const receiver_val = try ctx.emitExpr(dot.receiver);
                const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);

                if (ctx.isFloatExpr(receiver_expr)) {
                    return ctx.wip.un(.fneg, receiver_val, "") catch return error.CompilationFailed;
                }

                const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, receiver_expr) catch return error.CompilationFailed;
                const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                return ctx.wip.bin(.sub, zero, receiver_val, "") catch return error.CompilationFailed;
            }

            // Handle abs_diff method call: (3.5).abs_diff(1.2)
            if (std.mem.eql(u8, field_name, "abs_diff")) {
                if (dot.args) |args_span| {
                    const args = ctx.module_env.store.sliceExpr(args_span);
                    if (args.len == 1) {
                        const lhs_expr = ctx.module_env.store.getExpr(dot.receiver);
                        const rhs_expr = ctx.module_env.store.getExpr(args[0]);

                        // For Dec literals, compute at compile time
                        if (ctx.isDecExpr(lhs_expr) and ctx.isDecExpr(rhs_expr)) {
                            const lhs_dec = ctx.getDecValue(lhs_expr) orelse return error.UnsupportedType;
                            const rhs_dec = ctx.getDecValue(rhs_expr) orelse return error.UnsupportedType;
                            const diff = if (lhs_dec >= rhs_dec) lhs_dec - rhs_dec else rhs_dec - lhs_dec;
                            return (ctx.builder.intConst(.i128, diff) catch return error.CompilationFailed).toValue();
                        }

                        const lhs_val = try ctx.emitExpr(dot.receiver);
                        const rhs_val = try ctx.emitExpr(args[0]);

                        // Compute a - b
                        const diff = if (ctx.isFloatExpr(lhs_expr))
                            ctx.wip.bin(.fsub, lhs_val, rhs_val, "") catch return error.CompilationFailed
                        else
                            ctx.wip.bin(.sub, lhs_val, rhs_val, "") catch return error.CompilationFailed;

                        // Then take abs
                        if (ctx.isFloatExpr(lhs_expr)) {
                            const neg_diff = ctx.wip.un(.fneg, diff, "") catch return error.CompilationFailed;
                            const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                            const is_neg = ctx.wip.fcmp(.normal, .olt, diff, zero, "") catch return error.CompilationFailed;
                            return ctx.wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                        }

                        const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, lhs_expr) catch return error.CompilationFailed;
                        const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                        const neg_diff = ctx.wip.bin(.sub, zero, diff, "") catch return error.CompilationFailed;
                        const is_signed = ctx.isSignedExpr(lhs_expr);
                        const is_neg = if (is_signed)
                            ctx.wip.icmp(.slt, diff, zero, "") catch return error.CompilationFailed
                        else
                            ctx.wip.icmp(.ult, lhs_val, rhs_val, "") catch return error.CompilationFailed;
                        return ctx.wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                    }
                }
            }

            // Handle type annotations via dot access (e.g., 3.14.F64, 42.I32)
            if (std.mem.eql(u8, field_name, "F32") or std.mem.eql(u8, field_name, "F64") or std.mem.eql(u8, field_name, "Dec")) {
                // Float types - emit the receiver and convert if needed
                const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);

                // If the receiver is already a float, just return it
                if (ctx.isFloatExpr(receiver_expr)) {
                    return try ctx.emitExpr(dot.receiver);
                }

                // If the receiver is a decimal/int, we need to convert
                if (receiver_expr == .e_dec_small) {
                    const dec = receiver_expr.e_dec_small;
                    // Convert from decimal representation to float
                    const value: f64 = @as(f64, @floatFromInt(dec.value.numerator)) / std.math.pow(f64, 10.0, @as(f64, @floatFromInt(dec.value.denominator_power_of_ten)));
                    return (ctx.builder.doubleConst(value) catch return error.CompilationFailed).toValue();
                }

                return try ctx.emitExpr(dot.receiver);
            }

            if (std.mem.eql(u8, field_name, "I8") or std.mem.eql(u8, field_name, "U8") or
                std.mem.eql(u8, field_name, "I16") or std.mem.eql(u8, field_name, "U16") or
                std.mem.eql(u8, field_name, "I32") or std.mem.eql(u8, field_name, "U32") or
                std.mem.eql(u8, field_name, "I64") or std.mem.eql(u8, field_name, "U64") or
                std.mem.eql(u8, field_name, "I128") or std.mem.eql(u8, field_name, "U128"))
            {
                // Integer types - just emit the receiver
                return try ctx.emitExpr(dot.receiver);
            }

            // Try record field access - if the receiver is a record literal
            const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);
            if (receiver_expr == .e_record) {
                const record = receiver_expr.e_record;
                const field_indices = ctx.module_env.store.sliceRecordFields(record.fields);
                for (field_indices) |field_idx| {
                    const field = ctx.module_env.store.getRecordField(field_idx);
                    const this_field_name = ctx.module_env.getIdentText(field.name);
                    if (std.mem.eql(u8, this_field_name, field_name)) {
                        // Found the field - emit its value
                        return ctx.emitExpr(field.value);
                    }
                }
                // Field not found in record - this would be a type error
                return error.UnsupportedType;
            }

            // Record field access on non-literal requires runtime evaluation
            return error.UnsupportedType;
        }

        /// Check if an expression is a float type
        fn isFloatExpr(ctx: *ExprContext, expr: CIR.Expr) bool {
            return switch (expr) {
                .e_frac_f32, .e_frac_f64 => true,
                .e_num => |num| num.kind == .f32 or num.kind == .f64,
                .e_binop => |binop| ctx.isFloatExpr(ctx.module_env.store.getExpr(binop.lhs)),
                .e_unary_minus => |unary| ctx.isFloatExpr(ctx.module_env.store.getExpr(unary.expr)),
                .e_lookup_local => false, // Would need type info
                else => false,
            };
        }

        /// Check if an expression is a Dec type
        fn isDecExpr(ctx: *ExprContext, expr: CIR.Expr) bool {
            return switch (expr) {
                .e_dec, .e_dec_small => true,
                .e_num => |num| num.kind == .dec,
                .e_typed_frac => |typed_frac| std.mem.eql(u8, ctx.module_env.getIdentText(typed_frac.type_name), "Dec"),
                .e_binop => |binop| ctx.isDecExpr(ctx.module_env.store.getExpr(binop.lhs)),
                .e_unary_minus => |unary| ctx.isDecExpr(ctx.module_env.store.getExpr(unary.expr)),
                else => false,
            };
        }

        /// Check if an expression is a signed integer type
        fn isSignedExpr(ctx: *ExprContext, expr: CIR.Expr) bool {
            return switch (expr) {
                .e_num => |num| switch (num.kind) {
                    .i8, .i16, .i32, .i64, .i128, .num_unbound, .int_unbound => true,
                    else => false,
                },
                .e_dec, .e_dec_small => true, // Dec is signed
                .e_binop => |binop| ctx.isSignedExpr(ctx.module_env.store.getExpr(binop.lhs)),
                .e_unary_minus => |unary| ctx.isSignedExpr(ctx.module_env.store.getExpr(unary.expr)),
                else => true, // Default to signed for safety
            };
        }

        /// Check if an expression is a string type
        fn isStringExpr(_: *ExprContext, expr: CIR.Expr) bool {
            return switch (expr) {
                .e_str, .e_str_segment => true,
                else => false,
            };
        }

        /// Emit string comparison operation
        /// For small strings, we can compare the bytes directly
        fn emitStringComparison(ctx: *ExprContext, binop: CIR.Expr.Binop, lhs_expr: CIR.Expr, rhs_expr: CIR.Expr) ExprError!LlvmBuilder.Value {
            // Get the string contents at compile time if possible
            const lhs_content = ctx.getStringContent(lhs_expr) orelse return error.UnsupportedType;
            const rhs_content = ctx.getStringContent(rhs_expr) orelse return error.UnsupportedType;

            // For compile-time known strings, we can evaluate the comparison directly
            const result: bool = switch (binop.op) {
                .eq => std.mem.eql(u8, lhs_content, rhs_content),
                .ne => !std.mem.eql(u8, lhs_content, rhs_content),
                .lt => std.mem.order(u8, lhs_content, rhs_content) == .lt,
                .le => std.mem.order(u8, lhs_content, rhs_content) != .gt,
                .gt => std.mem.order(u8, lhs_content, rhs_content) == .gt,
                .ge => std.mem.order(u8, lhs_content, rhs_content) != .lt,
                else => return error.UnsupportedType, // Other string ops not yet supported
            };

            // Return a boolean constant (i8 for Bool)
            const bool_val: i128 = if (result) 1 else 0;
            return (ctx.builder.intConst(.i8, bool_val) catch return error.CompilationFailed).toValue();
        }

        /// Get Dec value as scaled i128 from a Dec expression (if compile-time known)
        fn getDecValue(ctx: *ExprContext, expr: CIR.Expr) ?i128 {
            return switch (expr) {
                .e_dec => |dec| dec.value.num,
                .e_dec_small => |dec| blk: {
                    const decimal_places: u5 = 18;
                    const scale_factor = std.math.pow(i128, 10, decimal_places - dec.value.denominator_power_of_ten);
                    break :blk @as(i128, dec.value.numerator) * scale_factor;
                },
                .e_typed_frac => |typed_frac| blk: {
                    const type_name = ctx.module_env.getIdentText(typed_frac.type_name);
                    if (!std.mem.eql(u8, type_name, "Dec")) break :blk null;
                    break :blk typed_frac.value.toI128();
                },
                else => null,
            };
        }

        /// Get string content from a string expression (if compile-time known)
        fn getStringContent(ctx: *ExprContext, expr: CIR.Expr) ?[]const u8 {
            return switch (expr) {
                .e_str_segment => |seg| ctx.module_env.getString(seg.literal),
                .e_str => |str_expr| blk: {
                    const segments = ctx.module_env.store.sliceExpr(str_expr.span);
                    if (segments.len != 1) break :blk null;
                    const seg_expr = ctx.module_env.store.getExpr(segments[0]);
                    if (seg_expr != .e_str_segment) break :blk null;
                    break :blk ctx.module_env.getString(seg_expr.e_str_segment.literal);
                },
                else => null,
            };
        }
    };

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
        return self.generateBitcode(&module_env, final_expr_idx);
    }

    /// Try to resolve what expression a call will return (unused for now)
    /// Used to detect if a lambda call returns a record
    fn resolveCallResultExpr(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) ?CIR.Expr {
        _ = self;
        _ = module_env;
        _ = expr;
        // Disabled - record result detection is complex when records are inside lambda bodies
        return null;
    }

    /// Get the output layout for JIT execution from a CIR expression index
    /// This captures signedness which LLVM types don't distinguish
    fn getExprOutputLayout(self: *LlvmEvaluator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) layout.Idx {
        const expr = module_env.store.getExpr(expr_idx);
        return self.getExprOutputLayoutFromExpr(module_env, expr);
    }

    /// Get the output layout for JIT execution from a CIR expression
    fn getExprOutputLayoutFromExpr(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) layout.Idx {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .i8 => .i8,
                .i16 => .i16,
                .i32 => .i32,
                .i64, .num_unbound, .int_unbound => .i64,
                .u8 => .u8,
                .u16 => .u16,
                .u32 => .u32,
                .u64 => .u64,
                .i128 => .i128,
                .u128 => .u128,
                .f32 => .f32,
                .f64 => .f64,
                .dec => .dec,
            },
            .e_frac_f32 => .f32,
            .e_frac_f64 => .f64,
            .e_dec, .e_dec_small => .dec,
            .e_typed_int => |typed_int| {
                const type_name = module_env.getIdentText(typed_int.type_name);
                if (std.mem.eql(u8, type_name, "I8")) return .i8;
                if (std.mem.eql(u8, type_name, "U8")) return .u8;
                if (std.mem.eql(u8, type_name, "I16")) return .i16;
                if (std.mem.eql(u8, type_name, "U16")) return .u16;
                if (std.mem.eql(u8, type_name, "I32")) return .i32;
                if (std.mem.eql(u8, type_name, "U32")) return .u32;
                if (std.mem.eql(u8, type_name, "I64")) return .i64;
                if (std.mem.eql(u8, type_name, "U64")) return .u64;
                if (std.mem.eql(u8, type_name, "I128")) return .i128;
                if (std.mem.eql(u8, type_name, "U128")) return .u128;
                if (std.mem.eql(u8, type_name, "Dec")) return .dec;
                if (std.mem.eql(u8, type_name, "F32")) return .f32;
                if (std.mem.eql(u8, type_name, "F64")) return .f64;
                return .i64;
            },
            .e_typed_frac => |typed_frac| {
                const type_name = module_env.getIdentText(typed_frac.type_name);
                if (std.mem.eql(u8, type_name, "Dec")) return .dec;
                if (std.mem.eql(u8, type_name, "F32")) return .f32;
                if (std.mem.eql(u8, type_name, "F64")) return .f64;
                return .f64;
            },
            .e_binop => |binop| switch (binop.op) {
                // Comparison operators return Bool
                .lt, .le, .gt, .ge, .eq, .ne => .bool,
                // Logical operators return Bool
                .@"and", .@"or" => .bool,
                // Arithmetic operators inherit from LHS
                else => self.getExprOutputLayout(module_env, binop.lhs),
            },
            .e_unary_minus => |unary| self.getExprOutputLayout(module_env, unary.expr),
            .e_unary_not => .bool,
            .e_zero_argument_tag => .bool, // For now, assume Bool tags
            .e_tag => |tag| {
                // Check if this is a zero-argument boolean tag
                const args = module_env.store.sliceExpr(tag.args);
                if (args.len == 0) {
                    return .bool; // Zero-arg tags like True/False
                }
                return .i64; // Tags with args - default for now
            },
            .e_if => |if_expr| {
                // Get layout from final else (or first branch body)
                return self.getExprOutputLayout(module_env, if_expr.final_else);
            },
            .e_block => |blk| self.getExprOutputLayout(module_env, blk.final_expr),
            .e_lookup_local => .i64, // Would need type info for proper layout
            .e_str_segment, .e_str => .str,
            .e_call => |call| {
                // Try to determine output type from the called function
                const func_expr = module_env.store.getExpr(call.func);
                if (func_expr == .e_lookup_external) {
                    const lookup = func_expr.e_lookup_external;
                    const func_name = module_env.getIdentText(lookup.ident_idx);
                    // Bool.not returns Bool
                    if (std.mem.eql(u8, func_name, "not")) {
                        return .bool;
                    }
                    // Num.abs and Num.neg return the same type as their argument
                    if (std.mem.eql(u8, func_name, "abs") or std.mem.eql(u8, func_name, "neg")) {
                        const args = module_env.store.sliceExpr(call.args);
                        if (args.len == 1) {
                            return self.getExprOutputLayout(module_env, args[0]);
                        }
                    }
                }
                // Lambda call - get layout from lambda body
                if (func_expr == .e_lambda) {
                    const lambda = func_expr.e_lambda;
                    const body_expr = module_env.store.getExpr(lambda.body);
                    // If the body is a local lookup (identity lambda), check if it's a parameter
                    // and return the corresponding argument's layout
                    if (body_expr == .e_lookup_local) {
                        const lookup = body_expr.e_lookup_local;
                        const params = module_env.store.slicePatterns(lambda.args);
                        const args = module_env.store.sliceExpr(call.args);
                        for (params, 0..) |param_idx, i| {
                            if (param_idx == lookup.pattern_idx and i < args.len) {
                                return self.getExprOutputLayout(module_env, args[i]);
                            }
                        }
                    }
                    return self.getExprOutputLayout(module_env, lambda.body);
                }
                return .i64; // Default for unknown calls
            },
            .e_dot_access => |dot| {
                // Method calls like (-3.14).abs() return the same type as the receiver
                const field_name = module_env.getIdentText(dot.field_name);
                if (std.mem.eql(u8, field_name, "abs") or std.mem.eql(u8, field_name, "neg") or std.mem.eql(u8, field_name, "abs_diff")) {
                    return self.getExprOutputLayout(module_env, dot.receiver);
                }
                // Type annotations via dot access like 3.14.F64, 42.I32
                if (std.mem.eql(u8, field_name, "I8")) return .i8;
                if (std.mem.eql(u8, field_name, "U8")) return .u8;
                if (std.mem.eql(u8, field_name, "I16")) return .i16;
                if (std.mem.eql(u8, field_name, "U16")) return .u16;
                if (std.mem.eql(u8, field_name, "I32")) return .i32;
                if (std.mem.eql(u8, field_name, "U32")) return .u32;
                if (std.mem.eql(u8, field_name, "I64")) return .i64;
                if (std.mem.eql(u8, field_name, "U64")) return .u64;
                if (std.mem.eql(u8, field_name, "I128")) return .i128;
                if (std.mem.eql(u8, field_name, "U128")) return .u128;
                if (std.mem.eql(u8, field_name, "F32")) return .f32;
                if (std.mem.eql(u8, field_name, "F64")) return .f64;
                if (std.mem.eql(u8, field_name, "Dec")) return .dec;
                // Try record field access - get layout from the field value
                const receiver_expr = module_env.store.getExpr(dot.receiver);
                if (receiver_expr == .e_record) {
                    const record = receiver_expr.e_record;
                    const field_indices = module_env.store.sliceRecordFields(record.fields);
                    for (field_indices) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        const this_field_name = module_env.getIdentText(field.name);
                        if (std.mem.eql(u8, this_field_name, field_name)) {
                            return self.getExprOutputLayout(module_env, field.value);
                        }
                    }
                }
                return .i64; // Default for record field access
            },
            .e_nominal_external => |nominal| {
                // Get layout from the backing expression (e.g., Bool.True -> True tag)
                return self.getExprOutputLayout(module_env, nominal.backing_expr);
            },
            else => .i64, // Default for other expression types
        };
    }

    /// Get the LLVM type for a CIR expression
    fn getExprLlvmTypeFromExpr(_: *LlvmEvaluator, builder: *LlvmBuilder, expr: CIR.Expr) !LlvmBuilder.Type {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .u8, .i8 => .i8,
                .u16, .i16 => .i16,
                .u32, .i32 => .i32,
                .u64, .i64, .num_unbound, .int_unbound => .i64,
                .u128, .i128, .dec => .i128,
                .f32 => .float,
                .f64 => .double,
            },
            .e_frac_f32 => .float,
            .e_frac_f64 => .double,
            .e_dec, .e_dec_small => .i128,
            .e_zero_argument_tag => .i8, // Bool is stored as u8
            .e_unary_not => .i1, // Boolean result
            else => try builder.ptrType(.default),
        };
    }
};

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works - the evaluator should be usable
    try std.testing.expect(evaluator.allocator.ptr == allocator.ptr);
}
