//! LLVM-based Evaluator for Roc expressions
//!
//! This module generates LLVM bitcode from Roc expressions. The bitcode can then
//! be compiled to an object file using llvm_compile.compileToObject() and executed
//! using llvm_execute.executeAndFormat().
//!
//! Used when the `--opt=size` or `--opt=speed` flags are passed to the REPL.
//!
//! The evaluator works by:
//! 1. Parsing and type-checking the source expression
//! 2. Translating the CIR to LLVM IR using Zig's LLVM Builder
//! 3. Serializing to LLVM bitcode for compilation to native code
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

// LLVM Builder from Zig's standard library (for IR generation)
const LlvmBuilder = std.zig.llvm.Builder;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;

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
            .local_layouts = std.AutoHashMap(CIR.Pattern.Idx, layout.Idx).init(self.allocator),
        };
        defer ctx.locals.deinit();
        defer ctx.local_layouts.deinit();

        // Get the output pointer
        const out_ptr = wip.arg(0);

        // Special handling for list output - write RocList struct directly
        var record_field_names: ?[]const u8 = null;
        if (is_list_expr) {
            try ctx.emitListToPtr(top_expr, out_ptr);
        } else if (is_record_expr or is_record_result) {
            // Handle record output - emit field values to output buffer
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
        local_layouts: std.AutoHashMap(CIR.Pattern.Idx, layout.Idx),

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
            return switch (num.kind) {
                .f32 => blk: {
                    const int_value = num.value.toI128();
                    break :blk (ctx.builder.floatConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue();
                },
                .f64 => blk: {
                    const int_value = num.value.toI128();
                    break :blk (ctx.builder.doubleConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue();
                },
                .u128, .i128 => blk: {
                    // 128-bit values: use u128 interpretation since LLVM doesn't distinguish signedness
                    // This handles large unsigned values that would overflow i128 (like Rust's const_u128)
                    const u128_value: u128 = @bitCast(num.value.bytes);
                    break :blk (ctx.builder.intConst(.i128, u128_value) catch return error.CompilationFailed).toValue();
                },
                else => blk: {
                    const u128_value: u128 = @bitCast(num.value.bytes);
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, expr) catch return error.CompilationFailed;
                    // For i64 type, check if value fits - if not, emit as i128 (will be converted later)
                    if (llvm_type == .i64) {
                        if (u128_value <= std.math.maxInt(u64)) {
                            break :blk (ctx.builder.intConst(.i64, @as(u64, @truncate(u128_value))) catch return error.CompilationFailed).toValue();
                        }
                        // Value too large for i64 - emit as i128, convertNumericToLayout will handle it
                        break :blk (ctx.builder.intConst(.i128, u128_value) catch return error.CompilationFailed).toValue();
                    }
                    if (llvm_type == .i128) {
                        break :blk (ctx.builder.intConst(.i128, u128_value) catch return error.CompilationFailed).toValue();
                    }
                    const int_value = num.value.toI128();
                    break :blk (ctx.builder.intConst(llvm_type, int_value) catch return error.CompilationFailed).toValue();
                },
            };
        }

        /// Emit a typed integer literal (e.g., 42.U32, 100.I64)
        fn emitTypedInt(ctx: *ExprContext, typed_int: anytype) ExprError!LlvmBuilder.Value {
            const int_value = typed_int.value.toI128();
            const type_ident = typed_int.type_name;
            const idents = ctx.module_env.idents;

            // Handle Dec type - scale integer to Dec representation
            if (type_ident == idents.dec) {
                const dec_value = int_value * 1000000000000000000; // * 10^18
                return (ctx.builder.intConst(.i128, dec_value) catch return error.CompilationFailed).toValue();
            }

            // Handle float types
            if (type_ident == idents.f32) {
                return (ctx.builder.floatConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue();
            }
            if (type_ident == idents.f64) {
                return (ctx.builder.doubleConst(@floatFromInt(int_value)) catch return error.CompilationFailed).toValue();
            }

            // Determine LLVM type from the type ident
            const llvm_type: LlvmBuilder.Type = if (type_ident == idents.i8 or type_ident == idents.u8)
                .i8
            else if (type_ident == idents.i16 or type_ident == idents.u16)
                .i16
            else if (type_ident == idents.i32 or type_ident == idents.u32)
                .i32
            else if (type_ident == idents.i64 or type_ident == idents.u64)
                .i64
            else if (type_ident == idents.i128 or type_ident == idents.u128)
                .i128
            else
                return error.UnsupportedType;

            return (ctx.builder.intConst(llvm_type, int_value) catch return error.CompilationFailed).toValue();
        }

        /// Emit a typed fractional literal (e.g., 3.14.Dec, 2.0.F64)
        fn emitTypedFrac(ctx: *ExprContext, typed_frac: anytype) ExprError!LlvmBuilder.Value {
            const type_ident = typed_frac.type_name;
            const idents = ctx.module_env.idents;
            const int_value = typed_frac.value.toI128();

            if (type_ident == idents.dec) {
                // Dec values are stored as scaled i128 (like e_dec)
                return (ctx.builder.intConst(.i128, int_value) catch return error.CompilationFailed).toValue();
            } else if (type_ident == idents.f32) {
                // The value is stored as Dec-scaled (int * 10^18), need to convert to float
                const float_val: f32 = @as(f32, @floatFromInt(int_value)) / 1e18;
                return (ctx.builder.floatConst(float_val) catch return error.CompilationFailed).toValue();
            } else if (type_ident == idents.f64) {
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

            // Get layout to determine signedness (like Rust backend's IntWidth.is_signed())
            const lhs_layout = ctx.getExprLayout(binop.lhs);
            const is_float = ctx.isFloatExpr(lhs_expr);
            const is_dec = ctx.isDecExpr(lhs_expr);
            const is_signed = lhs_layout.isSigned();

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
        /// All booleans are i1, so we can use simple bitwise AND
        fn emitShortCircuitAnd(ctx: *ExprContext, binop: CIR.Expr.Binop) ExprError!LlvmBuilder.Value {
            const lhs = try ctx.emitExpr(binop.lhs);
            const rhs = try ctx.emitExpr(binop.rhs);
            // AND: if lhs is true, result is rhs; otherwise result is false
            return ctx.wip.select(.normal, lhs, rhs, try ctx.boolConst(false), "") catch return error.CompilationFailed;
        }

        /// Emit short-circuit OR operation using select
        /// All booleans are i1, so we can use simple bitwise OR
        fn emitShortCircuitOr(ctx: *ExprContext, binop: CIR.Expr.Binop) ExprError!LlvmBuilder.Value {
            const lhs = try ctx.emitExpr(binop.lhs);
            const rhs = try ctx.emitExpr(binop.rhs);
            // OR: if lhs is true, result is true; otherwise result is rhs
            return ctx.wip.select(.normal, lhs, try ctx.boolConst(true), rhs, "") catch return error.CompilationFailed;
        }

        /// Emit unary minus operation
        fn emitUnaryMinus(ctx: *ExprContext, unary: CIR.Expr.UnaryMinus) ExprError!LlvmBuilder.Value {
            const operand = try ctx.emitExpr(unary.expr);
            const operand_expr = ctx.module_env.store.getExpr(unary.expr);
            const is_float = ctx.isFloatExpr(operand_expr);

            if (is_float) {
                return ctx.wip.un(.fneg, operand, "") catch return error.CompilationFailed;
            } else {
                // For integers, use the neg instruction which gets the type from the value
                return ctx.wip.neg(operand, "") catch return error.CompilationFailed;
            }
        }

        /// Emit unary not operation
        fn emitUnaryNot(ctx: *ExprContext, unary: CIR.Expr.UnaryNot) ExprError!LlvmBuilder.Value {
            const operand = try ctx.emitExpr(unary.expr);
            // XOR with true to flip the boolean
            return ctx.wip.bin(.xor, operand, try ctx.boolConst(true), "") catch return error.CompilationFailed;
        }

        /// Emit if expression using proper control flow with basic blocks and phi nodes
        fn emitIf(ctx: *ExprContext, if_expr: anytype, _: CIR.Expr.Idx) ExprError!LlvmBuilder.Value {
            const branches = ctx.module_env.store.sliceIfBranches(if_expr.branches);
            if (branches.len == 0) {
                // No branches, just evaluate final else
                return ctx.emitExpr(if_expr.final_else);
            }

            // For single-branch if-else, use standard then/else/merge pattern
            // For multi-branch, chain the conditions
            const num_incoming: u32 = @intCast(branches.len + 1); // +1 for final else
            const merge_block = ctx.wip.block(num_incoming, "if_merge") catch return error.CompilationFailed;

            // We'll collect values and their source blocks for the phi node
            // Use @TypeOf to get the block index type since it's not publicly exported
            const BlockIndex = @TypeOf(merge_block);
            var phi_values: [16]LlvmBuilder.Value = undefined;
            var phi_blocks: [16]BlockIndex = undefined;
            if (branches.len + 1 > 16) {
                return error.UnsupportedType; // Too many branches
            }

            var phi_count: usize = 0;

            // Process each branch
            for (branches, 0..) |branch_idx, i| {
                const branch = ctx.module_env.store.getIfBranch(branch_idx);

                // Evaluate condition - may be i1 (from comparison) or i8 (from bool tag)
                const cond_val = try ctx.emitExpr(branch.cond);
                const cond_type = cond_val.typeOfWip(ctx.wip);

                // Convert to i1 if needed (brCond requires i1)
                const cond_i1 = if (cond_type == .i1)
                    cond_val
                else blk: {
                    // Bool is i8, compare with 0 to get i1
                    const zero = (ctx.builder.intConst(cond_type.scalarType(ctx.builder), 0) catch return error.CompilationFailed).toValue();
                    break :blk ctx.wip.icmp(.ne, cond_val, zero, "") catch return error.CompilationFailed;
                };

                // Create then block and next block (either next condition or final else)
                const then_block = ctx.wip.block(1, "if_then") catch return error.CompilationFailed;
                const is_last_branch = (i == branches.len - 1);
                const else_block = if (is_last_branch)
                    ctx.wip.block(1, "if_else") catch return error.CompilationFailed
                else
                    ctx.wip.block(1, "if_elseif") catch return error.CompilationFailed;

                // Branch based on condition
                _ = ctx.wip.brCond(cond_i1, then_block, else_block, .none) catch return error.CompilationFailed;

                // Emit then block
                ctx.wip.cursor = .{ .block = then_block };
                const then_val = try ctx.emitExpr(branch.body);
                // Get the current block (may have changed during emitExpr if there were nested ifs)
                const then_exit_block = ctx.wip.cursor.block;
                _ = ctx.wip.br(merge_block) catch return error.CompilationFailed;

                phi_values[phi_count] = then_val;
                phi_blocks[phi_count] = then_exit_block;
                phi_count += 1;

                // Move to else block for next iteration or final else
                ctx.wip.cursor = .{ .block = else_block };
            }

            // Emit final else block (we're already positioned there)
            const else_val = try ctx.emitExpr(if_expr.final_else);
            const else_exit_block = ctx.wip.cursor.block;
            _ = ctx.wip.br(merge_block) catch return error.CompilationFailed;

            phi_values[phi_count] = else_val;
            phi_blocks[phi_count] = else_exit_block;
            phi_count += 1;

            // Emit merge block with phi node
            ctx.wip.cursor = .{ .block = merge_block };

            // Determine the result type from the then value
            const result_type = phi_values[0].typeOfWip(ctx.wip);
            const wip_phi = ctx.wip.phi(result_type, "if_result") catch return error.CompilationFailed;
            wip_phi.finish(phi_values[0..phi_count], phi_blocks[0..phi_count], ctx.wip);

            return wip_phi.toValue();
        }

        /// Emit a zero-argument tag (like True, False, or any 2-tag union)
        fn emitZeroArgTag(ctx: *ExprContext, tag: anytype) ExprError!LlvmBuilder.Value {
            // All 2-tag unions with no payloads use i1: True=1, False=0
            return ctx.boolConst(tag.name == ctx.module_env.idents.true_tag);
        }

        /// Emit a tag expression (e.g., True, False, Ok(value), Err(msg))
        fn emitTag(ctx: *ExprContext, tag: anytype) ExprError!LlvmBuilder.Value {
            // Check if this is a zero-argument tag (like True, False)
            const args = ctx.module_env.store.sliceExpr(tag.args);
            if (args.len == 0) {
                // Zero-argument tag - all 2-tag unions with no payloads use i1
                return ctx.boolConst(tag.name == ctx.module_env.idents.true_tag);
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
                        // Get target layout from annotation first (like Rust mono's make_num_literal)
                        const decl_layout = if (decl.anno) |anno_idx|
                            ctx.evaluator.getLayoutFromAnnotation(ctx.module_env, anno_idx) orelse
                                ctx.evaluator.getExprOutputLayout(ctx.module_env, decl.expr)
                        else
                            ctx.evaluator.getExprOutputLayout(ctx.module_env, decl.expr);
                        // Evaluate the expression
                        var val = try ctx.emitExpr(decl.expr);
                        // Convert to target layout if needed (e.g., int literal with float annotation)
                        val = try ctx.convertNumericToLayout(val, decl_layout);
                        // Bind to pattern
                        try ctx.locals.put(decl.pattern, val);
                        ctx.local_layouts.put(decl.pattern, decl_layout) catch {};
                    },
                    .s_var => |var_stmt| {
                        // Get target layout from annotation first
                        const var_layout = if (var_stmt.anno) |anno_idx|
                            ctx.evaluator.getLayoutFromAnnotation(ctx.module_env, anno_idx) orelse
                                ctx.evaluator.getExprOutputLayout(ctx.module_env, var_stmt.expr)
                        else
                            ctx.evaluator.getExprOutputLayout(ctx.module_env, var_stmt.expr);
                        var val = try ctx.emitExpr(var_stmt.expr);
                        // Convert to target layout if needed
                        val = try ctx.convertNumericToLayout(val, var_layout);
                        try ctx.locals.put(var_stmt.pattern_idx, val);
                        ctx.local_layouts.put(var_stmt.pattern_idx, var_layout) catch {};
                    },
                    else => {}, // Ignore other statement types for now
                }
            }

            // Evaluate and return the final expression
            return ctx.emitExpr(blk.final_expr);
        }

        /// Emit a function call
        /// Supports builtin functions that have corresponding idents in CommonIdents
        fn emitCall(ctx: *ExprContext, call: anytype) ExprError!LlvmBuilder.Value {
            const func_expr = ctx.module_env.store.getExpr(call.func);

            // Handle e_lookup_external for builtin functions
            if (func_expr == .e_lookup_external) {
                const lookup = func_expr.e_lookup_external;
                const func_ident = lookup.ident_idx;
                const idents = ctx.module_env.idents;
                const args = ctx.module_env.store.sliceExpr(call.args);

                // Handle Bool.not - XOR with true to flip the boolean
                if (func_ident == idents.not and args.len == 1) {
                    const arg_val = try ctx.emitExpr(args[0]);
                    return ctx.wip.bin(.xor, arg_val, try ctx.boolConst(true), "") catch return error.CompilationFailed;
                }

                // Handle Num.abs - absolute value
                if (func_ident == idents.abs and args.len == 1) {
                    const arg_val = try ctx.emitExpr(args[0]);
                    const arg_expr = ctx.module_env.store.getExpr(args[0]);
                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, arg_expr) catch return error.CompilationFailed;

                    if (ctx.isFloatExpr(arg_expr)) {
                        const neg_val = ctx.wip.un(.fneg, arg_val, "") catch return error.CompilationFailed;
                        const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                        const is_neg = ctx.wip.fcmp(.normal, .olt, arg_val, zero, "") catch return error.CompilationFailed;
                        return ctx.wip.select(.normal, is_neg, neg_val, arg_val, "") catch return error.CompilationFailed;
                    }

                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                    const neg_val = ctx.wip.bin(.sub, zero, arg_val, "") catch return error.CompilationFailed;
                    const is_neg = ctx.wip.icmp(.slt, arg_val, zero, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, is_neg, neg_val, arg_val, "") catch return error.CompilationFailed;
                }

                // Handle Num.negate - negation
                if (func_ident == idents.negate and args.len == 1) {
                    const arg_val = try ctx.emitExpr(args[0]);
                    const arg_expr = ctx.module_env.store.getExpr(args[0]);

                    if (ctx.isFloatExpr(arg_expr)) {
                        return ctx.wip.un(.fneg, arg_val, "") catch return error.CompilationFailed;
                    }

                    const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, arg_expr) catch return error.CompilationFailed;
                    const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                    return ctx.wip.bin(.sub, zero, arg_val, "") catch return error.CompilationFailed;
                }

                // Handle Num.abs_diff - absolute difference |a - b|
                if (func_ident == idents.abs_diff and args.len == 2) {
                    const lhs_val = try ctx.emitExpr(args[0]);
                    const rhs_val = try ctx.emitExpr(args[1]);
                    const lhs_expr = ctx.module_env.store.getExpr(args[0]);

                    const diff = if (ctx.isFloatExpr(lhs_expr))
                        ctx.wip.bin(.fsub, lhs_val, rhs_val, "") catch return error.CompilationFailed
                    else
                        ctx.wip.bin(.sub, lhs_val, rhs_val, "") catch return error.CompilationFailed;

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

            // Handle method calls on values (e.g., (3.5).abs_diff(1.2))
            if (func_expr == .e_dot_access) {
                const dot = func_expr.e_dot_access;
                const method_ident = dot.field_name;
                const idents = ctx.module_env.idents;
                const args = ctx.module_env.store.sliceExpr(call.args);

                if (method_ident == idents.abs_diff and args.len == 1) {
                    const lhs_expr = ctx.module_env.store.getExpr(dot.receiver);
                    const rhs_expr = ctx.module_env.store.getExpr(args[0]);

                    // For Dec literals, compute at compile time
                    if (ctx.isDecExpr(lhs_expr) and ctx.isDecExpr(rhs_expr)) {
                        const lhs_dec = ctx.getDecValue(lhs_expr) orelse return error.UnsupportedType;
                        const rhs_dec = ctx.getDecValue(rhs_expr) orelse return error.UnsupportedType;
                        const diff = if (lhs_dec >= rhs_dec) lhs_dec - rhs_dec else rhs_dec - lhs_dec;
                        return (ctx.builder.intConst(.i128, @as(i128, @intCast(diff))) catch return error.CompilationFailed).toValue();
                    }

                    const lhs_val = try ctx.emitExpr(dot.receiver);
                    const rhs_val = try ctx.emitExpr(args[0]);

                    const diff = if (ctx.isFloatExpr(lhs_expr))
                        ctx.wip.bin(.fsub, lhs_val, rhs_val, "") catch return error.CompilationFailed
                    else
                        ctx.wip.bin(.sub, lhs_val, rhs_val, "") catch return error.CompilationFailed;

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
                                return ctx.boolConst(true);
                            }
                            if (arg_expr == .e_list) {
                                const list = arg_expr.e_list;
                                const elems = ctx.module_env.store.sliceExpr(list.elems);
                                return ctx.boolConst(elems.len == 0);
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
                    const field_ident = dot.field_name;
                    const field_indices = ctx.module_env.store.sliceRecordFields(record.fields);
                    for (field_indices) |field_idx| {
                        const field = ctx.module_env.store.getRecordField(field_idx);
                        if (field.name == field_ident) {
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
            const field_ident = dot.field_name;
            const idents = ctx.module_env.idents;

            // Handle instance method calls like (-3.14).abs()
            if (field_ident == idents.abs) {
                const receiver_val = try ctx.emitExpr(dot.receiver);
                const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);

                if (ctx.isFloatExpr(receiver_expr)) {
                    const neg_val = ctx.wip.un(.fneg, receiver_val, "") catch return error.CompilationFailed;
                    const zero = (ctx.builder.floatConst(0.0) catch return error.CompilationFailed).toValue();
                    const is_neg = ctx.wip.fcmp(.normal, .olt, receiver_val, zero, "") catch return error.CompilationFailed;
                    return ctx.wip.select(.normal, is_neg, neg_val, receiver_val, "") catch return error.CompilationFailed;
                }

                const llvm_type = ctx.evaluator.getExprLlvmTypeFromExpr(ctx.builder, receiver_expr) catch return error.CompilationFailed;
                const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();
                const neg_val = ctx.wip.bin(.sub, zero, receiver_val, "") catch return error.CompilationFailed;
                const is_neg = ctx.wip.icmp(.slt, receiver_val, zero, "") catch return error.CompilationFailed;
                return ctx.wip.select(.normal, is_neg, neg_val, receiver_val, "") catch return error.CompilationFailed;
            }

            if (field_ident == idents.negate) {
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
            if (field_ident == idents.abs_diff) {
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

                        const diff = if (ctx.isFloatExpr(lhs_expr))
                            ctx.wip.bin(.fsub, lhs_val, rhs_val, "") catch return error.CompilationFailed
                        else
                            ctx.wip.bin(.sub, lhs_val, rhs_val, "") catch return error.CompilationFailed;

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
            if (field_ident == idents.f32 or field_ident == idents.f64 or field_ident == idents.dec) {
                const receiver_expr = ctx.module_env.store.getExpr(dot.receiver);

                if (ctx.isFloatExpr(receiver_expr)) {
                    return try ctx.emitExpr(dot.receiver);
                }

                if (receiver_expr == .e_dec_small) {
                    const dec = receiver_expr.e_dec_small;
                    const value: f64 = @as(f64, @floatFromInt(dec.value.numerator)) / std.math.pow(f64, 10.0, @as(f64, @floatFromInt(dec.value.denominator_power_of_ten)));
                    return (ctx.builder.doubleConst(value) catch return error.CompilationFailed).toValue();
                }

                return try ctx.emitExpr(dot.receiver);
            }

            if (field_ident == idents.i8 or field_ident == idents.u8 or
                field_ident == idents.i16 or field_ident == idents.u16 or
                field_ident == idents.i32 or field_ident == idents.u32 or
                field_ident == idents.i64 or field_ident == idents.u64 or
                field_ident == idents.i128 or field_ident == idents.u128)
            {
                return try ctx.emitExpr(dot.receiver);
            }

            // Try record field access (handles nested dot access chains on record literals)
            if (ctx.resolveRecordFieldChain(dot.receiver, field_ident)) |field_expr_idx| {
                return ctx.emitExpr(field_expr_idx);
            }

            return error.UnsupportedType;
        }

        /// Get the layout for an expression, checking local_layouts for lookups
        fn getExprLayout(ctx: *ExprContext, expr_idx: CIR.Expr.Idx) layout.Idx {
            const expr = ctx.module_env.store.getExpr(expr_idx);
            return switch (expr) {
                .e_lookup_local => |lookup| ctx.local_layouts.get(lookup.pattern_idx) orelse .i64,
                .e_binop => |binop| ctx.getExprLayout(binop.lhs),
                .e_unary_minus => |unary| ctx.getExprLayout(unary.expr),
                else => ctx.evaluator.getExprOutputLayout(ctx.module_env, expr_idx),
            };
        }

        /// Convert a numeric value to match the target layout if needed.
        /// This mirrors Rust mono's make_num_literal which converts int values to floats
        /// when the layout says Float (see crates/compiler/mono/src/ir/literal.rs:105-110)
        fn convertNumericToLayout(ctx: *ExprContext, val: LlvmBuilder.Value, target_layout: layout.Idx) ExprError!LlvmBuilder.Value {
            const val_type = val.typeOfWip(ctx.wip);

            // Determine target LLVM type from layout
            const target_type: LlvmBuilder.Type = switch (target_layout) {
                .f32 => .float,
                .f64 => .double,
                .i128, .u128 => .i128,
                else => return val, // No conversion needed for other types
            };

            // If already the correct type, return as-is
            if (val_type == target_type) return val;

            // Convert integer to larger integer or float
            if (val_type == .i8 or val_type == .i16 or val_type == .i32 or val_type == .i64 or val_type == .i128) {
                // For float targets, convert int to float
                if (target_type == .float or target_type == .double) {
                    return ctx.wip.conv(.signed, val, target_type, "") catch return error.CompilationFailed;
                }
                // For int targets (like i128), use appropriate extension based on target signedness
                if (target_layout.isSigned()) {
                    return ctx.wip.conv(.signed, val, target_type, "") catch return error.CompilationFailed;
                } else {
                    return ctx.wip.conv(.unsigned, val, target_type, "") catch return error.CompilationFailed;
                }
            }

            // Convert float to float (f32 <-> f64)
            if (val_type == .float or val_type == .double) {
                return ctx.wip.conv(.unneeded, val, target_type, "") catch return error.CompilationFailed;
            }

            return val;
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
                .e_typed_frac => |typed_frac| typed_frac.type_name == ctx.module_env.idents.dec,
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

        /// Create an LLVM i1 constant from a Zig bool
        fn boolConst(ctx: *ExprContext, value: bool) ExprError!LlvmBuilder.Value {
            return (ctx.builder.intConst(.i1, @intFromBool(value)) catch return error.CompilationFailed).toValue();
        }

        /// Emit string comparison operation
        /// String comparisons are not supported in the LLVM evaluator - use interpreter instead
        fn emitStringComparison(_: *ExprContext, _: CIR.Expr.Binop, _: CIR.Expr, _: CIR.Expr) ExprError!LlvmBuilder.Value {
            return error.UnsupportedType;
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
                    if (typed_frac.type_name != ctx.module_env.idents.dec) break :blk null;
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

        /// Resolves a chain of dot accesses on record literals, returning the final field's expression index.
        /// For example: {a: {b: 42}}.a.b resolves through the chain to find the expression for 42.
        fn resolveRecordFieldChain(ctx: *ExprContext, expr_idx: CIR.Expr.Idx, field_ident: base.Ident.Idx) ?CIR.Expr.Idx {
            const expr = ctx.module_env.store.getExpr(expr_idx);

            switch (expr) {
                .e_record => |record| {
                    // Base case: we have a record, find the field
                    const field_indices = ctx.module_env.store.sliceRecordFields(record.fields);
                    for (field_indices) |field_idx| {
                        const field = ctx.module_env.store.getRecordField(field_idx);
                        if (field.name == field_ident) {
                            return field.value;
                        }
                    }
                    return null;
                },
                .e_dot_access => |dot| {
                    // Recursive case: first resolve the receiver to get intermediate record
                    const intermediate = ctx.resolveRecordFieldChain(dot.receiver, dot.field_name) orelse return null;
                    // Then find our field in that intermediate result
                    return ctx.resolveRecordFieldChain(intermediate, field_ident);
                },
                else => return null,
            }
        }
    };

    /// Generate bitcode from source code string
    /// This does the full pipeline: parse → canonicalize → type check → generate bitcode
    /// The caller is responsible for compiling (llvm_compile.compileToObject) and
    /// executing (llvm_execute.executeAndFormat) the bitcode.
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

    /// Scope for tracking local variable types during layout resolution
    const LocalScope = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, layout.Idx);

    /// Get layout from a type annotation (e.g., `a : U64` -> .u64)
    fn getLayoutFromAnnotation(_: *LlvmEvaluator, module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) ?layout.Idx {
        const annotation = module_env.store.getAnnotation(anno_idx);
        const type_anno = module_env.store.getTypeAnno(annotation.anno);

        // Handle simple type lookups like U64, I32, F64, etc.
        if (type_anno == .lookup) {
            const lookup = type_anno.lookup;
            const idents = module_env.idents;
            if (lookup.name == idents.i8) return .i8;
            if (lookup.name == idents.u8) return .u8;
            if (lookup.name == idents.i16) return .i16;
            if (lookup.name == idents.u16) return .u16;
            if (lookup.name == idents.i32) return .i32;
            if (lookup.name == idents.u32) return .u32;
            if (lookup.name == idents.i64) return .i64;
            if (lookup.name == idents.u64) return .u64;
            if (lookup.name == idents.i128) return .i128;
            if (lookup.name == idents.u128) return .u128;
            if (lookup.name == idents.f32) return .f32;
            if (lookup.name == idents.f64) return .f64;
            if (lookup.name == idents.dec) return .dec;
            if (lookup.name == idents.bool_type) return .bool;
        }

        return null; // Unknown annotation type
    }

    /// Get the output layout for JIT execution from a CIR expression index
    /// This captures signedness which LLVM types don't distinguish
    fn getExprOutputLayout(self: *LlvmEvaluator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) layout.Idx {
        const expr = module_env.store.getExpr(expr_idx);
        return self.getExprOutputLayoutWithScope(module_env, expr, null);
    }

    /// Get the output layout with an optional local scope for resolving variable types
    fn getExprOutputLayoutWithScope(self: *LlvmEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, local_scope: ?*const LocalScope) layout.Idx {
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
                const type_ident = typed_int.type_name;
                const idents = module_env.idents;
                if (type_ident == idents.i8) return .i8;
                if (type_ident == idents.u8) return .u8;
                if (type_ident == idents.i16) return .i16;
                if (type_ident == idents.u16) return .u16;
                if (type_ident == idents.i32) return .i32;
                if (type_ident == idents.u32) return .u32;
                if (type_ident == idents.i64) return .i64;
                if (type_ident == idents.u64) return .u64;
                if (type_ident == idents.i128) return .i128;
                if (type_ident == idents.u128) return .u128;
                if (type_ident == idents.dec) return .dec;
                if (type_ident == idents.f32) return .f32;
                if (type_ident == idents.f64) return .f64;
                return .i64;
            },
            .e_typed_frac => |typed_frac| {
                const type_ident = typed_frac.type_name;
                const idents = module_env.idents;
                if (type_ident == idents.dec) return .dec;
                if (type_ident == idents.f32) return .f32;
                if (type_ident == idents.f64) return .f64;
                return .f64;
            },
            .e_binop => |binop| switch (binop.op) {
                // Comparison operators return Bool
                .lt, .le, .gt, .ge, .eq, .ne => .bool,
                // Logical operators return Bool
                .@"and", .@"or" => .bool,
                // Arithmetic operators inherit from LHS
                else => self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(binop.lhs), local_scope),
            },
            .e_unary_minus => |unary| self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(unary.expr), local_scope),
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
                return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(if_expr.final_else), local_scope);
            },
            .e_block => |blk| {
                // Build a scope from the block's declarations
                var scope = LocalScope{};
                const stmts = module_env.store.sliceStatements(blk.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |decl| {
                            // First try to get layout from type annotation
                            const decl_layout = if (decl.anno) |anno_idx|
                                self.getLayoutFromAnnotation(module_env, anno_idx) orelse
                                    self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(decl.expr), &scope)
                            else
                                self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(decl.expr), &scope);
                            scope.put(self.allocator, decl.pattern, decl_layout) catch {};
                        },
                        .s_var => |var_stmt| {
                            // First try to get layout from type annotation
                            const var_layout = if (var_stmt.anno) |anno_idx|
                                self.getLayoutFromAnnotation(module_env, anno_idx) orelse
                                    self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(var_stmt.expr), &scope)
                            else
                                self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(var_stmt.expr), &scope);
                            scope.put(self.allocator, var_stmt.pattern_idx, var_layout) catch {};
                        },
                        else => {},
                    }
                }
                defer scope.deinit(self.allocator);
                return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(blk.final_expr), &scope);
            },
            .e_lookup_local => |lookup| {
                // Look up the layout from the scope if available
                if (local_scope) |scope| {
                    if (scope.get(lookup.pattern_idx)) |found_layout| {
                        return found_layout;
                    }
                }
                return .i64; // Default if not found
            },
            .e_str_segment, .e_str => .str,
            .e_call => |call| {
                const func_expr = module_env.store.getExpr(call.func);
                if (func_expr == .e_lookup_external) {
                    const lookup = func_expr.e_lookup_external;
                    const func_ident = lookup.ident_idx;
                    const idents = module_env.idents;
                    // Bool.not returns Bool
                    if (func_ident == idents.not) {
                        return .bool;
                    }
                    // Num.abs and Num.negate return the same type as their argument
                    if (func_ident == idents.abs or func_ident == idents.negate) {
                        const args = module_env.store.sliceExpr(call.args);
                        if (args.len == 1) {
                            return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(args[0]), local_scope);
                        }
                    }
                }
                // Lambda call - get layout from lambda body
                if (func_expr == .e_lambda) {
                    const lambda = func_expr.e_lambda;
                    const body_expr = module_env.store.getExpr(lambda.body);
                    if (body_expr == .e_lookup_local) {
                        const lookup = body_expr.e_lookup_local;
                        const params = module_env.store.slicePatterns(lambda.args);
                        const args = module_env.store.sliceExpr(call.args);
                        for (params, 0..) |param_idx, i| {
                            if (param_idx == lookup.pattern_idx and i < args.len) {
                                return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(args[i]), local_scope);
                            }
                        }
                    }
                    return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(lambda.body), local_scope);
                }
                return .i64;
            },
            .e_dot_access => |dot| {
                const field_ident = dot.field_name;
                const idents = module_env.idents;
                // Method calls like (-3.14).abs() return the same type as the receiver
                if (field_ident == idents.abs or field_ident == idents.negate or field_ident == idents.abs_diff) {
                    return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(dot.receiver), local_scope);
                }
                // Type annotations via dot access
                if (field_ident == idents.i8) return .i8;
                if (field_ident == idents.u8) return .u8;
                if (field_ident == idents.i16) return .i16;
                if (field_ident == idents.u16) return .u16;
                if (field_ident == idents.i32) return .i32;
                if (field_ident == idents.u32) return .u32;
                if (field_ident == idents.i64) return .i64;
                if (field_ident == idents.u64) return .u64;
                if (field_ident == idents.i128) return .i128;
                if (field_ident == idents.u128) return .u128;
                if (field_ident == idents.f32) return .f32;
                if (field_ident == idents.f64) return .f64;
                if (field_ident == idents.dec) return .dec;
                // Try record field access - get layout from the field value
                const receiver_expr = module_env.store.getExpr(dot.receiver);
                if (receiver_expr == .e_record) {
                    const record = receiver_expr.e_record;
                    const field_indices = module_env.store.sliceRecordFields(record.fields);
                    for (field_indices) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        if (field.name == field_ident) {
                            return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(field.value), local_scope);
                        }
                    }
                }
                return .i64;
            },
            .e_nominal_external => |nominal| {
                // Get layout from the backing expression (e.g., Bool.True -> True tag)
                return self.getExprOutputLayoutWithScope(module_env, module_env.store.getExpr(nominal.backing_expr), local_scope);
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
            .e_zero_argument_tag => .i1, // All 2-tag unions with no payloads use i1
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
