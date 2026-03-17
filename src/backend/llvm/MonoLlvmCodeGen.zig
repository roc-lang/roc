//! LIR to LLVM Code Generator
//!
//! This module generates LLVM IR from LIR (Low-level IR) expressions.
//! It uses Zig's std.zig.llvm.Builder for IR generation.
//!
//! Pipeline position:
//! ```
//! CIR -> MIR -> LIR -> MonoLlvmCodeGen -> LLVM Bitcode -> Native Code
//! ```
//!
//! Key properties:
//! - Consumes the same LIR as the dev backend
//! - Generates LLVM IR via Zig's llvm.Builder
//! - Produces bitcode that can be compiled to native code via LLVM

const std = @import("std");
const builtin = @import("builtin");
const layout = @import("layout");
const lir = @import("lir");

const LlvmBuilder = std.zig.llvm.Builder;

const LirExprStore = lir.LirExprStore;
const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const Symbol = lir.Symbol;
const LirProc = lir.LirProc;
const CFStmtId = lir.CFStmtId;

const Allocator = std.mem.Allocator;
const roc_str_abi_size = 3 * @sizeOf(usize);
const from_utf8_try_size = std.mem.alignForward(usize, 8 + roc_str_abi_size + 2, @alignOf(usize));
const from_utf8_try_string_offset = 8;
const from_utf8_try_is_ok_offset = 8 + roc_str_abi_size;
const from_utf8_try_problem_code_offset = from_utf8_try_is_ok_offset + 1;

const ClosureRepresentation = union(enum) {
    unwrapped_capture: struct {
        capture_layout: layout.Idx,
    },
    struct_captures: struct {
        captures: lir.LIR.LirCaptureSpan,
        struct_layout: layout.Idx,
    },
    enum_dispatch: struct {
        tag: u16,
        lambda_set: void,
    },
    union_repr: struct {
        captures: lir.LIR.LirCaptureSpan,
    },
    direct_call: void,
};

/// Get the LLVM target triple for the current platform.
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
        .windows => "-w64-windows",
        .macos => "-apple-macosx13.0.0",
        .ios => "-apple-ios",
        .linux => "-unknown-linux",
        .freebsd => "-unknown-freebsd",
        .openbsd => "-unknown-openbsd",
        .netbsd => "-unknown-netbsd",
        .freestanding => "-unknown-unknown",
        .wasi => "-wasi",
        else => "-unknown-unknown",
    };

    const abi = switch (builtin.os.tag) {
        .windows => "-gnu",
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
        else => "",
    };

    return arch ++ vendor_os ++ abi;
}

/// LLVM code generator for Mono IR expressions
pub const MonoLlvmCodeGen = struct {
    allocator: Allocator,

    /// The LIR store containing expressions to compile
    store: *const LirExprStore,

    /// Map from Symbol to LLVM value
    symbol_values: std.AutoHashMap(u64, LlvmBuilder.Value),

    /// Registry of compiled procedures (symbol -> function index)
    proc_registry: std.AutoHashMap(u64, LlvmBuilder.Function.Index),

    /// Join point blocks (join point id -> block index)
    join_points: std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index),

    /// Join point parameters (join point id -> parameter patterns)
    join_point_params: std.AutoHashMap(u32, []LirPatternId),

    /// Join point parameter allocas for SSA-correct join point handling.
    /// Maps join point id to an array of alloca pointers, one per parameter.
    /// Jump handlers store values here; join body loads before executing.
    join_param_allocas: std.AutoHashMap(u32, []LlvmBuilder.Value),

    /// Join point parameter LLVM types (recorded at first jump, used for correct loads).
    join_param_types: std.AutoHashMap(u32, []LlvmBuilder.Type),

    /// Current LLVM builder (set during code generation)
    builder: ?*LlvmBuilder = null,

    /// Current WIP function (set during code generation)
    wip: ?*LlvmBuilder.WipFunction = null,

    /// The roc_ops argument passed to roc_eval (second parameter)
    roc_ops_arg: ?LlvmBuilder.Value = null,

    /// Cache of declared builtin functions from builtins.bc.
    /// Maps builtin name → LLVM function index. Since builtins.bc is linked
    /// into the LLVM module at compile time, we declare them as external
    /// functions and call them directly — no function pointers or inttoptr.
    builtin_functions: std.StringHashMap(LlvmBuilder.Function.Index),

    /// Layout store for resolving composite type layouts (records, tuples).
    /// Set by the evaluator before calling generateCode.
    layout_store: ?*const layout.Store = null,

    /// Output pointer for the top-level expression result.
    /// Set during generateCode so that string/composite generators can
    /// store data directly to the output buffer, avoiding LLVM constant
    /// pool references to .rodata (which we don't extract from the object file).
    out_ptr: ?LlvmBuilder.Value = null,

    /// Function-level output pointer for early returns. Unlike `out_ptr` which
    /// gets nulled by intermediate expression handlers (if-then-else, when, block),
    /// this always points to the function's output destination.
    fn_out_ptr: ?LlvmBuilder.Value = null,

    /// Result layout for the top-level expression (needed by early_return).
    result_layout: ?layout.Idx = null,

    /// Allocas for mutable variables inside loop bodies.
    /// When a symbol is reassigned inside a for_loop or while_loop body,
    /// its value is stored to an alloca so that post-loop code can load
    /// the final value (avoiding SSA domination issues).
    /// Stores both the alloca pointer and the element type for correct loads.
    loop_var_allocas: std.AutoHashMap(u64, LoopVarAlloca),

    /// Stack of active loop exit blocks for lowering `break`.
    loop_exit_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index),

    /// Allocas backing LIR mutable cells.
    cell_allocas: std.AutoHashMap(u64, LoopVarAlloca),

    /// Cache of compiled lambda LLVM functions.
    /// Key = (expr_id << 32) | ret_layout — same strategy as dev backend.
    /// Prevents recompiling the same lambda body.
    compiled_lambdas: std.AutoHashMap(u64, LlvmBuilder.Function.Index),

    /// Tracks closure metadata for symbols bound to lambda/closure values.
    /// Needed because symbol_values only stores LlvmBuilder.Value (loses dispatch info).
    closure_bindings: std.AutoHashMap(u64, ClosureMeta),

    /// Debug breadcrumbs for pinpointing the last expression/op seen before
    /// a `CompilationFailed` bubbles out of codegen.
    last_expr_id: LirExprId = .none,
    last_low_level_name: []const u8 = "none",

    const LoopVarAlloca = struct {
        alloca_ptr: LlvmBuilder.Value,
        elem_type: LlvmBuilder.Type,
    };

    const ClosureMeta = struct {
        representation: ClosureRepresentation,
        lambda: LirExprId,
        captures: lir.LIR.LirCaptureSpan,
    };

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        result_layout: layout.Idx,
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    /// Errors that can occur during code generation
    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
    };

    /// Initialize the code generator
    pub fn init(
        allocator: Allocator,
        store: *const LirExprStore,
    ) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .store = store,
            .symbol_values = std.AutoHashMap(u64, LlvmBuilder.Value).init(allocator),
            .proc_registry = std.AutoHashMap(u64, LlvmBuilder.Function.Index).init(allocator),
            .join_points = std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index).init(allocator),
            .join_point_params = std.AutoHashMap(u32, []LirPatternId).init(allocator),
            .join_param_allocas = std.AutoHashMap(u32, []LlvmBuilder.Value).init(allocator),
            .join_param_types = std.AutoHashMap(u32, []LlvmBuilder.Type).init(allocator),
            .loop_var_allocas = std.AutoHashMap(u64, LoopVarAlloca).init(allocator),
            .loop_exit_blocks = .empty,
            .cell_allocas = std.AutoHashMap(u64, LoopVarAlloca).init(allocator),
            .compiled_lambdas = std.AutoHashMap(u64, LlvmBuilder.Function.Index).init(allocator),
            .closure_bindings = std.AutoHashMap(u64, ClosureMeta).init(allocator),
            .builtin_functions = std.StringHashMap(LlvmBuilder.Function.Index).init(allocator),
        };
    }

    /// Clean up resources
    pub fn deinit(self: *MonoLlvmCodeGen) void {
        self.symbol_values.deinit();
        self.proc_registry.deinit();
        self.join_points.deinit();
        // Free allocated param slices
        var it = self.join_point_params.valueIterator();
        while (it.next()) |params| {
            self.allocator.free(params.*);
        }
        self.join_point_params.deinit();
        // Free allocated alloca slices
        var it2 = self.join_param_allocas.valueIterator();
        while (it2.next()) |allocas| {
            self.allocator.free(allocas.*);
        }
        self.join_param_allocas.deinit();
        // Free allocated type slices
        var it3 = self.join_param_types.valueIterator();
        while (it3.next()) |types| {
            self.allocator.free(types.*);
        }
        self.join_param_types.deinit();
        self.loop_var_allocas.deinit();
        self.loop_exit_blocks.deinit(self.allocator);
        self.cell_allocas.deinit();
        self.compiled_lambdas.deinit();
        self.closure_bindings.deinit();
        self.builtin_functions.deinit();
    }

    /// Reset the code generator for a new expression
    pub fn reset(self: *MonoLlvmCodeGen) void {
        self.symbol_values.clearRetainingCapacity();
        self.proc_registry.clearRetainingCapacity();
        self.join_points.clearRetainingCapacity();
        // Free allocated param slices before clearing
        var it = self.join_point_params.valueIterator();
        while (it.next()) |params| {
            self.allocator.free(params.*);
        }
        self.join_point_params.clearRetainingCapacity();
        var it2 = self.join_param_allocas.valueIterator();
        while (it2.next()) |allocas| {
            self.allocator.free(allocas.*);
        }
        self.join_param_allocas.clearRetainingCapacity();
        var it3 = self.join_param_types.valueIterator();
        while (it3.next()) |types| {
            self.allocator.free(types.*);
        }
        self.join_param_types.clearRetainingCapacity();
        self.loop_var_allocas.clearRetainingCapacity();
        self.loop_exit_blocks.clearRetainingCapacity();
        self.cell_allocas.clearRetainingCapacity();
        self.compiled_lambdas.clearRetainingCapacity();
        self.closure_bindings.clearRetainingCapacity();
        self.builtin_functions.clearRetainingCapacity();
        self.last_expr_id = .none;
        self.last_low_level_name = "none";
    }

    /// Declare a builtin function from builtins.bc as an external LLVM function.
    /// builtins.bc is linked into the LLVM module during compilation, so these
    /// symbols are resolved at link time. Returns the cached function index.
    fn declareBuiltin(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        ret_type: LlvmBuilder.Type,
        param_types: []const LlvmBuilder.Type,
    ) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;

        if (self.builtin_functions.get(name)) |func_idx| {
            return func_idx;
        }

        const fn_type = builder.fnType(ret_type, param_types, .normal) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        self.builtin_functions.put(name, func) catch return error.OutOfMemory;
        return func;
    }

    /// Call a declared builtin function with the given arguments.
    fn callBuiltin(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        ret_type: LlvmBuilder.Type,
        param_types: []const LlvmBuilder.Type,
        args: []const LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const func = try self.declareBuiltin(name, ret_type, param_types);
        const fn_type = func.typeOf(builder);
        const callee = func.toValue(builder);
        return wip.call(.normal, .ccc, .none, fn_type, callee, args, "") catch return error.CompilationFailed;
    }

    /// Generate LLVM bitcode for a Mono IR expression
    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        expr_id: LirExprId,
        result_layout: layout.Idx,
    ) Error!BitcodeResult {
        // Create LLVM Builder
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_mono_eval",
            .target = &builtin.target,
            .triple = getLlvmTriple(),
        }) catch return error.OutOfMemory;
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        // Create the eval function: void roc_eval(<type>* out_ptr, roc_ops* ops_ptr)
        const ptr_type = builder.ptrType(.default) catch return error.OutOfMemory;
        const eval_fn_type = builder.fnType(.void, &.{ ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
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
            .strip = true,
        }) catch return error.OutOfMemory;
        defer wip.deinit();

        self.wip = &wip;
        defer self.wip = null;

        const entry_block = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry_block };

        // Get the output pointer and roc_ops pointer
        const out_ptr = wip.arg(0);
        self.out_ptr = out_ptr;
        self.fn_out_ptr = out_ptr;
        defer self.out_ptr = null;
        self.roc_ops_arg = wip.arg(1);
        defer self.roc_ops_arg = null;

        // Store result layout for early_return
        self.result_layout = result_layout;
        defer self.result_layout = null;

        // Compile all procedures now that the builder is available.
        // Must happen before generateExpr so that call sites can find procs.
        const procs = self.store.getProcs();
        if (procs.len > 0) {
            try self.compileAllProcs(procs);
        }

        // Generate LLVM IR for the expression
        const value = self.generateExpr(expr_id) catch |err| {
            if (builtin.mode == .Debug and err == error.CompilationFailed) {
                std.log.err(
                    "LLVM codegen failed near expr {d} ({s}), last low-level op={s}",
                    .{
                        @intFromEnum(self.last_expr_id),
                        @tagName(self.store.getExpr(self.last_expr_id)),
                        self.last_low_level_name,
                    },
                );
            }
            return err;
        };

        // Store the result to the output pointer.
        // Some generators (e.g., string literals) write directly to out_ptr and
        // return .none as a sentinel — skip the storage step for those.
        if (value == .none) {
            // Result already written to out_ptr by the generator.
        } else {

            // For scalar types, extend to the canonical size (i64 for ints, i128 for wide ints).
            // For composite types (records, tuples), store the struct directly.
            const is_scalar = switch (result_layout) {
                .bool,
                .i8,
                .i16,
                .i32,
                .i64,
                .u8,
                .u16,
                .u32,
                .u64,
                .i128,
                .u128,
                .dec,
                .f32,
                .f64,
                => true,
                else => false,
            };

            if (is_scalar) {
                const final_type: LlvmBuilder.Type = switch (result_layout) {
                    .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
                    .i128, .u128, .dec => .i128,
                    .f32 => .float,
                    .f64 => .double,
                    else => unreachable,
                };

                const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (result_layout) {
                    .i8, .i16, .i32, .i64, .i128 => .signed,
                    .bool, .u8, .u16, .u32, .u64, .u128 => .unsigned,
                    .f32, .f64, .dec => .unneeded,
                    else => .unneeded,
                };

                // Check actual generated value type vs expected store type.
                // value_type (from layoutToLlvmType) may not match the actual value if
                // the expression generates a narrower type (e.g., i64 when result is Dec/i128).
                const actual_value_type = value.typeOfWip(&wip);
                const store_value = if (actual_value_type == final_type)
                    value
                else conv: {
                    // For Dec result_layout, integer values need sign extension
                    const conv_sign: LlvmBuilder.Constant.Cast.Signedness = if (signedness == .unneeded and isIntType(actual_value_type))
                        .signed
                    else
                        signedness;
                    break :conv wip.conv(conv_sign, value, final_type, "") catch return error.CompilationFailed;
                };

                const alignment = LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
                    .i64 => 8,
                    .i128 => 16,
                    .float => 4,
                    .double => 8,
                    else => 0,
                });
                _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
            } else {
                // Composite type (record, tuple, tag_union, str, list, etc.)
                // For 24-byte types (str, list), decompose to individual field stores
                // to avoid aggregate store issues where only the first field is written.
                const is_24byte_struct = switch (result_layout) {
                    .str => true,
                    else => blk: {
                        if (self.layout_store) |ls2| {
                            const l = ls2.getLayout(result_layout);
                            break :blk (l.tag == .list or l.tag == .list_of_zst);
                        }
                        break :blk false;
                    },
                };
                if (is_24byte_struct) {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    for (0..3) |fi| {
                        const field_val = wip.extractValue(value, &.{@as(u32, @intCast(fi))}, "") catch return error.CompilationFailed;
                        const field_ptr = if (fi == 0) out_ptr else blk: {
                            const off = builder.intValue(.i32, @as(u32, @intCast(fi * 8))) catch return error.OutOfMemory;
                            break :blk wip.gep(.inbounds, .i8, out_ptr, &.{off}, "") catch return error.CompilationFailed;
                        };
                        _ = wip.store(.@"volatile", field_val, field_ptr, alignment) catch return error.CompilationFailed;
                    }
                } else {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    _ = wip.store(.normal, value, out_ptr, alignment) catch return error.CompilationFailed;
                }
            }
        } // end of value != .none else

        _ = wip.retVoid() catch return error.CompilationFailed;

        wip.finish() catch return error.CompilationFailed;

        // Serialize to bitcode
        const producer = LlvmBuilder.Producer{
            .name = "Roc Mono LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

        const bitcode = builder.toBitcode(self.allocator, producer) catch return error.CompilationFailed;

        return BitcodeResult{
            .bitcode = bitcode,
            .result_layout = result_layout,
            .allocator = self.allocator,
        };
    }

    /// Compile all procedures as LLVM functions.
    /// Mirrors dev backend's compileAllProcs: creates each proc as a callable
    /// LLVM function and registers it in proc_registry before compiling the body,
    /// so recursive calls within the body can find the function.
    pub fn compileAllProcs(self: *MonoLlvmCodeGen, procs: []const LirProc) Error!void {
        for (procs) |proc| {
            self.compileProc(proc) catch {
                // Skip procs that can't be compiled (e.g. OOM).
                // The proc won't be in proc_registry, so call sites will
                // hit unreachable if they try to invoke it.
                continue;
            };
        }
    }

    fn compileProc(self: *MonoLlvmCodeGen, proc: LirProc) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;

        // Build the LLVM function type from arg_layouts and ret_layout.
        // An extra ptr parameter is appended for roc_ops (hidden ABI argument)
        // so that proc bodies can call builtins like allocateWithRefcountC.
        const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        var param_types: std.ArrayList(LlvmBuilder.Type) = .{};
        defer param_types.deinit(self.allocator);
        for (arg_layouts) |arg_layout| {
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(arg_layout)) catch return error.OutOfMemory;
        }
        // Hidden roc_ops parameter at the end
        param_types.append(self.allocator, ptr_type) catch return error.OutOfMemory;

        const ret_type = try self.layoutToLlvmTypeFull(proc.ret_layout);
        const fn_type = builder.fnType(ret_type, param_types.items, .normal) catch return error.OutOfMemory;

        // Create a unique function name from the symbol
        const key: u64 = @bitCast(proc.name);
        var name_buf: [64]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_proc_{d}", .{key}) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;

        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        // Register in proc_registry BEFORE compiling body (for recursive calls)
        self.proc_registry.put(key, func) catch return error.OutOfMemory;
        errdefer _ = self.proc_registry.remove(key);

        // Save and restore outer wip state and roc_ops_arg
        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        self.out_ptr = null; // Procs don't have a top-level out_ptr
        self.fn_out_ptr = null;

        // Create a new WipFunction for this procedure
        var proc_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = func,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer proc_wip.deinit();

        self.wip = &proc_wip;

        const proc_entry = proc_wip.block(0, "entry") catch return error.OutOfMemory;
        proc_wip.cursor = .{ .block = proc_entry };

        // Bind parameters to argument values
        const params = self.store.getPatternSpan(proc.args);
        for (params, 0..) |param_id, i| {
            const arg_val = proc_wip.arg(@intCast(i));
            try self.bindPattern(param_id, arg_val);
        }

        // Set roc_ops_arg to the hidden last parameter
        self.roc_ops_arg = proc_wip.arg(@intCast(params.len));

        // Generate the body (control flow statements)
        self.generateStmt(proc.body) catch return error.CompilationFailed;

        proc_wip.finish() catch return error.CompilationFailed;
    }

    /// Convert layout to LLVM type (scalar types only — composite layouts fall through to i64)
    fn layoutToLlvmType(result_layout: layout.Idx) LlvmBuilder.Type {
        return switch (result_layout) {
            .zst => .i8,
            .bool => .i1,
            .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            else => .i64,
        };
    }

    fn layoutToLlvmTypeWithOptions(
        self: *MonoLlvmCodeGen,
        result_layout: layout.Idx,
        bool_in_memory: bool,
    ) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;

        return switch (result_layout) {
            .zst => .i8,
            .bool => if (bool_in_memory) .i8 else .i1,
            .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            .str => blk: {
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                break :blk builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            },
            else => blk: {
                const ls = self.layout_store orelse break :blk .i64;
                const stored_layout = ls.getLayout(result_layout);
                switch (stored_layout.tag) {
                    .list, .list_of_zst => {
                        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                        break :blk builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
                    },
                    .struct_ => {
                        const struct_data = ls.getStructData(stored_layout.data.struct_.idx);
                        const fields = struct_data.getFields();
                        if (fields.count == 0) break :blk .i8;

                        var field_types: [32]LlvmBuilder.Type = undefined;
                        for (0..fields.count) |field_idx| {
                            field_types[field_idx] = try self.layoutToLlvmTypeWithOptions(
                                ls.getStructFieldLayout(stored_layout.data.struct_.idx, @intCast(field_idx)),
                                true,
                            );
                        }
                        break :blk builder.structType(.normal, field_types[0..fields.count]) catch return error.CompilationFailed;
                    },
                    .tag_union => {
                        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                        const variants = ls.getTagUnionVariants(tu_data);
                        for (0..variants.len) |variant_idx| {
                            if (variants.get(@intCast(variant_idx)).payload_layout != .zst) {
                                break :blk builder.ptrType(.default) catch return error.CompilationFailed;
                            }
                        }
                        break :blk .i64;
                    },
                    else => break :blk .i64,
                }
            },
        };
    }

    /// Convert layout to LLVM type, handling str/list/struct layouts correctly for
    /// values passed between generated functions.
    fn layoutToLlvmTypeFull(self: *MonoLlvmCodeGen, result_layout: layout.Idx) Error!LlvmBuilder.Type {
        return self.layoutToLlvmTypeWithOptions(result_layout, false);
    }

    // Control Flow Statement generation (mirrors dev backend's generateStmt)

    fn generateStmt(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!void {
        const stmt = self.store.getCFStmt(stmt_id);

        switch (stmt) {
            .let_stmt => |let_s| {
                const val = try self.generateExpr(let_s.value);
                try self.bindPattern(let_s.pattern, val);
                try self.generateStmt(let_s.next);
            },
            .ret => |r| {
                const wip = self.wip orelse return error.CompilationFailed;
                const val = try self.generateExpr(r.value);
                _ = wip.ret(val) catch return error.CompilationFailed;
            },
            .join => |j| {
                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;

                // Store join point parameters for rebinding on jumps
                const jp_key = @intFromEnum(j.id);
                const params = self.store.getPatternSpan(j.params);
                const params_copy = self.allocator.dupe(LirPatternId, params) catch return error.OutOfMemory;
                self.join_point_params.put(jp_key, params_copy) catch return error.OutOfMemory;

                // Create allocas for each join point parameter so that jumps from
                // different predecessor blocks can store values SSA-correctly.
                // The join body loads from these allocas before executing.
                if (params.len > 0) {
                    const allocas = self.allocator.alloc(LlvmBuilder.Value, params.len) catch return error.OutOfMemory;
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
                    for (allocas) |*a| {
                        // Each param gets 24 bytes (3×i64), enough for str/list/record structs
                        a.* = wip.alloca(.normal, .i64, alloca_count, alignment, .default, "jp") catch return error.CompilationFailed;
                    }
                    self.join_param_allocas.put(jp_key, allocas) catch return error.OutOfMemory;

                    // Create type tracking array (default i64, updated at first jump)
                    const types = self.allocator.alloc(LlvmBuilder.Type, params.len) catch return error.OutOfMemory;
                    @memset(types, .i64);
                    self.join_param_types.put(jp_key, types) catch return error.OutOfMemory;

                    // Initialize allocas to 0 to avoid undef
                    const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                    for (allocas) |a| {
                        _ = wip.store(.normal, zero, a, alignment) catch return error.CompilationFailed;
                    }
                }

                // Create a block for the join point body
                const join_block = wip.block(2, "join") catch return error.CompilationFailed;
                self.join_points.put(jp_key, join_block) catch return error.OutOfMemory;

                // Generate the remainder first (code that jumps TO join point)
                try self.generateStmt(j.remainder);

                // Now generate the join point body: load params from allocas first
                wip.cursor = .{ .block = join_block };
                if (self.join_param_allocas.get(jp_key)) |allocas| {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    const types = self.join_param_types.get(jp_key);
                    for (params, allocas, 0..) |param_id, alloca_ptr, i| {
                        const load_type: LlvmBuilder.Type = if (types) |ts| ts[i] else .i64;
                        const loaded = wip.load(.normal, load_type, alloca_ptr, alignment, "") catch return error.CompilationFailed;
                        try self.bindPattern(param_id, loaded);
                    }
                }
                try self.generateStmt(j.body);
            },
            .jump => |jmp| {
                const wip = self.wip orelse return error.CompilationFailed;
                const jp_key = @intFromEnum(jmp.target);

                // Null out_ptr so sub-expressions produce SSA values (not write to out_ptr)
                const saved_out_ptr = self.out_ptr;
                self.out_ptr = null;
                defer self.out_ptr = saved_out_ptr;

                // Evaluate all arguments and store to join point allocas
                const args = self.store.getExprSpan(jmp.args);
                const allocas = self.join_param_allocas.get(jp_key);
                const types = self.join_param_types.get(jp_key);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                for (args, 0..) |arg_id, i| {
                    const val = try self.generateExpr(arg_id);
                    if (allocas) |a| {
                        if (i < a.len) {
                            _ = wip.store(.normal, val, a[i], alignment) catch return error.CompilationFailed;
                            // Record the actual LLVM type for correct loading later
                            if (types) |ts| {
                                if (i < ts.len) {
                                    ts[i] = val.typeOfWip(wip);
                                }
                            }
                        }
                    }
                }

                // Branch to the join point block
                if (self.join_points.get(jp_key)) |join_block| {
                    _ = wip.br(join_block) catch return error.CompilationFailed;
                } else {
                    return error.CompilationFailed;
                }
            },
            .switch_stmt => |sw| {
                try self.generateSwitchStmt(sw);
            },
            .expr_stmt => |e| {
                _ = try self.generateExpr(e.value);
                try self.generateStmt(e.next);
            },
            .match_stmt => |ms| {
                // Pattern match statement (when in tail position of a proc)
                // Similar to match_expr but each branch is a statement, not an expression.
                const scrutinee = try self.generateExpr(ms.value);
                const branches = self.store.getCFMatchBranches(ms.branches);
                std.debug.assert(branches.len != 0);

                for (branches, 0..) |branch, i| {
                    const pattern = self.store.getPattern(branch.pattern);
                    const is_last = (i == branches.len - 1);

                    switch (pattern) {
                        .wildcard, .bind => {
                            if (pattern == .bind) {
                                const bind = pattern.bind;
                                const symbol_key: u64 = @bitCast(bind.symbol);
                                self.symbol_values.put(symbol_key, scrutinee) catch return error.OutOfMemory;
                            }
                            try self.generateStmt(branch.body);
                            break;
                        },
                        .int_literal => |int_pat| {
                            const wip = self.wip orelse return error.CompilationFailed;
                            const builder = self.builder orelse return error.CompilationFailed;
                            const pat_type = layoutToLlvmType(int_pat.layout_idx);
                            const pat_val = builder.intValue(pat_type, @as(u64, @truncate(@as(u128, @bitCast(int_pat.value))))) catch return error.OutOfMemory;
                            const cmp_scrutinee = if (scrutinee.typeOfWip(wip) == pat_type)
                                scrutinee
                            else
                                wip.conv(.unsigned, scrutinee, pat_type, "") catch return error.CompilationFailed;
                            const cmp = wip.icmp(.eq, cmp_scrutinee, pat_val, "") catch return error.OutOfMemory;

                            if (is_last) {
                                try self.generateStmt(branch.body);
                            } else {
                                const then_block = wip.block(1, "match_then") catch return error.OutOfMemory;
                                const else_block = wip.block(1, "match_else") catch return error.OutOfMemory;
                                _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.CompilationFailed;
                                wip.cursor = .{ .block = then_block };
                                try self.generateStmt(branch.body);
                                wip.cursor = .{ .block = else_block };
                            }
                        },
                        else => {
                            // For other patterns, just generate the branch body (best effort)
                            try self.generateStmt(branch.body);
                            break;
                        },
                    }
                }
            },
        }
    }

    fn generateSwitchStmt(self: *MonoLlvmCodeGen, sw: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const cond_val = try self.generateExpr(sw.cond);
        const branches = self.store.getCFSwitchBranches(sw.branches);

        if (branches.len == 0) {
            // No branches, just generate the default
            try self.generateStmt(sw.default_branch);
            return;
        }

        // Create blocks for each branch and the default.
        // Each block has incoming=1 from the switch instruction.
        const default_block = wip.block(1, "switch_default") catch return error.CompilationFailed;

        var switch_inst = wip.@"switch"(cond_val, default_block, @intCast(branches.len), .none) catch return error.CompilationFailed;

        var branch_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index) = .{};
        defer branch_blocks.deinit(self.allocator);

        for (branches) |branch| {
            const branch_block = wip.block(1, "switch_case") catch return error.CompilationFailed;
            branch_blocks.append(self.allocator, branch_block) catch return error.OutOfMemory;
            const case_val = builder.intConst(cond_val.typeOfWip(wip), branch.value) catch return error.OutOfMemory;
            switch_inst.addCase(case_val, branch_block, wip) catch return error.CompilationFailed;
        }

        switch_inst.finish(wip);

        // Generate code for each branch
        for (branches, branch_blocks.items) |branch, branch_block| {
            wip.cursor = .{ .block = branch_block };
            try self.generateStmt(branch.body);
        }

        // Generate default branch
        wip.cursor = .{ .block = default_block };
        try self.generateStmt(sw.default_branch);
    }

    // Expression generation

    /// Generate LLVM IR for an expression
    fn generateExpr(self: *MonoLlvmCodeGen, expr_id: LirExprId) Error!LlvmBuilder.Value {
        self.last_expr_id = expr_id;
        const expr = self.store.getExpr(expr_id);

        return switch (expr) {
            // Literals
            .i64_literal => |val| self.emitI64(val.value),
            .i128_literal => |val| self.emitI128(val.value),
            .f64_literal => |val| self.emitF64(val),
            .f32_literal => |val| self.emitF32(val),
            .dec_literal => |val| self.emitI128(val),
            .bool_literal => |val| self.emitBool(val),

            // Lookups
            .lookup => |lookup| self.generateLookup(lookup.symbol, lookup.layout_idx),
            .cell_load => |load| self.generateCellLoad(load.cell, load.layout_idx),

            // Control flow
            .if_then_else => |ite| self.generateIfThenElse(ite),

            // Blocks
            .block => |block| self.generateBlock(block),

            // Function calls and lambdas
            .call => |call| self.generateCall(call),
            .lambda => |lambda| self.generateLambdaExpr(lambda, expr_id),

            // Structs
            .struct_ => |struct_expr| self.generateStruct(struct_expr),
            .struct_access => |access| self.generateStructAccess(access),

            // Tag unions
            .zero_arg_tag => |zat| self.generateZeroArgTag(zat),
            .tag => |t| self.generateTagWithPayload(t),
            .discriminant_switch => |ds| self.generateDiscriminantSwitch(ds),

            // Strings
            .str_literal => |str_idx| self.generateStrLiteral(str_idx),
            .int_to_str => |its| self.generateIntToStr(its),
            .float_to_str => |fts| self.generateFloatToStr(fts),
            .dec_to_str => |dts| self.generateDecToStr(dts),
            .str_escape_and_quote => |seq| self.generateStrEscapeAndQuote(seq),

            // Nominal wrappers are transparent
            .nominal => |nom| self.generateExpr(nom.backing_expr),

            // Pattern matching
            .match_expr => |m| self.generateMatchExpr(m),

            // Debug and expect — just evaluate the inner expression
            .dbg => |d| self.generateExpr(d.expr),
            .expect => |e| self.generateExpr(e.body),

            // Lists
            .empty_list => self.generateEmptyList(),
            .list => |l| self.generateList(l),

            // Low-level builtins
            .low_level => |ll| self.generateLowLevel(ll),

            // Early return (? operator)
            .early_return => |er| self.generateEarlyReturn(er),

            // Runtime error (unreachable) — emit LLVM unreachable
            .runtime_error => |re| self.generateRuntimeError(re.ret_layout),
            .crash => |c| self.generateRuntimeError(c.ret_layout),

            // Reference counting — no-ops in the evaluator (short-lived memory)
            .incref => |inc| {
                _ = try self.generateExpr(inc.value);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .decref => |dec_rc| {
                _ = try self.generateExpr(dec_rc.value);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .free => |f| {
                _ = try self.generateExpr(f.value);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },

            // Loops
            .while_loop => |wl| self.generateWhileLoop(wl),
            .for_loop => |fl| self.generateForLoop(fl),
            .break_expr => self.generateBreakExpr(),

            // These should never reach LLVM codegen:
            // str_concat is lowered to low_level ops before codegen
            // hosted_call is not used in the evaluator
            .str_concat,
            .hosted_call,
            => unreachable,

            .tag_payload_access => |tpa| self.generateTagPayloadAccess(tpa),
        };
    }

    fn emitI64(self: *MonoLlvmCodeGen, val: i64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i64, @as(u64, @bitCast(val))) catch return error.OutOfMemory).toValue();
    }

    fn emitI128(self: *MonoLlvmCodeGen, val: i128) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i128, val) catch return error.OutOfMemory).toValue();
    }

    fn emitF64(self: *MonoLlvmCodeGen, val: f64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.doubleConst(val) catch return error.OutOfMemory).toValue();
    }

    fn emitF32(self: *MonoLlvmCodeGen, val: f32) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.floatConst(val) catch return error.OutOfMemory).toValue();
    }

    fn emitBool(self: *MonoLlvmCodeGen, val: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i1, @intFromBool(val)) catch return error.OutOfMemory).toValue();
    }

    fn generateLookup(self: *MonoLlvmCodeGen, symbol: Symbol, _: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u64 = @bitCast(symbol);

        if (self.cell_allocas.get(symbol_key)) |cell_alloca| {
            const wip = self.wip orelse return error.CompilationFailed;
            return wip.load(.normal, cell_alloca.elem_type, cell_alloca.alloca_ptr, LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(cell_alloca.elem_type), 1))), "") catch return error.CompilationFailed;
        }

        // Check if we have a value for this symbol
        if (self.symbol_values.get(symbol_key)) |val| {
            return val;
        }

        // Check if it's a top-level definition
        if (self.store.getSymbolDef(symbol)) |def_expr_id| {
            const val = try self.generateExpr(def_expr_id);
            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
            return val;
        }

        unreachable; // Symbol must exist in symbol_values or as a top-level def
    }

    fn generateBinop(self: *MonoLlvmCodeGen, binop: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        // For string/list equality, operands write to out_ptr and return .none.
        // Handle these before generating sub-expressions by materializing to allocas.
        if (binop.op == .eq or binop.op == .neq) {
            const operand_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
            if (operand_layout == .str) {
                const result = try self.callStrStr2Bool(binop.lhs, binop.rhs, "roc_builtins_str_equal");
                if (binop.op == .neq) {
                    const builder = self.builder orelse return error.CompilationFailed;
                    const one = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                    return wip.bin(.xor, result, one, "") catch return error.CompilationFailed;
                }
                return result;
            }
            // Check if operands are lists — detect from expression or layout store
            const elem_layout_for_list: ?layout.Idx = blk: {
                // Check via layout store if operand_layout is a list
                if (self.layout_store) |ls| {
                    const stored = ls.getLayout(operand_layout);
                    if (stored.tag == .list) break :blk stored.data.list;
                    if (stored.tag == .list_of_zst) break :blk .zst;
                }
                // Check the LHS expression directly
                const lhs_expr = self.store.getExpr(binop.lhs);
                switch (lhs_expr) {
                    .list => |l| break :blk l.elem_layout,
                    .empty_list => |e| break :blk e.elem_layout,
                    .low_level => |ll| {
                        if (self.layout_store) |ls| {
                            const stored = ls.getLayout(ll.ret_layout);
                            if (stored.tag == .list) break :blk stored.data.list;
                            if (stored.tag == .list_of_zst) break :blk .zst;
                        }
                    },
                    else => {},
                }
                break :blk null;
            };
            if (elem_layout_for_list) |elem_layout_idx| {
                const result = try self.generateListEqualityByElem(binop.lhs, binop.rhs, elem_layout_idx);
                if (binop.op == .neq) {
                    const builder = self.builder orelse return error.CompilationFailed;
                    const one = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                    return wip.bin(.xor, result, one, "") catch return error.CompilationFailed;
                }
                return result;
            }
        }

        var lhs = try self.generateExpr(binop.lhs);
        var rhs = try self.generateExpr(binop.rhs);

        // Values that were written to out_ptr (lists, strings) return .none
        std.debug.assert(lhs != .none and rhs != .none);

        // Check if operands are aggregate types (structs) - LLVM can't compare them directly.
        // For eq/neq on structs, compare field-by-field.
        const lhs_llvm_type = lhs.typeOfWip(wip);
        if (!isIntType(lhs_llvm_type) and lhs_llvm_type != .float and lhs_llvm_type != .double) {
            const bldr = self.builder orelse return error.CompilationFailed;
            if ((binop.op == .eq or binop.op == .neq) and lhs_llvm_type.isStruct(bldr)) {
                return self.generateAggregateEquality(lhs, rhs, binop.op == .neq);
            }
            unreachable; // Non-int/non-float/non-struct binop operands should not occur
        }

        // For comparison operations, result_layout is .bool, so check operand type instead.
        const operand_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
        const is_float = isFloatLayout(binop.result_layout) or isFloatLayout(operand_layout);

        // When result is float but operands may be integer (e.g. `b : F32; b = 2`
        // lowers the literal as i64), cast operands to the target float type.
        // Check actual LLVM types to detect mismatches from lookups of int-valued symbols.
        if (is_float) {
            const float_layout = if (isFloatLayout(binop.result_layout)) binop.result_layout else operand_layout;
            const target_type = layoutToLlvmType(float_layout);
            const lhs_type = lhs.typeOfWip(wip);
            const rhs_type = rhs.typeOfWip(wip);
            if (lhs_type != target_type and lhs_type != .float and lhs_type != .double) {
                const lhs_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
                lhs = wip.cast(if (isSigned(lhs_layout)) .sitofp else .uitofp, lhs, target_type, "") catch return error.CompilationFailed;
            }
            if (rhs_type != target_type and rhs_type != .float and rhs_type != .double) {
                const rhs_layout = self.getExprResultLayout(binop.rhs) orelse binop.result_layout;
                rhs = wip.cast(if (isSigned(rhs_layout)) .sitofp else .uitofp, rhs, target_type, "") catch return error.CompilationFailed;
            }
        }

        // Align integer operand widths (e.g., i64 vs i128 from Dec literals)
        if (!is_float) {
            const lhs_type = lhs.typeOfWip(wip);
            const rhs_type = rhs.typeOfWip(wip);
            if (isIntType(lhs_type) and isIntType(rhs_type) and lhs_type != rhs_type) {
                const lhs_bits = intTypeBits(lhs_type);
                const rhs_bits = intTypeBits(rhs_type);
                if (lhs_bits < rhs_bits) {
                    const lhs_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
                    lhs = wip.cast(if (isSigned(lhs_layout)) .sext else .zext, lhs, rhs_type, "") catch return error.CompilationFailed;
                } else {
                    const rhs_layout = self.getExprResultLayout(binop.rhs) orelse binop.result_layout;
                    rhs = wip.cast(if (isSigned(rhs_layout)) .sext else .zext, rhs, lhs_type, "") catch return error.CompilationFailed;
                }
            }
        }

        return switch (binop.op) {
            .add => if (is_float)
                wip.bin(.fadd, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed,

            .sub => if (is_float)
                wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed,

            .mul => if (is_float)
                wip.bin(.fmul, lhs, rhs, "") catch return error.CompilationFailed
            else if (binop.result_layout == .dec)
                self.callDecMul(lhs, rhs) catch return error.CompilationFailed
            else
                wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed,

            .div => if (is_float)
                wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
            else if (binop.result_layout == .dec)
                self.callDecDiv(lhs, rhs) catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,

            .div_trunc => if (is_float)
                wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
            else if (binop.result_layout == .dec)
                self.callDecDivTrunc(lhs, rhs) catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,

            .rem => if (is_float)
                wip.bin(.frem, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed,

            .eq => if (is_float)
                wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.eq, lhs, rhs, "") catch return error.CompilationFailed,

            .neq => if (is_float)
                wip.fcmp(.normal, .one, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ne, lhs, rhs, "") catch return error.CompilationFailed,

            .lt => if (is_float)
                wip.fcmp(.normal, .olt, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.slt, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ult, lhs, rhs, "") catch return error.CompilationFailed,

            .lte => if (is_float)
                wip.fcmp(.normal, .ole, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.sle, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ule, lhs, rhs, "") catch return error.CompilationFailed,

            .gt => if (is_float)
                wip.fcmp(.normal, .ogt, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.sgt, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ugt, lhs, rhs, "") catch return error.CompilationFailed,

            .gte => if (is_float)
                wip.fcmp(.normal, .oge, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.sge, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.uge, lhs, rhs, "") catch return error.CompilationFailed,

            .mod => if (is_float)
                wip.bin(.frem, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed,

            .shl => wip.bin(.shl, lhs, rhs, "") catch return error.CompilationFailed,
            .shr => wip.bin(.ashr, lhs, rhs, "") catch return error.CompilationFailed,
            .shr_zf => wip.bin(.lshr, lhs, rhs, "") catch return error.CompilationFailed,

            .@"and" => wip.bin(.@"and", lhs, rhs, "") catch return error.CompilationFailed,
            .@"or" => wip.bin(.@"or", lhs, rhs, "") catch return error.CompilationFailed,
        };
    }

    /// Compare two aggregate values (structs) field-by-field.
    /// Returns i1: true if all fields are equal (or not equal for neq).
    fn generateAggregateEquality(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, is_neq: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const lhs_type = lhs.typeOfWip(wip);
        std.debug.assert(lhs_type.isStruct(builder));
        const fields = lhs_type.structFields(builder);
        if (fields.len == 0) {
            // Empty struct — always equal
            const eq_val: i64 = if (is_neq) 0 else 1;
            return builder.intValue(.i1, eq_val) catch return error.OutOfMemory;
        }

        // Check if this struct looks like a RocStr/RocList: {ptr, i64, i64}
        // If so, use the str_equal builtin instead of field-by-field comparison.
        if (fields.len == 3 and self.isStrLikeStruct(fields)) {
            {
                const result = try self.callStrStr2BoolFromValues(lhs, rhs, "roc_builtins_str_equal");
                if (is_neq) {
                    const one = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                    return wip.bin(.xor, result, one, "") catch return error.CompilationFailed;
                }
                return result;
            }
        }

        // Compare each field and AND results together
        var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        for (0..fields.len) |i| {
            const idx: u32 = @intCast(i);
            const lhs_field = wip.extractValue(lhs, &.{idx}, "") catch return error.CompilationFailed;
            const rhs_field = wip.extractValue(rhs, &.{idx}, "") catch return error.CompilationFailed;

            const field_type = lhs_field.typeOfWip(wip);
            const field_eq = if (!isIntType(field_type) and field_type != .float and field_type != .double and field_type.isStruct(builder))
                // Nested struct — recurse
                try self.generateAggregateEquality(lhs_field, rhs_field, false)
            else if (!isIntType(field_type) and field_type != .float and field_type != .double)
                // Non-struct aggregate (pointer, etc.) — should not occur
                unreachable
            else if (field_type == .float or field_type == .double)
                wip.fcmp(.normal, .oeq, lhs_field, rhs_field, "") catch return error.CompilationFailed
            else
                wip.icmp(.eq, lhs_field, rhs_field, "") catch return error.CompilationFailed;

            result = wip.bin(.@"and", result, field_eq, "") catch return error.CompilationFailed;
        }

        // For neq, invert the result
        if (is_neq) {
            const one = builder.intValue(.i1, 1) catch return error.OutOfMemory;
            result = wip.bin(.xor, result, one, "") catch return error.CompilationFailed;
        }

        return result;
    }

    /// Check if a struct's fields match the RocStr/RocList pattern: {ptr, i64, i64}
    fn isStrLikeStruct(self: *MonoLlvmCodeGen, fields: []const LlvmBuilder.Type) bool {
        if (fields.len != 3) return false;
        const builder_ptr = self.builder orelse return false;
        const ptr_type = builder_ptr.ptrType(.default) catch return false;
        return fields[0] == ptr_type and fields[1] == .i64 and fields[2] == .i64;
    }

    /// Call str_equal from already-generated struct values (not from expr IDs).
    /// Decomposes both structs into (ptr, len, cap) and calls the named builtin.
    fn callStrStr2BoolFromValues(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, builtin_name: []const u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        // Extract fields from both structs
        const a_bytes = wip.extractValue(lhs, &.{0}, "") catch return error.CompilationFailed;
        const a_len = wip.extractValue(lhs, &.{1}, "") catch return error.CompilationFailed;
        const a_cap = wip.extractValue(lhs, &.{2}, "") catch return error.CompilationFailed;

        const b_bytes = wip.extractValue(rhs, &.{0}, "") catch return error.CompilationFailed;
        const b_len = wip.extractValue(rhs, &.{1}, "") catch return error.CompilationFailed;
        const b_cap = wip.extractValue(rhs, &.{2}, "") catch return error.CompilationFailed;

        return self.callBuiltin(builtin_name, .i1, &.{
            ptr_type, .i64, .i64, ptr_type, .i64, .i64,
        }, &.{
            a_bytes, a_len, a_cap, b_bytes, b_len, b_cap,
        });
    }

    /// Generate inline list equality comparison.
    /// Materializes both lists, compares lengths, then loops through elements.
    /// Takes the element layout index directly.
    fn generateListEqualityByElem(self: *MonoLlvmCodeGen, lhs_expr: LirExprId, rhs_expr: LirExprId, elem_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        // ZST elements: just compare lengths
        if (elem_layout_idx == .zst) {
            const a_ptr = try self.materializeAsPtr(lhs_expr, 24);
            const b_ptr = try self.materializeAsPtr(rhs_expr, 24);
            const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
            const a_len_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off8}, "") catch return error.CompilationFailed;
            const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;
            const b_len_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off8}, "") catch return error.CompilationFailed;
            const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;
            return wip.icmp(.eq, a_len, b_len, "") catch return error.CompilationFailed;
        }

        const elem_layout = ls.getLayout(elem_layout_idx);
        const elem_sa = ls.layoutSizeAlign(elem_layout);
        const elem_size: u64 = elem_sa.size;
        std.debug.assert(elem_size != 0);

        // Determine element LLVM type for loading
        const elem_llvm_type = layoutToLlvmType(elem_layout_idx);

        // Determine if elements need special comparison (strings, nested lists)
        const is_str_elem = (elem_layout_idx == .str);
        const is_list_elem = (elem_layout.tag == .list or elem_layout.tag == .list_of_zst);

        // Materialize both lists
        const a_ptr = try self.materializeAsPtr(lhs_expr, 24);
        const b_ptr = try self.materializeAsPtr(rhs_expr, 24);

        // Load data pointers and lengths
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const a_data = wip.load(.normal, ptr_type, a_ptr, alignment, "") catch return error.CompilationFailed;
        const a_len_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;

        const b_data = wip.load(.normal, ptr_type, b_ptr, alignment, "") catch return error.CompilationFailed;
        const b_len_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;

        // Compare lengths
        const len_eq = wip.icmp(.eq, a_len, b_len, "") catch return error.CompilationFailed;

        // Blocks: entry -> loop_header(2: entry+loop_body) -> loop_body(1: loop_header)
        //         -> equal(1: loop_header) / not_equal(2: entry+loop_body) -> merge(2: equal+not_equal)
        const entry_block = wip.cursor.block;
        const loop_header = wip.block(2, "") catch return error.OutOfMemory;
        const loop_body = wip.block(1, "") catch return error.OutOfMemory;
        const equal_block = wip.block(1, "") catch return error.OutOfMemory;
        const not_equal_block = wip.block(2, "") catch return error.OutOfMemory;
        const merge_block = wip.block(2, "") catch return error.OutOfMemory;

        // If lengths differ, go to not_equal
        _ = wip.brCond(len_eq, loop_header, not_equal_block, .none) catch return error.CompilationFailed;

        // Loop header: phi for index
        wip.cursor = .{ .block = loop_header };
        const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const phi_idx = wip.phi(.i64, "") catch return error.CompilationFailed;

        // Check if index < length
        const in_bounds = wip.icmp(.ult, phi_idx.toValue(), a_len, "") catch return error.CompilationFailed;
        _ = wip.brCond(in_bounds, loop_body, equal_block, .none) catch return error.CompilationFailed;

        // Loop body: compare elements at index
        wip.cursor = .{ .block = loop_body };
        const elem_size_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
        const byte_offset = wip.bin(.mul, phi_idx.toValue(), elem_size_val, "") catch return error.CompilationFailed;
        const a_elem_ptr = wip.gep(.inbounds, .i8, a_data, &.{byte_offset}, "") catch return error.CompilationFailed;
        const b_elem_ptr = wip.gep(.inbounds, .i8, b_data, &.{byte_offset}, "") catch return error.CompilationFailed;

        const elem_eq = if (is_str_elem) blk: {
            // String elements: load as {ptr, i64, i64} structs, call str_equal
            const roc_str_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            const a_str = wip.load(.normal, roc_str_type, a_elem_ptr, alignment, "") catch return error.CompilationFailed;
            const b_str = wip.load(.normal, roc_str_type, b_elem_ptr, alignment, "") catch return error.CompilationFailed;
            break :blk try self.callStrStr2BoolFromValues(a_str, b_str, "roc_builtins_str_equal");
        } else if (is_list_elem) blk: {
            // Nested list elements: recursively compare via pointers
            break :blk try self.generateListEqualityFromPtrs(a_elem_ptr, b_elem_ptr, elem_layout_idx);
        } else blk: {
            // Scalar elements: load and compare directly
            const elem_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_sa.alignment.toByteUnits(), 1)));
            const a_elem = wip.load(.normal, elem_llvm_type, a_elem_ptr, elem_align, "") catch return error.CompilationFailed;
            const b_elem = wip.load(.normal, elem_llvm_type, b_elem_ptr, elem_align, "") catch return error.CompilationFailed;
            if (elem_llvm_type == .float or elem_llvm_type == .double) {
                break :blk wip.fcmp(.normal, .oeq, a_elem, b_elem, "") catch return error.CompilationFailed;
            }
            break :blk wip.icmp(.eq, a_elem, b_elem, "") catch return error.CompilationFailed;
        };

        // If not equal, jump to not_equal; otherwise increment and loop
        const next_idx = wip.bin(.add, phi_idx.toValue(), builder.intValue(.i64, 1) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
        _ = wip.brCond(elem_eq, loop_header, not_equal_block, .none) catch return error.CompilationFailed;

        // Finish loop index phi: [0 from entry, next_idx from loop_body]
        phi_idx.finish(&.{ zero_i64, next_idx }, &.{ entry_block, loop_body }, wip);

        // Equal block
        wip.cursor = .{ .block = equal_block };
        _ = wip.br(merge_block) catch return error.CompilationFailed;

        // Not-equal block
        wip.cursor = .{ .block = not_equal_block };
        _ = wip.br(merge_block) catch return error.CompilationFailed;

        // Merge block with phi for result
        wip.cursor = .{ .block = merge_block };
        const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
        const result_phi = wip.phi(.i1, "") catch return error.CompilationFailed;
        result_phi.finish(&.{ true_val, false_val }, &.{ equal_block, not_equal_block }, wip);

        return result_phi.toValue();
    }

    /// Generate list equality comparison from already-loaded list pointers.
    /// Used for nested list elements (list of lists).
    fn generateListEqualityFromPtrs(self: *MonoLlvmCodeGen, a_list_ptr: LlvmBuilder.Value, b_list_ptr: LlvmBuilder.Value, list_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const stored = ls.getLayout(list_layout_idx);
        const elem_layout_idx = if (stored.tag == .list) stored.data.list else unreachable;
        const elem_layout = ls.getLayout(elem_layout_idx);
        const elem_sa = ls.layoutSizeAlign(elem_layout);
        const elem_size: u64 = elem_sa.size;
        std.debug.assert(elem_size != 0);

        const elem_llvm_type = layoutToLlvmType(elem_layout_idx);
        const is_str_elem = (elem_layout_idx == .str);

        // Load data pointers and lengths from the 24-byte list structs
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const a_data = wip.load(.normal, ptr_type, a_list_ptr, alignment, "") catch return error.CompilationFailed;
        const a_len_ptr = wip.gep(.inbounds, .i8, a_list_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;

        const b_data = wip.load(.normal, ptr_type, b_list_ptr, alignment, "") catch return error.CompilationFailed;
        const b_len_ptr = wip.gep(.inbounds, .i8, b_list_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;

        const len_eq = wip.icmp(.eq, a_len, b_len, "") catch return error.CompilationFailed;

        const entry_blk = wip.cursor.block;
        const loop_header = wip.block(2, "") catch return error.OutOfMemory;
        const loop_body = wip.block(1, "") catch return error.OutOfMemory;
        const equal_block = wip.block(1, "") catch return error.OutOfMemory;
        const not_equal_block = wip.block(2, "") catch return error.OutOfMemory;
        const merge_block = wip.block(2, "") catch return error.OutOfMemory;

        _ = wip.brCond(len_eq, loop_header, not_equal_block, .none) catch return error.CompilationFailed;

        wip.cursor = .{ .block = loop_header };
        const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const phi_idx = wip.phi(.i64, "") catch return error.CompilationFailed;

        const in_bounds = wip.icmp(.ult, phi_idx.toValue(), a_len, "") catch return error.CompilationFailed;
        _ = wip.brCond(in_bounds, loop_body, equal_block, .none) catch return error.CompilationFailed;

        wip.cursor = .{ .block = loop_body };
        const elem_size_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
        const byte_offset = wip.bin(.mul, phi_idx.toValue(), elem_size_val, "") catch return error.CompilationFailed;
        const a_elem_ptr = wip.gep(.inbounds, .i8, a_data, &.{byte_offset}, "") catch return error.CompilationFailed;
        const b_elem_ptr = wip.gep(.inbounds, .i8, b_data, &.{byte_offset}, "") catch return error.CompilationFailed;

        const elem_eq = if (is_str_elem) blk: {
            const roc_str_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            const a_str = wip.load(.normal, roc_str_type, a_elem_ptr, alignment, "") catch return error.CompilationFailed;
            const b_str = wip.load(.normal, roc_str_type, b_elem_ptr, alignment, "") catch return error.CompilationFailed;
            break :blk try self.callStrStr2BoolFromValues(a_str, b_str, "roc_builtins_str_equal");
        } else blk: {
            // Scalar elements
            const elem_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_sa.alignment.toByteUnits(), 1)));
            const a_elem = wip.load(.normal, elem_llvm_type, a_elem_ptr, elem_align, "") catch return error.CompilationFailed;
            const b_elem = wip.load(.normal, elem_llvm_type, b_elem_ptr, elem_align, "") catch return error.CompilationFailed;
            if (elem_llvm_type == .float or elem_llvm_type == .double) {
                break :blk wip.fcmp(.normal, .oeq, a_elem, b_elem, "") catch return error.CompilationFailed;
            }
            break :blk wip.icmp(.eq, a_elem, b_elem, "") catch return error.CompilationFailed;
        };

        const next_idx = wip.bin(.add, phi_idx.toValue(), builder.intValue(.i64, 1) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
        _ = wip.brCond(elem_eq, loop_header, not_equal_block, .none) catch return error.CompilationFailed;

        phi_idx.finish(&.{ zero_i64, next_idx }, &.{ entry_blk, loop_body }, wip);

        wip.cursor = .{ .block = equal_block };
        _ = wip.br(merge_block) catch return error.CompilationFailed;

        wip.cursor = .{ .block = not_equal_block };
        _ = wip.br(merge_block) catch return error.CompilationFailed;

        wip.cursor = .{ .block = merge_block };
        const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
        const result_phi = wip.phi(.i1, "") catch return error.CompilationFailed;
        result_phi.finish(&.{ true_val, false_val }, &.{ equal_block, not_equal_block }, wip);

        return result_phi.toValue();
    }

    fn isSigned(result_layout: layout.Idx) bool {
        return switch (result_layout) {
            .i8, .i16, .i32, .i64, .i128, .dec => true,
            else => false,
        };
    }

    fn isFloatLayout(l: layout.Idx) bool {
        return l == .f32 or l == .f64;
    }

    /// Convert a layout.Idx to the LLVM type used for struct fields.
    /// Unlike layoutToLlvmType, this maps bool to i8 (1 byte in memory)
    /// instead of i1 (1 bit), matching the layout store's memory representation.
    fn layoutToStructFieldType(self: *MonoLlvmCodeGen, field_layout: layout.Idx) Error!LlvmBuilder.Type {
        return self.layoutToLlvmTypeWithOptions(field_layout, true);
    }

    /// Build an LLVM struct type from the actual LLVM types of generated values.
    fn buildStructTypeFromValues(builder: *LlvmBuilder, wip: *LlvmBuilder.WipFunction, values: []const LlvmBuilder.Value) Error!LlvmBuilder.Type {
        var field_types: [32]LlvmBuilder.Type = undefined;
        for (values, 0..) |val, i| {
            field_types[i] = val.typeOfWip(wip);
        }
        return builder.structType(.normal, field_types[0..values.len]) catch return error.OutOfMemory;
    }

    /// Convert a value to match the expected struct field type.
    /// Handles i1→i8 (bool), integer widening/narrowing, etc.
    fn convertToFieldType(self: *MonoLlvmCodeGen, val: LlvmBuilder.Value, field_layout: layout.Idx) Error!LlvmBuilder.Value {
        if (val == .none) return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_type = try self.layoutToStructFieldType(field_layout);
        const actual_type = val.typeOfWip(wip);

        if (actual_type == target_type) return val;

        // i1 → i8 (bool in struct)
        if (actual_type == .i1 and target_type == .i8) {
            return wip.cast(.zext, val, .i8, "") catch return error.CompilationFailed;
        }

        // Integer widening (e.g., i64 → i128 for Dec fields)
        if (isIntType(actual_type) and isIntType(target_type)) {
            const actual_bits = intTypeBits(actual_type);
            const target_bits = intTypeBits(target_type);
            if (actual_bits < target_bits) {
                // Widen: use sext for signed, zext for unsigned
                return wip.cast(if (isSigned(field_layout)) .sext else .zext, val, target_type, "") catch return error.CompilationFailed;
            } else if (actual_bits > target_bits) {
                return wip.cast(.trunc, val, target_type, "") catch return error.CompilationFailed;
            }
        }

        // Int → Float conversions
        if (isIntType(actual_type) and (target_type == .float or target_type == .double)) {
            return wip.cast(if (isSigned(field_layout)) .sitofp else .uitofp, val, target_type, "") catch return error.CompilationFailed;
        }

        // If types don't match and we can't convert, return as-is (may cause assertion)
        return val;
    }

    fn coerceValueToLayout(self: *MonoLlvmCodeGen, val: LlvmBuilder.Value, target_layout: layout.Idx) Error!LlvmBuilder.Value {
        if (val == .none) return error.CompilationFailed;

        const wip = self.wip orelse return error.CompilationFailed;
        const target_type = try self.layoutToLlvmTypeFull(target_layout);
        const actual_type = val.typeOfWip(wip);

        if (actual_type == target_type) return val;

        if (target_layout == .bool and isIntType(actual_type) and actual_type != .i1) {
            return wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        if (target_layout == .bool and actual_type == .ptr) {
            const raw_bool = wip.load(.normal, .i8, val, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.CompilationFailed;
            return wip.cast(.trunc, raw_bool, .i1, "") catch return error.CompilationFailed;
        }

        if (actual_type == .i1 and target_type == .i8) {
            return wip.cast(.zext, val, .i8, "") catch return error.CompilationFailed;
        }

        if (isIntType(actual_type) and isIntType(target_type)) {
            const actual_bits = intTypeBits(actual_type);
            const target_bits = intTypeBits(target_type);
            if (actual_bits < target_bits) {
                return wip.cast(if (isSigned(target_layout)) .sext else .zext, val, target_type, "") catch return error.CompilationFailed;
            }
            if (actual_bits > target_bits) {
                return wip.cast(.trunc, val, target_type, "") catch return error.CompilationFailed;
            }
        }

        if (isIntType(actual_type) and (target_type == .float or target_type == .double)) {
            return wip.cast(if (isSigned(target_layout)) .sitofp else .uitofp, val, target_type, "") catch return error.CompilationFailed;
        }

        if ((actual_type == .float or actual_type == .double) and isIntType(target_type)) {
            return wip.cast(if (isSigned(target_layout)) .fptosi else .fptoui, val, target_type, "") catch return error.CompilationFailed;
        }

        return val;
    }

    fn isIntType(t: LlvmBuilder.Type) bool {
        return t == .i1 or t == .i8 or t == .i16 or t == .i32 or t == .i64 or t == .i128;
    }

    fn intTypeBits(t: LlvmBuilder.Type) u32 {
        return switch (t) {
            .i1 => 1,
            .i8 => 8,
            .i16 => 16,
            .i32 => 32,
            .i64 => 64,
            .i128 => 128,
            else => 0,
        };
    }

    fn coerceValueToType(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, expected_type: LlvmBuilder.Type, value_layout: ?layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const actual_type = value.typeOfWip(wip);

        if (actual_type == expected_type) return value;

        if (expected_type == .i1 and isIntType(actual_type)) {
            return wip.cast(.trunc, value, .i1, "") catch return error.CompilationFailed;
        }

        if (actual_type == .i1 and expected_type == .i8) {
            return wip.cast(.zext, value, .i8, "") catch return error.CompilationFailed;
        }

        if (isIntType(actual_type) and isIntType(expected_type)) {
            const actual_bits = intTypeBits(actual_type);
            const expected_bits = intTypeBits(expected_type);
            if (actual_bits < expected_bits) {
                const signed = if (value_layout) |l| isSigned(l) else false;
                return wip.cast(if (signed) .sext else .zext, value, expected_type, "") catch return error.CompilationFailed;
            }
            if (actual_bits > expected_bits) {
                return wip.cast(.trunc, value, expected_type, "") catch return error.CompilationFailed;
            }
        }

        if (isIntType(actual_type) and (expected_type == .float or expected_type == .double)) {
            const signed = if (value_layout) |l| isSigned(l) else false;
            return wip.cast(if (signed) .sitofp else .uitofp, value, expected_type, "") catch return error.CompilationFailed;
        }

        if ((actual_type == .float or actual_type == .double) and isIntType(expected_type)) {
            const signed = if (value_layout) |l| isSigned(l) else false;
            return wip.cast(if (signed) .fptosi else .fptoui, value, expected_type, "") catch return error.CompilationFailed;
        }

        if ((actual_type == .float or actual_type == .double) and (expected_type == .float or expected_type == .double)) {
            return wip.cast(if (actual_type == .float) .fpext else .fptrunc, value, expected_type, "") catch return error.CompilationFailed;
        }

        if (actual_type.isPointer(self.builder orelse return error.CompilationFailed) and expected_type.isPointer(self.builder orelse return error.CompilationFailed)) {
            return wip.cast(.bitcast, value, expected_type, "") catch return error.CompilationFailed;
        }

        return value;
    }

    fn llvmTypeByteSize(t: LlvmBuilder.Type) u64 {
        return switch (t) {
            .i1, .i8 => 1,
            .i16 => 2,
            .i32, .float => 4,
            .i64, .double => 8,
            .i128 => 16,
            else => 0,
        };
    }

    // Record and tuple generation

    fn generateEmptyRecord(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i8, 0) catch return error.OutOfMemory).toValue();
    }

    fn generateStruct(self: *MonoLlvmCodeGen, struct_expr: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;

        const stored_layout = ls.getLayout(struct_expr.struct_layout);
        if (stored_layout.tag == .zst) {
            return self.generateEmptyRecord();
        }
        std.debug.assert(stored_layout.tag == .struct_);

        const struct_data = ls.getStructData(stored_layout.data.struct_.idx);
        const field_count = struct_data.getFields().count;
        if (field_count == 0) {
            return self.generateEmptyRecord();
        }

        const field_exprs = self.store.getExprSpan(struct_expr.fields);
        var field_values_buf: [32]LlvmBuilder.Value = undefined;

        for (field_exprs, 0..) |field_expr_id, i| {
            const raw_val = try self.generateExpr(field_expr_id);
            const field_layout = ls.getStructFieldLayout(stored_layout.data.struct_.idx, @intCast(i));
            field_values_buf[i] = try self.convertToFieldType(raw_val, field_layout);
        }

        const struct_type = try buildStructTypeFromValues(builder, wip, field_values_buf[0..field_count]);
        var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
        for (0..field_count) |i| {
            struct_val = wip.insertValue(struct_val, field_values_buf[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
        }

        return struct_val;
    }

    fn generateStructAccess(self: *MonoLlvmCodeGen, access: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const struct_val = try self.generateExpr(access.struct_expr);
        if (struct_val == .none) return error.CompilationFailed;
        if (!struct_val.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;

        var val = wip.extractValue(struct_val, &.{@intCast(access.field_idx)}, "") catch return error.CompilationFailed;
        if (access.field_layout == .bool and val.typeOfWip(wip) == .i8) {
            val = wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        return val;
    }

    fn generateCellLoad(self: *MonoLlvmCodeGen, cell: Symbol, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const key: u64 = @bitCast(cell);
        if (self.cell_allocas.get(key)) |cell_alloca| {
            const wip = self.wip orelse return error.CompilationFailed;
            return wip.load(.normal, cell_alloca.elem_type, cell_alloca.alloca_ptr, self.alignmentForLayout(layout_idx), "") catch return error.CompilationFailed;
        }

        if (self.loop_var_allocas.get(key)) |lva| {
            const wip = self.wip orelse return error.CompilationFailed;
            const alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(lva.elem_type), 1)));
            return wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
        }

        if (builtin.mode == .Debug) {
            std.log.err(
                "cell_load miss: layout={d} has_symbol_value={any}",
                .{ @intFromEnum(layout_idx), self.symbol_values.contains(key) },
            );
        }

        return self.generateLookup(cell, layout_idx);
    }

    fn generateRecord(self: *MonoLlvmCodeGen, rec: anytype) Error!LlvmBuilder.Value {
        // Clear out_ptr — record field values must not write to the output buffer.
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(rec.record_layout);
        std.debug.assert(stored_layout.tag == .record);

        const record_data = ls.getRecordData(stored_layout.data.record.idx);
        const field_count = record_data.getFields().count;

        if (field_count == 0) {
            return self.generateEmptyRecord();
        }

        // Get field expressions (already in sorted order from the lowerer)
        const field_exprs = self.store.getExprSpan(rec.fields);

        // Generate field values and convert to their expected types
        var field_values_buf: [32]LlvmBuilder.Value = undefined;

        for (field_exprs, 0..) |field_expr_id, i| {
            const raw_val = try self.generateExpr(field_expr_id);
            const field_layout = ls.getRecordFieldLayout(stored_layout.data.record.idx, @intCast(i));
            field_values_buf[i] = try self.convertToFieldType(raw_val, field_layout);
        }

        // Create LLVM struct type from actual generated value types
        const struct_type = try buildStructTypeFromValues(builder, wip, field_values_buf[0..field_count]);

        // Build struct value using insertvalue (types match by construction)
        var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
        for (0..field_count) |i| {
            struct_val = wip.insertValue(struct_val, field_values_buf[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
        }

        return struct_val;
    }

    fn generateFieldAccess(self: *MonoLlvmCodeGen, access: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Generate the record value
        const record_val = try self.generateExpr(access.record_expr);

        // Guard: if value is .none or not a struct, we can't extract fields
        if (record_val == .none) return error.CompilationFailed;
        const val_type = record_val.typeOfWip(wip);
        std.debug.assert(val_type.isStruct(builder));

        // Extract the field at the sorted index
        var val = wip.extractValue(record_val, &.{@intCast(access.field_idx)}, "") catch return error.CompilationFailed;

        // Truncate i8 back to i1 for bool fields
        if (access.field_layout == .bool and val.typeOfWip(wip) == .i8) {
            val = wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        return val;
    }

    fn generateTuple(self: *MonoLlvmCodeGen, tup: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(tup.tuple_layout);
        std.debug.assert(stored_layout.tag == .tuple);

        const tuple_data = ls.getTupleData(stored_layout.data.tuple.idx);
        const sorted_elements = ls.tuple_fields.sliceRange(tuple_data.getFields());
        const elem_count = sorted_elements.len;

        if (elem_count == 0) {
            return self.generateEmptyRecord();
        }

        // Get element expressions (in source/original order)
        const elem_exprs = self.store.getExprSpan(tup.elems);

        // Build sorted-position-to-value mapping.
        // elem_exprs[i] has original index i; we need to find its sorted position.
        var sorted_values: [32]LlvmBuilder.Value = undefined;
        var sorted_layouts: [32]layout.Idx = undefined;

        // First, fill in sorted layouts from the layout store
        for (0..elem_count) |sorted_i| {
            const element = sorted_elements.get(@intCast(sorted_i));
            sorted_layouts[sorted_i] = element.layout;
        }

        // Generate each source-order element, convert to field type, and map to sorted position
        for (elem_exprs, 0..) |elem_expr_id, original_i| {
            const raw_val = try self.generateExpr(elem_expr_id);
            // Find sorted position for this original index
            var sorted_pos: usize = 0;
            for (0..elem_count) |si| {
                const element = sorted_elements.get(@intCast(si));
                if (element.index == original_i) {
                    sorted_pos = si;
                    break;
                }
            }
            sorted_values[sorted_pos] = try self.convertToFieldType(raw_val, sorted_layouts[sorted_pos]);
        }

        // Create LLVM struct type from actual generated value types
        const struct_type = try buildStructTypeFromValues(builder, wip, sorted_values[0..elem_count]);

        // Build struct value using insertvalue (types match by construction)
        var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
        for (0..elem_count) |i| {
            struct_val = wip.insertValue(struct_val, sorted_values[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
        }

        return struct_val;
    }

    fn generateTupleAccess(self: *MonoLlvmCodeGen, access: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Generate the tuple value
        const tuple_val = try self.generateExpr(access.tuple_expr);

        // Guard: if value is .none or not a struct, we can't extract elements
        if (tuple_val == .none) return error.CompilationFailed;
        const val_type = tuple_val.typeOfWip(wip);
        std.debug.assert(val_type.isStruct(builder));

        // Extract the element at the sorted index
        var val = wip.extractValue(tuple_val, &.{@intCast(access.elem_idx)}, "") catch return error.CompilationFailed;

        // Truncate i8 back to i1 for bool fields
        if (access.elem_layout == .bool and val.typeOfWip(wip) == .i8) {
            val = wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        return val;
    }

    // Tag union generation

    /// Get the LLVM integer type for a discriminant size.
    fn discriminantIntType(disc_size: u8) LlvmBuilder.Type {
        return switch (disc_size) {
            1 => .i8,
            2 => .i16,
            4 => .i32,
            8 => .i64,
            else => .i8,
        };
    }

    /// Generate a zero-argument tag (just the discriminant value).
    fn generateZeroArgTag(self: *MonoLlvmCodeGen, zat: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(zat.union_layout);

        switch (stored_layout.tag) {
            .scalar => {
                // Tag union with no payloads (e.g., Bool, Color) → just the discriminant integer
                const llvm_type = layoutToLlvmType(zat.union_layout);
                return (builder.intConst(llvm_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory).toValue();
            },
            .zst => {
                // Zero-sized tag union
                return (builder.intConst(.i8, 0) catch return error.OutOfMemory).toValue();
            },
            .tag_union => {
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                var has_payloads = false;
                for (0..variants.len) |variant_idx| {
                    if (variants.get(@intCast(variant_idx)).payload_layout != .zst) {
                        has_payloads = true;
                        break;
                    }
                }

                if (!has_payloads) {
                    return (builder.intConst(.i64, @as(u64, zat.discriminant)) catch return error.OutOfMemory).toValue();
                }

                const tu_size = tu_data.size;
                const tu_align_bytes: u64 = @intCast(stored_layout.data.tag_union.alignment.toByteUnits());
                const min_align: u64 = @max(tu_align_bytes, 8);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(min_align);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const size_val_i64 = builder.intValue(.i64, tu_size) catch return error.OutOfMemory;
                const align_val = builder.intValue(.i32, @as(u32, @intCast(min_align))) catch return error.OutOfMemory;
                const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const heap_ptr = try self.callBuiltin(
                    "roc_builtins_allocate_with_refcount",
                    ptr_type,
                    &.{ .i64, .i32, .i1, ptr_type },
                    &.{ size_val_i64, align_val, refcounted_val, roc_ops },
                );

                // Zero the memory
                const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const size_val = builder.intValue(.i32, tu_size) catch return error.OutOfMemory;
                _ = wip.callMemSet(heap_ptr, alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

                // Store discriminant at discriminant_offset
                const disc_offset = tu_data.discriminant_offset;
                const disc_type = discriminantIntType(tu_data.discriminant_size);
                const disc_val = builder.intValue(disc_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory;
                const disc_ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

                return heap_ptr;
            },
            .closure, .struct_, .list, .list_of_zst, .box, .box_of_zst => unreachable,
        }
    }

    /// Generate a tag with payload arguments.
    fn generateTagWithPayload(self: *MonoLlvmCodeGen, tag_expr: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(tag_expr.union_layout);
        std.debug.assert(stored_layout.tag == .tag_union);

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const tu_size = tu_data.size;
        const tu_align_bytes: u64 = @intCast(stored_layout.data.tag_union.alignment.toByteUnits());
        // Heap-allocate the tag union so returned pointer values remain valid after
        // the current function returns.
        const min_align: u64 = @max(tu_align_bytes, 8);
        const padded_size: u32 = @intCast((@as(u64, tu_size) + min_align - 1) / min_align * min_align);
        const forced_alignment = LlvmBuilder.Alignment.fromByteUnits(min_align);
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const size_val_i64 = builder.intValue(.i64, padded_size) catch return error.OutOfMemory;
        const align_val = builder.intValue(.i32, @as(u32, @intCast(min_align))) catch return error.OutOfMemory;
        const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
        const heap_ptr = try self.callBuiltin(
            "roc_builtins_allocate_with_refcount",
            ptr_type,
            &.{ .i64, .i32, .i1, ptr_type },
            &.{ size_val_i64, align_val, refcounted_val, roc_ops },
        );

        // Zero the memory
        const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const size_val = builder.intValue(.i32, padded_size) catch return error.OutOfMemory;
        _ = wip.callMemSet(heap_ptr, forced_alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

        // Store payload arguments
        const arg_exprs = self.store.getExprSpan(tag_expr.args);
        const variants = ls.getTagUnionVariants(tu_data);
        const variant = variants.get(tag_expr.discriminant);

        if (arg_exprs.len == 1) {
            // Single argument — store directly at offset 0
            const arg = try self.generateExprAsValue(arg_exprs[0]);
            const store_val = try self.convertToFieldType(arg.value, variant.payload_layout);
            const payload_align = LlvmBuilder.Alignment.fromByteUnits(
                @intCast(@max(ls.getLayout(variant.payload_layout).alignment(ls.targetUsize()).toByteUnits(), 1)),
            );
            _ = wip.store(.normal, store_val, heap_ptr, payload_align) catch return error.CompilationFailed;
        } else if (arg_exprs.len > 1) {
            // Multiple arguments — payload is a tuple
            const payload_layout = ls.getLayout(variant.payload_layout);
            if (payload_layout.tag == .struct_) {
                const struct_data = ls.getStructData(payload_layout.data.struct_.idx);
                const sorted_fields = ls.struct_fields.sliceRange(struct_data.getFields());

                for (arg_exprs, 0..) |arg_expr_id, i| {
                    const field_layout = ls.getStructFieldLayout(payload_layout.data.struct_.idx, @intCast(i));
                    const arg = try self.generateExprAsValue(arg_expr_id);
                    const arg_val = try self.convertToFieldType(arg.value, field_layout);
                    var offset: u32 = 0;
                    for (0..sorted_fields.len) |si| {
                        const field = sorted_fields.get(@intCast(si));
                        if (field.index == i) {
                            offset = ls.getStructFieldOffset(payload_layout.data.struct_.idx, @intCast(si));
                            break;
                        }
                    }
                    const field_ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{builder.intValue(.i32, offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                    const field_align = LlvmBuilder.Alignment.fromByteUnits(
                        @intCast(@max(ls.getLayout(field_layout).alignment(ls.targetUsize()).toByteUnits(), 1)),
                    );
                    _ = wip.store(.normal, arg_val, field_ptr, field_align) catch return error.CompilationFailed;
                }
            }
        }

        // Store discriminant at discriminant_offset
        const disc_offset = tu_data.discriminant_offset;
        const disc_type = discriminantIntType(tu_data.discriminant_size);
        const disc_val = builder.intValue(disc_type, @as(u64, tag_expr.discriminant)) catch return error.OutOfMemory;
        const disc_ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
        _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

        return heap_ptr;
    }

    /// Extract the payload from a tag union value.
    fn generateTagPayloadAccess(self: *MonoLlvmCodeGen, tpa: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;

        const raw_value = try self.generateExpr(tpa.value);
        const union_layout = ls.getLayout(tpa.union_layout);
        const payload_layout = ls.getLayout(tpa.payload_layout);

        if (tpa.payload_layout == .zst or payload_layout.tag == .zst) {
            return builder.intValue(.i8, 0) catch return error.OutOfMemory;
        }

        switch (union_layout.tag) {
            .tag_union => {
                const load_type = try self.layoutToStructFieldType(tpa.payload_layout);
                const payload_alignment = LlvmBuilder.Alignment.fromByteUnits(
                    @intCast(@max(payload_layout.alignment(ls.targetUsize()).toByteUnits(), 1)),
                );
                var payload = wip.load(.normal, load_type, raw_value, payload_alignment, "") catch return error.CompilationFailed;
                if (tpa.payload_layout == .bool and load_type == .i8) {
                    payload = wip.cast(.trunc, payload, .i1, "") catch return error.CompilationFailed;
                }
                return payload;
            },
            .box => {
                const inner_layout = ls.getLayout(union_layout.data.box);
                if (inner_layout.tag == .tag_union) {
                    const load_type = try self.layoutToStructFieldType(tpa.payload_layout);
                    const payload_alignment = LlvmBuilder.Alignment.fromByteUnits(
                        @intCast(@max(payload_layout.alignment(ls.targetUsize()).toByteUnits(), 1)),
                    );
                    var payload = wip.load(.normal, load_type, raw_value, payload_alignment, "") catch return error.CompilationFailed;
                    if (tpa.payload_layout == .bool and load_type == .i8) {
                        payload = wip.cast(.trunc, payload, .i1, "") catch return error.CompilationFailed;
                    }
                    return payload;
                }
                return raw_value;
            },
            .scalar, .zst => return raw_value,
            .closure, .struct_, .list, .list_of_zst, .box_of_zst => return error.CompilationFailed,
        }
    }

    /// Generate a discriminant switch — dispatch on a tag union's discriminant.
    fn generateDiscriminantSwitch(self: *MonoLlvmCodeGen, ds: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        // Generate the value to switch on
        const value = try self.generateExpr(ds.value);

        // Get branch expressions
        const branch_exprs = self.store.getExprSpan(ds.branches);
        std.debug.assert(branch_exprs.len != 0);

        // Extract discriminant based on layout type
        const stored_layout = ls.getLayout(ds.union_layout);
        const discriminant: LlvmBuilder.Value = switch (stored_layout.tag) {
            .scalar => blk: {
                // Scalar tag union — value IS the discriminant (just an integer)
                // May need truncation from i64 to the actual discriminant type
                const val_type = value.typeOfWip(wip);
                if (val_type == .i8 or val_type == .i1) break :blk value;
                // Truncate to i8 for comparison
                break :blk wip.cast(.trunc, value, .i8, "") catch return error.CompilationFailed;
            },
            .tag_union => blk: {
                // Tag union — load discriminant from pointer
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                const disc_type = discriminantIntType(tu_data.discriminant_size);
                const disc_offset = tu_data.discriminant_offset;
                const disc_ptr = wip.gep(.inbounds, .i8, value, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                break :blk wip.load(.normal, disc_type, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)), "") catch return error.OutOfMemory;
            },
            .closure, .struct_, .list, .list_of_zst, .box, .box_of_zst, .zst => unreachable,
        };

        // Create basic blocks for each branch and the merge block
        const merge_block = wip.block(@intCast(branch_exprs.len), "ds_merge") catch return error.OutOfMemory;

        // Generate branches as a chain of compare-and-branch (like if-else-if)
        // For each discriminant value, check if it matches, and if so generate that branch.
        var result_vals: [64]LlvmBuilder.Value = undefined;
        var result_blocks: [64]LlvmBuilder.Function.Block.Index = undefined;
        var branch_count: usize = 0;
        for (branch_exprs, 0..) |branch_expr_id, i| {
            const is_last = (i == branch_exprs.len - 1);

            if (!is_last) {
                // Compare discriminant == i
                const disc_type = discriminant.typeOfWip(wip);
                const idx_val = builder.intValue(disc_type, @as(u64, @intCast(i))) catch return error.OutOfMemory;
                const cmp = wip.icmp(.eq, discriminant, idx_val, "") catch return error.OutOfMemory;

                const then_block = wip.block(1, "") catch return error.OutOfMemory;
                const else_block = wip.block(1, "") catch return error.OutOfMemory;
                _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                // Then block — generate branch body
                wip.cursor = .{ .block = then_block };
                const branch_val = try self.generateControlFlowValue(branch_expr_id, ds.result_layout);
                _ = wip.br(merge_block) catch return error.OutOfMemory;
                result_vals[branch_count] = branch_val;
                result_blocks[branch_count] = wip.cursor.block;
                branch_count += 1;

                // Else block — continue to next comparison
                wip.cursor = .{ .block = else_block };
            } else {
                // Last branch — no comparison needed (default case)
                const branch_val = try self.generateControlFlowValue(branch_expr_id, ds.result_layout);
                _ = wip.br(merge_block) catch return error.OutOfMemory;
                result_vals[branch_count] = branch_val;
                result_blocks[branch_count] = wip.cursor.block;
                branch_count += 1;
            }
        }

        // Merge block
        wip.cursor = .{ .block = merge_block };

        const result_type = result_vals[0].typeOfWip(wip);
        const phi_inst = wip.phi(result_type, "") catch return error.OutOfMemory;
        phi_inst.finish(
            result_vals[0..branch_count],
            result_blocks[0..branch_count],
            wip,
        );
        return phi_inst.toValue();
    }

    // When/match expression generation

    /// Generate a when/match expression.
    /// Evaluates the scrutinee, then checks each branch pattern sequentially.
    /// Unconditional patterns (wildcard, bind) go directly to the body.
    /// Conditional patterns (int_literal, tag) compare and branch.
    // Loop generation-------

    /// Generate a while loop: header checks condition, body executes, then loops back.
    /// Returns unit (i8 0).
    fn generateWhileLoop(self: *MonoLlvmCodeGen, wl: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Clear out_ptr for loop body
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Promote existing symbol bindings to allocas for SSA correctness
        // (same as for_loop — loop body mutations must be visible after exit)
        var promoted_keys: std.ArrayList(u64) = .{};
        defer promoted_keys.deinit(self.allocator);
        {
            var sym_it = self.symbol_values.iterator();
            while (sym_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const val = entry.value_ptr.*;
                if (self.loop_var_allocas.contains(key)) continue;
                const val_type = val.typeOfWip(wip);
                const alloca_val = wip.alloca(.normal, val_type, .none, alignment, .default, "lv") catch return error.CompilationFailed;
                _ = wip.store(.normal, val, alloca_val, alignment) catch return error.CompilationFailed;
                self.loop_var_allocas.put(key, .{ .alloca_ptr = alloca_val, .elem_type = val_type }) catch return error.OutOfMemory;
                promoted_keys.append(self.allocator, key) catch return error.OutOfMemory;
            }
        }

        // Create blocks: cond has 2 incoming (entry + back-edge), body/exit have 1 each
        const cond_block = wip.block(2, "while_cond") catch return error.OutOfMemory;
        const body_block = wip.block(1, "while_body") catch return error.OutOfMemory;
        const exit_incoming = 1 + self.countBreakEdges(wl.body);
        const exit_block = wip.block(exit_incoming, "while_exit") catch return error.OutOfMemory;
        self.loop_exit_blocks.append(self.allocator, exit_block) catch return error.OutOfMemory;
        defer _ = self.loop_exit_blocks.pop();

        // Branch to condition check
        _ = wip.br(cond_block) catch return error.CompilationFailed;

        // Condition block: load loop-carried variables before evaluating condition
        wip.cursor = .{ .block = cond_block };
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const loaded = wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(key, loaded) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const cell_alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(entry.value_ptr.elem_type), 1)));
                const loaded = wip.load(.normal, entry.value_ptr.elem_type, entry.value_ptr.alloca_ptr, cell_alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(entry.key_ptr.*, loaded) catch return error.OutOfMemory;
            }
        }
        var cond_val = try self.generateExpr(wl.cond);
        if (cond_val.typeOfWip(wip) != .i1) {
            cond_val = wip.cast(.trunc, cond_val, .i1, "") catch return error.CompilationFailed;
        }
        _ = wip.brCond(cond_val, body_block, exit_block, .none) catch return error.CompilationFailed;

        // Body block: load loop-carried variables, execute body
        wip.cursor = .{ .block = body_block };
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const loaded = wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(key, loaded) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const cell_alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(entry.value_ptr.elem_type), 1)));
                const loaded = wip.load(.normal, entry.value_ptr.elem_type, entry.value_ptr.alloca_ptr, cell_alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(entry.key_ptr.*, loaded) catch return error.OutOfMemory;
            }
        }
        _ = try self.generateExpr(wl.body);
        _ = wip.br(cond_block) catch return error.CompilationFailed;

        // Exit block: load final values from allocas
        wip.cursor = .{ .block = exit_block };
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const final_val = wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(key, final_val) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const cell_alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(entry.value_ptr.elem_type), 1)));
                const loaded = wip.load(.normal, entry.value_ptr.elem_type, entry.value_ptr.alloca_ptr, cell_alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(entry.key_ptr.*, loaded) catch return error.OutOfMemory;
            }
        }

        // Clean up loop variable allocas
        for (promoted_keys.items) |key| {
            _ = self.loop_var_allocas.remove(key);
        }

        return builder.intValue(.i8, 0) catch return error.OutOfMemory;
    }

    /// Generate a for loop over a list: iterate elements, bind pattern, execute body.
    /// Returns unit (i8 0).
    fn generateForLoop(self: *MonoLlvmCodeGen, fl: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        // Get element size for pointer arithmetic
        const elem_layout_data = ls.getLayout(fl.elem_layout);
        const elem_sa = ls.layoutSizeAlign(elem_layout_data);
        const elem_size: u32 = elem_sa.size;

        // Materialize the list as a pointer so we can read ptr/len
        const list_ptr = try self.materializeAsPtr(fl.list_expr, 24);

        // Load list data pointer (offset 0) and length (offset 8)
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;

        const len_off = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{len_off}, "") catch return error.CompilationFailed;
        const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;

        // Clear out_ptr for loop body
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        // Promote existing symbol bindings to allocas so that loop body
        // mutations are visible after the loop exit (SSA domination fix).
        // The loop body may rebind variables via let_stmts; without allocas,
        // the new SSA values from the body block don't dominate the exit block.
        var promoted_keys: std.ArrayList(u64) = .{};
        defer promoted_keys.deinit(self.allocator);
        {
            var sym_it = self.symbol_values.iterator();
            while (sym_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const val = entry.value_ptr.*;
                // Skip if already promoted (nested loops)
                if (self.loop_var_allocas.contains(key)) continue;
                const val_type = val.typeOfWip(wip);
                const alloca_val = wip.alloca(.normal, val_type, .none, alignment, .default, "lv") catch return error.CompilationFailed;
                _ = wip.store(.normal, val, alloca_val, alignment) catch return error.CompilationFailed;
                self.loop_var_allocas.put(key, .{ .alloca_ptr = alloca_val, .elem_type = val_type }) catch return error.OutOfMemory;
                promoted_keys.append(self.allocator, key) catch return error.OutOfMemory;
            }
        }

        // Create blocks: header (phi for index), body, exit
        const header_block = wip.block(2, "for_header") catch return error.OutOfMemory;
        const body_block = wip.block(1, "for_body") catch return error.OutOfMemory;
        const exit_incoming = 1 + self.countBreakEdges(fl.body);
        const exit_block = wip.block(exit_incoming, "for_exit") catch return error.OutOfMemory;
        self.loop_exit_blocks.append(self.allocator, exit_block) catch return error.OutOfMemory;
        defer _ = self.loop_exit_blocks.pop();

        // Entry → header
        const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const entry_block = wip.cursor.block;
        _ = wip.br(header_block) catch return error.CompilationFailed;

        // Header: phi for loop index, compare with length
        wip.cursor = .{ .block = header_block };
        const idx_phi = wip.phi(.i64, "idx") catch return error.CompilationFailed;
        const idx_val = idx_phi.toValue();
        const cond = wip.icmp(.ult, idx_val, list_len, "") catch return error.OutOfMemory;
        _ = wip.brCond(cond, body_block, exit_block, .none) catch return error.CompilationFailed;

        // Body: load element, bind pattern, execute body, increment index
        wip.cursor = .{ .block = body_block };

        // Load loop-carried variables from allocas at the start of the body
        // so lookups within the body see the latest values.
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const loaded = wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(key, loaded) catch return error.OutOfMemory;
            }
        }

        // Load element from data_ptr + idx * elem_size
        const size_const = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
        const byte_offset = wip.bin(.mul, idx_val, size_const, "") catch return error.CompilationFailed;
        const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;
        const elem_val = try self.loadValueFromPtr(elem_ptr, fl.elem_layout);

        // Bind the element to the pattern
        try self.bindPattern(fl.elem_pattern, elem_val);

        // Execute the body
        _ = try self.generateExpr(fl.body);

        // Increment index and loop back
        const one = builder.intValue(.i64, 1) catch return error.OutOfMemory;
        const next_idx = wip.bin(.add, idx_val, one, "") catch return error.CompilationFailed;
        const body_end_block = wip.cursor.block;
        _ = wip.br(header_block) catch return error.CompilationFailed;

        // Finish phi: entry→0, body_end→next_idx
        idx_phi.finish(
            &.{ zero, next_idx },
            &.{ entry_block, body_end_block },
            wip,
        );

        // Exit block: load final values from allocas into symbol_values
        wip.cursor = .{ .block = exit_block };
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const final_val = wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
                self.symbol_values.put(key, final_val) catch return error.OutOfMemory;
            }
        }

        // Clean up loop variable allocas (un-promote for this loop level)
        for (promoted_keys.items) |key| {
            _ = self.loop_var_allocas.remove(key);
        }

        return builder.intValue(.i8, 0) catch return error.OutOfMemory;
    }

    // Pattern matching

    fn generateMatchExpr(self: *MonoLlvmCodeGen, w: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Evaluate the scrutinee
        const scrutinee = try self.generateExpr(w.value);

        // Get branches
        const branches = self.store.getMatchBranches(w.branches);
        std.debug.assert(branches.len != 0);

        // Compute the incoming count for the merge block by scanning patterns.
        // Each branch contributes one incoming edge. Unconditional patterns
        // (wildcard/bind/struct_) stop further branches, so count up to
        // and including the first unconditional one.
        var merge_incoming: u32 = 0;
        for (branches) |branch| {
            merge_incoming += 1;
            const pat = self.store.getPattern(branch.pattern);
            if (pat == .wildcard or pat == .bind or pat == .struct_) break;
        }

        const merge_block = wip.block(merge_incoming, "when_merge") catch return error.OutOfMemory;

        // Buffers for phi node
        var result_vals: [32]LlvmBuilder.Value = undefined;
        var result_blocks: [32]LlvmBuilder.Function.Block.Index = undefined;
        var branch_count: u32 = 0;

        for (branches, 0..) |branch, i| {
            const pattern = self.store.getPattern(branch.pattern);
            const is_last = (i == branches.len - 1);

            switch (pattern) {
                .wildcard => {
                    // Always matches — generate body directly
                    const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = body_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                    break; // No more branches after wildcard
                },

                .bind => |bind| {
                    // Always matches, bind the scrutinee to the symbol
                    const symbol_key: u64 = @bitCast(bind.symbol);
                    self.symbol_values.put(symbol_key, scrutinee) catch return error.OutOfMemory;
                    const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = body_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                    break; // No more branches after bind
                },

                .int_literal => |int_pat| {
                    // Compare scrutinee with pattern value
                    const pat_type = layoutToLlvmType(int_pat.layout_idx);
                    const pat_val = builder.intValue(pat_type, @as(u64, @truncate(@as(u128, @bitCast(int_pat.value))))) catch return error.OutOfMemory;
                    // Ensure scrutinee is the right type for comparison
                    const cmp_scrutinee = if (scrutinee.typeOfWip(wip) == pat_type)
                        scrutinee
                    else
                        wip.conv(.unsigned, scrutinee, pat_type, "") catch return error.CompilationFailed;
                    const cmp = wip.icmp(.eq, cmp_scrutinee, pat_val, "") catch return error.OutOfMemory;

                    if (is_last) {
                        // Last branch — treat as default (skip comparison)
                        const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    } else {
                        const then_block = wip.block(1, "int_match") catch return error.OutOfMemory;
                        const else_block = wip.block(1, "int_next") catch return error.OutOfMemory;
                        _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                        // Then block — pattern matches
                        wip.cursor = .{ .block = then_block };
                        const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;

                        // Else block — continue to next branch
                        wip.cursor = .{ .block = else_block };
                    }
                },

                .tag => |tag_pat| {
                    // Extract discriminant and compare
                    const ls = self.layout_store orelse unreachable;
                    const stored_layout = ls.getLayout(tag_pat.union_layout);

                    // For tag unions, GEP requires a pointer. If the scrutinee is a
                    // struct value (not a pointer), materialize it to an alloca.
                    const tag_scrutinee = if (stored_layout.tag == .tag_union) ts: {
                        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                        if (scrutinee.typeOfWip(wip) == ptr_type) {
                            break :ts scrutinee;
                        }
                        const scrutinee_type = scrutinee.typeOfWip(wip);
                        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                        const alloca_ptr = wip.alloca(.normal, scrutinee_type, .none, alignment, .default, "tag_val") catch return error.CompilationFailed;
                        _ = wip.store(.normal, scrutinee, alloca_ptr, alignment) catch return error.CompilationFailed;
                        break :ts alloca_ptr;
                    } else scrutinee;

                    const discriminant = switch (stored_layout.tag) {
                        .scalar => scrutinee, // Scalar tag — value IS the discriminant
                        .tag_union => blk: {
                            const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                            const disc_type = discriminantIntType(tu_data.discriminant_size);
                            const disc_offset_val = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;
                            const disc_ptr = wip.gep(.inbounds, .i8, tag_scrutinee, &.{disc_offset_val}, "") catch return error.CompilationFailed;
                            break :blk wip.load(.normal, disc_type, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)), "") catch return error.CompilationFailed;
                        },
                        .closure, .struct_, .list, .list_of_zst, .box, .box_of_zst, .zst => unreachable,
                    };

                    const disc_type = discriminant.typeOfWip(wip);
                    const pat_disc = builder.intValue(disc_type, @as(u64, tag_pat.discriminant)) catch return error.OutOfMemory;
                    const cmp = wip.icmp(.eq, discriminant, pat_disc, "") catch return error.OutOfMemory;

                    if (is_last) {
                        // Last branch — bind payload args if needed, generate body
                        try self.bindTagPayloadArgs(tag_pat, tag_scrutinee);
                        const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    } else {
                        const then_block = wip.block(1, "tag_match") catch return error.OutOfMemory;
                        const else_block = wip.block(1, "tag_next") catch return error.OutOfMemory;
                        _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                        wip.cursor = .{ .block = then_block };
                        try self.bindTagPayloadArgs(tag_pat, tag_scrutinee);
                        const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;

                        wip.cursor = .{ .block = else_block };
                    }
                },

                .list => |list_pat| {
                    // List pattern in when: check length matches, then bind elements
                    // Guard: scrutinee must be a {ptr, i64, i64} struct
                    std.debug.assert(scrutinee != .none and scrutinee.typeOfWip(wip).isStruct(builder));
                    const prefix_patterns = self.store.getPatternSpan(list_pat.prefix);
                    const expected_len_val = builder.intValue(.i64, @as(u64, @intCast(prefix_patterns.len))) catch return error.OutOfMemory;
                    const actual_len = wip.extractValue(scrutinee, &.{1}, "") catch return error.CompilationFailed;
                    // Use >= for list rest patterns (.. as rest), == for exact-length patterns
                    const has_rest = !list_pat.rest.isNone();
                    const cmp_pred: LlvmBuilder.IntegerCondition = if (has_rest) .uge else .eq;
                    const cmp = wip.icmp(cmp_pred, actual_len, expected_len_val, "") catch return error.OutOfMemory;

                    if (is_last) {
                        // Last branch — bind elements, generate body
                        try self.bindPattern(branch.pattern, scrutinee);
                        const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    } else {
                        const then_block = wip.block(1, "list_match") catch return error.OutOfMemory;
                        const else_block = wip.block(1, "list_next") catch return error.OutOfMemory;
                        _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                        wip.cursor = .{ .block = then_block };
                        try self.bindPattern(branch.pattern, scrutinee);
                        const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;

                        wip.cursor = .{ .block = else_block };
                    }
                },

                .struct_ => {
                    // Struct pattern: always matches structurally, bind fields.
                    try self.bindPattern(branch.pattern, scrutinee);
                    const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = body_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                    break;
                },

                .float_literal, .str_literal, .as_pattern => unreachable,
            }
        }

        std.debug.assert(branch_count != 0);

        // Check if all branch values have the same type for the phi node
        wip.cursor = .{ .block = merge_block };
        const result_type = try self.layoutToLlvmTypeFull(w.result_layout);
        for (result_vals[0..branch_count], 0..) |val, i| {
            result_vals[i] = try self.coerceValueToLayout(val, w.result_layout);
            if (result_vals[i].typeOfWip(wip) != result_type) {
                const branch_expr = self.store.getExpr(branches[i].body);
                const branch_tag = @tagName(std.meta.activeTag(branch_expr));
                const branch_layout = self.getExprResultLayout(branches[i].body);
                const branch_layout_tag: ?[]const u8 = if (branch_layout) |layout_idx|
                    if (self.layout_store) |ls|
                        @tagName(ls.getLayout(layout_idx).tag)
                    else
                        null
                else
                    null;
                const block_final_tag: ?[]const u8 = switch (branch_expr) {
                    .block => |block| @tagName(std.meta.activeTag(self.store.getExpr(block.final_expr))),
                    else => null,
                };
                const block_final_layout = switch (branch_expr) {
                    .block => |block| self.getExprResultLayout(block.final_expr),
                    else => null,
                };
                std.debug.panic(
                    "generateMatchExpr result mismatch at {d}: layout={d} branch_tag={s} branch_layout={any} branch_layout_tag={any} block_final_tag={any} block_final_layout={any} expected {f}, got {f}",
                    .{
                        i,
                        @intFromEnum(w.result_layout),
                        branch_tag,
                        branch_layout,
                        branch_layout_tag,
                        block_final_tag,
                        block_final_layout,
                        result_type.fmt(builder, .percent),
                        result_vals[i].typeOfWip(wip).fmt(builder, .percent),
                    },
                );
            }
        }

        // Merge block with phi
        const phi_inst = wip.phi(result_type, "") catch return error.OutOfMemory;
        phi_inst.finish(
            result_vals[0..branch_count],
            result_blocks[0..branch_count],
            wip,
        );
        return phi_inst.toValue();
    }

    /// Bind tag payload arguments from a matched tag pattern.
    /// For tag unions with payloads, extracts field values from the tag struct.
    fn bindTagPayloadArgs(self: *MonoLlvmCodeGen, tag_pat: anytype, scrutinee: LlvmBuilder.Value) Error!void {
        const args = self.store.getPatternSpan(tag_pat.args);
        if (args.len == 0) return;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(tag_pat.union_layout);
        if (stored_layout.tag != .tag_union) return;

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const variants = ls.getTagUnionVariants(tu_data);
        if (tag_pat.discriminant >= variants.len) return;
        const variant = variants.get(tag_pat.discriminant);

        // Get the payload layout to determine field offsets
        const payload_layout = ls.getLayout(variant.payload_layout);

        if (args.len == 1) {
            const pattern_layout = self.getPatternLayoutIdx(args[0]);
            var value_ptr = scrutinee;
            var value_layout_idx = variant.payload_layout;

            while (pattern_layout != null and value_layout_idx != pattern_layout.?) {
                const current_layout = ls.getLayout(value_layout_idx);
                if (current_layout.tag != .struct_) break;

                const struct_data = ls.getStructData(current_layout.data.struct_.idx);
                if (struct_data.getFields().count != 1) break;

                const offset = ls.getStructFieldOffsetByOriginalIndex(current_layout.data.struct_.idx, 0);
                const offset_val = builder.intValue(.i32, offset) catch return error.OutOfMemory;
                value_ptr = wip.gep(.inbounds, .i8, value_ptr, &.{offset_val}, "") catch return error.CompilationFailed;
                value_layout_idx = ls.getStructFieldLayoutByOriginalIndex(current_layout.data.struct_.idx, 0);
            }

            const payload_value = try self.loadValueFromPtr(value_ptr, value_layout_idx);
            try self.bindPattern(args[0], payload_value);
            return;
        }

        if (payload_layout.tag != .struct_) return error.CompilationFailed;

        for (args, 0..) |arg_id, arg_i| {
            const field_layout = ls.getStructFieldLayoutByOriginalIndex(payload_layout.data.struct_.idx, @intCast(arg_i));
            const offset = ls.getStructFieldOffsetByOriginalIndex(payload_layout.data.struct_.idx, @intCast(arg_i));
            const offset_val = builder.intValue(.i32, offset) catch return error.OutOfMemory;
            const field_ptr = wip.gep(.inbounds, .i8, scrutinee, &.{offset_val}, "") catch return error.CompilationFailed;
            const field_value = try self.loadValueFromPtr(field_ptr, field_layout);
            try self.bindPattern(arg_id, field_value);
        }
    }

    fn generateBreakExpr(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const exit_block = self.loop_exit_blocks.getLastOrNull() orelse return error.CompilationFailed;
        _ = wip.br(exit_block) catch return error.CompilationFailed;

        const dead_block = wip.block(0, "after_break") catch return error.OutOfMemory;
        wip.cursor = .{ .block = dead_block };
        return builder.poisonValue(.i8) catch return error.OutOfMemory;
    }

    // Early return

    /// Generate an early return — stores the result to out_ptr and branches to the
    /// early return block (which contains retVoid). Used for the `?` operator.
    fn generateEarlyReturn(self: *MonoLlvmCodeGen, er: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Generate the return value
        const value_info = try self.generateExprAsValue(er.expr);
        const value = value_info.value;

        if (self.fn_out_ptr) |out_ptr| {
            // Top-level function (void return) — store to output pointer, then retVoid
            const ret_layout = self.result_layout orelse unreachable;

            const is_scalar = switch (ret_layout) {
                .bool,
                .i8,
                .i16,
                .i32,
                .i64,
                .u8,
                .u16,
                .u32,
                .u64,
                .i128,
                .u128,
                .dec,
                .f32,
                .f64,
                => true,
                else => false,
            };

            if (is_scalar) {
                const final_type: LlvmBuilder.Type = switch (ret_layout) {
                    .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
                    .i128, .u128, .dec => .i128,
                    .f32 => .float,
                    .f64 => .double,
                    else => unreachable,
                };
                const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (ret_layout) {
                    .i8, .i16, .i32, .i64, .i128 => .signed,
                    .bool, .u8, .u16, .u32, .u64, .u128 => .unsigned,
                    .f32, .f64, .dec => .unneeded,
                    else => .unneeded,
                };
                const store_value = if (value.typeOfWip(wip) == final_type)
                    value
                else
                    wip.conv(signedness, value, final_type, "") catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
                    .i64 => 8,
                    .i128 => 16,
                    .float => 4,
                    .double => 8,
                    else => 0,
                });
                _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
            } else {
                // Composite — store struct value directly to out_ptr
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                _ = wip.store(.normal, value, out_ptr, alignment) catch return error.CompilationFailed;
            }

            _ = wip.retVoid() catch return error.OutOfMemory;
        } else {
            // Inside a proc (typed return) — return the value directly
            const return_value = try self.coerceValueToLayout(value, er.ret_layout);
            _ = wip.ret(return_value) catch return error.CompilationFailed;
        }

        // Create a dead block for subsequent code after the return
        const dead_block = wip.block(0, "") catch return error.OutOfMemory;
        wip.cursor = .{ .block = dead_block };
        return builder.poisonValue(try self.layoutToLlvmTypeFull(er.ret_layout)) catch return error.OutOfMemory;
    }

    // Runtime error / unreachable

    /// Generate an LLVM unreachable instruction for runtime_error and crash expressions.
    /// Returns a poison value so the caller has something to work with
    /// (the unreachable guarantees this code is never actually reached).
    /// Generate an empty list: ptr=null(0), len=0, capacity=0.
    /// A RocList is 24 bytes (3 x i64). Write zeros to out_ptr.
    fn generateEmptyList(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        // If no out_ptr, build a zero struct directly in SSA registers
        if (self.out_ptr == null) {
            const roc_list_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
            const null_ptr = wip.cast(.inttoptr, zero, ptr_type, "") catch return error.CompilationFailed;
            var result = builder.poisonValue(roc_list_type) catch return error.OutOfMemory;
            result = wip.insertValue(result, null_ptr, &.{0}, "") catch return error.CompilationFailed;
            result = wip.insertValue(result, zero, &.{1}, "") catch return error.CompilationFailed;
            result = wip.insertValue(result, zero, &.{2}, "") catch return error.CompilationFailed;
            return result;
        }

        const dest_ptr = self.out_ptr.?;
        const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Store ptr (offset 0)
        _ = wip.store(.normal, zero, dest_ptr, alignment) catch return error.CompilationFailed;

        // Store len (offset 8)
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const ptr8 = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, zero, ptr8, alignment) catch return error.CompilationFailed;

        // Store capacity (offset 16)
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const ptr16 = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, zero, ptr16, alignment) catch return error.CompilationFailed;

        return .none;
    }

    /// Generate a list with elements: allocate heap, store elements, write RocList to out_ptr.
    fn generateList(self: *MonoLlvmCodeGen, list: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        // If no out_ptr is set, create a temporary alloca for the 24-byte RocList struct
        const needs_temp = self.out_ptr == null;
        const dest_ptr = self.out_ptr orelse blk: {
            const alloca = wip.alloca(.normal, .i64, builder.intValue(.i32, 3) catch return error.OutOfMemory, LlvmBuilder.Alignment.fromByteUnits(8), .default, "list_tmp") catch return error.CompilationFailed;
            break :blk alloca;
        };

        const elems = self.store.getExprSpan(list.elems);
        if (elems.len == 0) return self.generateEmptyList();

        // Get element layout info
        const elem_layout_data = ls.getLayout(list.elem_layout);
        const elem_sa = ls.layoutSizeAlign(elem_layout_data);
        const elem_size: u64 = elem_sa.size;
        const elem_align: u32 = @intCast(elem_sa.alignment.toByteUnits());
        const num_elems: u64 = @intCast(elems.len);
        const total_bytes: u64 = elem_size * num_elems;

        // ZST elements: no allocation needed, just set length
        if (elem_size == 0) {
            const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
            const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
            const len_val = builder.intValue(.i64, num_elems) catch return error.OutOfMemory;
            const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
            const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
            _ = wip.store(.@"volatile", zero, dest_ptr, alignment) catch return error.CompilationFailed;
            const len_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
            _ = wip.store(.@"volatile", len_val, len_ptr, alignment) catch return error.CompilationFailed;
            const cap_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
            _ = wip.store(.@"volatile", len_val, cap_ptr, alignment) catch return error.CompilationFailed;
            return .none;
        }

        // Call allocateWithRefcountC(data_bytes, elem_align, elements_refcounted, roc_ops)
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const size_val = builder.intValue(.i64, total_bytes) catch return error.OutOfMemory;
        const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
        const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;

        const heap_ptr = try self.callBuiltin("roc_builtins_allocate_with_refcount", ptr_type, &.{ .i64, .i32, .i1, ptr_type }, &.{ size_val, align_val, refcounted_val, roc_ops });

        // Store each element to heap memory
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        // Check if elements are composite (str/list) — need out_ptr for inner generation
        const is_composite_elem = (list.elem_layout == .str or elem_layout_data.tag == .list or elem_layout_data.tag == .list_of_zst);
        const is_tag_union_elem = (elem_layout_data.tag == .tag_union);

        for (elems, 0..) |elem_id, i| {
            const offset: u64 = @as(u64, @intCast(i)) * elem_size;
            const elem_ptr = if (offset == 0)
                heap_ptr
            else blk: {
                const off_val = builder.intValue(.i32, @as(u32, @intCast(offset))) catch return error.OutOfMemory;
                break :blk wip.gep(.inbounds, .i8, heap_ptr, &.{off_val}, "") catch return error.CompilationFailed;
            };

            if (is_composite_elem) {
                // Composite elements (str/list): set out_ptr to the heap slot so the
                // inner generateExpr writes the 24-byte struct directly to heap memory.
                self.out_ptr = elem_ptr;
                const elem_val = try self.generateExpr(elem_id);
                if (elem_val != .none) {
                    // If it returned a value (e.g. from a lookup), store it
                    const store_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_align, 1)));
                    _ = wip.store(.@"volatile", elem_val, elem_ptr, store_align) catch return error.CompilationFailed;
                }
                self.out_ptr = null;
            } else if (is_tag_union_elem) {
                // Tag union elements are pointer-based (alloca ptrs). Copy the full
                // tag union data (payload + discriminant) from the alloca to the heap slot.
                const elem_val = try self.generateExpr(elem_id);
                const store_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_align, 1)));
                const size_val_copy = builder.intValue(.i32, @as(u32, @intCast(elem_size))) catch return error.OutOfMemory;
                _ = wip.callMemCpy(elem_ptr, store_align, elem_val, store_align, size_val_copy, .normal, false) catch return error.CompilationFailed;
            } else {
                const elem_val = try self.generateExpr(elem_id);
                // Convert value to match element layout
                const store_val = try self.convertToFieldType(elem_val, list.elem_layout);
                const store_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_align, 1)));
                // Use volatile stores to prevent LLVM from optimizing away element writes
                _ = wip.store(.@"volatile", store_val, elem_ptr, store_align) catch return error.CompilationFailed;
            }
        }

        // Write RocList struct to out_ptr: {ptr, len, capacity}
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Store ptr (offset 0)
        _ = wip.store(.normal, heap_ptr, dest_ptr, alignment) catch return error.CompilationFailed;

        // Store len (offset 8)
        const len_val = builder.intValue(.i64, num_elems) catch return error.OutOfMemory;
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const ptr8 = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, len_val, ptr8, alignment) catch return error.CompilationFailed;

        // Store capacity (offset 16) — same as len for new lists
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const ptr16 = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, len_val, ptr16, alignment) catch return error.CompilationFailed;

        if (needs_temp) {
            // Load the 24-byte struct from the temp alloca and return it as a value
            const list_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            return wip.load(.normal, list_type, dest_ptr, alignment, "list_val") catch return error.CompilationFailed;
        }
        return .none;
    }

    fn generateRuntimeError(self: *MonoLlvmCodeGen, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        _ = wip.@"unreachable"() catch return error.CompilationFailed;
        // We need to return a value for the switch expression even though
        // this code is unreachable. Create a new block so subsequent code
        // (like phi nodes) has somewhere to live.
        const dead_block = wip.block(0, "unreachable") catch return error.OutOfMemory;
        wip.cursor = .{ .block = dead_block };
        return builder.poisonValue(try self.layoutToLlvmTypeFull(ret_layout)) catch return error.OutOfMemory;
    }

    // Low-level builtins

    /// Generate code for low-level builtin operations.
    /// Handles numeric conversions and simple operations directly as LLVM
    /// instructions.
    fn generateLowLevel(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Str/list operations write their 24-byte result to out_ptr and return .none.
        // When out_ptr is null, provide a temp alloca so these ops can write their result,
        // then load the struct value from the alloca.
        const saved_out_ptr = self.out_ptr;
        const needs_temp = self.out_ptr == null;
        var temp_alloca: LlvmBuilder.Value = .none;
        if (needs_temp) {
            const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
            temp_alloca = wip.alloca(.normal, .i64, alloca_count, LlvmBuilder.Alignment.fromByteUnits(8), .default, "ll_tmp") catch return error.CompilationFailed;
            self.out_ptr = temp_alloca;
        }

        const result = self.generateLowLevelInner(ll) catch |err| {
            if (needs_temp) self.out_ptr = saved_out_ptr;
            return err;
        };

        if (needs_temp) {
            self.out_ptr = saved_out_ptr;
            if (result == .none) {
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
                return wip.load(.normal, struct_type, temp_alloca, LlvmBuilder.Alignment.fromByteUnits(8), "ll_val") catch return error.CompilationFailed;
            }
        }

        return result;
    }

    fn generateLowLevelInner(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const args = self.store.getExprSpan(ll.args);
        self.last_low_level_name = @tagName(ll.op);

        switch (ll.op) {
            // --- Numeric arithmetic ---
            .num_plus => {
                std.debug.assert(args.len >= 2);
                var lhs = try self.generateExpr(args[0]);
                var rhs = try self.generateExpr(args[1]);
                const lhs_layout = self.getExprResultLayout(args[0]);
                const rhs_layout = self.getExprResultLayout(args[1]);
                if (ll.ret_layout == .dec) {
                    lhs = self.coerceValueToLayout(lhs, ll.ret_layout) catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "num_plus lhs coerce failed: ret_layout={d} lhs_expr={s} lhs_layout={any} rhs_expr={s} rhs_layout={any}",
                                .{ @intFromEnum(ll.ret_layout), @tagName(self.store.getExpr(args[0])), lhs_layout, @tagName(self.store.getExpr(args[1])), rhs_layout },
                            );
                        }
                        return err;
                    };
                    rhs = self.coerceValueToLayout(rhs, ll.ret_layout) catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "num_plus rhs coerce failed: ret_layout={d} lhs_expr={s} lhs_layout={any} rhs_expr={s} rhs_layout={any}",
                                .{ @intFromEnum(ll.ret_layout), @tagName(self.store.getExpr(args[0])), lhs_layout, @tagName(self.store.getExpr(args[1])), rhs_layout },
                            );
                        }
                        return err;
                    };
                    return wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed;
                }
                const is_float = isFloatLayout(ll.ret_layout);
                lhs = self.coerceValueToLayout(lhs, ll.ret_layout) catch |err| {
                    if (builtin.mode == .Debug) {
                        std.log.err(
                            "num_plus lhs coerce failed: ret_layout={d} lhs_expr={s} lhs_layout={any} rhs_expr={s} rhs_layout={any}",
                            .{ @intFromEnum(ll.ret_layout), @tagName(self.store.getExpr(args[0])), lhs_layout, @tagName(self.store.getExpr(args[1])), rhs_layout },
                        );
                    }
                    return err;
                };
                rhs = self.coerceValueToLayout(rhs, ll.ret_layout) catch |err| {
                    if (builtin.mode == .Debug) {
                        std.log.err(
                            "num_plus rhs coerce failed: ret_layout={d} lhs_expr={s} lhs_layout={any} rhs_expr={s} rhs_layout={any}",
                            .{ @intFromEnum(ll.ret_layout), @tagName(self.store.getExpr(args[0])), lhs_layout, @tagName(self.store.getExpr(args[1])), rhs_layout },
                        );
                    }
                    return err;
                };
                return if (is_float)
                    (wip.bin(.fadd, lhs, rhs, "") catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "num_plus fadd failed: ret_layout={d} lhs_expr={s} lhs_layout={any} rhs_expr={s} rhs_layout={any}",
                                .{ @intFromEnum(ll.ret_layout), @tagName(self.store.getExpr(args[0])), lhs_layout, @tagName(self.store.getExpr(args[1])), rhs_layout },
                            );
                        }
                        return err;
                    })
                else
                    (wip.bin(.add, lhs, rhs, "") catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "num_plus add failed: ret_layout={d} lhs_expr={s} lhs_layout={any} rhs_expr={s} rhs_layout={any}",
                                .{ @intFromEnum(ll.ret_layout), @tagName(self.store.getExpr(args[0])), lhs_layout, @tagName(self.store.getExpr(args[1])), rhs_layout },
                            );
                        }
                        return err;
                    });
            },
            .num_minus => {
                std.debug.assert(args.len >= 2);
                var lhs = try self.generateExpr(args[0]);
                var rhs = try self.generateExpr(args[1]);
                if (ll.ret_layout == .dec) {
                    lhs = try self.coerceValueToLayout(lhs, ll.ret_layout);
                    rhs = try self.coerceValueToLayout(rhs, ll.ret_layout);
                    return wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed;
                }
                const is_float = isFloatLayout(ll.ret_layout);
                lhs = try self.coerceValueToLayout(lhs, ll.ret_layout);
                rhs = try self.coerceValueToLayout(rhs, ll.ret_layout);
                return if (is_float)
                    wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_times => {
                std.debug.assert(args.len >= 2);
                var lhs = try self.generateExpr(args[0]);
                var rhs = try self.generateExpr(args[1]);
                if (ll.ret_layout == .dec) {
                    lhs = try self.coerceValueToLayout(lhs, ll.ret_layout);
                    rhs = try self.coerceValueToLayout(rhs, ll.ret_layout);
                    return self.callDecMul(lhs, rhs) catch return error.CompilationFailed;
                }
                const is_float = isFloatLayout(ll.ret_layout);
                lhs = try self.coerceValueToLayout(lhs, ll.ret_layout);
                rhs = try self.coerceValueToLayout(rhs, ll.ret_layout);
                return if (is_float)
                    wip.bin(.fmul, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_negate => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const is_float = isFloatLayout(ll.ret_layout);
                if (is_float) {
                    return wip.un(.fneg, operand, "") catch return error.CompilationFailed;
                } else {
                    const zero = builder.intValue(operand.typeOfWip(wip), 0) catch return error.OutOfMemory;
                    return wip.bin(.sub, zero, operand, "") catch return error.CompilationFailed;
                }
            },
            .num_abs => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const is_float = isFloatLayout(ll.ret_layout);
                if (is_float) {
                    // abs(x) = x < 0.0 ? -x : x
                    const zero = if (ll.ret_layout == .f32)
                        (builder.floatConst(0.0) catch return error.OutOfMemory).toValue()
                    else
                        (builder.doubleConst(0.0) catch return error.OutOfMemory).toValue();
                    const is_neg = wip.fcmp(.normal, .olt, operand, zero, "") catch return error.OutOfMemory;
                    const neg_val = wip.un(.fneg, operand, "") catch return error.CompilationFailed;
                    return wip.select(.normal, is_neg, neg_val, operand, "") catch return error.CompilationFailed;
                } else {
                    // abs(x) = x < 0 ? -x : x
                    const zero = builder.intValue(operand.typeOfWip(wip), 0) catch return error.OutOfMemory;
                    const is_neg = wip.icmp(.slt, operand, zero, "") catch return error.OutOfMemory;
                    const neg_val = wip.bin(.sub, zero, operand, "") catch return error.CompilationFailed;
                    return wip.select(.normal, is_neg, neg_val, operand, "") catch return error.CompilationFailed;
                }
            },

            // --- Widening integer conversions (always safe) ---
            .u8_to_i16,
            .u8_to_i32,
            .u8_to_i64,
            .u8_to_i128,
            .u8_to_u16,
            .u8_to_u32,
            .u8_to_u64,
            .u8_to_u128,
            .u16_to_i32,
            .u16_to_i64,
            .u16_to_i128,
            .u16_to_u32,
            .u16_to_u64,
            .u16_to_u128,
            .u32_to_i64,
            .u32_to_i128,
            .u32_to_u64,
            .u32_to_u128,
            .u64_to_i128,
            .u64_to_u128,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.conv(.unsigned, operand, target_type, "") catch return error.CompilationFailed;
            },

            .i8_to_i16,
            .i8_to_i32,
            .i8_to_i64,
            .i8_to_i128,
            .i8_to_u16_wrap,
            .i8_to_u32_wrap,
            .i8_to_u64_wrap,
            .i8_to_u128_wrap,
            .i16_to_i32,
            .i16_to_i64,
            .i16_to_i128,
            .i16_to_u32_wrap,
            .i16_to_u64_wrap,
            .i16_to_u128_wrap,
            .i32_to_i64,
            .i32_to_i128,
            .i32_to_u64_wrap,
            .i32_to_u128_wrap,
            .i64_to_i128,
            .i64_to_u128_wrap,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.conv(.signed, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Narrowing/wrapping integer conversions ---
            .u8_to_i8_wrap,
            .i8_to_u8_wrap,
            .u16_to_i8_wrap,
            .u16_to_i16_wrap,
            .u16_to_u8_wrap,
            .i16_to_i8_wrap,
            .i16_to_u8_wrap,
            .i16_to_u16_wrap,
            .u32_to_i8_wrap,
            .u32_to_i16_wrap,
            .u32_to_i32_wrap,
            .u32_to_u8_wrap,
            .u32_to_u16_wrap,
            .i32_to_i8_wrap,
            .i32_to_i16_wrap,
            .i32_to_u8_wrap,
            .i32_to_u16_wrap,
            .i32_to_u32_wrap,
            .u64_to_i8_wrap,
            .u64_to_i16_wrap,
            .u64_to_i32_wrap,
            .u64_to_i64_wrap,
            .u64_to_u8_wrap,
            .u64_to_u16_wrap,
            .u64_to_u32_wrap,
            .i64_to_i8_wrap,
            .i64_to_i16_wrap,
            .i64_to_i32_wrap,
            .i64_to_u8_wrap,
            .i64_to_u16_wrap,
            .i64_to_u32_wrap,
            .i64_to_u64_wrap,
            .u128_to_i8_wrap,
            .u128_to_i16_wrap,
            .u128_to_i32_wrap,
            .u128_to_i64_wrap,
            .u128_to_i128_wrap,
            .u128_to_u8_wrap,
            .u128_to_u16_wrap,
            .u128_to_u32_wrap,
            .u128_to_u64_wrap,
            .i128_to_i8_wrap,
            .i128_to_i16_wrap,
            .i128_to_i32_wrap,
            .i128_to_i64_wrap,
            .i128_to_u8_wrap,
            .i128_to_u16_wrap,
            .i128_to_u32_wrap,
            .i128_to_u64_wrap,
            .i128_to_u128_wrap,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.conv(.unsigned, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Integer to float conversions ---
            .u8_to_f32,
            .u16_to_f32,
            .u32_to_f32,
            .u64_to_f32,
            .u128_to_f32,
            .u8_to_f64,
            .u16_to_f64,
            .u32_to_f64,
            .u64_to_f64,
            .u128_to_f64,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.uitofp, operand, target_type, "") catch return error.CompilationFailed;
            },
            .i8_to_f32,
            .i16_to_f32,
            .i32_to_f32,
            .i64_to_f32,
            .i128_to_f32,
            .i8_to_f64,
            .i16_to_f64,
            .i32_to_f64,
            .i64_to_f64,
            .i128_to_f64,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.sitofp, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Float to integer conversions (truncating) ---
            .f32_to_i8_trunc,
            .f32_to_i16_trunc,
            .f32_to_i32_trunc,
            .f32_to_i64_trunc,
            .f32_to_i128_trunc,
            .f64_to_i8_trunc,
            .f64_to_i16_trunc,
            .f64_to_i32_trunc,
            .f64_to_i64_trunc,
            .f64_to_i128_trunc,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.fptosi, operand, target_type, "") catch return error.CompilationFailed;
            },
            .f32_to_u8_trunc,
            .f32_to_u16_trunc,
            .f32_to_u32_trunc,
            .f32_to_u64_trunc,
            .f32_to_u128_trunc,
            .f64_to_u8_trunc,
            .f64_to_u16_trunc,
            .f64_to_u32_trunc,
            .f64_to_u64_trunc,
            .f64_to_u128_trunc,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.fptoui, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Float to integer "try_unsafe" conversions (same as trunc, assumes no overflow) ---
            .f32_to_i8_try_unsafe,
            .f32_to_i16_try_unsafe,
            .f32_to_i32_try_unsafe,
            .f32_to_i64_try_unsafe,
            .f32_to_i128_try_unsafe,
            .f64_to_i8_try_unsafe,
            .f64_to_i16_try_unsafe,
            .f64_to_i32_try_unsafe,
            .f64_to_i64_try_unsafe,
            .f64_to_i128_try_unsafe,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.fptosi, operand, target_type, "") catch return error.CompilationFailed;
            },
            .f32_to_u8_try_unsafe,
            .f32_to_u16_try_unsafe,
            .f32_to_u32_try_unsafe,
            .f32_to_u64_try_unsafe,
            .f32_to_u128_try_unsafe,
            .f64_to_u8_try_unsafe,
            .f64_to_u16_try_unsafe,
            .f64_to_u32_try_unsafe,
            .f64_to_u64_try_unsafe,
            .f64_to_u128_try_unsafe,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.fptoui, operand, target_type, "") catch return error.CompilationFailed;
            },
            .f64_to_f32_try_unsafe => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                return wip.cast(.fptrunc, operand, .float, "") catch return error.CompilationFailed;
            },

            // --- Integer "try" conversions (return Result tag union via C wrapper) ---
            .u8_to_i8_try,
            .i8_to_u8_try,
            .i8_to_u16_try,
            .i8_to_u32_try,
            .i8_to_u64_try,
            .i8_to_u128_try,
            .u16_to_i8_try,
            .u16_to_i16_try,
            .u16_to_u8_try,
            .i16_to_i8_try,
            .i16_to_u8_try,
            .i16_to_u16_try,
            .i16_to_u32_try,
            .i16_to_u64_try,
            .i16_to_u128_try,
            .u32_to_i8_try,
            .u32_to_i16_try,
            .u32_to_i32_try,
            .u32_to_u8_try,
            .u32_to_u16_try,
            .i32_to_i8_try,
            .i32_to_i16_try,
            .i32_to_u8_try,
            .i32_to_u16_try,
            .i32_to_u32_try,
            .i32_to_u64_try,
            .i32_to_u128_try,
            .u64_to_i8_try,
            .u64_to_i16_try,
            .u64_to_i32_try,
            .u64_to_i64_try,
            .u64_to_u8_try,
            .u64_to_u16_try,
            .u64_to_u32_try,
            .i64_to_i8_try,
            .i64_to_i16_try,
            .i64_to_i32_try,
            .i64_to_u8_try,
            .i64_to_u16_try,
            .i64_to_u32_try,
            .i64_to_u64_try,
            .i64_to_u128_try,
            .u128_to_i8_try,
            .u128_to_i16_try,
            .u128_to_i32_try,
            .u128_to_i64_try,
            .u128_to_i128_try,
            .u128_to_u8_try,
            .u128_to_u16_try,
            .u128_to_u32_try,
            .u128_to_u64_try,
            .i128_to_i8_try,
            .i128_to_i16_try,
            .i128_to_i32_try,
            .i128_to_i64_try,
            .i128_to_u8_try,
            .i128_to_u16_try,
            .i128_to_u32_try,
            .i128_to_u64_try,
            .i128_to_u128_try,
            => {
                return try self.generateIntTryConversion(ll);
            },
            .u128_to_dec_try_unsafe, .i128_to_dec_try_unsafe => {
                return try self.generateDecTryUnsafeConversion(ll);
            },

            // --- Dec truncation conversions: sdiv by 10^18, then trunc ---
            .dec_to_i8_trunc,
            .dec_to_i8_try_unsafe,
            .dec_to_i16_trunc,
            .dec_to_i16_try_unsafe,
            .dec_to_i32_trunc,
            .dec_to_i32_try_unsafe,
            .dec_to_i128_trunc,
            .dec_to_i128_try_unsafe,
            .dec_to_u8_trunc,
            .dec_to_u8_try_unsafe,
            .dec_to_u16_trunc,
            .dec_to_u16_try_unsafe,
            .dec_to_u32_trunc,
            .dec_to_u32_try_unsafe,
            .dec_to_u64_trunc,
            .dec_to_u64_try_unsafe,
            .dec_to_u128_trunc,
            .dec_to_u128_try_unsafe,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                // Dec is i128 scaled by 10^18. Divide to get whole number part.
                const scale = (builder.intConst(.i128, 1_000_000_000_000_000_000) catch return error.OutOfMemory).toValue();
                const whole = wip.bin(.sdiv, operand, scale, "") catch return error.CompilationFailed;
                // Truncate to target integer type
                const target_type = layoutToLlvmType(ll.ret_layout);
                if (target_type == .i128) return whole;
                return wip.cast(.trunc, whole, target_type, "") catch return error.CompilationFailed;
            },
            .dec_to_f32_wrap, .dec_to_f32_try_unsafe => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                // Convert i128 to f64, divide by 10^18, then fptrunc to f32
                const as_f64 = wip.cast(.sitofp, operand, .double, "") catch return error.CompilationFailed;
                const scale = (builder.doubleConst(1_000_000_000_000_000_000.0) catch return error.OutOfMemory).toValue();
                const f64_result = wip.bin(.fdiv, as_f64, scale, "") catch return error.CompilationFailed;
                return wip.cast(.fptrunc, f64_result, .float, "") catch return error.CompilationFailed;
            },

            // --- Float to float conversions ---
            .f32_to_f64 => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                return wip.cast(.fpext, operand, .double, "") catch return error.CompilationFailed;
            },
            .f64_to_f32_wrap => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                return wip.cast(.fptrunc, operand, .float, "") catch return error.CompilationFailed;
            },

            // --- Integer to Dec conversion (multiply by 10^18) ---
            .u8_to_dec,
            .u16_to_dec,
            .u32_to_dec,
            .u64_to_dec,
            .i8_to_dec,
            .i16_to_dec,
            .i32_to_dec,
            .i64_to_dec,
            => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                // Extend to i128 first
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                const ext = wip.conv(
                    if (isSigned(operand_layout)) .signed else .unsigned,
                    operand,
                    .i128,
                    "",
                ) catch return error.CompilationFailed;
                // Multiply by 10^18 (Dec fixed-point scale)
                const scale = builder.intValue(.i128, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
                return wip.bin(.mul, ext, scale, "") catch return error.CompilationFailed;
            },

            .num_div_by => {
                std.debug.assert(args.len >= 2);
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                if (ll.ret_layout == .dec) {
                    return self.callDecDiv(lhs, rhs) catch return error.CompilationFailed;
                }
                const is_float = isFloatLayout(ll.ret_layout);
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                return if (is_float)
                    wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
                else if (isSigned(operand_layout))
                    wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_div_trunc_by => {
                std.debug.assert(args.len >= 2);
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                if (ll.ret_layout == .dec) {
                    return self.callDecDivTrunc(lhs, rhs) catch return error.CompilationFailed;
                }
                const is_float = isFloatLayout(ll.ret_layout);
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                return if (is_float)
                    wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
                else if (isSigned(operand_layout))
                    wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_rem_by => {
                std.debug.assert(args.len >= 2);
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                const is_float = isFloatLayout(ll.ret_layout);
                return if (is_float)
                    wip.bin(.frem, lhs, rhs, "") catch return error.CompilationFailed
                else if (isSigned(operand_layout) or operand_layout == .dec)
                    wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_mod_by => {
                std.debug.assert(args.len >= 2);
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                const is_float = isFloatLayout(ll.ret_layout);
                if (is_float) {
                    return wip.bin(.frem, lhs, rhs, "") catch return error.CompilationFailed;
                }

                if (!(isSigned(operand_layout) or operand_layout == .dec)) {
                    return wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed;
                }

                const rem = wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed;
                const zero = builder.intValue(rem.typeOfWip(wip), 0) catch return error.OutOfMemory;
                const rem_is_zero = wip.icmp(.eq, rem, zero, "") catch return error.OutOfMemory;
                const rem_is_negative = wip.icmp(.slt, rem, zero, "") catch return error.OutOfMemory;
                const rhs_is_negative = wip.icmp(.slt, rhs, zero, "") catch return error.OutOfMemory;
                const signs_differ = wip.bin(.xor, rem_is_negative, rhs_is_negative, "") catch return error.CompilationFailed;
                const rem_is_nonzero = wip.bin(.xor, rem_is_zero, builder.intValue(.i1, 1) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
                const needs_adjust = wip.bin(.@"and", rem_is_nonzero, signs_differ, "") catch return error.CompilationFailed;
                const adjusted = wip.bin(.add, rem, rhs, "") catch return error.CompilationFailed;
                return wip.select(.normal, needs_adjust, adjusted, rem, "") catch return error.CompilationFailed;
            },
            .num_pow => {
                std.debug.assert(args.len >= 2);
                const base = try self.generateExpr(args[0]);
                const exp = try self.generateExpr(args[1]);
                const is_float = isFloatLayout(ll.ret_layout);
                std.debug.assert(is_float);
                const float_type = layoutToLlvmType(ll.ret_layout);
                return wip.callIntrinsic(.normal, .none, .pow, &.{float_type}, &.{ base, exp }, "") catch return error.CompilationFailed;
            },
            .num_round => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const float_type = operand.typeOfWip(wip);
                std.debug.assert(float_type == .float or float_type == .double);
                const rounded = wip.callIntrinsic(.normal, .none, .round, &.{float_type}, &.{operand}, "") catch return error.CompilationFailed;
                // Round returns float; if ret_layout is an integer, convert
                const ret_type = layoutToLlvmType(ll.ret_layout);
                if (isIntType(ret_type)) {
                    return wip.cast(if (isSigned(ll.ret_layout)) .fptosi else .fptoui, rounded, ret_type, "") catch return error.CompilationFailed;
                }
                return rounded;
            },
            .num_floor => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const float_type = operand.typeOfWip(wip);
                std.debug.assert(float_type == .float or float_type == .double);
                const floored = wip.callIntrinsic(.normal, .none, .floor, &.{float_type}, &.{operand}, "") catch return error.CompilationFailed;
                const ret_type = layoutToLlvmType(ll.ret_layout);
                if (isIntType(ret_type)) {
                    return wip.cast(if (isSigned(ll.ret_layout)) .fptosi else .fptoui, floored, ret_type, "") catch return error.CompilationFailed;
                }
                return floored;
            },
            .num_ceiling => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const float_type = operand.typeOfWip(wip);
                std.debug.assert(float_type == .float or float_type == .double);
                const ceiled = wip.callIntrinsic(.normal, .none, .ceil, &.{float_type}, &.{operand}, "") catch return error.CompilationFailed;
                const ret_type = layoutToLlvmType(ll.ret_layout);
                if (isIntType(ret_type)) {
                    return wip.cast(if (isSigned(ll.ret_layout)) .fptosi else .fptoui, ceiled, ret_type, "") catch return error.CompilationFailed;
                }
                return ceiled;
            },
            .num_sqrt => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const float_type = operand.typeOfWip(wip);
                std.debug.assert(float_type == .float or float_type == .double);
                return wip.callIntrinsic(.normal, .none, .sqrt, &.{float_type}, &.{operand}, "") catch return error.CompilationFailed;
            },
            .num_log => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                const float_type = operand.typeOfWip(wip);
                std.debug.assert(float_type == .float or float_type == .double);
                return wip.callIntrinsic(.normal, .none, .log, &.{float_type}, &.{operand}, "") catch return error.CompilationFailed;
            },
            // --- List operations (need builtins) ---
            .list_len => {
                // List is a (ptr, len, capacity) triple — length at offset 8
                std.debug.assert(args.len >= 1);
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const len_offset = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{len_offset}, "") catch return error.CompilationFailed;
                return wip.load(.normal, .i64, len_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.CompilationFailed;
            },

            .list_get_unsafe => {
                // list_get(list, index) — load element at index from list data pointer
                std.debug.assert(args.len >= 2);
                const ls = self.layout_store orelse unreachable;
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const raw_index = try self.generateExpr(args[1]);

                // Ensure index is i64 (it may be a different int type from the literal)
                const index = if (raw_index.typeOfWip(wip) == .i64)
                    raw_index
                else
                    wip.conv(.unsigned, raw_index, .i64, "") catch return error.CompilationFailed;

                // Load data pointer from list (offset 0)
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;

                // Get element size from ret_layout
                const elem_layout = ls.getLayout(ll.ret_layout);
                const elem_sa = ls.layoutSizeAlign(elem_layout);
                const elem_size: u64 = elem_sa.size;

                // Calculate element pointer: data_ptr + index * elem_size
                const size_const = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const byte_offset = wip.bin(.mul, index, size_const, "") catch return error.CompilationFailed;
                const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;

                // Load the element
                const elem_type = try self.layoutToLlvmTypeForLoad(ll.ret_layout);
                const elem_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_sa.alignment.toByteUnits(), 1)));
                return wip.load(.normal, elem_type, elem_ptr, elem_align, "") catch return error.CompilationFailed;
            },

            .str_count_utf8_bytes => {
                // For small strings: length = byte[23] ^ 0x80
                // For heap strings: length = *(u64*)(ptr + 8)
                // Small string check: byte[23] & 0x80 != 0
                std.debug.assert(args.len >= 1);
                const str_ptr = try self.materializeAsPtr(args[0], 24);
                const byte_align = LlvmBuilder.Alignment.fromByteUnits(1);
                const word_align = LlvmBuilder.Alignment.fromByteUnits(8);

                // Load byte 23 (small string marker)
                const off23 = builder.intValue(.i32, 23) catch return error.OutOfMemory;
                const last_byte_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off23}, "") catch return error.CompilationFailed;
                const last_byte = wip.load(.normal, .i8, last_byte_ptr, byte_align, "") catch return error.CompilationFailed;

                // Check if small string: byte[23] & 0x80 != 0
                const mask = builder.intValue(.i8, 0x80) catch return error.OutOfMemory;
                const masked = wip.bin(.@"and", last_byte, mask, "") catch return error.CompilationFailed;
                const zero8 = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const is_small = wip.icmp(.ne, masked, zero8, "") catch return error.OutOfMemory;

                // Small string length: byte[23] ^ 0x80
                const small_len_u8 = wip.bin(.xor, last_byte, mask, "") catch return error.CompilationFailed;
                const small_len = wip.cast(.zext, small_len_u8, .i64, "") catch return error.CompilationFailed;

                // Heap string length: offset 8
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const heap_len = wip.load(.normal, .i64, len_ptr, word_align, "") catch return error.CompilationFailed;

                // Select based on small string flag
                return wip.select(.normal, is_small, small_len, heap_len, "") catch return error.CompilationFailed;
            },

            // --- Dec truncation/conversion operations ---
            .dec_to_i64_trunc, .dec_to_i64_try_unsafe => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                // Dec is i128 fixed-point with 10^18 scale. Truncate: divide by 10^18
                const scale = builder.intValue(.i128, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
                const divided = wip.bin(.sdiv, operand, scale, "") catch return error.CompilationFailed;
                return wip.conv(.signed, divided, .i64, "") catch return error.CompilationFailed;
            },
            .dec_to_f64 => {
                std.debug.assert(args.len >= 1);
                const operand = try self.generateExpr(args[0]);
                // Convert i128 to f64, then divide by 10^18
                const as_f64 = wip.cast(.sitofp, operand, .double, "") catch return error.CompilationFailed;
                const scale = (builder.doubleConst(1_000_000_000_000_000_000.0) catch return error.OutOfMemory).toValue();
                return wip.bin(.fdiv, as_f64, scale, "") catch return error.CompilationFailed;
            },

            .num_is_eq,
            .num_is_gt,
            .num_is_gte,
            .num_is_lt,
            .num_is_lte,
            => {
                std.debug.assert(args.len >= 2);
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                const arg_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;

                if (isFloatLayout(arg_layout)) {
                    return switch (ll.op) {
                        .num_is_eq => wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.OutOfMemory,
                        .num_is_gt => wip.fcmp(.normal, .ogt, lhs, rhs, "") catch return error.OutOfMemory,
                        .num_is_gte => wip.fcmp(.normal, .oge, lhs, rhs, "") catch return error.OutOfMemory,
                        .num_is_lt => wip.fcmp(.normal, .olt, lhs, rhs, "") catch return error.OutOfMemory,
                        .num_is_lte => wip.fcmp(.normal, .ole, lhs, rhs, "") catch return error.OutOfMemory,
                        else => unreachable,
                    };
                }

                const signed = isSigned(arg_layout);
                return switch (ll.op) {
                    .num_is_eq => wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory,
                    .num_is_gt => if (signed)
                        wip.icmp(.sgt, lhs, rhs, "") catch return error.OutOfMemory
                    else
                        wip.icmp(.ugt, lhs, rhs, "") catch return error.OutOfMemory,
                    .num_is_gte => if (signed)
                        wip.icmp(.sge, lhs, rhs, "") catch return error.OutOfMemory
                    else
                        wip.icmp(.uge, lhs, rhs, "") catch return error.OutOfMemory,
                    .num_is_lt => if (signed)
                        wip.icmp(.slt, lhs, rhs, "") catch return error.OutOfMemory
                    else
                        wip.icmp(.ult, lhs, rhs, "") catch return error.OutOfMemory,
                    .num_is_lte => if (signed)
                        wip.icmp(.sle, lhs, rhs, "") catch return error.OutOfMemory
                    else
                        wip.icmp(.ule, lhs, rhs, "") catch return error.OutOfMemory,
                    else => unreachable,
                };
            },

            // --- Comparison ---
            .compare => {
                // compare(a, b) -> {-1, 0, 1} as i8
                std.debug.assert(args.len >= 2);
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                // Get the layout of the first argument to determine comparison type
                const arg_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                const is_float = isFloatLayout(arg_layout);
                if (is_float) {
                    const lt = wip.fcmp(.normal, .olt, lhs, rhs, "") catch return error.OutOfMemory;
                    const gt = wip.fcmp(.normal, .ogt, lhs, rhs, "") catch return error.OutOfMemory;
                    const lt_val = wip.conv(.unsigned, lt, .i8, "") catch return error.CompilationFailed;
                    const gt_val = wip.conv(.unsigned, gt, .i8, "") catch return error.CompilationFailed;
                    // result = gt - lt => 1 if gt, -1 (255 unsigned but signed -1) if lt, 0 if eq
                    return wip.bin(.sub, gt_val, lt_val, "") catch return error.CompilationFailed;
                } else {
                    const lt = wip.icmp(.slt, lhs, rhs, "") catch return error.OutOfMemory;
                    const gt = wip.icmp(.sgt, lhs, rhs, "") catch return error.OutOfMemory;
                    const lt_val = wip.conv(.unsigned, lt, .i8, "") catch return error.CompilationFailed;
                    const gt_val = wip.conv(.unsigned, gt, .i8, "") catch return error.CompilationFailed;
                    return wip.bin(.sub, gt_val, lt_val, "") catch return error.CompilationFailed;
                }
            },

            // --- Crash ---
            .crash => {
                _ = wip.@"unreachable"() catch return error.CompilationFailed;
                const dead_block = wip.block(0, "after_crash") catch return error.OutOfMemory;
                wip.cursor = .{ .block = dead_block };
                return builder.intValue(.i64, 0) catch return error.OutOfMemory;
            },

            // --- String operations via decomposed wrappers ---
            .str_concat => {
                // str_concat(a, b) -> RocStr (written to out_ptr)
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Str(args[0], args[1], "roc_builtins_str_concat");
            },
            .str_is_eq => {
                // str_is_eq(a, b) -> Bool
                std.debug.assert(args.len >= 2);

                return try self.callStrStr2Bool(args[0], args[1], "roc_builtins_str_equal");
            },
            .str_contains => {
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Bool(args[0], args[1], "roc_builtins_str_contains");
            },
            .str_starts_with => {
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Bool(args[0], args[1], "roc_builtins_str_starts_with");
            },
            .str_ends_with => {
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Bool(args[0], args[1], "roc_builtins_str_ends_with");
            },

            // --- Unsupported string operations ---
            .str_to_utf8 => {
                // str_to_utf8(str) -> List(U8) (written to out_ptr)
                // Str and List(U8) have the same decomposed layout, so callStr2Str works
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_to_utf8");
            },

            .str_caseless_ascii_equals => {
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Bool(args[0], args[1], "roc_builtins_str_caseless_ascii_equals");
            },
            .str_repeat => {
                std.debug.assert(args.len >= 2);
                return try self.callStrU642Str(args[0], args[1], "roc_builtins_str_repeat");
            },
            .str_trim => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_trim");
            },
            .str_trim_start => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_trim_start");
            },
            .str_trim_end => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_trim_end");
            },
            .str_drop_prefix => {
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Str(args[0], args[1], "roc_builtins_str_drop_prefix");
            },
            .str_drop_suffix => {
                std.debug.assert(args.len >= 2);
                return try self.callStrStr2Str(args[0], args[1], "roc_builtins_str_drop_suffix");
            },
            .str_with_ascii_lowercased => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_with_ascii_lowercased");
            },
            .str_with_ascii_uppercased => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_with_ascii_uppercased");
            },
            .str_with_capacity => {
                std.debug.assert(args.len >= 1);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

                const saved_out_ptr = self.out_ptr;
                self.out_ptr = null;
                const cap_val = try self.generateExpr(args[0]);
                self.out_ptr = saved_out_ptr;

                _ = try self.callBuiltin("roc_builtins_str_with_capacity", .void, &.{ ptr_type, .i64, ptr_type }, &.{ dest_ptr, cap_val, roc_ops });
                return .none;
            },
            .str_reserve => {
                std.debug.assert(args.len >= 2);
                return try self.callStrU642Str(args[0], args[1], "roc_builtins_str_reserve");
            },
            .str_release_excess_capacity => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_release_excess_capacity");
            },

            .str_split_on => {
                // str_split_on(string, delimiter) -> List(Str) (written to out_ptr)
                std.debug.assert(args.len >= 2);
                // Same pattern as callStrStr2Str but writes List instead of Str (both 24 bytes)
                return try self.callStrStr2Str(args[0], args[1], "roc_builtins_str_split");
            },
            .str_join_with => {
                // str_join_with(list, separator) -> Str (written to out_ptr)
                std.debug.assert(args.len >= 2);
                // list is first arg, separator is second; both are 24-byte structs
                return try self.callStrStr2Str(args[0], args[1], "roc_builtins_str_join_with");
            },

            .str_from_utf8_lossy => {
                std.debug.assert(args.len >= 1);
                return try self.callStr2Str(args[0], "roc_builtins_str_from_utf8_lossy");
            },
            .str_from_utf8 => {
                std.debug.assert(args.len >= 1);
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const raw_type = builder.arrayType(from_utf8_try_size, .i8) catch return error.OutOfMemory;
                const raw_align = LlvmBuilder.Alignment.fromByteUnits(@alignOf(usize));
                const raw_ptr = wip.alloca(.normal, raw_type, .none, raw_align, .default, "from_utf8_raw") catch return error.CompilationFailed;
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                const ls = self.layout_store orelse unreachable;
                const ret_layout_val = ls.getLayout(ll.ret_layout);
                std.debug.assert(ret_layout_val.tag == .tag_union);
                const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
                const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const total_size_val = builder.intValue(.i32, tu_data.size) catch return error.OutOfMemory;
                _ = wip.callMemSet(dest_ptr, alignment, zero_byte, total_size_val, .normal, false) catch return error.CompilationFailed;

                _ = try self.callBuiltin("roc_builtins_str_from_utf8", .void, &.{ ptr_type, ptr_type, .i64, .i64, ptr_type }, &.{ raw_ptr, data_ptr, list_len, list_cap, roc_ops });

                const ok_ptr = wip.gep(.inbounds, .i8, raw_ptr, &.{builder.intValue(.i32, from_utf8_try_is_ok_offset) catch return error.OutOfMemory}, "") catch return error.CompilationFailed;
                const is_ok = wip.load(.normal, .i8, ok_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.CompilationFailed;
                const ok_is_zero = wip.icmp(.eq, is_ok, builder.intValue(.i8, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;

                const ok_block = wip.block(1, "str_from_utf8_ok") catch return error.OutOfMemory;
                const err_block = wip.block(1, "str_from_utf8_err") catch return error.OutOfMemory;
                const cont_block = wip.block(2, "str_from_utf8_done") catch return error.OutOfMemory;
                _ = wip.brCond(ok_is_zero, err_block, ok_block, .none) catch return error.OutOfMemory;

                wip.cursor = .{ .block = ok_block };
                {
                    const string_ptr = wip.gep(.inbounds, .i8, raw_ptr, &.{builder.intValue(.i32, from_utf8_try_string_offset) catch return error.OutOfMemory}, "") catch return error.CompilationFailed;
                    const string_size_val = builder.intValue(.i32, roc_str_abi_size) catch return error.OutOfMemory;
                    _ = wip.callMemCpy(dest_ptr, alignment, string_ptr, alignment, string_size_val, .normal, false) catch return error.CompilationFailed;

                    const disc_type = discriminantIntType(tu_data.discriminant_size);
                    const disc_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory}, "") catch return error.CompilationFailed;
                    _ = wip.store(.normal, builder.intValue(disc_type, 1) catch return error.OutOfMemory, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(@as(u64, tu_data.discriminant_size), 1))) catch return error.CompilationFailed;
                    _ = wip.br(cont_block) catch return error.OutOfMemory;
                }

                wip.cursor = .{ .block = err_block };
                {
                    const index_ptr = raw_ptr;
                    const problem_ptr = wip.gep(.inbounds, .i8, raw_ptr, &.{builder.intValue(.i32, from_utf8_try_problem_code_offset) catch return error.OutOfMemory}, "") catch return error.CompilationFailed;
                    const err_index = wip.load(.normal, .i64, index_ptr, alignment, "") catch return error.CompilationFailed;
                    const err_problem = wip.load(.normal, .i8, problem_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.CompilationFailed;

                    _ = wip.store(.normal, err_index, dest_ptr, alignment) catch return error.CompilationFailed;
                    const problem_dest_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{builder.intValue(.i32, 8) catch return error.OutOfMemory}, "") catch return error.CompilationFailed;
                    _ = wip.store(.normal, err_problem, problem_dest_ptr, LlvmBuilder.Alignment.fromByteUnits(1)) catch return error.CompilationFailed;

                    const disc_type = discriminantIntType(tu_data.discriminant_size);
                    const disc_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory}, "") catch return error.CompilationFailed;
                    _ = wip.store(.normal, builder.intValue(disc_type, 0) catch return error.OutOfMemory, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(@as(u64, tu_data.discriminant_size), 1))) catch return error.CompilationFailed;
                    _ = wip.br(cont_block) catch return error.OutOfMemory;
                }

                wip.cursor = .{ .block = cont_block };
                return .none;
            },
            .num_from_str => {
                return try self.generateNumFromStr(ll, args);
            },

            .list_append_unsafe => {
                // list_append_unsafe(list, element) -> new_list
                std.debug.assert(args.len >= 2);
                const ls = self.layout_store orelse unreachable;
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;

                // Materialize the list to get ptr/len/cap
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

                // Load list fields: data_ptr (offset 0), len (offset 8), cap (offset 16)
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                // Get element size/alignment from the list's element layout
                const ret_layout = ls.getLayout(ll.ret_layout);
                if (ret_layout.tag == .list_of_zst) {
                    // ZST list: just increment the length, no data allocation needed.
                    const new_len = wip.bin(.add, list_len, builder.intValue(.i64, 1) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
                    _ = wip.store(.@"volatile", data_ptr, dest_ptr, alignment) catch return error.CompilationFailed;
                    const dest_len_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
                    _ = wip.store(.@"volatile", new_len, dest_len_ptr, alignment) catch return error.CompilationFailed;
                    const dest_cap_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
                    _ = wip.store(.@"volatile", new_len, dest_cap_ptr, alignment) catch return error.CompilationFailed;
                    return .none;
                }
                const elem_info = try self.getListElementInfo(ll.ret_layout);
                const elem_size = elem_info.elem_size;
                const elem_align = elem_info.elem_align;

                // Create element alloca with proper size
                const elem_alignment = LlvmBuilder.Alignment.fromByteUnits(@max(elem_align, 1));
                const elem_count: i32 = @intCast(elem_size);
                const elem_alloca = wip.alloca(.normal, .i8, builder.intValue(.i32, elem_count) catch return error.OutOfMemory, elem_alignment, .default, "elem") catch return error.CompilationFailed;

                // Generate element value — set out_ptr to alloca for composite types
                const saved_out_ptr = self.out_ptr;
                self.out_ptr = elem_alloca;
                defer self.out_ptr = saved_out_ptr;
                var elem_val = try self.generateExpr(args[1]);
                if (elem_val != .none) {
                    elem_val = try self.coerceValueToLayout(elem_val, elem_info.elem_layout_idx);
                    _ = wip.store(.normal, elem_val, elem_alloca, elem_alignment) catch return error.CompilationFailed;
                }

                // Call roc_builtins_list_append_safe(out, bytes, len, cap, elem, align, width, roc_ops)
                const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_append_safe", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, ptr_type, .i32, .i64, .i1, ptr_type,
                }, &.{
                    dest_ptr, data_ptr, list_len, list_cap, elem_alloca, align_val, width_val, elements_refcounted_val, roc_ops,
                });

                return .none;
            },

            .list_prepend => {
                // list_prepend(list, element) -> new_list
                std.debug.assert(args.len >= 2);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

                // Materialize the list
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                // Get element layout info
                const elem_info = try self.getListElementInfo(ll.ret_layout);
                const elem_size = elem_info.elem_size;
                const elem_align = elem_info.elem_align;

                // Materialize element to alloca
                const elem_alignment = LlvmBuilder.Alignment.fromByteUnits(@max(elem_align, 1));
                const elem_count: i32 = @intCast(elem_size);
                const elem_alloca = wip.alloca(.normal, .i8, builder.intValue(.i32, elem_count) catch return error.OutOfMemory, elem_alignment, .default, "prep_elem") catch return error.CompilationFailed;

                const saved_out_ptr = self.out_ptr;
                self.out_ptr = elem_alloca;
                defer self.out_ptr = saved_out_ptr;
                var elem_val = try self.generateExpr(args[1]);
                if (elem_val != .none) {
                    elem_val = try self.coerceValueToLayout(elem_val, elem_info.elem_layout_idx);
                    _ = wip.store(.normal, elem_val, elem_alloca, elem_alignment) catch return error.CompilationFailed;
                }

                // Call roc_builtins_list_prepend(out, bytes, len, cap, elem, align, width, roc_ops)
                const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_prepend", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, .i32, ptr_type, .i64, .i1, ptr_type,
                }, &.{
                    dest_ptr, data_ptr, list_len, list_cap, align_val, elem_alloca, width_val, elements_refcounted_val, roc_ops,
                });
                return .none;
            },

            .list_concat => {
                // list_concat(list_a, list_b) -> new_list
                std.debug.assert(args.len >= 2);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

                // Materialize both lists
                const a_ptr = try self.materializeAsPtr(args[0], 24);
                const b_ptr = try self.materializeAsPtr(args[1], 24);

                // Load list A fields
                const a_bytes = wip.load(.normal, ptr_type, a_ptr, alignment, "") catch return error.CompilationFailed;
                const a_len_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;
                const a_cap_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const a_cap = wip.load(.normal, .i64, a_cap_ptr, alignment, "") catch return error.CompilationFailed;

                // Load list B fields
                const b_bytes = wip.load(.normal, ptr_type, b_ptr, alignment, "") catch return error.CompilationFailed;
                const b_len_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;
                const b_cap_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const b_cap = wip.load(.normal, .i64, b_cap_ptr, alignment, "") catch return error.CompilationFailed;

                // Get element layout info
                const elem_info = try self.getListElementInfo(ll.ret_layout);

                // Call roc_builtins_list_concat(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, align, elem_width, roc_ops)
                const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_concat", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, ptr_type, .i64, .i64, .i32, .i64, .i1, ptr_type,
                }, &.{
                    dest_ptr, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, align_val, width_val, elements_refcounted_val, roc_ops,
                });
                return .none;
            },

            .list_with_capacity => {
                // list_with_capacity(capacity) -> empty list with given capacity
                std.debug.assert(args.len >= 1);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

                // Get element layout info
                const elem_info = try self.getListElementInfo(ll.ret_layout);

                // Generate capacity arg
                const saved_out_ptr = self.out_ptr;
                self.out_ptr = null;
                const cap_val = try self.generateExpr(args[0]);
                self.out_ptr = saved_out_ptr;

                // Call roc_builtins_list_with_capacity(out, cap, align, width, roc_ops)
                const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_with_capacity", .void, &.{
                    ptr_type, .i64, .i32, .i64, .i1, ptr_type,
                }, &.{
                    dest_ptr, cap_val, align_val, width_val, elements_refcounted_val, roc_ops,
                });
                return .none;
            },

            .list_reserve => {
                // list_reserve(list, spare) -> new_list
                std.debug.assert(args.len >= 2);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

                // Materialize list
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const list_bytes = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                // Generate spare arg
                const saved_out_ptr = self.out_ptr;
                self.out_ptr = null;
                const spare_val = try self.generateExpr(args[1]);
                self.out_ptr = saved_out_ptr;

                // Get element layout info
                const elem_info = try self.getListElementInfo(ll.ret_layout);

                // Call roc_builtins_list_reserve(out, bytes, len, cap, spare, align, width, roc_ops)
                const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_reserve", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, .i32, .i64, .i64, .i1, ptr_type,
                }, &.{
                    dest_ptr, list_bytes, list_len, list_cap, align_val, spare_val, width_val, elements_refcounted_val, roc_ops,
                });
                return .none;
            },

            .list_release_excess_capacity => {
                // list_release_excess_capacity(list) -> new_list
                std.debug.assert(args.len >= 1);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

                // Materialize list
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const list_bytes = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                // Get element layout info
                const elem_info = try self.getListElementInfo(ll.ret_layout);

                // Call roc_builtins_list_release_excess_capacity(out, bytes, len, cap, align, width, roc_ops)
                const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_release_excess_capacity", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, .i32, .i64, .i1, ptr_type,
                }, &.{
                    dest_ptr, list_bytes, list_len, list_cap, align_val, width_val, elements_refcounted_val, roc_ops,
                });
                return .none;
            },

            .list_sort_with => {
                std.debug.assert(args.len >= 2);
                return try self.generateListSortWith(args[0], args[1], ll.ret_layout);
            },

            .list_first => {
                // list_first(list) -> element at index 0
                std.debug.assert(args.len >= 1);
                const ls = self.layout_store orelse unreachable;
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;

                const elem_layout = ls.getLayout(ll.ret_layout);
                const elem_sa = ls.layoutSizeAlign(elem_layout);
                const elem_type = try self.layoutToLlvmTypeForLoad(ll.ret_layout);
                const elem_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_sa.alignment.toByteUnits(), 1)));
                return wip.load(.normal, elem_type, data_ptr, elem_align, "") catch return error.CompilationFailed;
            },

            .list_last => {
                // list_last(list) -> element at index (len - 1)
                std.debug.assert(args.len >= 1);
                const ls = self.layout_store orelse unreachable;
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;

                // Load length (offset 8)
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;

                // Calculate offset: (len - 1) * elem_size
                const elem_layout = ls.getLayout(ll.ret_layout);
                const elem_sa = ls.layoutSizeAlign(elem_layout);
                const elem_size: u64 = elem_sa.size;
                const one = builder.intValue(.i64, 1) catch return error.OutOfMemory;
                const last_idx = wip.bin(.sub, list_len, one, "") catch return error.CompilationFailed;
                const size_const = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const byte_offset = wip.bin(.mul, last_idx, size_const, "") catch return error.CompilationFailed;
                const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;

                const elem_type = try self.layoutToLlvmTypeForLoad(ll.ret_layout);
                const elem_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_sa.alignment.toByteUnits(), 1)));
                return wip.load(.normal, elem_type, elem_ptr, elem_align, "") catch return error.CompilationFailed;
            },

            .list_take_first => {
                // list_take_first(list, n) -> sublist(list, start=0, count=n)
                std.debug.assert(args.len >= 2);
                // list_sublist is available via roc_builtins_list_sublist
                const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const saved = self.out_ptr;
                self.out_ptr = null;
                const n_val = try self.generateExpr(args[1]);
                self.out_ptr = saved;
                return try self.callListSublist(args[0], zero, n_val, ll);
            },

            .list_sublist => {
                // list_sublist(list, { start, len }) -> sublist(list, start, len)
                std.debug.assert(args.len == 2);
                const ls = self.layout_store orelse unreachable;
                const range_layout_idx = self.getExprResultLayout(args[1]) orelse return error.CompilationFailed;
                const range_layout = ls.getLayout(range_layout_idx);
                if (range_layout.tag != .struct_) return error.CompilationFailed;

                const record_idx = range_layout.data.struct_.idx;
                const record_size = ls.getStructData(record_idx).size;
                const len_offset = ls.getStructFieldOffsetByOriginalIndex(record_idx, 0);
                const start_offset = ls.getStructFieldOffsetByOriginalIndex(record_idx, 1);

                if (builtin.mode == .Debug) {
                    const fields = ls.struct_fields.sliceRange(ls.getStructData(record_idx).getFields());
                    if (fields.len != 2 or
                        record_size != 16 or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .u64 or
                        ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .u64)
                    {
                        std.debug.panic(
                            "LLVM list_sublist expected {{ len: U64, start: U64 }} record, got layout {d}",
                            .{@intFromEnum(range_layout_idx)},
                        );
                    }
                }

                const range_ptr = try self.materializeAsPtr(args[1], @intCast(record_size));
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const start_ptr = wip.gep(
                    .inbounds,
                    .i8,
                    range_ptr,
                    &.{builder.intValue(.i32, @as(u32, @intCast(start_offset))) catch return error.OutOfMemory},
                    "",
                ) catch return error.CompilationFailed;
                const count_ptr = wip.gep(
                    .inbounds,
                    .i8,
                    range_ptr,
                    &.{builder.intValue(.i32, @as(u32, @intCast(len_offset))) catch return error.OutOfMemory},
                    "",
                ) catch return error.CompilationFailed;
                const start_val = wip.load(.normal, .i64, start_ptr, alignment, "") catch return error.CompilationFailed;
                const count_val = wip.load(.normal, .i64, count_ptr, alignment, "") catch return error.CompilationFailed;
                return try self.callListSublist(args[0], start_val, count_val, ll);
            },

            .list_take_last => {
                // list_take_last(list, n) -> sublist(list, start=max(0,len-n), count=n)
                std.debug.assert(args.len >= 2);
                // list_sublist is available via roc_builtins_list_sublist
                const saved = self.out_ptr;
                self.out_ptr = null;
                const n_val = try self.generateExpr(args[1]);
                self.out_ptr = saved;
                // Load list length
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                // start = max(0, len - n)
                const diff = wip.bin(.sub, list_len, n_val, "") catch return error.CompilationFailed;
                const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const is_neg = wip.icmp(.slt, diff, zero, "") catch return error.OutOfMemory;
                const start = wip.select(.normal, is_neg, zero, diff, "") catch return error.CompilationFailed;
                return try self.callListSublistFromPtr(list_ptr, start, n_val, ll);
            },

            .list_drop_first => {
                // list_drop_first(list, n) -> sublist(list, start=n, count=max(0,len-n))
                std.debug.assert(args.len >= 2);
                // list_sublist is available via roc_builtins_list_sublist
                const saved = self.out_ptr;
                self.out_ptr = null;
                const n_val = try self.generateExpr(args[1]);
                self.out_ptr = saved;
                // Load list length
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                // count = max(0, len - n)
                const diff = wip.bin(.sub, list_len, n_val, "") catch return error.CompilationFailed;
                const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const is_neg = wip.icmp(.slt, diff, zero, "") catch return error.OutOfMemory;
                const count = wip.select(.normal, is_neg, zero, diff, "") catch return error.CompilationFailed;
                return try self.callListSublistFromPtr(list_ptr, n_val, count, ll);
            },

            .list_drop_last => {
                // list_drop_last(list, n) -> sublist(list, start=0, count=max(0,len-n))
                std.debug.assert(args.len >= 2);
                // list_sublist is available via roc_builtins_list_sublist
                const saved = self.out_ptr;
                self.out_ptr = null;
                const n_val = try self.generateExpr(args[1]);
                self.out_ptr = saved;
                // Load list length
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                // count = max(0, len - n)
                const diff = wip.bin(.sub, list_len, n_val, "") catch return error.CompilationFailed;
                const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const is_neg = wip.icmp(.slt, diff, zero, "") catch return error.OutOfMemory;
                const count = wip.select(.normal, is_neg, zero, diff, "") catch return error.CompilationFailed;
                return try self.callListSublistFromPtr(list_ptr, zero, count, ll);
            },

            .list_set => {
                // list_set(list, index, element) -> new_list
                std.debug.assert(args.len >= 3);
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

                // Get element layout info from the return list type
                const elem_info = try self.getListElementInfo(ll.ret_layout);
                const elem_size = elem_info.elem_size;
                const elem_align = elem_info.elem_align;

                // Materialize list as pointer, generate index and element
                const list_ptr = try self.materializeAsPtr(args[0], 24);

                const saved = self.out_ptr;
                self.out_ptr = null;
                const index_val = try self.generateExpr(args[1]);
                self.out_ptr = saved;

                // Materialize element to stack
                const elem_ptr = try self.materializeAsPtr(args[2], @intCast(elem_size));

                // Allocate scratch for old element (required by listReplace)
                const old_elem_size: u32 = @intCast(if (elem_size > 0) elem_size else 8);
                const old_elem_alloca = wip.alloca(.normal, .i8, builder.intValue(.i32, old_elem_size) catch return error.OutOfMemory, alignment, .default, "old_elem") catch return error.CompilationFailed;

                // Decompose list
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
                const list_bytes = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;

                // roc_builtins_list_replace(out, bytes, len, cap, align, index, elem, width, old_elem, elements_refcounted, roc_ops)
                const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_replace", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, .i32, .i64, ptr_type, .i64, ptr_type, .i1, ptr_type,
                }, &.{
                    dest_ptr, list_bytes, list_len, list_cap, align_val, index_val, elem_ptr, width_val, old_elem_alloca, elements_refcounted_val, roc_ops,
                });
                return .none;
            },

            .list_reverse => {
                // list_reverse(list) -> new_list
                std.debug.assert(args.len >= 1);
                const ls = self.layout_store orelse unreachable;
                const dest_ptr = self.out_ptr orelse unreachable;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

                const ret_layout = ls.getLayout(ll.ret_layout);
                const elem_layout_idx = if (ret_layout.tag == .list) ret_layout.data.list else unreachable;
                const elem_sa = ls.layoutSizeAlign(ls.getLayout(elem_layout_idx));
                const elem_size: u64 = elem_sa.size;
                const elem_align: u32 = @intCast(elem_sa.alignment.toByteUnits());

                const list_ptr = try self.materializeAsPtr(args[0], 24);

                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
                const list_bytes = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;

                // roc_builtins_list_reverse(out, bytes, len, cap, align, width, roc_ops)
                const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
                const width_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;

                _ = try self.callBuiltin("roc_builtins_list_reverse", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, .i32, .i64, ptr_type,
                }, &.{
                    dest_ptr, list_bytes, list_len, list_cap, align_val, width_val, roc_ops,
                });
                return .none;
            },

            .list_contains => {
                // list_contains(list, needle) -> Bool
                // Inline loop: iterate through list, compare each element
                std.debug.assert(args.len >= 2);
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

                // Generate needle value first to determine element type
                const saved = self.out_ptr;
                self.out_ptr = null;
                const needle = try self.generateExpr(args[1]);
                self.out_ptr = saved;

                const elem_type = needle.typeOfWip(wip);
                const elem_size: u64 = llvmTypeByteSize(elem_type);
                std.debug.assert(elem_size != 0);
                const elem_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(elem_size));
                const is_float = (elem_type == .float or elem_type == .double);

                // Materialize list
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr_val = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr_val, alignment, "") catch return error.CompilationFailed;

                // Build loop: header -> body -> exit
                const header_block = wip.block(2, "contains_hdr") catch return error.OutOfMemory;
                const body_block = wip.block(1, "contains_body") catch return error.OutOfMemory;
                const found_block = wip.block(1, "contains_found") catch return error.OutOfMemory;
                const exit_block = wip.block(3, "contains_exit") catch return error.OutOfMemory;

                const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const one_i64 = builder.intValue(.i64, 1) catch return error.OutOfMemory;
                const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;

                const entry_block = wip.cursor.block;
                _ = wip.br(header_block) catch return error.CompilationFailed;

                // Header: phi for counter, compare with len
                wip.cursor = .{ .block = header_block };
                const counter_phi = wip.phi(.i64, "ctr") catch return error.CompilationFailed;
                const ctr_val = counter_phi.toValue();
                const cond = wip.icmp(.ult, ctr_val, list_len, "") catch return error.OutOfMemory;
                _ = wip.brCond(cond, body_block, exit_block, .none) catch return error.CompilationFailed;

                // Body: load element, compare with needle
                wip.cursor = .{ .block = body_block };
                const size_const = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const byte_offset = wip.bin(.mul, ctr_val, size_const, "") catch return error.CompilationFailed;
                const elem_ptr_val = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;
                const elem_val = wip.load(.normal, elem_type, elem_ptr_val, elem_align, "") catch return error.CompilationFailed;

                // Compare based on type (integer or float)
                const is_equal = if (is_float)
                    wip.fcmp(.normal, .oeq, elem_val, needle, "") catch return error.OutOfMemory
                else
                    wip.icmp(.eq, elem_val, needle, "") catch return error.OutOfMemory;

                _ = wip.brCond(is_equal, found_block, header_block, .none) catch return error.CompilationFailed;

                // Increment counter for back-edge
                const next_ctr = wip.bin(.add, ctr_val, one_i64, "") catch return error.CompilationFailed;
                const body_end_block = wip.cursor.block;

                // Found block
                wip.cursor = .{ .block = found_block };
                _ = wip.br(exit_block) catch return error.CompilationFailed;

                // Finish counter phi: entry->0, body_end->next_ctr
                counter_phi.finish(
                    &.{ zero_i64, next_ctr },
                    &.{ entry_block, body_end_block },
                    wip,
                );

                // Exit block: phi for result (false from header, true from found)
                wip.cursor = .{ .block = exit_block };
                const result_phi = wip.phi(.i1, "result") catch return error.CompilationFailed;
                result_phi.finish(
                    &.{ false_val, true_val },
                    &.{ header_block, found_block },
                    wip,
                );
                return result_phi.toValue();
            },

            else => std.debug.panic(
                "LLVM backend missing LowLevel handler for {s}",
                .{@tagName(ll.op)},
            ),
        }
    }

    fn generateNumFromStr(self: *MonoLlvmCodeGen, ll: anytype, args: []const LirExprId) Error!LlvmBuilder.Value {
        if (args.len != 1) unreachable;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        const ret_layout_val = ls.getLayout(ll.ret_layout);
        if (ret_layout_val.tag != .tag_union) {
            return error.CompilationFailed;
        }
        const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const total_size_val = builder.intValue(.i32, tu_data.size) catch return error.OutOfMemory;
        _ = wip.callMemSet(dest_ptr, alignment, zero_byte, total_size_val, .normal, false) catch return error.CompilationFailed;

        const variants = ls.getTagUnionVariants(tu_data);
        var ok_payload_idx: ?layout.Idx = null;
        for (0..variants.len) |i| {
            const payload_layout = variants.get(@intCast(i)).payload_layout;
            const candidate = self.unwrapSingleFieldPayloadLayout(payload_layout) orelse payload_layout;
            switch (candidate) {
                .u8, .u16, .u32, .u64, .u128,
                .i8, .i16, .i32, .i64, .i128,
                .f32, .f64, .dec,
                => {
                    ok_payload_idx = candidate;
                    break;
                },
                else => {},
            }
        }
        const payload_idx = ok_payload_idx orelse return error.CompilationFailed;

        const str_ptr = try self.materializeAsPtr(args[0], 24);
        const str_bytes = wip.load(.normal, ptr_type, str_ptr, alignment, "") catch return error.CompilationFailed;
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const str_len_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const str_len = wip.load(.normal, .i64, str_len_ptr, alignment, "") catch return error.CompilationFailed;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const str_cap_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const str_cap = wip.load(.normal, .i64, str_cap_ptr, alignment, "") catch return error.CompilationFailed;
        const disc_offset_val = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;

        switch (payload_idx) {
            .dec => {
                _ = try self.callBuiltin("roc_builtins_dec_from_str", .void, &.{ ptr_type, ptr_type, .i64, .i64, .i32 }, &.{ dest_ptr, str_bytes, str_len, str_cap, disc_offset_val });
            },
            .f32, .f64 => {
                const float_width: u8 = if (payload_idx == .f32) 4 else 8;
                const float_width_val = builder.intValue(.i8, float_width) catch return error.OutOfMemory;
                _ = try self.callBuiltin("roc_builtins_float_from_str", .void, &.{ ptr_type, ptr_type, .i64, .i64, .i8, .i32 }, &.{ dest_ptr, str_bytes, str_len, str_cap, float_width_val, disc_offset_val });
            },
            else => {
                const int_width: u8 = switch (payload_idx) {
                    .u8, .i8 => 1,
                    .u16, .i16 => 2,
                    .u32, .i32 => 4,
                    .u64, .i64 => 8,
                    .u128, .i128 => 16,
                    else => unreachable,
                };
                const int_width_val = builder.intValue(.i8, int_width) catch return error.OutOfMemory;
                const is_signed_val = builder.intValue(.i1, @intFromBool(switch (payload_idx) {
                    .i8, .i16, .i32, .i64, .i128 => true,
                    else => false,
                })) catch return error.OutOfMemory;
                _ = try self.callBuiltin("roc_builtins_int_from_str", .void, &.{ ptr_type, ptr_type, .i64, .i64, .i8, .i1, .i32 }, &.{ dest_ptr, str_bytes, str_len, str_cap, int_width_val, is_signed_val, disc_offset_val });
            },
        }

        return .none;
    }

    fn unwrapSingleFieldPayloadLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) ?layout.Idx {
        const layout_val = self.layout_store.?.getLayout(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layout_store.?.getStructData(layout_val.data.struct_.idx);
        const fields = self.layout_store.?.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;

        if (builtin.mode == .Debug) {
            const field_offset = self.layout_store.?.getStructFieldOffsetByOriginalIndex(layout_val.data.struct_.idx, 0);
            std.debug.assert(field_offset == 0);
        }

        return field.layout;
    }

    /// Materialize a sub-expression as a pointer to memory.
    /// For composite types (lists, strings) that write to out_ptr, this allocates
    /// a temporary stack slot via alloca, points out_ptr at it, generates the
    /// expression, and returns the alloca pointer. For scalar types, it allocates,
    /// stores the value, and returns the pointer.
    fn materializeAsPtr(self: *MonoLlvmCodeGen, expr_id: LirExprId, size: u32) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Allocate stack space via alloca
        const byte_array_type = builder.arrayType(size, .i8) catch return error.OutOfMemory;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const alloca_ptr = wip.alloca(.normal, byte_array_type, .none, alignment, .default, "buf") catch return error.CompilationFailed;

        // Zero-initialize
        const zero64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        for (0..size / 8) |i| {
            if (i == 0) {
                _ = wip.store(.normal, zero64, alloca_ptr, alignment) catch return error.CompilationFailed;
            } else {
                const off = builder.intValue(.i32, @as(u32, @intCast(i * 8))) catch return error.OutOfMemory;
                const ptr = wip.gep(.inbounds, .i8, alloca_ptr, &.{off}, "") catch return error.CompilationFailed;
                _ = wip.store(.normal, zero64, ptr, alignment) catch return error.CompilationFailed;
            }
        }

        // Set out_ptr to the alloca, generate the expression, then restore
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = alloca_ptr;
        defer self.out_ptr = saved_out_ptr;

        const result = try self.generateExpr(expr_id);
        if (result != .none) {
            // Scalar value — store it to the alloca
            _ = wip.store(.normal, result, alloca_ptr, alignment) catch return error.CompilationFailed;
        }

        return alloca_ptr;
    }

    fn materializedLayoutSize(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!u32 {
        if (layout_idx == .str) return 24;

        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(layout_idx);
        return switch (stored_layout.tag) {
            .list, .list_of_zst => 24,
            else => @intCast(@max(ls.layoutSizeAlign(stored_layout).size, 1)),
        };
    }

    fn shouldMaterializeCallArg(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) bool {
        if (layout_idx == .str) return true;

        const ls = self.layout_store orelse return false;
        const stored_layout = ls.getLayout(layout_idx);
        return stored_layout.tag == .list or stored_layout.tag == .list_of_zst;
    }

    fn generateExprAsValue(self: *MonoLlvmCodeGen, expr_id: LirExprId) Error!struct {
        value: LlvmBuilder.Value,
        layout_idx: ?layout.Idx,
    } {
        const value_layout = self.getExprResultLayout(expr_id);

        if (value_layout) |layout_idx| {
            if (self.shouldMaterializeCallArg(layout_idx)) {
                const ptr = try self.materializeAsPtr(expr_id, try self.materializedLayoutSize(layout_idx));
                return .{
                    .value = try self.loadValueFromPtr(ptr, layout_idx),
                    .layout_idx = layout_idx,
                };
            }
        }

        const raw_value = try self.generateExpr(expr_id);
        if (raw_value != .none) {
            return .{ .value = raw_value, .layout_idx = value_layout };
        }

        if (value_layout) |layout_idx| {
            const ptr = try self.materializeAsPtr(expr_id, try self.materializedLayoutSize(layout_idx));
            return .{
                .value = try self.loadValueFromPtr(ptr, layout_idx),
                .layout_idx = layout_idx,
            };
        }

        return error.CompilationFailed;
    }

    fn generateControlFlowValue(self: *MonoLlvmCodeGen, expr_id: LirExprId, result_layout: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;

        if (self.exprNeverReturns(expr_id)) {
            _ = try self.generateExpr(expr_id);
            return builder.poisonValue(try self.layoutToLlvmTypeFull(result_layout)) catch return error.OutOfMemory;
        }

        return self.coerceValueToLayout((try self.generateExprAsValue(expr_id)).value, result_layout);
    }

    /// Generate a checked integer try-conversion returning a Result tag union.
    /// Calls a C wrapper that checks range and writes to a tag union buffer.
    fn generateIntTryConversion(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;
        const dest_ptr = self.out_ptr orelse unreachable;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const args = self.store.getExprSpan(ll.args);
        std.debug.assert(args.len >= 1);

        // Get the tag union layout for the Result return type
        const ret_layout_val = ls.getLayout(ll.ret_layout);
        std.debug.assert(ret_layout_val.tag == .tag_union);
        const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
        const disc_offset: u32 = tu_data.discriminant_offset;
        const payload_size: u32 = disc_offset; // payload is before discriminant
        const total_size: u32 = tu_data.size;

        // Zero the output buffer first
        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const total_size_val = builder.intValue(.i32, total_size) catch return error.OutOfMemory;
        _ = wip.callMemSet(dest_ptr, alignment, zero_byte, total_size_val, .normal, false) catch return error.CompilationFailed;

        // Get conversion info
        const info = intTryConvInfo(ll.op);

        // Generate source value
        const saved = self.out_ptr;
        self.out_ptr = null;
        const operand = try self.generateExpr(args[0]);
        self.out_ptr = saved;

        if (info.src_bits > 64) {
            // 128-bit source: split into low/high u64 halves
            const builtin_name = if (info.src_signed) "roc_builtins_i128_try_convert" else "roc_builtins_u128_try_convert";

            const low = wip.cast(.trunc, operand, .i64, "") catch return error.CompilationFailed;
            const shifted = wip.bin(.lshr, operand, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
            const high = wip.cast(.trunc, shifted, .i64, "") catch return error.CompilationFailed;

            const tgt_bits_val = builder.intValue(.i32, @as(u32, info.tgt_bits)) catch return error.OutOfMemory;
            const tgt_signed_val = builder.intValue(.i32, @as(u32, if (info.tgt_signed) 1 else 0)) catch return error.OutOfMemory;
            const payload_size_val = builder.intValue(.i32, payload_size) catch return error.OutOfMemory;
            const disc_offset_val = builder.intValue(.i32, disc_offset) catch return error.OutOfMemory;

            _ = try self.callBuiltin(builtin_name, .void, &.{
                ptr_type, .i64, .i64, .i32, .i32, .i32, .i32,
            }, &.{
                dest_ptr, low, high, tgt_bits_val, tgt_signed_val, payload_size_val, disc_offset_val,
            });
        } else {
            // ≤64-bit source: sign/zero extend to i64, call C wrapper
            const src_type = operand.typeOfWip(wip);
            const val_i64 = if (src_type == .i64)
                operand
            else if (info.src_signed)
                wip.cast(.sext, operand, .i64, "") catch return error.CompilationFailed
            else
                wip.cast(.zext, operand, .i64, "") catch return error.CompilationFailed;

            if (info.src_signed) {
                // Signed source
                const min_val: i64 = if (info.tgt_signed) blk: {
                    if (info.tgt_bits >= 64) break :blk std.math.minInt(i64);
                    const shift: u6 = @intCast(info.tgt_bits - 1);
                    break :blk -(@as(i64, 1) << shift);
                } else 0;

                const max_val: i64 = if (info.tgt_bits >= 64) blk: {
                    break :blk std.math.maxInt(i64);
                } else if (info.tgt_signed) blk: {
                    const shift: u6 = @intCast(info.tgt_bits - 1);
                    break :blk (@as(i64, 1) << shift) - 1;
                } else blk: {
                    const shift: u6 = @intCast(info.tgt_bits);
                    break :blk (@as(i64, 1) << shift) - 1;
                };

                _ = try self.callBuiltin("roc_builtins_int_try_signed", .void, &.{
                    ptr_type, .i64, .i64, .i64, .i32, .i32,
                }, &.{
                    dest_ptr,
                    val_i64,
                    builder.intValue(.i64, min_val) catch return error.OutOfMemory,
                    builder.intValue(.i64, max_val) catch return error.OutOfMemory,
                    builder.intValue(.i32, payload_size) catch return error.OutOfMemory,
                    builder.intValue(.i32, disc_offset) catch return error.OutOfMemory,
                });
            } else {
                // Unsigned source
                const max_val: u64 = if (info.tgt_bits >= 64) blk: {
                    break :blk std.math.maxInt(u64);
                } else if (info.tgt_signed) blk: {
                    const shift: u6 = @intCast(info.tgt_bits - 1);
                    break :blk (@as(u64, 1) << shift) - 1;
                } else blk: {
                    const shift: u6 = @intCast(info.tgt_bits);
                    break :blk (@as(u64, 1) << shift) - 1;
                };

                _ = try self.callBuiltin("roc_builtins_int_try_unsigned", .void, &.{
                    ptr_type, .i64, .i64, .i32, .i32,
                }, &.{
                    dest_ptr,
                    val_i64,
                    builder.intValue(.i64, max_val) catch return error.OutOfMemory,
                    builder.intValue(.i32, payload_size) catch return error.OutOfMemory,
                    builder.intValue(.i32, disc_offset) catch return error.OutOfMemory,
                });
            }
        }
        return .none;
    }

    const IntTryInfo = struct {
        src_bits: u8,
        src_signed: bool,
        tgt_bits: u8,
        tgt_signed: bool,
    };

    fn intTryConvInfo(op: anytype) IntTryInfo {
        return switch (op) {
            .u8_to_i8_try => .{ .src_bits = 8, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
            .i8_to_u8_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
            .i8_to_u16_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
            .i8_to_u32_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
            .i8_to_u64_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
            .i8_to_u128_try => .{ .src_bits = 8, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
            .u16_to_i8_try => .{ .src_bits = 16, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
            .u16_to_i16_try => .{ .src_bits = 16, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
            .u16_to_u8_try => .{ .src_bits = 16, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
            .i16_to_i8_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
            .i16_to_u8_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
            .i16_to_u16_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
            .i16_to_u32_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
            .i16_to_u64_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
            .i16_to_u128_try => .{ .src_bits = 16, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
            .u32_to_i8_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
            .u32_to_i16_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
            .u32_to_i32_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 32, .tgt_signed = true },
            .u32_to_u8_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
            .u32_to_u16_try => .{ .src_bits = 32, .src_signed = false, .tgt_bits = 16, .tgt_signed = false },
            .i32_to_i8_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
            .i32_to_i16_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 16, .tgt_signed = true },
            .i32_to_u8_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
            .i32_to_u16_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
            .i32_to_u32_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
            .i32_to_u64_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
            .i32_to_u128_try => .{ .src_bits = 32, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
            .u64_to_i8_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
            .u64_to_i16_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
            .u64_to_i32_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 32, .tgt_signed = true },
            .u64_to_i64_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 64, .tgt_signed = true },
            .u64_to_u8_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
            .u64_to_u16_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 16, .tgt_signed = false },
            .u64_to_u32_try => .{ .src_bits = 64, .src_signed = false, .tgt_bits = 32, .tgt_signed = false },
            .i64_to_i8_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
            .i64_to_i16_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 16, .tgt_signed = true },
            .i64_to_i32_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 32, .tgt_signed = true },
            .i64_to_u8_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
            .i64_to_u16_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
            .i64_to_u32_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
            .i64_to_u64_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
            .i64_to_u128_try => .{ .src_bits = 64, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
            .u128_to_i8_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 8, .tgt_signed = true },
            .u128_to_i16_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 16, .tgt_signed = true },
            .u128_to_i32_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 32, .tgt_signed = true },
            .u128_to_i64_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 64, .tgt_signed = true },
            .u128_to_i128_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 128, .tgt_signed = true },
            .u128_to_u8_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 8, .tgt_signed = false },
            .u128_to_u16_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 16, .tgt_signed = false },
            .u128_to_u32_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 32, .tgt_signed = false },
            .u128_to_u64_try => .{ .src_bits = 128, .src_signed = false, .tgt_bits = 64, .tgt_signed = false },
            .i128_to_i8_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 8, .tgt_signed = true },
            .i128_to_i16_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 16, .tgt_signed = true },
            .i128_to_i32_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 32, .tgt_signed = true },
            .i128_to_i64_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 64, .tgt_signed = true },
            .i128_to_u8_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 8, .tgt_signed = false },
            .i128_to_u16_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 16, .tgt_signed = false },
            .i128_to_u32_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 32, .tgt_signed = false },
            .i128_to_u64_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 64, .tgt_signed = false },
            .i128_to_u128_try => .{ .src_bits = 128, .src_signed = true, .tgt_bits = 128, .tgt_signed = false },
            else => unreachable,
        };
    }

    /// Generate a Dec try-unsafe conversion (u128_to_dec_try_unsafe, i128_to_dec_try_unsafe).
    /// The source is an i128 value, the result is a record {val: Dec(i128), success: Bool}.
    /// Calls a C wrapper that does RocDec.fromWholeInt and writes {dec_bytes, success} to output.
    fn generateDecTryUnsafeConversion(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse unreachable;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const args = self.store.getExprSpan(ll.args);
        std.debug.assert(args.len >= 1);

        // Zero the output buffer first (result is {Dec(i128), Bool} = 17 bytes typically)
        const ls = self.layout_store orelse unreachable;
        const ret_layout_val = ls.getLayout(ll.ret_layout);
        const size_align = ls.layoutSizeAlign(ret_layout_val);
        const total_size: u32 = size_align.size;
        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const total_size_val = builder.intValue(.i32, total_size) catch return error.OutOfMemory;
        _ = wip.callMemSet(dest_ptr, alignment, zero_byte, total_size_val, .normal, false) catch return error.CompilationFailed;

        // Determine which builtin to call based on signedness
        const is_signed = ll.op == .i128_to_dec_try_unsafe;
        const builtin_name = if (is_signed) "roc_builtins_dec_i128_to_dec_try_unsafe" else "roc_builtins_dec_u128_to_dec_try_unsafe";

        // Generate source i128 value
        const saved = self.out_ptr;
        self.out_ptr = null;
        const operand = try self.generateExpr(args[0]);
        self.out_ptr = saved;

        // Split i128 into low/high u64 halves
        const low = wip.cast(.trunc, operand, .i64, "") catch return error.CompilationFailed;
        const shifted = wip.bin(.lshr, operand, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
        const high = wip.cast(.trunc, shifted, .i64, "") catch return error.CompilationFailed;

        // fn(out, val_low, val_high) -> void
        _ = try self.callBuiltin(builtin_name, .void, &.{
            ptr_type, .i64, .i64,
        }, &.{
            dest_ptr, low, high,
        });

        return .none;
    }

    /// Call a (str, str) -> bool builtin with decomposed args.
    /// Materializes both string args, loads their 3 fields, calls the named builtin.
    fn callStrStr2Bool(self: *MonoLlvmCodeGen, arg_a: LirExprId, arg_b: LirExprId, builtin_name: []const u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        const a_ptr = try self.materializeAsPtr(arg_a, 24);
        const b_ptr = try self.materializeAsPtr(arg_b, 24);

        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

        const a_bytes = wip.load(.normal, ptr_type, a_ptr, alignment, "") catch return error.CompilationFailed;
        const a_len_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;
        const a_cap_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const a_cap = wip.load(.normal, .i64, a_cap_ptr, alignment, "") catch return error.CompilationFailed;

        const b_bytes = wip.load(.normal, ptr_type, b_ptr, alignment, "") catch return error.CompilationFailed;
        const b_len_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;
        const b_cap_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const b_cap = wip.load(.normal, .i64, b_cap_ptr, alignment, "") catch return error.CompilationFailed;

        return self.callBuiltin(builtin_name, .i1, &.{
            ptr_type, .i64, .i64, ptr_type, .i64, .i64,
        }, &.{
            a_bytes, a_len, a_cap, b_bytes, b_len, b_cap,
        });
    }

    /// Helper: call a (str, roc_ops) -> str builtin, writing result to out_ptr.
    /// Pattern: fn(out, bytes, len, cap, roc_ops) -> void
    fn callStr2Str(self: *MonoLlvmCodeGen, arg: LirExprId, builtin_name: []const u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse unreachable;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

        const str_ptr = try self.materializeAsPtr(arg, 24);
        const str_bytes = wip.load(.normal, ptr_type, str_ptr, alignment, "") catch return error.CompilationFailed;
        const len_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const str_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
        const cap_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const str_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

        _ = try self.callBuiltin(builtin_name, .void, &.{ ptr_type, ptr_type, .i64, .i64, ptr_type }, &.{ dest_ptr, str_bytes, str_len, str_cap, roc_ops });
        return .none;
    }

    /// Helper: call a (str, str, roc_ops) -> str builtin, writing result to out_ptr.
    /// Pattern: fn(out, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops) -> void
    fn callStrStr2Str(self: *MonoLlvmCodeGen, arg_a: LirExprId, arg_b: LirExprId, builtin_name: []const u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse unreachable;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

        const a_ptr = try self.materializeAsPtr(arg_a, 24);
        const b_ptr = try self.materializeAsPtr(arg_b, 24);

        const a_bytes = wip.load(.normal, ptr_type, a_ptr, alignment, "") catch return error.CompilationFailed;
        const a_len_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;
        const a_cap_ptr = wip.gep(.inbounds, .i8, a_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const a_cap = wip.load(.normal, .i64, a_cap_ptr, alignment, "") catch return error.CompilationFailed;

        const b_bytes = wip.load(.normal, ptr_type, b_ptr, alignment, "") catch return error.CompilationFailed;
        const b_len_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;
        const b_cap_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const b_cap = wip.load(.normal, .i64, b_cap_ptr, alignment, "") catch return error.CompilationFailed;

        _ = try self.callBuiltin(builtin_name, .void, &.{ ptr_type, ptr_type, .i64, .i64, ptr_type, .i64, .i64, ptr_type }, &.{ dest_ptr, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops });
        return .none;
    }

    /// Helper: call a (str, u64, roc_ops) -> str builtin, writing result to out_ptr.
    /// Pattern: fn(out, bytes, len, cap, u64_val, roc_ops) -> void
    fn callStrU642Str(self: *MonoLlvmCodeGen, str_arg: LirExprId, u64_arg: LirExprId, builtin_name: []const u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse unreachable;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

        const str_ptr = try self.materializeAsPtr(str_arg, 24);
        const str_bytes = wip.load(.normal, ptr_type, str_ptr, alignment, "") catch return error.CompilationFailed;
        const len_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const str_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
        const cap_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const str_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

        // Generate the u64 argument
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        const u64_val = try self.generateExpr(u64_arg);
        self.out_ptr = saved_out_ptr;

        _ = try self.callBuiltin(builtin_name, .void, &.{ ptr_type, ptr_type, .i64, .i64, .i64, ptr_type }, &.{ dest_ptr, str_bytes, str_len, str_cap, u64_val, roc_ops });
        return .none;
    }

    /// Helper: call listSublist wrapper, materializing list from expression.
    fn callListSublist(self: *MonoLlvmCodeGen, list_arg: LirExprId, start: LlvmBuilder.Value, count: LlvmBuilder.Value, ll: anytype) Error!LlvmBuilder.Value {
        const list_ptr = try self.materializeAsPtr(list_arg, 24);
        return try self.callListSublistFromPtr(list_ptr, start, count, ll);
    }

    /// Helper: call listSublist builtin from pre-materialized list pointer.
    /// roc_builtins_list_sublist(out, list_bytes, list_len, list_cap, align, elem_width, start, count, elements_refcounted, roc_ops)
    fn callListSublistFromPtr(self: *MonoLlvmCodeGen, list_ptr: LlvmBuilder.Value, start: LlvmBuilder.Value, count: LlvmBuilder.Value, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse unreachable;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;

        const list_bytes = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
        const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
        const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

        // Get element layout info
        const elem_info = try self.getListElementInfo(ll.ret_layout);

        const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
        const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;
        const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

        _ = try self.callBuiltin("roc_builtins_list_sublist", .void, &.{
            ptr_type, ptr_type, .i64, .i64, .i32, .i64, .i64, .i64, .i1, ptr_type,
        }, &.{
            dest_ptr, list_bytes, list_len, list_cap, align_val, width_val, start, count, elements_refcounted_val, roc_ops,
        });
        return .none;
    }

    // String generation

    /// RocStr size in bytes (3 pointer-sized words: ptr/bytes, length, capacity)
    const roc_str_size: u32 = 24; // 3 * 8 on 64-bit
    const small_str_max_len: u32 = roc_str_size - 1; // 23 bytes

    /// Generate a string literal by storing bytes directly to the output pointer.
    /// Small strings (≤ 23 bytes) are stored inline in the RocStr struct.
    /// Large strings (> 23 bytes) are NOT yet supported.
    ///
    /// Stores bytes directly to `out_ptr` to avoid LLVM placing constants
    /// in .rodata (which we don't extract from the object file). Individual
    /// i8 stores use small immediates that stay inline in the code.
    ///
    /// Returns a sentinel value (.none) since the result is written directly
    /// to the output buffer. The caller should skip the result storage step.
    fn generateStrLiteral(self: *MonoLlvmCodeGen, str_idx: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // If no out_ptr, create a temporary alloca for the 24-byte RocStr struct
        const needs_temp = self.out_ptr == null;
        const dest_ptr = self.out_ptr orelse blk: {
            const alloca = wip.alloca(.normal, .i64, builder.intValue(.i32, 3) catch return error.OutOfMemory, LlvmBuilder.Alignment.fromByteUnits(8), .default, "str_tmp") catch return error.CompilationFailed;
            // Zero initialize to avoid garbage in unused small string bytes
            const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
            const align8 = LlvmBuilder.Alignment.fromByteUnits(8);
            _ = wip.store(.normal, zero_i64, alloca, align8) catch return error.CompilationFailed;
            const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
            const p8 = wip.gep(.inbounds, .i8, alloca, &.{off8}, "") catch return error.CompilationFailed;
            _ = wip.store(.normal, zero_i64, p8, align8) catch return error.CompilationFailed;
            const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
            const p16 = wip.gep(.inbounds, .i8, alloca, &.{off16}, "") catch return error.CompilationFailed;
            _ = wip.store(.normal, zero_i64, p16, align8) catch return error.CompilationFailed;
            break :blk alloca;
        };

        const str_bytes = self.store.getString(str_idx);

        if (str_bytes.len >= roc_str_size) {
            // Large string: allocate heap memory, copy bytes, write RocStr to dest_ptr
            _ = try self.generateLargeStrLiteral(str_bytes, dest_ptr);
            if (needs_temp) {
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const str_struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
                return wip.load(.normal, str_struct_type, dest_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "str_val") catch return error.CompilationFailed;
            }
            return .none;
        }

        const byte_alignment = LlvmBuilder.Alignment.fromByteUnits(1);

        // Store each string byte individually to dest_ptr using volatile stores.
        // Volatile prevents LLVM from combining adjacent byte stores into
        // word stores that reference a constant pool in .rodata (which we
        // don't extract from the object file).
        for (str_bytes, 0..) |byte, i| {
            if (byte == 0) continue;
            const byte_val = builder.intValue(.i8, byte) catch return error.OutOfMemory;
            if (i == 0) {
                _ = wip.store(.@"volatile", byte_val, dest_ptr, byte_alignment) catch return error.CompilationFailed;
            } else {
                const offset = builder.intValue(.i32, @as(u32, @intCast(i))) catch return error.OutOfMemory;
                const ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{offset}, "") catch return error.CompilationFailed;
                _ = wip.store(.@"volatile", byte_val, ptr, byte_alignment) catch return error.CompilationFailed;
            }
        }

        // Store the length byte at position 23: length | 0x80
        const len_byte: u8 = @intCast(str_bytes.len | 0x80);
        const len_val = builder.intValue(.i8, len_byte) catch return error.OutOfMemory;
        const len_offset = builder.intValue(.i32, small_str_max_len) catch return error.OutOfMemory;
        const len_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{len_offset}, "") catch return error.CompilationFailed;
        _ = wip.store(.@"volatile", len_val, len_ptr, byte_alignment) catch return error.CompilationFailed;

        if (needs_temp) {
            const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
            const str_struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            return wip.load(.normal, str_struct_type, dest_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "str_val") catch return error.CompilationFailed;
        }
        // Return .none as sentinel — data already written to out_ptr
        return .none;
    }

    /// Generate a large string literal (>= 24 bytes) by allocating heap memory.
    fn generateLargeStrLiteral(self: *MonoLlvmCodeGen, str_bytes: []const u8, dest_ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;

        // Call roc_builtins_allocate_with_refcount(total_bytes, alignment=1, refcounted=false, roc_ops)
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const str_len: u64 = @intCast(str_bytes.len);
        const size_val = builder.intValue(.i64, str_len) catch return error.OutOfMemory;
        const align_val = builder.intValue(.i32, 1) catch return error.OutOfMemory;
        const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;

        const heap_ptr = try self.callBuiltin("roc_builtins_allocate_with_refcount", ptr_type, &.{ .i64, .i32, .i1, ptr_type }, &.{ size_val, align_val, refcounted_val, roc_ops });

        // Copy string bytes to heap memory using volatile stores
        const byte_alignment = LlvmBuilder.Alignment.fromByteUnits(1);
        for (str_bytes, 0..) |byte, i| {
            if (byte == 0) continue;
            const byte_val = builder.intValue(.i8, byte) catch return error.OutOfMemory;
            if (i == 0) {
                _ = wip.store(.@"volatile", byte_val, heap_ptr, byte_alignment) catch return error.CompilationFailed;
            } else {
                const offset = builder.intValue(.i32, @as(u32, @intCast(i))) catch return error.OutOfMemory;
                const ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{offset}, "") catch return error.CompilationFailed;
                _ = wip.store(.@"volatile", byte_val, ptr, byte_alignment) catch return error.CompilationFailed;
            }
        }

        // Write RocStr struct to dest_ptr: {ptr, len, capacity}
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Store data pointer (offset 0)
        _ = wip.store(.normal, heap_ptr, dest_ptr, alignment) catch return error.CompilationFailed;

        // Store len (offset 8)
        const len_val = builder.intValue(.i64, str_len) catch return error.OutOfMemory;
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const ptr8 = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, len_val, ptr8, alignment) catch return error.CompilationFailed;

        // Store capacity (offset 16) — same as len for new strings
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const ptr16 = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, len_val, ptr16, alignment) catch return error.CompilationFailed;

        return .none;
    }

    /// Generate int_to_str using the current builtin wrapper ABI:
    /// fn(out, val_low, val_high, int_width, is_signed, roc_ops)
    fn generateIntToStr(self: *MonoLlvmCodeGen, its: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const needs_temp = self.out_ptr == null;
        if (needs_temp) {
            const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
            self.out_ptr = wip.alloca(.normal, .i64, alloca_count, LlvmBuilder.Alignment.fromByteUnits(8), .default, "its_tmp") catch return error.CompilationFailed;
        }
        const dest_ptr = self.out_ptr.?;

        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        const value = try self.generateExpr(its.value);
        self.out_ptr = saved_out_ptr;

        const is_signed_precision = switch (its.int_precision) {
            .i8, .i16, .i32, .i64, .i128 => true,
            .u8, .u16, .u32, .u64, .u128 => false,
        };
        const int_width = builder.intValue(.i8, its.int_precision.size()) catch return error.OutOfMemory;
        const is_signed = builder.intValue(.i1, @intFromBool(is_signed_precision)) catch return error.OutOfMemory;

        const val_low, const val_high = switch (its.int_precision) {
            .u128, .i128 => blk: {
                const val_i128 = if (value.typeOfWip(wip) == .i128)
                    value
                else
                    wip.cast(if (is_signed_precision) .sext else .zext, value, .i128, "") catch return error.CompilationFailed;
                const lo = wip.cast(.trunc, val_i128, .i64, "") catch return error.CompilationFailed;
                const sixty_four = builder.intValue(.i128, 64) catch return error.OutOfMemory;
                const shifted = wip.bin(.lshr, val_i128, sixty_four, "") catch return error.CompilationFailed;
                const hi = wip.cast(.trunc, shifted, .i64, "") catch return error.CompilationFailed;
                break :blk .{ lo, hi };
            },
            else => blk: {
                const low = if (value.typeOfWip(wip) == .i64)
                    value
                else
                    wip.cast(if (is_signed_precision) .sext else .zext, value, .i64, "") catch return error.CompilationFailed;
                break :blk .{ low, builder.intValue(.i64, 0) catch return error.OutOfMemory };
            },
        };

        _ = try self.callBuiltin(
            "roc_builtins_int_to_str",
            .void,
            &.{ ptr_type, .i64, .i64, .i8, .i1, ptr_type },
            &.{ dest_ptr, val_low, val_high, int_width, is_signed, roc_ops },
        );

        if (needs_temp) {
            const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            return wip.load(.normal, struct_type, dest_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "its_val") catch return error.CompilationFailed;
        }
        return .none;
    }

    /// Generate float_to_str using the current builtin wrapper ABI:
    /// fn(out, val_bits, is_f32, roc_ops)
    fn generateFloatToStr(self: *MonoLlvmCodeGen, fts: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        // Check for Dec precision
        if (fts.float_precision == .dec) {
            return self.generateDecToStr(fts.value);
        }

        const needs_temp = self.out_ptr == null;
        if (needs_temp) {
            const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
            self.out_ptr = wip.alloca(.normal, .i64, alloca_count, LlvmBuilder.Alignment.fromByteUnits(8), .default, "fts_tmp") catch return error.CompilationFailed;
        }
        const dest_ptr = self.out_ptr.?;

        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        const value = try self.generateExpr(fts.value);
        self.out_ptr = saved_out_ptr;

        const val_bits = switch (fts.float_precision) {
            .f64 => wip.cast(.bitcast, value, .i64, "") catch return error.CompilationFailed,
            .f32 => blk: {
                const bits32 = wip.cast(.bitcast, value, .i32, "") catch return error.CompilationFailed;
                break :blk wip.cast(.zext, bits32, .i64, "") catch return error.CompilationFailed;
            },
            .dec => unreachable,
        };
        const is_f32 = builder.intValue(.i1, @intFromBool(fts.float_precision == .f32)) catch return error.OutOfMemory;

        _ = try self.callBuiltin(
            "roc_builtins_float_to_str",
            .void,
            &.{ ptr_type, .i64, .i1, ptr_type },
            &.{ dest_ptr, val_bits, is_f32, roc_ops },
        );

        if (needs_temp) {
            const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            return wip.load(.normal, struct_type, dest_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "fts_val") catch return error.CompilationFailed;
        }
        return .none;
    }

    /// Generate dec_to_str: decompose i128 into two u64s, call builtin fn(out, lo, hi, roc_ops)
    fn generateDecToStr(self: *MonoLlvmCodeGen, expr_id: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const needs_temp = self.out_ptr == null;
        if (needs_temp) {
            const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
            self.out_ptr = wip.alloca(.normal, .i64, alloca_count, LlvmBuilder.Alignment.fromByteUnits(8), .default, "dts_tmp") catch return error.CompilationFailed;
        }
        const dest_ptr = self.out_ptr.?;

        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        const value = try self.generateExpr(expr_id);
        self.out_ptr = saved_out_ptr;

        // Decompose i128 into lo (lower 64 bits) and hi (upper 64 bits)
        const lo = wip.cast(.trunc, value, .i64, "") catch return error.CompilationFailed;
        const sixty_four = builder.intValue(.i128, 64) catch return error.OutOfMemory;
        const shifted = wip.bin(.lshr, value, sixty_four, "") catch return error.CompilationFailed;
        const hi = wip.cast(.trunc, shifted, .i64, "") catch return error.CompilationFailed;

        _ = try self.callBuiltin("roc_builtins_dec_to_str", .void, &.{ ptr_type, .i64, .i64, ptr_type }, &.{ dest_ptr, lo, hi, roc_ops });

        if (needs_temp) {
            const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            return wip.load(.normal, struct_type, dest_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "dts_val") catch return error.CompilationFailed;
        }
        return .none;
    }

    /// Generate str_escape_and_quote: calls builtin fn(out, str_bytes, str_len, str_cap, roc_ops)
    fn generateStrEscapeAndQuote(self: *MonoLlvmCodeGen, expr_id: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        const needs_temp = self.out_ptr == null;
        if (needs_temp) {
            const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
            self.out_ptr = wip.alloca(.normal, .i64, alloca_count, LlvmBuilder.Alignment.fromByteUnits(8), .default, "seq_tmp") catch return error.CompilationFailed;
        }
        const dest_ptr = self.out_ptr.?;

        // Materialize the string arg
        const str_ptr = try self.materializeAsPtr(expr_id, 24);

        // Load decomposed fields
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const str_bytes = wip.load(.normal, ptr_type, str_ptr, alignment, "") catch return error.CompilationFailed;
        const str_len_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const str_len = wip.load(.normal, .i64, str_len_ptr, alignment, "") catch return error.CompilationFailed;
        const str_cap_ptr = wip.gep(.inbounds, .i8, str_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const str_cap = wip.load(.normal, .i64, str_cap_ptr, alignment, "") catch return error.CompilationFailed;

        _ = try self.callBuiltin("roc_builtins_str_escape_and_quote", .void, &.{ ptr_type, ptr_type, .i64, .i64, ptr_type }, &.{ dest_ptr, str_bytes, str_len, str_cap, roc_ops });

        if (needs_temp) {
            const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            return wip.load(.normal, struct_type, dest_ptr, alignment, "seq_val") catch return error.CompilationFailed;
        }
        return .none;
    }

    /// Get the result layout of a LIR expression (for determining operand types).
    fn getExprResultLayout(self: *const MonoLlvmCodeGen, expr_id: LirExprId) ?layout.Idx {
        const LirExpr = lir.LirExpr;
        const expr: LirExpr = self.store.getExpr(expr_id);
        return switch (expr) {
            .block => |b| self.getExprResultLayout(b.final_expr),
            .call => |c| c.ret_layout,
            .low_level => |ll| ll.ret_layout,
            .lookup => |l| l.layout_idx,
            .i64_literal => |i| i.layout_idx,
            .f64_literal => .f64,
            .f32_literal => .f32,
            .bool_literal => .bool,
            .i128_literal => |i| i.layout_idx,
            .dec_literal => .dec,
            .str_literal => .str,
            .empty_list => |l| l.list_layout,
            .list => |l| l.list_layout,
            .struct_ => |s| s.struct_layout,
            .struct_access => |sa| sa.field_layout,
            .cell_load => |load| load.layout_idx,
            .nominal => |nom| self.getExprResultLayout(nom.backing_expr),
            .if_then_else => |ite| ite.result_layout,
            .zero_arg_tag => |zat| zat.union_layout,
            .tag => |t| t.union_layout,
            .discriminant_switch => |ds| ds.result_layout,
            .match_expr => |m| m.result_layout,
            .dbg => |d| d.result_layout,
            .expect => |e| e.result_layout,
            .early_return => |er| er.ret_layout,
            .runtime_error => |re| re.ret_layout,
            .crash => |c| c.ret_layout,
            else => null,
        };
    }

    fn exprNeverReturns(self: *const MonoLlvmCodeGen, expr_id: LirExprId) bool {
        const expr = self.store.getExpr(expr_id);
        return switch (expr) {
            .early_return, .runtime_error, .crash, .break_expr => true,
            .block => |block| self.exprNeverReturns(block.final_expr),
            else => false,
        };
    }

    fn countBreakEdges(self: *const MonoLlvmCodeGen, expr_id: LirExprId) u32 {
        const expr = self.store.getExpr(expr_id);
        return switch (expr) {
            .break_expr => 1,
            .block => |block| blk: {
                var count: u32 = self.countBreakEdges(block.final_expr);
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| count += self.countBreakEdges(binding.expr),
                        .cell_init, .cell_store => |binding| count += self.countBreakEdges(binding.expr),
                        .cell_drop => {},
                    }
                }
                break :blk count;
            },
            .if_then_else => |ite| blk: {
                var count: u32 = self.countBreakEdges(ite.final_else);
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    count += self.countBreakEdges(branch.body);
                }
                break :blk count;
            },
            .match_expr => |m| blk: {
                var count: u32 = 0;
                for (self.store.getMatchBranches(m.branches)) |branch| {
                    count += self.countBreakEdges(branch.body);
                }
                break :blk count;
            },
            .dbg => |d| self.countBreakEdges(d.expr),
            .expect => |e| self.countBreakEdges(e.body),
            .nominal => |nom| self.countBreakEdges(nom.backing_expr),
            .lambda, .while_loop, .for_loop => 0,
            else => 0,
        };
    }

    /// Decompose an i128 value into (low_i64, high_i64) for C ABI calls.
    fn decomposeI128(self: *MonoLlvmCodeGen, val: LlvmBuilder.Value) Error!struct { low: LlvmBuilder.Value, high: LlvmBuilder.Value } {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const low = wip.cast(.trunc, val, .i64, "") catch return error.CompilationFailed;
        const shifted = wip.bin(.lshr, val, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
        const high = wip.cast(.trunc, shifted, .i64, "") catch return error.CompilationFailed;
        return .{ .low = low, .high = high };
    }

    /// Reconstruct an i128 from (low_i64, high_i64) loaded from C ABI output pointers.
    fn reconstructI128(self: *MonoLlvmCodeGen, low: LlvmBuilder.Value, high: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const low_wide = wip.cast(.zext, low, .i128, "") catch return error.CompilationFailed;
        const high_wide = wip.cast(.zext, high, .i128, "") catch return error.CompilationFailed;
        const high_shifted = wip.bin(.shl, high_wide, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.CompilationFailed;
        return wip.bin(.@"or", high_shifted, low_wide, "") catch return error.CompilationFailed;
    }

    /// Call a Dec builtin that takes two RocDec args and returns RocDec.
    /// builtins.bc ABI: void @func(ptr %out_low, ptr %out_high, i64, i64, i64, i64, [ptr roc_ops])
    fn callDecBuiltin(self: *MonoLlvmCodeGen, name: []const u8, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, pass_roc_ops: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Allocate output space: two i64s for low/high halves of result
        const out_low = wip.alloca(.normal, .i64, .none, alignment, .default, "dec_lo") catch return error.CompilationFailed;
        const out_high = wip.alloca(.normal, .i64, .none, alignment, .default, "dec_hi") catch return error.CompilationFailed;

        // Decompose i128 args into i64 pairs
        const lhs_parts = try self.decomposeI128(lhs);
        const rhs_parts = try self.decomposeI128(rhs);

        // Call builtin with ABI-matching signature
        if (pass_roc_ops) {
            const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
            _ = try self.callBuiltin(name, .void, &.{
                ptr_type, ptr_type, .i64, .i64, .i64, .i64, ptr_type,
            }, &.{
                out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, roc_ops,
            });
        } else {
            _ = try self.callBuiltin(name, .void, &.{
                ptr_type, ptr_type, .i64, .i64, .i64, .i64,
            }, &.{
                out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high,
            });
        }

        // Load result halves and reconstruct i128
        const result_low = wip.load(.normal, .i64, out_low, alignment, "") catch return error.CompilationFailed;
        const result_high = wip.load(.normal, .i64, out_high, alignment, "") catch return error.CompilationFailed;
        return self.reconstructI128(result_low, result_high);
    }

    /// Call Dec multiply builtin. Dec multiplication requires (a * b) / 10^18.
    fn callDecMul(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        return self.callDecBuiltin("roc_builtins_dec_mul_saturated", lhs, rhs, false);
    }

    /// Call Dec divide builtin. Dec division requires (a * 10^18) / b.
    fn callDecDiv(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        return self.callDecBuiltin("roc_builtins_dec_div", lhs, rhs, true);
    }

    /// Call Dec truncating divide builtin.
    fn callDecDivTrunc(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        return self.callDecBuiltin("roc_builtins_dec_div_trunc", lhs, rhs, true);
    }

    fn generateUnaryMinus(self: *MonoLlvmCodeGen, unary: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        var val = try self.generateExpr(unary.expr);
        const is_float = isFloatLayout(unary.result_layout);

        if (is_float) {
            // Cast integer operand to float if needed
            const expr_layout = self.getExprResultLayout(unary.expr) orelse unary.result_layout;
            if (!isFloatLayout(expr_layout)) {
                val = wip.cast(if (isSigned(expr_layout)) .sitofp else .uitofp, val, layoutToLlvmType(unary.result_layout), "") catch return error.CompilationFailed;
            }
            return wip.un(.fneg, val, "") catch return error.CompilationFailed;
        } else {
            // Align value type to result type (e.g., i64_literal produces i64 but result may be i8)
            const result_type = layoutToLlvmType(unary.result_layout);
            const val_type = val.typeOfWip(wip);
            if (isIntType(val_type) and isIntType(result_type) and val_type != result_type) {
                const val_bits = intTypeBits(val_type);
                const result_bits = intTypeBits(result_type);
                if (val_bits > result_bits) {
                    val = wip.cast(.trunc, val, result_type, "") catch return error.CompilationFailed;
                } else {
                    val = wip.cast(if (isSigned(unary.result_layout)) .sext else .zext, val, result_type, "") catch return error.CompilationFailed;
                }
            }
            // For integers, subtract from 0
            const zero = (builder.intConst(result_type, 0) catch return error.OutOfMemory).toValue();
            return wip.bin(.sub, zero, val, "") catch return error.CompilationFailed;
        }
    }

    fn generateUnaryNot(self: *MonoLlvmCodeGen, unary: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const val = try self.generateExpr(unary.expr);
        // XOR with 1 to flip the boolean
        const one = (builder.intConst(.i1, 1) catch return error.OutOfMemory).toValue();
        return wip.bin(.xor, val, one, "") catch return error.CompilationFailed;
    }

    fn generateIfThenElse(self: *MonoLlvmCodeGen, ite: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;

        // Get the branches
        const branches = self.store.getIfBranches(ite.branches);

        if (branches.len == 0) {
            // No branches, just generate the else
            return self.generateExpr(ite.final_else);
        }

        // For simplicity, handle single branch if-then-else
        const first_branch = branches[0];
        var cond_val = try self.generateExpr(first_branch.cond);

        // Ensure condition is i1 for brCond (tag unions may produce i8 for Bool-like types)
        if (cond_val.typeOfWip(wip) != .i1) {
            cond_val = wip.cast(.trunc, cond_val, .i1, "") catch return error.CompilationFailed;
        }

        // Create basic blocks
        // Each of then/else has 1 incoming edge (from the conditional branch),
        // merge has 2 incoming edges (one from then, one from else).
        const then_block = wip.block(1, "then") catch return error.CompilationFailed;
        const else_block = wip.block(1, "else") catch return error.CompilationFailed;
        const merge_block = wip.block(2, "merge") catch return error.CompilationFailed;

        // Conditional branch
        _ = wip.brCond(cond_val, then_block, else_block, .none) catch return error.CompilationFailed;

        // Then block
        wip.cursor = .{ .block = then_block };
        const then_val = try self.generateControlFlowValue(first_branch.body, ite.result_layout);
        _ = wip.br(merge_block) catch return error.CompilationFailed;
        const then_exit_block = wip.cursor.block;

        // Else block
        wip.cursor = .{ .block = else_block };
        const else_val = try self.generateControlFlowValue(ite.final_else, ite.result_layout);
        _ = wip.br(merge_block) catch return error.CompilationFailed;
        const else_exit_block = wip.cursor.block;

        // Merge block with phi
        wip.cursor = .{ .block = merge_block };

        // Check that both branch values have the same type for the phi node
        const then_type = then_val.typeOfWip(wip);
        const else_type = else_val.typeOfWip(wip);
        std.debug.assert(then_type == else_type);

        const phi_inst = wip.phi(then_type, "") catch return error.CompilationFailed;
        phi_inst.finish(
            &.{ then_val, else_val },
            &.{ then_exit_block, else_exit_block },
            wip,
        );

        return phi_inst.toValue();
    }

    fn generateBlock(self: *MonoLlvmCodeGen, block_data: anytype) Error!LlvmBuilder.Value {
        // Save out_ptr — intermediate statements must not write to it.
        // Only the final expression should use out_ptr for direct-store types (strings).
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;

        // Process all statements (let bindings)
        const stmts = self.store.getStmts(block_data.stmts);
        for (stmts) |stmt| {
            switch (stmt) {
                .decl, .mutate => |b| {
                    const stmt_expr = self.store.getExpr(b.expr);
                    switch (stmt_expr) {
                        .lambda => |_| {
                            const val = try self.generateExpr(b.expr);
                            try self.bindPattern(b.pattern, val);
                            const pattern = self.store.getPattern(b.pattern);
                            if (pattern == .bind) {
                                const key: u64 = @bitCast(pattern.bind.symbol);
                                self.closure_bindings.put(key, .{
                                    .representation = .{ .direct_call = {} },
                                    .lambda = b.expr,
                                    .captures = lir.LIR.LirCaptureSpan.empty(),
                                }) catch return error.OutOfMemory;
                            }
                            continue;
                        },
                        else => {},
                    }

                    const val = try self.generateExprAsValue(b.expr);
                    try self.bindPattern(b.pattern, val.value);
                },
                .cell_init => |cell| {
                    const value = (self.generateExprAsValue(cell.expr) catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "cell_init expr failed: layout={d} expr={s}",
                                .{ @intFromEnum(cell.layout_idx), @tagName(self.store.getExpr(cell.expr)) },
                            );
                        }
                        return err;
                    }).value;
                    self.initializeCell(cell.cell, cell.layout_idx, value) catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "cell_init store failed: layout={d} expr={s}",
                                .{ @intFromEnum(cell.layout_idx), @tagName(self.store.getExpr(cell.expr)) },
                            );
                        }
                        return err;
                    };
                },
                .cell_store => |cell| {
                    const value = (self.generateExprAsValue(cell.expr) catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "cell_store expr failed: layout={d} expr={s}",
                                .{ @intFromEnum(cell.layout_idx), @tagName(self.store.getExpr(cell.expr)) },
                            );
                        }
                        return err;
                    }).value;
                    self.storeCell(cell.cell, cell.layout_idx, value) catch |err| {
                        if (builtin.mode == .Debug) {
                            std.log.err(
                                "cell_store write failed: layout={d} expr={s}",
                                .{ @intFromEnum(cell.layout_idx), @tagName(self.store.getExpr(cell.expr)) },
                            );
                        }
                        return err;
                    };
                },
                .cell_drop => |cell| try self.dropCell(cell.cell, cell.layout_idx),
            }
        }

        // Restore out_ptr for the final expression
        self.out_ptr = saved_out_ptr;
        // Generate and return the final expression
        return self.generateExpr(block_data.final_expr);
    }

    fn alignmentForLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) LlvmBuilder.Alignment {
        if (self.layout_store) |ls| {
            const stored_layout = ls.getLayout(layout_idx);
            const sa = ls.layoutSizeAlign(stored_layout);
            return LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(sa.alignment.toByteUnits(), 1)));
        }

        const llvm_type = self.layoutToLlvmTypeFull(layout_idx) catch return LlvmBuilder.Alignment.fromByteUnits(8);
        return LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(llvm_type), 1)));
    }

    fn initializeCell(self: *MonoLlvmCodeGen, cell: Symbol, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const key: u64 = @bitCast(cell);
        const cell_type = try self.layoutToLlvmTypeFull(layout_idx);
        const normalized = try self.coerceValueToLayout(value, layout_idx);
        const alignment = self.alignmentForLayout(layout_idx);
        const alloca_ptr = wip.alloca(.normal, cell_type, .none, alignment, .default, "cell") catch return error.CompilationFailed;
        _ = wip.store(.normal, normalized, alloca_ptr, alignment) catch return error.CompilationFailed;
        self.cell_allocas.put(key, .{ .alloca_ptr = alloca_ptr, .elem_type = cell_type }) catch return error.OutOfMemory;
        self.symbol_values.put(key, normalized) catch return error.OutOfMemory;
    }

    fn storeCell(self: *MonoLlvmCodeGen, cell: Symbol, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const key: u64 = @bitCast(cell);
        const cell_alloca = self.cell_allocas.get(key) orelse {
            // Mutable locals can legitimately reach their first write before this backend
            // has materialized the backing alloca in the current function state. A lazy
            // initialization is equivalent to an explicit cell_init for that first write.
            try self.initializeCell(cell, layout_idx, value);
            return;
        };
        const normalized = self.coerceValueToLayout(value, layout_idx) catch |err| {
            if (builtin.mode == .Debug) {
                std.log.err(
                    "storeCell coerce failed: layout={d} cell_type_raw=0x{x}",
                    .{ @intFromEnum(layout_idx), @intFromEnum(cell_alloca.elem_type) },
                );
            }
            return err;
        };
        _ = wip.store(.normal, normalized, cell_alloca.alloca_ptr, self.alignmentForLayout(layout_idx)) catch |err| {
            if (builtin.mode == .Debug) {
                std.log.err(
                    "storeCell store failed: layout={d} value_type_raw=0x{x} cell_type_raw=0x{x}",
                    .{ @intFromEnum(layout_idx), @intFromEnum(normalized.typeOfWip(wip)), @intFromEnum(cell_alloca.elem_type) },
                );
            }
            return err;
        };
        self.symbol_values.put(key, normalized) catch return error.OutOfMemory;
    }

    fn dropCell(self: *MonoLlvmCodeGen, cell: Symbol, _: layout.Idx) Error!void {
        const key: u64 = @bitCast(cell);
        _ = self.cell_allocas.remove(key);
    }

    // Call generation (mirrors dev backend's dispatch)

    fn generateCall(self: *MonoLlvmCodeGen, call: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        return self.callExprWithArgs(call.fn_expr, call.args, call.ret_layout);
    }

    fn buildLambdaFunctionType(self: *MonoLlvmCodeGen, lambda: anytype, closure_layout: ?layout.Idx) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const params = self.store.getPatternSpan(lambda.params);

        var param_types: std.ArrayList(LlvmBuilder.Type) = .{};
        defer param_types.deinit(self.allocator);

        for (params) |param_id| {
            const param_layout = self.getPatternLayoutIdx(param_id) orelse return error.CompilationFailed;
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(param_layout)) catch return error.OutOfMemory;
        }

        if (closure_layout) |cl| {
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(cl)) catch return error.OutOfMemory;
        }

        param_types.append(self.allocator, ptr_type) catch return error.OutOfMemory;

        return builder.fnType(try self.layoutToLlvmTypeFull(lambda.ret_layout), param_types.items, .normal) catch return error.OutOfMemory;
    }

    /// Resolve a function expression and call it with the given arguments.
    fn callExprWithArgs(self: *MonoLlvmCodeGen, fn_expr_id: LirExprId, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        const fn_expr = self.store.getExpr(fn_expr_id);
        return switch (fn_expr) {
            .lookup => |lookup| self.generateLookupCall(lookup, args_span, ret_layout),
            .lambda => |lambda| {
                // Direct lambda call: compile as func, call immediately
                const func_idx = try self.compileLambdaAsFunc(fn_expr_id, lambda, ret_layout, null);
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(lambda, null), args_span, null, ret_layout);
            },
            .call => |inner_call| self.callChainedExpr(inner_call, args_span, ret_layout),
            .block => |block_data| {
                _ = try self.generateBlock(block_data);
                return self.callExprWithArgs(block_data.final_expr, args_span, ret_layout);
            },
            else => unreachable, // Call fn_expr must be lookup/lambda/call/block
        };
    }

    /// Generate a call through a lookup: check proc_registry, closure_bindings, and top-level defs.
    fn generateLookupCall(self: *MonoLlvmCodeGen, lookup: anytype, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u64 = @bitCast(lookup.symbol);

        if (self.proc_registry.get(symbol_key)) |func_index| {
            return self.generateCallToCompiledProc(func_index, args_span, ret_layout);
        }

        // Check closure_bindings for symbols bound to closures/lambdas
        if (self.closure_bindings.get(symbol_key)) |meta| {
            return self.callClosureMetaWithArgs(meta, args_span, ret_layout);
        }

        // Check top-level definitions — the symbol might resolve to a lambda.
        if (self.store.getSymbolDef(lookup.symbol)) |def_expr_id| {
            const def_expr = self.store.getExpr(def_expr_id);
            switch (def_expr) {
                .lambda => |lambda| {
                    const func_idx = try self.compileLambdaAsFunc(def_expr_id, lambda, ret_layout, null);
                    return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(lambda, null), args_span, null, ret_layout);
                },
                else => {},
            }
        }

        unreachable; // Symbol must exist in proc_registry, closure_bindings, or as a top-level def
    }

    /// Generate a call to a compiled procedure via LLVM call instruction.
    fn generateCallToCompiledProc(self: *MonoLlvmCodeGen, func_index: LlvmBuilder.Function.Index, args_span: anytype, _: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const fn_type = func_index.typeOf(builder);
        const expected_params = fn_type.functionParameters(builder);

        const args = self.store.getExprSpan(args_span);
        var arg_values: std.ArrayList(LlvmBuilder.Value) = .{};
        defer arg_values.deinit(self.allocator);
        const expected_params_copy = self.allocator.dupe(LlvmBuilder.Type, expected_params) catch return error.OutOfMemory;
        defer self.allocator.free(expected_params_copy);

        for (args, 0..) |arg_id, i| {
            const arg = try self.generateExprAsValue(arg_id);
            const val = try self.coerceValueToType(arg.value, expected_params_copy[i], arg.layout_idx);
            arg_values.append(self.allocator, val) catch return error.OutOfMemory;
        }

        // Append the hidden roc_ops parameter
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const roc_ops_idx = arg_values.items.len;
        const coerced_roc_ops = try self.coerceValueToType(roc_ops, expected_params_copy[roc_ops_idx], null);
        arg_values.append(self.allocator, coerced_roc_ops) catch return error.OutOfMemory;

        const callee = func_index.toValue(builder);
        return wip.call(.normal, .ccc, .none, fn_type, callee, arg_values.items, "") catch return error.CompilationFailed;
    }

    fn lambdaCacheKey(lambda_expr_id: LirExprId, ret_layout: layout.Idx, closure_layout: ?layout.Idx) u64 {
        const Key = extern struct {
            expr_id: u32,
            ret_layout: u32,
            closure_layout: u32,
        };

        const closure_raw: u32 = if (closure_layout) |cl|
            @intFromEnum(cl)
        else
            std.math.maxInt(u32);

        const key = Key{
            .expr_id = @intFromEnum(lambda_expr_id),
            .ret_layout = @intFromEnum(ret_layout),
            .closure_layout = closure_raw,
        };

        return std.hash.Wyhash.hash(0, std.mem.asBytes(&key));
    }

    // Lambda and closure compilation

    /// Compile a lambda expression as a standalone LLVM function.
    /// Optional closure_layout adds a single closure-data parameter between
    /// user params and roc_ops.
    fn compileLambdaAsFunc(
        self: *MonoLlvmCodeGen,
        lambda_expr_id: LirExprId,
        lambda: anytype,
        caller_ret_layout: layout.Idx,
        closure_layout: ?layout.Idx,
    ) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;

        const expr_id_raw: u32 = @intFromEnum(lambda_expr_id);
        const cache_key = lambdaCacheKey(lambda_expr_id, caller_ret_layout, closure_layout);

        if (self.compiled_lambdas.get(cache_key)) |func_idx| {
            return func_idx;
        }

        // Build param types from lambda params
        const params = self.store.getPatternSpan(lambda.params);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        var param_types: std.ArrayList(LlvmBuilder.Type) = .{};
        defer param_types.deinit(self.allocator);

        for (params) |param_id| {
            const param_layout = self.getPatternLayoutIdx(param_id) orelse unreachable;
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(param_layout)) catch return error.OutOfMemory;
        }

        // Optional closure-data parameter
        if (closure_layout) |cl| {
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(cl)) catch return error.OutOfMemory;
        }

        // Hidden roc_ops parameter at the end
        param_types.append(self.allocator, ptr_type) catch return error.OutOfMemory;

        const ret_type = try self.layoutToLlvmTypeFull(lambda.ret_layout);
        const fn_type = builder.fnType(ret_type, param_types.items, .normal) catch return error.OutOfMemory;
        const fn_params = fn_type.functionParameters(builder);
        if (fn_params.len != param_types.items.len or fn_params[fn_params.len - 1] != ptr_type) {
            std.debug.panic(
                "compileLambdaAsFunc bad fn_type for expr {d}: expected {d} params, last ptr={any}, got {d}",
                .{ expr_id_raw, param_types.items.len, fn_params.len != 0 and fn_params[fn_params.len - 1] == ptr_type, fn_params.len },
            );
        }

        // Create a unique function name
        var name_buf: [96]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_lambda_{d}_{x}", .{ expr_id_raw, cache_key }) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;

        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        // Register in cache BEFORE compiling body (enables recursion)
        self.compiled_lambdas.put(cache_key, func) catch return error.OutOfMemory;

        // Save outer state
        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        self.out_ptr = null;
        self.fn_out_ptr = null;

        // Save and clear symbol_values — LLVM functions are isolated
        const outer_symbols_1 = self.symbol_values;
        self.symbol_values = std.AutoHashMap(u64, LlvmBuilder.Value).init(self.allocator);
        defer {
            self.symbol_values.deinit();
            self.symbol_values = outer_symbols_1;
        }

        // Save and clear closure_bindings (they belong to outer scope)
        const outer_closure_bindings_1 = self.closure_bindings;
        self.closure_bindings = std.AutoHashMap(u64, ClosureMeta).init(self.allocator);
        defer {
            self.closure_bindings.deinit();
            self.closure_bindings = outer_closure_bindings_1;
        }

        // Create a new WipFunction for this lambda
        var lambda_wip_1 = LlvmBuilder.WipFunction.init(builder, .{
            .function = func,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer lambda_wip_1.deinit();

        self.wip = &lambda_wip_1;

        const entry_block = lambda_wip_1.block(0, "entry") catch return error.OutOfMemory;
        lambda_wip_1.cursor = .{ .block = entry_block };

        // Bind user params from function args 0..N-1
        for (params, 0..) |param_id, i| {
            const arg_val = lambda_wip_1.arg(@intCast(i));
            try self.bindPattern(param_id, arg_val);
        }

        // If closure_data parameter exists, extract captures and bind them
        if (closure_layout != null) {
            // The caller binds captures from the hidden closure-data parameter.
            // We only need to reserve the slot in the ABI here.
        }

        // Set roc_ops_arg to the hidden last parameter
        const roc_ops_idx: u32 = @intCast(param_types.items.len - 1);
        self.roc_ops_arg = lambda_wip_1.arg(roc_ops_idx);

        // Generate the body and coerce it to the lambda's declared return layout.
        const body_val = try self.generateControlFlowValue(lambda.body, lambda.ret_layout);
        _ = lambda_wip_1.ret(body_val) catch return error.CompilationFailed;

        lambda_wip_1.finish() catch return error.CompilationFailed;

        return func;
    }

    /// Compile a lambda with captures: bind capture symbols from the closure data parameter.
    fn compileLambdaWithCaptures(
        self: *MonoLlvmCodeGen,
        lambda_expr_id: LirExprId,
        lambda: anytype,
        ret_layout: layout.Idx,
        closure_layout: layout.Idx,
        captures: lir.LIR.LirCaptureSpan,
        representation: ClosureRepresentation,
    ) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;

        // Cache key
        const expr_id_raw: u32 = @intFromEnum(lambda_expr_id);
        const cache_key = lambdaCacheKey(lambda_expr_id, ret_layout, closure_layout);

        if (self.compiled_lambdas.get(cache_key)) |func_idx| {
            return func_idx;
        }

        // Build param types
        const params = self.store.getPatternSpan(lambda.params);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        var param_types: std.ArrayList(LlvmBuilder.Type) = .{};
        defer param_types.deinit(self.allocator);

        for (params) |param_id| {
            const param_layout = self.getPatternLayoutIdx(param_id) orelse unreachable;
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(param_layout)) catch return error.OutOfMemory;
        }

        // Closure-data parameter
        param_types.append(self.allocator, try self.layoutToLlvmTypeFull(closure_layout)) catch return error.OutOfMemory;

        // Hidden roc_ops parameter
        param_types.append(self.allocator, ptr_type) catch return error.OutOfMemory;

        const ret_type = try self.layoutToLlvmTypeFull(lambda.ret_layout);
        const fn_type = builder.fnType(ret_type, param_types.items, .normal) catch return error.OutOfMemory;
        const fn_params = fn_type.functionParameters(builder);
        if (fn_params.len != param_types.items.len or fn_params[fn_params.len - 1] != ptr_type) {
            std.debug.panic(
                "compileLambdaWithCaptures bad fn_type for expr {d}: expected {d} params, last ptr={any}, got {d}",
                .{ expr_id_raw, param_types.items.len, fn_params.len != 0 and fn_params[fn_params.len - 1] == ptr_type, fn_params.len },
            );
        }

        var name_buf: [96]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_lambda_{d}_{x}", .{ expr_id_raw, cache_key }) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;

        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;
        self.compiled_lambdas.put(cache_key, func) catch return error.OutOfMemory;

        // Save outer state
        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        self.out_ptr = null;
        self.fn_out_ptr = null;

        const outer_symbols_2 = self.symbol_values;
        self.symbol_values = std.AutoHashMap(u64, LlvmBuilder.Value).init(self.allocator);
        defer {
            self.symbol_values.deinit();
            self.symbol_values = outer_symbols_2;
        }

        const outer_closure_bindings_2 = self.closure_bindings;
        self.closure_bindings = std.AutoHashMap(u64, ClosureMeta).init(self.allocator);
        defer {
            self.closure_bindings.deinit();
            self.closure_bindings = outer_closure_bindings_2;
        }

        var lambda_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = func,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer lambda_wip.deinit();

        self.wip = &lambda_wip;

        const entry_block = lambda_wip.block(0, "entry") catch return error.OutOfMemory;
        lambda_wip.cursor = .{ .block = entry_block };

        // Bind user params
        for (params, 0..) |param_id, i| {
            const arg_val = lambda_wip.arg(@intCast(i));
            try self.bindPattern(param_id, arg_val);
        }

        // Extract captures from the closure data parameter
        const closure_arg_idx: u32 = @intCast(params.len);
        const closure_data_val = lambda_wip.arg(closure_arg_idx);
        try self.bindCapturesFromClosureData(closure_data_val, captures, representation);

        // Set roc_ops
        const roc_ops_idx: u32 = @intCast(param_types.items.len - 1);
        self.roc_ops_arg = lambda_wip.arg(roc_ops_idx);

        // Generate the body and coerce it to the lambda's declared return layout.
        const body_val = try self.generateControlFlowValue(lambda.body, lambda.ret_layout);
        _ = lambda_wip.ret(body_val) catch return error.CompilationFailed;

        lambda_wip.finish() catch return error.CompilationFailed;

        return func;
    }

    /// Extract individual captures from a closure-data value and bind to symbol_values.
    fn bindCapturesFromClosureData(
        self: *MonoLlvmCodeGen,
        closure_data: LlvmBuilder.Value,
        captures: lir.LIR.LirCaptureSpan,
        representation: ClosureRepresentation,
    ) Error!void {
        const capture_list = self.store.getCaptures(captures);
        if (capture_list.len == 0) return;

        switch (representation) {
            .unwrapped_capture => {
                // Single capture — the closure data IS the capture value
                if (capture_list.len >= 1) {
                    const cap = capture_list[0];
                    const key: u64 = @bitCast(cap.symbol);
                    self.symbol_values.put(key, closure_data) catch return error.OutOfMemory;
                }
            },
            .struct_captures => {
                // Multiple captures — extract each field from the struct
                const wip = self.wip orelse return error.CompilationFailed;
                for (capture_list, 0..) |cap, i| {
                    const field_val = wip.extractValue(closure_data, &.{@as(u32, @intCast(i))}, "") catch return error.CompilationFailed;
                    const key: u64 = @bitCast(cap.symbol);
                    self.symbol_values.put(key, field_val) catch return error.OutOfMemory;
                }
            },
            .union_repr => |ur| {
                // Extract captures from the payload portion of the union
                // The closure_data for a specific member is just the captures struct
                const member_captures = self.store.getCaptures(ur.captures);
                if (member_captures.len == 0) return;
                if (member_captures.len == 1) {
                    const cap = member_captures[0];
                    const key: u64 = @bitCast(cap.symbol);
                    self.symbol_values.put(key, closure_data) catch return error.OutOfMemory;
                } else {
                    const wip = self.wip orelse return error.CompilationFailed;
                    for (member_captures, 0..) |cap, i| {
                        const field_val = wip.extractValue(closure_data, &.{@as(u32, @intCast(i))}, "") catch return error.CompilationFailed;
                        const key: u64 = @bitCast(cap.symbol);
                        self.symbol_values.put(key, field_val) catch return error.OutOfMemory;
                    }
                }
            },
            .direct_call, .enum_dispatch => {
                // No captures to bind
            },
        }
    }

    /// Generate a lambda expression (not a call — just materializes the function).
    /// For standalone lambda expressions, we compile the function and return a dummy value.
    fn generateLambdaExpr(self: *MonoLlvmCodeGen, lambda: anytype, expr_id: LirExprId) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        // Compile the lambda as a function (will be called later)
        _ = try self.compileLambdaAsFunc(expr_id, lambda, lambda.ret_layout, null);
        // Lambda expressions that aren't immediately called produce a dummy value
        return builder.intValue(.i8, 0) catch return error.OutOfMemory;
    }

    /// Generate a closure expression — materializes the closure's runtime value.
    fn generateClosureExpr(self: *MonoLlvmCodeGen, closure: anytype, _: LirExprId) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;

        if (builtin.mode == .Debug) {
            const rep_name = switch (closure.representation) {
                .direct_call => "direct_call",
                .unwrapped_capture => "unwrapped_capture",
                .struct_captures => "struct_captures",
                .enum_dispatch => "enum_dispatch",
                .union_repr => "union_repr",
            };
            std.debug.print("LLVM closure repr: {s}\n", .{rep_name});
        }

        switch (closure.representation) {
            .direct_call => {
                // No runtime value needed — compile the lambda eagerly
                const inner_lambda = self.store.getExpr(closure.lambda);
                if (inner_lambda == .lambda) {
                    _ = try self.compileLambdaAsFunc(closure.lambda, inner_lambda.lambda, inner_lambda.lambda.ret_layout, null);
                }
                return builder.intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .unwrapped_capture => |_| {
                // Return the single captured value
                const capture_list = self.store.getCaptures(closure.captures);
                if (capture_list.len == 0) return builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const cap = capture_list[0];
                const cap_key: u64 = @bitCast(cap.symbol);
                if (self.symbol_values.get(cap_key)) |val| return val;
                // Try to look up via symbol def
                if (self.store.getSymbolDef(cap.symbol)) |def_id| {
                    const val = try self.generateExpr(def_id);
                    self.symbol_values.put(cap_key, val) catch return error.OutOfMemory;
                    return val;
                }
                unreachable; // Capture symbol must exist in symbol_values or as a top-level def
            },
            .struct_captures => |sc| {
                // Build LLVM struct of all capture values
                const wip = self.wip orelse return error.CompilationFailed;
                const capture_list = self.store.getCaptures(sc.captures);
                if (capture_list.len == 0) return builder.intValue(.i8, 0) catch return error.OutOfMemory;

                var field_values: [32]LlvmBuilder.Value = undefined;
                for (capture_list, 0..) |cap, i| {
                    const cap_key: u64 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(cap_key)) |val| {
                        field_values[i] = val;
                    } else if (self.store.getSymbolDef(cap.symbol)) |def_id| {
                        const val = try self.generateExpr(def_id);
                        self.symbol_values.put(cap_key, val) catch return error.OutOfMemory;
                        field_values[i] = val;
                    } else {
                        unreachable; // Capture symbol must exist in symbol_values or as a top-level def
                    }
                }

                const struct_type = try buildStructTypeFromValues(builder, wip, field_values[0..capture_list.len]);
                var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
                for (0..capture_list.len) |i| {
                    struct_val = wip.insertValue(struct_val, field_values[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
                }
                return struct_val;
            },
            .enum_dispatch => |ed| {
                // Return the tag as an i8 constant
                // Also eagerly compile all members
                const members = self.store.getLambdaSetMembers(ed.lambda_set);
                for (members) |member| {
                    const member_expr = self.store.getExpr(member.lambda_body);
                    if (member_expr == .lambda) {
                        _ = self.compileLambdaAsFunc(member.lambda_body, member_expr.lambda, member_expr.lambda.ret_layout, null) catch continue;
                    }
                }
                return builder.intValue(.i8, @as(u64, ed.tag)) catch return error.OutOfMemory;
            },
            .union_repr => |_| {
                // Build tagged union: {tag, captures...}
                // For now, just return the tag — dispatch functions handle this
                return builder.intValue(.i8, 0) catch return error.OutOfMemory;
            },
        }
    }

    /// Call a closure with args based on its ClosureRepresentation.
    fn callClosureWithArgs(self: *MonoLlvmCodeGen, closure: anytype, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        switch (closure.representation) {
            .direct_call => {
                // No captures — compile inner lambda and call directly
                const inner_expr = self.store.getExpr(closure.lambda);
                std.debug.assert(inner_expr == .lambda);
                const func_idx = try self.compileLambdaAsFunc(closure.lambda, inner_expr.lambda, ret_layout, null);
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, null), args_span, null, ret_layout);
            },
            .unwrapped_capture => |uc| {
                // Single capture — look up the capture value, pass as closure data
                const capture_list = self.store.getCaptures(closure.captures);
                std.debug.assert(capture_list.len != 0);
                const cap = capture_list[0];
                const cap_key: u64 = @bitCast(cap.symbol);
                const capture_val = self.symbol_values.get(cap_key) orelse blk: {
                    if (self.store.getSymbolDef(cap.symbol)) |def_id| {
                        const val = try self.generateExpr(def_id);
                        self.symbol_values.put(cap_key, val) catch return error.OutOfMemory;
                        break :blk val;
                    }
                    unreachable; // Capture symbol must exist in symbol_values or as a top-level def
                };

                const inner_expr = self.store.getExpr(closure.lambda);
                std.debug.assert(inner_expr == .lambda);
                const func_idx = try self.compileLambdaWithCaptures(
                    closure.lambda,
                    inner_expr.lambda,
                    ret_layout,
                    uc.capture_layout,
                    closure.captures,
                    closure.representation,
                );
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, uc.capture_layout), args_span, capture_val, ret_layout);
            },
            .struct_captures => |sc| {
                // Build captures struct, pass as closure data
                const capture_list = self.store.getCaptures(sc.captures);
                std.debug.assert(capture_list.len != 0);

                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;

                var field_values: [32]LlvmBuilder.Value = undefined;
                for (capture_list, 0..) |cap, i| {
                    const cap_key: u64 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(cap_key)) |val| {
                        field_values[i] = val;
                    } else if (self.store.getSymbolDef(cap.symbol)) |def_id| {
                        const val = try self.generateExpr(def_id);
                        self.symbol_values.put(cap_key, val) catch return error.OutOfMemory;
                        field_values[i] = val;
                    } else {
                        unreachable; // Capture symbol must exist in symbol_values or as a top-level def
                    }
                }

                const struct_type = try buildStructTypeFromValues(builder, wip, field_values[0..capture_list.len]);
                var captures_struct = builder.poisonValue(struct_type) catch return error.OutOfMemory;
                for (0..capture_list.len) |i| {
                    captures_struct = wip.insertValue(captures_struct, field_values[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
                }

                const inner_expr = self.store.getExpr(closure.lambda);
                std.debug.assert(inner_expr == .lambda);
                const func_idx = try self.compileLambdaWithCaptures(
                    closure.lambda,
                    inner_expr.lambda,
                    ret_layout,
                    sc.struct_layout,
                    closure.captures,
                    closure.representation,
                );
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, sc.struct_layout), args_span, captures_struct, ret_layout);
            },
            .enum_dispatch => |ed| {
                return self.dispatchEnumClosure(ed, args_span, ret_layout);
            },
            .union_repr => |ur| {
                return self.dispatchUnionClosure(ur, closure.captures, args_span, ret_layout);
            },
        }
    }

    /// Call a closure with args when we already have the closure value (from block evaluation).
    fn callClosureWithArgsAndValue(self: *MonoLlvmCodeGen, closure: anytype, args_span: anytype, ret_layout: layout.Idx, closure_val: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        switch (closure.representation) {
            .direct_call => {
                const inner_expr = self.store.getExpr(closure.lambda);
                std.debug.assert(inner_expr == .lambda);
                const func_idx = try self.compileLambdaAsFunc(closure.lambda, inner_expr.lambda, ret_layout, null);
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, null), args_span, null, ret_layout);
            },
            .unwrapped_capture => |uc| {
                const inner_expr = self.store.getExpr(closure.lambda);
                std.debug.assert(inner_expr == .lambda);
                const func_idx = try self.compileLambdaWithCaptures(
                    closure.lambda,
                    inner_expr.lambda,
                    ret_layout,
                    uc.capture_layout,
                    closure.captures,
                    closure.representation,
                );
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, uc.capture_layout), args_span, closure_val, ret_layout);
            },
            .struct_captures => |sc| {
                const inner_expr = self.store.getExpr(closure.lambda);
                std.debug.assert(inner_expr == .lambda);
                const func_idx = try self.compileLambdaWithCaptures(
                    closure.lambda,
                    inner_expr.lambda,
                    ret_layout,
                    sc.struct_layout,
                    closure.captures,
                    closure.representation,
                );
                return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, sc.struct_layout), args_span, closure_val, ret_layout);
            },
            .enum_dispatch => |ed| {
                return self.dispatchEnumClosure(ed, args_span, ret_layout);
            },
            .union_repr => |ur| {
                return self.dispatchUnionClosure(ur, closure.captures, args_span, ret_layout);
            },
        }
    }

    /// Call via ClosureMeta (from closure_bindings).
    fn callClosureMetaWithArgs(self: *MonoLlvmCodeGen, meta: ClosureMeta, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        const inner_expr = self.store.getExpr(meta.lambda);
        if (inner_expr != .lambda) return error.CompilationFailed;
        if (meta.representation != .direct_call) return error.CompilationFailed;

        const func_idx = try self.compileLambdaAsFunc(meta.lambda, inner_expr.lambda, ret_layout, null);
        return self.callCompiledFuncWithClosureData(func_idx, try self.buildLambdaFunctionType(inner_expr.lambda, null), args_span, null, ret_layout);
    }

    /// Call a compiled LLVM function with user args + optional closure data + roc_ops.
    fn callCompiledFuncWithClosureData(
        self: *MonoLlvmCodeGen,
        func_index: LlvmBuilder.Function.Index,
        fn_type: LlvmBuilder.Type,
        args_span: anytype,
        closure_data: ?LlvmBuilder.Value,
        _: layout.Idx,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const expected_params = fn_type.functionParameters(builder);

        const args = self.store.getExprSpan(args_span);
        var arg_values: std.ArrayList(LlvmBuilder.Value) = .{};
        defer arg_values.deinit(self.allocator);
        const expected_params_copy = self.allocator.dupe(LlvmBuilder.Type, expected_params) catch return error.OutOfMemory;
        defer self.allocator.free(expected_params_copy);

        for (args, 0..) |arg_id, i| {
            const arg = try self.generateExprAsValue(arg_id);
            const val = try self.coerceValueToType(arg.value, expected_params_copy[i], arg.layout_idx);
            arg_values.append(self.allocator, val) catch return error.OutOfMemory;
        }

        // Append closure data if present
        if (closure_data) |cd| {
            const closure_param_idx = arg_values.items.len;
            const coerced_closure = try self.coerceValueToType(cd, expected_params_copy[closure_param_idx], null);
            arg_values.append(self.allocator, coerced_closure) catch return error.OutOfMemory;
        }

        // Append hidden roc_ops parameter
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const roc_ops_idx = arg_values.items.len;
        if (roc_ops_idx >= expected_params_copy.len) {
            std.debug.panic(
                "callCompiledFuncWithClosureData missing roc_ops slot: user_args={d} closure={any} expected_params={d}",
                .{ args.len, closure_data != null, expected_params_copy.len },
            );
        }
        const expected_roc_ops_type = expected_params_copy[roc_ops_idx];
        const expected_roc_ops_raw = @intFromEnum(expected_roc_ops_type);
        if (std.enums.tagName(LlvmBuilder.Type, expected_roc_ops_type) == null and expected_roc_ops_raw > 1024) {
            std.debug.panic(
                "callCompiledFuncWithClosureData invalid roc_ops param type: raw=0x{x} user_args={d} closure={any} expected_params={d} func={d}",
                .{ expected_roc_ops_raw, args.len, closure_data != null, expected_params_copy.len, @intFromEnum(func_index) },
            );
        }
        const coerced_roc_ops = try self.coerceValueToType(roc_ops, expected_roc_ops_type, null);
        arg_values.append(self.allocator, coerced_roc_ops) catch return error.OutOfMemory;

        const callee = func_index.toValue(builder);
        for (expected_params_copy, arg_values.items, 0..) |expected_param, actual_arg, i| {
            const actual_type = actual_arg.typeOfWip(wip);
            if (expected_param != actual_type) {
                const expr_kind = if (i < args.len) @tagName(self.store.getExpr(args[i])) else "hidden";
                const expr_layout = if (i < args.len) blk: {
                    if (self.getExprResultLayout(args[i])) |layout_idx| {
                        break :blk @intFromEnum(layout_idx);
                    }
                    break :blk 0;
                } else 0;
                std.debug.panic(
                    "callCompiledFuncWithClosureData param mismatch at {d}: expr={s} layout={d} expected {f}, got {f}",
                    .{ i, expr_kind, expr_layout, expected_param.fmt(builder, .percent), actual_type.fmt(builder, .percent) },
                );
            }
        }

        return wip.call(.normal, .ccc, .none, fn_type, callee, arg_values.items, "") catch return error.CompilationFailed;
    }

    /// Handle chained calls: `(|a| |b| a*b)(5)(10)`.
    /// The inner call returns a closure value; we dispatch based on the inner result's representation.
    fn callChainedExpr(self: *MonoLlvmCodeGen, inner_call: anytype, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        if (self.resolveToClosureMeta(inner_call.fn_expr)) |meta| {
            return self.callClosureMetaWithArgs(meta, args_span, ret_layout);
        }

        return error.CompilationFailed;
    }

    /// IR introspection: trace fn_expr through lambdas/closures to find ClosureMeta.
    fn resolveToClosureMeta(self: *MonoLlvmCodeGen, fn_expr_id: LirExprId) ?ClosureMeta {
        const expr = self.store.getExpr(fn_expr_id);
        switch (expr) {
            .lambda => return .{
                .representation = .{ .direct_call = {} },
                .lambda = fn_expr_id,
                .captures = lir.LIR.LirCaptureSpan.empty(),
            },
            .block => |block_data| return self.resolveToClosureMeta(block_data.final_expr),
            .lookup => |lookup| {
                const symbol_key: u64 = @bitCast(lookup.symbol);
                if (self.closure_bindings.get(symbol_key)) |meta| {
                    return meta;
                }
                if (self.store.getSymbolDef(lookup.symbol)) |def_id| {
                    return self.resolveToClosureMeta(def_id);
                }
            },
            else => {},
        }
        return null;
    }

    /// Dispatch an enum_dispatch closure: LLVM switch on tag, each case calls that member's lambda.
    fn dispatchEnumClosure(self: *MonoLlvmCodeGen, ed: anytype, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        _ = self;
        _ = ed;
        _ = args_span;
        _ = ret_layout;
        return error.CompilationFailed;
    }

    /// Dispatch a union_repr closure: LLVM switch on tag, each case extracts captures and calls.
    fn dispatchUnionClosure(self: *MonoLlvmCodeGen, ur: anytype, _: lir.LIR.LirCaptureSpan, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        _ = self;
        _ = ur;
        _ = args_span;
        _ = ret_layout;
        return error.CompilationFailed;
    }

    /// Extract layout index from a pattern (for lambda parameter typing).
    fn getPatternLayoutIdx(self: *MonoLlvmCodeGen, pattern_id: LirPatternId) ?layout.Idx {
        const pattern = self.store.getPattern(pattern_id);
        return switch (pattern) {
            .bind => |b| b.layout_idx,
            .wildcard => |w| w.layout_idx,
            .tag => |t| t.union_layout,
            .struct_ => |s| s.struct_layout,
            .list => |l| l.list_layout,
            .as_pattern => |a| a.layout_idx,
            else => null,
        };
    }

    // Pattern binding helpers

    /// Load a value from a pointer based on layout type.
    /// Handles scalars, composite types (str/list as 24-byte {ptr,i64,i64}), and aggregates.
    fn loadValueFromPtr(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Composite types (str/list) are 24-byte structs: {ptr, i64, i64}
        if (layout_idx == .str) {
            const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
            const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
            const roc_struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
            return wip.load(.normal, roc_struct_type, ptr, alignment, "") catch return error.CompilationFailed;
        }
        if (self.layout_store) |ls| {
            const l = ls.getLayout(layout_idx);
            switch (l.tag) {
                .list, .list_of_zst => {
                    const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    const roc_struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
                    return wip.load(.normal, roc_struct_type, ptr, alignment, "") catch return error.CompilationFailed;
                },
                .struct_ => {
                    const llvm_type = try self.layoutToLlvmTypeFull(layout_idx);
                    const sa = ls.layoutSizeAlign(l);
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(sa.alignment.toByteUnits(), 1)));
                    return wip.load(.normal, llvm_type, ptr, alignment, "") catch return error.CompilationFailed;
                },
                .tag_union => {
                    // Tag unions are pointer-based in this backend — return
                    // the raw pointer so the discriminant + payload are accessible.
                    return ptr;
                },
                else => {},
            }
        }

        // Scalar types
        const elem_type = layoutToLlvmType(layout_idx);
        return wip.load(.normal, elem_type, ptr, .default, "") catch return error.CompilationFailed;
    }

    const ListElementInfo = struct {
        elem_layout_idx: layout.Idx,
        elem_size: u64,
        elem_align: u32,
        elements_refcounted: bool,
    };

    const SortComparatorThunk = struct {
        fn_ptr: LlvmBuilder.Value,
        cmp_data: LlvmBuilder.Value,
    };

    fn getListElementInfo(self: *MonoLlvmCodeGen, list_layout_idx: layout.Idx) Error!ListElementInfo {
        const ls = self.layout_store orelse return error.CompilationFailed;
        const list_layout = ls.getLayout(list_layout_idx);
        const elem_layout_idx = switch (list_layout.tag) {
            .list => list_layout.data.list,
            .list_of_zst => layout.Idx.zst,
            else => return error.CompilationFailed,
        };
        const elem_layout = ls.getLayout(elem_layout_idx);
        const elem_sa = ls.layoutSizeAlign(elem_layout);
        return .{
            .elem_layout_idx = elem_layout_idx,
            .elem_size = elem_sa.size,
            .elem_align = @intCast(elem_sa.alignment.toByteUnits()),
            .elements_refcounted = ls.layoutContainsRefcounted(elem_layout),
        };
    }

    fn generateListSortWith(self: *MonoLlvmCodeGen, list_expr_id: LirExprId, cmp_expr_id: LirExprId, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        const list_ptr = try self.materializeAsPtr(list_expr_id, 24);
        const list_bytes = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
        const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
        const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

        const elem_info = try self.getListElementInfo(ret_layout);
        if (elem_info.elem_size == 0) {
            const list_size_val = builder.intValue(.i32, 24) catch return error.OutOfMemory;
            _ = wip.callMemCpy(dest_ptr, alignment, list_ptr, alignment, list_size_val, .normal, false) catch return error.CompilationFailed;
            return .none;
        }

        const cmp_thunk = try self.buildListSortComparatorThunk(cmp_expr_id, elem_info);
        const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
        const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;

        _ = try self.callBuiltin("roc_builtins_list_sort_with", .void, &.{
            ptr_type, ptr_type, .i64, .i64, ptr_type, ptr_type, .i32, .i64, ptr_type,
        }, &.{
            dest_ptr, list_bytes, list_len, list_cap, cmp_thunk.fn_ptr, cmp_thunk.cmp_data, align_val, width_val, roc_ops,
        });

        return .none;
    }

    fn buildListSortComparatorThunk(self: *MonoLlvmCodeGen, cmp_expr_id: LirExprId, elem_info: ListElementInfo) Error!SortComparatorThunk {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const meta = self.resolveToClosureMeta(cmp_expr_id) orelse return error.CompilationFailed;
        switch (meta.representation) {
            .direct_call => {},
            else => return error.CompilationFailed,
        }
        if (self.store.getCaptures(meta.captures).len != 0) {
            return error.CompilationFailed;
        }

        const lambda_expr = self.store.getExpr(meta.lambda);
        if (lambda_expr != .lambda) return error.CompilationFailed;
        const lambda = lambda_expr.lambda;

        const func_idx = try self.compileLambdaAsFunc(meta.lambda, lambda, lambda.ret_layout, null);
        const thunk_idx = try self.compileListSortComparatorThunk(meta.lambda, lambda, func_idx, elem_info);
        var fn_ptr = thunk_idx.toValue(builder);
        if (fn_ptr.typeOfWip(wip) != ptr_type) {
            fn_ptr = wip.cast(.bitcast, fn_ptr, ptr_type, "") catch return error.CompilationFailed;
        }

        return .{
            .fn_ptr = fn_ptr,
            .cmp_data = self.roc_ops_arg orelse return error.CompilationFailed,
        };
    }

    fn compileListSortComparatorThunk(
        self: *MonoLlvmCodeGen,
        lambda_expr_id: LirExprId,
        lambda: anytype,
        func_idx: LlvmBuilder.Function.Index,
        elem_info: ListElementInfo,
    ) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const fn_type = builder.fnType(.i8, &.{ ptr_type, ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
        var name_buf: [96]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_sort_cmp_{d}_{d}", .{
            @intFromEnum(lambda_expr_id),
            @intFromEnum(elem_info.elem_layout_idx),
        }) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;
        const thunk_fn = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        self.out_ptr = null;
        self.fn_out_ptr = null;

        const outer_symbols = self.symbol_values;
        self.symbol_values = std.AutoHashMap(u64, LlvmBuilder.Value).init(self.allocator);
        defer {
            self.symbol_values.deinit();
            self.symbol_values = outer_symbols;
        }

        const outer_closure_bindings = self.closure_bindings;
        self.closure_bindings = std.AutoHashMap(u64, ClosureMeta).init(self.allocator);
        defer {
            self.closure_bindings.deinit();
            self.closure_bindings = outer_closure_bindings;
        }

        var thunk_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = thunk_fn,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer thunk_wip.deinit();

        self.wip = &thunk_wip;

        const entry_block = thunk_wip.block(0, "entry") catch return error.OutOfMemory;
        thunk_wip.cursor = .{ .block = entry_block };

        const cmp_data = thunk_wip.arg(0);
        const a_ptr = thunk_wip.arg(1);
        const b_ptr = thunk_wip.arg(2);
        self.roc_ops_arg = cmp_data;

        const params = self.store.getPatternSpan(lambda.params);
        if (params.len != 2) return error.CompilationFailed;

        const elem_alignment = LlvmBuilder.Alignment.fromByteUnits(@max(@as(u64, elem_info.elem_align), 1));
        const param0_layout = self.getPatternLayoutIdx(params[0]) orelse return error.CompilationFailed;
        const param1_layout = self.getPatternLayoutIdx(params[1]) orelse return error.CompilationFailed;
        const param0_type = try self.layoutToLlvmTypeFull(param0_layout);
        const param1_type = try self.layoutToLlvmTypeFull(param1_layout);
        const lhs = thunk_wip.load(.normal, param0_type, a_ptr, elem_alignment, "") catch return error.CompilationFailed;
        const rhs = thunk_wip.load(.normal, param1_type, b_ptr, elem_alignment, "") catch return error.CompilationFailed;

        const lambda_fn_type = try self.buildLambdaFunctionType(lambda, null);
        const callee = func_idx.toValue(builder);
        const cmp_result = thunk_wip.call(.normal, .ccc, .none, lambda_fn_type, callee, &.{ lhs, rhs, cmp_data }, "") catch return error.CompilationFailed;
        const coerced = try self.coerceValueToType(cmp_result, .i8, lambda.ret_layout);
        _ = thunk_wip.ret(coerced) catch return error.CompilationFailed;
        thunk_wip.finish() catch return error.CompilationFailed;

        return thunk_fn;
    }

    /// Build an LLVM struct type for a record layout by looking up each field's layout.
    fn buildLlvmTypeForRecordLayout(self: *MonoLlvmCodeGen, ls: *const layout.Store, record_idx: anytype) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;
        const record_data = ls.getRecordData(record_idx);
        const field_count = record_data.getFields().count;
        var field_types: [32]LlvmBuilder.Type = undefined;
        for (0..field_count) |i| {
            const field_layout = ls.getRecordFieldLayout(record_idx, @intCast(i));
            field_types[i] = try self.layoutToLlvmTypeForLoad(field_layout);
        }
        return builder.structType(.normal, field_types[0..field_count]) catch return error.OutOfMemory;
    }

    /// Build an LLVM struct type for a tuple layout by looking up each element's layout.
    fn buildLlvmTypeForTupleLayout(self: *MonoLlvmCodeGen, ls: *const layout.Store, tuple_idx: anytype) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;
        const tuple_data = ls.getTupleData(tuple_idx);
        const sorted_elements = ls.tuple_fields.sliceRange(tuple_data.getFields());
        const elem_count = sorted_elements.len;
        var field_types: [32]LlvmBuilder.Type = undefined;
        for (0..elem_count) |i| {
            const elem = sorted_elements.get(@intCast(i));
            field_types[i] = try self.layoutToLlvmTypeForLoad(elem.layout);
        }
        return builder.structType(.normal, field_types[0..elem_count]) catch return error.OutOfMemory;
    }

    /// Convert a layout index to an LLVM type suitable for memory loads.
    /// Uses struct field types (bool→i8) for correct memory layout matching.
    fn layoutToLlvmTypeForLoad(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;
        return switch (layout_idx) {
            .bool => .i8, // bools stored as i8 in structs
            .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            .str => blk: {
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                break :blk builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
            },
            else => blk: {
                if (self.layout_store) |ls| {
                    const l = ls.getLayout(layout_idx);
                    switch (l.tag) {
                        .list, .list_of_zst => {
                            const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                            break :blk builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
                        },
                        .struct_ => break :blk try self.layoutToLlvmTypeFull(layout_idx),
                        else => {},
                    }
                }
                break :blk try self.layoutToStructFieldType(layout_idx);
            },
        };
    }

    /// Build a rest-sublist value: creates a list struct {ptr + prefix_len*elem_size, len - prefix_len, 0}
    /// representing the remainder of a list after prefix elements have been matched.
    fn buildRestSublist(self: *MonoLlvmCodeGen, data_ptr: LlvmBuilder.Value, list_len: LlvmBuilder.Value, prefix_len: LlvmBuilder.Value, elem_size: u64) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // rest_ptr = data_ptr + prefix_len * elem_size (via GEP on i8)
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const elem_size_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
        const byte_offset = wip.bin(.mul, prefix_len, elem_size_val, "") catch return error.CompilationFailed;
        const rest_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;

        // rest_len = list_len - prefix_len
        const rest_len = wip.bin(.sub, list_len, prefix_len, "") catch return error.CompilationFailed;

        // Build {ptr, len, cap=0} struct
        const roc_list_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
        const zero_cap = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        var result = builder.poisonValue(roc_list_type) catch return error.OutOfMemory;
        result = wip.insertValue(result, rest_ptr, &.{0}, "") catch return error.CompilationFailed;
        result = wip.insertValue(result, rest_len, &.{1}, "") catch return error.CompilationFailed;
        result = wip.insertValue(result, zero_cap, &.{2}, "") catch return error.CompilationFailed;
        return result;
    }

    /// Bind a pattern to an LLVM value.
    /// If the symbol has a loop variable alloca, also stores the value there
    /// so that post-loop code can load the final value.
    fn bindPattern(self: *MonoLlvmCodeGen, pattern_id: LirPatternId, value: LlvmBuilder.Value) Error!void {
        const pattern = self.store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| {
                const key: u64 = @bitCast(bind.symbol);
                self.symbol_values.put(key, value) catch return error.OutOfMemory;

                // If this symbol has a loop variable alloca, store the updated value
                if (self.loop_var_allocas.get(key)) |lva| {
                    const wip = self.wip orelse return error.CompilationFailed;
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    _ = wip.store(.normal, value, lva.alloca_ptr, alignment) catch return error.CompilationFailed;
                }
            },
            .wildcard => {},
            .struct_ => |struct_pat| {
                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;
                if (value == .none or !value.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;
                const fields = self.store.getPatternSpan(struct_pat.fields);
                for (fields, 0..) |field_pat_id, idx| {
                    const field_val = wip.extractValue(value, &.{@as(u32, @intCast(idx))}, "") catch return error.CompilationFailed;
                    try self.bindPattern(field_pat_id, field_val);
                }
            },
            .list => |list_pat| {
                // List destructuring pattern: extract prefix elements from the list value
                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;
                const ls = self.layout_store orelse unreachable;

                // Guard: value must be a {ptr, i64, i64} struct
                if (value == .none or !value.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;

                const prefix_patterns = self.store.getPatternSpan(list_pat.prefix);

                // The value is a {ptr, i64, i64} struct. Extract data pointer and length.
                const data_ptr = wip.extractValue(value, &.{0}, "") catch return error.CompilationFailed;
                const list_len = wip.extractValue(value, &.{1}, "") catch return error.CompilationFailed;

                // Get element size info
                const elem_layout_data = ls.getLayout(list_pat.elem_layout);
                const elem_sa = ls.layoutSizeAlign(elem_layout_data);
                const elem_size: u64 = elem_sa.size;

                // Bind each prefix element
                for (prefix_patterns, 0..) |sub_pat_id, idx| {
                    const byte_off = builder.intValue(.i64, @as(u64, @intCast(idx)) * elem_size) catch return error.OutOfMemory;
                    const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_off}, "") catch return error.CompilationFailed;
                    const elem_val = try self.loadValueFromPtr(elem_ptr, list_pat.elem_layout);
                    try self.bindPattern(sub_pat_id, elem_val);
                }

                // Handle rest pattern if present (.. as rest)
                if (!list_pat.rest.isNone()) {
                    const expected_len = builder.intValue(.i64, @as(u64, @intCast(prefix_patterns.len))) catch return error.OutOfMemory;
                    const rest_val = try self.buildRestSublist(data_ptr, list_len, expected_len, elem_size);
                    try self.bindPattern(list_pat.rest, rest_val);
                }
            },
            else => {},
        }
    }
};
