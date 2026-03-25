//! LIR to LLVM Code Generator
//!
//! This module generates LLVM IR from LIR (Low-level IR) expressions.
//! It uses Roc's in-tree LLVM builder for IR generation.
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
//!
const std = @import("std");
const builtin = @import("builtin");
const layout = @import("layout");
const lir = @import("lir");

const LlvmBuilder = @import("Builder.zig");

const LirExprStore = lir.LirExprStore;
const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const Symbol = lir.Symbol;
const LirProcSpec = lir.LirProcSpec;
const CFStmtId = lir.CFStmtId;

const Allocator = std.mem.Allocator;

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
    target: std.Target,
    triple: []const u8,
    builtin_symbol_mode: BuiltinSymbolMode,

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

    /// Lazily generated RC helper functions keyed by canonical layout helper identity
    /// and the storage convention used by the helper input pointer.
    compiled_rc_helpers: std.AutoHashMap(u64, LlvmBuilder.Function.Index),

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

    const LoopVarAlloca = struct {
        alloca_ptr: LlvmBuilder.Value,
        elem_type: LlvmBuilder.Type,
        alignment: LlvmBuilder.Alignment,
    };

    const BuiltinSymbolMode = enum {
        bitcode,
        native_object,
    };

    const ScopeSnapshot = struct {
        symbol_keys: std.AutoHashMap(u64, void),
        cell_keys: std.AutoHashMap(u64, void),

        fn init(self: *const MonoLlvmCodeGen) Error!ScopeSnapshot {
            var snapshot = ScopeSnapshot{
                .symbol_keys = std.AutoHashMap(u64, void).init(self.allocator),
                .cell_keys = std.AutoHashMap(u64, void).init(self.allocator),
            };
            errdefer snapshot.deinit();

            var symbol_it = self.symbol_values.keyIterator();
            while (symbol_it.next()) |key| {
                try snapshot.symbol_keys.put(key.*, {});
            }

            var cell_it = self.cell_allocas.keyIterator();
            while (cell_it.next()) |key| {
                try snapshot.cell_keys.put(key.*, {});
            }

            return snapshot;
        }

        fn deinit(self: *ScopeSnapshot) void {
            self.symbol_keys.deinit();
            self.cell_keys.deinit();
        }
    };

    const LexicalEnvSnapshot = struct {
        symbol_values: std.AutoHashMap(u64, LlvmBuilder.Value),
        loop_var_allocas: std.AutoHashMap(u64, LoopVarAlloca),
        cell_allocas: std.AutoHashMap(u64, LoopVarAlloca),

        fn init(self: *const MonoLlvmCodeGen) Error!LexicalEnvSnapshot {
            var snapshot = LexicalEnvSnapshot{
                .symbol_values = std.AutoHashMap(u64, LlvmBuilder.Value).init(self.allocator),
                .loop_var_allocas = std.AutoHashMap(u64, LoopVarAlloca).init(self.allocator),
                .cell_allocas = std.AutoHashMap(u64, LoopVarAlloca).init(self.allocator),
            };
            errdefer snapshot.deinit();

            var symbol_it = self.symbol_values.iterator();
            while (symbol_it.next()) |entry| {
                try snapshot.symbol_values.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            var loop_it = self.loop_var_allocas.iterator();
            while (loop_it.next()) |entry| {
                try snapshot.loop_var_allocas.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                try snapshot.cell_allocas.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            return snapshot;
        }

        fn restore(self: *const LexicalEnvSnapshot, codegen: *MonoLlvmCodeGen) Error!void {
            codegen.symbol_values.clearRetainingCapacity();
            codegen.loop_var_allocas.clearRetainingCapacity();
            codegen.cell_allocas.clearRetainingCapacity();

            var symbol_it = self.symbol_values.iterator();
            while (symbol_it.next()) |entry| {
                try codegen.symbol_values.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            var loop_it = self.loop_var_allocas.iterator();
            while (loop_it.next()) |entry| {
                try codegen.loop_var_allocas.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                try codegen.cell_allocas.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }

        fn deinit(self: *LexicalEnvSnapshot) void {
            self.symbol_values.deinit();
            self.loop_var_allocas.deinit();
            self.cell_allocas.deinit();
        }
    };

    const FunctionState = struct {
        symbol_values: std.AutoHashMap(u64, LlvmBuilder.Value),
        join_points: std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index),
        join_point_params: std.AutoHashMap(u32, []LirPatternId),
        join_param_allocas: std.AutoHashMap(u32, []LlvmBuilder.Value),
        join_param_types: std.AutoHashMap(u32, []LlvmBuilder.Type),
        loop_var_allocas: std.AutoHashMap(u64, LoopVarAlloca),
        loop_exit_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index),
        cell_allocas: std.AutoHashMap(u64, LoopVarAlloca),
    };

    fn initFunctionState(self: *const MonoLlvmCodeGen) FunctionState {
        return .{
            .symbol_values = std.AutoHashMap(u64, LlvmBuilder.Value).init(self.allocator),
            .join_points = std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index).init(self.allocator),
            .join_point_params = std.AutoHashMap(u32, []LirPatternId).init(self.allocator),
            .join_param_allocas = std.AutoHashMap(u32, []LlvmBuilder.Value).init(self.allocator),
            .join_param_types = std.AutoHashMap(u32, []LlvmBuilder.Type).init(self.allocator),
            .loop_var_allocas = std.AutoHashMap(u64, LoopVarAlloca).init(self.allocator),
            .loop_exit_blocks = .empty,
            .cell_allocas = std.AutoHashMap(u64, LoopVarAlloca).init(self.allocator),
        };
    }

    fn captureFunctionState(self: *MonoLlvmCodeGen) FunctionState {
        return .{
            .symbol_values = self.symbol_values,
            .join_points = self.join_points,
            .join_point_params = self.join_point_params,
            .join_param_allocas = self.join_param_allocas,
            .join_param_types = self.join_param_types,
            .loop_var_allocas = self.loop_var_allocas,
            .loop_exit_blocks = self.loop_exit_blocks,
            .cell_allocas = self.cell_allocas,
        };
    }

    fn applyFunctionState(self: *MonoLlvmCodeGen, state: FunctionState) void {
        self.symbol_values = state.symbol_values;
        self.join_points = state.join_points;
        self.join_point_params = state.join_point_params;
        self.join_param_allocas = state.join_param_allocas;
        self.join_param_types = state.join_param_types;
        self.loop_var_allocas = state.loop_var_allocas;
        self.loop_exit_blocks = state.loop_exit_blocks;
        self.cell_allocas = state.cell_allocas;
    }

    fn deinitFunctionState(self: *MonoLlvmCodeGen, state: *FunctionState) void {
        state.symbol_values.deinit();
        state.join_points.deinit();

        var params_it = state.join_point_params.valueIterator();
        while (params_it.next()) |params| {
            self.allocator.free(params.*);
        }
        state.join_point_params.deinit();

        var allocas_it = state.join_param_allocas.valueIterator();
        while (allocas_it.next()) |allocas| {
            self.allocator.free(allocas.*);
        }
        state.join_param_allocas.deinit();

        var types_it = state.join_param_types.valueIterator();
        while (types_it.next()) |types| {
            self.allocator.free(types.*);
        }
        state.join_param_types.deinit();

        state.loop_var_allocas.deinit();
        state.loop_exit_blocks.deinit(self.allocator);
        state.cell_allocas.deinit();
    }

    fn swapInFreshFunctionState(self: *MonoLlvmCodeGen) FunctionState {
        const outer = self.captureFunctionState();
        self.applyFunctionState(self.initFunctionState());
        return outer;
    }

    fn restoreFunctionState(self: *MonoLlvmCodeGen, outer: FunctionState) void {
        var current = self.captureFunctionState();
        self.deinitFunctionState(&current);
        self.applyFunctionState(outer);
    }

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        result_layout: layout.Idx,
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    pub const ModuleBitcodeResult = struct {
        bitcode: []const u32,
        allocator: Allocator,

        pub fn deinit(self: *ModuleBitcodeResult) void {
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
        return initWithTarget(allocator, store, builtin.target, getLlvmTriple());
    }

    pub fn initWithTarget(
        allocator: Allocator,
        store: *const LirExprStore,
        target: std.Target,
        triple: []const u8,
    ) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .target = target,
            .triple = triple,
            .builtin_symbol_mode = .bitcode,
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
            .builtin_functions = std.StringHashMap(LlvmBuilder.Function.Index).init(allocator),
            .compiled_rc_helpers = std.AutoHashMap(u64, LlvmBuilder.Function.Index).init(allocator),
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
        self.builtin_functions.deinit();
        self.compiled_rc_helpers.deinit();
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
        self.builtin_functions.clearRetainingCapacity();
        self.compiled_rc_helpers.clearRetainingCapacity();
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
        const fn_name = if (self.builtin_symbol_mode == .native_object)
            try self.exportedFunctionName(builder, name)
        else
            builder.strtabString(name) catch return error.OutOfMemory;
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

    fn callFunctionIndex(
        self: *MonoLlvmCodeGen,
        func_idx: LlvmBuilder.Function.Index,
        args: []const LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const fn_type = func_idx.typeOf(builder);
        const callee = func_idx.toValue(builder);
        return wip.call(.normal, .ccc, .none, fn_type, callee, args, "") catch return error.CompilationFailed;
    }

    fn ptrSizedIntType(self: *const MonoLlvmCodeGen) LlvmBuilder.Type {
        return switch (self.target.ptrBitWidth()) {
            16 => .i16,
            32 => .i32,
            64 => .i64,
            else => .i64,
        };
    }

    fn emitPtrSizedInt(self: *MonoLlvmCodeGen, value: u64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return builder.intValue(self.ptrSizedIntType(), value) catch return error.OutOfMemory;
    }

    const PointerValueStorage = enum(u1) {
        materialized_value,
        field_slot,
    };

    fn emitRcHelperCall(
        self: *MonoLlvmCodeGen,
        helper_key: layout.RcHelperKey,
        value_ptr: LlvmBuilder.Value,
        count: ?LlvmBuilder.Value,
        roc_ops: LlvmBuilder.Value,
        storage: PointerValueStorage,
    ) Error!void {
        const resolver = layout.RcHelperResolver.init(self.layout_store orelse return error.CompilationFailed);
        if (resolver.plan(helper_key) == .noop) return;

        const func_idx = try self.compileRcHelper(helper_key, storage);
        switch (helper_key.op) {
            .incref => _ = try self.callFunctionIndex(func_idx, &.{ value_ptr, count orelse return error.CompilationFailed, roc_ops }),
            .decref, .free => _ = try self.callFunctionIndex(func_idx, &.{ value_ptr, roc_ops }),
        }
    }

    fn emitRcWrapperCall(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        param_types: []const LlvmBuilder.Type,
        args: []const LlvmBuilder.Value,
    ) Error!void {
        _ = try self.callBuiltin(name, .void, param_types, args);
    }

    fn rcHelperCacheKey(helper_key: layout.RcHelperKey, storage: PointerValueStorage) u64 {
        return (helper_key.encode() << 1) | @intFromEnum(storage);
    }

    fn compileRcHelper(
        self: *MonoLlvmCodeGen,
        helper_key: layout.RcHelperKey,
        storage: PointerValueStorage,
    ) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        const resolver = layout.RcHelperResolver.init(self.layout_store orelse return error.CompilationFailed);
        if (resolver.plan(helper_key) == .noop) return error.CompilationFailed;

        const cache_key = rcHelperCacheKey(helper_key, storage);
        if (self.compiled_rc_helpers.get(cache_key)) |func_idx| {
            return func_idx;
        }

        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const count_type = self.ptrSizedIntType();
        const fn_type = switch (helper_key.op) {
            .incref => builder.fnType(.void, &.{ ptr_type, count_type, ptr_type }, .normal) catch return error.OutOfMemory,
            .decref, .free => builder.fnType(.void, &.{ ptr_type, ptr_type }, .normal) catch return error.OutOfMemory,
        };

        var name_buf: [96]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_rc_{s}_{d}_{s}", .{
            @tagName(helper_key.op),
            @intFromEnum(helper_key.layout_idx),
            @tagName(storage),
        }) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;
        const func_idx = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;
        func_idx.setLinkage(.internal, builder);
        self.compiled_rc_helpers.put(cache_key, func_idx) catch return error.OutOfMemory;

        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        const outer_result_layout = self.result_layout;
        defer self.result_layout = outer_result_layout;
        self.out_ptr = null;
        self.fn_out_ptr = null;
        self.result_layout = null;

        const outer_fn_state = self.swapInFreshFunctionState();
        defer self.restoreFunctionState(outer_fn_state);

        var helper_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = func_idx,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer helper_wip.deinit();

        self.wip = &helper_wip;

        const entry_block = helper_wip.block(0, "entry") catch return error.OutOfMemory;
        helper_wip.cursor = .{ .block = entry_block };

        const value_ptr = helper_wip.arg(0);
        const count_arg: ?LlvmBuilder.Value = switch (helper_key.op) {
            .incref => helper_wip.arg(1),
            .decref, .free => null,
        };
        self.roc_ops_arg = switch (helper_key.op) {
            .incref => helper_wip.arg(2),
            .decref, .free => helper_wip.arg(1),
        };

        try self.generateRcHelperBody(helper_key, value_ptr, count_arg, storage);

        if (!self.currentBlockHasTerminator()) {
            _ = helper_wip.retVoid() catch return error.CompilationFailed;
        }
        helper_wip.finish() catch return error.CompilationFailed;

        return func_idx;
    }

    fn generateRcHelperBody(
        self: *MonoLlvmCodeGen,
        helper_key: layout.RcHelperKey,
        value_ptr: LlvmBuilder.Value,
        count: ?LlvmBuilder.Value,
        storage: PointerValueStorage,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const count_type = self.ptrSizedIntType();
        const resolver = layout.RcHelperResolver.init(ls);

        switch (resolver.plan(helper_key)) {
            .noop => {},
            .str_incref => {
                try self.emitRcWrapperCall(
                    "roc_builtins_str_incref_value",
                    &.{ ptr_type, count_type, ptr_type },
                    &.{ value_ptr, count orelse return error.CompilationFailed, roc_ops },
                );
            },
            .str_decref => {
                try self.emitRcWrapperCall(
                    "roc_builtins_str_decref_value",
                    &.{ ptr_type, ptr_type },
                    &.{ value_ptr, roc_ops },
                );
            },
            .str_free => {
                try self.emitRcWrapperCall(
                    "roc_builtins_str_free_value",
                    &.{ ptr_type, ptr_type },
                    &.{ value_ptr, roc_ops },
                );
            },
            .list_incref => {
                const elem_info = try self.getListElementInfo(helper_key.layout_idx);
                const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;
                try self.emitRcWrapperCall(
                    "roc_builtins_list_incref_value",
                    &.{ ptr_type, .i1, count_type, ptr_type },
                    &.{ value_ptr, elements_refcounted_val, count orelse return error.CompilationFailed, roc_ops },
                );
            },
            .list_decref => |list_plan| {
                var callback_val = builder.nullValue(ptr_type) catch return error.OutOfMemory;
                if (list_plan.child) |child_key| {
                    var fn_ptr = (try self.compileRcHelper(child_key, .materialized_value)).toValue(builder);
                    if (fn_ptr.typeOfWip(wip) != ptr_type) {
                        fn_ptr = wip.cast(.bitcast, fn_ptr, ptr_type, "") catch return error.CompilationFailed;
                    }
                    callback_val = fn_ptr;
                }

                try self.emitRcWrapperCall(
                    "roc_builtins_list_decref_value",
                    &.{ ptr_type, .i32, count_type, ptr_type, ptr_type },
                    &.{
                        value_ptr,
                        builder.intValue(.i32, list_plan.elem_alignment) catch return error.OutOfMemory,
                        try self.emitPtrSizedInt(list_plan.elem_width),
                        callback_val,
                        roc_ops,
                    },
                );
            },
            .list_free => |list_plan| {
                var callback_val = builder.nullValue(ptr_type) catch return error.OutOfMemory;
                if (list_plan.child) |child_key| {
                    var fn_ptr = (try self.compileRcHelper(child_key, .materialized_value)).toValue(builder);
                    if (fn_ptr.typeOfWip(wip) != ptr_type) {
                        fn_ptr = wip.cast(.bitcast, fn_ptr, ptr_type, "") catch return error.CompilationFailed;
                    }
                    callback_val = fn_ptr;
                }

                try self.emitRcWrapperCall(
                    "roc_builtins_list_free_value",
                    &.{ ptr_type, .i32, count_type, ptr_type, ptr_type },
                    &.{
                        value_ptr,
                        builder.intValue(.i32, list_plan.elem_alignment) catch return error.OutOfMemory,
                        try self.emitPtrSizedInt(list_plan.elem_width),
                        callback_val,
                        roc_ops,
                    },
                );
            },
            .box_incref => {
                try self.emitRcWrapperCall(
                    "roc_builtins_box_incref_value",
                    &.{ ptr_type, count_type, ptr_type },
                    &.{ value_ptr, count orelse return error.CompilationFailed, roc_ops },
                );
            },
            .box_decref => |box_plan| {
                var callback_val = builder.nullValue(ptr_type) catch return error.OutOfMemory;
                if (box_plan.child) |child_key| {
                    var fn_ptr = (try self.compileRcHelper(child_key, .materialized_value)).toValue(builder);
                    if (fn_ptr.typeOfWip(wip) != ptr_type) {
                        fn_ptr = wip.cast(.bitcast, fn_ptr, ptr_type, "") catch return error.CompilationFailed;
                    }
                    callback_val = fn_ptr;
                }

                try self.emitRcWrapperCall(
                    "roc_builtins_box_decref_value",
                    &.{ ptr_type, .i32, ptr_type, ptr_type },
                    &.{
                        value_ptr,
                        builder.intValue(.i32, box_plan.elem_alignment) catch return error.OutOfMemory,
                        callback_val,
                        roc_ops,
                    },
                );
            },
            .box_free => |box_plan| {
                var callback_val = builder.nullValue(ptr_type) catch return error.OutOfMemory;
                if (box_plan.child) |child_key| {
                    var fn_ptr = (try self.compileRcHelper(child_key, .materialized_value)).toValue(builder);
                    if (fn_ptr.typeOfWip(wip) != ptr_type) {
                        fn_ptr = wip.cast(.bitcast, fn_ptr, ptr_type, "") catch return error.CompilationFailed;
                    }
                    callback_val = fn_ptr;
                }

                try self.emitRcWrapperCall(
                    "roc_builtins_box_free_value",
                    &.{ ptr_type, .i32, ptr_type, ptr_type },
                    &.{
                        value_ptr,
                        builder.intValue(.i32, box_plan.elem_alignment) catch return error.OutOfMemory,
                        callback_val,
                        roc_ops,
                    },
                );
            },
            .struct_ => |struct_plan| {
                const field_count = resolver.structFieldCount(struct_plan);
                var field_index: u32 = 0;
                while (field_index < field_count) : (field_index += 1) {
                    const field_plan = resolver.structFieldPlan(struct_plan, field_index) orelse continue;
                    const field_ptr = if (field_plan.offset == 0)
                        value_ptr
                    else
                        wip.gep(
                            .inbounds,
                            .i8,
                            value_ptr,
                            &.{builder.intValue(.i32, field_plan.offset) catch return error.OutOfMemory},
                            "",
                        ) catch return error.CompilationFailed;
                    try self.emitRcHelperCall(
                        field_plan.child,
                        field_ptr,
                        count,
                        roc_ops,
                        try self.abiSlotStorageForLayout(field_plan.child.layout_idx),
                    );
                }
            },
            .tag_union => |tag_plan| {
                const variant_count = resolver.tagUnionVariantCount(tag_plan);
                if (variant_count == 0) return;

                switch (try self.tagUnionValueMode(helper_key.layout_idx)) {
                    .scalar_discriminant => return,
                    .transparent_payload => {
                        if (resolver.tagUnionVariantPlan(tag_plan, 0)) |child_key| {
                            try self.emitRcHelperCall(
                                child_key,
                                value_ptr,
                                count,
                                roc_ops,
                                try self.abiSlotStorageForLayout(child_key.layout_idx),
                            );
                        }
                    },
                    .indirect_pointer => {
                        const root_ptr = switch (storage) {
                            .materialized_value => value_ptr,
                            .field_slot => wip.load(.normal, ptr_type, value_ptr, self.pointerAlignment(), "") catch return error.CompilationFailed,
                        };

                        if (variant_count == 1) {
                            if (resolver.tagUnionVariantPlan(tag_plan, 0)) |child_key| {
                                try self.emitRcHelperCall(
                                    child_key,
                                    root_ptr,
                                    count,
                                    roc_ops,
                                    try self.abiSlotStorageForLayout(child_key.layout_idx),
                                );
                            }
                            return;
                        }

                        const tu_data = ls.getTagUnionData(tag_plan.tag_union_idx);
                        const disc_type = discriminantIntType(tu_data.discriminant_size);
                        const disc_ptr = wip.gep(
                            .inbounds,
                            .i8,
                            root_ptr,
                            &.{builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory},
                            "",
                        ) catch return error.CompilationFailed;
                        const discriminant = wip.load(
                            .normal,
                            disc_type,
                            disc_ptr,
                            LlvmBuilder.Alignment.fromByteUnits(@max(@as(u64, tu_data.discriminant_size), 1)),
                            "",
                        ) catch return error.CompilationFailed;

                        const exit_block = wip.block(variant_count, "rc_done") catch return error.OutOfMemory;

                        var variant_index: u32 = 0;
                        while (variant_index < variant_count) : (variant_index += 1) {
                            const is_last = variant_index == variant_count - 1;
                            const child_key = resolver.tagUnionVariantPlan(tag_plan, variant_index);

                            if (!is_last) {
                                const then_block = wip.block(1, "rc_variant") catch return error.OutOfMemory;
                                const else_block = wip.block(1, "rc_next") catch return error.OutOfMemory;
                                const variant_val = builder.intValue(disc_type, variant_index) catch return error.OutOfMemory;
                                const matches = wip.icmp(.eq, discriminant, variant_val, "") catch return error.OutOfMemory;
                                _ = wip.brCond(matches, then_block, else_block, .none) catch return error.OutOfMemory;

                                wip.cursor = .{ .block = then_block };
                                if (child_key) |child| {
                                    try self.emitRcHelperCall(
                                        child,
                                        root_ptr,
                                        count,
                                        roc_ops,
                                        try self.abiSlotStorageForLayout(child.layout_idx),
                                    );
                                }
                                _ = wip.br(exit_block) catch return error.CompilationFailed;

                                wip.cursor = .{ .block = else_block };
                            } else {
                                if (child_key) |child| {
                                    try self.emitRcHelperCall(
                                        child,
                                        root_ptr,
                                        count,
                                        roc_ops,
                                        try self.abiSlotStorageForLayout(child.layout_idx),
                                    );
                                }
                                _ = wip.br(exit_block) catch return error.CompilationFailed;
                            }
                        }

                        wip.cursor = .{ .block = exit_block };
                    },
                }
            },
            .closure => |child_key| {
                try self.emitRcHelperCall(
                    child_key,
                    value_ptr,
                    count,
                    roc_ops,
                    try self.abiSlotStorageForLayout(child_key.layout_idx),
                );
            },
        }
    }

    fn abiSlotStorageForLayout(
        self: *const MonoLlvmCodeGen,
        layout_idx: layout.Idx,
    ) Error!PointerValueStorage {
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(layout_idx);

        return switch (stored_layout.tag) {
            .tag_union => switch (try self.tagUnionValueMode(layout_idx)) {
                .scalar_discriminant, .transparent_payload => .materialized_value,
                .indirect_pointer => .field_slot,
            },
            .scalar,
            .zst,
            .closure,
            .struct_,
            .list,
            .list_of_zst,
            .box,
            .box_of_zst,
            => .materialized_value,
        };
    }

    fn generateRcOp(
        self: *MonoLlvmCodeGen,
        op: layout.RcOp,
        expr_id: LirExprId,
        layout_idx: layout.Idx,
        count: u16,
    ) Error!void {
        const resolver = layout.RcHelperResolver.init(self.layout_store orelse return error.CompilationFailed);
        const helper_key = layout.RcHelperKey{
            .op = op,
            .layout_idx = layout_idx,
        };
        if (resolver.plan(helper_key) == .noop) {
            _ = try self.generateExpr(expr_id);
            return;
        }

        const value_ptr = try self.materializeAsPtr(expr_id, try self.materializedLayoutSize(layout_idx));
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const count_val = switch (op) {
            .incref => try self.emitPtrSizedInt(count),
            .decref, .free => null,
        };
        try self.emitRcHelperCall(
            helper_key,
            value_ptr,
            count_val,
            roc_ops,
            try self.abiSlotStorageForLayout(layout_idx),
        );
    }

    /// Generate LLVM bitcode for a Mono IR expression
    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        expr_id: LirExprId,
        result_layout: layout.Idx,
    ) Error!BitcodeResult {
        var builder = try self.createBuilder("roc_mono_eval");
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        // Create the eval function: void roc_eval(<type>* out_ptr, roc_ops* ops_ptr)
        const ptr_type = builder.ptrType(.default) catch return error.OutOfMemory;
        const eval_fn_type = builder.fnType(.void, &.{ ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
        const eval_name = try self.exportedFunctionName(&builder, "roc_eval");
        const eval_fn = builder.addFunction(eval_fn_type, eval_name, .default) catch return error.OutOfMemory;
        eval_fn.setLinkage(.external, &builder);
        self.configureExportCallConv(eval_fn, &builder);

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

        const outer_fn_state = self.swapInFreshFunctionState();
        defer self.restoreFunctionState(outer_fn_state);

        // Compile all procedures now that the builder is available.
        // Must happen before generateExpr so that call sites can find procs.
        const procs = self.store.getProcSpecs();
        if (procs.len > 0) {
            try self.compileAllProcSpecs(procs);
        }

        // Generate LLVM IR for the expression
        const value = try self.generateExpr(expr_id);

        // Store the result to the output pointer.
        // Some generators (e.g., string literals) write directly to out_ptr and
        // return .none as a sentinel — skip the storage step for those.
        if (value != .none) {
            try self.storeValueToPointer(out_ptr, value, result_layout, true);
        }

        _ = wip.retVoid() catch return error.CompilationFailed;

        wip.finish() catch return error.CompilationFailed;

        // Serialize to bitcode
        const bitcode = try self.serializeBuilderToBitcode(&builder);

        return BitcodeResult{
            .bitcode = bitcode,
            .result_layout = result_layout,
            .allocator = self.allocator,
        };
    }

    pub fn generateEntrypointModule(
        self: *MonoLlvmCodeGen,
        module_name: []const u8,
        entrypoints: anytype,
    ) Error!ModuleBitcodeResult {
        self.reset();

        var builder = try self.createBuilder(module_name);
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        const procs = self.store.getProcSpecs();
        if (procs.len > 0) {
            try self.compileAllProcSpecs(procs);
        }

        for (entrypoints) |entrypoint| {
            try self.generateEntrypointWrapper(
                entrypoint.symbol_name,
                entrypoint.proc,
                entrypoint.arg_layouts,
                entrypoint.ret_layout,
            );
        }

        return ModuleBitcodeResult{
            .bitcode = try self.serializeBuilderToBitcode(&builder),
            .allocator = self.allocator,
        };
    }

    /// Compile all procedures as LLVM functions.
    /// Mirrors dev backend's compileAllProcs: creates each proc as a callable
    /// LLVM function and registers it in proc_registry before compiling the body,
    /// so recursive calls within the body can find the function.
    pub fn compileAllProcSpecs(self: *MonoLlvmCodeGen, procs: []const LirProcSpec) Error!void {
        for (procs, 0..) |proc, i| {
            try self.declareProcSpec(@enumFromInt(i), proc);
        }
        for (procs, 0..) |proc, i| {
            try self.compileProcBody(@enumFromInt(i), proc);
        }
    }

    fn declareProcSpec(self: *MonoLlvmCodeGen, proc_id: lir.LIR.LirProcSpecId, proc: LirProcSpec) Error!void {
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
        const key: u64 = @intFromEnum(proc_id);
        var name_buf: [64]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_proc_{d}", .{key}) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;

        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        self.proc_registry.put(key, func) catch return error.OutOfMemory;
    }

    fn compileProcBody(self: *MonoLlvmCodeGen, proc_id: lir.LIR.LirProcSpecId, proc: LirProcSpec) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const key: u64 = @intFromEnum(proc_id);
        const func = self.proc_registry.get(key) orelse return error.CompilationFailed;

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

        const outer_fn_state = self.swapInFreshFunctionState();
        defer self.restoreFunctionState(outer_fn_state);

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

    pub fn generateEntrypointWrapper(
        self: *MonoLlvmCodeGen,
        symbol_name: []const u8,
        entry_proc: lir.LIR.LirProcSpecId,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;

        _ = self.store.getProcSpec(entry_proc);
        const proc_key: u64 = @intFromEnum(entry_proc);
        const proc_fn = self.proc_registry.get(proc_key) orelse return error.CompilationFailed;

        const ptr_type = builder.ptrType(.default) catch return error.OutOfMemory;
        const wrapper_type = builder.fnType(.void, &.{ ptr_type, ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
        const wrapper_name = try self.exportedFunctionName(builder, symbol_name);
        const wrapper_fn = builder.addFunction(wrapper_type, wrapper_name, .default) catch return error.OutOfMemory;
        wrapper_fn.setLinkage(.external, builder);
        self.configureExportCallConv(wrapper_fn, builder);

        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        const outer_result_layout = self.result_layout;
        defer self.result_layout = outer_result_layout;

        const outer_fn_state = self.swapInFreshFunctionState();
        defer self.restoreFunctionState(outer_fn_state);

        var wrapper_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = wrapper_fn,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer wrapper_wip.deinit();

        self.wip = &wrapper_wip;

        const entry_block = wrapper_wip.block(0, "entry") catch return error.OutOfMemory;
        wrapper_wip.cursor = .{ .block = entry_block };

        const roc_ops = wrapper_wip.arg(0);
        const ret_ptr = wrapper_wip.arg(1);
        const args_ptr = wrapper_wip.arg(2);

        self.roc_ops_arg = roc_ops;
        self.out_ptr = ret_ptr;
        self.fn_out_ptr = ret_ptr;
        self.result_layout = ret_layout;

        const compiled_type = proc_fn.typeOf(builder);
        const expected_params = compiled_type.functionParameters(builder);
        const expected_params_copy = try self.allocator.dupe(LlvmBuilder.Type, expected_params);
        defer self.allocator.free(expected_params_copy);

        var arg_values: std.ArrayList(LlvmBuilder.Value) = .empty;
        defer arg_values.deinit(self.allocator);
        try arg_values.ensureTotalCapacity(self.allocator, arg_layouts.len + 1);

        const arg_offsets = try self.allocator.alloc(u32, arg_layouts.len);
        defer self.allocator.free(arg_offsets);
        try self.computeEntrypointArgOffsets(arg_layouts, arg_offsets);

        for (arg_layouts, 0..) |arg_layout, i| {
            const loaded = try self.loadEntrypointArg(args_ptr, arg_offsets[i], arg_layout);
            const coerced = try self.coerceValueToType(loaded, expected_params_copy[i], arg_layout);
            arg_values.appendAssumeCapacity(coerced);
        }

        const roc_ops_idx = arg_values.items.len;
        const coerced_roc_ops = try self.coerceValueToType(roc_ops, expected_params_copy[roc_ops_idx], null);
        arg_values.appendAssumeCapacity(coerced_roc_ops);

        const proc_value = proc_fn.toValue(builder);
        const call_result = wrapper_wip.call(.normal, .ccc, .none, compiled_type, proc_value, arg_values.items, "") catch return error.CompilationFailed;

        if (try self.layoutByteSize(ret_layout) > 0) {
            try self.storeValueToPointer(ret_ptr, call_result, ret_layout, false);
        }

        _ = wrapper_wip.retVoid() catch return error.CompilationFailed;
        wrapper_wip.finish() catch return error.CompilationFailed;
    }

    fn createBuilder(self: *MonoLlvmCodeGen, name: []const u8) Error!LlvmBuilder {
        return LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = name,
            .target = &self.target,
            .triple = self.triple,
        }) catch return error.OutOfMemory;
    }

    fn exportedFunctionName(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, name: []const u8) Error!LlvmBuilder.StrtabString {
        if (self.target.os.tag != .macos) {
            return builder.strtabString(name) catch return error.OutOfMemory;
        }

        const exact_name = try std.fmt.allocPrint(self.allocator, "\x01_{s}", .{name});
        defer self.allocator.free(exact_name);
        return builder.strtabString(exact_name) catch return error.OutOfMemory;
    }

    fn configureExportCallConv(self: *MonoLlvmCodeGen, func: LlvmBuilder.Function.Index, builder: *LlvmBuilder) void {
        if (self.target.cpu.arch != .x86_64) return;

        if (self.target.os.tag == .windows) {
            func.setCallConv(.win64cc, builder);
        } else {
            func.setCallConv(.x86_64_sysvcc, builder);
        }
    }

    fn serializeBuilderToBitcode(self: *MonoLlvmCodeGen, builder: *LlvmBuilder) Error![]const u32 {
        const producer = LlvmBuilder.Producer{
            .name = "Roc Mono LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

        if (std.process.getEnvVarOwned(self.allocator, "ROC_LLVM_KEEP_IR")) |keep_path| {
            defer self.allocator.free(keep_path);
            builder.printToFilePath(std.fs.cwd(), keep_path) catch return error.CompilationFailed;
        } else |_| {}

        return builder.toBitcode(self.allocator, producer) catch return error.CompilationFailed;
    }

    const EntrypointArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    fn computeEntrypointArgOffsets(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, offsets: []u32) Error!void {
        std.debug.assert(arg_layouts.len == offsets.len);

        var ordered = try self.allocator.alloc(EntrypointArgOrder, arg_layouts.len);
        defer self.allocator.free(ordered);

        for (arg_layouts, 0..) |arg_layout, i| {
            const ls = self.layout_store orelse return error.CompilationFailed;
            const size_align = ls.layoutSizeAlign(ls.getLayout(arg_layout));
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(@max(size_align.alignment.toByteUnits(), 1)),
                .size = size_align.size,
            };
        }

        const SortCtx = struct {
            fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                if (lhs.alignment != rhs.alignment) {
                    return lhs.alignment > rhs.alignment;
                }
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

        var current_offset: u32 = 0;
        for (ordered) |arg| {
            current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
            offsets[arg.index] = current_offset;
            current_offset += arg.size;
        }
    }

    fn loadEntrypointArg(
        self: *MonoLlvmCodeGen,
        args_ptr: LlvmBuilder.Value,
        offset: u32,
        arg_layout: layout.Idx,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;

        const arg_ptr = if (offset == 0)
            args_ptr
        else blk: {
            const offset_val = builder.intValue(.i32, offset) catch return error.OutOfMemory;
            break :blk wip.gep(.inbounds, .i8, args_ptr, &.{offset_val}, "") catch return error.CompilationFailed;
        };

        if (arg_layout == .zst) {
            return (builder.intConst(.i8, 0) catch return error.OutOfMemory).toValue();
        }

        return self.loadFieldValueFromPtr(arg_ptr, arg_layout);
    }

    fn layoutByteSize(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!u32 {
        const ls = self.layout_store orelse return error.CompilationFailed;
        return ls.layoutSizeAlign(ls.getLayout(layout_idx)).size;
    }

    fn storeValueToPointer(
        self: *MonoLlvmCodeGen,
        out_ptr: LlvmBuilder.Value,
        value: LlvmBuilder.Value,
        result_layout: layout.Idx,
        canonical_scalars: bool,
    ) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

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
            const final_type: LlvmBuilder.Type = if (canonical_scalars)
                switch (result_layout) {
                    .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
                    .i128, .u128, .dec => .i128,
                    .f32 => .float,
                    .f64 => .double,
                    else => unreachable,
                }
            else
                try self.layoutToLlvmTypeWithOptions(result_layout, true);

            const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (result_layout) {
                .i8, .i16, .i32, .i64, .i128 => .signed,
                .bool, .u8, .u16, .u32, .u64, .u128 => .unsigned,
                .f32, .f64, .dec => .unneeded,
                else => .unneeded,
            };

            const actual_value_type = value.typeOfWip(wip);
            const store_value = if (actual_value_type == final_type)
                value
            else conv: {
                const conv_sign: LlvmBuilder.Constant.Cast.Signedness = if (signedness == .unneeded and isIntType(actual_value_type))
                    .signed
                else
                    signedness;
                break :conv wip.conv(conv_sign, value, final_type, "") catch return error.CompilationFailed;
            };

            const alignment = if (canonical_scalars)
                LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
                    .i64 => 8,
                    .i128 => 16,
                    .float => 4,
                    .double => 8,
                    else => 0,
                })
            else
                self.alignmentForLayout(result_layout);
            _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
            return;
        }

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
            const builder = self.builder orelse return error.CompilationFailed;
            const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
            for (0..3) |fi| {
                const field_val = wip.extractValue(value, &.{@as(u32, @intCast(fi))}, "") catch return error.CompilationFailed;
                const field_ptr = if (fi == 0) out_ptr else blk: {
                    const off = builder.intValue(.i32, @as(u32, @intCast(fi * 8))) catch return error.OutOfMemory;
                    break :blk wip.gep(.inbounds, .i8, out_ptr, &.{off}, "") catch return error.CompilationFailed;
                };
                _ = wip.store(.@"volatile", field_val, field_ptr, alignment) catch return error.CompilationFailed;
            }
            return;
        }

        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        _ = wip.store(.normal, value, out_ptr, alignment) catch return error.CompilationFailed;
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
                    .tag_union => switch (try self.tagUnionValueMode(result_layout)) {
                        .scalar_discriminant => {
                            const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                            break :blk discriminantIntTypeOrZst(tu_data.discriminant_size);
                        },
                        .transparent_payload => {
                            const payload_layout = try self.tagUnionTransparentPayloadLayout(result_layout);
                            break :blk try self.layoutToLlvmTypeWithOptions(payload_layout, bool_in_memory);
                        },
                        .indirect_pointer => break :blk builder.ptrType(.default) catch return error.CompilationFailed,
                    },
                    .box => break :blk builder.ptrType(.default) catch return error.CompilationFailed,
                    .scalar,
                    .zst,
                    .closure,
                    .box_of_zst,
                    => break :blk .i64,
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
        if (self.currentBlockHasTerminator()) return;

        const stmt = self.store.getCFStmt(stmt_id);

        switch (stmt) {
            .let_stmt => |let_s| {
                const val = try self.generateExpr(let_s.value);
                if (self.currentBlockHasTerminator()) return;
                try self.bindPattern(let_s.pattern, val);
                try self.generateStmt(let_s.next);
            },
            .ret => |r| {
                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;
                const val = try self.generateExpr(r.value);
                if (val == .none) return error.CompilationFailed;

                const fn_ret_type = wip.function.typeOf(builder).functionReturn(builder);
                const coerced = try self.coerceValueToType(val, fn_ret_type, self.getExprResultLayout(r.value));
                _ = wip.ret(coerced) catch return error.CompilationFailed;
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
                if (self.currentBlockHasTerminator()) return;
                try self.generateStmt(e.next);
            },
            .match_stmt => |ms| {
                // Pattern match statement (when in tail position of a proc)
                // Similar to match_expr but each branch is a statement, not an expression.
                const wip = self.wip orelse return error.CompilationFailed;
                const scrutinee = try self.generateExpr(ms.value);
                if (self.currentBlockHasTerminator()) return;
                const branches = self.store.getCFMatchBranches(ms.branches);
                std.debug.assert(branches.len != 0);

                for (branches, 0..) |branch, i| {
                    const pattern = self.store.getPattern(branch.pattern);
                    const is_last = (i == branches.len - 1);

                    switch (pattern) {
                        .wildcard => {
                            var branch_scope = try self.beginScope();
                            defer branch_scope.deinit();
                            try self.generateStmt(branch.body);
                            try self.endScope(&branch_scope);
                            break;
                        },
                        .bind => {
                            const bind = pattern.bind;
                            const symbol_key: u64 = @bitCast(bind.symbol);
                            var branch_scope = try self.beginScope();
                            defer branch_scope.deinit();
                            self.symbol_values.put(symbol_key, scrutinee) catch return error.OutOfMemory;
                            try self.generateStmt(branch.body);
                            try self.endScope(&branch_scope);
                            break;
                        },
                        .int_literal,
                        .tag,
                        .struct_,
                        .list,
                        .as_pattern,
                        => {
                            const cmp = try self.emitPatternMatchCondition(branch.pattern, scrutinee);
                            const then_block = wip.block(1, "match_then") catch return error.OutOfMemory;
                            const else_block = wip.block(1, if (is_last) "match_nomatch" else "match_else") catch return error.OutOfMemory;
                            _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.CompilationFailed;

                            wip.cursor = .{ .block = then_block };
                            var branch_scope = try self.beginScope();
                            defer branch_scope.deinit();
                            try self.bindPattern(branch.pattern, scrutinee);
                            try self.generateStmt(branch.body);
                            try self.endScope(&branch_scope);

                            wip.cursor = .{ .block = else_block };
                            if (is_last) {
                                _ = wip.@"unreachable"() catch return error.CompilationFailed;
                            }
                        },
                        .float_literal, .str_literal => unreachable,
                    }
                }
            },
        }
    }

    fn generateSwitchStmt(self: *MonoLlvmCodeGen, sw: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const cond_val = try self.generateExpr(sw.cond);
        if (self.currentBlockHasTerminator()) return;
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

            // Function calls
            .proc_call => |call| self.generateCall(call),

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

            .incref => |inc| {
                try self.generateRcOp(.incref, inc.value, inc.layout_idx, inc.count);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .decref => |dec_rc| {
                try self.generateRcOp(.decref, dec_rc.value, dec_rc.layout_idx, 1);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .free => |f| {
                try self.generateRcOp(.free, f.value, f.layout_idx, 1);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },

            // Loops
            .while_loop => |wl| self.generateWhileLoop(wl),
            .for_loop => |fl| self.generateForLoop(fl),
            .break_expr => self.generateBreakExpr(),

            // String concatenation (fold-left over a span of string expressions)
            .str_concat => |exprs| self.generateStrConcatExpr(exprs),

            .hosted_call => |hc| self.generateHostedCall(hc),

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

    fn generateHostedCall(self: *MonoLlvmCodeGen, hc: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.OutOfMemory;
        const ptr_size: u32 = @intCast(@divExact(self.target.ptrBitWidth(), 8));
        const explicit_args = self.store.getExprSpan(hc.args);

        const ret_size = try self.layoutByteSize(hc.ret_layout);
        const ret_slot_size: u32 = if (ret_size > 0) ret_size else 1;
        const ret_slot_type = builder.arrayType(ret_slot_size, .i8) catch return error.OutOfMemory;
        const ret_alignment = if (ret_size > 0)
            self.alignmentForLayout(hc.ret_layout)
        else
            LlvmBuilder.Alignment.fromByteUnits(1);
        const ret_ptr = wip.alloca(.normal, ret_slot_type, .none, ret_alignment, .default, "hosted_ret") catch return error.CompilationFailed;

        const arg_offsets = try self.allocator.alloc(u32, explicit_args.len);
        defer self.allocator.free(arg_offsets);

        var args_size: u32 = 0;
        var args_alignment_bytes: u32 = 1;
        if (explicit_args.len > 0) {
            const arg_layouts = try self.allocator.alloc(layout.Idx, explicit_args.len);
            defer self.allocator.free(arg_layouts);

            for (explicit_args, 0..) |arg_id, i| {
                const arg_layout = self.getExprResultLayout(arg_id) orelse return error.CompilationFailed;
                arg_layouts[i] = arg_layout;
                const arg_alignment = self.alignmentForLayout(arg_layout).toByteUnits() orelse 1;
                args_alignment_bytes = @intCast(@max(args_alignment_bytes, @as(u32, @intCast(arg_alignment))));
            }

            try self.computeEntrypointArgOffsets(arg_layouts, arg_offsets);

            for (arg_layouts, 0..) |arg_layout, i| {
                args_size = @max(args_size, arg_offsets[i] + try self.layoutByteSize(arg_layout));
            }
        }

        const args_slot_size: u32 = if (args_size > 0) args_size else ptr_size;
        const args_slot_type = builder.arrayType(args_slot_size, .i8) catch return error.OutOfMemory;
        const args_alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(args_alignment_bytes, 1)));
        const args_ptr = wip.alloca(.normal, args_slot_type, .none, args_alignment, .default, "hosted_args") catch return error.CompilationFailed;

        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const args_size_val = builder.intValue(.i32, args_slot_size) catch return error.OutOfMemory;
        _ = wip.callMemSet(args_ptr, args_alignment, zero_byte, args_size_val, .normal, false) catch return error.CompilationFailed;

        for (explicit_args, 0..) |arg_id, i| {
            const arg_layout = self.getExprResultLayout(arg_id) orelse return error.CompilationFailed;
            const arg_value = (try self.generateExprAsValue(arg_id)).value;
            if (try self.layoutByteSize(arg_layout) == 0) continue;

            const arg_ptr = if (arg_offsets[i] == 0)
                args_ptr
            else blk: {
                const offset_val = builder.intValue(.i32, arg_offsets[i]) catch return error.OutOfMemory;
                break :blk wip.gep(.inbounds, .i8, args_ptr, &.{offset_val}, "") catch return error.CompilationFailed;
            };
            try self.storeValueToPointer(arg_ptr, arg_value, arg_layout, false);
        }

        const hosted_fns_offset = ptr_size * 7;
        const hosted_fns_fns_offset = hosted_fns_offset + std.mem.alignForward(u32, @sizeOf(u32), ptr_size);
        const hosted_fns_ptr = if (hosted_fns_fns_offset == 0)
            roc_ops
        else blk: {
            const offset_val = builder.intValue(.i32, hosted_fns_fns_offset) catch return error.OutOfMemory;
            break :blk wip.gep(.inbounds, .i8, roc_ops, &.{offset_val}, "") catch return error.CompilationFailed;
        };
        const fns_ptr = wip.load(.normal, ptr_type, hosted_fns_ptr, LlvmBuilder.Alignment.fromByteUnits(ptr_size), "") catch return error.CompilationFailed;

        const fn_offset = hc.index * ptr_size;
        const fn_ptr_ptr = if (fn_offset == 0)
            fns_ptr
        else blk: {
            const offset_val = builder.intValue(.i32, fn_offset) catch return error.OutOfMemory;
            break :blk wip.gep(.inbounds, .i8, fns_ptr, &.{offset_val}, "") catch return error.CompilationFailed;
        };
        const fn_ptr = wip.load(.normal, ptr_type, fn_ptr_ptr, LlvmBuilder.Alignment.fromByteUnits(ptr_size), "") catch return error.CompilationFailed;

        const hosted_fn_type = builder.fnType(.void, &.{ ptr_type, ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
        _ = wip.call(.normal, .ccc, .none, hosted_fn_type, fn_ptr, &.{ roc_ops, ret_ptr, args_ptr }, "") catch return error.CompilationFailed;

        if (ret_size == 0) {
            return builder.intValue(.i8, 0) catch return error.OutOfMemory;
        }

        return self.loadFieldValueFromPtr(ret_ptr, hc.ret_layout);
    }

    fn generateLookup(self: *MonoLlvmCodeGen, symbol: Symbol, _: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u64 = @bitCast(symbol);

        if (self.cell_allocas.get(symbol_key)) |cell_alloca| {
            return self.loadStoredAlloca(cell_alloca);
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

        return error.CompilationFailed; // Symbol must exist in symbol_values or as a top-level def
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

    fn compareTagUnionValues(self: *MonoLlvmCodeGen, lhs_value: LlvmBuilder.Value, rhs_value: LlvmBuilder.Value, union_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(union_layout_idx);

        return switch (stored_layout.tag) {
            .tag_union => blk: {
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                const lhs_disc = try self.loadTagDiscriminant(lhs_value, union_layout_idx);
                const rhs_disc = try self.loadTagDiscriminant(rhs_value, union_layout_idx);
                const disc_eq = wip.icmp(.eq, lhs_disc, rhs_disc, "") catch return error.OutOfMemory;
                const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;

                if (variants.len == 0) {
                    break :blk disc_eq;
                }

                var all_zst = true;
                for (0..variants.len) |variant_idx| {
                    if (variants.get(@intCast(variant_idx)).payload_layout != .zst) {
                        all_zst = false;
                        break;
                    }
                }
                if (all_zst) {
                    break :blk disc_eq;
                }

                const disc_mismatch_block = wip.block(1, "tag_eq_disc_mismatch") catch return error.OutOfMemory;
                const exit_block = wip.block(@intCast(variants.len + 1), "tag_eq_exit") catch return error.OutOfMemory;
                const first_check_block = wip.block(1, "tag_eq_check_0") catch return error.OutOfMemory;

                var result_vals = std.ArrayList(LlvmBuilder.Value){};
                defer result_vals.deinit(self.allocator);
                var result_blocks = std.ArrayList(LlvmBuilder.Function.Block.Index){};
                defer result_blocks.deinit(self.allocator);

                _ = wip.brCond(disc_eq, first_check_block, disc_mismatch_block, .none) catch return error.CompilationFailed;

                wip.cursor = .{ .block = disc_mismatch_block };
                _ = wip.br(exit_block) catch return error.CompilationFailed;
                result_vals.append(self.allocator, false_val) catch return error.OutOfMemory;
                result_blocks.append(self.allocator, disc_mismatch_block) catch return error.OutOfMemory;

                var next_check_block = first_check_block;
                for (0..variants.len) |variant_idx| {
                    const variant = variants.get(@intCast(variant_idx));
                    const variant_block = wip.block(1, "tag_eq_variant") catch return error.OutOfMemory;
                    const is_last = variant_idx + 1 == variants.len;
                    const current_check_block = next_check_block;

                    wip.cursor = .{ .block = current_check_block };
                    if (is_last) {
                        _ = wip.br(variant_block) catch return error.CompilationFailed;
                    } else {
                        next_check_block = wip.block(1, "tag_eq_next_check") catch return error.OutOfMemory;
                        const disc_value = builder.intValue(lhs_disc.typeOfWip(wip), @as(u64, @intCast(variant_idx))) catch return error.OutOfMemory;
                        const disc_match = wip.icmp(.eq, lhs_disc, disc_value, "") catch return error.OutOfMemory;
                        _ = wip.brCond(disc_match, variant_block, next_check_block, .none) catch return error.CompilationFailed;
                    }

                    wip.cursor = .{ .block = variant_block };
                    const variant_result = if (variant.payload_layout == .zst)
                        true_val
                    else payload_blk: {
                        const lhs_payload = try self.getTagPayloadInfo(lhs_value, union_layout_idx, variant_idx);
                        const rhs_payload = try self.getTagPayloadInfo(rhs_value, union_layout_idx, variant_idx);
                        break :payload_blk try self.compareFieldPtrsByLayout(lhs_payload.root_ptr, rhs_payload.root_ptr, variant.payload_layout);
                    };
                    const variant_result_block = wip.cursor.block;
                    _ = wip.br(exit_block) catch return error.CompilationFailed;
                    result_vals.append(self.allocator, variant_result) catch return error.OutOfMemory;
                    result_blocks.append(self.allocator, variant_result_block) catch return error.OutOfMemory;
                }

                wip.cursor = .{ .block = exit_block };
                const result_phi = wip.phi(.i1, "tag_eq_result") catch return error.CompilationFailed;
                result_phi.finish(result_vals.items, result_blocks.items, wip);
                break :blk result_phi.toValue();
            },
            .scalar, .zst, .closure, .struct_, .list, .list_of_zst, .box, .box_of_zst => error.CompilationFailed,
        };
    }

    fn comparePtrsByLayout(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        switch (layout_idx) {
            .zst => return builder.intValue(.i1, 1) catch return error.OutOfMemory,
            .bool => {
                const lhs = try self.loadValueFromPtr(lhs_ptr, .bool);
                const rhs = try self.loadValueFromPtr(rhs_ptr, .bool);
                return wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec => {
                const lhs = try self.loadValueFromPtr(lhs_ptr, layout_idx);
                const rhs = try self.loadValueFromPtr(rhs_ptr, layout_idx);
                return wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .f32, .f64 => {
                const lhs = try self.loadValueFromPtr(lhs_ptr, layout_idx);
                const rhs = try self.loadValueFromPtr(rhs_ptr, layout_idx);
                return wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .str => {
                const lhs = try self.loadValueFromPtr(lhs_ptr, .str);
                const rhs = try self.loadValueFromPtr(rhs_ptr, .str);
                return self.callStrStr2BoolFromValues(lhs, rhs, "roc_builtins_str_equal");
            },
            else => {},
        }

        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(layout_idx);
        return switch (stored_layout.tag) {
            .scalar => blk: {
                const lhs = try self.loadValueFromPtr(lhs_ptr, layout_idx);
                const rhs = try self.loadValueFromPtr(rhs_ptr, layout_idx);
                break :blk wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .zst => builder.intValue(.i1, 1) catch return error.OutOfMemory,
            .closure => error.CompilationFailed,
            .struct_ => self.compareStructPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .list => self.compareListPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .list_of_zst => self.compareListPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .tag_union => switch (try self.tagUnionValueMode(layout_idx)) {
                .scalar_discriminant => blk: {
                    const lhs = try self.loadValueFromPtr(lhs_ptr, layout_idx);
                    const rhs = try self.loadValueFromPtr(rhs_ptr, layout_idx);
                    break :blk try self.compareTagUnionValues(lhs, rhs, layout_idx);
                },
                .transparent_payload => blk: {
                    const payload_layout = try self.tagUnionTransparentPayloadLayout(layout_idx);
                    break :blk try self.comparePtrsByLayout(lhs_ptr, rhs_ptr, payload_layout);
                },
                .indirect_pointer => self.compareTagUnionValues(lhs_ptr, rhs_ptr, layout_idx),
            },
            .box => self.compareBoxPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .box_of_zst => builder.intValue(.i1, 1) catch return error.OutOfMemory,
        };
    }

    fn compareFieldPtrsByLayout(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        switch (layout_idx) {
            .zst => return builder.intValue(.i1, 1) catch return error.OutOfMemory,
            .bool => {
                const lhs = try self.loadFieldValueFromPtr(lhs_ptr, .bool);
                const rhs = try self.loadFieldValueFromPtr(rhs_ptr, .bool);
                return wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec => {
                const lhs = try self.loadFieldValueFromPtr(lhs_ptr, layout_idx);
                const rhs = try self.loadFieldValueFromPtr(rhs_ptr, layout_idx);
                return wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .f32, .f64 => {
                const lhs = try self.loadFieldValueFromPtr(lhs_ptr, layout_idx);
                const rhs = try self.loadFieldValueFromPtr(rhs_ptr, layout_idx);
                return wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .str => {
                const lhs = try self.loadFieldValueFromPtr(lhs_ptr, .str);
                const rhs = try self.loadFieldValueFromPtr(rhs_ptr, .str);
                return self.callStrStr2BoolFromValues(lhs, rhs, "roc_builtins_str_equal");
            },
            else => {},
        }

        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(layout_idx);
        return switch (stored_layout.tag) {
            .scalar => blk: {
                const lhs = try self.loadFieldValueFromPtr(lhs_ptr, layout_idx);
                const rhs = try self.loadFieldValueFromPtr(rhs_ptr, layout_idx);
                break :blk wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .zst => builder.intValue(.i1, 1) catch return error.OutOfMemory,
            .closure => error.CompilationFailed,
            .struct_ => self.compareStructPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .list => self.compareListPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .list_of_zst => self.compareListPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .tag_union => switch (try self.tagUnionValueMode(layout_idx)) {
                .scalar_discriminant => blk: {
                    const lhs = try self.loadFieldValueFromPtr(lhs_ptr, layout_idx);
                    const rhs = try self.loadFieldValueFromPtr(rhs_ptr, layout_idx);
                    break :blk try self.compareTagUnionValues(lhs, rhs, layout_idx);
                },
                .transparent_payload => blk: {
                    const payload_layout = try self.tagUnionTransparentPayloadLayout(layout_idx);
                    break :blk try self.compareFieldPtrsByLayout(lhs_ptr, rhs_ptr, payload_layout);
                },
                .indirect_pointer => blk: {
                    const lhs = try self.loadFieldValueFromPtr(lhs_ptr, layout_idx);
                    const rhs = try self.loadFieldValueFromPtr(rhs_ptr, layout_idx);
                    break :blk try self.compareTagUnionValues(lhs, rhs, layout_idx);
                },
            },
            .box => self.compareBoxPtrs(lhs_ptr, rhs_ptr, layout_idx),
            .box_of_zst => builder.intValue(.i1, 1) catch return error.OutOfMemory,
        };
    }

    fn compareStructPtrs(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, struct_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(struct_layout_idx);

        return switch (stored_layout.tag) {
            .struct_ => blk: {
                const struct_idx = stored_layout.data.struct_.idx;
                const struct_data = ls.getStructData(struct_idx);
                var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;

                for (0..struct_data.getFields().count) |field_i| {
                    const field_size = ls.getStructFieldSize(struct_idx, @intCast(field_i));
                    if (field_size == 0) continue;

                    const field_layout_idx = ls.getStructFieldLayout(struct_idx, @intCast(field_i));
                    const field_offset = ls.getStructFieldOffset(struct_idx, @intCast(field_i));
                    const field_offset_val = builder.intValue(.i32, field_offset) catch return error.OutOfMemory;
                    const lhs_field_ptr = wip.gep(.inbounds, .i8, lhs_ptr, &.{field_offset_val}, "") catch return error.CompilationFailed;
                    const rhs_field_ptr = wip.gep(.inbounds, .i8, rhs_ptr, &.{field_offset_val}, "") catch return error.CompilationFailed;
                    const field_eq = try self.compareFieldPtrsByLayout(lhs_field_ptr, rhs_field_ptr, field_layout_idx);
                    result = wip.bin(.@"and", result, field_eq, "") catch return error.CompilationFailed;
                }

                break :blk result;
            },
            .scalar, .zst, .closure, .list, .list_of_zst, .tag_union, .box, .box_of_zst => error.CompilationFailed,
        };
    }

    fn compareListPtrs(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, list_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const list_layout = ls.getLayout(list_layout_idx);

        switch (list_layout.tag) {
            .list, .list_of_zst => {},
            .scalar, .zst, .closure, .struct_, .tag_union, .box, .box_of_zst => return error.CompilationFailed,
        }

        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const lhs_data = wip.load(.normal, ptr_type, lhs_ptr, alignment, "") catch return error.CompilationFailed;
        const rhs_data = wip.load(.normal, ptr_type, rhs_ptr, alignment, "") catch return error.CompilationFailed;
        const lhs_len_ptr = wip.gep(.inbounds, .i8, lhs_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const rhs_len_ptr = wip.gep(.inbounds, .i8, rhs_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const lhs_len = wip.load(.normal, .i64, lhs_len_ptr, alignment, "") catch return error.CompilationFailed;
        const rhs_len = wip.load(.normal, .i64, rhs_len_ptr, alignment, "") catch return error.CompilationFailed;
        const len_eq = wip.icmp(.eq, lhs_len, rhs_len, "") catch return error.OutOfMemory;

        const elem_info = try self.getListElementInfo(list_layout_idx);
        if (elem_info.elem_size == 0) {
            return len_eq;
        }

        const len_mismatch_block = wip.block(1, "list_eq_len_mismatch") catch return error.OutOfMemory;
        const header_block = wip.block(2, "list_eq_header") catch return error.OutOfMemory;
        const body_block = wip.block(1, "list_eq_body") catch return error.OutOfMemory;
        const continue_block = wip.block(1, "list_eq_continue") catch return error.OutOfMemory;
        const mismatch_elem_block = wip.block(1, "list_eq_elem_mismatch") catch return error.OutOfMemory;
        const all_equal_block = wip.block(1, "list_eq_all_equal") catch return error.OutOfMemory;
        const exit_block = wip.block(3, "list_eq_exit") catch return error.OutOfMemory;

        const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const one_i64 = builder.intValue(.i64, 1) catch return error.OutOfMemory;
        const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
        const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        const elem_size_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;

        const entry_block = wip.cursor.block;
        _ = wip.brCond(len_eq, header_block, len_mismatch_block, .none) catch return error.CompilationFailed;

        wip.cursor = .{ .block = len_mismatch_block };
        _ = wip.br(exit_block) catch return error.CompilationFailed;

        wip.cursor = .{ .block = header_block };
        const idx_phi = wip.phi(.i64, "list_eq_idx") catch return error.CompilationFailed;
        const idx = idx_phi.toValue();
        const in_bounds = wip.icmp(.ult, idx, lhs_len, "") catch return error.OutOfMemory;
        _ = wip.brCond(in_bounds, body_block, all_equal_block, .none) catch return error.CompilationFailed;

        wip.cursor = .{ .block = body_block };
        const elem_offset = wip.bin(.mul, idx, elem_size_val, "") catch return error.CompilationFailed;
        const lhs_elem_ptr = wip.gep(.inbounds, .i8, lhs_data, &.{elem_offset}, "") catch return error.CompilationFailed;
        const rhs_elem_ptr = wip.gep(.inbounds, .i8, rhs_data, &.{elem_offset}, "") catch return error.CompilationFailed;
        const elem_eq = try self.comparePtrsByLayout(lhs_elem_ptr, rhs_elem_ptr, elem_info.elem_layout_idx);
        _ = wip.brCond(elem_eq, continue_block, mismatch_elem_block, .none) catch return error.CompilationFailed;

        wip.cursor = .{ .block = continue_block };
        const next_idx = wip.bin(.add, idx, one_i64, "") catch return error.CompilationFailed;
        _ = wip.br(header_block) catch return error.CompilationFailed;
        idx_phi.finish(
            &.{ zero_i64, next_idx },
            &.{ entry_block, continue_block },
            wip,
        );

        wip.cursor = .{ .block = mismatch_elem_block };
        _ = wip.br(exit_block) catch return error.CompilationFailed;

        wip.cursor = .{ .block = all_equal_block };
        _ = wip.br(exit_block) catch return error.CompilationFailed;

        wip.cursor = .{ .block = exit_block };
        const result_phi = wip.phi(.i1, "list_eq_result") catch return error.CompilationFailed;
        result_phi.finish(
            &.{ false_val, false_val, true_val },
            &.{ len_mismatch_block, mismatch_elem_block, all_equal_block },
            wip,
        );
        return result_phi.toValue();
    }

    fn compareTagUnionPtrs(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, union_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        return self.compareTagUnionValues(lhs_ptr, rhs_ptr, union_layout_idx);
    }

    fn compareBoxPtrs(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, box_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(box_layout_idx);

        return switch (stored_layout.tag) {
            .box => blk: {
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const null_ptr = builder.nullValue(ptr_type) catch return error.OutOfMemory;
                const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const lhs_box = try self.loadValueFromPtr(lhs_ptr, box_layout_idx);
                const rhs_box = try self.loadValueFromPtr(rhs_ptr, box_layout_idx);
                const same_ptr = wip.icmp(.eq, lhs_box, rhs_box, "") catch return error.OutOfMemory;
                const lhs_null = wip.icmp(.eq, lhs_box, null_ptr, "") catch return error.OutOfMemory;
                const rhs_null = wip.icmp(.eq, rhs_box, null_ptr, "") catch return error.OutOfMemory;
                const either_null = wip.bin(.@"or", lhs_null, rhs_null, "") catch return error.CompilationFailed;

                const same_block = wip.block(1, "box_eq_same") catch return error.OutOfMemory;
                const check_null_block = wip.block(1, "box_eq_check_null") catch return error.OutOfMemory;
                const null_block = wip.block(1, "box_eq_null") catch return error.OutOfMemory;
                const compare_block = wip.block(1, "box_eq_compare") catch return error.OutOfMemory;
                const exit_block = wip.block(3, "box_eq_exit") catch return error.OutOfMemory;

                _ = wip.brCond(same_ptr, same_block, check_null_block, .none) catch return error.CompilationFailed;

                wip.cursor = .{ .block = same_block };
                _ = wip.br(exit_block) catch return error.CompilationFailed;

                wip.cursor = .{ .block = check_null_block };
                _ = wip.brCond(either_null, null_block, compare_block, .none) catch return error.CompilationFailed;

                wip.cursor = .{ .block = null_block };
                _ = wip.br(exit_block) catch return error.CompilationFailed;

                wip.cursor = .{ .block = compare_block };
                const inner_eq = try self.comparePtrsByLayout(lhs_box, rhs_box, stored_layout.data.box);
                const compare_result_block = wip.cursor.block;
                _ = wip.br(exit_block) catch return error.CompilationFailed;

                wip.cursor = .{ .block = exit_block };
                const result_phi = wip.phi(.i1, "box_eq_result") catch return error.CompilationFailed;
                result_phi.finish(
                    &.{ true_val, false_val, inner_eq },
                    &.{ same_block, null_block, compare_result_block },
                    wip,
                );
                break :blk result_phi.toValue();
            },
            .box_of_zst => builder.intValue(.i1, 1) catch return error.OutOfMemory,
            .scalar, .zst, .closure, .struct_, .list, .list_of_zst, .tag_union => error.CompilationFailed,
        };
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

    const TagUnionValueMode = enum {
        scalar_discriminant,
        transparent_payload,
        indirect_pointer,
    };

    fn tagUnionValueMode(self: *const MonoLlvmCodeGen, union_layout_idx: layout.Idx) Error!TagUnionValueMode {
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(union_layout_idx);
        if (stored_layout.tag != .tag_union) return error.CompilationFailed;

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const variants = ls.getTagUnionVariants(tu_data);

        if (tu_data.discriminant_size == 0) {
            if (variants.len != 1) return error.CompilationFailed;

            return if (variants.get(0).payload_layout == .zst)
                .scalar_discriminant
            else
                .transparent_payload;
        }

        for (0..variants.len) |variant_idx| {
            if (variants.get(@intCast(variant_idx)).payload_layout != .zst) {
                return .indirect_pointer;
            }
        }

        return .scalar_discriminant;
    }

    fn tagUnionTransparentPayloadLayout(self: *const MonoLlvmCodeGen, union_layout_idx: layout.Idx) Error!layout.Idx {
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(union_layout_idx);
        if (stored_layout.tag != .tag_union) return error.CompilationFailed;
        if (try self.tagUnionValueMode(union_layout_idx) != .transparent_payload) return error.CompilationFailed;

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const variants = ls.getTagUnionVariants(tu_data);
        if (variants.len != 1) return error.CompilationFailed;

        const payload_layout = variants.get(0).payload_layout;
        if (payload_layout == .zst) return error.CompilationFailed;

        return payload_layout;
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

    fn coerceShiftAmountToType(self: *MonoLlvmCodeGen, val: LlvmBuilder.Value, target_type: LlvmBuilder.Type) Error!LlvmBuilder.Value {
        if (val == .none) return error.CompilationFailed;

        const wip = self.wip orelse return error.CompilationFailed;
        const actual_type = val.typeOfWip(wip);

        if (actual_type == target_type) return val;
        if (!isIntType(actual_type) or !isIntType(target_type)) return error.CompilationFailed;

        const actual_bits = intTypeBits(actual_type);
        const target_bits = intTypeBits(target_type);

        if (actual_bits < target_bits) {
            return wip.cast(.zext, val, target_type, "") catch return error.CompilationFailed;
        }

        if (actual_bits > target_bits) {
            return wip.cast(.trunc, val, target_type, "") catch return error.CompilationFailed;
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
        const builder = self.builder orelse return error.CompilationFailed;
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

        if (actual_type.isPointer(builder) and expected_type.isPointer(builder)) {
            return wip.cast(.bitcast, value, expected_type, "") catch return error.CompilationFailed;
        }

        if (!actual_type.isPointer(builder) and expected_type.isPointer(builder)) {
            if (value_layout) |layout_idx| {
                return try self.materializeGeneratedValueToPtr(value, layout_idx);
            }
        }

        return value;
    }

    fn materializeGeneratedValueToPtr(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const size = try self.materializedLayoutSize(layout_idx);
        const byte_array_type = builder.arrayType(size, .i8) catch return error.OutOfMemory;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const alloca_ptr = wip.alloca(.normal, byte_array_type, .none, alignment, .default, "coerce_tmp") catch return error.CompilationFailed;

        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const size_val = builder.intValue(.i32, size) catch return error.OutOfMemory;
        _ = wip.callMemSet(alloca_ptr, alignment, zero_byte, size_val, .normal, false) catch return error.CompilationFailed;
        _ = wip.store(.normal, value, alloca_ptr, alignment) catch return error.CompilationFailed;
        return alloca_ptr;
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
            return self.loadStoredAlloca(cell_alloca);
        }

        if (self.loop_var_allocas.get(key)) |lva| {
            return self.loadStoredAlloca(lva);
        }

        return self.generateLookup(cell, layout_idx);
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

    fn discriminantIntTypeOrZst(disc_size: u8) LlvmBuilder.Type {
        return switch (disc_size) {
            0 => .i8,
            1 => .i8,
            2 => .i16,
            4 => .i32,
            8 => .i64,
            else => .i8,
        };
    }

    const TagPayloadInfo = struct {
        root_ptr: LlvmBuilder.Value,
        payload_layout: layout.Idx,
    };

    fn materializeTagValuePtr(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, union_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const stored_layout = (self.layout_store orelse return error.CompilationFailed).getLayout(union_layout_idx);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        return switch (stored_layout.tag) {
            .tag_union => switch (try self.tagUnionValueMode(union_layout_idx)) {
                .scalar_discriminant, .transparent_payload => try self.materializeGeneratedValueToPtr(value, union_layout_idx),
                .indirect_pointer => blk: {
                    if (value.typeOfWip(self.wip orelse return error.CompilationFailed) != ptr_type) {
                        return error.CompilationFailed;
                    }
                    break :blk value;
                },
            },
            .box => blk: {
                if (value.typeOfWip(self.wip orelse return error.CompilationFailed) != ptr_type) return error.CompilationFailed;
                break :blk value;
            },
            .scalar, .zst => return error.CompilationFailed,
            .closure, .struct_, .list, .list_of_zst, .box_of_zst => return error.CompilationFailed,
        };
    }

    fn loadTagDiscriminant(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, union_layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(union_layout_idx);

        return switch (stored_layout.tag) {
            .scalar => blk: {
                const val_type = value.typeOfWip(wip);
                if (!isIntType(val_type)) return error.CompilationFailed;
                break :blk value;
            },
            .zst => builder.intValue(.i8, 0) catch return error.OutOfMemory,
            .tag_union => blk: {
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                switch (try self.tagUnionValueMode(union_layout_idx)) {
                    .scalar_discriminant => {
                        if (tu_data.discriminant_size == 0) {
                            break :blk builder.intValue(.i8, 0) catch return error.OutOfMemory;
                        }

                        const disc_type = discriminantIntType(tu_data.discriminant_size);
                        if (value.typeOfWip(wip).isPointer(builder)) {
                            const disc_offset = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;
                            const disc_ptr = wip.gep(.inbounds, .i8, value, &.{disc_offset}, "") catch return error.CompilationFailed;
                            break :blk wip.load(
                                .normal,
                                disc_type,
                                disc_ptr,
                                LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)),
                                "",
                            ) catch return error.CompilationFailed;
                        }

                        break :blk try self.coerceValueToType(value, disc_type, null);
                    },
                    .transparent_payload => break :blk builder.intValue(.i8, 0) catch return error.OutOfMemory,
                    .indirect_pointer => {
                        const root_ptr = try self.materializeTagValuePtr(value, union_layout_idx);
                        const disc_type = discriminantIntType(tu_data.discriminant_size);
                        const disc_offset = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;
                        const disc_ptr = wip.gep(.inbounds, .i8, root_ptr, &.{disc_offset}, "") catch return error.CompilationFailed;
                        break :blk wip.load(
                            .normal,
                            disc_type,
                            disc_ptr,
                            LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)),
                            "",
                        ) catch return error.CompilationFailed;
                    },
                }
            },
            .box => blk: {
                const inner_layout = ls.getLayout(stored_layout.data.box);
                if (inner_layout.tag != .tag_union) return error.CompilationFailed;
                const tu_data = ls.getTagUnionData(inner_layout.data.tag_union.idx);
                switch (try self.tagUnionValueMode(stored_layout.data.box)) {
                    .scalar_discriminant, .transparent_payload => break :blk builder.intValue(.i8, 0) catch return error.OutOfMemory,
                    .indirect_pointer => {
                        const root_ptr = try self.materializeTagValuePtr(value, union_layout_idx);
                        const disc_type = discriminantIntType(tu_data.discriminant_size);
                        const disc_offset = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;
                        const disc_ptr = wip.gep(.inbounds, .i8, root_ptr, &.{disc_offset}, "") catch return error.CompilationFailed;
                        break :blk wip.load(
                            .normal,
                            disc_type,
                            disc_ptr,
                            LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)),
                            "",
                        ) catch return error.CompilationFailed;
                    },
                }
            },
            .closure, .struct_, .list, .list_of_zst, .box_of_zst => return error.CompilationFailed,
        };
    }

    fn getTagPayloadInfo(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, union_layout_idx: layout.Idx, discriminant: usize) Error!TagPayloadInfo {
        const ls = self.layout_store orelse return error.CompilationFailed;
        const stored_layout = ls.getLayout(union_layout_idx);

        return switch (stored_layout.tag) {
            .tag_union => blk: {
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                if (discriminant >= variants.len) return error.CompilationFailed;
                switch (try self.tagUnionValueMode(union_layout_idx)) {
                    .scalar_discriminant => break :blk .{
                        .root_ptr = value,
                        .payload_layout = variants.get(@intCast(discriminant)).payload_layout,
                    },
                    .transparent_payload => {
                        if (discriminant != 0) return error.CompilationFailed;
                        break :blk .{
                            .root_ptr = try self.materializeTagValuePtr(value, union_layout_idx),
                            .payload_layout = variants.get(0).payload_layout,
                        };
                    },
                    .indirect_pointer => break :blk .{
                        .root_ptr = try self.materializeTagValuePtr(value, union_layout_idx),
                        .payload_layout = variants.get(@intCast(discriminant)).payload_layout,
                    },
                }
            },
            .box => blk: {
                const inner_layout = ls.getLayout(stored_layout.data.box);
                if (inner_layout.tag != .tag_union) return error.CompilationFailed;
                const tu_data = ls.getTagUnionData(inner_layout.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                if (discriminant >= variants.len) return error.CompilationFailed;
                switch (try self.tagUnionValueMode(stored_layout.data.box)) {
                    .scalar_discriminant => break :blk .{
                        .root_ptr = value,
                        .payload_layout = variants.get(@intCast(discriminant)).payload_layout,
                    },
                    .transparent_payload => {
                        if (discriminant != 0) return error.CompilationFailed;
                        break :blk .{
                            .root_ptr = try self.materializeTagValuePtr(value, union_layout_idx),
                            .payload_layout = variants.get(0).payload_layout,
                        };
                    },
                    .indirect_pointer => break :blk .{
                        .root_ptr = try self.materializeTagValuePtr(value, union_layout_idx),
                        .payload_layout = variants.get(@intCast(discriminant)).payload_layout,
                    },
                }
            },
            .scalar, .zst => .{
                .root_ptr = value,
                .payload_layout = .zst,
            },
            .closure, .struct_, .list, .list_of_zst, .box_of_zst => return error.CompilationFailed,
        };
    }

    /// Generate a zero-argument tag (just the discriminant value).
    fn generateZeroArgTag(self: *MonoLlvmCodeGen, zat: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;

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
                switch (try self.tagUnionValueMode(zat.union_layout)) {
                    .scalar_discriminant => {
                        const repr_type = discriminantIntTypeOrZst(tu_data.discriminant_size);
                        return (builder.intConst(repr_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory).toValue();
                    },
                    .transparent_payload => return error.CompilationFailed,
                    .indirect_pointer => {
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

                        const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                        const size_val = builder.intValue(.i32, tu_size) catch return error.OutOfMemory;
                        _ = wip.callMemSet(heap_ptr, alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

                        const disc_offset = tu_data.discriminant_offset;
                        const disc_type = discriminantIntType(tu_data.discriminant_size);
                        const disc_val = builder.intValue(disc_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory;
                        const disc_ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                        _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

                        return heap_ptr;
                    },
                }
            },
            .closure, .struct_, .list, .list_of_zst, .box, .box_of_zst => unreachable,
        }
    }

    fn generateTagPayloadValue(
        self: *MonoLlvmCodeGen,
        payload_layout_idx: layout.Idx,
        arg_exprs: []const LirExprId,
    ) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const payload_layout = ls.getLayout(payload_layout_idx);

        if (payload_layout_idx == .zst or payload_layout.tag == .zst) {
            if (arg_exprs.len != 0) return error.CompilationFailed;
            return builder.intValue(.i8, 0) catch return error.OutOfMemory;
        }

        return switch (payload_layout.tag) {
            .struct_ => blk: {
                const struct_data = ls.getStructData(payload_layout.data.struct_.idx);
                const sorted_fields = ls.struct_fields.sliceRange(struct_data.getFields());

                if (arg_exprs.len == 1) {
                    const arg = try self.generateExprAsValue(arg_exprs[0]);
                    if (arg.layout_idx != null and arg.layout_idx.? == payload_layout_idx) {
                        break :blk try self.coerceValueToType(
                            arg.value,
                            try self.layoutToLlvmTypeFull(payload_layout_idx),
                            arg.layout_idx,
                        );
                    }
                }

                if (sorted_fields.len != arg_exprs.len) return error.CompilationFailed;

                var field_values_buf: [32]LlvmBuilder.Value = undefined;
                for (0..sorted_fields.len) |field_i| {
                    const field = sorted_fields.get(@intCast(field_i));
                    if (field.index >= arg_exprs.len) return error.CompilationFailed;

                    const field_layout = ls.getStructFieldLayout(payload_layout.data.struct_.idx, @intCast(field_i));
                    const arg = try self.generateExprAsValue(arg_exprs[field.index]);
                    field_values_buf[field_i] = try self.convertToFieldType(arg.value, field_layout);
                }

                const wip = self.wip orelse return error.CompilationFailed;
                const struct_type = try buildStructTypeFromValues(builder, wip, field_values_buf[0..sorted_fields.len]);
                var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
                for (0..sorted_fields.len) |field_i| {
                    struct_val = wip.insertValue(struct_val, field_values_buf[field_i], &.{@intCast(field_i)}, "") catch return error.CompilationFailed;
                }
                break :blk struct_val;
            },
            .scalar,
            .tag_union,
            .box,
            .closure,
            .list,
            .list_of_zst,
            .box_of_zst,
            => blk: {
                if (arg_exprs.len != 1) return error.CompilationFailed;
                const arg = try self.generateExprAsValue(arg_exprs[0]);
                break :blk try self.coerceValueToLayout(arg.value, payload_layout_idx);
            },
            .zst => unreachable,
        };
    }

    /// Generate a tag with payload arguments.
    fn generateTagWithPayload(self: *MonoLlvmCodeGen, tag_expr: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;

        const stored_layout = ls.getLayout(tag_expr.union_layout);
        std.debug.assert(stored_layout.tag == .tag_union);

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const arg_exprs = self.store.getExprSpan(tag_expr.args);
        const variants = ls.getTagUnionVariants(tu_data);
        const variant = variants.get(tag_expr.discriminant);

        switch (try self.tagUnionValueMode(tag_expr.union_layout)) {
            .scalar_discriminant => return error.CompilationFailed,
            .transparent_payload => {
                if (tag_expr.discriminant != 0) return error.CompilationFailed;
                return try self.generateTagPayloadValue(variant.payload_layout, arg_exprs);
            },
            .indirect_pointer => {
                const tu_size = tu_data.size;
                const tu_align_bytes: u64 = @intCast(stored_layout.data.tag_union.alignment.toByteUnits());
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

                const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const size_val = builder.intValue(.i32, padded_size) catch return error.OutOfMemory;
                _ = wip.callMemSet(heap_ptr, forced_alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

                if (variant.payload_layout != .zst) {
                    const payload_value = try self.generateTagPayloadValue(variant.payload_layout, arg_exprs);
                    const store_val = try self.convertToFieldType(payload_value, variant.payload_layout);
                    const payload_align = self.fieldAlignmentForLayout(variant.payload_layout);
                    _ = wip.store(.normal, store_val, heap_ptr, payload_align) catch return error.CompilationFailed;
                }

                const disc_offset = tu_data.discriminant_offset;
                const disc_type = discriminantIntType(tu_data.discriminant_size);
                const disc_val = builder.intValue(disc_type, @as(u64, tag_expr.discriminant)) catch return error.OutOfMemory;
                const disc_ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

                return heap_ptr;
            },
        }
    }

    /// Extract the payload from a tag union value.
    fn generateTagPayloadAccess(self: *MonoLlvmCodeGen, tpa: anytype) Error!LlvmBuilder.Value {
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
                return switch (try self.tagUnionValueMode(tpa.union_layout)) {
                    .scalar_discriminant => raw_value,
                    .transparent_payload => blk: {
                        const payload_ptr = try self.materializeTagValuePtr(raw_value, tpa.union_layout);
                        break :blk try self.loadFieldValueFromPtr(payload_ptr, tpa.payload_layout);
                    },
                    .indirect_pointer => self.loadFieldValueFromPtr(raw_value, tpa.payload_layout),
                };
            },
            .box => {
                const inner_layout = ls.getLayout(union_layout.data.box);
                if (inner_layout.tag == .tag_union) {
                    return self.loadFieldValueFromPtr(raw_value, tpa.payload_layout);
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

        // Generate the value to switch on
        const value = try self.generateExpr(ds.value);
        if (self.currentBlockHasTerminator()) {
            return builder.poisonValue(try self.layoutToLlvmTypeFull(ds.result_layout)) catch return error.OutOfMemory;
        }
        var env_snapshot = try LexicalEnvSnapshot.init(self);
        defer env_snapshot.deinit();

        // Get branch expressions
        const branch_exprs = self.store.getExprSpan(ds.branches);
        std.debug.assert(branch_exprs.len != 0);

        const discriminant = try self.loadTagDiscriminant(value, ds.union_layout);

        var merge_incoming: u32 = 0;
        var branch_returns: [64]bool = undefined;
        for (branch_exprs, 0..) |branch_expr_id, i| {
            const reaches_merge = !self.exprNeverReturns(branch_expr_id);
            branch_returns[i] = reaches_merge;
            if (reaches_merge) merge_incoming += 1;
        }

        const has_merge = merge_incoming != 0;
        const merge_block = if (has_merge)
            try wip.block(merge_incoming, "ds_merge")
        else
            LlvmBuilder.Function.Block.Index.entry;

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
                try env_snapshot.restore(self);
                var branch_scope = try self.beginScope();
                defer branch_scope.deinit();
                const branch_val = try self.generateControlFlowValue(branch_expr_id, ds.result_layout);
                if (branch_returns[i]) {
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = branch_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                }
                try self.endScope(&branch_scope);

                // Else block — continue to next comparison
                wip.cursor = .{ .block = else_block };
            } else {
                // Last branch — no comparison needed (default case)
                try env_snapshot.restore(self);
                var branch_scope = try self.beginScope();
                defer branch_scope.deinit();
                const branch_val = try self.generateControlFlowValue(branch_expr_id, ds.result_layout);
                if (branch_returns[i]) {
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = branch_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                }
                try self.endScope(&branch_scope);
            }
        }

        if (!has_merge) {
            return builder.poisonValue(try self.layoutToLlvmTypeFull(ds.result_layout)) catch return error.OutOfMemory;
        }

        // Merge block
        try env_snapshot.restore(self);
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
                if (self.cell_allocas.contains(key)) continue;
                const val_type = val.typeOfWip(wip);
                const alloca_val = wip.alloca(.normal, val_type, .none, alignment, .default, "lv") catch return error.CompilationFailed;
                _ = wip.store(.normal, val, alloca_val, alignment) catch return error.CompilationFailed;
                self.loop_var_allocas.put(key, .{ .alloca_ptr = alloca_val, .elem_type = val_type, .alignment = alignment }) catch return error.OutOfMemory;
                promoted_keys.append(self.allocator, key) catch return error.OutOfMemory;
            }
        }

        const body_returns = !self.exprNeverReturns(wl.body);

        // Create blocks: cond has entry plus an optional back-edge from the body.
        const cond_block = wip.block(1 + @as(u32, @intFromBool(body_returns)), "while_cond") catch return error.OutOfMemory;
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
                const loaded = try self.loadStoredAlloca(lva);
                self.symbol_values.put(key, loaded) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const loaded = try self.loadStoredAlloca(entry.value_ptr.*);
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
                const loaded = try self.loadStoredAlloca(lva);
                self.symbol_values.put(key, loaded) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const loaded = try self.loadStoredAlloca(entry.value_ptr.*);
                self.symbol_values.put(entry.key_ptr.*, loaded) catch return error.OutOfMemory;
            }
        }
        _ = try self.generateExpr(wl.body);
        if (!self.currentBlockHasTerminator()) {
            _ = wip.br(cond_block) catch return error.CompilationFailed;
        }

        // Exit block: load final values from allocas
        wip.cursor = .{ .block = exit_block };
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const final_val = try self.loadStoredAlloca(lva);
                self.symbol_values.put(key, final_val) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const loaded = try self.loadStoredAlloca(entry.value_ptr.*);
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
        const ls = self.layout_store orelse return error.CompilationFailed;

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
                if (self.cell_allocas.contains(key)) continue;
                const val_type = val.typeOfWip(wip);
                const alloca_val = wip.alloca(.normal, val_type, .none, alignment, .default, "lv") catch return error.CompilationFailed;
                _ = wip.store(.normal, val, alloca_val, alignment) catch return error.CompilationFailed;
                self.loop_var_allocas.put(key, .{ .alloca_ptr = alloca_val, .elem_type = val_type, .alignment = alignment }) catch return error.OutOfMemory;
                promoted_keys.append(self.allocator, key) catch return error.OutOfMemory;
            }
        }

        const body_returns = !self.exprNeverReturns(fl.body);

        // Create blocks: header gets entry plus an optional back-edge from the body.
        const header_block = wip.block(1 + @as(u32, @intFromBool(body_returns)), "for_header") catch return error.OutOfMemory;
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
                const loaded = try self.loadStoredAlloca(lva);
                self.symbol_values.put(key, loaded) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const loaded = try self.loadStoredAlloca(entry.value_ptr.*);
                self.symbol_values.put(entry.key_ptr.*, loaded) catch return error.OutOfMemory;
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
        const body_end_block = wip.cursor.block;
        var next_idx: LlvmBuilder.Value = .none;
        if (!self.currentBlockHasTerminator()) {
            const one = builder.intValue(.i64, 1) catch return error.OutOfMemory;
            next_idx = wip.bin(.add, idx_val, one, "") catch return error.CompilationFailed;
            _ = wip.br(header_block) catch return error.CompilationFailed;
        }

        // Finish phi with the entry edge and an optional loop back-edge.
        if (body_returns) {
            idx_phi.finish(
                &.{ zero, next_idx },
                &.{ entry_block, body_end_block },
                wip,
            );
        } else {
            idx_phi.finish(
                &.{zero},
                &.{entry_block},
                wip,
            );
        }

        // Exit block: load final values from allocas into symbol_values
        wip.cursor = .{ .block = exit_block };
        for (promoted_keys.items) |key| {
            if (self.loop_var_allocas.get(key)) |lva| {
                const final_val = try self.loadStoredAlloca(lva);
                self.symbol_values.put(key, final_val) catch return error.OutOfMemory;
            }
        }
        {
            var cell_it = self.cell_allocas.iterator();
            while (cell_it.next()) |entry| {
                const loaded = try self.loadStoredAlloca(entry.value_ptr.*);
                self.symbol_values.put(entry.key_ptr.*, loaded) catch return error.OutOfMemory;
            }
        }

        // Clean up loop variable allocas (un-promote for this loop level)
        for (promoted_keys.items) |key| {
            _ = self.loop_var_allocas.remove(key);
        }

        return builder.intValue(.i8, 0) catch return error.OutOfMemory;
    }

    // Pattern matching

    fn patternAlwaysMatches(self: *MonoLlvmCodeGen, pattern_id: LirPatternId) bool {
        const pattern = self.store.getPattern(pattern_id);
        return switch (pattern) {
            .wildcard, .bind => true,
            .as_pattern => |as_pat| blk: {
                if (as_pat.inner.isNone()) {
                    break :blk true;
                }
                break :blk self.patternAlwaysMatches(as_pat.inner);
            },
            .int_literal,
            .tag,
            .struct_,
            .list,
            .float_literal,
            .str_literal,
            => false,
        };
    }

    fn emitPatternMatchCondition(self: *MonoLlvmCodeGen, pattern_id: LirPatternId, value: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const pattern = self.store.getPattern(pattern_id);

        return switch (pattern) {
            .wildcard, .bind => builder.intValue(.i1, 1) catch return error.OutOfMemory,
            .as_pattern => |as_pat| blk: {
                if (as_pat.inner.isNone()) {
                    break :blk builder.intValue(.i1, 1) catch return error.OutOfMemory;
                }
                break :blk try self.emitPatternMatchCondition(as_pat.inner, value);
            },
            .int_literal => |int_pat| blk: {
                const pat_type = layoutToLlvmType(int_pat.layout_idx);
                const pat_val = builder.intValue(pat_type, @as(u64, @truncate(@as(u128, @bitCast(int_pat.value))))) catch return error.OutOfMemory;
                const cmp_value = if (value.typeOfWip(wip) == pat_type)
                    value
                else
                    wip.conv(.unsigned, value, pat_type, "") catch return error.CompilationFailed;
                break :blk wip.icmp(.eq, cmp_value, pat_val, "") catch return error.OutOfMemory;
            },
            .struct_ => |struct_pat| blk: {
                if (value == .none or !value.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;
                var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                const fields = self.store.getPatternSpan(struct_pat.fields);
                for (fields, 0..) |field_pat_id, idx| {
                    const field_val = wip.extractValue(value, &.{@intCast(idx)}, "") catch return error.CompilationFailed;
                    const field_match = try self.emitPatternMatchCondition(field_pat_id, field_val);
                    result = wip.bin(.@"and", result, field_match, "") catch return error.CompilationFailed;
                }
                break :blk result;
            },
            .tag => |tag_pat| blk: {
                const discriminant = try self.loadTagDiscriminant(value, tag_pat.union_layout);
                const disc_type = discriminant.typeOfWip(wip);
                const pat_disc = builder.intValue(disc_type, @as(u64, tag_pat.discriminant)) catch return error.OutOfMemory;
                var result = wip.icmp(.eq, discriminant, pat_disc, "") catch return error.OutOfMemory;

                const args = self.store.getPatternSpan(tag_pat.args);
                if (args.len == 0) break :blk result;

                const payload_info = try self.getTagPayloadInfo(value, tag_pat.union_layout, tag_pat.discriminant);
                if (payload_info.payload_layout == .zst) break :blk result;

                if (args.len == 1) {
                    const pattern_layout = self.getPatternLayoutIdx(args[0]);
                    var value_ptr = payload_info.root_ptr;
                    var value_layout_idx = payload_info.payload_layout;

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

                    const payload_value = try self.loadFieldValueFromPtr(value_ptr, value_layout_idx);
                    const payload_match = try self.emitPatternMatchCondition(args[0], payload_value);
                    result = wip.bin(.@"and", result, payload_match, "") catch return error.CompilationFailed;
                    break :blk result;
                }

                const payload_layout = ls.getLayout(payload_info.payload_layout);
                if (payload_layout.tag != .struct_) return error.CompilationFailed;

                for (args, 0..) |arg_id, arg_i| {
                    const field_layout = ls.getStructFieldLayoutByOriginalIndex(payload_layout.data.struct_.idx, @intCast(arg_i));
                    const offset = ls.getStructFieldOffsetByOriginalIndex(payload_layout.data.struct_.idx, @intCast(arg_i));
                    const offset_val = builder.intValue(.i32, offset) catch return error.OutOfMemory;
                    const field_ptr = wip.gep(.inbounds, .i8, payload_info.root_ptr, &.{offset_val}, "") catch return error.CompilationFailed;
                    const field_value = try self.loadFieldValueFromPtr(field_ptr, field_layout);
                    const field_match = try self.emitPatternMatchCondition(arg_id, field_value);
                    result = wip.bin(.@"and", result, field_match, "") catch return error.CompilationFailed;
                }

                break :blk result;
            },
            .list => |list_pat| blk: {
                if (value == .none or !value.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;

                const prefix_patterns = self.store.getPatternSpan(list_pat.prefix);
                const suffix_patterns = self.store.getPatternSpan(list_pat.suffix);
                const data_ptr = wip.extractValue(value, &.{0}, "") catch return error.CompilationFailed;
                const list_len = wip.extractValue(value, &.{1}, "") catch return error.CompilationFailed;

                const elem_layout_data = ls.getLayout(list_pat.elem_layout);
                const elem_sa = ls.layoutSizeAlign(elem_layout_data);
                const elem_size = builder.intValue(.i64, elem_sa.size) catch return error.OutOfMemory;

                const min_len = builder.intValue(.i64, @as(u64, @intCast(prefix_patterns.len + suffix_patterns.len))) catch return error.OutOfMemory;
                const len_pred: LlvmBuilder.IntegerCondition = if (list_pat.rest.isNone()) .eq else .uge;
                const len_matches = wip.icmp(len_pred, list_len, min_len, "") catch return error.OutOfMemory;
                const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const match_block = wip.block(1, "list_pat_match") catch return error.OutOfMemory;
                const exit_block = wip.block(2, "list_pat_exit") catch return error.OutOfMemory;
                const entry_block = wip.cursor.block;

                _ = wip.brCond(len_matches, match_block, exit_block, .none) catch return error.CompilationFailed;

                wip.cursor = .{ .block = match_block };
                var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;

                for (prefix_patterns, 0..) |sub_pat_id, idx| {
                    const elem_index = builder.intValue(.i64, @as(u64, @intCast(idx))) catch return error.OutOfMemory;
                    const byte_offset = wip.bin(.mul, elem_index, elem_size, "") catch return error.CompilationFailed;
                    const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;
                    const elem_val = try self.loadValueFromPtr(elem_ptr, list_pat.elem_layout);
                    const elem_match = try self.emitPatternMatchCondition(sub_pat_id, elem_val);
                    result = wip.bin(.@"and", result, elem_match, "") catch return error.CompilationFailed;
                }

                if (suffix_patterns.len > 0) {
                    const suffix_count = builder.intValue(.i64, @as(u64, @intCast(suffix_patterns.len))) catch return error.OutOfMemory;
                    const suffix_start = wip.bin(.sub, list_len, suffix_count, "") catch return error.CompilationFailed;
                    for (suffix_patterns, 0..) |sub_pat_id, idx| {
                        const suffix_idx = builder.intValue(.i64, @as(u64, @intCast(idx))) catch return error.OutOfMemory;
                        const elem_index = wip.bin(.add, suffix_start, suffix_idx, "") catch return error.CompilationFailed;
                        const byte_offset = wip.bin(.mul, elem_index, elem_size, "") catch return error.CompilationFailed;
                        const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;
                        const elem_val = try self.loadValueFromPtr(elem_ptr, list_pat.elem_layout);
                        const elem_match = try self.emitPatternMatchCondition(sub_pat_id, elem_val);
                        result = wip.bin(.@"and", result, elem_match, "") catch return error.CompilationFailed;
                    }
                }

                if (!list_pat.rest.isNone()) {
                    const prefix_len = builder.intValue(.i64, @as(u64, @intCast(prefix_patterns.len))) catch return error.OutOfMemory;
                    const rest_val = try self.buildRestSublist(value, list_pat.list_layout, prefix_len);
                    const rest_match = try self.emitPatternMatchCondition(list_pat.rest, rest_val);
                    result = wip.bin(.@"and", result, rest_match, "") catch return error.CompilationFailed;
                }

                const match_result_block = wip.cursor.block;
                _ = wip.br(exit_block) catch return error.CompilationFailed;

                wip.cursor = .{ .block = exit_block };
                const result_phi = wip.phi(.i1, "list_pat_result") catch return error.CompilationFailed;
                result_phi.finish(
                    &.{ false_val, result },
                    &.{ entry_block, match_result_block },
                    wip,
                );
                break :blk result_phi.toValue();
            },
            .float_literal, .str_literal => unreachable,
        };
    }

    fn generateMatchExpr(self: *MonoLlvmCodeGen, w: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        var env_snapshot = try LexicalEnvSnapshot.init(self);
        defer env_snapshot.deinit();

        // Evaluate the scrutinee
        const scrutinee = try self.generateExpr(w.value);
        if (self.currentBlockHasTerminator()) {
            return builder.poisonValue(try self.layoutToLlvmTypeFull(w.result_layout)) catch return error.OutOfMemory;
        }

        // Get branches
        const branches = self.store.getMatchBranches(w.branches);
        std.debug.assert(branches.len != 0);

        // Compute the incoming count for the merge block by scanning patterns.
        // Only branches that can actually continue to the merge contribute
        // incoming edges. Unconditional patterns stop further branch generation.
        var merge_incoming: u32 = 0;
        var branch_returns: [32]bool = undefined;
        var reachable_branch_count: usize = 0;
        for (branches) |branch| {
            const reaches_merge = !self.exprNeverReturns(branch.body);
            branch_returns[reachable_branch_count] = reaches_merge;
            if (reaches_merge) merge_incoming += 1;
            reachable_branch_count += 1;
            if (self.patternAlwaysMatches(branch.pattern)) break;
        }

        const has_merge = merge_incoming != 0;
        const merge_block = if (has_merge)
            try wip.block(merge_incoming, "when_merge")
        else
            LlvmBuilder.Function.Block.Index.entry;

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
                    try env_snapshot.restore(self);
                    var branch_scope = try self.beginScope();
                    defer branch_scope.deinit();
                    const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                    if (branch_returns[i]) {
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    }
                    try self.endScope(&branch_scope);
                    break; // No more branches after wildcard
                },

                .bind => |bind| {
                    // Always matches, bind the scrutinee to the symbol
                    const symbol_key: u64 = @bitCast(bind.symbol);
                    try env_snapshot.restore(self);
                    var branch_scope = try self.beginScope();
                    defer branch_scope.deinit();
                    self.symbol_values.put(symbol_key, scrutinee) catch return error.OutOfMemory;
                    const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                    if (branch_returns[i]) {
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    }
                    try self.endScope(&branch_scope);
                    break; // No more branches after bind
                },

                .int_literal,
                .tag,
                .list,
                .struct_,
                .as_pattern,
                => {
                    const cmp = try self.emitPatternMatchCondition(branch.pattern, scrutinee);
                    const then_block = wip.block(1, "pat_match") catch return error.OutOfMemory;
                    const else_block = wip.block(1, if (is_last) "pat_nomatch" else "pat_next") catch return error.OutOfMemory;
                    _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                    wip.cursor = .{ .block = then_block };
                    try env_snapshot.restore(self);
                    var branch_scope = try self.beginScope();
                    defer branch_scope.deinit();
                    try self.bindPattern(branch.pattern, scrutinee);
                    const body_val = try self.generateControlFlowValue(branch.body, w.result_layout);
                    if (branch_returns[i]) {
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    }
                    try self.endScope(&branch_scope);

                    wip.cursor = .{ .block = else_block };
                    if (is_last) {
                        _ = wip.@"unreachable"() catch return error.CompilationFailed;
                    }
                },

                .float_literal, .str_literal => unreachable,
            }
        }

        if (!has_merge) {
            return builder.poisonValue(try self.layoutToLlvmTypeFull(w.result_layout)) catch return error.OutOfMemory;
        }

        std.debug.assert(branch_count != 0);

        // Check if all branch values have the same type for the phi node
        try env_snapshot.restore(self);
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
        const ls = self.layout_store orelse return error.CompilationFailed;
        const payload_info = try self.getTagPayloadInfo(scrutinee, tag_pat.union_layout, tag_pat.discriminant);
        const payload_layout = ls.getLayout(payload_info.payload_layout);

        if (args.len == 1) {
            const pattern_layout = self.getPatternLayoutIdx(args[0]);
            var value_ptr = payload_info.root_ptr;
            var value_layout_idx = payload_info.payload_layout;

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

            const payload_value = try self.loadFieldValueFromPtr(value_ptr, value_layout_idx);
            try self.bindPattern(args[0], payload_value);
            return;
        }

        if (payload_layout.tag != .struct_) return error.CompilationFailed;

        for (args, 0..) |arg_id, arg_i| {
            const field_layout = ls.getStructFieldLayoutByOriginalIndex(payload_layout.data.struct_.idx, @intCast(arg_i));
            const offset = ls.getStructFieldOffsetByOriginalIndex(payload_layout.data.struct_.idx, @intCast(arg_i));
            const offset_val = builder.intValue(.i32, offset) catch return error.OutOfMemory;
            const field_ptr = wip.gep(.inbounds, .i8, payload_info.root_ptr, &.{offset_val}, "") catch return error.CompilationFailed;
            const field_value = try self.loadFieldValueFromPtr(field_ptr, field_layout);
            try self.bindPattern(arg_id, field_value);
        }
    }

    fn generateBreakExpr(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const exit_block = self.loop_exit_blocks.getLastOrNull() orelse return error.CompilationFailed;
        _ = wip.br(exit_block) catch return error.CompilationFailed;

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
            const ret_layout = self.result_layout orelse return error.CompilationFailed;

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
        const ls = self.layout_store orelse return error.CompilationFailed;

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
        const refcounted_val = builder.intValue(.i1, @intFromBool(ls.layoutContainsRefcounted(elem_layout_data))) catch return error.OutOfMemory;

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
                const elem_val = try self.generateExpr(elem_id);
                const store_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(elem_align, 1)));
                if (elem_val.typeOfWip(wip).isPointer(builder)) {
                    // Payload-carrying tag unions are materialized as pointers to their
                    // full in-memory representation. Copy the bytes into the list slot.
                    const size_val_copy = builder.intValue(.i32, @as(u32, @intCast(elem_size))) catch return error.OutOfMemory;
                    _ = wip.callMemCpy(elem_ptr, store_align, elem_val, store_align, size_val_copy, .normal, false) catch return error.CompilationFailed;
                } else {
                    // Zero-arg tag unions can lower to a scalar discriminant value even
                    // when the layout itself is still classified as .tag_union.
                    const store_val = try self.convertToFieldType(elem_val, list.elem_layout);
                    _ = wip.store(.@"volatile", store_val, elem_ptr, store_align) catch return error.CompilationFailed;
                }
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
            const ls = self.layout_store orelse return error.CompilationFailed;
            const ret_layout = ls.getLayout(ll.ret_layout);
            const sa = ls.layoutSizeAlign(ret_layout);
            const temp_words = @max((sa.size + 7) / 8, 1);
            const alloca_count = builder.intValue(.i32, @as(u64, @intCast(temp_words))) catch return error.OutOfMemory;
            const temp_alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(sa.alignment.toByteUnits(), 1)));
            temp_alloca = wip.alloca(.normal, .i64, alloca_count, temp_alignment, .default, "ll_tmp") catch return error.CompilationFailed;
            self.out_ptr = temp_alloca;
        }

        const result = self.generateLowLevelInner(ll) catch |err| {
            if (needs_temp) self.out_ptr = saved_out_ptr;
            return err;
        };

        if (needs_temp) {
            self.out_ptr = saved_out_ptr;
            if (result == .none) {
                const ls = self.layout_store orelse return error.CompilationFailed;
                const stored_layout = ls.getLayout(ll.ret_layout);
                if (stored_layout.tag == .tag_union) {
                    return switch (try self.tagUnionValueMode(ll.ret_layout)) {
                        .scalar_discriminant, .transparent_payload => blk: {
                            const load_type = try self.layoutToLlvmTypeFull(ll.ret_layout);
                            break :blk wip.load(.normal, load_type, temp_alloca, self.alignmentForLayout(ll.ret_layout), "ll_val") catch return error.CompilationFailed;
                        },
                        .indirect_pointer => temp_alloca,
                    };
                }

                const load_type = try self.layoutToLlvmTypeFull(ll.ret_layout);
                return wip.load(.normal, load_type, temp_alloca, self.alignmentForLayout(ll.ret_layout), "ll_val") catch return error.CompilationFailed;
            }
        }

        return result;
    }

    fn generateLowLevelInner(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const args = self.store.getExprSpan(ll.args);
        switch (ll.op) {
            // --- Numeric arithmetic ---
            .num_plus => {
                std.debug.assert(args.len >= 2);
                var lhs = try self.generateExpr(args[0]);
                var rhs = try self.generateExpr(args[1]);
                if (ll.ret_layout == .dec) {
                    lhs = try self.coerceValueToLayout(lhs, ll.ret_layout);
                    rhs = try self.coerceValueToLayout(rhs, ll.ret_layout);
                    return wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed;
                }
                const is_float = isFloatLayout(ll.ret_layout);
                lhs = try self.coerceValueToLayout(lhs, ll.ret_layout);
                rhs = try self.coerceValueToLayout(rhs, ll.ret_layout);
                return if (is_float)
                    wip.bin(.fadd, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed;
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
            .num_abs_diff => {
                std.debug.assert(args.len >= 2);
                const arg_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                var lhs = try self.generateExpr(args[0]);
                var rhs = try self.generateExpr(args[1]);

                if (isFloatLayout(arg_layout)) {
                    lhs = try self.coerceValueToLayout(lhs, arg_layout);
                    rhs = try self.coerceValueToLayout(rhs, arg_layout);
                    const diff = wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed;
                    const zero = if (arg_layout == .f32)
                        (builder.floatConst(0.0) catch return error.OutOfMemory).toValue()
                    else
                        (builder.doubleConst(0.0) catch return error.OutOfMemory).toValue();
                    const is_neg = wip.fcmp(.normal, .olt, diff, zero, "") catch return error.OutOfMemory;
                    const neg_diff = wip.un(.fneg, diff, "") catch return error.CompilationFailed;
                    return wip.select(.normal, is_neg, neg_diff, diff, "") catch return error.CompilationFailed;
                }

                lhs = try self.coerceValueToLayout(lhs, arg_layout);
                rhs = try self.coerceValueToLayout(rhs, arg_layout);

                const lhs_ge_rhs = wip.icmp(
                    if (isSigned(arg_layout)) .sge else .uge,
                    lhs,
                    rhs,
                    "",
                ) catch return error.OutOfMemory;
                const larger = wip.select(.normal, lhs_ge_rhs, lhs, rhs, "") catch return error.CompilationFailed;
                const smaller = wip.select(.normal, lhs_ge_rhs, rhs, lhs, "") catch return error.CompilationFailed;
                return wip.bin(.sub, larger, smaller, "") catch return error.CompilationFailed;
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
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            => {
                std.debug.assert(args.len >= 2);
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                var lhs = try self.generateExpr(args[0]);
                lhs = try self.coerceValueToLayout(lhs, operand_layout);

                var rhs = try self.generateExpr(args[1]);
                rhs = try self.coerceShiftAmountToType(rhs, lhs.typeOfWip(wip));

                return switch (ll.op) {
                    .num_shift_left_by => wip.bin(.shl, lhs, rhs, "") catch return error.CompilationFailed,
                    .num_shift_right_by => wip.bin(.ashr, lhs, rhs, "") catch return error.CompilationFailed,
                    .num_shift_right_zf_by => wip.bin(.lshr, lhs, rhs, "") catch return error.CompilationFailed,
                    else => unreachable,
                };
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
                const ls = self.layout_store orelse return error.CompilationFailed;
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
                    const signed = isSigned(arg_layout);
                    const lt = wip.icmp(if (signed) .slt else .ult, lhs, rhs, "") catch return error.OutOfMemory;
                    const gt = wip.icmp(if (signed) .sgt else .ugt, lhs, rhs, "") catch return error.OutOfMemory;
                    const lt_val = wip.conv(.unsigned, lt, .i8, "") catch return error.CompilationFailed;
                    const gt_val = wip.conv(.unsigned, gt, .i8, "") catch return error.CompilationFailed;
                    return wip.bin(.sub, gt_val, lt_val, "") catch return error.CompilationFailed;
                }
            },

            // --- Crash ---
            .crash => {
                _ = wip.@"unreachable"() catch return error.CompilationFailed;
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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
                const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
                const cap_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off16}, "") catch return error.CompilationFailed;
                const list_cap = wip.load(.normal, .i64, cap_ptr, alignment, "") catch return error.CompilationFailed;

                const ls = self.layout_store orelse return error.CompilationFailed;
                const ret_layout_val = ls.getLayout(ll.ret_layout);
                std.debug.assert(ret_layout_val.tag == .tag_union);
                const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
                const variants = ls.getTagUnionVariants(tu_data);
                var ok_index: ?usize = null;
                var err_index: ?usize = null;

                for (0..variants.len) |variant_idx| {
                    const payload_layout = variants.get(@intCast(variant_idx)).payload_layout;
                    const candidate = self.unwrapSingleFieldPayloadLayout(payload_layout) orelse payload_layout;

                    if (candidate == .str) {
                        ok_index = variant_idx;
                    } else if (err_index == null) {
                        err_index = variant_idx;
                    }
                }

                const resolved_ok_index = ok_index orelse return error.CompilationFailed;
                const resolved_err_index = err_index orelse return error.CompilationFailed;
                const err_layout_idx = variants.get(@intCast(resolved_err_index)).payload_layout;
                const unwrapped_err_layout_idx = self.unwrapSingleFieldPayloadLayout(err_layout_idx) orelse err_layout_idx;
                const err_layout_val = ls.getLayout(unwrapped_err_layout_idx);
                const record_idx = switch (err_layout_val.tag) {
                    .struct_ => err_layout_val.data.struct_.idx,
                    .tag_union => blk: {
                        const inner_tu_data = ls.getTagUnionData(err_layout_val.data.tag_union.idx);
                        const inner_variants = ls.getTagUnionVariants(inner_tu_data);
                        if (inner_variants.len == 0) return error.CompilationFailed;
                        const inner_payload_layout_idx = inner_variants.get(0).payload_layout;
                        const unwrapped_inner_payload_idx = self.unwrapSingleFieldPayloadLayout(inner_payload_layout_idx) orelse inner_payload_layout_idx;
                        const inner_payload_layout = ls.getLayout(unwrapped_inner_payload_idx);
                        if (inner_payload_layout.tag != .struct_) return error.CompilationFailed;
                        break :blk inner_payload_layout.data.struct_.idx;
                    },
                    else => return error.CompilationFailed,
                };
                const struct_data = ls.getStructData(record_idx);
                const fields = ls.struct_fields.sliceRange(struct_data.getFields());
                if (fields.len != 2) return error.CompilationFailed;
                // Shared layout uses canonical alphabetical field indices.
                // For { problem : Utf8ByteProblem, index : U64 }, that means
                // original index 0 = index and original index 1 = problem.
                const resolved_index_offset = ls.getStructFieldOffsetByOriginalIndex(record_idx, 0);
                const resolved_problem_offset = ls.getStructFieldOffsetByOriginalIndex(record_idx, 1);

                const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const total_size_val = builder.intValue(.i32, tu_data.size) catch return error.OutOfMemory;
                _ = wip.callMemSet(dest_ptr, alignment, zero_byte, total_size_val, .normal, false) catch return error.CompilationFailed;
                const ok_tag_val = builder.intValue(.i64, @as(u64, @intCast(resolved_ok_index))) catch return error.OutOfMemory;
                const err_tag_val = builder.intValue(.i64, @as(u64, @intCast(resolved_err_index))) catch return error.OutOfMemory;
                const outer_disc_offset_val = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;
                const outer_disc_size_val = builder.intValue(.i32, tu_data.discriminant_size) catch return error.OutOfMemory;
                const err_index_offset_val = builder.intValue(.i32, resolved_index_offset) catch return error.OutOfMemory;
                const err_problem_offset_val = builder.intValue(.i32, resolved_problem_offset) catch return error.OutOfMemory;

                _ = self.callBuiltin("roc_builtins_str_from_utf8_result", .void, &.{
                    ptr_type, ptr_type, .i64, .i64, .i64, .i64, .i32, .i32, .i32, .i32, ptr_type,
                }, &.{
                    dest_ptr,
                    data_ptr,
                    list_len,
                    list_cap,
                    ok_tag_val,
                    err_tag_val,
                    outer_disc_offset_val,
                    outer_disc_size_val,
                    err_index_offset_val,
                    err_problem_offset_val,
                    roc_ops,
                }) catch return error.CompilationFailed;
                return .none;
            },
            .num_from_str => {
                return try self.generateNumFromStr(ll, args);
            },

            .list_append_unsafe => {
                // list_append_unsafe(list, element) -> new_list
                std.debug.assert(args.len >= 2);
                const ls = self.layout_store orelse return error.CompilationFailed;
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;

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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                return try self.generateListSortWith(args[0], ll.callable_proc, ll.ret_layout);
            },

            .list_first => {
                // list_first(list) -> element at index 0
                std.debug.assert(args.len >= 1);
                const ls = self.layout_store orelse return error.CompilationFailed;
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
                const ls = self.layout_store orelse return error.CompilationFailed;
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
                const ls = self.layout_store orelse return error.CompilationFailed;
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
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const ls = self.layout_store orelse return error.CompilationFailed;
                const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

                const ret_layout = ls.getLayout(ll.ret_layout);
                const elem_layout_idx = if (ret_layout.tag == .list) ret_layout.data.list else return error.CompilationFailed;
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
                std.debug.assert(args.len >= 2);
                const list_layout_idx = self.getExprResultLayout(args[0]) orelse return error.CompilationFailed;
                const elem_info = try self.getListElementInfo(list_layout_idx);
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                const needle_size = try self.materializedLayoutSize(elem_info.elem_layout_idx);
                const needle_ptr = try self.materializeAsPtr(args[1], needle_size);
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;
                const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr_val = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
                const list_len = wip.load(.normal, .i64, len_ptr_val, alignment, "") catch return error.CompilationFailed;

                if (elem_info.elem_size == 0) {
                    const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                    return wip.icmp(.ne, list_len, zero_i64, "") catch return error.OutOfMemory;
                }

                const header_block = wip.block(2, "contains_hdr") catch return error.OutOfMemory;
                const body_block = wip.block(1, "contains_body") catch return error.OutOfMemory;
                const continue_block = wip.block(1, "contains_continue") catch return error.OutOfMemory;
                const found_block = wip.block(1, "contains_found") catch return error.OutOfMemory;
                const exit_block = wip.block(2, "contains_exit") catch return error.OutOfMemory;

                const zero_i64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const one_i64 = builder.intValue(.i64, 1) catch return error.OutOfMemory;
                const false_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const true_val = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                const elem_size_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;

                const entry_block = wip.cursor.block;
                _ = wip.br(header_block) catch return error.CompilationFailed;

                wip.cursor = .{ .block = header_block };
                const counter_phi = wip.phi(.i64, "ctr") catch return error.CompilationFailed;
                const ctr_val = counter_phi.toValue();
                const cond = wip.icmp(.ult, ctr_val, list_len, "") catch return error.OutOfMemory;
                _ = wip.brCond(cond, body_block, exit_block, .none) catch return error.CompilationFailed;

                wip.cursor = .{ .block = body_block };
                const byte_offset = wip.bin(.mul, ctr_val, elem_size_val, "") catch return error.CompilationFailed;
                const elem_ptr_val = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;
                const is_equal = try self.comparePtrsByLayout(elem_ptr_val, needle_ptr, elem_info.elem_layout_idx);
                _ = wip.brCond(is_equal, found_block, continue_block, .none) catch return error.CompilationFailed;

                wip.cursor = .{ .block = continue_block };
                const next_ctr = wip.bin(.add, ctr_val, one_i64, "") catch return error.CompilationFailed;
                _ = wip.br(header_block) catch return error.CompilationFailed;

                wip.cursor = .{ .block = found_block };
                _ = wip.br(exit_block) catch return error.CompilationFailed;

                counter_phi.finish(
                    &.{ zero_i64, next_ctr },
                    &.{ entry_block, continue_block },
                    wip,
                );

                wip.cursor = .{ .block = exit_block };
                const result_phi = wip.phi(.i1, "result") catch return error.CompilationFailed;
                result_phi.finish(
                    &.{ false_val, true_val },
                    &.{ header_block, found_block },
                    wip,
                );
                return result_phi.toValue();
            },

            .box_box => {
                // Box.box(value) -> Box(value): heap-allocate and copy value
                std.debug.assert(args.len >= 1);
                const ls = self.layout_store orelse return error.CompilationFailed;
                const ret_layout_data = ls.getLayout(ll.ret_layout);

                if (ret_layout_data.tag == .box_of_zst) {
                    _ = try self.generateExpr(args[0]);
                    return builder.intValue(.i64, 0) catch return error.OutOfMemory;
                }

                const box_info = ls.getBoxInfo(ret_layout_data);
                const elem_size: u32 = box_info.elem_size;
                const elem_align: u32 = box_info.elem_alignment;

                if (elem_size == 0) {
                    _ = try self.generateExpr(args[0]);
                    return builder.intValue(.i64, 0) catch return error.OutOfMemory;
                }

                // Allocate heap memory via allocateWithRefcountC
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const size_val = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
                const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
                const refcounted_val = builder.intValue(.i1, @as(u64, if (box_info.contains_refcounted) 1 else 0)) catch return error.OutOfMemory;
                const heap_ptr = try self.callBuiltin(
                    "roc_builtins_allocate_with_refcount",
                    ptr_type,
                    &.{ .i64, .i32, .i1, ptr_type },
                    &.{ size_val, align_val, refcounted_val, roc_ops },
                );

                // Generate the value and store to heap
                const saved_out_ptr = self.out_ptr;
                self.out_ptr = heap_ptr;
                const value = try self.generateExpr(args[0]);
                self.out_ptr = saved_out_ptr;

                // If the value wasn't stored via out_ptr, store it now
                if (value != .none) {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(@max(elem_align, 1));
                    _ = wip.store(.normal, value, heap_ptr, alignment) catch return error.CompilationFailed;
                }

                return heap_ptr;
            },
            .box_unbox => {
                // Box.unbox(box) -> value: dereference the box pointer
                std.debug.assert(args.len >= 1);
                const ls = self.layout_store orelse return error.CompilationFailed;
                const box_arg_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                const box_layout_data = ls.getLayout(box_arg_layout);

                if (box_layout_data.tag == .box_of_zst) {
                    _ = try self.generateExpr(args[0]);
                    return builder.intValue(.i64, 0) catch return error.OutOfMemory;
                }

                const box_info = ls.getBoxInfo(box_layout_data);
                const elem_size: u32 = box_info.elem_size;

                if (elem_size == 0) {
                    _ = try self.generateExpr(args[0]);
                    return builder.intValue(.i64, 0) catch return error.OutOfMemory;
                }

                // Generate the box pointer
                const box_ptr = try self.generateExpr(args[0]);
                const elem_type = try self.layoutToLlvmTypeFull(box_info.elem_layout_idx);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(@max(box_info.elem_alignment, 1));
                return wip.load(.normal, elem_type, box_ptr, alignment, "") catch return error.CompilationFailed;
            },

            else => {
                std.log.err("LLVM backend missing LowLevel handler for {s}", .{@tagName(ll.op)});
                return error.CompilationFailed;
            },
        }
    }

    fn generateNumFromStr(self: *MonoLlvmCodeGen, ll: anytype, args: []const LirExprId) Error!LlvmBuilder.Value {
        if (args.len != 1) return error.CompilationFailed;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
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
                .u8,
                .u16,
                .u32,
                .u64,
                .u128,
                .i8,
                .i16,
                .i32,
                .i64,
                .i128,
                .f32,
                .f64,
                .dec,
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

    fn beginScope(self: *MonoLlvmCodeGen) Error!ScopeSnapshot {
        return ScopeSnapshot.init(self);
    }

    fn endScope(self: *MonoLlvmCodeGen, scope: *ScopeSnapshot) Error!void {
        var symbol_keys_to_remove: std.ArrayList(u64) = .{};
        defer symbol_keys_to_remove.deinit(self.allocator);

        var symbol_it = self.symbol_values.keyIterator();
        while (symbol_it.next()) |key| {
            if (!scope.symbol_keys.contains(key.*)) {
                try symbol_keys_to_remove.append(self.allocator, key.*);
            }
        }
        for (symbol_keys_to_remove.items) |key| {
            _ = self.symbol_values.remove(key);
        }

        var cell_keys_to_remove: std.ArrayList(u64) = .{};
        defer cell_keys_to_remove.deinit(self.allocator);

        var cell_it = self.cell_allocas.keyIterator();
        while (cell_it.next()) |key| {
            if (!scope.cell_keys.contains(key.*)) {
                try cell_keys_to_remove.append(self.allocator, key.*);
            }
        }
        for (cell_keys_to_remove.items) |key| {
            _ = self.cell_allocas.remove(key);
        }
    }

    /// Generate a checked integer try-conversion returning a Result tag union.
    /// Calls a C wrapper that checks range and writes to a tag union buffer.
    fn generateIntTryConversion(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const args = self.store.getExprSpan(ll.args);
        std.debug.assert(args.len >= 1);

        // Zero the output buffer first (result is {Dec(i128), Bool} = 17 bytes typically)
        const ls = self.layout_store orelse return error.CompilationFailed;
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
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
        const dest_ptr = self.out_ptr orelse return error.CompilationFailed;
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
                const value_type = value.typeOfWip(wip);
                const low = if (value_type == .i64)
                    value
                else if (!isIntType(value_type))
                    return error.CompilationFailed
                else blk2: {
                    const value_bits = intTypeBits(value_type);
                    if (value_bits < 64) {
                        break :blk2 wip.cast(if (is_signed_precision) .sext else .zext, value, .i64, "") catch return error.CompilationFailed;
                    }
                    if (value_bits > 64) {
                        break :blk2 wip.cast(.trunc, value, .i64, "") catch return error.CompilationFailed;
                    }
                    break :blk2 value;
                };
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

    /// Generate str_concat expression: fold-left concatenation of a span of string expressions.
    fn generateStrConcatExpr(self: *MonoLlvmCodeGen, exprs: anytype) Error!LlvmBuilder.Value {
        const expr_ids = self.store.getExprSpan(exprs);
        if (expr_ids.len == 0) {
            // Empty concat returns empty string
            return self.generateEmptyString();
        }
        if (expr_ids.len == 1) {
            return self.generateExpr(expr_ids[0]);
        }

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const saved_out_ptr = self.out_ptr;
        const needs_temp = saved_out_ptr == null;
        if (needs_temp) {
            const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
            self.out_ptr = wip.alloca(.normal, .i64, alloca_count, LlvmBuilder.Alignment.fromByteUnits(8), .default, "str_concat_tmp") catch return error.CompilationFailed;
        }
        const dest_ptr = self.out_ptr.?;

        // First pair writes directly to out_ptr
        _ = try self.callStrStr2Str(expr_ids[0], expr_ids[1], "roc_builtins_str_concat");

        // Subsequent elements: concat accumulated result (at dest_ptr) with next element
        for (expr_ids[2..]) |next_id| {
            self.out_ptr = dest_ptr;

            // Load accumulator fields directly from dest_ptr
            const a_bytes = wip.load(.normal, ptr_type, dest_ptr, alignment, "") catch return error.CompilationFailed;
            const a_len_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
            const a_len = wip.load(.normal, .i64, a_len_ptr, alignment, "") catch return error.CompilationFailed;
            const a_cap_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
            const a_cap = wip.load(.normal, .i64, a_cap_ptr, alignment, "") catch return error.CompilationFailed;

            // Materialize the next string element
            const b_ptr = try self.materializeAsPtr(next_id, 24);
            const b_bytes = wip.load(.normal, ptr_type, b_ptr, alignment, "") catch return error.CompilationFailed;
            const b_len_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off8}, "") catch return error.CompilationFailed;
            const b_len = wip.load(.normal, .i64, b_len_ptr, alignment, "") catch return error.CompilationFailed;
            const b_cap_ptr = wip.gep(.inbounds, .i8, b_ptr, &.{off16}, "") catch return error.CompilationFailed;
            const b_cap = wip.load(.normal, .i64, b_cap_ptr, alignment, "") catch return error.CompilationFailed;

            _ = try self.callBuiltin("roc_builtins_str_concat", .void, &.{
                ptr_type, ptr_type, .i64, .i64, ptr_type, .i64, .i64, ptr_type,
            }, &.{
                dest_ptr, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops,
            });
        }

        self.out_ptr = saved_out_ptr;
        if (needs_temp) {
            const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
            return wip.load(.normal, struct_type, dest_ptr, alignment, "str_concat_val") catch return error.CompilationFailed;
        }
        return .none;
    }

    fn generateEmptyString(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.out_ptr) |dest_ptr| {
            const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
            const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
            const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
            const null_ptr = builder.nullValue(ptr_type) catch return error.OutOfMemory;
            _ = wip.store(.normal, null_ptr, dest_ptr, alignment) catch return error.CompilationFailed;
            const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
            const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
            const len_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
            _ = wip.store(.normal, zero, len_ptr, alignment) catch return error.CompilationFailed;
            const cap_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
            _ = wip.store(.normal, zero, cap_ptr, alignment) catch return error.CompilationFailed;
            return .none;
        }
        // Return zero-initialized struct
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const struct_type = builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.OutOfMemory;
        return builder.nullValue(struct_type) catch return error.OutOfMemory;
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
            .proc_call => |c| c.ret_layout,
            .low_level => |ll| ll.ret_layout,
            .lookup => |l| l.layout_idx,
            .i64_literal => |i| i.layout_idx,
            .f64_literal => .f64,
            .f32_literal => .f32,
            .bool_literal => .bool,
            .i128_literal => |i| i.layout_idx,
            .dec_literal => .dec,
            .str_literal => .str,
            .int_to_str => .str,
            .float_to_str => .str,
            .dec_to_str => .str,
            .str_escape_and_quote => .str,
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
            .block => |block| blk: {
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| {
                            if (self.exprNeverReturns(binding.expr)) break :blk true;
                        },
                        .cell_init, .cell_store => |binding| {
                            if (self.exprNeverReturns(binding.expr)) break :blk true;
                        },
                        .cell_drop => {},
                    }
                }

                break :blk self.exprNeverReturns(block.final_expr);
            },
            .if_then_else => |ite| blk: {
                if (!self.exprNeverReturns(ite.final_else)) break :blk false;
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    if (!self.exprNeverReturns(branch.body)) break :blk false;
                }
                break :blk true;
            },
            .discriminant_switch => |ds| blk: {
                for (self.store.getExprSpan(ds.branches)) |branch_expr_id| {
                    if (!self.exprNeverReturns(branch_expr_id)) break :blk false;
                }
                break :blk true;
            },
            .match_expr => |m| blk: {
                for (self.store.getMatchBranches(m.branches)) |branch| {
                    if (!self.exprNeverReturns(branch.body)) break :blk false;
                }
                break :blk true;
            },
            .dbg => |d| self.exprNeverReturns(d.expr),
            .expect => |e| self.exprNeverReturns(e.body),
            .nominal => |nom| self.exprNeverReturns(nom.backing_expr),
            else => false,
        };
    }

    fn blockHasTerminator(self: *const MonoLlvmCodeGen, block_idx: LlvmBuilder.Function.Block.Index) bool {
        const wip = self.wip orelse return false;
        const block = block_idx.ptrConst(wip);
        if (block.instructions.items.len == 0) return false;

        const last_inst = block.instructions.items[block.instructions.items.len - 1];
        return last_inst.isTerminatorWip(wip);
    }

    fn currentBlockHasTerminator(self: *const MonoLlvmCodeGen) bool {
        const wip = self.wip orelse return false;
        return self.blockHasTerminator(wip.cursor.block);
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
            .while_loop, .for_loop => 0,
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

    fn generateIfThenElse(self: *MonoLlvmCodeGen, ite: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        var env_snapshot = try LexicalEnvSnapshot.init(self);
        defer env_snapshot.deinit();

        // Get the branches
        const branches = self.store.getIfBranches(ite.branches);

        if (branches.len == 0) {
            // No branches, just generate the else
            return self.generateExpr(ite.final_else);
        }

        // For simplicity, handle single branch if-then-else
        const first_branch = branches[0];
        var cond_val = try self.generateExpr(first_branch.cond);
        if (self.currentBlockHasTerminator()) {
            return builder.poisonValue(try self.layoutToLlvmTypeFull(ite.result_layout)) catch return error.OutOfMemory;
        }

        // Ensure condition is i1 for brCond (tag unions may produce i8 for Bool-like types)
        if (cond_val.typeOfWip(wip) != .i1) {
            cond_val = wip.cast(.trunc, cond_val, .i1, "") catch return error.CompilationFailed;
        }

        const then_returns = !self.exprNeverReturns(first_branch.body);
        const else_returns = !self.exprNeverReturns(ite.final_else);
        const merge_incoming: u32 = @intFromBool(then_returns) + @intFromBool(else_returns);
        const has_merge = merge_incoming != 0;

        // Create basic blocks
        // Each of then/else has 1 incoming edge (from the conditional branch),
        // merge has 2 incoming edges (one from then, one from else).
        const then_block = wip.block(1, "then") catch return error.CompilationFailed;
        const else_block = wip.block(1, "else") catch return error.CompilationFailed;
        const merge_block = if (has_merge)
            try wip.block(merge_incoming, "merge")
        else
            LlvmBuilder.Function.Block.Index.entry;

        // Conditional branch
        _ = wip.brCond(cond_val, then_block, else_block, .none) catch return error.CompilationFailed;

        // Then block
        wip.cursor = .{ .block = then_block };
        try env_snapshot.restore(self);
        var then_scope = try self.beginScope();
        defer then_scope.deinit();
        const then_val = try self.generateControlFlowValue(first_branch.body, ite.result_layout);
        const then_exit_block = wip.cursor.block;
        if (then_returns) {
            _ = wip.br(merge_block) catch return error.CompilationFailed;
        }
        try self.endScope(&then_scope);

        // Else block
        wip.cursor = .{ .block = else_block };
        try env_snapshot.restore(self);
        var else_scope = try self.beginScope();
        defer else_scope.deinit();
        const else_val = try self.generateControlFlowValue(ite.final_else, ite.result_layout);
        const else_exit_block = wip.cursor.block;
        if (else_returns) {
            _ = wip.br(merge_block) catch return error.CompilationFailed;
        }
        try self.endScope(&else_scope);

        if (!has_merge) {
            return builder.poisonValue(try self.layoutToLlvmTypeFull(ite.result_layout)) catch return error.OutOfMemory;
        }

        // Merge block with phi
        try env_snapshot.restore(self);
        wip.cursor = .{ .block = merge_block };

        var merge_vals: [2]LlvmBuilder.Value = undefined;
        var merge_blocks: [2]LlvmBuilder.Function.Block.Index = undefined;
        var merge_val_count: usize = 0;

        if (then_returns) {
            merge_vals[merge_val_count] = then_val;
            merge_blocks[merge_val_count] = then_exit_block;
            merge_val_count += 1;
        }

        if (else_returns) {
            merge_vals[merge_val_count] = else_val;
            merge_blocks[merge_val_count] = else_exit_block;
            merge_val_count += 1;
        }

        std.debug.assert(merge_val_count == merge_incoming);

        // Check that all branch values have the same type for the phi node
        const phi_type = merge_vals[0].typeOfWip(wip);
        for (merge_vals[1..merge_val_count]) |merge_val| {
            std.debug.assert(phi_type == merge_val.typeOfWip(wip));
        }

        const phi_inst = wip.phi(phi_type, "") catch return error.CompilationFailed;
        phi_inst.finish(
            merge_vals[0..merge_val_count],
            merge_blocks[0..merge_val_count],
            wip,
        );

        return phi_inst.toValue();
    }

    fn generateBlock(self: *MonoLlvmCodeGen, block_data: anytype) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        var scope = try self.beginScope();
        defer scope.deinit();

        // Save out_ptr — intermediate statements must not write to it.
        // Only the final expression should use out_ptr for direct-store types (strings).
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;

        // Process all statements (let bindings)
        const stmts = self.store.getStmts(block_data.stmts);
        for (stmts) |stmt| {
            switch (stmt) {
                .decl, .mutate => |b| {
                    const val = try self.generateExprAsValue(b.expr);
                    if (self.currentBlockHasTerminator()) {
                        self.out_ptr = saved_out_ptr;
                        try self.endScope(&scope);
                        const block_layout = self.getExprResultLayout(block_data.final_expr) orelse return error.CompilationFailed;
                        return builder.poisonValue(try self.layoutToLlvmTypeFull(block_layout)) catch return error.OutOfMemory;
                    }
                    try self.bindPattern(b.pattern, val.value);
                },
                .cell_init => |cell| {
                    const value = (try self.generateExprAsValue(cell.expr)).value;
                    if (self.currentBlockHasTerminator()) {
                        self.out_ptr = saved_out_ptr;
                        try self.endScope(&scope);
                        const block_layout = self.getExprResultLayout(block_data.final_expr) orelse return error.CompilationFailed;
                        return builder.poisonValue(try self.layoutToLlvmTypeFull(block_layout)) catch return error.OutOfMemory;
                    }
                    try self.initializeCell(cell.cell, cell.layout_idx, value);
                },
                .cell_store => |cell| {
                    const value = (try self.generateExprAsValue(cell.expr)).value;
                    if (self.currentBlockHasTerminator()) {
                        self.out_ptr = saved_out_ptr;
                        try self.endScope(&scope);
                        const block_layout = self.getExprResultLayout(block_data.final_expr) orelse return error.CompilationFailed;
                        return builder.poisonValue(try self.layoutToLlvmTypeFull(block_layout)) catch return error.OutOfMemory;
                    }
                    try self.storeCell(cell.cell, cell.layout_idx, value);
                },
                .cell_drop => |cell| try self.dropCell(cell.cell, cell.layout_idx),
            }
        }

        // Restore out_ptr for the final expression
        self.out_ptr = saved_out_ptr;
        // Generate and return the final expression
        const result = try self.generateExpr(block_data.final_expr);
        try self.endScope(&scope);
        return result;
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

    fn pointerAlignment(self: *MonoLlvmCodeGen) LlvmBuilder.Alignment {
        if (self.layout_store) |ls| {
            return LlvmBuilder.Alignment.fromByteUnits(@intCast(ls.targetUsize().alignment().toByteUnits()));
        }

        return LlvmBuilder.Alignment.fromByteUnits(@intCast(@divExact(self.target.ptrBitWidth(), 8)));
    }

    fn fieldAlignmentForLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) LlvmBuilder.Alignment {
        if (layout_idx == .str) {
            return LlvmBuilder.Alignment.fromByteUnits(8);
        }

        if (self.layout_store) |ls| {
            const stored_layout = ls.getLayout(layout_idx);
            return switch (stored_layout.tag) {
                .tag_union => switch (self.tagUnionValueMode(layout_idx) catch unreachable) {
                    .scalar_discriminant => self.alignmentForLayout(layout_idx),
                    .transparent_payload => blk: {
                        const payload_layout = self.tagUnionTransparentPayloadLayout(layout_idx) catch unreachable;
                        break :blk self.fieldAlignmentForLayout(payload_layout);
                    },
                    .indirect_pointer => self.pointerAlignment(),
                },
                .box => self.pointerAlignment(),
                .scalar,
                .zst,
                .closure,
                .struct_,
                .list,
                .list_of_zst,
                .box_of_zst,
                => self.alignmentForLayout(layout_idx),
            };
        }

        return self.alignmentForLayout(layout_idx);
    }

    fn initializeCell(self: *MonoLlvmCodeGen, cell: Symbol, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const key: u64 = @bitCast(cell);
        const cell_type = try self.layoutToLlvmTypeFull(layout_idx);
        const normalized = try self.coerceValueToLayout(value, layout_idx);
        const alignment = self.alignmentForLayout(layout_idx);
        const alloca_ptr = wip.alloca(.normal, cell_type, .none, alignment, .default, "cell") catch return error.CompilationFailed;
        _ = wip.store(.normal, normalized, alloca_ptr, alignment) catch return error.CompilationFailed;
        self.cell_allocas.put(key, .{ .alloca_ptr = alloca_ptr, .elem_type = cell_type, .alignment = alignment }) catch return error.OutOfMemory;
        self.symbol_values.put(key, normalized) catch return error.OutOfMemory;
    }

    fn storeCell(self: *MonoLlvmCodeGen, cell: Symbol, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const key: u64 = @bitCast(cell);
        const cell_alloca = self.cell_allocas.get(key) orelse {
            // Mutable locals can legitimately reach their first write before this backend
            // has materialized the backing alloca in the current function state. A lazy
            // initialization is equivalent to an explicit cell_init for that first write.
            try self.initializeCell(cell, layout_idx, value);
            return;
        };
        const normalized = try self.coerceValueToLayout(value, layout_idx);
        try self.storeStoredAlloca(cell_alloca, normalized);
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

        const key: u64 = @intFromEnum(call.proc);
        if (self.proc_registry.get(key)) |func_index| {
            return self.generateCallToCompiledProc(func_index, call.args, call.ret_layout);
        }
        return error.CompilationFailed;
    }

    fn loadStoredAlloca(self: *MonoLlvmCodeGen, stored: LoopVarAlloca) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, stored.elem_type, stored.alloca_ptr, stored.alignment, "") catch return error.CompilationFailed;
    }

    fn storeStoredAlloca(self: *MonoLlvmCodeGen, stored: LoopVarAlloca, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.store(.normal, value, stored.alloca_ptr, stored.alignment) catch return error.CompilationFailed;
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

    /// Extract layout index from a pattern (for parameter typing).
    fn getPatternLayoutIdx(self: *MonoLlvmCodeGen, pattern_id: LirPatternId) ?layout.Idx {
        const pattern = self.store.getPattern(pattern_id);
        return switch (pattern) {
            .bind => |b| b.layout_idx,
            .wildcard => |w| w.layout_idx,
            .tag => |t| t.union_layout,
            .struct_ => |s| s.struct_layout,
            .list => |l| l.list_layout,
            .as_pattern => |a| a.layout_idx,
            .int_literal, .float_literal, .str_literal => null,
        };
    }

    // Pattern binding helpers

    /// Load a value from a pointer based on layout type.
    /// `materialized_value` pointers reference the layout's full in-memory bytes.
    /// `field_slot` pointers reference the ABI slot used for stored fields and call buffers.
    fn loadValueFromStoragePtr(
        self: *MonoLlvmCodeGen,
        ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
        storage: PointerValueStorage,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        if (layout_idx == .bool) {
            const raw_bool = wip.load(.normal, .i8, ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.CompilationFailed;
            return wip.cast(.trunc, raw_bool, .i1, "") catch return error.CompilationFailed;
        }

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
                .tag_union => switch (try self.tagUnionValueMode(layout_idx)) {
                    .scalar_discriminant => {
                        const llvm_type = try self.layoutToStructFieldType(layout_idx);
                        return wip.load(.normal, llvm_type, ptr, self.alignmentForLayout(layout_idx), "") catch return error.CompilationFailed;
                    },
                    .transparent_payload => {
                        const payload_layout = try self.tagUnionTransparentPayloadLayout(layout_idx);
                        return self.loadValueFromStoragePtr(ptr, payload_layout, storage);
                    },
                    .indirect_pointer => {
                        return switch (storage) {
                            .materialized_value => ptr,
                            .field_slot => blk: {
                                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                                break :blk wip.load(.normal, ptr_type, ptr, self.pointerAlignment(), "") catch return error.CompilationFailed;
                            },
                        };
                    },
                },
                .box => {
                    const llvm_type = try self.layoutToStructFieldType(layout_idx);
                    return wip.load(.normal, llvm_type, ptr, self.fieldAlignmentForLayout(layout_idx), "") catch return error.CompilationFailed;
                },
                .scalar, .zst, .closure, .box_of_zst => {},
            }
        }

        // Scalar types
        const elem_type = layoutToLlvmType(layout_idx);
        return wip.load(.normal, elem_type, ptr, self.alignmentForLayout(layout_idx), "") catch return error.CompilationFailed;
    }

    fn loadValueFromPtr(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        return self.loadValueFromStoragePtr(ptr, layout_idx, .materialized_value);
    }

    fn loadFieldValueFromPtr(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        return self.loadValueFromStoragePtr(ptr, layout_idx, .field_slot);
    }

    const ListElementInfo = struct {
        elem_layout_idx: layout.Idx,
        elem_size: u64,
        elem_align: u32,
        elements_refcounted: bool,
    };

    fn getListElementInfo(self: *MonoLlvmCodeGen, list_layout_idx: layout.Idx) Error!ListElementInfo {
        const ls = self.layout_store orelse return error.CompilationFailed;
        const list_layout = ls.getLayout(list_layout_idx);
        const elem_layout_idx = switch (list_layout.tag) {
            .list => list_layout.data.list,
            .list_of_zst => layout.Idx.zst,
            .scalar, .zst, .closure, .struct_, .tag_union, .box, .box_of_zst => return error.CompilationFailed,
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

    fn generateListSortWith(self: *MonoLlvmCodeGen, list_expr_id: LirExprId, callable_proc: lir.LIR.LirProcSpecId, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
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

        if (callable_proc.isNone()) return error.CompilationFailed;

        const proc_key: u64 = @intFromEnum(callable_proc);
        const func_idx = self.proc_registry.get(proc_key) orelse return error.CompilationFailed;
        const thunk_idx = try self.compileListSortComparatorThunk(callable_proc, func_idx, elem_info);
        var fn_ptr = thunk_idx.toValue(builder);
        if (fn_ptr.typeOfWip(wip) != ptr_type) {
            fn_ptr = wip.cast(.bitcast, fn_ptr, ptr_type, "") catch return error.CompilationFailed;
        }

        const align_val = builder.intValue(.i32, elem_info.elem_align) catch return error.OutOfMemory;
        const width_val = builder.intValue(.i64, elem_info.elem_size) catch return error.OutOfMemory;
        const elements_refcounted_val = builder.intValue(.i1, @intFromBool(elem_info.elements_refcounted)) catch return error.OutOfMemory;

        const resolver = layout.RcHelperResolver.init(self.layout_store orelse return error.CompilationFailed);
        var element_incref_fn = builder.nullValue(ptr_type) catch return error.OutOfMemory;
        var element_decref_fn = builder.nullValue(ptr_type) catch return error.OutOfMemory;
        if (elem_info.elements_refcounted) {
            const incref_key = layout.RcHelperKey{
                .op = .incref,
                .layout_idx = elem_info.elem_layout_idx,
            };
            const decref_key = layout.RcHelperKey{
                .op = .decref,
                .layout_idx = elem_info.elem_layout_idx,
            };

            if (resolver.plan(incref_key) == .noop or resolver.plan(decref_key) == .noop) {
                return error.CompilationFailed;
            }

            element_incref_fn = (try self.compileRcHelper(incref_key, .materialized_value)).toValue(builder);
            if (element_incref_fn.typeOfWip(wip) != ptr_type) {
                element_incref_fn = wip.cast(.bitcast, element_incref_fn, ptr_type, "") catch return error.CompilationFailed;
            }

            element_decref_fn = (try self.compileRcHelper(decref_key, .materialized_value)).toValue(builder);
            if (element_decref_fn.typeOfWip(wip) != ptr_type) {
                element_decref_fn = wip.cast(.bitcast, element_decref_fn, ptr_type, "") catch return error.CompilationFailed;
            }
        }

        _ = try self.callBuiltin("roc_builtins_list_sort_with_rc", .void, &.{
            ptr_type, ptr_type, .i64, .i64, ptr_type, ptr_type, .i32, .i64, .i1, ptr_type, ptr_type, ptr_type,
        }, &.{
            dest_ptr,
            list_bytes,
            list_len,
            list_cap,
            fn_ptr,
            roc_ops,
            align_val,
            width_val,
            elements_refcounted_val,
            element_incref_fn,
            element_decref_fn,
            roc_ops,
        });

        return .none;
    }

    fn compileListSortComparatorThunk(
        self: *MonoLlvmCodeGen,
        callable_proc_id: lir.LIR.LirProcSpecId,
        func_idx: LlvmBuilder.Function.Index,
        elem_info: ListElementInfo,
    ) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        const proc = self.store.getProcSpec(callable_proc_id);

        const fn_type = builder.fnType(.i8, &.{ ptr_type, ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
        var name_buf: [96]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_sort_cmp_{d}_{d}", .{
            @intFromEnum(callable_proc_id),
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

        const outer_fn_state = self.swapInFreshFunctionState();
        defer self.restoreFunctionState(outer_fn_state);

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

        // Load element values from pointers using the proc's arg layouts
        const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
        if (arg_layouts.len < 2) return error.CompilationFailed;

        const elem_alignment = LlvmBuilder.Alignment.fromByteUnits(@max(@as(u64, elem_info.elem_align), 1));
        const param0_type = try self.layoutToLlvmTypeFull(arg_layouts[0]);
        const param1_type = try self.layoutToLlvmTypeFull(arg_layouts[1]);
        const lhs = thunk_wip.load(.normal, param0_type, a_ptr, elem_alignment, "") catch return error.CompilationFailed;
        const rhs = thunk_wip.load(.normal, param1_type, b_ptr, elem_alignment, "") catch return error.CompilationFailed;

        // Call the compiled comparator proc
        const callee_fn_type = func_idx.typeOf(builder);
        const callee = func_idx.toValue(builder);
        const cmp_result = thunk_wip.call(.normal, .ccc, .none, callee_fn_type, callee, &.{ lhs, rhs, cmp_data }, "") catch return error.CompilationFailed;
        const coerced = try self.coerceValueToType(cmp_result, .i8, proc.ret_layout);
        _ = thunk_wip.ret(coerced) catch return error.CompilationFailed;
        thunk_wip.finish() catch return error.CompilationFailed;

        return thunk_fn;
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

    /// Build a list rest-pattern value using the regular list_sublist builtin so the
    /// resulting RocList has correct ownership and refcount behavior.
    fn buildRestSublist(self: *MonoLlvmCodeGen, list_value: LlvmBuilder.Value, list_layout_idx: layout.Idx, prefix_len: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const list_ptr = try self.materializeGeneratedValueToPtr(list_value, list_layout_idx);
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{off8}, "") catch return error.CompilationFailed;
        const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;
        const rest_len = wip.bin(.sub, list_len, prefix_len, "") catch return error.CompilationFailed;
        const temp_size = try self.materializedLayoutSize(list_layout_idx);
        const byte_array_type = builder.arrayType(temp_size, .i8) catch return error.OutOfMemory;
        const temp_ptr = wip.alloca(.normal, byte_array_type, .none, alignment, .default, "rest_sublist") catch return error.CompilationFailed;
        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const size_val = builder.intValue(.i32, temp_size) catch return error.OutOfMemory;
        _ = wip.callMemSet(temp_ptr, alignment, zero_byte, size_val, .normal, false) catch return error.CompilationFailed;

        const saved_out_ptr = self.out_ptr;
        self.out_ptr = temp_ptr;
        defer self.out_ptr = saved_out_ptr;

        _ = try self.callListSublistFromPtr(list_ptr, prefix_len, rest_len, .{ .ret_layout = list_layout_idx });
        return try self.loadValueFromPtr(temp_ptr, list_layout_idx);
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
                    try self.storeStoredAlloca(lva, value);
                }
            },
            .wildcard => {},
            .as_pattern => |as_pat| {
                const key: u64 = @bitCast(as_pat.symbol);
                self.symbol_values.put(key, value) catch return error.OutOfMemory;

                if (self.loop_var_allocas.get(key)) |lva| {
                    try self.storeStoredAlloca(lva, value);
                }

                if (!as_pat.inner.isNone()) {
                    try self.bindPattern(as_pat.inner, value);
                }
            },
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
                const ls = self.layout_store orelse return error.CompilationFailed;

                // Guard: value must be a {ptr, i64, i64} struct
                if (value == .none or !value.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;

                const prefix_patterns = self.store.getPatternSpan(list_pat.prefix);

                // The value is a {ptr, i64, i64} struct. Extract the data pointer.
                const data_ptr = wip.extractValue(value, &.{0}, "") catch return error.CompilationFailed;

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
                    const rest_val = try self.buildRestSublist(value, list_pat.list_layout, expected_len);
                    try self.bindPattern(list_pat.rest, rest_val);
                }
            },
            .tag => |tag_pat| {
                try self.bindTagPayloadArgs(tag_pat, value);
            },
            .int_literal, .float_literal, .str_literal => {},
        }
    }

    fn materializeMutablePatternCells(self: *MonoLlvmCodeGen, pattern_id: LirPatternId) Error!void {
        const pattern = self.store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| {
                if (!bind.reassignable) return;
                const key: u64 = @bitCast(bind.symbol);
                if (self.cell_allocas.contains(key)) return;
                const current_value = self.symbol_values.get(key) orelse return error.CompilationFailed;
                try self.initializeCell(bind.symbol, bind.layout_idx, current_value);
            },
            .as_pattern => |as_pat| {
                if (as_pat.reassignable) {
                    const key: u64 = @bitCast(as_pat.symbol);
                    if (!self.cell_allocas.contains(key)) {
                        const current_value = self.symbol_values.get(key) orelse return error.CompilationFailed;
                        try self.initializeCell(as_pat.symbol, as_pat.layout_idx, current_value);
                    }
                }
                if (!as_pat.inner.isNone()) {
                    try self.materializeMutablePatternCells(as_pat.inner);
                }
            },
            .struct_ => |struct_pat| {
                for (self.store.getPatternSpan(struct_pat.fields)) |field_pat| {
                    try self.materializeMutablePatternCells(field_pat);
                }
            },
            .list => |list_pat| {
                for (self.store.getPatternSpan(list_pat.prefix)) |prefix_pat| {
                    try self.materializeMutablePatternCells(prefix_pat);
                }
                if (!list_pat.rest.isNone()) {
                    try self.materializeMutablePatternCells(list_pat.rest);
                }
                for (self.store.getPatternSpan(list_pat.suffix)) |suffix_pat| {
                    try self.materializeMutablePatternCells(suffix_pat);
                }
            },
            .wildcard, .int_literal, .float_literal, .str_literal, .tag => {},
        }
    }
};
