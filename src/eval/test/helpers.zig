//! Shared eval test helpers built on the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const backend = @import("backend");
const collections = @import("collections");
const pipeline = @import("../pipeline.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("RuntimeHostEnv.zig");

const ModuleEnv = can.ModuleEnv;
const RocStr = builtins.str.RocStr;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const layout_mod = @import("layout");
const LayoutStore = layout_mod.Store;
const LayoutIdx = layout_mod.Idx;
const Value = @import("../value.zig").Value;
const LayoutHelper = @import("../value.zig").LayoutHelper;

pub const interpreter_allocator = std.heap.page_allocator;
/// Public enum `TraceMode`.
pub const TraceMode = enum { trace, no_trace };

/// Public struct `ExpectedField`.
pub const ExpectedField = struct {
    name: []const u8,
    value: i128,
};

/// Public struct `ExpectedElement`.
pub const ExpectedElement = struct {
    index: u32,
    value: i128,
};

pub const SourceKind = pipeline.SourceKind;
pub const ModuleSource = pipeline.ModuleSource;
pub const CheckedModule = pipeline.CheckedModule;
pub const ParsedResources = pipeline.ParsedResources;
pub const LoweredProgram = pipeline.LoweredProgram;

/// Public struct `CompiledProgram`.
pub const CompiledProgram = struct {
    resources: ParsedResources,
    lowered: LoweredProgram,
    wasm_lowered: LoweredProgram,

    pub fn deinit(self: *CompiledProgram, allocator: std.mem.Allocator) void {
        self.wasm_lowered.deinit();
        self.lowered.deinit();
        cleanupParseAndCanonical(allocator, self.resources);
    }
};

/// Public value `CompiledInspectedExpr`.
pub const CompiledInspectedExpr = CompiledProgram;

/// Public function `parseAndCanonicalizeExpr`.
pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgram(allocator, .expr, source, &.{});
}

/// Public function `parseAndCanonicalizeInspectedExpr`.
pub fn parseAndCanonicalizeInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, .expr, source, &.{}, true);
}

/// Public function `compileInspectedExpr`.
pub fn compileInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !CompiledInspectedExpr {
    return compileInspectedProgram(allocator, .expr, source, &.{});
}

/// Public function `compileProgram`.
pub fn compileProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !CompiledProgram {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedExprToLir(allocator, &resources);
    errdefer {
        var lowered_mut = lowered;
        lowered_mut.deinit();
    }
    const wasm_lowered = try lowerParsedExprToLirForTarget(allocator, &resources, .u32);
    errdefer {
        var lowered_mut = wasm_lowered;
        lowered_mut.deinit();
    }

    return .{
        .resources = resources,
        .lowered = lowered,
        .wasm_lowered = wasm_lowered,
    };
}

/// Public function `parseAndCanonicalizeProgram`.
pub fn parseAndCanonicalizeProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
}

/// Public function `compileInspectedProgram`.
pub fn compileInspectedProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !CompiledInspectedExpr {
    var resources = try parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, true);
    errdefer cleanupParseAndCanonical(allocator, resources);

    const lowered = try lowerParsedExprToLir(allocator, &resources);
    errdefer {
        var lowered_mut = lowered;
        lowered_mut.deinit();
    }
    const wasm_lowered = try lowerParsedExprToLirForTarget(allocator, &resources, .u32);
    errdefer {
        var lowered_mut = wasm_lowered;
        lowered_mut.deinit();
    }

    return .{
        .resources = resources,
        .lowered = lowered,
        .wasm_lowered = wasm_lowered,
    };
}

/// Public function `cleanupParseAndCanonical`.
pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: ParsedResources) void {
    var mutable = resources;
    mutable.deinit(allocator);
}

/// Public function `lowerParsedExprToLir`.
pub fn lowerParsedExprToLir(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
) !LoweredProgram {
    return pipeline.lowerParsedExprToLir(allocator, resources);
}

/// Public function `lowerParsedExprToLirForTarget`.
pub fn lowerParsedExprToLirForTarget(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
    target_usize: base.target.TargetUsize,
) !LoweredProgram {
    return pipeline.lowerParsedExprToLirForTarget(allocator, resources, target_usize);
}

/// Public function `lowerTypedCIRToLir`.
pub fn lowerTypedCIRToLir(
    allocator: std.mem.Allocator,
    typed_cir_modules: *check.TypedCIR.Modules,
    module_envs: []const *const ModuleEnv,
) !LoweredProgram {
    return pipeline.lowerTypedCIRToLir(allocator, typed_cir_modules, module_envs);
}

/// Public function `mainProcArgLayouts`.
pub fn mainProcArgLayouts(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]layout_mod.Idx {
    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const arg_locals = lowered.lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout_mod.Idx, arg_locals.len);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = lowered.lir_result.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

/// Public function `entrypointParamSlotSize`.
pub fn entrypointParamSlotSize(lowered: *const LoweredProgram, layout_idx: layout_mod.Idx) u32 {
    const layouts = &lowered.lir_result.layouts;
    const runtime_layout_idx = layouts.runtimeRepresentationLayoutIdx(layout_idx);
    if (runtime_layout_idx == .str) return 24;
    if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;

    if (@intFromEnum(runtime_layout_idx) < layouts.layouts.len()) {
        const layout_val = layouts.getLayout(runtime_layout_idx);
        const size = layouts.layoutSizeAlign(layout_val).size;
        if (layout_val.tag == .zst or size == 0) return 0;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
            if (size > 8) return @intCast(std.mem.alignForward(u32, size, 8));
        }
    }

    const size = layouts.layoutSizeAlign(layouts.getLayout(layout_idx)).size;
    return if (size == 0) 0 else 8;
}

/// Public function `zeroedEntrypointArgBuffer`.
pub fn zeroedEntrypointArgBuffer(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
    arg_layouts: []const layout_mod.Idx,
) !?[]u8 {
    const EntrypointArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const arg_offsets = try allocator.alloc(u32, arg_layouts.len);
    defer allocator.free(arg_offsets);
    if (arg_layouts.len != 0) {
        const ordered = try allocator.alloc(EntrypointArgOrder, arg_layouts.len);
        defer allocator.free(ordered);

        for (arg_layouts, 0..) |arg_layout, i| {
            const size_align = lowered.lir_result.layouts.layoutSizeAlign(
                lowered.lir_result.layouts.getLayout(arg_layout),
            );
            const slot_size = entrypointParamSlotSize(lowered, arg_layout);
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(size_align.alignment.toByteUnits()),
                .size = slot_size,
            };
        }

        const SortCtx = struct {
            fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

        var current_offset: u32 = 0;
        for (ordered) |arg| {
            current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
            arg_offsets[arg.index] = current_offset;
            current_offset += arg.size;
        }
    }

    var total_size: usize = 0;
    for (arg_layouts, 0..) |arg_layout, i| {
        total_size = @max(total_size, @as(usize, arg_offsets[i]) + entrypointParamSlotSize(lowered, arg_layout));
    }

    if (total_size == 0) return null;

    const buffer = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(total_size, 1));
    @memset(buffer, 0);
    return buffer;
}

/// Public function `lirInterpreterInspectedStr`.
pub fn lirInterpreterInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = interp.eval(.{
        .proc_id = lowered.main_proc,
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.RuntimeError => {
            if (interp.getRuntimeErrorMessage()) |msg| {
                std.debug.print("eval interpreter runtime error: {s}\n", .{msg});
            }
            return error.Crash;
        },
        error.Crash => {
            if (interp.getCrashMessage()) |msg| {
                std.debug.print("eval interpreter crash: {s}\n", .{msg});
            }
            return error.Crash;
        },
        else => return err,
    };
    const ret_layout = lowered.lir_result.store.getProcSpec(lowered.main_proc).ret_layout;
    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        result.value.ptr,
        null,
    );
}

/// Public function `devEvaluatorInspectedStr`.
pub fn devEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    var codegen = try HostLirCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_test_main",
        lowered.main_proc,
        arg_layouts,
        proc.ret_layout,
    );
    var exec_mem = try ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer exec_mem.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const arg_buffer = try zeroedEntrypointArgBuffer(allocator, lowered, arg_layouts);
    defer if (arg_buffer) |buf| allocator.free(buf);

    const ret_layout = proc.ret_layout;
    const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(ret_layout));
    const alloc_len = @max(size_align.size, 1);
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    var crash_boundary = runtime_env.enterCrashBoundary();
    defer crash_boundary.deinit();
    const sj = crash_boundary.set();
    if (sj != 0) {
        switch (runtime_env.crashState()) {
            .crashed => |msg| std.debug.print("eval dev crash: {s}\n", .{msg}),
            .did_not_crash => {},
        }
        return error.Crash;
    }

    exec_mem.callRocABI(
        @ptrCast(runtime_env.get_ops()),
        @ptrCast(ret_buf.ptr),
        if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
    );
    switch (runtime_env.crashState()) {
        .did_not_crash => {},
        .crashed => |msg| {
            std.debug.print("eval dev crash: {s}\n", .{msg});
            return error.Crash;
        },
    }

    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        ret_buf.ptr,
        runtime_env.get_ops(),
    );
}

/// Public function `wasmEvaluatorInspectedStr`.
pub fn wasmEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    return @import("../wasm_evaluator.zig").evalLoweredToStr(allocator, lowered);
}

test "dev evaluator repro: F32.to_str" {
    var compiled = try compileInspectedExpr(
        std.testing.allocator,
        \\{
        \\a : F32
        \\a = 3.14.F32
        \\x = F32.to_str(a)
        \\x
        \\}
    );
    defer compiled.deinit(std.testing.allocator);

    const actual = try devEvaluatorInspectedStr(std.testing.allocator, &compiled.lowered);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings("\"3.140000104904175\"", actual);
}

fn valueToRocStr(val: Value) RocStr {
    var roc_str: RocStr = undefined;
    @memcpy(std.mem.asBytes(&roc_str), val.ptr[0..@sizeOf(RocStr)]);
    return roc_str;
}

fn valueToRocList(val: Value) builtins.list.RocList {
    var rl: builtins.list.RocList = undefined;
    @memcpy(std.mem.asBytes(&rl), val.ptr[0..@sizeOf(builtins.list.RocList)]);
    return rl;
}

fn readScalarI128(val: Value, layout_val: layout_mod.Layout) !i128 {
    if (layout_val.tag != .scalar) return error.NotScalar;
    if (layout_val.data.scalar.tag != .int) return error.NotInt;

    return switch (layout_val.data.scalar.data.int) {
        .u8 => @as(i128, @intCast(val.read(u8))),
        .u16 => @as(i128, @intCast(val.read(u16))),
        .u32 => @as(i128, @intCast(val.read(u32))),
        .u64 => @as(i128, @intCast(val.read(u64))),
        .u128 => @as(i128, @intCast(val.read(u128))),
        .i8 => @as(i128, @intCast(val.read(i8))),
        .i16 => @as(i128, @intCast(val.read(i16))),
        .i32 => @as(i128, @intCast(val.read(i32))),
        .i64 => @as(i128, @intCast(val.read(i64))),
        .i128 => val.read(i128),
    };
}

fn readDecI128(val: Value, layout_val: layout_mod.Layout) !i128 {
    if (layout_val.tag != .scalar or layout_val.data.scalar.tag != .frac) return error.NotDec;
    if (layout_val.data.scalar.data.frac != .dec) return error.NotDec;
    const roc_dec = val.read(builtins.dec.RocDec);
    return roc_dec.num;
}

fn readNumericI128(val: Value, layout_val: layout_mod.Layout) !i128 {
    if (layout_val.tag == .scalar and layout_val.data.scalar.tag == .int) {
        return readScalarI128(val, layout_val);
    }
    if (layout_val.tag == .scalar and layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .dec) {
        const roc_dec = val.read(builtins.dec.RocDec);
        return @divTrunc(roc_dec.num, builtins.dec.RocDec.one_point_zero_i128);
    }
    return error.NotNumeric;
}

fn recordFieldSemanticIndex(
    allocator: std.mem.Allocator,
    module_env: *const ModuleEnv,
    record_var: types.Var,
    field_name: []const u8,
) !u16 {
    const types_store = &module_env.types;
    const ident_store = module_env.getIdentStoreConst();
    var gathered = std.ArrayList(types.RecordField).empty;
    defer gathered.deinit(allocator);
    var seen = std.AutoHashMap(base.Ident.Idx, void).init(allocator);
    defer seen.deinit();

    var current_var = record_var;
    var guard = types.debug.IterationGuard.init("recordFieldSemanticIndex");
    while (true) {
        guard.tick();
        const resolved = types_store.resolveVar(current_var);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current_var = types_store.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat| switch (flat) {
                .record => |record| {
                    const fields_slice = types_store.getRecordFieldsSlice(record.fields);
                    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
                        if (seen.contains(name)) continue;
                        try seen.put(name, {});
                        try gathered.append(allocator, .{ .name = name, .var_ = var_ });
                    }
                    current_var = record.ext;
                    continue;
                },
                .record_unbound => |fields| {
                    const fields_slice = types_store.getRecordFieldsSlice(fields);
                    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
                        if (seen.contains(name)) continue;
                        try seen.put(name, {});
                        try gathered.append(allocator, .{ .name = name, .var_ = var_ });
                    }
                    break;
                },
                .empty_record => break,
                .nominal_type => |nominal| {
                    current_var = types_store.getNominalBackingVar(nominal);
                    continue;
                },
                else => return error.NotRecord,
            },
            else => return error.NotRecord,
        }
        break;
    }

    std.mem.sort(types.RecordField, gathered.items, ident_store, types.RecordField.sortByNameAsc);
    for (gathered.items, 0..) |field, i| {
        if (std.mem.eql(u8, ident_store.getText(field.name), field_name)) {
            return @intCast(i);
        }
    }

    return error.MissingRecordField;
}

/// Public struct `EvalState`.
pub const EvalState = struct {
    compiled: *CompiledProgram,
    runtime_env: *RuntimeHostEnv,
    interp: ?Interpreter,
    value: Value,
    ret_layout: LayoutIdx,

    fn interpPtr(self: *EvalState) *Interpreter {
        if (self.interp) |*interp| return interp;
        unreachable;
    }

    pub fn deinit(self: *EvalState, allocator: std.mem.Allocator) void {
        if (self.interp) |*interp| {
            interp.deinit();
        }
        self.runtime_env.deinit();
        allocator.destroy(self.runtime_env);
        self.compiled.deinit(allocator);
        allocator.destroy(self.compiled);
    }
};

/// Public function `evalExprToValue`.
pub fn evalExprToValue(
    allocator: std.mem.Allocator,
    src: []const u8,
) !EvalState {
    const compiled_ptr = try allocator.create(CompiledProgram);
    var compiled_inited = false;
    errdefer {
        if (compiled_inited) compiled_ptr.deinit(allocator);
        allocator.destroy(compiled_ptr);
    }
    compiled_ptr.* = try compileProgram(allocator, .expr, src, &.{});
    compiled_inited = true;

    const runtime_env_ptr = try allocator.create(RuntimeHostEnv);
    var runtime_env_inited = false;
    errdefer {
        if (runtime_env_inited) runtime_env_ptr.deinit();
        allocator.destroy(runtime_env_ptr);
    }
    runtime_env_ptr.* = RuntimeHostEnv.init(allocator);
    runtime_env_inited = true;

    var eval_state: EvalState = .{
        .compiled = compiled_ptr,
        .runtime_env = runtime_env_ptr,
        .interp = null,
        .value = undefined,
        .ret_layout = undefined,
    };
    errdefer eval_state.deinit(allocator);
    compiled_inited = false;
    runtime_env_inited = false;

    const interp = try Interpreter.init(
        allocator,
        &eval_state.compiled.lowered.lir_result.store,
        &eval_state.compiled.lowered.lir_result.layouts,
        eval_state.runtime_env.get_ops(),
    );
    eval_state.interp = interp;

    const arg_layouts = try mainProcArgLayouts(allocator, &eval_state.compiled.lowered);
    defer allocator.free(arg_layouts);

    const result = eval_state.interp.?.eval(.{
        .proc_id = eval_state.compiled.lowered.main_proc,
        .arg_layouts = arg_layouts,
    }) catch |err| {
        return err;
    };
    const ret_layout = eval_state.compiled.lowered.lir_result.store.getProcSpec(eval_state.compiled.lowered.main_proc).ret_layout;

    eval_state.value = result.value;
    eval_state.ret_layout = ret_layout;

    return eval_state;
}

/// Public function `evalLoweredNumericI128`.
pub fn evalLoweredNumericI128(allocator: std.mem.Allocator, lowered: *const LoweredProgram) !i128 {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = try interp.eval(.{
        .proc_id = lowered.main_proc,
        .arg_layouts = arg_layouts,
    });
    const ret_layout = lowered.lir_result.store.getProcSpec(lowered.main_proc).ret_layout;
    const layout_val = lowered.lir_result.layouts.getLayout(ret_layout);
    const int_value = try readNumericI128(result.value, layout_val);
    interp.dropValue(result.value, ret_layout);
    return int_value;
}

/// Public function `runExpectI64`.
pub fn runExpectI64(src: []const u8, expected_int: i128, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_val = eval_state.compiled.lowered.lir_result.layouts.getLayout(eval_state.ret_layout);
    const int_value = try readNumericI128(eval_state.value, layout_val);
    try std.testing.expectEqual(expected_int, int_value);
}

/// Public function `runExpectSuccess`.
pub fn runExpectSuccess(src: []const u8, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    try std.testing.expect(eval_state.runtime_env.terminationState() == .returned);
}

/// Public function `runExpectIntDec`.
pub fn runExpectIntDec(src: []const u8, expected_int: i128, should_trace: TraceMode) !void {
    return runExpectI64(src, expected_int, should_trace);
}

/// Public function `runExpectDec`.
pub fn runExpectDec(src: []const u8, expected_dec: i128, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_val = eval_state.compiled.lowered.lir_result.layouts.getLayout(eval_state.ret_layout);
    const dec_value = try readDecI128(eval_state.value, layout_val);
    try std.testing.expectEqual(expected_dec, dec_value);
}

/// Public function `runExpectBool`.
pub fn runExpectBool(src: []const u8, expected_bool: bool, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_store = &eval_state.compiled.lowered.lir_result.layouts;
    const layout_val = layout_store.getLayout(eval_state.ret_layout);
    if (layout_val.tag != .tag_union) return error.NotBool;

    const helper = LayoutHelper.init(layout_store);
    const disc = helper.readTagDiscriminant(eval_state.value, eval_state.ret_layout);
    const actual_bool = disc != 0;

    try std.testing.expectEqual(expected_bool, actual_bool);
}

/// Public function `runExpectError`.
pub fn runExpectError(src: []const u8, expected_error: anyerror, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var compiled = try compileProgram(interpreter_allocator, .expr, src, &.{});
    defer compiled.deinit(interpreter_allocator);

    var runtime_env = RuntimeHostEnv.init(interpreter_allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        interpreter_allocator,
        &compiled.lowered.lir_result.store,
        &compiled.lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(interpreter_allocator, &compiled.lowered);
    defer interpreter_allocator.free(arg_layouts);

    _ = interp.eval(.{
        .proc_id = compiled.lowered.main_proc,
        .arg_layouts = arg_layouts,
    }) catch |err| {
        try std.testing.expectEqual(expected_error, err);
        return;
    };

    try std.testing.expect(false);
}

/// Public function `runExpectStr`.
pub fn runExpectStr(src: []const u8, expected_str: []const u8, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_val = eval_state.compiled.lowered.lir_result.layouts.getLayout(eval_state.ret_layout);
    if (layout_val.tag != .scalar or layout_val.data.scalar.tag != .str) return error.NotStr;

    const roc_str = valueToRocStr(eval_state.value);
    const copied = try interpreter_allocator.dupe(u8, roc_str.asSlice());
    defer interpreter_allocator.free(copied);

    try std.testing.expectEqualStrings(expected_str, copied);
}

/// Public function `runDevOnlyExpectStr`.
pub fn runDevOnlyExpectStr(src: []const u8, expected_str: []const u8) !void {
    var compiled = try compileInspectedExpr(interpreter_allocator, src);
    defer compiled.deinit(interpreter_allocator);

    const actual = try lirInterpreterInspectedStr(interpreter_allocator, &compiled.lowered);
    defer interpreter_allocator.free(actual);

    try std.testing.expectEqualStrings(expected_str, actual);
}

/// Public function `runExpectTuple`.
pub fn runExpectTuple(src: []const u8, expected_elements: []const ExpectedElement, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_store = &eval_state.compiled.lowered.lir_result.layouts;
    const layout_val = layout_store.getLayout(eval_state.ret_layout);
    if (layout_val.tag != .struct_) return error.NotTuple;

    const struct_idx = layout_val.data.struct_.idx;
    const struct_data = layout_store.getStructData(struct_idx);
    const sorted_fields = layout_store.struct_fields.sliceRange(struct_data.getFields());

    try std.testing.expectEqual(expected_elements.len, sorted_fields.len);

    for (expected_elements) |expected| {
        var found = false;
        for (0..sorted_fields.len) |i| {
            const field = sorted_fields.get(@intCast(i));
            if (field.index != expected.index) continue;
            const offset = layout_store.getStructFieldOffset(struct_idx, @intCast(i));
            const field_ptr = eval_state.value.offset(offset);
            const field_layout = layout_store.getLayout(field.layout);
            const int_val = try readNumericI128(field_ptr, field_layout);
            try std.testing.expectEqual(expected.value, int_val);
            found = true;
            break;
        }
        if (!found) return error.MissingTupleElement;
    }
}

/// Public function `runExpectRecord`.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_store = &eval_state.compiled.lowered.lir_result.layouts;
    const layout_val = layout_store.getLayout(eval_state.ret_layout);
    if (layout_val.tag != .struct_) return error.NotRecord;

    const module_env = eval_state.compiled.resources.module_env;
    const expr_var = ModuleEnv.varFrom(eval_state.compiled.resources.expr_idx);
    const struct_idx = layout_val.data.struct_.idx;
    const struct_data = layout_store.getStructData(struct_idx);
    const sorted_fields = layout_store.struct_fields.sliceRange(struct_data.getFields());

    for (expected_fields) |expected| {
        const semantic_index = try recordFieldSemanticIndex(interpreter_allocator, module_env, expr_var, expected.name);
        var found = false;
        for (0..sorted_fields.len) |i| {
            const field = sorted_fields.get(@intCast(i));
            if (field.index != semantic_index) continue;
            const offset = layout_store.getStructFieldOffset(struct_idx, @intCast(i));
            const field_ptr = eval_state.value.offset(offset);
            const field_layout = layout_store.getLayout(field.layout);
            const int_val = try readNumericI128(field_ptr, field_layout);
            try std.testing.expectEqual(expected.value, int_val);
            found = true;
            break;
        }
        if (!found) return error.MissingRecordField;
    }
}

/// Public function `runExpectListI64`.
pub fn runExpectListI64(src: []const u8, expected_elements: []const i64, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_store = &eval_state.compiled.lowered.lir_result.layouts;
    const resolved_list_layout = layout_store.resolvedListLayoutIdx(eval_state.ret_layout) orelse return error.NotList;
    const list_layout_val = layout_store.getLayout(resolved_list_layout);
    if (list_layout_val.tag != .list) return error.NotList;

    const elem_layout_idx = list_layout_val.data.list;
    const elem_layout = layout_store.getLayout(elem_layout_idx);
    const rl = valueToRocList(eval_state.value);
    try std.testing.expectEqual(expected_elements.len, rl.len());

    const elem_size = layout_store.layoutSize(elem_layout);
    if (expected_elements.len == 0) {
        if (rl.bytes == null) return;
    }
    const bytes = rl.bytes orelse return error.NullListBytes;
    for (expected_elements, 0..) |expected, i| {
        const element_ptr = Value{ .ptr = bytes + i * elem_size };
        const int_val = try readNumericI128(element_ptr, elem_layout);
        try std.testing.expectEqual(@as(i128, expected), int_val);
    }
}

/// Public function `runExpectEmptyListI64`.
pub fn runExpectEmptyListI64(src: []const u8, should_trace: TraceMode) !void {
    return runExpectListI64(src, &.{}, should_trace);
}

/// Public function `runExpectListZst`.
pub fn runExpectListZst(src: []const u8, expected_element_count: usize, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    const layout_store = &eval_state.compiled.lowered.lir_result.layouts;
    const resolved_list_layout = layout_store.resolvedListLayoutIdx(eval_state.ret_layout) orelse return error.NotList;
    const list_layout_val = layout_store.getLayout(resolved_list_layout);
    if (list_layout_val.tag != .list_of_zst and list_layout_val.tag != .list) return error.NotList;

    const rl = valueToRocList(eval_state.value);
    try std.testing.expectEqual(expected_element_count, rl.len());
}

/// Public function `runExpectProblem`.
pub fn runExpectProblem(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(interpreter_allocator, src);
    defer cleanupParseAndCanonical(interpreter_allocator, resources);

    const can_diags_slice = try resources.module_env.getDiagnostics();
    defer interpreter_allocator.free(can_diags_slice);
    const can_diags = can_diags_slice.len;
    const type_problems = resources.checker.problems.problems.items.len;

    try std.testing.expect(can_diags + type_problems > 0);
}

/// Public function `runExpectTypeMismatchAndCrash`.
pub fn runExpectTypeMismatchAndCrash(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(interpreter_allocator, src);
    defer cleanupParseAndCanonical(interpreter_allocator, resources);

    const problems = resources.checker.problems.problems.items;
    var found_dispatch_failure = false;
    for (problems) |problem| {
        if (problem == .type_mismatch or problem == .where_requirement) {
            found_dispatch_failure = true;
            break;
        }
    }
    if (!found_dispatch_failure) {
        const can_diags = try resources.module_env.getDiagnostics();
        defer interpreter_allocator.free(can_diags);
        for (can_diags) |diag| {
            if (diag != .unused_variable) {
                found_dispatch_failure = true;
                break;
            }
        }
        if (!found_dispatch_failure) {
            return error.ExpectedTypeMismatch;
        }
    }

    var compiled = try compileProgram(interpreter_allocator, .expr, src, &.{});
    defer compiled.deinit(interpreter_allocator);

    var runtime_env = RuntimeHostEnv.init(interpreter_allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        interpreter_allocator,
        &compiled.lowered.lir_result.store,
        &compiled.lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(interpreter_allocator, &compiled.lowered);
    defer interpreter_allocator.free(arg_layouts);

    _ = interp.eval(.{
        .proc_id = compiled.lowered.main_proc,
        .arg_layouts = arg_layouts,
    }) catch |err| {
        switch (err) {
            error.Crash, error.RuntimeError => return,
            else => return error.UnexpectedError,
        }
    };

    return error.ExpectedCrash;
}

fn parseAndCanonicalizeProgramWrapped(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
    inspect_wrap: bool,
) !ParsedResources {
    return pipeline.parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, inspect_wrap);
}

fn copyReturnedRocStr(
    allocator: std.mem.Allocator,
    layout_store: *const LayoutStore,
    ret_layout: LayoutIdx,
    value_ptr: [*]u8,
    roc_ops: ?*builtins.host_abi.RocOps,
) ![]u8 {
    const layout_val = layout_store.getLayout(ret_layout);
    const is_str =
        ret_layout == .str or
        (layout_val.tag == .scalar and layout_val.data.scalar.tag == .str);

    if (!is_str) {
        std.debug.panic(
            "eval inspect invariant violated: expected Str return layout, found {s}",
            .{@tagName(layout_val.tag)},
        );
    }

    const roc_str = @as(*align(1) const RocStr, @ptrCast(value_ptr)).*;
    const copied = try allocator.dupe(u8, roc_str.asSlice());
    if (roc_ops) |ops| roc_str.decref(ops);
    return copied;
}
