//! Shared eval test helpers built on the cor-style lowering pipeline.

const std = @import("std");
const builtin = @import("builtin");
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

const Check = check.Check;
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
pub const TraceMode = enum { trace, no_trace };

pub const ExpectedField = struct {
    name: []const u8,
    value: i128,
};

pub const ExpectedElement = struct {
    index: u32,
    value: i128,
};

pub const SourceKind = pipeline.SourceKind;
pub const ModuleSource = pipeline.ModuleSource;
pub const CheckedModule = pipeline.CheckedModule;
pub const ParsedResources = pipeline.ParsedResources;
pub const LoweredProgram = pipeline.LoweredProgram;

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

pub const CompiledInspectedExpr = CompiledProgram;

pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgram(allocator, .expr, source, &.{});
}

pub fn parseAndCanonicalizeInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, .expr, source, &.{}, true);
}

pub fn compileInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !CompiledInspectedExpr {
    return compileInspectedProgram(allocator, .expr, source, &.{});
}

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

pub fn parseAndCanonicalizeProgram(
    allocator: std.mem.Allocator,
    source_kind: SourceKind,
    source: []const u8,
    imports: []const ModuleSource,
) !ParsedResources {
    return parseAndCanonicalizeProgramWrapped(allocator, source_kind, source, imports, false);
}

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

pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: ParsedResources) void {
    var mutable = resources;
    mutable.deinit(allocator);
}

pub fn lowerParsedExprToLir(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
) !LoweredProgram {
    return pipeline.lowerParsedExprToLir(allocator, resources);
}

pub fn lowerParsedExprToLirForTarget(
    allocator: std.mem.Allocator,
    resources: *ParsedResources,
    target_usize: base.target.TargetUsize,
) !LoweredProgram {
    return pipeline.lowerParsedExprToLirForTarget(allocator, resources, target_usize);
}

pub fn lowerTypedCIRToLir(
    allocator: std.mem.Allocator,
    typed_cir_modules: *check.TypedCIR.Modules,
    module_envs: []const *const ModuleEnv,
) !LoweredProgram {
    return pipeline.lowerTypedCIRToLir(allocator, typed_cir_modules, module_envs);
}

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

    const result = interp.eval(.{ .proc_id = lowered.main_proc }) catch |err| switch (err) {
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
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_test_main",
        lowered.main_proc,
        &.{},
        proc.ret_layout,
    );
    var exec_mem = try ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer exec_mem.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

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

    exec_mem.callRocABI(@ptrCast(runtime_env.get_ops()), @ptrCast(ret_buf.ptr), null);

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

pub fn wasmEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    lowered: *const LoweredProgram,
) ![]u8 {
    return @import("../wasm_evaluator.zig").evalLoweredToStr(allocator, lowered);
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

fn readTagUnionDiscriminant(
    helper: LayoutHelper,
    val: Value,
    layout_idx: LayoutIdx,
) u16 {
    return helper.readTagDiscriminant(val, layout_idx);
}

fn resolveTagUnion(
    types_store: *const types.Store,
    initial_var: types.Var,
) ?types.TagUnion {
    var current_var = initial_var;
    var guard = types.debug.IterationGuard.init("resolveTagUnion");
    while (true) {
        guard.tick();
        const resolved = types_store.resolveVar(current_var);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current_var = types_store.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat| switch (flat) {
                .tag_union => |tu| return tu,
                else => return null,
            },
            else => return null,
        }
    }
    return null;
}

fn boolDiscriminantIndex(
    allocator: std.mem.Allocator,
    module_env: *const ModuleEnv,
    tag_union: types.TagUnion,
    target_ident: base.Ident.Idx,
) !u16 {
    const ident_store = module_env.getIdentStoreConst();
    const tags_slice = module_env.types.getTagsSlice(tag_union.tags);
    const count = tags_slice.len;
    var copied = try allocator.alloc(types.Tag, count);
    defer allocator.free(copied);

    for (0..count) |i| {
        copied[i] = tags_slice.get(@intCast(i));
    }

    std.mem.sort(types.Tag, copied, ident_store, types.Tag.sortByNameAsc);

    for (copied, 0..) |tag, i| {
        if (tag.name == target_ident) {
            return @intCast(i);
        }
    }

    return error.MissingTag;
}

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

    const result = eval_state.interp.?.eval(.{ .proc_id = eval_state.compiled.lowered.main_proc }) catch |err| {
        return err;
    };
    const ret_layout = eval_state.compiled.lowered.lir_result.store.getProcSpec(eval_state.compiled.lowered.main_proc).ret_layout;

    eval_state.value = result.value;
    eval_state.ret_layout = ret_layout;

    return eval_state;
}

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

    const result = try interp.eval(.{ .proc_id = lowered.main_proc });
    const ret_layout = lowered.lir_result.store.getProcSpec(lowered.main_proc).ret_layout;
    const layout_val = lowered.lir_result.layouts.getLayout(ret_layout);
    const int_value = try readNumericI128(result.value, layout_val);
    interp.dropValue(result.value, ret_layout);
    return int_value;
}

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

pub fn runExpectSuccess(src: []const u8, should_trace: TraceMode) !void {
    std.mem.doNotOptimizeAway(should_trace);
    var eval_state = try evalExprToValue(interpreter_allocator, src);
    defer {
        eval_state.interpPtr().dropValue(eval_state.value, eval_state.ret_layout);
        eval_state.deinit(interpreter_allocator);
    }

    try std.testing.expect(eval_state.runtime_env.terminationState() == .returned);
}

pub fn runExpectIntDec(src: []const u8, expected_int: i128, should_trace: TraceMode) !void {
    return runExpectI64(src, expected_int, should_trace);
}

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

    _ = interp.eval(.{ .proc_id = compiled.lowered.main_proc }) catch |err| {
        try std.testing.expectEqual(expected_error, err);
        return;
    };

    try std.testing.expect(false);
}

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

pub fn runDevOnlyExpectStr(src: []const u8, expected_str: []const u8) !void {
    var compiled = try compileInspectedExpr(interpreter_allocator, src);
    defer compiled.deinit(interpreter_allocator);

    const actual = try lirInterpreterInspectedStr(interpreter_allocator, &compiled.lowered);
    defer interpreter_allocator.free(actual);

    try std.testing.expectEqualStrings(expected_str, actual);
}

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

pub fn runExpectEmptyListI64(src: []const u8, should_trace: TraceMode) !void {
    return runExpectListI64(src, &.{}, should_trace);
}

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

pub fn runExpectProblem(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(interpreter_allocator, src);
    defer cleanupParseAndCanonical(interpreter_allocator, resources);

    const can_diags_slice = try resources.module_env.getDiagnostics();
    defer interpreter_allocator.free(can_diags_slice);
    const can_diags = can_diags_slice.len;
    const type_problems = resources.checker.problems.problems.items.len;

    try std.testing.expect(can_diags + type_problems > 0);
}

pub fn runExpectTypeMismatchAndCrash(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(interpreter_allocator, src);
    defer cleanupParseAndCanonical(interpreter_allocator, resources);

    const problems = resources.checker.problems.problems.items;
    var found_dispatch_failure = false;
    for (problems) |problem| {
        if (problem == .type_mismatch or problem == .static_dispatch) {
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

    _ = interp.eval(.{ .proc_id = compiled.lowered.main_proc }) catch |err| {
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
