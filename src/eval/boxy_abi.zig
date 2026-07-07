//! C-ABI wrappers over the boxy runtime.
//!
//! Machine-code backends lower boxy LIR statements to calls into these
//! wrappers, sharing descriptor-guided semantics with the LIR interpreter by
//! construction. At this level descriptor and dictionary handles are ordinary
//! pointer-sized values (resolved `*const BoxyTypeDesc` / `*const BoxyDict`),
//! layouts and tag names are their `u32` ids, and results are written through
//! caller-provided out-pointers.
//!
//! One process-global runtime backs every wrapper. Embedders initialize it
//! once before calling entrypoints — either from live stores
//! (`initGlobal`) or from a mapped image's boxy sidecar
//! (`initGlobalFromSidecarView`) — and register a native callee per worker
//! proc for dictionary dispatch (`roc_boxy_register_proc`).
//!
//! Dictionary callee ABI: a registered `BoxyProcFn` receives the fully
//! adapted argument list as an array of value pointers (explicit args first,
//! then hidden descriptor pointers, then nested dictionary pointers, each
//! passed as a pointer to a pointer-sized slot; zero-sized arguments pass
//! null), writes its result bytes through `ret` in the registered return
//! layout, and stores the result's descriptor (or null) through `ret_desc`.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const builtins = @import("builtins");
const lir_value = @import("value.zig");
const boxy_runtime = @import("boxy_runtime.zig");

const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LirProgram = lir.Program;
const RocOps = builtins.host_abi.RocOps;
const Allocator = std.mem.Allocator;
const Value = lir_value.Value;
const BoxyRuntime = boxy_runtime.BoxyRuntime;
const BoxyTables = boxy_runtime.BoxyTables;
const Error = boxy_runtime.Error;
const BoxyTypeDesc = LirProgram.BoxyTypeDesc;
const BoxyDict = LirProgram.BoxyDict;

/// Native callee for one dictionary worker proc.
pub const BoxyProcFn = *const fn (
    args: [*]const ?*const anyopaque,
    arg_count: usize,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void;

/// One explicit argument to `roc_boxy_call_dict`.
pub const RocBoxyCallArg = extern struct {
    value: ?[*]const u8,
    layout: u32,
    desc: ?*const BoxyTypeDesc,
};

const RegisteredProc = struct {
    callee: BoxyProcFn,
    ret_layout: layout_mod.Idx,
};

/// The process-global boxy runtime state behind the C-ABI wrappers.
pub const GlobalBoxyRuntime = struct {
    gpa: Allocator,
    runtime: BoxyRuntime,
    /// Carries the string literal store when the global was initialized from
    /// a sidecar view; the runtime reads tag and field names through it.
    store_shell: LirStore,
    runtime_boxy_type_descs: std.ArrayList(*const BoxyTypeDesc) = .empty,
    runtime_boxy_desc_refs: std.ArrayList(LirProgram.BoxyDescRef) = .empty,
    runtime_boxy_tag_variants: std.ArrayList(LirProgram.BoxyTagVariant) = .empty,
    runtime_boxy_tag_payload_descs: std.ArrayList(LirProgram.BoxyTagPayloadDesc) = .empty,
    runtime_boxy_payload_steps: std.ArrayList(LirProgram.BoxyPayloadStep) = .empty,
    /// Backs runtime-materialized descriptors; they live until deinit.
    desc_arena: std.heap.ArenaAllocator,
    /// Backs value temporaries; reset when the outermost wrapper call
    /// returns.
    value_scratch: std.heap.ArenaAllocator,
    call_depth: usize = 0,
    procs: std.AutoHashMapUnmanaged(u32, RegisteredProc) = .empty,
    /// Local-id to descriptor bindings for the descriptor template being
    /// materialized by the active `roc_boxy_desc_copy` call.
    capture_ids: []const u32 = &.{},
    capture_descs: []const ?*const BoxyTypeDesc = &.{},
};

var global: ?*GlobalBoxyRuntime = null;

fn requireGlobal() *GlobalBoxyRuntime {
    return global orelse @panic("boxy ABI wrapper called before roc_boxy runtime initialization");
}

/// Initialize the process-global boxy runtime from live stores. `store`,
/// `layout_store`, the table slices, and `roc_ops` must outlive the global.
pub fn initGlobal(
    gpa: Allocator,
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    tables: BoxyTables,
    roc_ops: *RocOps,
) error{ OutOfMemory, AlreadyInitialized }!void {
    if (global != null) return error.AlreadyInitialized;
    const g = try gpa.create(GlobalBoxyRuntime);
    errdefer gpa.destroy(g);
    g.* = .{
        .gpa = gpa,
        .store_shell = LirStore.init(gpa),
        .desc_arena = std.heap.ArenaAllocator.init(gpa),
        .value_scratch = std.heap.ArenaAllocator.init(gpa),
        .runtime = .{
            .store = store,
            .layout_store = layout_store,
            .helper = lir_value.LayoutHelper.init(layout_store),
            .boxy_tables = tables,
            .runtime_boxy_type_descs = undefined,
            .runtime_boxy_desc_refs = undefined,
            .runtime_boxy_tag_variants = undefined,
            .runtime_boxy_tag_payload_descs = undefined,
            .runtime_boxy_payload_steps = undefined,
            .roc_ops = roc_ops,
            .scratch = gpa,
            .eval_arena = undefined,
        },
    };
    g.runtime.runtime_boxy_type_descs = &g.runtime_boxy_type_descs;
    g.runtime.runtime_boxy_desc_refs = &g.runtime_boxy_desc_refs;
    g.runtime.runtime_boxy_tag_variants = &g.runtime_boxy_tag_variants;
    g.runtime.runtime_boxy_tag_payload_descs = &g.runtime_boxy_tag_payload_descs;
    g.runtime.runtime_boxy_payload_steps = &g.runtime_boxy_payload_steps;
    g.runtime.eval_arena = g.desc_arena.allocator();
    global = g;
}

/// Initialize the process-global boxy runtime from a mapped boxy sidecar
/// view. The view (and the buffer behind it) and `roc_ops` must outlive the
/// global.
pub fn initGlobalFromSidecarView(
    gpa: Allocator,
    view: *const lir.LirImage.BoxySidecar.View,
    roc_ops: *RocOps,
) error{ OutOfMemory, AlreadyInitialized }!void {
    if (global != null) return error.AlreadyInitialized;
    const tables = BoxyTables{
        .type_descs = view.tables.type_descs,
        .dicts = view.tables.dicts,
        .adapters = view.tables.adapters,
        .desc_refs = view.tables.desc_refs,
        .dict_refs = view.tables.dict_refs,
        .tag_variants = view.tables.tag_variants,
        .tag_payload_descs = view.tables.tag_payload_descs,
        .field_names = view.tables.field_names,
        .adapt_steps = view.tables.adapt_steps,
        .payload_steps = view.tables.payload_steps,
        .method_slots = view.tables.method_slots,
        .method_arg_layouts = view.tables.method_arg_layouts,
        .method_hidden_desc_sources = view.tables.method_hidden_desc_sources,
    };
    try initGlobal(gpa, undefined, &view.layouts, tables, roc_ops);
    const g = global.?;
    g.store_shell.strings = view.strings;
    g.runtime.store = &g.store_shell;
}

/// Tear down the process-global boxy runtime. The embedder owns the stores
/// and buffers the global pointed at.
pub fn deinitGlobal() void {
    const g = global orelse return;
    global = null;
    g.runtime_boxy_payload_steps.deinit(g.gpa);
    g.runtime_boxy_tag_payload_descs.deinit(g.gpa);
    g.runtime_boxy_tag_variants.deinit(g.gpa);
    g.runtime_boxy_desc_refs.deinit(g.gpa);
    g.runtime_boxy_type_descs.deinit(g.gpa);
    g.procs.deinit(g.gpa);
    g.desc_arena.deinit();
    g.value_scratch.deinit();
    g.gpa.destroy(g);
}

/// Engine services for wrapper-initiated boxy operations: descriptor and
/// dictionary handles resolve against the global tables (plus the active
/// desc-copy capture bindings), values allocate from the per-call scratch
/// arena, and RC plans come uncached from the layout store.
const AbiHooks = struct {
    g: *GlobalBoxyRuntime,

    pub fn resolveDescRef(self: AbiHooks, desc_ref: LIR.BoxyDescRef) Error!*const BoxyTypeDesc {
        return switch (desc_ref) {
            .static => |desc_id| self.g.runtime.requireBoxyTypeDesc(desc_id),
            .runtime => |runtime_id| blk: {
                if (runtime_id >= self.g.runtime_boxy_type_descs.items.len) {
                    return error.RuntimeError;
                }
                break :blk self.g.runtime_boxy_type_descs.items[runtime_id];
            },
            .local => |local| blk: {
                for (self.g.capture_ids, self.g.capture_descs) |capture_id, capture_desc| {
                    if (capture_id == @intFromEnum(local)) {
                        break :blk capture_desc orelse return error.RuntimeError;
                    }
                }
                return error.RuntimeError;
            },
        };
    }

    pub fn resolveDictRef(self: AbiHooks, dict_ref: LIR.BoxyDictRef) Error!*const BoxyDict {
        return switch (dict_ref) {
            .static => |dict_id| self.g.runtime.requireBoxyDict(dict_id),
            .local => error.RuntimeError,
        };
    }

    pub fn allocValue(self: AbiHooks, layout_idx: layout_mod.Idx) Error!Value {
        const sa = self.g.runtime.helper.sizeAlignOf(layout_idx);
        if (sa.size == 0) return Value.zst;
        const scratch = self.g.value_scratch.allocator();
        const slice = switch (sa.alignment) {
            .@"1" => scratch.alignedAlloc(u8, .@"1", sa.size),
            .@"2" => scratch.alignedAlloc(u8, .@"2", sa.size),
            .@"4" => scratch.alignedAlloc(u8, .@"4", sa.size),
            .@"8" => scratch.alignedAlloc(u8, .@"8", sa.size),
            .@"16" => scratch.alignedAlloc(u8, .@"16", sa.size),
            _ => unreachable,
        } catch return error.OutOfMemory;
        @memset(slice, 0);
        return Value.fromSlice(slice);
    }

    pub fn allocRocDataWithRc(self: AbiHooks, data_bytes: usize, element_alignment: u32, elements_refcounted: bool) Error![*]u8 {
        return builtins.utils.allocateWithRefcount(data_bytes, element_alignment, elements_refcounted, self.g.runtime.roc_ops);
    }

    pub fn layoutContainsRc(self: AbiHooks, layout_idx: layout_mod.Idx) bool {
        const store = self.g.runtime.layout_store;
        return store.layoutContainsRefcounted(store.getLayout(layout_idx));
    }

    pub fn rcPlanFor(self: AbiHooks, helper: layout_mod.RcHelperKey) layout_mod.RcHelperPlan {
        return self.g.runtime.layout_store.rcHelperPlan(helper);
    }

    pub fn rcStructFieldPlan(self: AbiHooks, struct_plan: layout_mod.RcStructPlan, field_index: u32) ?layout_mod.RcFieldPlan {
        return self.g.runtime.layout_store.rcHelperStructFieldPlan(struct_plan, field_index);
    }

    pub fn rcTagVariantPlan(self: AbiHooks, tag_plan: layout_mod.RcTagUnionPlan, variant_index: u32) ?layout_mod.RcHelperKey {
        return self.g.runtime.layout_store.rcHelperTagUnionVariantPlan(tag_plan, variant_index);
    }

    pub fn traceProcId(_: AbiHooks) u32 {
        return std.math.maxInt(u32);
    }

    pub fn debugDumpProc(_: AbiHooks) void {}
};

fn hooks(g: *GlobalBoxyRuntime) AbiHooks {
    return .{ .g = g };
}

fn enter(g: *GlobalBoxyRuntime) void {
    g.call_depth += 1;
}

fn leave(g: *GlobalBoxyRuntime) void {
    g.call_depth -= 1;
    if (g.call_depth == 0) {
        _ = g.value_scratch.reset(.retain_capacity);
    }
}

fn abiCrash(g: *GlobalBoxyRuntime, comptime what: []const u8) noreturn {
    g.runtime.roc_ops.crash("boxy runtime " ++ what ++ " failed");
    unreachable;
}

fn layoutIdx(raw: u32) layout_mod.Idx {
    return @enumFromInt(raw);
}

fn valueAt(ptr: ?[*]const u8) Value {
    const p = ptr orelse return Value.zst;
    return .{ .ptr = @constCast(p) };
}

fn writeResult(g: *GlobalBoxyRuntime, out: ?[*]u8, result: Value, result_layout: layout_mod.Idx) void {
    const size = g.runtime.helper.sizeOf(result_layout);
    if (size == 0) return;
    const out_ptr = out orelse abiCrash(g, "result write without an out pointer");
    @memcpy(out_ptr[0..size], result.readBytes(size));
}

/// Register the native callee and return layout for one dictionary worker
/// proc. `roc_boxy_call_dict` dispatches slots that reference `proc_id` to
/// `callee`.
pub fn roc_boxy_register_proc(proc_id: u32, callee: BoxyProcFn, ret_layout: u32) callconv(.c) void {
    const g = requireGlobal();
    g.procs.put(g.gpa, proc_id, .{ .callee = callee, .ret_layout = layoutIdx(ret_layout) }) catch abiCrash(g, "proc registration");
}

/// Box a payload into dynamic storage. Writes the boxed value through `out`
/// and the target local's descriptor through `out_desc`.
pub fn roc_boxy_box(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    payload: ?[*]const u8,
    payload_layout: u32,
    source_desc: ?*const BoxyTypeDesc,
    payload_desc: *const BoxyTypeDesc,
    payload_mode: u8,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const boxed = g.runtime.boxyBoxValue(
        hooks(g),
        valueAt(payload),
        layoutIdx(payload_layout),
        source_desc orelse payload_desc,
        payload_desc,
        @enumFromInt(payload_mode),
        layoutIdx(target_layout),
    ) catch abiCrash(g, "box");
    writeResult(g, out, boxed.value, layoutIdx(target_layout));
    out_desc.* = boxed.desc;
}

/// Read a dynamic box's payload back out. Writes the payload value through
/// `out` and the target local's descriptor through `out_desc`.
pub fn roc_boxy_unbox(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    source: ?[*]const u8,
    source_layout: u32,
    source_desc: *const BoxyTypeDesc,
    target_desc: ?*const BoxyTypeDesc,
    target_layout: u32,
    source_mode: u8,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const unboxed = g.runtime.boxyUnboxValue(
        hooks(g),
        valueAt(source),
        layoutIdx(source_layout),
        source_desc,
        target_desc,
        layoutIdx(target_layout),
        @enumFromInt(source_mode),
    ) catch abiCrash(g, "unbox");
    writeResult(g, out, unboxed.value, layoutIdx(target_layout));
    out_desc.* = unboxed.desc;
}

/// Construct a tag value guided by the target descriptor, encoding through
/// the row extension when the tag is not local to the descriptor.
pub fn roc_boxy_tag(
    out: ?[*]u8,
    target_desc: *const BoxyTypeDesc,
    tag_name: u32,
    payload: ?[*]const u8,
    payload_layout: u32,
    payload_desc: ?*const BoxyTypeDesc,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const constructed = g.runtime.constructBoxyTagValue(
        hooks(g),
        target_desc,
        @enumFromInt(tag_name),
        if (payload) |p| Value{ .ptr = @constCast(p) } else null,
        layoutIdx(payload_layout),
        payload_desc,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "tag construction");
    writeResult(g, out, constructed, layoutIdx(target_layout));
}

/// Read one payload of a tag value by tag name. Writes the payload through
/// `out` and the payload's resolved descriptor (or null) through `out_desc`.
pub fn roc_boxy_tag_payload(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    source: ?[*]const u8,
    source_layout: u32,
    source_desc: *const BoxyTypeDesc,
    tag_name: u32,
    payload_index: u32,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const read = g.runtime.readBoxyTagPayloadByName(
        hooks(g),
        valueAt(source),
        layoutIdx(source_layout),
        source_desc,
        @enumFromInt(tag_name),
        payload_index,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "tag payload read");
    writeResult(g, out, read.value, layoutIdx(target_layout));
    out_desc.* = if (read.desc) |desc_ref|
        hooks(g).resolveDescRef(desc_ref) catch abiCrash(g, "tag payload descriptor resolution")
    else
        null;
}

/// Descriptor-guided structural equality.
pub fn roc_boxy_eq(
    lhs: ?[*]const u8,
    rhs: ?[*]const u8,
    value_layout: u32,
    desc: *const BoxyTypeDesc,
) callconv(.c) bool {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return g.runtime.boxyValuesEqual(
        hooks(g),
        valueAt(lhs),
        valueAt(rhs),
        layoutIdx(value_layout),
        desc,
    ) catch abiCrash(g, "equality");
}

/// Render a descriptor-guided inspect string for a boxy value, writing the
/// resulting `RocStr` through `out`.
pub fn roc_boxy_inspect(
    out: ?[*]u8,
    source: ?[*]const u8,
    source_layout: u32,
    desc: *const BoxyTypeDesc,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);

    // Render into the per-call scratch arena so the accumulated bytes are
    // released when the outermost wrapper call returns.
    const scratch = g.value_scratch.allocator();
    const saved_arena = g.runtime.eval_arena;
    g.runtime.eval_arena = scratch;
    defer g.runtime.eval_arena = saved_arena;

    var bytes = std.ArrayList(u8).empty;
    g.runtime.appendBoxyInspect(
        hooks(g),
        &bytes,
        valueAt(source),
        layoutIdx(source_layout),
        desc,
    ) catch abiCrash(g, "inspect");

    const rendered = builtins.str.RocStr.fromSlice(bytes.items, g.runtime.roc_ops);
    const out_ptr = out orelse abiCrash(g, "inspect result write without an out pointer");
    const out_str: *align(1) builtins.str.RocStr = @ptrCast(out_ptr);
    out_str.* = rendered;
}

/// Descriptor-guided refcount operation (`op`: 0 = incref, 1 = decref,
/// 2 = free; `atomicity`: 0 = atomic, 1 = single-thread).
pub fn roc_boxy_drop(
    value: ?[*]u8,
    value_layout: u32,
    desc: ?*const BoxyTypeDesc,
    op: u8,
    count: u16,
    atomicity: u8,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const rc_op: layout_mod.RcOp = @enumFromInt(op);
    const rc_atomicity: builtins.utils.RcAtomicity = @enumFromInt(atomicity);
    const val: Value = if (value) |p| .{ .ptr = p } else Value.zst;
    switch (rc_op) {
        .incref => g.runtime.performConcreteRc(hooks(g), .incref, layoutIdx(value_layout), val, count, rc_atomicity),
        .decref, .free => g.runtime.performBoxyLayoutDrop(
            hooks(g),
            val,
            layoutIdx(value_layout),
            desc,
            rc_op,
            count,
            rc_atomicity,
        ) catch abiCrash(g, "drop"),
    }
}

/// Descriptor-guided tag test, following row extensions.
pub fn roc_boxy_tag_match(
    source: ?[*]const u8,
    source_layout: u32,
    source_desc: *const BoxyTypeDesc,
    tag_name: u32,
) callconv(.c) bool {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return g.runtime.boxyTagMatches(
        hooks(g),
        valueAt(source),
        layoutIdx(source_layout),
        source_desc,
        @enumFromInt(tag_name),
    ) catch abiCrash(g, "tag match");
}

/// Materialize a static descriptor template (id `desc_id`, optionally the
/// nested descriptor at `nested_index`; pass 0xFFFF_FFFF for none) into the
/// runtime descriptor tables. `capture_ids`/`capture_descs` bind the
/// template's local descriptor references. Returns the resolved descriptor.
pub fn roc_boxy_desc_copy(
    desc_id: u32,
    nested_index: u32,
    capture_ids: ?[*]const u32,
    capture_descs: ?[*]const ?*const BoxyTypeDesc,
    capture_count: usize,
) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    g.capture_ids = if (capture_ids) |ids| ids[0..capture_count] else &.{};
    g.capture_descs = if (capture_descs) |descs| descs[0..capture_count] else &.{};
    defer {
        g.capture_ids = &.{};
        g.capture_descs = &.{};
    }
    const desc_ref = LIR.BoxyDescRef{ .static = @enumFromInt(desc_id) };
    const captures = LIR.LocalSpan{ .start = 0, .len = @intCast(capture_count) };
    const desc = if (nested_index == std.math.maxInt(u32))
        g.runtime.materializeBoxyDescRefValueWithCaptures(hooks(g), desc_ref, captures) catch abiCrash(g, "descriptor materialization")
    else
        g.runtime.materializeNestedBoxyDescRefValue(hooks(g), desc_ref, nested_index, captures) catch abiCrash(g, "nested descriptor materialization");
    return desc;
}

/// Resolve a static descriptor id to its descriptor pointer in the global
/// descriptor table.
pub fn roc_boxy_static_desc(desc_id: u32) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    return g.runtime.requireBoxyTypeDesc(@enumFromInt(desc_id));
}

/// Resolve a static dictionary id to its dictionary pointer in the global
/// dictionary table.
pub fn roc_boxy_static_dict(dict_id: u32) callconv(.c) *const BoxyDict {
    const g = requireGlobal();
    return g.runtime.requireBoxyDict(@enumFromInt(dict_id));
}

/// Encode a numeric literal per the descriptor's payload layout and box it
/// into dynamic storage.
pub fn roc_boxy_dynamic_num_literal(
    out: ?[*]u8,
    value: *align(1) const i128,
    desc: *const BoxyTypeDesc,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const literal = g.runtime.boxyDynamicNumLiteral(
        hooks(g),
        value.*,
        desc,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "dynamic numeric literal");
    writeResult(g, out, literal, layoutIdx(target_layout));
}

/// Encode a numeric literal per `desc`'s payload layout, falling back to
/// `default_layout` when the descriptor carries no concrete scalar payload
/// (the binding is erased), and box the result into dynamic storage.
pub fn roc_boxy_dynamic_num_literal_ref(
    out: ?[*]u8,
    value: *align(1) const i128,
    desc: *const BoxyTypeDesc,
    default_layout: u32,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const effective = if (g.runtime.boxyDescHasConcreteScalarPayload(desc))
        desc
    else
        g.runtime.makeRuntimeScalarDesc(layoutIdx(default_layout)) catch abiCrash(g, "dynamic numeric literal descriptor");
    const literal = g.runtime.boxyDynamicNumLiteral(
        hooks(g),
        value.*,
        effective,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "dynamic numeric literal");
    writeResult(g, out, literal, layoutIdx(target_layout));
}

/// Dispatch one dictionary method call: adapt the explicit arguments per the
/// slot's adapter, append hidden descriptors and nested dictionaries, and
/// either run descriptor-guided structural equality (writing a `u8` result)
/// or invoke the registered callee for the slot's worker proc. The result is
/// materialized into `out_layout` guided by the callee's returned descriptor
/// and `result_desc`; the result's descriptor is written through `out_desc`.
pub fn roc_boxy_call_dict(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    dict: *const BoxyDict,
    method_slot: u32,
    args: ?[*]const RocBoxyCallArg,
    args_len: usize,
    hidden_args: ?[*]const usize,
    hidden_len: usize,
    result_desc: ?*const BoxyTypeDesc,
    out_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const scratch = g.value_scratch.allocator();

    const raw_args: []const RocBoxyCallArg = if (args) |a| a[0..args_len] else &.{};
    const call_args = scratch.alloc(boxy_runtime.DictCallArg, raw_args.len) catch abiCrash(g, "dictionary call argument collection");
    for (raw_args, 0..) |raw, i| {
        call_args[i] = .{
            .value = valueAt(raw.value),
            .layout = layoutIdx(raw.layout),
            .source_desc = raw.desc,
        };
    }
    const raw_hidden: []const usize = if (hidden_args) |h| h[0..hidden_len] else &.{};
    const hidden_values = scratch.alloc(Value, raw_hidden.len) catch abiCrash(g, "dictionary call hidden argument collection");
    for (raw_hidden, 0..) |raw, i| {
        const slot = scratch.alloc(usize, 1) catch abiCrash(g, "dictionary call hidden argument collection");
        slot[0] = raw;
        hidden_values[i] = .{ .ptr = @ptrCast(slot.ptr) };
    }

    const prepared = g.runtime.prepareDictCall(
        hooks(g),
        scratch,
        dict,
        method_slot,
        call_args,
        hidden_values,
    ) catch abiCrash(g, "dictionary call preparation");

    switch (prepared) {
        .structural_eq => |operand_desc| {
            const equal = g.runtime.boxyValuesEqual(
                hooks(g),
                call_args[0].value,
                call_args[1].value,
                call_args[0].layout,
                operand_desc,
            ) catch abiCrash(g, "dictionary structural equality");
            const out_ptr = out orelse abiCrash(g, "result write without an out pointer");
            out_ptr[0] = if (equal) 1 else 0;
            out_desc.* = null;
        },
        .call => |call| {
            const registered = g.procs.get(@intFromEnum(call.proc)) orelse
                abiCrash(g, "dictionary dispatch to an unregistered proc");
            const arg_ptrs = scratch.alloc(?*const anyopaque, call.arg_values.len) catch abiCrash(g, "dictionary call argument collection");
            for (call.arg_values, call.arg_layouts, 0..) |arg_value, arg_layout, i| {
                arg_ptrs[i] = if (g.runtime.helper.sizeOf(arg_layout) == 0) null else @ptrCast(arg_value.ptr);
            }
            const ret_size = g.runtime.helper.sizeOf(registered.ret_layout);
            const ret_value = hooks(g).allocValue(registered.ret_layout) catch abiCrash(g, "dictionary call result buffer");
            var ret_desc: ?*const anyopaque = null;
            registered.callee(
                arg_ptrs.ptr,
                arg_ptrs.len,
                if (ret_size == 0) null else @ptrCast(ret_value.ptr),
                &ret_desc,
            );
            const materialized = g.runtime.materializeCallResult(
                hooks(g),
                ret_value,
                registered.ret_layout,
                @ptrCast(@alignCast(ret_desc)),
                result_desc,
                layoutIdx(out_layout),
            ) catch abiCrash(g, "dictionary call result materialization");
            writeResult(g, out, materialized, layoutIdx(out_layout));
            out_desc.* = result_desc orelse @ptrCast(@alignCast(ret_desc));
        },
    }
}
