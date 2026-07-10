//! Shared boxy value runtime.
//!
//! `BoxyRuntime` bundles the descriptor-guided boxy value machinery around the
//! dependencies it needs — a layout store, the boxy descriptor tables, the
//! string store used for tag names, `RocOps`, and a scratch allocator — so the
//! same semantics back both the LIR interpreter and machine-code output.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const builtins = @import("builtins");
const build_options = @import("build_options");
const lir_value = @import("value.zig");

const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LirProgram = lir.Program;
const RocOps = builtins.host_abi.RocOps;
const Allocator = std.mem.Allocator;
const Value = lir_value.Value;
const LayoutHelper = lir_value.LayoutHelper;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RcOp = layout_mod.RcOp;
const RcAtomicity = builtins.utils.RcAtomicity;
const Layout = layout_mod.Layout;

const is_freestanding = builtin.target.os.tag == .freestanding;

/// The error set shared by the boxy runtime and the LIR interpreter.
pub const Error = error{
    OutOfMemory,
    RuntimeError,
    ComptimeExhaustiveness,
    DivisionByZero,
    Crash,
    ExpectErr,
};

/// Comptime-gated tracing for the boxy runtime's RC plan execution.
/// Enabled via `-Dtrace-eval=true`. Zero cost when disabled.
const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            debugPrint("[interp] " ++ fmt ++ "\n", args);
        }
    }
};

const debugPrint = if (is_freestanding)
    struct {
        fn print(comptime _: []const u8, _: anytype) void {}
    }.print
else
    struct {
        fn print(comptime fmt: []const u8, args: anytype) void {
            std.debug.print(fmt, args);
        }
    }.print;

/// Marks a boxy span as indexing the interpreter's runtime descriptor tables
/// rather than the static tables lowered with the program.
pub const runtimeBoxySpanTag: u32 = 0x8000_0000;

pub fn runtimeBoxySpanStart(span: LIR.BoxySpan) ?usize {
    if ((span.start & runtimeBoxySpanTag) == 0) return null;
    return @intCast(span.start & ~runtimeBoxySpanTag);
}

pub fn makeRuntimeBoxySpan(start: usize, len: usize) LIR.BoxySpan {
    if (start >= runtimeBoxySpanTag) {
        @panic("LIR/interpreter invariant violated: runtime boxy span exceeded encodable range");
    }
    return .{ .start = runtimeBoxySpanTag | @as(u32, @intCast(start)), .len = @intCast(len) };
}

pub fn isUnsigned(layout_idx: layout_mod.Idx) bool {
    return switch (layout_idx) {
        .u8, .u16, .u32, .u64, .u128 => true,
        else => false,
    };
}

/// The nested op an aggregate's children receive when the parent is released:
/// releasing the parent releases exactly one reference to each child.
pub fn nestedDropOp(op: RcOp) RcOp {
    return switch (op) {
        .incref => .incref,
        .decref, .free => .decref,
    };
}

pub const AllocatedTag = struct {
    outer: Value,
    base: Value,
    base_layout: layout_mod.Idx,
};

pub const BoxAllocInfo = struct {
    elem_layout: layout_mod.Idx,
    elem_size: u32,
    elem_alignment: u32,
    contains_rc: bool,
};

/// A produced boxy value together with the descriptor the producing statement
/// binds to its target local.
pub const BoxyAssignedValue = struct {
    value: Value,
    desc: ?*const LirProgram.BoxyTypeDesc,
};

/// One explicit argument to a dictionary method call: its value bytes, its
/// layout, and the resolved descriptor attached to the argument's local (null
/// when the local carries none).
pub const DictCallArg = struct {
    value: Value,
    layout: layout_mod.Idx,
    source_desc: ?*const LirProgram.BoxyTypeDesc = null,
};

/// The execution plan for one dictionary method call.
pub const PreparedWorkerCall = struct {
    proc: LIR.LirProcSpecId,
    arg_values: []Value,
    arg_layouts: []layout_mod.Idx,
    arg_descs: []?*const LirProgram.BoxyTypeDesc,
    borrowed_args: u64 = 0,
};

pub const PreparedDictCall = union(enum) {
    /// The slot dispatches to descriptor-guided structural equality of the
    /// two explicit arguments; no worker runs.
    structural_eq: *const LirProgram.BoxyTypeDesc,
    /// The slot dispatches to a worker proc with the fully adapted argument
    /// list (explicit args first, then hidden descriptors, then nested
    /// dictionaries).
    call: PreparedWorkerCall,
};

pub const InspectCallResult = struct {
    value: Value,
    layout: layout_mod.Idx,
    desc: ?*const LirProgram.BoxyTypeDesc,
    borrowed: bool,
};

pub const CallArgumentMode = enum { move, borrow };

pub fn valueToRocStr(val: Value) RocStr {
    var rs: RocStr = undefined;
    @memcpy(std.mem.asBytes(&rs), val.ptr[0..@sizeOf(RocStr)]);
    return rs;
}

pub fn valueToRocList(val: Value) RocList {
    var rl: RocList = undefined;
    @memcpy(std.mem.asBytes(&rl), val.ptr[0..@sizeOf(RocList)]);
    return rl;
}

pub const ResolvedListBase = struct {
    value: Value,
    layout: layout_mod.Idx,
};

pub const ResolvedTagUnionBase = struct {
    value: Value,
    layout: layout_mod.Idx,
};

pub fn readRocStr(val: Value) []const u8 {
    const rs = valueToRocStr(val);
    if (rs.isSmallStr()) {
        return val.ptr[0..rs.len()];
    }
    return rs.asSlice();
}

pub fn canonicalZstList(len: usize) RocList {
    return .{
        .bytes = null,
        .length = len,
        .capacity_or_alloc_ptr = 0,
    };
}

pub const BoxyTagPayloadRead = struct {
    value: Value,
    desc: ?LIR.BoxyDescRef,
};

pub const RawBoxyTagPayloadRead = struct {
    value: Value,
    layout: layout_mod.Idx,
};

pub const BoxyPayloadValue = struct {
    value: Value,
    layout: layout_mod.Idx,
    desc: ?*const LirProgram.BoxyTypeDesc,
};

pub const SingleFieldPayloadInfo = struct {
    layout: layout_mod.Idx,
    offset: u32,
};

/// The boxy descriptor tables lowered with a program: the static side inputs to
/// the boxy runtime.
pub const BoxyTables = struct {
    type_descs: []const LirProgram.BoxyTypeDesc = &.{},
    dicts: []const LirProgram.BoxyDict = &.{},
    adapters: []const LirProgram.BoxyAdapter = &.{},
    desc_refs: []const LirProgram.BoxyDescRef = &.{},
    dict_refs: []const LirProgram.BoxyDictRef = &.{},
    tag_variants: []const LirProgram.BoxyTagVariant = &.{},
    tag_payload_descs: []const LirProgram.BoxyTagPayloadDesc = &.{},
    field_names: []const base.StringLiteral.Idx = &.{},
    adapt_steps: []const LirProgram.BoxyAdaptStep = &.{},
    payload_steps: []const LirProgram.BoxyPayloadStep = &.{},
    method_slots: []const LirProgram.BoxyMethodSlot = &.{},
    method_arg_layouts: []const layout_mod.Idx = &.{},
    method_hidden_desc_sources: []const LirProgram.BoxyMethodHiddenDescSource = &.{},

    pub fn fromResult(result: *const LirProgram.Result) BoxyTables {
        return .{
            .type_descs = result.boxy_type_descs.items,
            .dicts = result.boxy_dicts.items,
            .adapters = result.boxy_adapters.items,
            .desc_refs = result.boxy_desc_refs.items,
            .dict_refs = result.boxy_dict_refs.items,
            .tag_variants = result.boxy_tag_variants.items,
            .tag_payload_descs = result.boxy_tag_payload_descs.items,
            .field_names = result.boxy_field_names.items,
            .adapt_steps = result.boxy_adapt_steps.items,
            .payload_steps = result.boxy_payload_steps.items,
            .method_slots = result.boxy_method_slots.items,
            .method_arg_layouts = result.boxy_method_arg_layouts.items,
            .method_hidden_desc_sources = result.boxy_method_hidden_desc_sources.items,
        };
    }

    pub fn fromImageView(view: *const lir.LirImage.ProgramView) BoxyTables {
        return .{
            .type_descs = view.boxy_type_descs,
            .dicts = view.boxy_dicts,
            .adapters = view.boxy_adapters,
            .desc_refs = view.boxy_desc_refs,
            .dict_refs = view.boxy_dict_refs,
            .tag_variants = view.boxy_tag_variants,
            .tag_payload_descs = view.boxy_tag_payload_descs,
            .field_names = view.boxy_field_names,
            .adapt_steps = view.boxy_adapt_steps,
            .payload_steps = view.boxy_payload_steps,
            .method_slots = view.boxy_method_slots,
            .method_arg_layouts = view.boxy_method_arg_layouts,
            .method_hidden_desc_sources = view.boxy_method_hidden_desc_sources,
        };
    }
};

/// Descriptor-guided boxy value operations bound to their table and store
/// dependencies. The runtime never resolves frame-local descriptor handles
/// itself: operations that walk descriptor references take a `hooks` value
/// whose `resolveDescRef` method maps a `BoxyDescRef` to a resolved
/// `*const BoxyTypeDesc`. Hooks also supply the concrete refcount operation
/// (`performRcHelper`), refcount-presence queries (`layoutContainsRc`), and
/// value materialization into fresh storage (`materializeValue`), which remain
/// the embedding execution engine's responsibility.
pub const BoxyRuntime = struct {
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    helper: LayoutHelper,
    boxy_tables: BoxyTables,
    runtime_boxy_type_descs: *std.ArrayList(*const LirProgram.BoxyTypeDesc),
    runtime_boxy_desc_refs: *std.ArrayList(LirProgram.BoxyDescRef),
    runtime_boxy_tag_variants: *std.ArrayList(LirProgram.BoxyTagVariant),
    runtime_boxy_tag_payload_descs: *std.ArrayList(LirProgram.BoxyTagPayloadDesc),
    runtime_boxy_payload_steps: *std.ArrayList(LirProgram.BoxyPayloadStep),
    roc_ops: *RocOps,
    /// Backs the runtime descriptor tables' storage.
    scratch: Allocator,
    /// Backs runtime-created descriptors and inspect text scratch; allocations
    /// stay alive for the rest of the evaluation.
    eval_arena: Allocator,

    fn invariantFailed(_: *const BoxyRuntime, comptime fmt: []const u8, args: anytype) noreturn {
        if (builtin.mode == .Debug) {
            debugPrint(fmt, args);
            debugPrint("\n", .{});
            std.debug.assert(false);
        }
        unreachable;
    }

    fn invariantFailedError(self: *const BoxyRuntime, comptime fmt: []const u8, args: anytype) Error {
        self.invariantFailed(fmt, args);
    }

    pub fn requireBoxyTypeDesc(self: *const BoxyRuntime, desc_id: LIR.BoxyTypeDescId) *const LirProgram.BoxyTypeDesc {
        const index = @intFromEnum(desc_id);
        if (index >= self.boxy_tables.type_descs.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy descriptor id {d} exceeded descriptor table length {d}",
                .{ index, self.boxy_tables.type_descs.len },
            );
        }
        return &self.boxy_tables.type_descs[index];
    }

    pub fn requireBoxyAdapter(self: *const BoxyRuntime, adapter_id: LIR.BoxyAdapterId) *const LirProgram.BoxyAdapter {
        const index = @intFromEnum(adapter_id);
        if (index >= self.boxy_tables.adapters.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy adapter id {d} exceeded adapter table length {d}",
                .{ index, self.boxy_tables.adapters.len },
            );
        }
        return &self.boxy_tables.adapters[index];
    }

    pub fn requireBoxyTagVariants(self: *const BoxyRuntime, span: LIR.BoxySpan) []const LirProgram.BoxyTagVariant {
        if (runtimeBoxySpanStart(span)) |start| {
            const end = start + span.len;
            if (end > self.runtime_boxy_tag_variants.items.len) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: runtime boxy tag variant span [{d}, {d}) exceeded tag variant table length {d}",
                    .{ start, end, self.runtime_boxy_tag_variants.items.len },
                );
            }
            return self.runtime_boxy_tag_variants.items[start..end];
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.tag_variants.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy tag variant span [{d}, {d}) exceeded tag variant table length {d}",
                .{ start, end, self.boxy_tables.tag_variants.len },
            );
        }
        return self.boxy_tables.tag_variants[start..end];
    }

    pub fn requireBoxyDescRefs(self: *const BoxyRuntime, span: LIR.BoxySpan) []const LIR.BoxyDescRef {
        if (runtimeBoxySpanStart(span)) |start| {
            const end = start + span.len;
            if (end > self.runtime_boxy_desc_refs.items.len) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: runtime boxy descriptor-ref span [{d}, {d}) exceeded descriptor-ref table length {d}",
                    .{ start, end, self.runtime_boxy_desc_refs.items.len },
                );
            }
            return self.runtime_boxy_desc_refs.items[start..end];
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.desc_refs.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy descriptor-ref span [{d}, {d}) exceeded descriptor-ref table length {d}",
                .{ start, end, self.boxy_tables.desc_refs.len },
            );
        }
        return self.boxy_tables.desc_refs[start..end];
    }

    pub fn requireBoxyTagPayloadDescs(self: *const BoxyRuntime, span: LIR.BoxySpan) []const LirProgram.BoxyTagPayloadDesc {
        if (runtimeBoxySpanStart(span)) |start| {
            const end = start + span.len;
            if (end > self.runtime_boxy_tag_payload_descs.items.len) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: runtime boxy tag payload descriptor span [{d}, {d}) exceeded table length {d}",
                    .{ start, end, self.runtime_boxy_tag_payload_descs.items.len },
                );
            }
            return self.runtime_boxy_tag_payload_descs.items[start..end];
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.tag_payload_descs.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy tag payload descriptor span [{d}, {d}) exceeded table length {d}",
                .{ start, end, self.boxy_tables.tag_payload_descs.len },
            );
        }
        return self.boxy_tables.tag_payload_descs[start..end];
    }

    pub fn findLocalBoxyTagVariant(
        self: *const BoxyRuntime,
        desc: *const LirProgram.BoxyTypeDesc,
        tag_name: base.StringLiteral.Idx,
    ) ?*const LirProgram.BoxyTagVariant {
        const wanted = self.store.getString(tag_name);
        for (self.requireBoxyTagVariants(desc.tag_variants)) |*variant| {
            if (std.mem.eql(u8, wanted, self.store.getString(variant.name))) return variant;
        }
        return null;
    }

    pub fn requireBoxyTagVariantByDiscriminant(
        self: *const BoxyRuntime,
        desc: *const LirProgram.BoxyTypeDesc,
        discriminant: u16,
    ) *const LirProgram.BoxyTagVariant {
        if (self.findBoxyTagVariantByDiscriminant(desc, discriminant)) |variant| return variant;
        self.invariantFailed(
            "LIR/interpreter invariant violated: boxy descriptor had no tag variant with discriminant {d} payload_layout={d}",
            .{
                discriminant,
                @intFromEnum(desc.payload_layout),
            },
        );
    }

    pub fn findBoxyTagVariantByDiscriminant(
        self: *const BoxyRuntime,
        desc: *const LirProgram.BoxyTypeDesc,
        discriminant: u16,
    ) ?*const LirProgram.BoxyTagVariant {
        for (self.requireBoxyTagVariants(desc.tag_variants)) |*variant| {
            if (variant.discriminant == discriminant) return variant;
        }
        return null;
    }

    pub fn boxyTagExtDiscriminant(self: *const BoxyRuntime, desc: *const LirProgram.BoxyTypeDesc) ?u16 {
        if (desc.tag_ext_desc == null) return null;
        if (desc.tag_variants.len > std.math.maxInt(u16)) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy tag descriptor had too many variants for row-extension discriminant: {d}",
                .{desc.tag_variants.len},
            );
        }
        return @intCast(desc.tag_variants.len);
    }

    pub fn requireBoxyTagPayloadLayout(
        self: *const BoxyRuntime,
        union_layout: layout_mod.Idx,
        discriminant: u16,
    ) layout_mod.Idx {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .zst) {
            if (discriminant == 0) return .zst;
            self.invariantFailed(
                "LIR/interpreter invariant violated: zero-sized boxy tag descriptor payload layout {d} received nonzero discriminant {d}",
                .{ @intFromEnum(union_layout), discriminant },
            );
        }
        if (union_layout_val.tag != .tag_union) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy tag descriptor payload layout {d} was not a tag union",
                .{@intFromEnum(union_layout)},
            );
        }
        const tu_data = self.layout_store.getTagUnionData(union_layout_val.getTagUnion().idx);
        const variants = self.layout_store.getTagUnionVariants(tu_data);
        if (discriminant >= variants.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy tag discriminant {d} exceeded payload layout {d} variant count {d}",
                .{ discriminant, @intFromEnum(union_layout), variants.len },
            );
        }
        return variants.get(discriminant).payload_layout;
    }

    pub fn findBoxyPayloadDesc(
        self: *const BoxyRuntime,
        variant: *const LirProgram.BoxyTagVariant,
        payload_index: u32,
    ) ?LIR.BoxyDescRef {
        for (self.requireBoxyTagPayloadDescs(variant.payload_descs)) |payload_desc| {
            if (payload_desc.payload_index == payload_index) return payload_desc.desc;
        }
        return null;
    }

    pub fn boxyDescHasConcreteScalarPayload(self: *const BoxyRuntime, desc: *const LirProgram.BoxyTypeDesc) bool {
        const payload_val = self.layout_store.getLayout(desc.payload_layout);
        return payload_val.tag == .scalar and payload_val.getScalar().tag != .opaque_ptr;
    }

    pub fn boxyDescIsBoxSelfForBoxValue(
        self: *const BoxyRuntime,
        box_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) bool {
        const value_tag = self.layout_store.getLayout(box_layout).tag;
        if (value_tag != .box and value_tag != .box_of_zst) return false;
        const desc_payload_tag = self.layout_store.getLayout(desc.payload_layout).tag;
        return desc_payload_tag == .box or desc_payload_tag == .box_of_zst;
    }

    pub fn readPointerInt(self: *const BoxyRuntime, value: Value) usize {
        return switch (self.layout_store.targetUsize().size()) {
            4 => value.read(u32),
            8 => value.read(usize),
            else => unreachable,
        };
    }

    pub fn writePointerInt(self: *const BoxyRuntime, value: Value, raw_ptr: usize) void {
        switch (self.layout_store.targetUsize().size()) {
            4 => value.write(u32, @intCast(raw_ptr)),
            8 => value.write(usize, raw_ptr),
            else => unreachable,
        }
    }

    pub fn readBoxedDataPointer(self: *const BoxyRuntime, boxed: Value) ?[*]u8 {
        const raw_ptr = self.readPointerInt(boxed);

        if (raw_ptr == 0) return null;
        return @ptrFromInt(raw_ptr);
    }

    pub fn writeBoxedDataPointer(self: *const BoxyRuntime, boxed: Value, data_ptr: ?[*]u8) void {
        const raw_ptr: usize = if (data_ptr) |ptr| @intFromPtr(ptr) else 0;
        self.writePointerInt(boxed, raw_ptr);
    }

    pub fn normalizeValueToLayout(
        self: *const BoxyRuntime,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Value {
        if (actual_layout == expected_layout) return value;

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        switch (actual_layout_val.tag) {
            .box => {
                if (actual_layout_val.getIdx() == expected_layout) {
                    const data_ptr = self.readBoxedDataPointer(value) orelse self.invariantFailed(
                        "LIR/interpreter invariant violated: expected boxed layout {d} to contain data for inner layout {d}, but observed null box pointer",
                        .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                    );
                    return .{ .ptr = data_ptr };
                }
            },
            .box_of_zst => if (expected_layout == .zst) return Value.zst,
            else => {},
        }

        return value;
    }

    pub fn resolveListBaseValue(
        self: *const BoxyRuntime,
        list_val: Value,
        list_layout: layout_mod.Idx,
    ) ResolvedListBase {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@intFromEnum(list_layout)},
        );
        return .{
            .value = self.normalizeValueToLayout(list_val, list_layout, resolved_layout),
            .layout = resolved_layout,
        };
    }

    pub fn valueToRocListForLayout(
        self: *const BoxyRuntime,
        list_val: Value,
        list_layout: layout_mod.Idx,
    ) RocList {
        return valueToRocList(self.resolveListBaseValue(list_val, list_layout).value);
    }

    pub fn listElemLayout(self: *const BoxyRuntime, list_layout: layout_mod.Idx) layout_mod.Idx {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@intFromEnum(list_layout)},
        );
        const l = self.layout_store.getLayout(resolved_layout);
        if (l.tag == .list) return l.getIdx();
        return .zst;
    }

    pub fn resolveTagUnionBaseValue(
        self: *const BoxyRuntime,
        union_val: Value,
        union_layout: layout_mod.Idx,
    ) ResolvedTagUnionBase {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const inner_layout = union_layout_val.getIdx();
            const data_ptr = self.readBoxedDataPointer(union_val) orelse self.invariantFailed(
                "LIR/interpreter invariant violated: boxed tag union layout {d} had null data pointer for inner layout {d}",
                .{ @intFromEnum(union_layout), @intFromEnum(inner_layout) },
            );
            return .{
                .value = .{ .ptr = data_ptr },
                .layout = inner_layout,
            };
        }

        return .{
            .value = union_val,
            .layout = union_layout,
        };
    }

    pub fn resolveBoxyTagBaseValue(
        self: *const BoxyRuntime,
        source_val: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
    ) ResolvedTagUnionBase {
        const source_layout_val = self.layout_store.getLayout(source_layout);
        return switch (source_layout_val.tag) {
            .tag_union, .box => self.resolveTagUnionBaseValue(source_val, source_layout),
            .box_of_zst => blk: {
                const data_ptr = self.readBoxedDataPointer(source_val) orelse {
                    if (self.helper.sizeOf(source_desc.payload_layout) == 0) {
                        break :blk .{
                            .value = Value.zst,
                            .layout = source_desc.payload_layout,
                        };
                    }
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: dynamic boxy tag source had null payload pointer for source layout {d} descriptor payload layout {d}",
                        .{
                            @intFromEnum(source_layout),
                            @intFromEnum(source_desc.payload_layout),
                        },
                    );
                };
                break :blk self.resolveTagUnionBaseValue(.{ .ptr = data_ptr }, source_desc.payload_layout);
            },
            else => self.invariantFailed(
                "LIR/interpreter invariant violated: boxy tag source layout {d} was not a tag-union-compatible layout",
                .{@intFromEnum(source_layout)},
            ),
        };
    }

    pub fn tagPayloadLayout(self: *const BoxyRuntime, union_layout: layout_mod.Idx, discriminant: u16) layout_mod.Idx {
        const l = self.layout_store.getLayout(union_layout);
        return switch (l.tag) {
            .tag_union => blk: {
                const tu_data = self.layout_store.getTagUnionData(l.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            .box => blk: {
                const inner_layout = self.layout_store.getLayout(l.getIdx());
                if (inner_layout.tag != .tag_union) break :blk .zst;
                const tu_data = self.layout_store.getTagUnionData(inner_layout.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            else => .zst,
        };
    }

    pub fn layoutNeedsBoxyStructuralDesc(self: *const BoxyRuntime, layout_idx: layout_mod.Idx) bool {
        return switch (self.layout_store.getLayout(layout_idx).tag) {
            .box_of_zst,
            .box,
            .list,
            .list_of_zst,
            .struct_,
            .tag_union,
            => true,
            .scalar,
            .closure,
            .erased_callable,
            .zst,
            .ptr,
            => false,
        };
    }

    pub fn boxyDynamicPayloadAllocationContainsRc(
        self: *const BoxyRuntime,
        desc: *const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
    ) bool {
        _ = self;
        _ = target_layout;
        return desc.contains_refcounted;
    }

    pub fn firstNestedBoxyDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!?*const LirProgram.BoxyTypeDesc {
        const refs = self.requireBoxyDescRefs(desc.nested_descs);
        if (refs.len == 0) return null;
        return try hooks.resolveDescRef(refs[0]);
    }

    fn boxyStructFieldDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
        struct_layout: layout_mod.Idx,
        field_index: u32,
    ) Error!?*const LirProgram.BoxyTypeDesc {
        const layout_val = self.layout_store.getLayout(struct_layout);
        if (layout_val.tag != .struct_) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy struct field descriptor lookup received layout {d} ({s})",
                .{ @intFromEnum(struct_layout), @tagName(layout_val.tag) },
            );
        }
        const struct_idx = layout_val.getStruct().idx;
        const struct_data = self.layout_store.getStructData(struct_idx);
        if (field_index >= struct_data.fields.count) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy struct field descriptor index {d} exceeded layout {d} field count {d}",
                .{ field_index, @intFromEnum(struct_layout), struct_data.fields.count },
            );
        }

        const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, field_index);
        if (!self.layoutNeedsBoxyStructuralDesc(field_layout)) return null;

        var nested_index: usize = 0;
        var preceding_index: u32 = 0;
        while (preceding_index < field_index) : (preceding_index += 1) {
            const preceding_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, preceding_index);
            if (self.layoutNeedsBoxyStructuralDesc(preceding_layout)) nested_index += 1;
        }

        const refs = self.requireBoxyDescRefs(desc.nested_descs);
        if (nested_index >= refs.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy struct descriptor for layout {d} was missing nested descriptor {d}",
                .{ @intFromEnum(struct_layout), nested_index },
            );
        }
        return try hooks.resolveDescRef(refs[nested_index]);
    }

    fn sourceStructFieldIndexForTarget(
        self: *const BoxyRuntime,
        source_desc: *const LirProgram.BoxyTypeDesc,
        source_field_count: u32,
        target_desc: *const LirProgram.BoxyTypeDesc,
        target_field_count: u32,
        target_field_index: u32,
    ) Error!u32 {
        const source_names = self.requireBoxyFieldNames(source_desc.field_names);
        const target_names = self.requireBoxyFieldNames(target_desc.field_names);
        if (source_names.len == 0 or target_names.len == 0) {
            if ((source_names.len != 0 and source_names.len != source_field_count) or
                (target_names.len != 0 and target_names.len != target_field_count))
            {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: positional boxy struct boundary had malformed field-name metadata (source {d}/{d}, target {d}/{d})",
                    .{ source_names.len, source_field_count, target_names.len, target_field_count },
                );
            }
            if (source_field_count != target_field_count) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: positional boxy struct adaptation had source field count {d} and target field count {d}",
                    .{ source_field_count, target_field_count },
                );
            }
            return target_field_index;
        }
        if (source_names.len != source_field_count or target_names.len != target_field_count) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: named boxy struct descriptor field counts disagreed with layouts (source {d}/{d}, target {d}/{d})",
                .{ source_names.len, source_field_count, target_names.len, target_field_count },
            );
        }

        const target_name = self.store.getString(target_names[target_field_index]);
        for (source_names, 0..) |source_name_id, source_index| {
            if (std.mem.eql(u8, self.store.getString(source_name_id), target_name)) {
                return @intCast(source_index);
            }
        }
        return self.invariantFailedError(
            "LIR/interpreter invariant violated: source boxy struct descriptor was missing target field {s}",
            .{target_name},
        );
    }

    fn targetStructFieldIndexForSource(
        self: *const BoxyRuntime,
        source_desc: *const LirProgram.BoxyTypeDesc,
        source_field_count: u32,
        target_desc: *const LirProgram.BoxyTypeDesc,
        target_field_count: u32,
        source_field_index: u32,
    ) Error!?u32 {
        const source_names = self.requireBoxyFieldNames(source_desc.field_names);
        const target_names = self.requireBoxyFieldNames(target_desc.field_names);
        if (source_names.len == 0 or target_names.len == 0) {
            if ((source_names.len != 0 and source_names.len != source_field_count) or
                (target_names.len != 0 and target_names.len != target_field_count))
            {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: positional moved boxy struct boundary had malformed field-name metadata (source {d}/{d}, target {d}/{d})",
                    .{ source_names.len, source_field_count, target_names.len, target_field_count },
                );
            }
            if (source_field_count != target_field_count) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: positional moved boxy struct had source field count {d} and target field count {d}",
                    .{ source_field_count, target_field_count },
                );
            }
            return source_field_index;
        }
        if (source_names.len != source_field_count or target_names.len != target_field_count) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: moved named boxy struct descriptor field counts disagreed with layouts (source {d}/{d}, target {d}/{d})",
                .{ source_names.len, source_field_count, target_names.len, target_field_count },
            );
        }

        const source_name = self.store.getString(source_names[source_field_index]);
        for (target_names, 0..) |target_name_id, target_index| {
            if (std.mem.eql(u8, self.store.getString(target_name_id), source_name)) {
                return @intCast(target_index);
            }
        }
        return null;
    }

    pub fn resolveBoxyTagExtDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!*const LirProgram.BoxyTypeDesc {
        const desc_ref = desc.tag_ext_desc orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy tag descriptor had no row-extension descriptor",
                .{},
            );
        };
        return try hooks.resolveDescRef(desc_ref);
    }

    pub fn boxyTagMatches(
        self: *const BoxyRuntime,
        hooks: anytype,
        source_value: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        tag_name: base.StringLiteral.Idx,
    ) Error!bool {
        const tag_base = self.resolveBoxyTagBaseValue(source_value, source_layout, source_desc);
        const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
        if (self.findLocalBoxyTagVariant(source_desc, tag_name)) |variant| {
            return disc == variant.discriminant;
        }

        const ext_discriminant = self.boxyTagExtDiscriminant(source_desc) orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy descriptor had no tag variant named {s}",
                .{self.store.getString(tag_name)},
            );
        };
        if (disc != ext_discriminant) return false;

        const ext_desc = try self.resolveBoxyTagExtDesc(hooks, source_desc);
        const ext_payload_layout = self.requireBoxyTagPayloadLayout(source_desc.payload_layout, ext_discriminant);
        const ext_value = try self.materializeLocalValue(hooks, tag_base.value, ext_payload_layout);
        return try self.boxyTagMatches(hooks, ext_value, ext_payload_layout, ext_desc, tag_name);
    }

    pub fn makeRuntimeScalarDesc(self: *const BoxyRuntime, payload_layout: layout_mod.Idx) Error!*const LirProgram.BoxyTypeDesc {
        for (self.runtime_boxy_type_descs.items) |existing| {
            if (existing.payload_layout == payload_layout and
                existing.nested_descs.len == 0 and
                existing.tag_variants.len == 0 and
                !existing.contains_refcounted)
            {
                return existing;
            }
        }
        const desc = try self.eval_arena.create(LirProgram.BoxyTypeDesc);
        desc.* = .{
            .payload_layout = payload_layout,
            .contains_refcounted = false,
        };
        try self.runtime_boxy_type_descs.append(self.scratch, desc);
        return desc;
    }

    pub fn runtimeBoxyDescIdForPtr(self: *const BoxyRuntime, desc: *const LirProgram.BoxyTypeDesc) ?u32 {
        for (self.runtime_boxy_type_descs.items, 0..) |existing, index| {
            if (existing == desc) return @intCast(index);
        }
        return null;
    }

    pub fn requireBoxyPayloadSteps(self: *const BoxyRuntime, span: LIR.BoxySpan) []const LirProgram.BoxyPayloadStep {
        if (runtimeBoxySpanStart(span)) |start| {
            const end = start + span.len;
            if (end > self.runtime_boxy_payload_steps.items.len) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: runtime boxy payload step span [{d}, {d}) exceeded table length {d}",
                    .{ start, end, self.runtime_boxy_payload_steps.items.len },
                );
            }
            return self.runtime_boxy_payload_steps.items[start..end];
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.payload_steps.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy payload step span [{d}, {d}) exceeded payload step table length {d}",
                .{ start, end, self.boxy_tables.payload_steps.len },
            );
        }
        return self.boxy_tables.payload_steps[start..end];
    }

    pub fn materializeBoxyDescRefValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc_ref: LIR.BoxyDescRef,
    ) Error!*const LirProgram.BoxyTypeDesc {
        return try self.materializeBoxyDescRefValueWithCaptures(hooks, desc_ref, LIR.LocalSpan.empty());
    }

    pub fn materializeBoxyDescRefValueWithCaptures(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc_ref: LIR.BoxyDescRef,
        captures: LIR.LocalSpan,
    ) Error!*const LirProgram.BoxyTypeDesc {
        switch (desc_ref) {
            .local, .runtime => return try hooks.resolveDescRef(desc_ref),
            .static => {},
        }

        var copied = std.AutoHashMap(usize, u32).init(self.scratch);
        defer copied.deinit();
        const runtime_ref = try self.copyBoxyDescRefToRuntime(hooks, desc_ref, &copied, captures.len == 0);
        return try hooks.resolveDescRef(runtime_ref);
    }

    pub fn materializeNestedBoxyDescRefValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc_ref: LIR.BoxyDescRef,
        nested_index: u32,
        captures: LIR.LocalSpan,
    ) Error!*const LirProgram.BoxyTypeDesc {
        const desc = try self.materializeBoxyDescRefValueWithCaptures(hooks, desc_ref, captures);
        const nested = self.requireBoxyDescRefs(desc.nested_descs);
        if (nested_index >= nested.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy descriptor payload layout {d} missing nested descriptor {d}",
                .{ @intFromEnum(desc.payload_layout), nested_index },
            );
        }
        return try hooks.resolveDescRef(nested[nested_index]);
    }

    pub fn materializeTagExtBoxyDescRefValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc_ref: LIR.BoxyDescRef,
        captures: LIR.LocalSpan,
    ) Error!*const LirProgram.BoxyTypeDesc {
        const desc = try self.materializeBoxyDescRefValueWithCaptures(hooks, desc_ref, captures);
        return try self.resolveBoxyTagExtDesc(hooks, desc);
    }

    pub fn materializeTagResidualBoxyDescRefValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        source_ref: LIR.BoxyDescRef,
        target_ref: LIR.BoxyDescRef,
        captures: LIR.LocalSpan,
    ) Error!*const LirProgram.BoxyTypeDesc {
        const source = try self.materializeBoxyDescRefValueWithCaptures(hooks, source_ref, captures);
        const target_shape = try self.materializeBoxyDescRefValueWithCaptures(hooks, target_ref, captures);
        return try self.materializeTagResidualBoxyDescValues(source, target_shape);
    }

    pub fn materializeTagResidualBoxyDescValues(
        self: *const BoxyRuntime,
        source: *const LirProgram.BoxyTypeDesc,
        target_shape: *const LirProgram.BoxyTypeDesc,
    ) Error!*const LirProgram.BoxyTypeDesc {
        const target_variants = self.requireBoxyTagVariants(target_shape.tag_variants);

        const residual = try self.eval_arena.create(LirProgram.BoxyTypeDesc);
        residual.* = source.*;
        try self.runtime_boxy_type_descs.append(self.scratch, residual);

        const start = self.runtime_boxy_tag_variants.items.len;
        for (self.requireBoxyTagVariants(source.tag_variants)) |source_variant| {
            var belongs_to_target = false;
            for (target_variants) |target_variant| {
                if (source_variant.name == target_variant.name) {
                    belongs_to_target = true;
                    break;
                }
            }
            if (!belongs_to_target) {
                try self.runtime_boxy_tag_variants.append(self.scratch, source_variant);
            }
        }
        residual.tag_variants = makeRuntimeBoxySpan(start, self.runtime_boxy_tag_variants.items.len - start);
        return residual;
    }

    fn copyBoxyDescRefToRuntime(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc_ref: LIR.BoxyDescRef,
        copied: *std.AutoHashMap(usize, u32),
        allow_global_reuse: bool,
    ) Error!LIR.BoxyDescRef {
        if (desc_ref == .runtime) return desc_ref;

        const source = try hooks.resolveDescRef(desc_ref);
        if (self.runtimeBoxyDescIdForPtr(source)) |runtime_id| {
            return .{ .runtime = runtime_id };
        }
        const source_key = @intFromPtr(source);
        if (copied.get(source_key)) |runtime_id| {
            return .{ .runtime = runtime_id };
        }

        const runtime_id: u32 = @intCast(self.runtime_boxy_type_descs.items.len);
        const target = try self.eval_arena.create(LirProgram.BoxyTypeDesc);
        target.* = .{
            .payload_layout = source.payload_layout,
            .contains_refcounted = source.contains_refcounted,
            .debug_checked_type = source.debug_checked_type,
        };
        try self.runtime_boxy_type_descs.append(self.scratch, target);
        try copied.put(source_key, runtime_id);

        target.nested_descs = try self.copyBoxyDescRefSpanToRuntime(hooks, source.nested_descs, copied, allow_global_reuse);
        target.tag_variants = try self.copyBoxyTagVariantSpanToRuntime(hooks, source.tag_variants, copied, allow_global_reuse);
        target.tag_ext_desc = if (source.tag_ext_desc) |tag_ext|
            try self.copyBoxyDescRefToRuntime(hooks, tag_ext, copied, allow_global_reuse)
        else
            null;
        target.copy_plan = try self.copyBoxyPayloadStepSpanToRuntime(hooks, source.copy_plan, copied, allow_global_reuse);
        target.drop_plan = try self.copyBoxyPayloadStepSpanToRuntime(hooks, source.drop_plan, copied, allow_global_reuse);
        target.structural_eq = source.structural_eq;
        target.structural_hash = source.structural_hash;
        target.inspect_method = source.inspect_method;
        // Field names are immutable static-pool data; runtime copies keep the
        // static span.
        target.field_names = source.field_names;
        target.inspect_opaque = source.inspect_opaque;

        return .{ .runtime = runtime_id };
    }

    fn copyBoxyDescRefSpanToRuntime(
        self: *const BoxyRuntime,
        hooks: anytype,
        span: LIR.BoxySpan,
        copied: *std.AutoHashMap(usize, u32),
        allow_global_reuse: bool,
    ) Error!LIR.BoxySpan {
        if (runtimeBoxySpanStart(span) != null) return span;

        const source_refs = self.requireBoxyDescRefs(span);
        if (source_refs.len == 0) return .{};

        const start = self.runtime_boxy_desc_refs.items.len;
        try self.runtime_boxy_desc_refs.appendNTimes(self.scratch, .{ .static = @enumFromInt(0) }, source_refs.len);
        for (source_refs, 0..) |source_ref, index| {
            // Materialize the nested reference before indexing the destination
            // list: the recursive copy appends to `runtime_boxy_desc_refs`, which
            // can reallocate its backing buffer. Computing the store address
            // first would target the pre-reallocation buffer and lose the write.
            const copied_ref = try self.copyBoxyDescRefToRuntime(hooks, source_ref, copied, allow_global_reuse);
            self.runtime_boxy_desc_refs.items[start + index] = copied_ref;
        }
        return makeRuntimeBoxySpan(start, source_refs.len);
    }

    fn copyBoxyTagVariantSpanToRuntime(
        self: *const BoxyRuntime,
        hooks: anytype,
        span: LIR.BoxySpan,
        copied: *std.AutoHashMap(usize, u32),
        allow_global_reuse: bool,
    ) Error!LIR.BoxySpan {
        if (runtimeBoxySpanStart(span) != null) return span;

        const source_variants = self.requireBoxyTagVariants(span);
        if (source_variants.len == 0) return .{};

        const start = self.runtime_boxy_tag_variants.items.len;
        try self.runtime_boxy_tag_variants.appendNTimes(self.scratch, .{
            .name = @enumFromInt(0),
            .discriminant = 0,
            .payload_layout = .zst,
        }, source_variants.len);
        for (source_variants, 0..) |variant, index| {
            // Materialize before indexing: the recursive copy can append to and
            // reallocate `runtime_boxy_tag_variants`, invalidating a destination
            // address computed ahead of the call.
            const payload_descs = try self.copyBoxyTagPayloadDescSpanToRuntime(hooks, variant.payload_descs, copied, allow_global_reuse);
            self.runtime_boxy_tag_variants.items[start + index] = .{
                .name = variant.name,
                .discriminant = variant.discriminant,
                .payload_layout = variant.payload_layout,
                .payload_descs = payload_descs,
            };
        }
        return makeRuntimeBoxySpan(start, source_variants.len);
    }

    fn copyBoxyTagPayloadDescSpanToRuntime(
        self: *const BoxyRuntime,
        hooks: anytype,
        span: LIR.BoxySpan,
        copied: *std.AutoHashMap(usize, u32),
        allow_global_reuse: bool,
    ) Error!LIR.BoxySpan {
        if (runtimeBoxySpanStart(span) != null) return span;

        const source_descs = self.requireBoxyTagPayloadDescs(span);
        if (source_descs.len == 0) return .{};

        const start = self.runtime_boxy_tag_payload_descs.items.len;
        try self.runtime_boxy_tag_payload_descs.appendNTimes(self.scratch, .{
            .payload_index = 0,
            .desc = .{ .static = @enumFromInt(0) },
        }, source_descs.len);
        for (source_descs, 0..) |payload_desc, index| {
            // Materialize before indexing: the recursive copy can append to and
            // reallocate `runtime_boxy_tag_payload_descs`, invalidating a
            // destination address computed ahead of the call.
            const desc_ref = try self.copyBoxyDescRefToRuntime(hooks, payload_desc.desc, copied, allow_global_reuse);
            self.runtime_boxy_tag_payload_descs.items[start + index] = .{
                .payload_index = payload_desc.payload_index,
                .desc = desc_ref,
            };
        }
        return makeRuntimeBoxySpan(start, source_descs.len);
    }

    fn copyBoxyPayloadStepSpanToRuntime(
        self: *const BoxyRuntime,
        hooks: anytype,
        span: LIR.BoxySpan,
        copied: *std.AutoHashMap(usize, u32),
        allow_global_reuse: bool,
    ) Error!LIR.BoxySpan {
        if (runtimeBoxySpanStart(span) != null) return span;

        const source_steps = self.requireBoxyPayloadSteps(span);
        if (source_steps.len == 0) return .{};

        const start = self.runtime_boxy_payload_steps.items.len;
        try self.runtime_boxy_payload_steps.appendNTimes(self.scratch, .{ .concrete = .{
            .op = .copy,
            .layout_idx = .zst,
        } }, source_steps.len);
        for (source_steps, 0..) |step, index| {
            // Materialize before indexing: the recursive copy can append to and
            // reallocate `runtime_boxy_payload_steps`, invalidating a destination
            // address computed ahead of the call.
            const copied_step: LirProgram.BoxyPayloadStep = switch (step) {
                .concrete => |concrete| .{ .concrete = concrete },
                .dynamic => |dynamic| .{ .dynamic = .{
                    .op = dynamic.op,
                    .desc = try self.copyBoxyDescRefToRuntime(hooks, dynamic.desc, copied, allow_global_reuse),
                } },
            };
            self.runtime_boxy_payload_steps.items[start + index] = copied_step;
        }
        return makeRuntimeBoxySpan(start, source_steps.len);
    }

    pub fn boxyValuesEqual(
        self: *const BoxyRuntime,
        hooks: anytype,
        a: Value,
        b: Value,
        value_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!bool {
        const maybe_hooks: ?@TypeOf(hooks) = hooks;
        const layout_val = self.layout_store.getLayout(value_layout);
        if (layout_val.tag == .box_of_zst) {
            const a_ptr = self.readBoxedDataPointer(a);
            const b_ptr = self.readBoxedDataPointer(b);
            if (a_ptr == null or b_ptr == null) {
                if (a_ptr == null and b_ptr == null and self.helper.sizeOf(desc.payload_layout) == 0) return true;
                return false;
            }
            return self.valuesEqualWithDesc(maybe_hooks, .{ .ptr = a_ptr.? }, .{ .ptr = b_ptr.? }, desc.payload_layout, desc);
        }
        return self.valuesEqualWithDesc(maybe_hooks, a, b, value_layout, desc);
    }

    pub fn valuesEqualWithDesc(
        self: *const BoxyRuntime,
        maybe_hooks: anytype,
        a: Value,
        b: Value,
        layout_idx: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!bool {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .zst => true,
            .scalar => switch (layout_val.getScalar().tag) {
                .str => builtins.str.strEqual(valueToRocStr(a), valueToRocStr(b)),
                .frac => switch (self.helper.sizeOf(layout_idx)) {
                    4 => a.read(f32) == b.read(f32),
                    8 => a.read(f64) == b.read(f64),
                    16 => a.read(i128) == b.read(i128),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: fractional layout {d} has unsupported size {d}",
                        .{ @intFromEnum(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .int => switch (self.helper.sizeOf(layout_idx)) {
                    1 => if (isUnsigned(layout_idx)) a.read(u8) == b.read(u8) else a.read(i8) == b.read(i8),
                    2 => if (isUnsigned(layout_idx)) a.read(u16) == b.read(u16) else a.read(i16) == b.read(i16),
                    4 => if (isUnsigned(layout_idx)) a.read(u32) == b.read(u32) else a.read(i32) == b.read(i32),
                    8 => if (isUnsigned(layout_idx)) a.read(u64) == b.read(u64) else a.read(i64) == b.read(i64),
                    16 => if (isUnsigned(layout_idx)) a.read(u128) == b.read(u128) else a.read(i128) == b.read(i128),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: scalar layout {d} has unsupported size {d}",
                        .{ @intFromEnum(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .opaque_ptr => switch (self.helper.sizeOf(layout_idx)) {
                    4 => a.read(u32) == b.read(u32),
                    8 => a.read(usize) == b.read(usize),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: opaque pointer layout {d} has unsupported size {d}",
                        .{ @intFromEnum(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
            },
            .box_of_zst => if (desc) |payload_desc| blk: {
                const hooks = maybe_hooks orelse
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: descriptor-backed boxy equality had no frame for layout {d}",
                        .{@intFromEnum(layout_idx)},
                    );
                break :blk try self.boxyValuesEqual(hooks, a, b, layout_idx, payload_desc);
            } else true,
            .box => blk: {
                const a_ptr = self.readBoxedDataPointer(a);
                const b_ptr = self.readBoxedDataPointer(b);
                if (a_ptr == null or b_ptr == null) break :blk a_ptr == null and b_ptr == null;
                const elem_desc = if (desc) |box_desc| blk_desc: {
                    const hooks = maybe_hooks orelse
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: descriptor-backed box equality had no frame for layout {d}",
                            .{@intFromEnum(layout_idx)},
                        );
                    break :blk_desc try self.firstNestedBoxyDesc(hooks, box_desc);
                } else null;
                break :blk try self.valuesEqualWithDesc(maybe_hooks, .{ .ptr = a_ptr.? }, .{ .ptr = b_ptr.? }, layout_val.getIdx(), elem_desc);
            },
            .erased_callable => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on erased callable layout {d} survived lowering",
                .{@intFromEnum(layout_idx)},
            ),
            .ptr => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on compiler-internal ptr layout {d}",
                .{@intFromEnum(layout_idx)},
            ),
            .struct_ => blk: {
                const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
                const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                const desc_refs = if (desc) |struct_desc| self.requireBoxyDescRefs(struct_desc.nested_descs) else &.{};
                var next_desc: usize = 0;
                var field_index: usize = 0;
                while (field_index < fields.len) : (field_index += 1) {
                    const field = fields.get(@intCast(field_index));
                    // Padding spacers hold uninitialized bytes; they are not part
                    // of a value's identity and must never be compared.
                    if (field.is_padding) continue;
                    const field_layout = field.layout;
                    const field_size = self.helper.sizeOf(field_layout);
                    if (field_size == 0) continue;
                    const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                        layout_val.getStruct().idx,
                        field.index,
                    );
                    const field_desc = if (desc != null and self.layoutNeedsBoxyStructuralDesc(field_layout) and next_desc < desc_refs.len) blk_desc: {
                        const hooks = maybe_hooks orelse
                            return self.invariantFailedError(
                                "LIR/interpreter invariant violated: descriptor-backed struct equality had no frame for layout {d}",
                                .{@intFromEnum(layout_idx)},
                            );
                        const resolved = try hooks.resolveDescRef(desc_refs[next_desc]);
                        next_desc += 1;
                        break :blk_desc resolved;
                    } else null;
                    if (!try self.valuesEqualWithDesc(maybe_hooks, a.offset(field_offset), b.offset(field_offset), field_layout, field_desc)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => blk: {
                const a_base = self.resolveTagUnionBaseValue(a, layout_idx);
                const b_base = self.resolveTagUnionBaseValue(b, layout_idx);
                const a_disc = self.helper.readTagDiscriminant(a_base.value, a_base.layout);
                const b_disc = self.helper.readTagDiscriminant(b_base.value, b_base.layout);
                if (a_disc != b_disc) break :blk false;
                if (desc) |tag_desc| {
                    const hooks = maybe_hooks orelse
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: descriptor-backed tag equality had no frame for layout {d}",
                            .{@intFromEnum(layout_idx)},
                        );
                    if (self.boxyTagExtDiscriminant(tag_desc)) |ext_discriminant| {
                        if (a_disc == ext_discriminant) {
                            const ext_desc = try self.resolveBoxyTagExtDesc(hooks, tag_desc);
                            const ext_payload_layout = self.requireBoxyTagPayloadLayout(tag_desc.payload_layout, ext_discriminant);
                            break :blk try self.valuesEqualWithDesc(maybe_hooks, a_base.value, b_base.value, ext_payload_layout, ext_desc);
                        }
                    }

                    const variant = self.requireBoxyTagVariantByDiscriminant(tag_desc, a_disc);
                    if (self.helper.sizeOf(variant.payload_layout) == 0) break :blk true;
                    const payload_layout_val = self.layout_store.getLayout(variant.payload_layout);
                    switch (payload_layout_val.tag) {
                        .struct_ => {
                            // A single-argument tag stores its argument as the
                            // whole payload area and its recorded descriptor
                            // describes that whole value; the per-field pairing
                            // below is for multi-argument tags.
                            if (self.findBoxyPayloadDesc(variant, 0)) |first_desc_ref| {
                                const first_desc = try hooks.resolveDescRef(first_desc_ref);
                                if (first_desc.payload_layout == variant.payload_layout) {
                                    break :blk try self.valuesEqualWithDesc(maybe_hooks, a_base.value, b_base.value, variant.payload_layout, first_desc);
                                }
                            }
                            const struct_idx = payload_layout_val.getStruct().idx;
                            const struct_data = self.layout_store.getStructData(struct_idx);
                            var original_index: u32 = 0;
                            while (original_index < struct_data.fields.count) : (original_index += 1) {
                                const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, original_index);
                                if (self.helper.sizeOf(field_layout) == 0) continue;
                                const field_desc = if (self.findBoxyPayloadDesc(variant, original_index)) |payload_desc|
                                    try hooks.resolveDescRef(payload_desc)
                                else
                                    null;
                                const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, original_index);
                                if (!try self.valuesEqualWithDesc(maybe_hooks, a_base.value.offset(field_offset), b_base.value.offset(field_offset), field_layout, field_desc)) {
                                    break :blk false;
                                }
                            }
                            break :blk true;
                        },
                        else => {
                            const payload_desc = if (self.findBoxyPayloadDesc(variant, 0)) |payload_desc|
                                try hooks.resolveDescRef(payload_desc)
                            else
                                null;
                            break :blk try self.valuesEqualWithDesc(maybe_hooks, a_base.value, b_base.value, variant.payload_layout, payload_desc);
                        },
                    }
                }
                const payload_layout = self.tagPayloadLayout(a_base.layout, a_disc);
                if (self.helper.sizeOf(payload_layout) == 0) break :blk true;
                break :blk try self.valuesEqualWithDesc(maybe_hooks, a_base.value, b_base.value, payload_layout, null);
            },
            .list_of_zst => self.valueToRocListForLayout(a, layout_idx).len() == self.valueToRocListForLayout(b, layout_idx).len(),
            .list => blk: {
                const a_list = self.valueToRocListForLayout(a, layout_idx);
                const b_list = self.valueToRocListForLayout(b, layout_idx);
                if (a_list.len() != b_list.len()) break :blk false;
                const elem_layout = self.listElemLayout(layout_idx);
                const elem_size = self.helper.sizeOf(elem_layout);
                if (elem_size == 0) break :blk true;
                const elem_desc = if (desc) |list_desc| blk_desc: {
                    const hooks = maybe_hooks orelse
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: descriptor-backed list equality had no frame for layout {d}",
                            .{@intFromEnum(layout_idx)},
                        );
                    break :blk_desc try self.firstNestedBoxyDesc(hooks, list_desc);
                } else null;
                const a_bytes = a_list.bytes orelse break :blk b_list.bytes == null;
                const b_bytes = b_list.bytes orelse break :blk false;
                var i: usize = 0;
                while (i < a_list.len()) : (i += 1) {
                    const offset = i * elem_size;
                    if (!try self.valuesEqualWithDesc(maybe_hooks, .{ .ptr = a_bytes + offset }, .{ .ptr = b_bytes + offset }, elem_layout, elem_desc)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .closure => return self.invariantFailedError(
                "LIR/interpreter invariant violated: function equality survived lowering",
                .{},
            ),
        };
    }

    pub fn performBoxyPayloadDrop(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
        data_ptr: [*]u8,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        try self.performBoxyLayoutDrop(hooks, .{ .ptr = data_ptr }, desc.payload_layout, desc, .decref, count, atomicity);
    }

    pub fn performBoxyLayoutDrop(
        self: *const BoxyRuntime,
        hooks: anytype,
        val: Value,
        layout_idx: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
        op: RcOp,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (op == .incref) {
            self.performConcreteRc(hooks, .incref, layout_idx, val, count, atomicity);
            return;
        }

        if (desc == null) {
            self.performConcreteRc(hooks, op, layout_idx, val, count, atomicity);
            return;
        }

        const resolved_desc = desc.?;
        switch (layout_val.tag) {
            .list, .list_of_zst => try self.performBoxyListDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .struct_ => try self.performBoxyStructDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .tag_union => try self.performBoxyTagUnionDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .box, .box_of_zst => try self.performBoxyBoxDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .scalar, .closure, .erased_callable => {
                self.performConcreteRc(hooks, op, layout_idx, val, count, atomicity);
            },
            .zst, .ptr => {},
        }
    }

    fn performBoxyBoxDrop(
        self: *const BoxyRuntime,
        hooks: anytype,
        val: Value,
        layout_idx: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        op: RcOp,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .box and layout_val.tag != .box_of_zst) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: descriptor-guided box drop expected box layout {d}",
                .{@intFromEnum(layout_idx)},
            );
        }

        const payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, layout_idx, desc) orelse {
            if (layout_val.tag == .box_of_zst) {
                if (self.readBoxedDataPointer(val) != null) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: dynamic box drop for layout {d} had an allocation pointer but no payload descriptor",
                        .{@intFromEnum(layout_idx)},
                    );
                }
                return;
            }
            self.performConcreteRc(hooks, op, layout_idx, val, count, atomicity);
            return;
        };
        const data_ptr = self.readBoxedDataPointer(val) orelse return;
        const payload_sa = self.helper.sizeAlignOf(payload_desc.payload_layout);
        const payload_alignment: u32 = @intCast(payload_sa.alignment.toByteUnits());
        const allocation_contains_refcounted = self.boxyDynamicPayloadAllocationContainsRc(payload_desc, layout_idx);
        const should_drop_payload = allocation_contains_refcounted and switch (op) {
            .incref => false,
            .decref => builtins.utils.isUnique(data_ptr, self.roc_ops),
            .free => true,
        };
        if (should_drop_payload) {
            try self.performBoxyPayloadDrop(hooks, payload_desc, data_ptr, count, atomicity);
        }
        switch (op) {
            .incref => builtins.utils.increfDataPtr(data_ptr, @intCast(count), atomicity, self.roc_ops),
            .decref => builtins.utils.decrefDataPtr(data_ptr, payload_alignment, allocation_contains_refcounted, atomicity, self.roc_ops),
            .free => builtins.utils.freeDataPtrC(data_ptr, payload_alignment, allocation_contains_refcounted, self.roc_ops),
        }
    }

    pub fn boxyBoxAllocationPayloadDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        box_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!?*const LirProgram.BoxyTypeDesc {
        if (desc.payload_layout != box_layout and !self.boxyDescIsBoxSelfForBoxValue(box_layout, desc)) return desc;
        if (try self.firstNestedBoxyDesc(hooks, desc)) |nested_desc| return nested_desc;
        if (self.layout_store.getLayout(box_layout).tag == .box_of_zst) return null;

        // Dynamic storage uses a pointer-sized box layout even when the payload
        // layout is also pointer-sized. In that case the descriptor itself is
        // the only explicit source of the outer allocation's RC header shape.
        if (desc.contains_refcounted) return desc;
        return null;
    }

    fn performBoxyListDrop(
        self: *const BoxyRuntime,
        hooks: anytype,
        val: Value,
        list_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        op: RcOp,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        const elem_desc = try self.firstNestedBoxyDesc(hooks, desc) orelse {
            self.performConcreteRc(hooks, op, list_layout, val, count, atomicity);
            return;
        };

        const rl = self.valueToRocListForLayout(val, list_layout);
        const elem_layout = self.listElemLayout(list_layout);
        const elem_size = self.helper.sizeOf(elem_layout);
        const elem_sa = self.helper.sizeAlignOf(elem_layout);
        const elem_alignment: u32 = if (elem_size == 0) 1 else @intCast(elem_sa.alignment.toByteUnits());
        const elements_refcounted = hooks.layoutContainsRc(elem_layout);
        if (op == .incref) {
            rl.increfWithAtomicity(@intCast(count), elements_refcounted, atomicity, self.roc_ops);
            return;
        }

        const should_drop_elements = switch (op) {
            .incref => unreachable,
            .decref => rl.isUnique(self.roc_ops),
            .free => true,
        };
        if (should_drop_elements) {
            if (rl.getAllocationDataPtr(self.roc_ops)) |source| {
                const allocation_count = rl.getAllocationElementCount(elements_refcounted, self.roc_ops);
                var index: usize = 0;
                while (index < allocation_count) : (index += 1) {
                    const element = if (elem_size == 0) Value.zst else Value{ .ptr = source + index * elem_size };
                    try self.performBoxyLayoutDrop(hooks, element, elem_layout, elem_desc, .decref, count, atomicity);
                }
            }
        }

        builtins.utils.decref(
            rl.getAllocationDataPtr(self.roc_ops),
            rl.capacity_or_alloc_ptr,
            elem_alignment,
            elements_refcounted,
            atomicity,
            self.roc_ops,
        );
    }

    fn performBoxyStructDrop(
        self: *const BoxyRuntime,
        hooks: anytype,
        val: Value,
        struct_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        op: RcOp,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        const struct_layout_val = self.layout_store.getLayout(struct_layout);
        const struct_idx = struct_layout_val.getStruct().idx;
        const struct_data = self.layout_store.getStructData(struct_idx);
        const desc_refs = self.requireBoxyDescRefs(desc.nested_descs);
        var next_desc: usize = 0;

        var original_index: u32 = 0;
        while (original_index < struct_data.fields.count) : (original_index += 1) {
            const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, original_index);
            if (self.helper.sizeOf(field_layout) == 0) continue;
            const field_desc = if (self.layoutNeedsBoxyStructuralDesc(field_layout)) blk: {
                if (next_desc >= desc_refs.len) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: boxy struct drop descriptor for layout {d} was missing nested descriptor {d}",
                        .{ @intFromEnum(struct_layout), next_desc },
                    );
                }
                const resolved = try hooks.resolveDescRef(desc_refs[next_desc]);
                next_desc += 1;
                break :blk resolved;
            } else null;
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, original_index);
            try self.performBoxyLayoutDrop(hooks, val.offset(field_offset), field_layout, field_desc, op, count, atomicity);
        }
    }

    fn performBoxyTagUnionDrop(
        self: *const BoxyRuntime,
        hooks: anytype,
        val: Value,
        union_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        op: RcOp,
        count: u16,
        atomicity: RcAtomicity,
    ) Error!void {
        const tag_base = self.resolveTagUnionBaseValue(val, union_layout);
        const discriminant: u16 = @intCast(self.helper.readTagDiscriminant(tag_base.value, tag_base.layout));
        const actual_payload_layout = self.requireBoxyTagPayloadLayout(tag_base.layout, discriminant);
        if (self.helper.sizeOf(actual_payload_layout) == 0) return;

        const variant = self.findBoxyTagVariantByDiscriminant(desc, discriminant) orelse {
            const ext_discriminant = self.boxyTagExtDiscriminant(desc) orelse {
                _ = self.requireBoxyTagVariantByDiscriminant(desc, discriminant);
                unreachable;
            };
            if (discriminant != ext_discriminant) {
                _ = self.requireBoxyTagVariantByDiscriminant(desc, discriminant);
                unreachable;
            }
            const ext_desc = try self.resolveBoxyTagExtDesc(hooks, desc);
            const ext_value = try self.materializeLocalValue(hooks, tag_base.value, actual_payload_layout);
            try self.performBoxyLayoutDrop(hooks, ext_value, actual_payload_layout, ext_desc, op, count, atomicity);
            return;
        };

        const payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
        switch (payload_layout_val.tag) {
            .struct_ => {
                // A single-argument tag stores its argument as the whole
                // payload area, and its recorded payload descriptor describes
                // that whole value; the per-field pairing below is for
                // multi-argument tags, whose payload area is a struct with one
                // field per argument.
                if (self.findBoxyPayloadDesc(variant, 0)) |first_desc_ref| {
                    const first_desc = try hooks.resolveDescRef(first_desc_ref);
                    if (first_desc.payload_layout == actual_payload_layout) {
                        try self.performBoxyLayoutDrop(hooks, tag_base.value, actual_payload_layout, first_desc, op, count, atomicity);
                        return;
                    }
                }
                const struct_idx = payload_layout_val.getStruct().idx;
                const struct_data = self.layout_store.getStructData(struct_idx);
                var original_index: u32 = 0;
                while (original_index < struct_data.fields.count) : (original_index += 1) {
                    const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, original_index);
                    if (self.helper.sizeOf(field_layout) == 0) continue;
                    const field_desc = if (self.layoutNeedsBoxyStructuralDesc(field_layout)) blk: {
                        const desc_ref = self.findBoxyPayloadDesc(variant, original_index) orelse {
                            return self.invariantFailedError(
                                "LIR/interpreter invariant violated: boxy tag drop descriptor for tag payload {d} was missing",
                                .{original_index},
                            );
                        };
                        break :blk try hooks.resolveDescRef(desc_ref);
                    } else null;
                    const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, original_index);
                    try self.performBoxyLayoutDrop(hooks, tag_base.value.offset(field_offset), field_layout, field_desc, op, count, atomicity);
                }
            },
            else => {
                const payload_desc = if (self.layoutNeedsBoxyStructuralDesc(actual_payload_layout)) blk: {
                    const desc_ref = self.findBoxyPayloadDesc(variant, 0) orelse {
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: boxy tag drop descriptor for single payload was missing",
                            .{},
                        );
                    };
                    break :blk try hooks.resolveDescRef(desc_ref);
                } else null;
                try self.performBoxyLayoutDrop(hooks, tag_base.value, actual_payload_layout, payload_desc, op, count, atomicity);
            },
        }
    }

    pub fn singleFieldPayloadInfo(self: *const BoxyRuntime, layout_idx: layout_mod.Idx) ?SingleFieldPayloadInfo {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_idx = layout_val.getStruct().idx;
        const struct_data = self.layout_store.getStructData(struct_idx);
        const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return .{
            .layout = field.layout,
            .offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
        };
    }

    pub fn unwrapSingleFieldPayloadLayout(self: *const BoxyRuntime, layout_idx: layout_mod.Idx) ?layout_mod.Idx {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
        const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return field.layout;
    }

    pub fn allocBoxyDynamicPayload(
        self: *const BoxyRuntime,
        hooks: anytype,
        payload: Value,
        payload_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const boxed = try hooks.allocValue(target_layout);
        const payload_size = self.helper.sizeOf(desc.payload_layout);
        if (payload_size == 0) {
            self.writeBoxedDataPointer(boxed, null);
            return boxed;
        }

        const payload_sa = self.helper.sizeAlignOf(desc.payload_layout);
        const data_ptr = try hooks.allocRocDataWithRc(
            payload_size,
            @intCast(payload_sa.alignment.toByteUnits()),
            self.boxyDynamicPayloadAllocationContainsRc(desc, target_layout),
        );
        const materialized = try self.materializeConcreteValueToErasedLayout(hooks, payload, payload_layout, desc.payload_layout);
        @memcpy(data_ptr[0..payload_size], materialized.readBytes(payload_size));
        self.writeBoxedDataPointer(boxed, data_ptr);
        return boxed;
    }

    pub fn releaseMovedBoxyDynamicPayload(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: Value,
        source_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const data_ptr = self.readBoxedDataPointer(source) orelse return;
        const payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, source_layout, desc) orelse desc;
        const payload_sa = self.helper.sizeAlignOf(payload_desc.payload_layout);
        builtins.utils.decrefDataPtr(
            data_ptr,
            @intCast(payload_sa.alignment.toByteUnits()),
            self.boxyDynamicPayloadAllocationContainsRc(payload_desc, source_layout),
            .atomic,
            self.roc_ops,
        );
    }

    fn releaseOwnedPayloadBoxesReboxedIntoDynamicResult(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        result: Value,
        result_layout: layout_mod.Idx,
        result_desc: *const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const result_payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, result_layout, result_desc) orelse result_desc;
        const result_ptr = self.readBoxedDataPointer(result) orelse return;
        try self.releaseMovedPayloadBoxesReboxedIntoResult(
            hooks,
            source,
            source_layout,
            source_desc,
            .{ .ptr = result_ptr },
            result_payload_desc.payload_layout,
            result_payload_desc,
        );
    }

    fn releaseMovedPayloadBoxesReboxedIntoResult(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        result: Value,
        result_layout: layout_mod.Idx,
        result_desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const source_layout_val = self.layout_store.getLayout(source_layout);
        const result_layout_val = self.layout_store.getLayout(result_layout);

        switch (source_layout_val.tag) {
            .box, .box_of_zst => {
                if (result_layout_val.tag != .box and result_layout_val.tag != .box_of_zst) {
                    try self.releaseMovedBoxyDynamicPayload(hooks, source, source_layout, source_desc);
                    return;
                }
                const source_ptr = self.readBoxedDataPointer(source) orelse return;
                const result_ptr = self.readBoxedDataPointer(result);
                if (result_ptr != null and result_ptr.? == source_ptr) return;
                const source_allocation_desc = try self.boxyBoxAllocationPayloadDesc(hooks, source_layout, source_desc);
                const drop_desc = source_allocation_desc orelse result_desc orelse source_desc;
                try self.performBoxyLayoutDrop(hooks, source, source_layout, drop_desc, .decref, 1, .atomic);
            },
            .list, .list_of_zst => {
                if (!try self.resultSharesListAllocation(
                    hooks,
                    source,
                    source_layout,
                    source_desc,
                    result,
                    result_layout,
                    result_desc,
                )) {
                    try self.performBoxyLayoutDrop(hooks, source, source_layout, source_desc, .decref, 1, .atomic);
                }
            },
            .struct_ => {
                if (result_layout_val.tag != .struct_) return;
                const source_struct_idx = source_layout_val.getStruct().idx;
                const result_struct_idx = result_layout_val.getStruct().idx;
                const source_data = self.layout_store.getStructData(source_struct_idx);
                const result_data = self.layout_store.getStructData(result_struct_idx);

                var original_index: u32 = 0;
                while (original_index < source_data.fields.count) : (original_index += 1) {
                    const source_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(source_struct_idx, original_index);
                    if (self.helper.sizeOf(source_field_layout) == 0) continue;

                    const field_desc = try self.boxyStructFieldDesc(hooks, source_desc, source_layout, original_index);
                    const resolved_result_desc = result_desc orelse {
                        if (field_desc) |resolved_desc| {
                            const source_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(source_struct_idx, original_index);
                            try self.performBoxyLayoutDrop(
                                hooks,
                                source.offset(source_offset),
                                source_field_layout,
                                resolved_desc,
                                .decref,
                                1,
                                .atomic,
                            );
                        }
                        continue;
                    };
                    const result_field_index = try self.targetStructFieldIndexForSource(
                        source_desc,
                        source_data.fields.count,
                        resolved_result_desc,
                        result_data.fields.count,
                        original_index,
                    ) orelse {
                        const source_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(source_struct_idx, original_index);
                        try self.performBoxyLayoutDrop(
                            hooks,
                            source.offset(source_offset),
                            source_field_layout,
                            field_desc,
                            .decref,
                            1,
                            .atomic,
                        );
                        continue;
                    };
                    const result_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(result_struct_idx, result_field_index);
                    const result_field_desc = try self.boxyStructFieldDesc(
                        hooks,
                        resolved_result_desc,
                        result_layout,
                        result_field_index,
                    );

                    if (field_desc) |resolved_desc| {
                        const source_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(source_struct_idx, original_index);
                        const result_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(result_struct_idx, result_field_index);
                        try self.releaseMovedPayloadBoxesReboxedIntoResult(
                            hooks,
                            source.offset(source_offset),
                            source_field_layout,
                            resolved_desc,
                            result.offset(result_offset),
                            result_field_layout,
                            result_field_desc,
                        );
                    }
                }
            },
            .tag_union => {
                if (result_layout_val.tag != .tag_union and result_layout_val.tag != .box) return;
                const source_base = self.resolveBoxyTagBaseValue(source, source_layout, source_desc);
                const result_base = self.resolveTagUnionBaseValue(result, result_layout);
                const source_disc: u16 = @intCast(self.helper.readTagDiscriminant(source_base.value, source_base.layout));
                const result_disc: u16 = @intCast(self.helper.readTagDiscriminant(result_base.value, result_base.layout));
                if (source_disc != result_disc) return;

                if (self.boxyTagExtDiscriminant(source_desc)) |source_ext_discriminant| {
                    if (source_disc == source_ext_discriminant) {
                        const source_ext_desc = try self.resolveBoxyTagExtDesc(hooks, source_desc);
                        const source_payload_layout = self.requireBoxyTagPayloadLayout(source_base.layout, source_disc);
                        const result_payload_layout = self.requireBoxyTagPayloadLayout(result_base.layout, result_disc);
                        if (self.helper.sizeOf(source_payload_layout) == 0) return;

                        const result_payload_desc = if (result_desc) |resolved| blk: {
                            const result_ext_discriminant = self.boxyTagExtDiscriminant(resolved) orelse break :blk null;
                            if (result_disc != result_ext_discriminant) break :blk null;
                            break :blk try self.resolveBoxyTagExtDesc(hooks, resolved);
                        } else null;

                        try self.releaseMovedPayloadBoxesReboxedIntoResult(
                            hooks,
                            source_base.value,
                            source_payload_layout,
                            source_ext_desc,
                            result_base.value,
                            result_payload_layout,
                            result_payload_desc,
                        );
                        return;
                    }
                }

                const source_variant = self.requireBoxyTagVariantByDiscriminant(source_desc, source_disc);
                const result_variant = if (result_desc) |resolved|
                    self.findBoxyTagVariantByDiscriminant(resolved, result_disc)
                else
                    null;
                const source_payload_layout = self.requireBoxyTagPayloadLayout(source_base.layout, source_disc);
                const result_payload_layout = self.requireBoxyTagPayloadLayout(result_base.layout, result_disc);
                if (self.helper.sizeOf(source_payload_layout) == 0) return;

                if (self.findBoxyPayloadDesc(source_variant, 0)) |first_desc_ref| {
                    const first_desc = try hooks.resolveDescRef(first_desc_ref);
                    if (first_desc.payload_layout == source_payload_layout) {
                        const result_payload_desc = if (result_variant) |variant| blk: {
                            const result_desc_ref = self.findBoxyPayloadDesc(variant, 0) orelse break :blk null;
                            const resolved = try hooks.resolveDescRef(result_desc_ref);
                            const result_payload_layout_tag = self.layout_store.getLayout(result_payload_layout).tag;
                            if (resolved.payload_layout == result_payload_layout or
                                result_payload_layout_tag == .box or
                                result_payload_layout_tag == .box_of_zst)
                            {
                                break :blk resolved;
                            }
                            break :blk null;
                        } else null;
                        try self.releaseMovedPayloadBoxesReboxedIntoResult(
                            hooks,
                            source_base.value,
                            source_payload_layout,
                            first_desc,
                            result_base.value,
                            result_payload_layout,
                            result_payload_desc,
                        );
                        return;
                    }
                }

                const source_payload_layout_val = self.layout_store.getLayout(source_payload_layout);
                const result_payload_layout_val = self.layout_store.getLayout(result_payload_layout);
                if (source_payload_layout_val.tag == .struct_ and result_payload_layout_val.tag == .struct_) {
                    const source_struct_idx = source_payload_layout_val.getStruct().idx;
                    const result_struct_idx = result_payload_layout_val.getStruct().idx;
                    const source_data = self.layout_store.getStructData(source_struct_idx);

                    var original_index: u32 = 0;
                    while (original_index < source_data.fields.count) : (original_index += 1) {
                        const source_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(source_struct_idx, original_index);
                        const result_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(result_struct_idx, original_index);
                        if (self.helper.sizeOf(source_field_layout) == 0) continue;
                        if (!self.layoutNeedsBoxyStructuralDesc(source_field_layout)) continue;
                        const field_desc_ref = self.findBoxyPayloadDesc(source_variant, original_index) orelse {
                            return self.invariantFailedError(
                                "LIR/interpreter invariant violated: moved source tag descriptor for tag payload {d} was missing",
                                .{original_index},
                            );
                        };
                        const field_desc = try hooks.resolveDescRef(field_desc_ref);
                        const result_field_desc = if (result_variant) |variant| blk: {
                            const result_desc_ref = self.findBoxyPayloadDesc(variant, original_index) orelse break :blk null;
                            break :blk try hooks.resolveDescRef(result_desc_ref);
                        } else null;
                        const source_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(source_struct_idx, original_index);
                        const result_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(result_struct_idx, original_index);
                        try self.releaseMovedPayloadBoxesReboxedIntoResult(
                            hooks,
                            source_base.value.offset(source_offset),
                            source_field_layout,
                            field_desc,
                            result_base.value.offset(result_offset),
                            result_field_layout,
                            result_field_desc,
                        );
                    }
                    return;
                }

                if (self.layoutNeedsBoxyStructuralDesc(source_payload_layout)) {
                    const payload_desc_ref = self.findBoxyPayloadDesc(source_variant, 0) orelse {
                        return self.invariantFailedError(
                            "LIR/interpreter invariant violated: moved source tag descriptor for single payload was missing",
                            .{},
                        );
                    };
                    const payload_desc = try hooks.resolveDescRef(payload_desc_ref);
                    const result_payload_desc = if (result_variant) |variant| blk: {
                        const result_desc_ref = self.findBoxyPayloadDesc(variant, 0) orelse break :blk null;
                        break :blk try hooks.resolveDescRef(result_desc_ref);
                    } else null;
                    try self.releaseMovedPayloadBoxesReboxedIntoResult(
                        hooks,
                        source_base.value,
                        source_payload_layout,
                        payload_desc,
                        result_base.value,
                        result_payload_layout,
                        result_payload_desc,
                    );
                }
            },
            else => {},
        }
    }

    fn decrefMovedBoxySourceIfReboxed(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        result: Value,
        result_layout: layout_mod.Idx,
    ) Error!void {
        const source_tag = self.layout_store.getLayout(source_layout).tag;
        const result_tag = self.layout_store.getLayout(result_layout).tag;
        const source_is_box = source_tag == .box or source_tag == .box_of_zst;
        const result_is_box = result_tag == .box or result_tag == .box_of_zst;
        if (!source_is_box or !result_is_box) return;

        const source_ptr = self.readBoxedDataPointer(source) orelse return;
        const result_ptr = self.readBoxedDataPointer(result);
        if (result_ptr != null and result_ptr.? == source_ptr) return;

        try self.releaseMovedBoxyDynamicPayload(hooks, source, source_layout, source_desc);
    }

    pub fn constructBoxyTagValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
        tag_name: base.StringLiteral.Idx,
        payload: ?Value,
        payload_layout: layout_mod.Idx,
        payload_desc: ?*const LirProgram.BoxyTypeDesc,
        payload_mode: LIR.BoxyTransferMode,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        if (payload) |payload_value| {
            switch (payload_mode) {
                .move => {},
                .borrow, .copy => try self.performBoxyLayoutDrop(
                    hooks,
                    payload_value,
                    payload_layout,
                    payload_desc,
                    .incref,
                    1,
                    .atomic,
                ),
            }
        }

        if (self.findLocalBoxyTagVariant(desc, tag_name)) |variant| {
            const allocated = try self.allocTagValue(hooks, desc.payload_layout);
            if (self.helper.sizeOf(allocated.base_layout) > 0) {
                self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, variant.discriminant);
            }
            if (payload) |payload_value| {
                if (payload_desc) |desc_for_payload| {
                    try self.writeBoxyPayloadToDestination(
                        hooks,
                        allocated.base,
                        variant.payload_layout,
                        payload_value,
                        payload_layout,
                        desc_for_payload,
                    );
                } else {
                    try self.writeConstructedVariantPayload(hooks, allocated.base, variant, payload_value, payload_layout);
                }
            } else if (self.helper.sizeOf(variant.payload_layout) != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy tag {s} required a payload but construction had none",
                    .{self.store.getString(tag_name)},
                );
            }
            const result = try self.allocBoxyDynamicPayload(hooks, allocated.outer, desc.payload_layout, desc, target_layout);
            try self.releaseOwnedPayloadBoxesReboxedIntoDynamicResult(
                hooks,
                allocated.outer,
                desc.payload_layout,
                desc,
                result,
                target_layout,
                desc,
            );
            return result;
        }

        const ext_discriminant = self.boxyTagExtDiscriminant(desc) orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy descriptor had no tag variant named {s}",
                .{self.store.getString(tag_name)},
            );
        };
        const ext_desc = try self.resolveBoxyTagExtDesc(hooks, desc);
        const ext_payload_layout = self.requireBoxyTagPayloadLayout(desc.payload_layout, ext_discriminant);
        const ext_value = try self.constructBoxyTagValue(
            hooks,
            ext_desc,
            tag_name,
            payload,
            payload_layout,
            payload_desc,
            .move,
            ext_payload_layout,
        );
        const allocated = try self.allocTagValue(hooks, desc.payload_layout);
        if (self.helper.sizeOf(allocated.base_layout) > 0) {
            self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, ext_discriminant);
        }
        try self.writeVariantPayloadValue(hooks, allocated.base, ext_payload_layout, ext_value, ext_payload_layout);
        const result = try self.allocBoxyDynamicPayload(hooks, allocated.outer, desc.payload_layout, desc, target_layout);
        try self.releaseOwnedPayloadBoxesReboxedIntoDynamicResult(
            hooks,
            allocated.outer,
            desc.payload_layout,
            desc,
            result,
            target_layout,
            desc,
        );
        return result;
    }

    /// Write a freshly constructed tag payload into its descriptor slot. The
    /// payload value was prepared for this descriptor's conventions, so the
    /// variant's recorded payload descriptors are its description; use them to
    /// guide any conversion the slot layout requires (e.g. boxing a by-value
    /// field into a descriptor-backed box).
    fn writeConstructedVariantPayload(
        self: *const BoxyRuntime,
        hooks: anytype,
        destination: Value,
        variant: *const LirProgram.BoxyTagVariant,
        payload_value: Value,
        payload_layout: layout_mod.Idx,
    ) Error!void {
        const expected_layout = variant.payload_layout;
        if (self.helper.sizeOf(expected_layout) == 0) return;
        if (expected_layout == payload_layout or variant.payload_descs.len == 0) {
            return try self.writeVariantPayloadValue(hooks, destination, expected_layout, payload_value, payload_layout);
        }

        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const payload_layout_val = self.layout_store.getLayout(payload_layout);
        if (expected_layout_val.tag == .struct_ and payload_layout_val.tag == .struct_) {
            // A single-argument tag's recorded payload descriptor describes
            // the whole payload area; the per-field pairing below is for
            // multi-argument tags.
            if (self.findBoxyPayloadDesc(variant, 0)) |first_desc_ref| single: {
                const first_desc = try hooks.resolveDescRef(first_desc_ref);
                if (first_desc.payload_layout != expected_layout) break :single;
                const materialized = try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                    hooks,
                    payload_value,
                    payload_layout,
                    first_desc,
                    first_desc,
                    expected_layout,
                );
                destination.copyFrom(materialized, self.helper.sizeOf(expected_layout));
                return;
            }
            const expected_struct_idx = expected_layout_val.getStruct().idx;
            const actual_struct_idx = payload_layout_val.getStruct().idx;
            const expected_data = self.layout_store.getStructData(expected_struct_idx);
            var original_index: u32 = 0;
            while (original_index < expected_data.fields.count) : (original_index += 1) {
                const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
                if (self.helper.sizeOf(expected_field_layout) == 0) continue;
                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, original_index);
                const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, original_index);
                const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_struct_idx, original_index);
                const field_value = payload_value.offset(actual_field_offset);
                const materialized = if (self.findBoxyPayloadDesc(variant, original_index)) |field_desc_ref| blk: {
                    const field_desc = try hooks.resolveDescRef(field_desc_ref);
                    break :blk try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                        hooks,
                        field_value,
                        actual_field_layout,
                        field_desc,
                        field_desc,
                        expected_field_layout,
                    );
                } else try self.materializeLocalValue(
                    hooks,
                    self.normalizeValueToLayout(field_value, actual_field_layout, expected_field_layout),
                    expected_field_layout,
                );
                destination.offset(expected_field_offset).copyFrom(materialized, self.helper.sizeOf(expected_field_layout));
            }
            return;
        }

        if (self.findBoxyPayloadDesc(variant, 0)) |payload_desc_ref| {
            const slot_desc = try hooks.resolveDescRef(payload_desc_ref);
            const materialized = try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                payload_value,
                payload_layout,
                slot_desc,
                slot_desc,
                expected_layout,
            );
            destination.copyFrom(materialized, self.helper.sizeOf(expected_layout));
            return;
        }

        try self.writeVariantPayloadValue(hooks, destination, expected_layout, payload_value, payload_layout);
    }

    pub fn readBoxyTagPayloadByName(
        self: *const BoxyRuntime,
        hooks: anytype,
        source_value: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        tag_name: base.StringLiteral.Idx,
        payload_index: u32,
        target_layout: layout_mod.Idx,
    ) Error!BoxyTagPayloadRead {
        const tag_base = self.resolveBoxyTagBaseValue(source_value, source_layout, source_desc);
        const disc = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);

        if (self.findLocalBoxyTagVariant(source_desc, tag_name)) |variant| {
            if (disc != variant.discriminant) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy tag payload access expected discriminant {d} but observed {d}",
                    .{ variant.discriminant, disc },
                );
            }
            const actual_payload_layout = self.requireBoxyTagPayloadLayout(tag_base.layout, variant.discriminant);
            const payload_desc_ref = self.findBoxyPayloadDesc(variant, payload_index);
            const payload_value = if (payload_desc_ref) |desc_ref| blk: {
                const payload_desc = try hooks.resolveDescRef(desc_ref);
                // A single-argument tag's recorded payload descriptor
                // describes the whole payload area, not a field of it.
                const raw_payload = if (payload_index == 0 and payload_desc.payload_layout == actual_payload_layout)
                    RawBoxyTagPayloadRead{
                        .value = try self.materializeLocalValue(hooks, tag_base.value, actual_payload_layout),
                        .layout = actual_payload_layout,
                    }
                else
                    try self.readRawBoxyTagPayloadValue(hooks, tag_base.value, actual_payload_layout, payload_index);
                break :blk try self.materializeBoxyPayloadToLayout(
                    hooks,
                    raw_payload.value,
                    raw_payload.layout,
                    payload_desc,
                    target_layout,
                );
            } else no_desc: {
                break :no_desc try self.readBoxyTagPayloadValue(hooks, tag_base.value, actual_payload_layout, payload_index, target_layout);
            };
            return .{
                .value = payload_value,
                .desc = payload_desc_ref,
            };
        }

        const ext_discriminant = self.boxyTagExtDiscriminant(source_desc) orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy descriptor had no tag variant named {s}",
                .{self.store.getString(tag_name)},
            );
        };
        if (disc != ext_discriminant) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy tag payload access expected row-extension discriminant {d} but observed {d}",
                .{ ext_discriminant, disc },
            );
        }

        const ext_desc = try self.resolveBoxyTagExtDesc(hooks, source_desc);
        const ext_payload_layout = self.requireBoxyTagPayloadLayout(source_desc.payload_layout, ext_discriminant);
        const ext_value = try self.materializeLocalValue(hooks, tag_base.value, ext_payload_layout);
        return try self.readBoxyTagPayloadByName(hooks, ext_value, ext_payload_layout, ext_desc, tag_name, payload_index, target_layout);
    }

    pub fn readBoxyTagPayloadValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        tag_base: Value,
        actual_payload_layout: layout_mod.Idx,
        payload_index: u32,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        if (payload_index == 0 and actual_payload_layout == target_layout) {
            return try self.materializeLocalValue(hooks, tag_base, target_layout);
        }
        const raw_payload = try self.readRawBoxyTagPayloadValue(hooks, tag_base, actual_payload_layout, payload_index);
        const payload_value = try self.coerceExplicitRefValueToLayout(hooks, raw_payload.value, raw_payload.layout, target_layout);
        return try self.materializeLocalValue(hooks, payload_value, target_layout);
    }

    fn readActiveTagPayloadValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        tag_value: Value,
        tag_layout: layout_mod.Idx,
    ) Error!RawBoxyTagPayloadRead {
        const discriminant = if (self.helper.sizeOf(tag_layout) == 0)
            @as(u16, 0)
        else
            self.helper.readTagDiscriminant(tag_value, tag_layout);
        const payload_layout = self.requireBoxyTagPayloadLayout(tag_layout, discriminant);
        return try self.readRawBoxyTagPayloadValue(hooks, tag_value, payload_layout, 0);
    }

    pub fn readRawBoxyTagPayloadValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        tag_base: Value,
        actual_payload_layout: layout_mod.Idx,
        payload_index: u32,
    ) Error!RawBoxyTagPayloadRead {
        const payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
        switch (payload_layout_val.tag) {
            .struct_ => {
                const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                    payload_layout_val.getStruct().idx,
                    payload_index,
                );
                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(
                    payload_layout_val.getStruct().idx,
                    payload_index,
                );
                return .{
                    .value = try self.materializeLocalValue(hooks, tag_base.offset(field_offset), actual_field_layout),
                    .layout = actual_field_layout,
                };
            },
            else => {
                if (builtin.mode == .Debug and payload_index != 0) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: scalar boxy tag payload access requested payload_idx {d} from non-struct payload layout {d}",
                        .{ payload_index, @intFromEnum(actual_payload_layout) },
                    );
                }
                return .{
                    .value = try self.materializeLocalValue(hooks, tag_base, actual_payload_layout),
                    .layout = actual_payload_layout,
                };
            },
        }
    }

    pub fn materializeBoxyPayloadToLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (actual_layout == expected_layout) {
            return try self.materializeLocalValue(hooks, value, expected_layout);
        }

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if ((expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst) and
            actual_layout_val.tag != .box and actual_layout_val.tag != .box_of_zst)
        {
            const payload_desc = desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: concrete payload layout {d} needed descriptor-guided boxing into layout {d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                );
            };
            return try self.allocBoxyDynamicPayload(hooks, value, actual_layout, payload_desc, expected_layout);
        }
        if (actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst) {
            if (desc) |box_desc| {
                const payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, actual_layout, box_desc);
                const payload_layout = if (payload_desc) |resolved| resolved.payload_layout else expected_layout;
                const payload_size = self.helper.sizeOf(payload_layout);
                const data_ptr = self.readBoxedDataPointer(value);
                if (data_ptr == null) {
                    if (payload_size == 0) return try self.materializeLocalValue(hooks, Value.zst, expected_layout);
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: descriptor-backed box layout {d} had null payload pointer for nonzero payload layout {d}",
                        .{ @intFromEnum(actual_layout), @intFromEnum(payload_layout) },
                    );
                }
                return try self.materializeBoxyPayloadToLayout(
                    hooks,
                    .{ .ptr = data_ptr.? },
                    payload_layout,
                    payload_desc,
                    expected_layout,
                );
            }
        }
        if (actual_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_) {
            const payload_desc = desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy struct payload layout {d} needed descriptor-guided materialization into layout {d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                );
            };
            return try self.materializeBoxyStructPayloadToLayout(hooks, value, actual_layout, payload_desc, expected_layout);
        }

        if (self.singleFieldPayloadInfo(expected_layout)) |target_field| {
            const target = try hooks.allocValue(expected_layout);
            const field_size = self.helper.sizeOf(target_field.layout);
            if (field_size == 0) return target;
            const materialized_field = try self.materializeBoxyPayloadToLayout(
                hooks,
                value,
                actual_layout,
                desc,
                target_field.layout,
            );
            target.offset(target_field.offset).copyFrom(materialized_field, field_size);
            return target;
        }

        if (actual_layout_val.tag == .zst and expected_layout_val.tag == .tag_union) {
            const payload_desc = desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized boxy tag payload needed descriptor-guided materialization into layout {d}",
                    .{@intFromEnum(expected_layout)},
                );
            };
            const variants = self.requireBoxyTagVariants(payload_desc.tag_variants);
            if (variants.len != 1) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized boxy tag payload descriptor for layout {d} had {d} variants",
                    .{ @intFromEnum(expected_layout), variants.len },
                );
            }
            const variant = variants[0];
            if (variant.payload_layout != .zst or variant.payload_descs.len != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized boxy tag payload descriptor variant {s} had nonzero payload metadata",
                    .{self.store.getString(variant.name)},
                );
            }
            const target = try self.allocTagValue(hooks, expected_layout);
            const expected_payload_layout = self.requireBoxyTagPayloadLayout(target.base_layout, variant.discriminant);
            if (self.helper.sizeOf(expected_payload_layout) != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized boxy tag payload materialized into nonzero target payload layout {d}",
                    .{@intFromEnum(expected_payload_layout)},
                );
            }
            if (self.helper.sizeOf(target.base_layout) > 0) {
                self.helper.writeTagDiscriminant(target.base, target.base_layout, variant.discriminant);
            } else if (variant.discriminant != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized boxy tag payload wrote nonzero discriminant {d} into zero-sized layout {d}",
                    .{ variant.discriminant, @intFromEnum(target.base_layout) },
                );
            }
            return target.outer;
        }

        const actual_is_tag = actual_layout_val.tag == .tag_union or actual_layout_val.tag == .box;
        const expected_is_tag = expected_layout_val.tag == .tag_union or expected_layout_val.tag == .box;
        if (actual_is_tag and expected_is_tag) {
            const payload_desc = desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy tag payload layout {d} needed descriptor-guided materialization into layout {d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                );
            };
            return try self.materializeBoxyTagPayloadToLayout(hooks, value, actual_layout, payload_desc, expected_layout);
        }
        if (actual_layout_val.tag == .tag_union and !expected_is_tag) {
            const payload_desc = desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: tag wrapper payload layout {d} needed descriptor-guided materialization into layout {d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                );
            };
            if (payload_desc.tag_variants.len != 0) {
                return try self.materializeBoxyTagPayloadToNonTagLayout(
                    hooks,
                    value,
                    actual_layout,
                    payload_desc,
                    expected_layout,
                );
            }
            if (payload_desc.tag_variants.len == 0 and payload_desc.payload_layout != actual_layout) {
                const raw_payload = try self.readActiveTagPayloadValue(hooks, value, actual_layout);
                return try self.materializeBoxyPayloadToLayout(
                    hooks,
                    raw_payload.value,
                    raw_payload.layout,
                    payload_desc,
                    expected_layout,
                );
            }
        }

        const coerced = try self.coerceExplicitRefValueToLayout(hooks, value, actual_layout, expected_layout);
        return try self.materializeLocalValue(hooks, coerced, expected_layout);
    }

    fn materializeBoxyTagPayloadToNonTagLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_base = self.resolveBoxyTagBaseValue(value, actual_layout, source_desc);
        const source_discriminant = if (self.helper.sizeOf(actual_base.layout) == 0)
            @as(u16, 0)
        else
            self.helper.readTagDiscriminant(actual_base.value, actual_base.layout);
        const source_variant = self.requireBoxyTagVariantByDiscriminant(source_desc, source_discriminant);
        const actual_payload_layout = self.requireBoxyTagPayloadLayout(actual_base.layout, source_discriminant);
        const source_payload_desc = if (self.findBoxyPayloadDesc(source_variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const actual_payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const payload = if (actual_payload_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_)
            RawBoxyTagPayloadRead{
                .value = try self.materializeLocalValue(hooks, actual_base.value, actual_payload_layout),
                .layout = actual_payload_layout,
            }
        else
            try self.readRawBoxyTagPayloadValue(hooks, actual_base.value, actual_payload_layout, 0);
        return try self.materializeBoxyPayloadToLayout(
            hooks,
            payload.value,
            payload.layout,
            source_payload_desc,
            expected_layout,
        );
    }

    pub fn materializeBoxyPayloadToLayoutWithOptionalSourceDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if ((actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst) and
            (expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst))
        {
            return try self.materializeBoxyListPayloadToLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_desc,
                expected_layout,
            );
        }

        if (source_desc) |resolved_source_desc| {
            return try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                resolved_source_desc,
                target_desc,
                expected_layout,
            );
        }

        return try self.materializeConcreteValueToErasedLayout(
            hooks,
            value,
            actual_layout,
            expected_layout,
        );
    }

    pub fn materializeBoxyListPayloadToLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const source_list = self.valueToRocListForLayout(value, actual_layout);
        const source_elem_desc = if (source_desc) |resolved_source_desc|
            try self.firstNestedBoxyDesc(hooks, resolved_source_desc)
        else
            null;
        const target_elem_desc = try self.firstNestedBoxyDesc(hooks, target_desc);
        // The list's storage layout is the only truth about element stride and
        // shape. The element descriptor may legitimately describe the payload
        // INSIDE a boxed element (payload-direct convention), so it must not
        // decide how the buffer is walked.
        const source_elem_layout = self.listElemLayout(actual_layout);
        const target_elem_layout = self.listElemLayout(expected_layout);
        const source_elem_size = self.helper.sizeOf(source_elem_layout);
        const target_elem_size = self.helper.sizeOf(target_elem_layout);

        if (actual_layout == expected_layout and
            ((source_elem_desc == null and target_elem_desc == null) or
                (source_elem_desc != null and target_elem_desc != null and source_elem_desc.? == target_elem_desc.?)))
        {
            return try self.materializeLocalValue(hooks, value, expected_layout);
        }

        if (target_elem_size == 0) {
            return try self.rocListToValue(hooks, canonicalZstList(source_list.len()), expected_layout);
        }

        const target_capacity = @max(source_list.getCapacity(), source_list.len());
        if (target_capacity == 0) {
            return try self.rocListToValue(hooks, canonicalZstList(0), expected_layout);
        }

        const total_elem_bytes = target_elem_size * target_capacity;
        const target_sa = self.helper.sizeAlignOf(target_elem_layout);
        const target_alignment: u32 = @intCast(target_sa.alignment.toByteUnits());
        const elems_rc = hooks.layoutContainsRc(target_elem_layout);
        const target_bytes = try hooks.allocRocDataWithRc(total_elem_bytes, target_alignment, elems_rc);
        errdefer builtins.utils.decref(
            target_bytes,
            builtins.list.RocList.encodeCapacity(source_list.len()),
            target_alignment,
            elems_rc,
            .atomic,
            self.roc_ops,
        );

        if (source_list.len() == 0) {
            return try self.rocListToValue(hooks, .{
                .bytes = target_bytes,
                .length = 0,
                .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(target_capacity),
            }, expected_layout);
        }

        const source_bytes = if (source_elem_size == 0)
            null
        else
            source_list.bytes orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: non-empty boxy list payload had null source bytes for layout {d}",
                    .{@intFromEnum(actual_layout)},
                );
            };

        var index: usize = 0;
        while (index < source_list.len()) : (index += 1) {
            const source_elem = if (source_elem_size == 0)
                Value.zst
            else
                Value{ .ptr = source_bytes.? + index * source_elem_size };
            const materialized = if (target_elem_desc) |resolved_target_elem_desc|
                try self.materializeBoxyPayloadToLayoutWithOptionalSourceDesc(
                    hooks,
                    source_elem,
                    source_elem_layout,
                    source_elem_desc,
                    resolved_target_elem_desc,
                    target_elem_layout,
                )
            else
                try self.materializeBoxyPayloadToLayout(
                    hooks,
                    source_elem,
                    source_elem_layout,
                    source_elem_desc,
                    target_elem_layout,
                );
            try self.retainBorrowedMaterializedValue(
                hooks,
                source_elem,
                source_elem_layout,
                source_elem_desc,
                materialized,
                target_elem_layout,
                target_elem_desc,
            );
            @memcpy(target_bytes[index * target_elem_size ..][0..target_elem_size], materialized.readBytes(target_elem_size));
        }

        return try self.rocListToValue(hooks, .{
            .bytes = target_bytes,
            .length = source_list.len(),
            .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(target_capacity),
        }, expected_layout);
    }

    fn retainBorrowedMaterializedValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: Value,
        source_layout: layout_mod.Idx,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        target: Value,
        target_layout: layout_mod.Idx,
        target_desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        if (self.helper.sizeOf(target_layout) == 0) return;
        if (source_layout == target_layout and source_desc == target_desc) {
            try self.performBoxyLayoutDrop(hooks, target, target_layout, target_desc, .incref, 1, .atomic);
            return;
        }

        const source_layout_val = self.layout_store.getLayout(source_layout);
        const target_layout_val = self.layout_store.getLayout(target_layout);
        const source_is_box = source_layout_val.tag == .box or source_layout_val.tag == .box_of_zst;
        const target_is_box = target_layout_val.tag == .box or target_layout_val.tag == .box_of_zst;

        if (source_is_box and target_is_box) {
            const source_ptr = self.readBoxedDataPointer(source);
            const target_ptr = self.readBoxedDataPointer(target);
            if (source_ptr == target_ptr) {
                try self.performBoxyLayoutDrop(hooks, target, target_layout, target_desc, .incref, 1, .atomic);
                return;
            }
        }

        if (source_is_box) {
            const resolved_source_desc = source_desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: borrowed box materialization lacked a source descriptor",
                    .{},
                );
            };
            const target_allocation_desc = if (target_is_box) blk: {
                const resolved_target_desc = target_desc orelse {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: borrowed box materialization lacked a target descriptor",
                        .{},
                    );
                };
                break :blk try self.boxyBoxAllocationPayloadDesc(hooks, target_layout, resolved_target_desc);
            } else target_desc;
            const source_payload = try self.boxyPayloadValueForTargetDesc(
                hooks,
                source,
                source_layout,
                resolved_source_desc,
                target_allocation_desc,
            );

            if (target_is_box) {
                const allocation_desc = target_allocation_desc orelse {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: allocated borrowed target box lacked a payload descriptor",
                        .{},
                    );
                };
                const target_ptr = self.readBoxedDataPointer(target) orelse {
                    if (self.helper.sizeOf(allocation_desc.payload_layout) == 0) return;
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: allocated borrowed target box had null payload storage",
                        .{},
                    );
                };
                try self.retainBorrowedMaterializedValue(
                    hooks,
                    source_payload.value,
                    source_payload.layout,
                    source_payload.desc,
                    .{ .ptr = target_ptr },
                    allocation_desc.payload_layout,
                    allocation_desc,
                );
                return;
            }

            try self.retainBorrowedMaterializedValue(
                hooks,
                source_payload.value,
                source_payload.layout,
                source_payload.desc,
                target,
                target_layout,
                target_desc,
            );
            return;
        }

        if (target_is_box) {
            const resolved_target_desc = target_desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: borrowed target box lacked a descriptor",
                    .{},
                );
            };
            const allocation_desc = try self.boxyBoxAllocationPayloadDesc(hooks, target_layout, resolved_target_desc) orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: allocated borrowed target box lacked a payload descriptor",
                    .{},
                );
            };
            const target_ptr = self.readBoxedDataPointer(target) orelse {
                if (self.helper.sizeOf(allocation_desc.payload_layout) == 0) return;
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: allocated borrowed target box had null payload storage",
                    .{},
                );
            };
            try self.retainBorrowedMaterializedValue(
                hooks,
                source,
                source_layout,
                source_desc,
                .{ .ptr = target_ptr },
                allocation_desc.payload_layout,
                allocation_desc,
            );
            return;
        }

        if ((source_layout_val.tag == .list or source_layout_val.tag == .list_of_zst) and
            (target_layout_val.tag == .list or target_layout_val.tag == .list_of_zst))
        {
            const source_list = self.valueToRocListForLayout(source, source_layout);
            const target_list = self.valueToRocListForLayout(target, target_layout);
            if (source_list.getAllocationDataPtr(self.roc_ops) == target_list.getAllocationDataPtr(self.roc_ops) and
                source_list.getAllocationDataPtr(self.roc_ops) != null)
            {
                try self.performBoxyLayoutDrop(hooks, target, target_layout, target_desc, .incref, 1, .atomic);
            }
            return;
        }

        if (source_layout_val.tag == .struct_ and target_layout_val.tag == .struct_) {
            const source_struct_idx = source_layout_val.getStruct().idx;
            const target_struct_idx = target_layout_val.getStruct().idx;
            const source_data = self.layout_store.getStructData(source_struct_idx);
            const target_data = self.layout_store.getStructData(target_struct_idx);
            if ((source_desc == null or target_desc == null) and source_data.fields.count != target_data.fields.count) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: borrowed struct materialization changed field count without descriptors",
                    .{},
                );
            }

            var target_field_index: u32 = 0;
            while (target_field_index < target_data.fields.count) : (target_field_index += 1) {
                const source_is_named = if (source_desc) |resolved| resolved.field_names.len != 0 else false;
                const target_is_named = if (target_desc) |resolved| resolved.field_names.len != 0 else false;
                const source_field_index = if (source_is_named and target_is_named)
                    try self.sourceStructFieldIndexForTarget(
                        source_desc.?,
                        source_data.fields.count,
                        target_desc.?,
                        target_data.fields.count,
                        target_field_index,
                    )
                else
                    target_field_index;
                const source_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(source_struct_idx, source_field_index);
                const target_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(target_struct_idx, target_field_index);
                if (self.helper.sizeOf(target_field_layout) == 0) continue;
                const source_field_desc = if (source_desc) |resolved|
                    try self.boxyStructFieldDesc(hooks, resolved, source_layout, source_field_index)
                else
                    null;
                const target_field_desc = if (target_desc) |resolved|
                    try self.boxyStructFieldDesc(hooks, resolved, target_layout, target_field_index)
                else
                    null;
                try self.retainBorrowedMaterializedValue(
                    hooks,
                    source.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(source_struct_idx, source_field_index)),
                    source_field_layout,
                    source_field_desc,
                    target.offset(self.layout_store.getStructFieldOffsetByOriginalIndex(target_struct_idx, target_field_index)),
                    target_field_layout,
                    target_field_desc,
                );
            }
            return;
        }

        try self.performBoxyLayoutDrop(hooks, target, target_layout, target_desc, .incref, 1, .atomic);
    }

    fn materializeMovedBoxyListPayloadToLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const source_list = self.valueToRocListForLayout(value, actual_layout);
        if (!source_list.isUnique(self.roc_ops) or source_list.isSeamlessSlice()) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: moved boxy list materialization requires a unique non-slice source",
                .{},
            );
        }

        const source_elem_desc = if (source_desc) |desc| try self.firstNestedBoxyDesc(hooks, desc) else null;
        const target_elem_desc = try self.firstNestedBoxyDesc(hooks, target_desc);
        const source_elem_layout = self.listElemLayout(actual_layout);
        const target_elem_layout = self.listElemLayout(expected_layout);
        const source_elem_size = self.helper.sizeOf(source_elem_layout);
        const target_elem_size = self.helper.sizeOf(target_elem_layout);
        if (target_elem_size == 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: moved boxy list materialization targeted zero-sized element storage",
                .{},
            );
        }

        const target_capacity = @max(source_list.getCapacity(), source_list.len());
        const target_sa = self.helper.sizeAlignOf(target_elem_layout);
        const target_alignment: u32 = @intCast(target_sa.alignment.toByteUnits());
        const target_elems_rc = hooks.layoutContainsRc(target_elem_layout);
        const target_bytes = if (target_capacity == 0)
            null
        else
            try hooks.allocRocDataWithRc(target_elem_size * target_capacity, target_alignment, target_elems_rc);
        errdefer if (target_bytes) |bytes| builtins.utils.freeDataPtrC(
            bytes,
            target_alignment,
            target_elems_rc,
            self.roc_ops,
        );

        if (source_list.len() != 0) {
            const source_bytes = source_list.bytes orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: moved non-empty boxy list had null source bytes",
                    .{},
                );
            };
            const output_bytes = target_bytes orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: moved non-empty boxy list had null target bytes",
                    .{},
                );
            };

            var index: usize = 0;
            while (index < source_list.len()) : (index += 1) {
                const source_elem = if (source_elem_size == 0)
                    Value.zst
                else
                    Value{ .ptr = source_bytes + index * source_elem_size };
                const materialized = try self.materializeCallResult(
                    hooks,
                    source_elem,
                    source_elem_layout,
                    source_elem_desc,
                    target_elem_desc,
                    target_elem_layout,
                );
                @memcpy(output_bytes[index * target_elem_size ..][0..target_elem_size], materialized.value.readBytes(target_elem_size));
            }
        }

        if (source_list.getAllocationDataPtr(self.roc_ops)) |source_allocation| {
            const source_sa = self.helper.sizeAlignOf(source_elem_layout);
            const source_alignment: u32 = if (source_elem_size == 0) 1 else @intCast(source_sa.alignment.toByteUnits());
            builtins.utils.freeDataPtrC(
                source_allocation,
                source_alignment,
                hooks.layoutContainsRc(source_elem_layout),
                self.roc_ops,
            );
        }

        return try self.rocListToValue(hooks, .{
            .bytes = target_bytes,
            .length = source_list.len(),
            .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(target_capacity),
        }, expected_layout);
    }

    pub fn materializeBoxyPayloadToLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if ((actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst) and
            (expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst))
        {
            return try self.materializeBoxyListPayloadToLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_desc,
                expected_layout,
            );
        }
        if (actual_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_) {
            return try self.materializeBoxyStructPayloadToLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_desc,
                expected_layout,
            );
        }

        const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
        if (expected_is_box) {
            const source_payload_desc = if (actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst)
                try self.boxyBoxAllocationPayloadDesc(hooks, actual_layout, source_desc)
            else
                source_desc;
            const target_payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, expected_layout, target_desc);
            if (actual_layout == expected_layout and source_payload_desc == null and target_payload_desc == null) {
                return try self.materializeLocalValue(hooks, value, expected_layout);
            }
            if (actual_layout == expected_layout and
                source_payload_desc != null and
                target_payload_desc != null and
                (source_payload_desc.? == target_payload_desc.? or
                    (source_payload_desc.?.payload_layout == target_payload_desc.?.payload_layout and
                        !self.layoutNeedsBoxyStructuralDesc(source_payload_desc.?.payload_layout))))
            {
                return try self.materializeLocalValue(hooks, value, expected_layout);
            }

            const target_allocation_desc = target_payload_desc orelse {
                switch (expected_layout_val.tag) {
                    .box_of_zst => {
                        // The erased target box carries no payload descriptor. A
                        // source value that already holds an allocation pointer (a
                        // box or opaque pointer) only needs relabelling, so preserve
                        // it. A concrete non-box source still has to be stored in a
                        // fresh allocation the box points at, so downstream readers
                        // can recover it through their own descriptor; the source
                        // payload descriptor tells us its shape and refcounting.
                        // Only a payload-free (ZST) source has nothing to carry,
                        // where a canonical null box_of_zst is correct.
                        if (actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst or
                            (actual_layout_val.tag == .scalar and actual_layout_val.getScalar().tag == .opaque_ptr))
                        {
                            return try self.materializeLocalValue(hooks, value, expected_layout);
                        }
                        if (self.helper.sizeOf(actual_layout) == 0) {
                            return try self.allocBoxOfZstValue(hooks, expected_layout);
                        }
                        if (source_payload_desc) |sdesc| {
                            return try self.allocBoxyDynamicPayload(hooks, value, actual_layout, sdesc, expected_layout);
                        }
                        return try self.allocBoxOfZstValue(hooks, expected_layout);
                    },
                    .box => {
                        const target_payload_layout = expected_layout_val.getIdx();
                        const materialized_payload = try self.materializeBoxyPayloadToLayout(
                            hooks,
                            value,
                            actual_layout,
                            source_desc,
                            target_payload_layout,
                        );
                        return try self.boxBox(hooks, materialized_payload, expected_layout);
                    },
                    else => unreachable,
                }
            };
            const source_payload = try self.boxyPayloadValueForTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_allocation_desc,
            );
            const materialized_payload = try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                source_payload.value,
                source_payload.layout,
                source_payload.desc orelse source_desc,
                target_allocation_desc,
                target_allocation_desc.payload_layout,
            );
            return try self.allocBoxyDynamicPayload(hooks, materialized_payload, target_allocation_desc.payload_layout, target_allocation_desc, expected_layout);
        }

        if (actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst) {
            const source_payload = try self.boxyPayloadValueForTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_desc,
            );
            return try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                source_payload.value,
                source_payload.layout,
                source_payload.desc orelse source_desc,
                target_desc,
                expected_layout,
            );
        }

        const actual_is_tag = actual_layout_val.tag == .tag_union;
        const expected_is_tag = expected_layout_val.tag == .tag_union;
        if (actual_is_tag and expected_is_tag and target_desc.tag_variants.len != 0) {
            return try self.materializeBoxyTagPayloadToLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_desc,
                expected_layout,
            );
        }
        if (actual_layout_val.tag == .tag_union and !expected_is_tag and
            source_desc.tag_variants.len == 0 and source_desc.payload_layout != actual_layout)
        {
            const raw_payload = try self.readActiveTagPayloadValue(hooks, value, actual_layout);
            return try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                raw_payload.value,
                raw_payload.layout,
                source_desc,
                target_desc,
                expected_layout,
            );
        }
        if (actual_is_tag and !expected_is_tag and source_desc.tag_variants.len != 0) {
            return try self.materializeBoxyTagPayloadToNonTagLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_desc,
                expected_layout,
            );
        }
        if (actual_layout_val.tag == .zst and expected_is_tag and target_desc.tag_variants.len != 0) {
            return try self.materializeZstBoxyTagPayloadToLayoutWithTargetDesc(
                hooks,
                source_desc,
                target_desc,
                expected_layout,
            );
        }

        if (self.singleFieldPayloadInfo(expected_layout)) |target_field| {
            const target = try hooks.allocValue(expected_layout);
            const field_size = self.helper.sizeOf(target_field.layout);
            if (field_size == 0) return target;

            const target_field_desc = try self.firstNestedBoxyDesc(hooks, target_desc);
            const materialized_field = if (target_field_desc) |field_desc|
                try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                    hooks,
                    value,
                    actual_layout,
                    source_desc,
                    field_desc,
                    target_field.layout,
                )
            else
                try self.materializeBoxyPayloadToLayout(
                    hooks,
                    value,
                    actual_layout,
                    source_desc,
                    target_field.layout,
                );
            target.offset(target_field.offset).copyFrom(materialized_field, field_size);
            return target;
        }

        return try self.materializeBoxyPayloadToLayout(hooks, value, actual_layout, source_desc, expected_layout);
    }

    fn materializeZstBoxyTagPayloadToLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst) {
            const allocation_desc = try self.boxyBoxAllocationPayloadDesc(hooks, expected_layout, target_desc) orelse {
                if (self.helper.sizeOf(target_desc.payload_layout) == 0) {
                    return try self.allocBoxOfZstValue(hooks, expected_layout);
                }
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized tag materialization target box layout {d} had no allocation descriptor",
                    .{@intFromEnum(expected_layout)},
                );
            };
            if (allocation_desc == target_desc and allocation_desc.payload_layout == expected_layout) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized tag materialization target box descriptor did not identify its payload",
                    .{},
                );
            }
            const payload = try self.materializeZstBoxyTagPayloadToLayoutWithTargetDesc(
                hooks,
                source_desc,
                allocation_desc,
                allocation_desc.payload_layout,
            );
            return try self.allocBoxyDynamicPayload(
                hooks,
                payload,
                allocation_desc.payload_layout,
                allocation_desc,
                expected_layout,
            );
        }

        const source_variants = self.requireBoxyTagVariants(source_desc.tag_variants);
        if (source_variants.len != 1) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: zero-sized source boxy tag payload descriptor for layout {d} had {d} variants",
                .{ @intFromEnum(expected_layout), source_variants.len },
            );
        }
        const source_variant = source_variants[0];
        if (source_variant.payload_layout != .zst) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: zero-sized source boxy tag payload descriptor variant {s} had a nonzero payload layout",
                .{self.store.getString(source_variant.name)},
            );
        }

        const target_variant = self.findLocalBoxyTagVariant(target_desc, source_variant.name) orelse {
            const target_ext_discriminant = self.boxyTagExtDiscriminant(target_desc) orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: target boxy tag descriptor for layout {d} had no variant named {s}",
                    .{ @intFromEnum(expected_layout), self.store.getString(source_variant.name) },
                );
            };
            const target_ext_desc = try self.resolveBoxyTagExtDesc(hooks, target_desc);
            const target = try self.allocTagValue(hooks, expected_layout);
            if (self.helper.sizeOf(target.base_layout) > 0) {
                self.helper.writeTagDiscriminant(target.base, target.base_layout, target_ext_discriminant);
            } else if (target_ext_discriminant != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: zero-sized boxy tag materialization wrote nonzero extension discriminant {d} into zero-sized layout {d}",
                    .{ target_ext_discriminant, @intFromEnum(expected_layout) },
                );
            }
            const ext_slot_layout = self.requireBoxyTagPayloadLayout(target.base_layout, target_ext_discriminant);
            const ext_value = try self.materializeZstBoxyTagPayloadToLayoutWithTargetDesc(
                hooks,
                source_desc,
                target_ext_desc,
                ext_slot_layout,
            );
            try self.writeVariantPayloadValue(hooks, target.base, ext_slot_layout, ext_value, ext_slot_layout);
            return target.outer;
        };

        const target = try self.allocTagValue(hooks, expected_layout);
        if (self.helper.sizeOf(target.base_layout) > 0) {
            self.helper.writeTagDiscriminant(target.base, target.base_layout, target_variant.discriminant);
        } else if (target_variant.discriminant != 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: zero-sized boxy tag materialization wrote nonzero discriminant {d} into zero-sized layout {d}",
                .{ target_variant.discriminant, @intFromEnum(target.base_layout) },
            );
        }

        const expected_payload_layout = self.requireBoxyTagPayloadLayout(target.base_layout, target_variant.discriminant);
        if (self.helper.sizeOf(expected_payload_layout) == 0) return target.outer;
        const source_payload_desc = if (self.findBoxyPayloadDesc(&source_variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const target_payload_desc = if (self.findBoxyPayloadDesc(target_variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const payload = if (target_payload_desc) |resolved_target_desc|
            try self.materializeBoxyPayloadToLayoutWithOptionalSourceDesc(
                hooks,
                Value.zst,
                .zst,
                source_payload_desc,
                resolved_target_desc,
                expected_payload_layout,
            )
        else
            try self.materializeBoxyPayloadToLayout(
                hooks,
                Value.zst,
                .zst,
                source_payload_desc,
                expected_payload_layout,
            );
        try self.writeVariantPayloadValue(
            hooks,
            target.base,
            expected_payload_layout,
            payload,
            expected_payload_layout,
        );
        return target.outer;
    }

    fn materializeBoxyTagPayloadToNonTagLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_base = self.resolveBoxyTagBaseValue(value, actual_layout, source_desc);
        const source_discriminant = if (self.helper.sizeOf(actual_base.layout) == 0)
            @as(u16, 0)
        else
            self.helper.readTagDiscriminant(actual_base.value, actual_base.layout);
        const source_variant = self.requireBoxyTagVariantByDiscriminant(source_desc, source_discriminant);
        const actual_payload_layout = self.requireBoxyTagPayloadLayout(actual_base.layout, source_discriminant);
        const source_payload_desc = if (self.findBoxyPayloadDesc(source_variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const actual_payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const payload = if (actual_payload_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_)
            RawBoxyTagPayloadRead{
                .value = try self.materializeLocalValue(hooks, actual_base.value, actual_payload_layout),
                .layout = actual_payload_layout,
            }
        else
            try self.readRawBoxyTagPayloadValue(hooks, actual_base.value, actual_payload_layout, 0);

        if (source_payload_desc) |payload_desc| {
            return try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                payload.value,
                payload.layout,
                payload_desc,
                target_desc,
                expected_layout,
            );
        }

        return try self.materializeBoxyPayloadToLayoutWithOptionalSourceDesc(
            hooks,
            payload.value,
            payload.layout,
            null,
            target_desc,
            expected_layout,
        );
    }

    pub fn increfBoxyTransferSourceIfCopied(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        layout_idx: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        mode: LIR.BoxyTransferMode,
    ) Error!void {
        switch (mode) {
            .move => {},
            .borrow, .copy => try self.performBoxyLayoutDrop(hooks, value, layout_idx, desc, .incref, 1, .atomic),
        }
    }

    fn boxyPayloadValueForDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        layout_idx: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!BoxyPayloadValue {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag == .box or layout_val.tag == .box_of_zst) {
            const payload_desc = (try self.boxyBoxAllocationPayloadDesc(hooks, layout_idx, desc)) orelse blk: {
                if (layout_val.tag == .box_of_zst) {
                    return .{ .value = Value.zst, .layout = .zst, .desc = null };
                }
                break :blk desc;
            };
            const data_ptr = self.readBoxedDataPointer(value);
            if (data_ptr == null) {
                if (self.helper.sizeOf(payload_desc.payload_layout) == 0) {
                    return .{ .value = Value.zst, .layout = payload_desc.payload_layout, .desc = payload_desc };
                }
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy source box layout {d} had null data for payload layout {d}",
                    .{ @intFromEnum(layout_idx), @intFromEnum(payload_desc.payload_layout) },
                );
            }
            return .{ .value = .{ .ptr = data_ptr.? }, .layout = payload_desc.payload_layout, .desc = payload_desc };
        }
        return .{ .value = value, .layout = layout_idx, .desc = desc };
    }

    pub fn boxyPayloadValueForTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        layout_idx: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_payload_desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!BoxyPayloadValue {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .box and layout_val.tag != .box_of_zst) {
            return .{ .value = value, .layout = layout_idx, .desc = source_desc };
        }

        if (try self.boxyBoxAllocationPayloadDesc(hooks, layout_idx, source_desc)) |source_payload_desc| {
            const data_ptr = self.readBoxedDataPointer(value);
            if (data_ptr == null) {
                if (self.helper.sizeOf(source_payload_desc.payload_layout) == 0) {
                    return .{ .value = Value.zst, .layout = source_payload_desc.payload_layout, .desc = source_payload_desc };
                }
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy source box layout {d} had null data for payload layout {d}",
                    .{ @intFromEnum(layout_idx), @intFromEnum(source_payload_desc.payload_layout) },
                );
            }
            return .{ .value = .{ .ptr = data_ptr.? }, .layout = source_payload_desc.payload_layout, .desc = source_payload_desc };
        }

        if (target_payload_desc) |payload_desc| {
            const data_ptr = self.readBoxedDataPointer(value);
            if (data_ptr == null) {
                if (self.helper.sizeOf(payload_desc.payload_layout) == 0) {
                    return .{ .value = Value.zst, .layout = payload_desc.payload_layout, .desc = payload_desc };
                }
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: target-guided boxy source box layout {d} had null data for payload layout {d}",
                    .{ @intFromEnum(layout_idx), @intFromEnum(payload_desc.payload_layout) },
                );
            }
            return .{ .value = .{ .ptr = data_ptr.? }, .layout = payload_desc.payload_layout, .desc = payload_desc };
        }

        if (layout_val.tag == .box_of_zst) {
            if (self.readBoxedDataPointer(value) != null) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: erased box layout {d} had a dynamic payload but no source or target payload descriptor",
                    .{@intFromEnum(layout_idx)},
                );
            }
            return .{ .value = Value.zst, .layout = .zst, .desc = null };
        }

        return try self.boxyPayloadValueForDesc(hooks, value, layout_idx, source_desc);
    }

    pub fn writeBoxyPayloadToDestination(
        self: *const BoxyRuntime,
        hooks: anytype,
        destination: Value,
        expected_layout: layout_mod.Idx,
        value: Value,
        actual_layout: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const expected_size = self.helper.sizeOf(expected_layout);
        if (expected_size == 0) return;
        if (expected_layout == actual_layout) {
            destination.copyFrom(value, expected_size);
            return;
        }

        if (self.unwrapSingleFieldPayloadLayout(expected_layout)) |field_layout| {
            const expected_layout_val = self.layout_store.getLayout(expected_layout);
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_layout_val.getStruct().idx, 0);
            const field_value = try self.materializeBoxyPayloadToLayout(hooks, value, actual_layout, desc, field_layout);
            destination.offset(field_offset).copyFrom(field_value, self.helper.sizeOf(field_layout));
            return;
        }

        const materialized = try self.materializeBoxyPayloadToLayout(hooks, value, actual_layout, desc, expected_layout);
        destination.copyFrom(materialized, expected_size);
    }

    fn materializeBoxyStructPayloadToLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (actual_layout_val.tag != .struct_ or expected_layout_val.tag != .struct_) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: descriptor-guided struct materialization expected struct layouts, got actual={d} ({s}) expected={d} ({s})",
                .{
                    @intFromEnum(actual_layout),
                    @tagName(actual_layout_val.tag),
                    @intFromEnum(expected_layout),
                    @tagName(expected_layout_val.tag),
                },
            );
        }

        const actual_struct_idx = actual_layout_val.getStruct().idx;
        const expected_struct_idx = expected_layout_val.getStruct().idx;
        const expected_data = self.layout_store.getStructData(expected_struct_idx);
        const desc_refs = self.requireBoxyDescRefs(desc.nested_descs);
        var next_desc: usize = 0;

        const target = try hooks.allocValue(expected_layout);
        var original_index: u32 = 0;
        while (original_index < expected_data.fields.count) : (original_index += 1) {
            const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
            const expected_field_size = self.helper.sizeOf(expected_field_layout);
            const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, original_index);

            const field_desc = if (self.layoutNeedsBoxyStructuralDesc(actual_field_layout)) blk: {
                if (next_desc >= desc_refs.len) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: boxy struct descriptor for layout {d} was missing nested descriptor {d}",
                        .{ @intFromEnum(actual_layout), next_desc },
                    );
                }
                const resolved = try hooks.resolveDescRef(desc_refs[next_desc]);
                next_desc += 1;
                break :blk resolved;
            } else null;

            if (expected_field_size == 0) continue;
            const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, original_index);
            const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_struct_idx, original_index);
            try self.writeBoxyPayloadToDestination(
                hooks,
                target.offset(expected_field_offset),
                expected_field_layout,
                value.offset(actual_field_offset),
                actual_field_layout,
                field_desc,
            );
        }

        return target;
    }

    /// Coerce a fully-concrete value into the erased representation a generic
    /// worker's parameter expects, driven entirely by layouts. This bridges a
    /// cross-module erased-callable ABI seam: the caller module resolved a
    /// closure's argument to a concrete layout while the closure's worker (built
    /// in the providing module against an abstract type) reads that argument in
    /// its erased form. The closure carries no target descriptor at the call, so
    /// the boxing shape is derived from the concrete source layout: every field
    /// the target boxes is a self-describing concrete value whose refcounting is
    /// known from its own layout.
    pub fn materializeConcreteValueToErasedLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (actual_layout == expected_layout) {
            return try self.materializeLocalValue(hooks, value, expected_layout);
        }

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);

        if (actual_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_) {
            const actual_struct_idx = actual_layout_val.getStruct().idx;
            const expected_struct_idx = expected_layout_val.getStruct().idx;
            const expected_data = self.layout_store.getStructData(expected_struct_idx);
            const target = try hooks.allocValue(expected_layout);
            var original_index: u32 = 0;
            while (original_index < expected_data.fields.count) : (original_index += 1) {
                const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
                const expected_field_size = self.helper.sizeOf(expected_field_layout);
                if (expected_field_size == 0) continue;
                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, original_index);
                const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_struct_idx, original_index);
                const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, original_index);
                const field_value = try self.materializeConcreteValueToErasedLayout(
                    hooks,
                    value.offset(actual_field_offset),
                    actual_field_layout,
                    expected_field_layout,
                );
                target.offset(expected_field_offset).copyFrom(field_value, expected_field_size);
            }
            return target;
        }

        if (actual_layout_val.tag == .tag_union and expected_layout_val.tag == .tag_union) {
            const target = try self.allocTagValue(hooks, expected_layout);
            const discriminant = if (self.helper.sizeOf(actual_layout) == 0)
                @as(u16, 0)
            else
                self.helper.readTagDiscriminant(value, actual_layout);

            if (self.helper.sizeOf(target.base_layout) > 0) {
                self.helper.writeTagDiscriminant(target.base, target.base_layout, discriminant);
            } else if (discriminant != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: concrete tag materialization wrote nonzero discriminant {d} into zero-sized layout {d}",
                    .{ discriminant, @intFromEnum(target.base_layout) },
                );
            }

            const expected_payload_layout = self.requireBoxyTagPayloadLayout(target.base_layout, discriminant);
            const expected_payload_size = self.helper.sizeOf(expected_payload_layout);
            if (expected_payload_size == 0) return target.outer;

            const actual_payload_layout = self.requireBoxyTagPayloadLayout(actual_layout, discriminant);
            const actual_payload_layout_val = self.layout_store.getLayout(actual_payload_layout);
            const expected_payload_layout_val = self.layout_store.getLayout(expected_payload_layout);
            const payload = if (actual_payload_layout_val.tag == .struct_ and expected_payload_layout_val.tag == .struct_)
                RawBoxyTagPayloadRead{
                    .value = try self.materializeLocalValue(hooks, value, actual_payload_layout),
                    .layout = actual_payload_layout,
                }
            else
                try self.readRawBoxyTagPayloadValue(hooks, value, actual_payload_layout, 0);
            const materialized_payload = try self.materializeConcreteValueToErasedLayout(
                hooks,
                payload.value,
                payload.layout,
                expected_payload_layout,
            );
            target.base.copyFrom(materialized_payload, expected_payload_size);
            return target.outer;
        }

        if ((expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst) and
            actual_layout_val.tag != .box and actual_layout_val.tag != .box_of_zst)
        {
            var synthesized = LirProgram.BoxyTypeDesc{
                .payload_layout = actual_layout,
                .contains_refcounted = self.layout_store.layoutContainsRefcounted(actual_layout_val),
            };
            return try self.allocBoxyDynamicPayload(hooks, value, actual_layout, &synthesized, expected_layout);
        }

        return try self.coerceExplicitRefValueToLayout(hooks, value, actual_layout, expected_layout);
    }

    fn materializeBoxyStructPayloadToLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (actual_layout_val.tag != .struct_ or expected_layout_val.tag != .struct_) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: target-guided struct materialization expected struct layouts, got actual={d} ({s}) expected={d} ({s})",
                .{
                    @intFromEnum(actual_layout),
                    @tagName(actual_layout_val.tag),
                    @intFromEnum(expected_layout),
                    @tagName(expected_layout_val.tag),
                },
            );
        }

        const actual_struct_idx = actual_layout_val.getStruct().idx;
        const expected_struct_idx = expected_layout_val.getStruct().idx;
        const actual_data = self.layout_store.getStructData(actual_struct_idx);
        const expected_data = self.layout_store.getStructData(expected_struct_idx);

        const target = try hooks.allocValue(expected_layout);
        var original_index: u32 = 0;
        while (original_index < expected_data.fields.count) : (original_index += 1) {
            const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
            const expected_field_size = self.helper.sizeOf(expected_field_layout);
            const actual_field_index = try self.sourceStructFieldIndexForTarget(
                source_desc,
                actual_data.fields.count,
                target_desc,
                expected_data.fields.count,
                original_index,
            );
            const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, actual_field_index);
            const source_field_desc = try self.boxyStructFieldDesc(hooks, source_desc, actual_layout, actual_field_index);
            const target_field_desc = try self.boxyStructFieldDesc(hooks, target_desc, expected_layout, original_index);

            if (expected_field_size == 0) continue;
            const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, actual_field_index);
            const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_struct_idx, original_index);
            try self.writeBoxyPayloadToDestinationWithTargetDesc(
                hooks,
                target.offset(expected_field_offset),
                expected_field_layout,
                value.offset(actual_field_offset),
                actual_field_layout,
                source_field_desc,
                target_field_desc,
            );
        }

        return target;
    }

    fn materializeBoxyTagPayloadToLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_base = self.resolveTagUnionBaseValue(value, actual_layout);
        const target = try self.allocTagValue(hooks, expected_layout);
        const discriminant = if (self.helper.sizeOf(actual_base.layout) == 0)
            @as(u16, 0)
        else
            self.helper.readTagDiscriminant(actual_base.value, actual_base.layout);

        if (self.helper.sizeOf(target.base_layout) > 0) {
            self.helper.writeTagDiscriminant(target.base, target.base_layout, discriminant);
        } else if (discriminant != 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy tag materialization wrote nonzero discriminant {d} into zero-sized layout {d}",
                .{ discriminant, @intFromEnum(target.base_layout) },
            );
        }

        const expected_payload_layout = self.requireBoxyTagPayloadLayout(target.base_layout, discriminant);
        if (self.helper.sizeOf(expected_payload_layout) == 0) return target.outer;

        if (self.boxyTagExtDiscriminant(desc)) |ext_discriminant| {
            if (discriminant == ext_discriminant) {
                const ext_desc = try self.resolveBoxyTagExtDesc(hooks, desc);
                const actual_payload_layout = self.requireBoxyTagPayloadLayout(desc.payload_layout, ext_discriminant);
                try self.writeBoxyPayloadToDestination(
                    hooks,
                    target.base,
                    expected_payload_layout,
                    actual_base.value,
                    actual_payload_layout,
                    ext_desc,
                );
                return target.outer;
            }
        }

        const variant = self.requireBoxyTagVariantByDiscriminant(desc, discriminant);
        try self.writeBoxyTagVariantPayloadToDestination(
            hooks,
            target.base,
            expected_payload_layout,
            actual_base.value,
            variant,
        );
        return target.outer;
    }

    pub fn materializeBoxyTagPayloadToLayoutWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_desc: *const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (actual_layout == expected_layout and source_desc == target_desc) {
            // Identical convention on both sides: the bytes are already in the
            // target's shape, and the surrounding RC statements account for the
            // references they share with the source.
            return try self.materializeLocalValue(hooks, value, expected_layout);
        }
        const actual_base = self.resolveBoxyTagBaseValue(value, actual_layout, source_desc);
        const source_discriminant = if (self.helper.sizeOf(actual_base.layout) == 0)
            @as(u16, 0)
        else
            self.helper.readTagDiscriminant(actual_base.value, actual_base.layout);
        if (self.boxyTagExtDiscriminant(source_desc)) |ext_discriminant| {
            if (source_discriminant == ext_discriminant) {
                // The source value sits in the row-extension slot, whose payload
                // is the extension union itself. Flatten it into the expected
                // target union by materializing the extension payload with the
                // extension descriptor as the new source.
                const ext_desc = try self.resolveBoxyTagExtDesc(hooks, source_desc);
                const actual_payload_layout = self.requireBoxyTagPayloadLayout(actual_base.layout, ext_discriminant);
                return try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                    hooks,
                    actual_base.value,
                    actual_payload_layout,
                    ext_desc,
                    target_desc,
                    expected_layout,
                );
            }
        }
        const source_variant = self.requireBoxyTagVariantByDiscriminant(source_desc, source_discriminant);
        const target_variant = self.findLocalBoxyTagVariant(target_desc, source_variant.name) orelse {
            // The variant lives in the target row's extension: encode it
            // through the extension slot (discriminant = local variant count,
            // payload boxed as the extension union).
            const target_ext_discriminant = self.boxyTagExtDiscriminant(target_desc) orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: target boxy tag descriptor for layout {d} had no variant named {s}",
                    .{ @intFromEnum(expected_layout), self.store.getString(source_variant.name) },
                );
            };
            const target_ext_desc = try self.resolveBoxyTagExtDesc(hooks, target_desc);
            const target = try self.allocTagValue(hooks, expected_layout);
            if (self.helper.sizeOf(target.base_layout) > 0) {
                self.helper.writeTagDiscriminant(target.base, target.base_layout, target_ext_discriminant);
            } else if (target_ext_discriminant != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy tag materialization wrote nonzero extension discriminant {d} into zero-sized layout {d}",
                    .{ target_ext_discriminant, @intFromEnum(expected_layout) },
                );
            }
            const ext_slot_layout = self.requireBoxyTagPayloadLayout(target.base_layout, target_ext_discriminant);
            const ext_value = try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                target_ext_desc,
                ext_slot_layout,
            );
            try self.writeVariantPayloadValue(hooks, target.base, ext_slot_layout, ext_value, ext_slot_layout);
            return target.outer;
        };

        const target = try self.allocTagValue(hooks, expected_layout);
        if (self.helper.sizeOf(target.base_layout) > 0) {
            self.helper.writeTagDiscriminant(target.base, target.base_layout, target_variant.discriminant);
        } else if (target_variant.discriminant != 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy tag materialization wrote nonzero discriminant {d} into zero-sized layout {d}",
                .{ target_variant.discriminant, @intFromEnum(target.base_layout) },
            );
        }

        const expected_payload_layout = self.requireBoxyTagPayloadLayout(target.base_layout, target_variant.discriminant);
        if (self.helper.sizeOf(expected_payload_layout) == 0) return target.outer;

        const actual_payload_layout = self.requireBoxyTagPayloadLayout(actual_base.layout, source_discriminant);
        try self.writeBoxyTagVariantPayloadToDestinationWithTargetDesc(
            hooks,
            target.base,
            expected_payload_layout,
            actual_base.value,
            actual_payload_layout,
            source_variant,
            target_variant,
        );
        return target.outer;
    }

    fn writeBoxyTagVariantPayloadToDestination(
        self: *const BoxyRuntime,
        hooks: anytype,
        destination: Value,
        expected_layout: layout_mod.Idx,
        value: Value,
        variant: *const LirProgram.BoxyTagVariant,
    ) Error!void {
        const actual_layout = variant.payload_layout;
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (actual_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_) {
            // A single-argument tag's recorded payload descriptor describes
            // the whole payload area; the per-field pairing below is for
            // multi-argument tags.
            if (self.findBoxyPayloadDesc(variant, 0)) |first_desc_ref| single: {
                const first_desc = try hooks.resolveDescRef(first_desc_ref);
                if (first_desc.payload_layout != actual_layout) break :single;
                return try self.writeBoxyPayloadToDestination(
                    hooks,
                    destination,
                    expected_layout,
                    value,
                    actual_layout,
                    first_desc,
                );
            }
            const actual_struct_idx = actual_layout_val.getStruct().idx;
            const expected_struct_idx = expected_layout_val.getStruct().idx;
            const expected_data = self.layout_store.getStructData(expected_struct_idx);
            var original_index: u32 = 0;
            while (original_index < expected_data.fields.count) : (original_index += 1) {
                const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
                const expected_field_size = self.helper.sizeOf(expected_field_layout);
                if (expected_field_size == 0) continue;

                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, original_index);
                const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, original_index);
                const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_struct_idx, original_index);
                const field_desc = if (self.findBoxyPayloadDesc(variant, original_index)) |payload_desc|
                    try hooks.resolveDescRef(payload_desc)
                else
                    null;
                try self.writeBoxyPayloadToDestination(
                    hooks,
                    destination.offset(expected_field_offset),
                    expected_field_layout,
                    value.offset(actual_field_offset),
                    actual_field_layout,
                    field_desc,
                );
            }
            return;
        }

        const payload_desc = if (self.findBoxyPayloadDesc(variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const payload_value = try self.readBoxyTagPayloadValue(hooks, value, actual_layout, 0, actual_layout);
        try self.writeBoxyPayloadToDestination(
            hooks,
            destination,
            expected_layout,
            payload_value,
            actual_layout,
            payload_desc,
        );
    }

    fn writeBoxyTagVariantPayloadToDestinationWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        destination: Value,
        expected_layout: layout_mod.Idx,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_variant: *const LirProgram.BoxyTagVariant,
        target_variant: *const LirProgram.BoxyTagVariant,
    ) Error!void {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (actual_layout_val.tag == .struct_ and expected_layout_val.tag == .struct_) {
            // A single-argument tag's recorded payload descriptor describes
            // the whole payload area; the per-field pairing below is for
            // multi-argument tags.
            if (self.findBoxyPayloadDesc(source_variant, 0)) |first_desc_ref| single: {
                const first_desc = try hooks.resolveDescRef(first_desc_ref);
                if (first_desc.payload_layout != actual_layout) break :single;
                const target_payload_desc = if (self.findBoxyPayloadDesc(target_variant, 0)) |desc_ref|
                    try hooks.resolveDescRef(desc_ref)
                else
                    null;
                return try self.writeBoxyPayloadToDestinationWithTargetDesc(
                    hooks,
                    destination,
                    expected_layout,
                    value,
                    actual_layout,
                    first_desc,
                    target_payload_desc,
                );
            }
            const actual_struct_idx = actual_layout_val.getStruct().idx;
            const expected_struct_idx = expected_layout_val.getStruct().idx;
            const expected_data = self.layout_store.getStructData(expected_struct_idx);
            var original_index: u32 = 0;
            while (original_index < expected_data.fields.count) : (original_index += 1) {
                const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
                const expected_field_size = self.helper.sizeOf(expected_field_layout);
                if (expected_field_size == 0) continue;

                const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, original_index);
                const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, original_index);
                const expected_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(expected_struct_idx, original_index);
                const source_field_desc = if (self.findBoxyPayloadDesc(source_variant, original_index)) |desc_ref|
                    try hooks.resolveDescRef(desc_ref)
                else
                    null;
                const target_field_desc = if (self.findBoxyPayloadDesc(target_variant, original_index)) |desc_ref|
                    try hooks.resolveDescRef(desc_ref)
                else
                    null;
                try self.writeBoxyPayloadToDestinationWithTargetDesc(
                    hooks,
                    destination.offset(expected_field_offset),
                    expected_field_layout,
                    value.offset(actual_field_offset),
                    actual_field_layout,
                    source_field_desc,
                    target_field_desc,
                );
            }
            return;
        }

        const source_payload_desc = if (self.findBoxyPayloadDesc(source_variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const target_payload_desc = if (self.findBoxyPayloadDesc(target_variant, 0)) |desc_ref|
            try hooks.resolveDescRef(desc_ref)
        else
            null;
        const payload_value = try self.readBoxyTagPayloadValue(hooks, value, actual_layout, 0, actual_layout);
        try self.writeBoxyPayloadToDestinationWithTargetDesc(
            hooks,
            destination,
            expected_layout,
            payload_value,
            actual_layout,
            source_payload_desc,
            target_payload_desc,
        );
    }

    pub fn writeBoxyPayloadToDestinationWithTargetDesc(
        self: *const BoxyRuntime,
        hooks: anytype,
        destination: Value,
        expected_layout: layout_mod.Idx,
        value: Value,
        actual_layout: layout_mod.Idx,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        target_desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        if (self.helper.sizeOf(expected_layout) == 0) return;
        if (target_desc) |resolved_target_desc| {
            const materialized = try self.materializeBoxyPayloadToLayoutWithOptionalSourceDesc(
                hooks,
                value,
                actual_layout,
                source_desc,
                resolved_target_desc,
                expected_layout,
            );
            destination.copyFrom(materialized, self.helper.sizeOf(expected_layout));
            return;
        }
        try self.writeBoxyPayloadToDestination(
            hooks,
            destination,
            expected_layout,
            value,
            actual_layout,
            source_desc orelse target_desc,
        );
    }

    pub fn readSwitchValue(self: *const BoxyRuntime, value: Value, layout_idx: layout_mod.Idx) Error!u64 {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .tag_union => {
                if (self.helper.sizeOf(layout_idx) == 0) return 0;
                const tu_info = self.layout_store.getTagUnionInfo(layout_val);
                return tu_info.readDiscriminant(value.ptr);
            },
            else => switch (self.helper.sizeOf(layout_idx)) {
                0 => 0,
                1 => value.read(u8),
                2 => value.read(u16),
                4 => value.read(u32),
                8 => value.read(u64),
                else => {
                    if (builtin.mode == .Debug) {
                        const layout_val_dbg = self.layout_store.getLayout(layout_idx);
                        debugPrint(
                            "LIR/interpreter bad switch layout idx={d} tag={s} size={d}\n",
                            .{ @intFromEnum(layout_idx), @tagName(layout_val_dbg.tag), self.helper.sizeOf(layout_idx) },
                        );
                    }
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: switch condition layout {d} is not a supported scalar width",
                        .{@intFromEnum(layout_idx)},
                    );
                },
            },
        };
    }

    pub fn requireBoxyFieldNames(self: *const BoxyRuntime, span: LIR.BoxySpan) []const base.StringLiteral.Idx {
        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.field_names.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy field name span [{d}, {d}) exceeded field name table length {d}",
                .{ start, end, self.boxy_tables.field_names.len },
            );
        }
        return self.boxy_tables.field_names[start..end];
    }

    pub fn appendBoxyInspect(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        value: Value,
        value_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!void {
        if (try self.appendInspectMethodIfPresent(hooks, out, value, value_layout, desc)) return;
        if (desc.inspect_opaque) {
            try out.appendSlice(self.eval_arena, "<opaque>");
            return;
        }
        const value_layout_val = self.layout_store.getLayout(value_layout);
        if (value_layout_val.tag == .box_of_zst) {
            const payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, value_layout, desc) orelse {
                try out.appendSlice(self.eval_arena, "Box({})");
                return;
            };
            if (self.readBoxedDataPointer(value)) |data_ptr| {
                return try self.appendLayoutInspect(hooks, out, .{ .ptr = data_ptr }, payload_desc.payload_layout, payload_desc);
            }
            if (self.helper.sizeOf(payload_desc.payload_layout) != 0) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: non-zero-sized boxy inspect payload layout {d} had a null box pointer",
                    .{@intFromEnum(payload_desc.payload_layout)},
                );
            }
            return try self.appendLayoutInspect(hooks, out, Value.zst, payload_desc.payload_layout, payload_desc);
        }

        return try self.appendLayoutInspect(hooks, out, value, value_layout, desc);
    }

    fn appendInspectMethodIfPresent(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        value: Value,
        value_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!bool {
        const method = desc.inspect_method orelse return false;
        const result = try hooks.callInspectMethod(method, value, value_layout, desc);
        if (result.layout != .str) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: to_inspect worker returned layout {d} instead of Str",
                .{@intFromEnum(result.layout)},
            );
        }
        try out.appendSlice(self.eval_arena, readRocStr(result.value));
        if (!result.borrowed) {
            try self.performBoxyLayoutDrop(
                hooks,
                result.value,
                result.layout,
                result.desc,
                .decref,
                1,
                .atomic,
            );
        }
        return true;
    }

    fn appendLayoutInspect(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        value: Value,
        layout_idx: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        if (desc) |opaque_desc| {
            if (try self.appendInspectMethodIfPresent(hooks, out, value, layout_idx, opaque_desc)) return;
            if (opaque_desc.inspect_opaque) {
                try out.appendSlice(self.eval_arena, "<opaque>");
                return;
            }
        }
        const layout_val = self.layout_store.getLayout(layout_idx);
        switch (layout_val.tag) {
            .zst => {
                if (desc) |zst_desc| {
                    if (zst_desc.tag_variants.len > 0) {
                        return try self.appendZstTagInspect(hooks, out, zst_desc);
                    }
                }
                try out.appendSlice(self.eval_arena, "{}");
            },
            .scalar => switch (layout_val.getScalar().tag) {
                .str => try self.appendQuotedInspectBytes(out, readRocStr(value)),
                .int, .frac, .opaque_ptr => try self.appendScalarInspect(out, value, layout_idx),
            },
            .box_of_zst => {
                if (desc) |payload_desc| {
                    try self.appendBoxyInspect(hooks, out, value, layout_idx, payload_desc);
                } else {
                    try out.appendSlice(self.eval_arena, "Box({})");
                }
            },
            .box => {
                try out.appendSlice(self.eval_arena, "Box(");
                if (self.readBoxedDataPointer(value)) |data_ptr| {
                    try self.appendLayoutInspect(hooks, out, .{ .ptr = data_ptr }, layout_val.getIdx(), if (desc) |box_desc| try self.firstNestedBoxyDesc(hooks, box_desc) else null);
                } else {
                    try out.appendSlice(self.eval_arena, "{}");
                }
                try out.append(self.eval_arena, ')');
            },
            .list, .list_of_zst => try self.appendListInspect(hooks, out, value, layout_idx, desc),
            .struct_ => try self.appendStructInspect(hooks, out, value, layout_idx, desc),
            .tag_union => if (desc) |tag_desc|
                try self.appendTagUnionInspect(hooks, out, value, layout_idx, tag_desc)
            else if (layout_idx == .bool)
                try out.appendSlice(self.eval_arena, if ((try self.readSwitchValue(value, layout_idx)) == 0) "False" else "True")
            else
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: boxy tag-union inspect for layout {d} had no descriptor",
                    .{@intFromEnum(layout_idx)},
                ),
            .erased_callable, .closure => try out.appendSlice(self.eval_arena, "<function>"),
            .ptr => return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy inspect reached compiler-internal pointer layout {d}",
                .{@intFromEnum(layout_idx)},
            ),
        }
    }

    fn appendScalarInspect(
        self: *const BoxyRuntime,
        out: *std.ArrayList(u8),
        value: Value,
        layout_idx: layout_mod.Idx,
    ) Error!void {
        const text = switch (self.helper.sizeOf(layout_idx)) {
            1 => if (isUnsigned(layout_idx))
                try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(u8)})
            else
                try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(i8)}),
            2 => if (isUnsigned(layout_idx))
                try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(u16)})
            else
                try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(i16)}),
            4 => blk: {
                const layout_val = self.layout_store.getLayout(layout_idx);
                break :blk if (layout_val.tag == .scalar and layout_val.getScalar().tag == .frac)
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(f32)})
                else if (isUnsigned(layout_idx))
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(u32)})
                else
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(i32)});
            },
            8 => blk: {
                const layout_val = self.layout_store.getLayout(layout_idx);
                break :blk if (layout_val.tag == .scalar and layout_val.getScalar().tag == .frac)
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(f64)})
                else if (isUnsigned(layout_idx))
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(u64)})
                else
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(i64)});
            },
            16 => blk: {
                const layout_val = self.layout_store.getLayout(layout_idx);
                if (layout_val.tag == .scalar and layout_val.getScalar().tag == .frac) {
                    var dec_buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
                    const dec = builtins.dec.RocDec{ .num = value.read(i128) };
                    break :blk try self.eval_arena.dupe(u8, dec.format_to_buf(&dec_buf));
                }
                break :blk if (isUnsigned(layout_idx))
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(u128)})
                else
                    try std.fmt.allocPrint(self.eval_arena, "{d}", .{value.read(i128)});
            },
            else => try std.fmt.allocPrint(self.eval_arena, "0", .{}),
        };
        defer self.eval_arena.free(text);
        try out.appendSlice(self.eval_arena, text);
    }

    fn appendQuotedInspectBytes(self: *const BoxyRuntime, out: *std.ArrayList(u8), bytes: []const u8) Error!void {
        try out.append(self.eval_arena, '"');
        for (bytes) |byte| {
            switch (byte) {
                '"' => try out.appendSlice(self.eval_arena, "\\\""),
                '\\' => try out.appendSlice(self.eval_arena, "\\\\"),
                '\n' => try out.appendSlice(self.eval_arena, "\\n"),
                '\r' => try out.appendSlice(self.eval_arena, "\\r"),
                '\t' => try out.appendSlice(self.eval_arena, "\\t"),
                else => if (byte < 0x20) {
                    const escaped = try std.fmt.allocPrint(self.eval_arena, "\\u({x})", .{byte});
                    defer self.eval_arena.free(escaped);
                    try out.appendSlice(self.eval_arena, escaped);
                } else {
                    try out.append(self.eval_arena, byte);
                },
            }
        }
        try out.append(self.eval_arena, '"');
    }

    fn appendListInspect(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        value: Value,
        list_layout: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const list = self.valueToRocListForLayout(value, list_layout);
        const elem_layout = self.listElemLayout(list_layout);
        const elem_size = self.helper.sizeOf(elem_layout);
        const elem_desc = if (desc) |list_desc| try self.firstNestedBoxyDesc(hooks, list_desc) else null;

        try out.append(self.eval_arena, '[');
        var index: usize = 0;
        while (index < list.len()) : (index += 1) {
            if (index != 0) try out.appendSlice(self.eval_arena, ", ");
            if (elem_size == 0) {
                try self.appendLayoutInspect(hooks, out, Value.zst, elem_layout, elem_desc);
            } else {
                const bytes = list.bytes orelse {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: non-empty list layout {d} had null bytes during boxy inspect",
                        .{@intFromEnum(list_layout)},
                    );
                };
                try self.appendLayoutInspect(hooks, out, .{ .ptr = bytes + index * elem_size }, elem_layout, elem_desc);
            }
        }
        try out.append(self.eval_arena, ']');
    }

    fn appendStructInspect(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        value: Value,
        struct_layout: layout_mod.Idx,
        desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const struct_layout_val = self.layout_store.getLayout(struct_layout);
        const struct_idx = struct_layout_val.getStruct().idx;
        const struct_data = self.layout_store.getStructData(struct_idx);
        const desc_refs = if (desc) |struct_desc| self.requireBoxyDescRefs(struct_desc.nested_descs) else &.{};
        const field_names = if (desc) |struct_desc| self.requireBoxyFieldNames(struct_desc.field_names) else &.{};
        var next_desc: usize = 0;

        // Records carry their field names in the descriptor; tuples have no
        // names and print positionally.
        const named = field_names.len == struct_data.fields.count;
        try out.append(self.eval_arena, if (named) '{' else '(');
        if (named) try out.append(self.eval_arena, ' ');
        var original_index: u32 = 0;
        var written: usize = 0;
        while (original_index < struct_data.fields.count) : (original_index += 1) {
            const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, original_index);
            if (self.helper.sizeOf(field_layout) == 0 and !named) continue;
            if (written != 0) try out.appendSlice(self.eval_arena, ", ");

            if (named) {
                try out.appendSlice(self.eval_arena, self.store.getString(field_names[original_index]));
                try out.appendSlice(self.eval_arena, ": ");
            }
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, original_index);
            const field_desc = if (self.layoutNeedsBoxyStructuralDesc(field_layout) and next_desc < desc_refs.len) blk: {
                const resolved = try hooks.resolveDescRef(desc_refs[next_desc]);
                next_desc += 1;
                break :blk resolved;
            } else null;
            try self.appendLayoutInspect(hooks, out, value.offset(field_offset), field_layout, field_desc);
            written += 1;
        }
        if (named) {
            try out.appendSlice(self.eval_arena, " }");
        } else {
            if (written == 1) try out.append(self.eval_arena, ',');
            try out.append(self.eval_arena, ')');
        }
    }

    fn appendZstTagInspect(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const variant = self.requireBoxyTagVariantByDiscriminant(desc, 0);
        try out.appendSlice(self.eval_arena, self.store.getString(variant.name));
        if (self.findBoxyPayloadDesc(variant, 0)) |payload_desc_ref| {
            const payload_desc = try hooks.resolveDescRef(payload_desc_ref);
            if (payload_desc.tag_variants.len > 0) {
                try out.append(self.eval_arena, '(');
                try self.appendZstTagInspect(hooks, out, payload_desc);
                try out.append(self.eval_arena, ')');
            }
        }
    }

    fn appendTagUnionInspect(
        self: *const BoxyRuntime,
        hooks: anytype,
        out: *std.ArrayList(u8),
        value: Value,
        union_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!void {
        const tag_base = self.resolveTagUnionBaseValue(value, union_layout);
        const discriminant = self.helper.readTagDiscriminant(tag_base.value, tag_base.layout);
        if (self.boxyTagExtDiscriminant(desc)) |ext_discriminant| {
            if (discriminant == ext_discriminant) {
                const ext_desc = try self.resolveBoxyTagExtDesc(hooks, desc);
                const ext_payload_layout = self.requireBoxyTagPayloadLayout(desc.payload_layout, ext_discriminant);
                const ext_value = try self.materializeLocalValue(hooks, tag_base.value, ext_payload_layout);
                return try self.appendBoxyInspect(hooks, out, ext_value, ext_payload_layout, ext_desc);
            }
        }

        const variant = self.requireBoxyTagVariantByDiscriminant(desc, discriminant);
        try out.appendSlice(self.eval_arena, self.store.getString(variant.name));

        const payload_size = self.helper.sizeOf(variant.payload_layout);
        if (payload_size == 0) {
            // Zero payload bytes can still carry semantic structure: nested
            // zero-sized tags keep their names in the payload descriptor.
            if (self.findBoxyPayloadDesc(variant, 0)) |payload_desc_ref| {
                const payload_desc = try hooks.resolveDescRef(payload_desc_ref);
                if (payload_desc.tag_variants.len > 0) {
                    try out.append(self.eval_arena, '(');
                    try self.appendZstTagInspect(hooks, out, payload_desc);
                    try out.append(self.eval_arena, ')');
                }
            }
            return;
        }

        try out.append(self.eval_arena, '(');
        const payload_layout_val = self.layout_store.getLayout(variant.payload_layout);
        switch (payload_layout_val.tag) {
            .struct_ => {
                // A single-argument tag's recorded payload descriptor
                // describes the whole payload area; the per-field pairing
                // below is for multi-argument tags.
                if (self.findBoxyPayloadDesc(variant, 0)) |first_desc_ref| single: {
                    const first_desc = try hooks.resolveDescRef(first_desc_ref);
                    if (first_desc.payload_layout != variant.payload_layout) break :single;
                    try self.appendLayoutInspect(hooks, out, tag_base.value, variant.payload_layout, first_desc);
                    try out.append(self.eval_arena, ')');
                    return;
                }
                const struct_idx = payload_layout_val.getStruct().idx;
                const struct_data = self.layout_store.getStructData(struct_idx);
                var original_index: u32 = 0;
                var written: usize = 0;
                while (original_index < struct_data.fields.count) : (original_index += 1) {
                    const field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, original_index);
                    if (self.helper.sizeOf(field_layout) == 0) continue;
                    if (written != 0) try out.appendSlice(self.eval_arena, ", ");
                    const field_desc = if (self.findBoxyPayloadDesc(variant, original_index)) |payload_desc|
                        try hooks.resolveDescRef(payload_desc)
                    else
                        null;
                    const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, original_index);
                    try self.appendLayoutInspect(hooks, out, tag_base.value.offset(field_offset), field_layout, field_desc);
                    written += 1;
                }
            },
            else => {
                const payload_desc = if (self.findBoxyPayloadDesc(variant, 0)) |payload_desc|
                    try hooks.resolveDescRef(payload_desc)
                else
                    null;
                try self.appendLayoutInspect(hooks, out, tag_base.value, variant.payload_layout, payload_desc);
            },
        }
        try out.append(self.eval_arena, ')');
    }

    pub fn requireBoxyDict(self: *const BoxyRuntime, dict_id: LIR.BoxyDictId) *const LirProgram.BoxyDict {
        const index = @intFromEnum(dict_id);
        if (index >= self.boxy_tables.dicts.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy dictionary id {d} exceeded dictionary table length {d}",
                .{ index, self.boxy_tables.dicts.len },
            );
        }
        return &self.boxy_tables.dicts[index];
    }

    pub fn requireBoxyMethodSlots(self: *const BoxyRuntime, span: LIR.BoxySpan) []const LirProgram.BoxyMethodSlot {
        if (runtimeBoxySpanStart(span)) |_| {
            self.invariantFailed(
                "LIR/interpreter invariant violated: runtime boxy method slot spans are not supported",
                .{},
            );
        }
        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.method_slots.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy method slot span [{d}, {d}) exceeded method slot table length {d}",
                .{ start, end, self.boxy_tables.method_slots.len },
            );
        }
        return self.boxy_tables.method_slots[start..end];
    }

    pub fn requireBoxyMethodSlot(
        self: *const BoxyRuntime,
        slot_id: LirProgram.BoxyMethodSlotId,
    ) *const LirProgram.BoxyMethodSlot {
        const index = @intFromEnum(slot_id);
        if (index >= self.boxy_tables.method_slots.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy method slot id {d} exceeded method slot table length {d}",
                .{ index, self.boxy_tables.method_slots.len },
            );
        }
        return &self.boxy_tables.method_slots[index];
    }

    pub fn requireBoxyDictRefs(self: *const BoxyRuntime, span: LIR.BoxySpan) []const LIR.BoxyDictRef {
        if (runtimeBoxySpanStart(span) != null) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: runtime boxy dictionary-ref spans are not supported",
                .{},
            );
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.dict_refs.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy dictionary-ref span [{d}, {d}) exceeded dictionary-ref table length {d}",
                .{ start, end, self.boxy_tables.dict_refs.len },
            );
        }
        return self.boxy_tables.dict_refs[start..end];
    }

    pub fn requireBoxyMethodArgLayouts(self: *const BoxyRuntime, span: LIR.BoxySpan) []const layout_mod.Idx {
        if (runtimeBoxySpanStart(span) != null) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: runtime boxy method-argument layout spans are not supported",
                .{},
            );
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.method_arg_layouts.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy method-argument layout span [{d}, {d}) exceeded method-argument layout table length {d}",
                .{ start, end, self.boxy_tables.method_arg_layouts.len },
            );
        }
        return self.boxy_tables.method_arg_layouts[start..end];
    }

    pub fn requireBoxyMethodHiddenDescSources(
        self: *const BoxyRuntime,
        span: LIR.BoxySpan,
    ) []const LirProgram.BoxyMethodHiddenDescSource {
        if (runtimeBoxySpanStart(span) != null) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: runtime boxy method hidden-descriptor source spans are not supported",
                .{},
            );
        }

        const start: usize = span.start;
        const end = start + span.len;
        if (end > self.boxy_tables.method_hidden_desc_sources.len) {
            self.invariantFailed(
                "LIR/interpreter invariant violated: boxy method hidden-descriptor source span [{d}, {d}) exceeded source table length {d}",
                .{ start, end, self.boxy_tables.method_hidden_desc_sources.len },
            );
        }
        return self.boxy_tables.method_hidden_desc_sources[start..end];
    }

    pub fn rcHelperForLayout(self: *const BoxyRuntime, op: RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .closure => self.rcHelperForLayout(nestedDropOp(op), layout_val.getClosure().captures_layout_idx),
            else => .{ .op = op, .layout_idx = layout_idx },
        };
    }

    /// Perform a concrete (descriptor-free) refcount operation on a value of
    /// the given layout, walking aggregates per the layout store's RC plans.
    pub fn performConcreteRc(self: *const BoxyRuntime, hooks: anytype, op: RcOp, layout_idx: layout_mod.Idx, val: Value, count: u16, atomicity: RcAtomicity) void {
        const helper = self.rcHelperForLayout(op, layout_idx);
        self.performRcHelperIfNeeded(hooks, helper, val, count, atomicity);
    }

    pub fn performRcHelperIfNeeded(self: *const BoxyRuntime, hooks: anytype, helper: layout_mod.RcHelper, val: Value, count: u16, atomicity: RcAtomicity) void {
        const plan = hooks.rcPlanFor(helper);
        if (plan == .noop) return;
        self.performRcPlan(hooks, plan, val, count, atomicity);
    }

    pub fn performRcPlan(self: *const BoxyRuntime, hooks: anytype, rc_plan: layout_mod.RcHelperPlan, val: Value, count: u16, atomicity: RcAtomicity) void {
        trace.log("performRawRcPlan: plan={s} val.ptr={*}", .{ @tagName(rc_plan), val.ptr });
        const utils = builtins.utils;
        switch (rc_plan) {
            .noop => {},
            .str_incref => {
                const rs = valueToRocStr(val);
                rs.increfWithAtomicity(count, atomicity, self.roc_ops);
            },
            .str_decref => {
                const rs = valueToRocStr(val);
                rs.decrefWithAtomicity(atomicity, self.roc_ops);
            },
            .str_free => {
                const rs = valueToRocStr(val);
                rs.decrefWithAtomicity(atomicity, self.roc_ops);
            },
            .list_incref => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                rl.increfWithAtomicity(@intCast(count), has_child, atomicity, self.roc_ops);
            },
            .list_decref => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                const alloc_ptr = rl.getAllocationDataPtr(self.roc_ops);
                // Before freeing the list, decref all child elements (mirrors RocList.decref logic)
                if (list_plan.child) |child_key| {
                    if (rl.isUnique(self.roc_ops)) {
                        self.decrefListElements(hooks, rl, list_plan, child_key, count, atomicity);
                    }
                }
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    atomicity,
                    self.roc_ops,
                );
            },
            .list_free => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                const alloc_ptr = rl.getAllocationDataPtr(self.roc_ops);
                // Before freeing the list, decref all child elements (mirrors RocList.decref logic)
                if (list_plan.child) |child_key| {
                    if (rl.isUnique(self.roc_ops)) {
                        self.decrefListElements(hooks, rl, list_plan, child_key, count, atomicity);
                    }
                }
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    atomicity,
                    self.roc_ops,
                );
            },
            .box_incref => {
                const alloc_ptr = val.read(?[*]u8);
                utils.increfDataPtr(alloc_ptr, @intCast(count), atomicity, self.roc_ops);
            },
            .box_decref => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.decrefDataPtr(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, atomicity, self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRcPlan(hooks, hooks.rcPlanFor(child_key), child_val, count, atomicity);
                    }
                }
                utils.decrefDataPtr(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, atomicity, self.roc_ops);
            },
            .box_free => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRcPlan(hooks, hooks.rcPlanFor(child_key), child_val, count, atomicity);
                    }
                }
                utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, self.roc_ops);
            },
            .erased_callable_incref => {
                const alloc_ptr = val.read(?[*]u8);
                builtins.utils.increfDataPtr(alloc_ptr, @intCast(count), atomicity, self.roc_ops);
            },
            .erased_callable_decref => {
                const alloc_ptr = val.read(?[*]u8);
                self.performErasedCallableFinalDropIfUnique(alloc_ptr, .decref, count);
                builtins.utils.decrefDataPtr(
                    alloc_ptr,
                    builtins.erased_callable.payload_alignment,
                    builtins.erased_callable.allocation_has_refcounted_children,
                    atomicity,
                    self.roc_ops,
                );
            },
            .erased_callable_free => {
                const alloc_ptr = val.read(?[*]u8);
                self.performErasedCallableFinalDrop(alloc_ptr, .free, count);
                builtins.utils.freeDataPtrC(
                    alloc_ptr,
                    builtins.erased_callable.payload_alignment,
                    builtins.erased_callable.allocation_has_refcounted_children,
                    self.roc_ops,
                );
            },
            .struct_ => |struct_plan| {
                const field_count = self.layout_store.rcHelperStructFieldCount(struct_plan);
                var i: u32 = 0;
                while (i < field_count) : (i += 1) {
                    const field_plan = hooks.rcStructFieldPlan(struct_plan, i) orelse continue;
                    const field_val = Value{ .ptr = val.ptr + field_plan.offset };
                    self.performRcPlan(hooks, hooks.rcPlanFor(field_plan.child), field_val, count, atomicity);
                }
            },
            .tag_union => |tag_plan| {
                const variant_count = self.layout_store.rcHelperTagUnionVariantCount(tag_plan);
                if (variant_count == 0) return;

                const disc: u32 = blk: {
                    const tu_data = self.layout_store.getTagUnionData(tag_plan.tag_union_idx);
                    const disc_offset = tu_data.discriminant_offset.get(self.layout_store.targetUsize());
                    break :blk switch (tu_data.discriminant_size) {
                        0 => 0,
                        1 => val.offset(disc_offset).read(u8),
                        2 => val.offset(disc_offset).read(u16),
                        else => return,
                    };
                };

                if (disc < variant_count) {
                    if (hooks.rcTagVariantPlan(tag_plan, disc)) |child_key| {
                        // Payload is always at offset 0 in the tag union.
                        self.performRcPlan(hooks, hooks.rcPlanFor(child_key), val, count, atomicity);
                    }
                }
            },
            .closure => |child_key| {
                self.performRcPlan(hooks, hooks.rcPlanFor(child_key), val, count, atomicity);
            },
        }
    }

    /// Iterate through list elements and recursively decref each child.
    /// This mirrors the element cleanup logic in RocList.decref.
    pub fn decrefListElements(
        self: *const BoxyRuntime,
        hooks: anytype,
        rl: builtins.list.RocList,
        list_plan: layout_mod.RcListPlan,
        child_key: layout_mod.RcHelperKey,
        count: u16,
        atomicity: RcAtomicity,
    ) void {
        if (rl.getAllocationDataPtr(self.roc_ops)) |source| {
            const elem_count = rl.getAllocationElementCount(true, self.roc_ops);
            const child_plan = hooks.rcPlanFor(child_key);
            var i: usize = 0;
            while (i < elem_count) : (i += 1) {
                const element_ptr = source + i * list_plan.elem_width;
                const element_val = Value{ .ptr = element_ptr };
                self.performRcPlan(hooks, child_plan, element_val, count, atomicity);
            }
        }
    }

    pub fn performErasedCallableFinalDropIfUnique(
        self: *const BoxyRuntime,
        data_ptr: ?[*]u8,
        op: layout_mod.RcOp,
        count: u16,
    ) void {
        if (data_ptr == null) return;
        if (!builtins.utils.isUnique(data_ptr, self.roc_ops)) return;
        self.performErasedCallableFinalDrop(data_ptr, op, count);
    }

    /// Runs the erased callable's capture cleanup. The `on_drop` slot is
    /// filled at closure creation, which is not an RC statement and makes no
    /// thread-confinement claim, so capture refcount updates behind it always
    /// run atomically, even when the callable's own RC statement is
    /// single-thread (atomic is always sound).
    pub fn performErasedCallableFinalDrop(
        self: *const BoxyRuntime,
        data_ptr: ?[*]u8,
        _: layout_mod.RcOp,
        _: u16,
    ) void {
        const ptr = data_ptr orelse return;
        const payload = builtins.erased_callable.payloadPtr(ptr);
        if (payload.on_drop) |on_drop| {
            on_drop(builtins.erased_callable.capturePtr(ptr), self.roc_ops);
        }
    }

    pub fn materializeLocalValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const size = self.helper.sizeOf(target_layout);
        if (size == 0) return Value.zst;

        const storage = try hooks.allocValue(target_layout);
        if (!value.isZst()) {
            storage.copyFrom(value, size);
        }
        return storage;
    }

    pub fn boxAllocInfo(self: *const BoxyRuntime, hooks: anytype, box_layout: Layout) BoxAllocInfo {
        return switch (box_layout.tag) {
            .box => blk: {
                const elem_layout = box_layout.getIdx();
                const elem_layout_val = self.layout_store.getLayout(elem_layout);
                break :blk .{
                    .elem_layout = elem_layout,
                    .elem_size = self.layout_store.layoutSize(elem_layout_val),
                    .elem_alignment = @intCast(elem_layout_val.alignment(self.layout_store.targetUsize()).toByteUnits()),
                    .contains_rc = hooks.layoutContainsRc(elem_layout),
                };
            },
            .box_of_zst => .{
                .elem_layout = .zst,
                .elem_size = 0,
                .elem_alignment = 1,
                .contains_rc = false,
            },
            else => self.invariantFailed(
                "LIR/interpreter invariant violated: expected box layout, got {s}",
                .{@tagName(box_layout.tag)},
            ),
        };
    }

    pub fn allocTagValue(self: *const BoxyRuntime, hooks: anytype, union_layout: layout_mod.Idx) Error!AllocatedTag {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const box_info = self.boxAllocInfo(hooks, union_layout_val);
            const data_ptr = try hooks.allocRocDataWithRc(
                box_info.elem_size,
                box_info.elem_alignment,
                box_info.contains_rc,
            );
            @memset(data_ptr[0..box_info.elem_size], 0);
            const boxed = try hooks.allocValue(union_layout);
            if (self.layout_store.targetUsize().size() == 8) {
                boxed.write(usize, @intFromPtr(data_ptr));
            } else {
                boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
            }
            return .{
                .outer = boxed,
                .base = .{ .ptr = data_ptr },
                .base_layout = union_layout_val.getIdx(),
            };
        }

        const outer = try hooks.allocValue(union_layout);
        return .{
            .outer = outer,
            .base = outer,
            .base_layout = union_layout,
        };
    }

    pub fn allocBoxOfZstValue(self: *const BoxyRuntime, hooks: anytype, layout_idx: layout_mod.Idx) Error!Value {
        const boxed = try hooks.allocValue(layout_idx);
        const target_usize = self.layout_store.targetUsize();
        if (target_usize.size() == 8) {
            boxed.write(usize, 0);
        } else {
            boxed.write(u32, 0);
        }
        return boxed;
    }

    pub fn allocPointerIntValue(self: *const BoxyRuntime, hooks: anytype, raw_ptr: usize) Error!Value {
        const value = try hooks.allocValue(.opaque_ptr);
        self.writePointerInt(value, raw_ptr);
        return value;
    }

    pub fn rocListToValue(self: *const BoxyRuntime, hooks: anytype, rl: RocList, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box => {
                const box_info = self.boxAllocInfo(hooks, ret_layout_val);
                const data_ptr = try hooks.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_rc,
                );
                @memcpy(data_ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));

                const boxed = try hooks.allocValue(ret_layout);
                const target_usize = self.layout_store.targetUsize();
                if (target_usize.size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            .box_of_zst => return try self.allocBoxOfZstValue(hooks, ret_layout),
            else => {
                const val = try hooks.allocValue(ret_layout);
                @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));
                return val;
            },
        }
    }

    pub fn boxBox(self: *const BoxyRuntime, hooks: anytype, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box_of_zst => return try self.allocBoxOfZstValue(hooks, ret_layout),
            .box => {
                const box_info = self.boxAllocInfo(hooks, ret_layout_val);
                const elem_size = box_info.elem_size;
                const elem_align = box_info.elem_alignment;
                const data_ptr = try hooks.allocRocDataWithRc(elem_size, elem_align, box_info.contains_rc);
                if (elem_size > 0) {
                    @memcpy(data_ptr[0..elem_size], arg.ptr[0..elem_size]);
                }
                const boxed = try hooks.allocValue(ret_layout);
                const target_usize = self.layout_store.targetUsize();
                if (target_usize.size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            else => return error.RuntimeError,
        }
    }

    pub fn coerceExplicitRefValueToLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (actual_layout == expected_layout) return value;

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst;
        const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
        if (actual_is_box or expected_is_box) {
            return try self.coerceExplicitNominalValueToLayout(hooks, value, actual_layout, expected_layout);
        }
        const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
        const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
        if (actual_is_list or expected_is_list) {
            return try self.coerceExplicitListValueToLayout(value, actual_layout, expected_layout);
        }

        if (builtin.mode == .Debug and
            (actual_layout_val.tag == .struct_ or expected_layout_val.tag == .struct_ or
                actual_layout_val.tag == .tag_union or expected_layout_val.tag == .tag_union))
        {
            self.invariantFailed(
                "LIR/interpreter invariant violated: explicit ref reinterpret reached aggregate coercion path actual={d} ({s}) expected={d} ({s})",
                .{
                    @intFromEnum(actual_layout),
                    @tagName(actual_layout_val.tag),
                    @intFromEnum(expected_layout),
                    @tagName(expected_layout_val.tag),
                },
            );
        }

        return self.normalizeValueToLayout(value, actual_layout, expected_layout);
    }

    pub fn coerceExplicitListValueToLayout(
        self: *const BoxyRuntime,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (builtin.mode == .Debug) {
            const actual_layout_val = self.layout_store.getLayout(actual_layout);
            const expected_layout_val = self.layout_store.getLayout(expected_layout);
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            if (!actual_is_list or !expected_is_list) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit list reinterpret expected list layouts, got actual={d} expected={d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(expected_layout) },
                );
            }
        }

        return value;
    }

    pub fn coerceExplicitNominalValueToLayout(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        {
            // Concrete values cross the erased ABI boundary boxed: a non-box
            // value expected as a box is wrapped into a fresh allocation, and
            // a boxed value expected concretely is read back out of it.
            const actual_val = self.layout_store.getLayout(actual_layout);
            const expected_val = self.layout_store.getLayout(expected_layout);
            const actual_is_box = actual_val.tag == .box or actual_val.tag == .box_of_zst;
            const expected_is_box = expected_val.tag == .box or expected_val.tag == .box_of_zst;
            const actual_is_erased_ptr = actual_val.tag == .scalar and actual_val.getScalar().tag == .opaque_ptr;
            const expected_is_erased_ptr = expected_val.tag == .scalar and expected_val.getScalar().tag == .opaque_ptr;
            if (expected_is_box and !actual_is_box and !actual_is_erased_ptr) {
                const sa = self.helper.sizeAlignOf(actual_layout);
                const boxed = try hooks.allocValue(expected_layout);
                if (sa.size == 0) {
                    self.writeBoxedDataPointer(boxed, null);
                    return boxed;
                }
                const data_ptr = try hooks.allocRocDataWithRc(
                    sa.size,
                    @intCast(sa.alignment.toByteUnits()),
                    hooks.layoutContainsRc(actual_layout),
                );
                @memcpy(data_ptr[0..sa.size], value.ptr[0..sa.size]);
                self.writeBoxedDataPointer(boxed, data_ptr);
                return boxed;
            }
            if (actual_is_box and !expected_is_box and !expected_is_erased_ptr) {
                const size = self.helper.sizeOf(expected_layout);
                if (size == 0) return Value.zst;
                const data_ptr = self.readBoxedDataPointer(value) orelse self.invariantFailed(
                    "LIR/interpreter invariant violated: erased boundary unbox found a null box for layout {d}",
                    .{@intFromEnum(expected_layout)},
                );
                const result = try hooks.allocValue(expected_layout);
                result.copyFrom(.{ .ptr = data_ptr }, size);
                return result;
            }
        }
        if (builtin.mode == .Debug) {
            const actual_layout_val = self.layout_store.getLayout(actual_layout);
            const expected_layout_val = self.layout_store.getLayout(expected_layout);
            const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst;
            const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
            const actual_is_erased_ptr = actual_layout_val.tag == .scalar and actual_layout_val.getScalar().tag == .opaque_ptr;
            const expected_is_erased_ptr = expected_layout_val.tag == .scalar and expected_layout_val.getScalar().tag == .opaque_ptr;
            if (actual_layout_val.tag == .zst and expected_layout_val.tag == .box_of_zst) {
                return try self.allocBoxOfZstValue(hooks, expected_layout);
            }
            if (actual_layout_val.tag == .box_of_zst and expected_layout_val.tag == .zst) {
                return Value.zst;
            }
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            if (actual_is_list or expected_is_list) {
                if (actual_is_list and expected_is_list) return try self.coerceExplicitListValueToLayout(value, actual_layout, expected_layout);
                if (!actual_is_box and !expected_is_box) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: explicit nominal reinterpret expected both layouts to be lists when either side is a list, got actual={d} ({s}) expected={d} ({s})",
                        .{
                            @intFromEnum(actual_layout),
                            @tagName(actual_layout_val.tag),
                            @intFromEnum(expected_layout),
                            @tagName(expected_layout_val.tag),
                        },
                    );
                }
            }
            const boxing_compatible =
                (actual_is_box == expected_is_box) or
                (actual_is_box and expected_is_erased_ptr) or
                (expected_is_box and actual_is_erased_ptr);
            if (!boxing_compatible) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit nominal reinterpret expected non-list layouts on the same side of layout boxing, got actual={d} ({s}) expected={d} ({s})",
                    .{
                        @intFromEnum(actual_layout),
                        @tagName(actual_layout_val.tag),
                        @intFromEnum(expected_layout),
                        @tagName(expected_layout_val.tag),
                    },
                );
            }
        }
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (expected_layout_val.tag == .box_of_zst) {
            return try self.allocBoxOfZstValue(hooks, expected_layout);
        }
        return value;
    }

    pub fn writeStructFieldValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        struct_base: Value,
        field_offset: usize,
        expected_layout: layout_mod.Idx,
        actual_value: Value,
        actual_layout: layout_mod.Idx,
    ) Error!void {
        const field_size = self.helper.sizeOf(expected_layout);
        if (field_size == 0) return;
        const coerced = try self.coerceExplicitRefValueToLayout(hooks, actual_value, actual_layout, expected_layout);
        struct_base.offset(field_offset).copyFrom(coerced, field_size);
    }

    pub fn writeVariantPayloadValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        destination: Value,
        variant_payload_layout: layout_mod.Idx,
        payload: Value,
        payload_layout: layout_mod.Idx,
    ) Error!void {
        if (self.helper.sizeOf(variant_payload_layout) == 0) return;
        if (variant_payload_layout == payload_layout) {
            destination.copyFrom(payload, self.helper.sizeOf(variant_payload_layout));
            return;
        }
        if (self.unwrapSingleFieldPayloadLayout(variant_payload_layout)) |field_layout| {
            const variant_layout_val = self.layout_store.getLayout(variant_payload_layout);
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(variant_layout_val.getStruct().idx, 0);
            return self.writeStructFieldValue(hooks, destination, field_offset, field_layout, payload, payload_layout);
        }
        const coerced = try self.coerceExplicitRefValueToLayout(hooks, payload, payload_layout, variant_payload_layout);
        destination.copyFrom(coerced, self.helper.sizeOf(variant_payload_layout));
    }

    pub fn i128LiteralValue(self: *const BoxyRuntime, hooks: anytype, value: i128, layout_idx: layout_mod.Idx) Error!Value {
        const val = try hooks.allocValue(layout_idx);
        const size = self.helper.sizeOf(layout_idx);
        const bits: u128 = @bitCast(value);
        switch (size) {
            1 => val.write(u8, @truncate(bits)),
            2 => val.write(u16, @truncate(bits)),
            4 => val.write(u32, @truncate(bits)),
            8 => val.write(u64, @truncate(bits)),
            16 => val.write(i128, value),
            else => return error.RuntimeError,
        }
        return val;
    }

    /// Encode a numeric literal per the descriptor's payload layout and box it
    /// into dynamic storage. Literal patterns against erased scrutinees only
    /// learn their numeric representation from the scrutinee's descriptor.
    pub fn boxyDynamicNumLiteral(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: i128,
        desc: *const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const payload_layout = desc.payload_layout;
        const payload = switch (payload_layout) {
            .f32 => blk: {
                const val = try hooks.allocValue(.f32);
                val.write(f32, @floatFromInt(value));
                break :blk val;
            },
            .f64 => blk: {
                const val = try hooks.allocValue(.f64);
                val.write(f64, @floatFromInt(value));
                break :blk val;
            },
            .dec => blk: {
                const val = try hooks.allocValue(.dec);
                val.write(i128, value * builtins.dec.RocDec.one_point_zero_i128);
                break :blk val;
            },
            else => try self.i128LiteralValue(hooks, value, payload_layout),
        };
        return try self.allocBoxyDynamicPayload(hooks, payload, payload_layout, desc, target_layout);
    }

    pub fn boxyDynamicFracLiteral(
        self: *const BoxyRuntime,
        hooks: anytype,
        dec_bits: i128,
        desc: *const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const payload_layout = desc.payload_layout;
        const dec = builtins.dec.RocDec{ .num = dec_bits };
        const payload = switch (payload_layout) {
            .f32 => blk: {
                const val = try hooks.allocValue(.f32);
                val.write(f32, @floatCast(dec.toF64()));
                break :blk val;
            },
            .f64 => blk: {
                const val = try hooks.allocValue(.f64);
                val.write(f64, dec.toF64());
                break :blk val;
            },
            .dec => blk: {
                const val = try hooks.allocValue(.dec);
                val.write(i128, dec_bits);
                break :blk val;
            },
            else => return self.invariantFailed(
                "boxy dynamic fractional literal descriptor resolved to a non-fractional payload layout",
                .{},
            ),
        };
        return try self.allocBoxyDynamicPayload(hooks, payload, payload_layout, desc, target_layout);
    }

    /// Execute one planned representation adapter. Adapter inputs are owned
    /// values; the plan transfers that ownership into the produced value.
    pub fn boxyAdaptValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        source_value: Value,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        target_desc: ?*const LirProgram.BoxyTypeDesc,
        adapter_id: LIR.BoxyAdapterId,
        source_mode: LIR.BoxyTransferMode,
    ) Error!BoxyAssignedValue {
        const adapter = self.requireBoxyAdapter(adapter_id);
        if (source_mode != .move or !adapter.consumes_source) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy adapter {d} did not describe an owned source transfer",
                .{@intFromEnum(adapter_id)},
            );
        }
        if (!adapter.produces_owned_result) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy adapter {d} did not produce an owned result",
                .{@intFromEnum(adapter_id)},
            );
        }
        const value = switch (adapter.operation) {
            .relabel => try self.materializeLocalValue(hooks, source_value, adapter.target_layout),
            .materialize => blk: {
                const source_layout_tag = self.layout_store.getLayout(adapter.source_layout).tag;
                const target_layout_tag = self.layout_store.getLayout(adapter.target_layout).tag;
                const source_is_list = source_layout_tag == .list or source_layout_tag == .list_of_zst;
                const source_list = if (source_is_list)
                    self.valueToRocListForLayout(source_value, adapter.source_layout)
                else
                    builtins.list.RocList.empty();
                const moved_list = source_layout_tag == .list and target_layout_tag == .list and
                    source_list.isUnique(self.roc_ops) and
                    !source_list.isSeamlessSlice() and
                    target_desc != null;
                const materialized = if (moved_list)
                    try self.materializeMovedBoxyListPayloadToLayoutWithTargetDesc(
                        hooks,
                        source_value,
                        adapter.source_layout,
                        source_desc,
                        target_desc.?,
                        adapter.target_layout,
                    )
                else call_result: {
                    const assigned = try self.materializeCallResult(
                        hooks,
                        source_value,
                        adapter.source_layout,
                        source_desc,
                        target_desc,
                        adapter.target_layout,
                    );
                    break :call_result assigned.value;
                };
                break :blk materialized;
            },
        };
        return .{ .value = value, .desc = target_desc };
    }

    fn resultSharesListAllocation(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: Value,
        source_layout: layout_mod.Idx,
        source_desc: ?*const LirProgram.BoxyTypeDesc,
        result: Value,
        result_layout: layout_mod.Idx,
        result_desc: ?*const LirProgram.BoxyTypeDesc,
    ) Error!bool {
        const source_list = self.valueToRocListForLayout(source, source_layout);
        const source_allocation = source_list.getAllocationDataPtr(self.roc_ops);
        if (source_allocation == null) return true;

        const result_layout_tag = self.layout_store.getLayout(result_layout).tag;
        if (result_layout_tag == .list or result_layout_tag == .list_of_zst) {
            const result_list = self.valueToRocListForLayout(result, result_layout);
            return result_list.getAllocationDataPtr(self.roc_ops) == source_allocation;
        }
        if (result_layout_tag == .box or result_layout_tag == .box_of_zst) {
            const desc = if (result_desc) |target_desc| blk: {
                if (try self.boxyBoxAllocationPayloadDesc(hooks, result_layout, target_desc) != null) {
                    break :blk target_desc;
                }
                break :blk source_desc orelse return false;
            } else source_desc orelse return false;
            const payload = try self.boxyPayloadValueForDesc(hooks, result, result_layout, desc);
            const payload_tag = self.layout_store.getLayout(payload.layout).tag;
            if (payload_tag != .list and payload_tag != .list_of_zst) return false;
            const result_list = self.valueToRocListForLayout(payload.value, payload.layout);
            return result_list.getAllocationDataPtr(self.roc_ops) == source_allocation;
        }
        return false;
    }

    /// Produce the value and target-local descriptor for boxing a payload into
    /// dynamic storage, honoring relabels of already-dynamic payloads.
    pub fn boxyBoxValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        payload_value: Value,
        payload_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        payload_desc: *const LirProgram.BoxyTypeDesc,
        payload_mode: LIR.BoxyTransferMode,
        target_layout: layout_mod.Idx,
    ) Error!BoxyAssignedValue {
        const target_layout_tag = self.layout_store.getLayout(target_layout).tag;
        try self.increfBoxyTransferSourceIfCopied(
            hooks,
            payload_value,
            payload_layout,
            source_desc,
            payload_mode,
        );
        var target_local_desc: *const LirProgram.BoxyTypeDesc = payload_desc;
        const result = switch (target_layout_tag) {
            .box, .box_of_zst => blk: {
                // The descriptor attached to a box value may describe the box
                // itself (payload_layout == the box layout, payload described by
                // nested descriptors) or describe the payload directly. Boxing
                // stores the payload, so resolve to the payload descriptor the
                // same way box readers do. When the target-side descriptor has
                // no payload information (a fully erased box descriptor), the
                // source descriptor still describes the exact payload being
                // stored, so it becomes both the allocation descriptor and the
                // target local's descriptor.
                const alloc_desc = try self.boxyBoxAllocationPayloadDesc(hooks, target_layout, payload_desc) orelse alloc: {
                    if (source_desc != payload_desc) {
                        target_local_desc = source_desc;
                        break :alloc source_desc;
                    }
                    break :blk try self.allocBoxOfZstValue(hooks, target_layout);
                };
                const payload_layout_tag = self.layout_store.getLayout(payload_layout).tag;
                const payload_is_box_value = payload_layout_tag == .box or payload_layout_tag == .box_of_zst;
                const alloc_payload_tag = self.layout_store.getLayout(alloc_desc.payload_layout).tag;
                const alloc_payload_is_box = alloc_payload_tag == .box or alloc_payload_tag == .box_of_zst;
                // The relabel is only sound when the target box's
                // own label carries no conflicting element
                // expectation: an erased box (or a box of erased
                // boxes) has none, and a concrete box must expect
                // exactly the payload layout the descriptor
                // describes.
                const target_accepts_relabel = switch (target_layout_tag) {
                    .box_of_zst => true,
                    .box => elem_check: {
                        const elem = self.layout_store.getLayout(target_layout).getIdx();
                        break :elem_check elem == alloc_desc.payload_layout or
                            self.layout_store.getLayout(elem).tag == .box_of_zst;
                    },
                    else => false,
                };
                const source_allocation_matches = switch (payload_layout_tag) {
                    .box => self.layout_store.getLayout(payload_layout).getIdx() == alloc_desc.payload_layout,
                    .box_of_zst => source_match: {
                        const source_allocation_desc = try self.boxyBoxAllocationPayloadDesc(hooks, payload_layout, source_desc);
                        break :source_match source_allocation_desc != null and
                            source_allocation_desc.?.payload_layout == alloc_desc.payload_layout;
                    },
                    else => false,
                };
                if (payload_is_box_value and !alloc_payload_is_box and target_accepts_relabel and source_allocation_matches) {
                    // The payload is already a dynamic box whose
                    // interior this allocation descriptor
                    // describes; boxing it is a relabel of the
                    // same allocation, not a new wrap.
                    break :blk try self.materializeLocalValue(hooks, payload_value, target_layout);
                }
                if (source_desc == alloc_desc and payload_layout == alloc_desc.payload_layout) {
                    break :blk try self.allocBoxyDynamicPayload(
                        hooks,
                        payload_value,
                        payload_layout,
                        alloc_desc,
                        target_layout,
                    );
                }
                const materialized_payload = try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                    hooks,
                    payload_value,
                    payload_layout,
                    source_desc,
                    alloc_desc,
                    alloc_desc.payload_layout,
                );
                break :blk try self.allocBoxyDynamicPayload(
                    hooks,
                    materialized_payload,
                    alloc_desc.payload_layout,
                    alloc_desc,
                    target_layout,
                );
            },
            else => try self.materializeBoxyPayloadToLayout(
                hooks,
                payload_value,
                payload_layout,
                source_desc,
                target_layout,
            ),
        };
        return .{ .value = result, .desc = target_local_desc };
    }

    /// Produce the value and target-local descriptor for reading a dynamic
    /// box's payload back out, honoring pure relabels of the source
    /// allocation.
    pub fn boxyUnboxValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        source_value: Value,
        source_layout: layout_mod.Idx,
        source_desc: *const LirProgram.BoxyTypeDesc,
        target_desc: ?*const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
        source_mode: LIR.BoxyTransferMode,
    ) Error!BoxyAssignedValue {
        const source_payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, source_layout, source_desc);
        const target_payload_desc = if (target_desc) |resolved_target_desc|
            try self.boxyBoxAllocationPayloadDesc(hooks, target_layout, resolved_target_desc)
        else
            null;
        const relabel_payload_desc = source_payload_desc orelse target_payload_desc;
        const payload_desc = source_payload_desc;
        const payload_layout = if (payload_desc) |resolved|
            resolved.payload_layout
        else
            target_layout;
        const source_layout_tag = self.layout_store.getLayout(source_layout).tag;
        const target_layout_value = self.layout_store.getLayout(target_layout);
        const source_is_box = source_layout_tag == .box or source_layout_tag == .box_of_zst;
        const relabel_payload_is_box = if (relabel_payload_desc) |resolved| blk: {
            const payload_tag = self.layout_store.getLayout(resolved.payload_layout).tag;
            break :blk payload_tag == .box or payload_tag == .box_of_zst;
        } else false;
        // The relabel is only sound when the target box's own label
        // carries no conflicting element expectation: an erased box
        // (or a box of erased boxes) has none, and a concrete box
        // must expect exactly the payload layout the descriptor
        // describes.
        const target_accepts_relabel = switch (target_layout_value.tag) {
            .box_of_zst => true,
            .box => if (relabel_payload_desc) |resolved|
                target_layout_value.getIdx() == resolved.payload_layout or
                    self.layout_store.getLayout(target_layout_value.getIdx()).tag == .box_of_zst
            else
                false,
            else => false,
        };
        if (source_is_box and target_accepts_relabel and relabel_payload_desc != null and !relabel_payload_is_box) {
            // Unboxing a box-family value into another box-family
            // label with a non-box payload is a pure relabel: the
            // result IS the source allocation. Rewrapping would
            // duplicate interior references the surrounding RC
            // statements never account for.
            const relabeled = try self.materializeLocalValue(hooks, source_value, target_layout);
            return .{ .value = relabeled, .desc = target_desc orelse relabel_payload_desc };
        }
        const data_ptr = self.readBoxedDataPointer(source_value);
        const result = if (data_ptr) |ptr| blk: {
            if (target_desc) |target_box_desc| {
                if (payload_desc) |payload_box_desc| {
                    break :blk try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                        hooks,
                        .{ .ptr = ptr },
                        payload_layout,
                        payload_box_desc,
                        target_box_desc,
                        target_layout,
                    );
                }
            }
            break :blk try self.materializeBoxyPayloadToLayout(
                hooks,
                .{ .ptr = ptr },
                payload_layout,
                payload_desc,
                target_layout,
            );
        } else try self.materializeLocalValue(hooks, Value.zst, target_layout);
        if (source_mode == .move) {
            if (source_is_box and target_layout_value.tag != .box and target_layout_value.tag != .box_of_zst) {
                if (data_ptr) |ptr| {
                    if (source_payload_desc) |moved_payload_desc| {
                        const moved_payload = Value{ .ptr = ptr };
                        const payload_tag = self.layout_store.getLayout(moved_payload_desc.payload_layout).tag;
                        if ((payload_tag == .list or payload_tag == .list_of_zst) and
                            !try self.resultSharesListAllocation(
                                hooks,
                                moved_payload,
                                moved_payload_desc.payload_layout,
                                moved_payload_desc,
                                result,
                                target_layout,
                                target_desc,
                            ))
                        {
                            try self.performBoxyLayoutDrop(
                                hooks,
                                moved_payload,
                                moved_payload_desc.payload_layout,
                                moved_payload_desc,
                                .decref,
                                1,
                                .atomic,
                            );
                        } else {
                            try self.releaseMovedPayloadBoxesReboxedIntoResult(
                                hooks,
                                moved_payload,
                                moved_payload_desc.payload_layout,
                                moved_payload_desc,
                                result,
                                target_layout,
                                target_desc,
                            );
                        }
                    }
                }
            }
            try self.releaseMovedBoxyDynamicPayload(hooks, source_value, source_layout, source_desc);
        }
        return .{ .value = result, .desc = target_desc orelse payload_desc };
    }

    /// Materialize a worker call's result into the caller's expected layout,
    /// guided by the callee's returned descriptor and the call site's declared
    /// result descriptor.
    pub fn materializeCallResult(
        self: *const BoxyRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        actual_desc: ?*const LirProgram.BoxyTypeDesc,
        result_desc: ?*const LirProgram.BoxyTypeDesc,
        expected_layout: layout_mod.Idx,
    ) Error!BoxyAssignedValue {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst;
        const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst;
        if (actual_desc) |returned_desc| {
            if (result_desc) |target_desc| {
                if (actual_layout == expected_layout and returned_desc == target_desc) return .{ .value = value, .desc = returned_desc };
                if (actual_is_box and !expected_is_box) {
                    return try self.boxyUnboxValue(
                        hooks,
                        value,
                        actual_layout,
                        returned_desc,
                        target_desc,
                        expected_layout,
                        .move,
                    );
                }
                const materialized = try self.materializeBoxyPayloadToLayoutWithTargetDesc(
                    hooks,
                    value,
                    actual_layout,
                    returned_desc,
                    target_desc,
                    expected_layout,
                );
                try self.decrefMovedBoxySourceIfReboxed(
                    hooks,
                    value,
                    actual_layout,
                    returned_desc,
                    materialized,
                    expected_layout,
                );
                if (!actual_is_box) {
                    try self.releaseMovedPayloadBoxesReboxedIntoResult(
                        hooks,
                        value,
                        actual_layout,
                        returned_desc,
                        materialized,
                        expected_layout,
                        target_desc,
                    );
                }
                const assigned_desc = if (actual_layout == expected_layout and !try self.descriptorContainsUnspecifiedBox(hooks, target_desc))
                    target_desc
                else if (actual_layout == expected_layout)
                    returned_desc
                else if (expected_is_box and try self.boxyBoxAllocationPayloadDesc(hooks, expected_layout, target_desc) == null)
                    returned_desc
                else
                    target_desc;
                return .{ .value = materialized, .desc = assigned_desc };
            }
            if (actual_is_box and !expected_is_box) {
                return try self.boxyUnboxValue(
                    hooks,
                    value,
                    actual_layout,
                    returned_desc,
                    null,
                    expected_layout,
                    .move,
                );
            }
            const materialized = try self.materializeBoxyPayloadToLayout(
                hooks,
                value,
                actual_layout,
                returned_desc,
                expected_layout,
            );
            try self.decrefMovedBoxySourceIfReboxed(
                hooks,
                value,
                actual_layout,
                returned_desc,
                materialized,
                expected_layout,
            );
            if (!actual_is_box) {
                try self.releaseMovedPayloadBoxesReboxedIntoResult(
                    hooks,
                    value,
                    actual_layout,
                    returned_desc,
                    materialized,
                    expected_layout,
                    null,
                );
            }
            return .{ .value = materialized, .desc = returned_desc };
        }

        if (actual_layout == expected_layout) return .{ .value = value, .desc = result_desc };

        if (result_desc) |target_desc| {
            if (actual_is_box and !expected_is_box) {
                return try self.boxyUnboxValue(
                    hooks,
                    value,
                    actual_layout,
                    target_desc,
                    target_desc,
                    expected_layout,
                    .move,
                );
            }
            const materialized = try self.materializeBoxyPayloadToLayoutWithOptionalSourceDesc(
                hooks,
                value,
                actual_layout,
                null,
                target_desc,
                expected_layout,
            );
            return .{ .value = materialized, .desc = target_desc };
        }

        return .{
            .value = try self.coerceExplicitRefValueToLayout(hooks, value, actual_layout, expected_layout),
            .desc = null,
        };
    }

    fn descriptorContainsUnspecifiedBox(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
    ) Error!bool {
        var visited = std.AutoHashMap(*const LirProgram.BoxyTypeDesc, void).init(self.scratch);
        defer visited.deinit();
        return try self.descriptorContainsUnspecifiedBoxInner(hooks, desc, &visited);
    }

    fn descriptorContainsUnspecifiedBoxInner(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
        visited: *std.AutoHashMap(*const LirProgram.BoxyTypeDesc, void),
    ) Error!bool {
        const entry = try visited.getOrPut(desc);
        if (entry.found_existing) return false;

        const layout_value = self.layout_store.getLayout(desc.payload_layout);
        if (layout_value.tag == .box or layout_value.tag == .box_of_zst) {
            if (try self.boxyBoxAllocationPayloadDesc(hooks, desc.payload_layout, desc) == null) return true;
        }

        const nested = self.requireBoxyDescRefs(desc.nested_descs);
        for (nested) |desc_ref| {
            const child = try hooks.resolveDescRef(desc_ref);
            if (try self.descriptorContainsUnspecifiedBoxInner(hooks, child, visited)) return true;
        }

        const variants = self.requireBoxyTagVariants(desc.tag_variants);
        for (variants) |variant| {
            const payload_descs = self.requireBoxyTagPayloadDescs(variant.payload_descs);
            for (payload_descs) |payload_desc| {
                const child = try hooks.resolveDescRef(payload_desc.desc);
                if (try self.descriptorContainsUnspecifiedBoxInner(hooks, child, visited)) return true;
            }
        }

        if (desc.tag_ext_desc) |ext_ref| {
            const ext = try hooks.resolveDescRef(ext_ref);
            if (try self.descriptorContainsUnspecifiedBoxInner(hooks, ext, visited)) return true;
        }
        return false;
    }

    const PreparedCallArgument = struct {
        assigned: BoxyAssignedValue,
        borrowed: bool,
    };

    fn prepareBorrowedCallArgument(
        self: *const BoxyRuntime,
        hooks: anytype,
        source: DictCallArg,
        target_desc: ?*const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
    ) Error!PreparedCallArgument {
        if (source.layout == target_layout) {
            return .{
                .assigned = .{ .value = source.value, .desc = target_desc orelse source.source_desc },
                .borrowed = true,
            };
        }

        const source_layout = self.layout_store.getLayout(source.layout);
        const target_layout_value = self.layout_store.getLayout(target_layout);
        const source_is_box = source_layout.tag == .box or source_layout.tag == .box_of_zst;
        const target_is_box = target_layout_value.tag == .box or target_layout_value.tag == .box_of_zst;
        if (source_is_box) {
            const source_desc = source.source_desc orelse target_desc orelse {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: borrowed boxy call argument had no descriptor",
                    .{},
                );
            };
            const assigned = try self.boxyUnboxValue(
                hooks,
                source.value,
                source.layout,
                source_desc,
                target_desc,
                target_layout,
                .borrow,
            );
            const shares_box = target_is_box and
                self.readBoxedDataPointer(source.value) == self.readBoxedDataPointer(assigned.value);
            if (target_is_box and !shares_box) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: borrowed inspect argument adapter allocated a distinct target box",
                    .{},
                );
            }
            return .{
                .assigned = assigned,
                .borrowed = true,
            };
        }

        return self.invariantFailedError(
            "LIR/interpreter invariant violated: borrowed inspect argument had incompatible concrete layouts {d} and {d}",
            .{ @intFromEnum(source.layout), @intFromEnum(target_layout) },
        );
    }

    pub fn prepareInspectCall(
        self: *const BoxyRuntime,
        hooks: anytype,
        alloc: Allocator,
        slot_id: LirProgram.BoxyMethodSlotId,
        source: DictCallArg,
    ) Error!PreparedWorkerCall {
        const slot = self.requireBoxyMethodSlot(slot_id);
        if (slot.structural_eq) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: inspect descriptor referenced a structural equality slot",
                .{},
            );
        }
        const arg_layouts = self.requireBoxyMethodArgLayouts(slot.adapter.arg_layouts);
        if (arg_layouts.len != 1) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: inspect method adapter had {d} explicit argument layouts",
                .{arg_layouts.len},
            );
        }

        const dict = LirProgram.BoxyDict{ .method_slots = .{
            .start = @intFromEnum(slot_id),
            .len = 1,
        } };
        const prepared = try self.prepareDictCall(
            hooks,
            alloc,
            &dict,
            0,
            @intFromEnum(slot.method),
            &.{source},
            &.{},
            .borrow,
        );
        return switch (prepared) {
            .call => |call| call,
            .structural_eq => self.invariantFailedError(
                "LIR/interpreter invariant violated: inspect method prepared as structural equality",
                .{},
            ),
        };
    }

    /// Resolve one dictionary method call into either the structural-equality
    /// plan or a worker call with fully adapted arguments: explicit args
    /// (materialized per the slot's adapter), then hidden descriptors, then
    /// nested dictionaries. `alloc` backs the produced argument arrays.
    pub fn prepareDictCall(
        self: *const BoxyRuntime,
        hooks: anytype,
        alloc: Allocator,
        dict: *const LirProgram.BoxyDict,
        method_slot_index: u32,
        required_method: u32,
        args: []const DictCallArg,
        hidden_args: []const Value,
        argument_mode: CallArgumentMode,
    ) Error!PreparedDictCall {
        const method_slots = self.requireBoxyMethodSlots(dict.method_slots);
        const method_slot = blk: {
            if (method_slot_index < method_slots.len and @intFromEnum(method_slots[method_slot_index].method) == required_method) {
                break :blk method_slots[method_slot_index];
            }
            for (method_slots) |slot| {
                if (@intFromEnum(slot.method) == required_method) break :blk slot;
            }
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary method {d} was missing from dictionary with {d} slot(s)",
                .{ required_method, method_slots.len },
            );
        };
        if (method_slot.structural_eq) {
            if (args.len != 2) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: structural equality dictionary slot received {d} args",
                    .{args.len},
                );
            }
            const eq_slot_descs = self.requireBoxyDescRefs(method_slot.hidden_descs);
            if (eq_slot_descs.len != 1) {
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: structural equality dictionary slot carried {d} descriptors",
                    .{eq_slot_descs.len},
                );
            }
            const operand_desc = try hooks.resolveDescRef(eq_slot_descs[0]);
            return .{ .structural_eq = operand_desc };
        }
        if (method_slot.adapter.ret_layout != null or
            method_slot.adapter.ret_desc != null or
            method_slot.adapter.nested_dicts.len != 0)
        {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary method adapters are not executable in the interpreter yet",
                .{},
            );
        }

        const adapter_arg_layouts = self.requireBoxyMethodArgLayouts(method_slot.adapter.arg_layouts);
        const adapter_arg_descs = self.requireBoxyDescRefs(method_slot.adapter.arg_descs);
        if (adapter_arg_layouts.len != 0 and adapter_arg_layouts.len != args.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary method adapter had {d} arg layouts for {d} explicit args",
                .{ adapter_arg_layouts.len, args.len },
            );
        }
        if (adapter_arg_descs.len != 0 and adapter_arg_descs.len != adapter_arg_layouts.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary method adapter had {d} arg descriptors for {d} arg layouts",
                .{ adapter_arg_descs.len, adapter_arg_layouts.len },
            );
        }
        const slot_hidden_descs = self.requireBoxyDescRefs(method_slot.hidden_descs);
        const slot_nested_dicts = self.requireBoxyDictRefs(method_slot.nested_dicts);
        const adapter_hidden_desc_sources = self.requireBoxyMethodHiddenDescSources(method_slot.adapter.hidden_desc_sources);
        if (adapter_hidden_desc_sources.len != 0 and adapter_hidden_desc_sources.len != slot_hidden_descs.len) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary method adapter had {d} hidden descriptor sources for {d} slot descriptors",
                .{ adapter_hidden_desc_sources.len, slot_hidden_descs.len },
            );
        }
        if (adapter_hidden_desc_sources.len == 0 and slot_hidden_descs.len != 0) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary method slot had {d} hidden descriptors but no adapter hidden descriptor sources",
                .{slot_hidden_descs.len},
            );
        }
        const hidden_desc_arg_count = adapter_hidden_desc_sources.len;
        const call_arg_count = args.len + hidden_desc_arg_count + slot_nested_dicts.len;
        const arg_values = try alloc.alloc(Value, call_arg_count);
        const arg_layouts = try alloc.alloc(layout_mod.Idx, call_arg_count);
        const arg_descs = try alloc.alloc(?*const LirProgram.BoxyTypeDesc, call_arg_count);
        var borrowed_args: u64 = 0;
        var call_arg_index: usize = 0;
        for (args, 0..) |arg, explicit_index| {
            if (adapter_arg_layouts.len != 0) {
                const target_layout = adapter_arg_layouts[explicit_index];
                const target_desc = if (adapter_arg_descs.len != 0)
                    try hooks.resolveDescRef(adapter_arg_descs[explicit_index])
                else
                    null;
                const prepared_arg = switch (argument_mode) {
                    .move => PreparedCallArgument{
                        .assigned = try self.materializeCallResult(
                            hooks,
                            arg.value,
                            arg.layout,
                            arg.source_desc,
                            target_desc,
                            target_layout,
                        ),
                        .borrowed = false,
                    },
                    .borrow => try self.prepareBorrowedCallArgument(hooks, arg, target_desc, target_layout),
                };
                arg_values[call_arg_index] = prepared_arg.assigned.value;
                arg_layouts[call_arg_index] = target_layout;
                arg_descs[call_arg_index] = prepared_arg.assigned.desc;
                if (prepared_arg.borrowed and call_arg_index < 64) {
                    borrowed_args |= @as(u64, 1) << @as(u6, @intCast(call_arg_index));
                }
            } else {
                arg_values[call_arg_index] = arg.value;
                arg_layouts[call_arg_index] = arg.layout;
                arg_descs[call_arg_index] = arg.source_desc;
                if (argument_mode == .borrow and call_arg_index < 64) {
                    borrowed_args |= @as(u64, 1) << @as(u6, @intCast(call_arg_index));
                }
            }
            call_arg_index += 1;
        }
        if (adapter_hidden_desc_sources.len != 0) {
            for (adapter_hidden_desc_sources) |source| {
                switch (source) {
                    .slot => |slot_index| {
                        if (slot_index >= slot_hidden_descs.len) {
                            return self.invariantFailedError(
                                "LIR/interpreter invariant violated: dictionary method hidden descriptor source slot {d} exceeded slot descriptor count {d}",
                                .{ slot_index, slot_hidden_descs.len },
                            );
                        }
                        const desc = try self.materializeBoxyDescRefValue(hooks, slot_hidden_descs[slot_index]);
                        arg_values[call_arg_index] = try self.allocPointerIntValue(hooks, @intFromPtr(desc));
                    },
                    .call => |call_index| {
                        if (call_index >= hidden_args.len) {
                            return self.invariantFailedError(
                                "LIR/interpreter invariant violated: dictionary method hidden descriptor source call {d} exceeded call descriptor count {d}",
                                .{ call_index, hidden_args.len },
                            );
                        }
                        arg_values[call_arg_index] = hidden_args[call_index];
                    },
                }
                arg_layouts[call_arg_index] = .opaque_ptr;
                arg_descs[call_arg_index] = null;
                call_arg_index += 1;
            }
        }
        for (slot_nested_dicts) |dict_ref| {
            const nested_dict = try hooks.resolveDictRef(dict_ref);
            arg_values[call_arg_index] = try self.allocPointerIntValue(hooks, @intFromPtr(nested_dict));
            arg_layouts[call_arg_index] = .opaque_ptr;
            arg_descs[call_arg_index] = null;
            call_arg_index += 1;
        }
        if (call_arg_index != call_arg_count) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: dictionary call argument collection produced {d} args but expected {d}",
                .{ call_arg_index, call_arg_count },
            );
        }
        return .{ .call = .{
            .proc = method_slot.proc,
            .arg_values = arg_values,
            .arg_layouts = arg_layouts,
            .arg_descs = arg_descs,
            .borrowed_args = borrowed_args,
        } };
    }
};
