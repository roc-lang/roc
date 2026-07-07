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

/// Comptime-gated tracing for refcount operations.
/// Enabled via `-Dtrace-refcount=true`. Zero cost when disabled.
const trace_rc = struct {
    const enabled = if (@hasDecl(build_options, "trace_refcount")) build_options.trace_refcount else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            debugPrint("[rc] " ++ fmt ++ "\n", args);
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
    /// Backs runtime-created descriptors themselves; they stay alive for the
    /// rest of the evaluation.
    desc_arena: Allocator,

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
        if (builtin.mode == .Debug) {
            debugPrint("boxy descriptor variants for payload_layout={d}:", .{@intFromEnum(desc.payload_layout)});
            for (self.requireBoxyTagVariants(desc.tag_variants)) |*variant| {
                debugPrint(" {s}:{d}", .{ self.store.getString(variant.name), variant.discriminant });
            }
            debugPrint(" tag_ext_desc={any}\n", .{desc.tag_ext_desc});
        }
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
        const ext_value = try hooks.materializeValue(tag_base.value, ext_payload_layout);
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
        const desc = try self.desc_arena.create(LirProgram.BoxyTypeDesc);
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
        const target = try self.desc_arena.create(LirProgram.BoxyTypeDesc);
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
        target.structural_inspect = source.structural_inspect;
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
            self.runtime_boxy_desc_refs.items[start + index] =
                try self.copyBoxyDescRefToRuntime(hooks, source_ref, copied, allow_global_reuse);
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
            self.runtime_boxy_tag_variants.items[start + index] = .{
                .name = variant.name,
                .discriminant = variant.discriminant,
                .payload_layout = variant.payload_layout,
                .payload_descs = try self.copyBoxyTagPayloadDescSpanToRuntime(hooks, variant.payload_descs, copied, allow_global_reuse),
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
            self.runtime_boxy_tag_payload_descs.items[start + index] = .{
                .payload_index = payload_desc.payload_index,
                .desc = try self.copyBoxyDescRefToRuntime(hooks, payload_desc.desc, copied, allow_global_reuse),
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
            self.runtime_boxy_payload_steps.items[start + index] = switch (step) {
                .concrete => |concrete| .{ .concrete = concrete },
                .dynamic => |dynamic| .{ .dynamic = .{
                    .op = dynamic.op,
                    .desc = try self.copyBoxyDescRefToRuntime(hooks, dynamic.desc, copied, allow_global_reuse),
                } },
            };
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
            hooks.performRcHelper(.incref, layout_idx, val, count, atomicity);
            return;
        }

        if (desc == null) {
            hooks.performRcHelper(op, layout_idx, val, count, atomicity);
            return;
        }

        const resolved_desc = desc.?;
        switch (layout_val.tag) {
            .list, .list_of_zst => try self.performBoxyListDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .struct_ => try self.performBoxyStructDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .tag_union => try self.performBoxyTagUnionDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .box, .box_of_zst => try self.performBoxyBoxDrop(hooks, val, layout_idx, resolved_desc, op, count, atomicity),
            .scalar, .closure, .erased_callable => {
                hooks.performRcHelper(op, layout_idx, val, count, atomicity);
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
            if (layout_val.tag == .box_of_zst) return;
            hooks.performRcHelper(op, layout_idx, val, count, atomicity);
            return;
        };
        const data_ptr = self.readBoxedDataPointer(val) orelse return;
        const payload_sa = self.helper.sizeAlignOf(payload_desc.payload_layout);
        const payload_alignment: u32 = @intCast(payload_sa.alignment.toByteUnits());
        if (hooks.traceProcId() == 97 or (hooks.traceProcId() == 4 and @intFromEnum(layout_idx) == 21)) {
            trace_rc.log(
                "boxy_box_drop proc={d} layout={d} desc_payload={d} desc_contains={} payload_desc_payload={d} payload_desc_contains={} payload_align={d} data=0x{x} op={s}",
                .{
                    hooks.traceProcId(),
                    @intFromEnum(layout_idx),
                    @intFromEnum(desc.payload_layout),
                    desc.contains_refcounted,
                    @intFromEnum(payload_desc.payload_layout),
                    payload_desc.contains_refcounted,
                    payload_alignment,
                    @intFromPtr(data_ptr),
                    @tagName(op),
                },
            );
        }
        const allocation_contains_refcounted = self.boxyDynamicPayloadAllocationContainsRc(desc, layout_idx);
        if (allocation_contains_refcounted and builtins.utils.isUnique(data_ptr, self.roc_ops)) {
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
            hooks.performRcHelper(op, list_layout, val, count, atomicity);
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
        if (hooks.traceProcId() == 97 or hooks.traceProcId() == 18) {
            trace_rc.log(
                "boxy_list_drop proc={d} list_layout={d} desc_payload={d} nested_start={d} nested_len={d} elem_layout={d} elem_size={d} elem_align={d} elem_desc_payload={d} elem_desc_contains={} len={d} cap={d}",
                .{
                    hooks.traceProcId(),
                    @intFromEnum(list_layout),
                    @intFromEnum(desc.payload_layout),
                    desc.nested_descs.start,
                    desc.nested_descs.len,
                    @intFromEnum(elem_layout),
                    elem_size,
                    elem_alignment,
                    @intFromEnum(elem_desc.payload_layout),
                    elem_desc.contains_refcounted,
                    rl.len(),
                    rl.capacity_or_alloc_ptr,
                },
            );
        }

        if (rl.isUnique(self.roc_ops)) {
            if (rl.getAllocationDataPtr(self.roc_ops)) |source| {
                var index: usize = 0;
                while (index < rl.len()) : (index += 1) {
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
            const ext_value = try hooks.materializeValue(tag_base.value, actual_payload_layout);
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
        const coerced = try hooks.coerceToLayout(payload, payload_layout, desc.payload_layout);
        @memcpy(data_ptr[0..payload_size], coerced.readBytes(payload_size));
        self.writeBoxedDataPointer(boxed, data_ptr);
        return boxed;
    }

    pub fn freeMovedBoxyDynamicPayload(
        self: *const BoxyRuntime,
        source: Value,
        source_layout: layout_mod.Idx,
        desc: *const LirProgram.BoxyTypeDesc,
    ) void {
        const data_ptr = self.readBoxedDataPointer(source) orelse return;
        const payload_sa = self.helper.sizeAlignOf(desc.payload_layout);
        builtins.utils.freeDataPtrC(
            data_ptr,
            @intCast(payload_sa.alignment.toByteUnits()),
            self.boxyDynamicPayloadAllocationContainsRc(desc, source_layout),
            self.roc_ops,
        );
    }

    pub fn constructBoxyTagValue(
        self: *const BoxyRuntime,
        hooks: anytype,
        desc: *const LirProgram.BoxyTypeDesc,
        tag_name: base.StringLiteral.Idx,
        payload: ?Value,
        payload_layout: layout_mod.Idx,
        payload_desc: ?*const LirProgram.BoxyTypeDesc,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        if (self.findLocalBoxyTagVariant(desc, tag_name)) |variant| {
            const allocated = try hooks.allocTagValue(desc.payload_layout);
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
            return try self.allocBoxyDynamicPayload(hooks, allocated.outer, desc.payload_layout, desc, target_layout);
        }

        const ext_discriminant = self.boxyTagExtDiscriminant(desc) orelse {
            if (builtin.mode == .Debug) {
                debugPrint(
                    "debug_construct_missing_tag proc={d} tag={s} desc_payload={d} debug_ty={any} variants=",
                    .{ hooks.traceProcId(), self.store.getString(tag_name), @intFromEnum(desc.payload_layout), desc.debug_checked_type },
                );
                for (self.requireBoxyTagVariants(desc.tag_variants)) |variant| {
                    debugPrint("{s}:{d} ", .{ self.store.getString(variant.name), variant.discriminant });
                }
                debugPrint("tag_ext={any}\n", .{desc.tag_ext_desc});
            }
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxy descriptor had no tag variant named {s}",
                .{self.store.getString(tag_name)},
            );
        };
        const ext_desc = try self.resolveBoxyTagExtDesc(hooks, desc);
        const ext_payload_layout = self.requireBoxyTagPayloadLayout(desc.payload_layout, ext_discriminant);
        const ext_value = try self.constructBoxyTagValue(hooks, ext_desc, tag_name, payload, payload_layout, payload_desc, ext_payload_layout);
        const allocated = try hooks.allocTagValue(desc.payload_layout);
        if (self.helper.sizeOf(allocated.base_layout) > 0) {
            self.helper.writeTagDiscriminant(allocated.base, allocated.base_layout, ext_discriminant);
        }
        try hooks.writeVariantPayloadValue(allocated.base, ext_payload_layout, ext_value, ext_payload_layout);
        return try self.allocBoxyDynamicPayload(hooks, allocated.outer, desc.payload_layout, desc, target_layout);
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
            return try hooks.writeVariantPayloadValue(destination, expected_layout, payload_value, payload_layout);
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
                } else try hooks.materializeValue(
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

        try hooks.writeVariantPayloadValue(destination, expected_layout, payload_value, payload_layout);
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
                        .value = try hooks.materializeValue(tag_base.value, actual_payload_layout),
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
            } else try self.readBoxyTagPayloadValue(hooks, tag_base.value, actual_payload_layout, payload_index, target_layout);
            return .{
                .value = payload_value,
                .desc = payload_desc_ref,
            };
        }

        const ext_discriminant = self.boxyTagExtDiscriminant(source_desc) orelse {
            if (builtin.mode == .Debug) {
                debugPrint(
                    "debug_missing_tag_payload_variant proc={d} source_layout={d} source_desc_payload={d} wanted={s} disc={d} variants=",
                    .{
                        hooks.traceProcId(),
                        @intFromEnum(source_layout),
                        @intFromEnum(source_desc.payload_layout),
                        self.store.getString(tag_name),
                        disc,
                    },
                );
                for (self.requireBoxyTagVariants(source_desc.tag_variants)) |variant| {
                    debugPrint("{s}:{d}:{d} ", .{
                        self.store.getString(variant.name),
                        variant.discriminant,
                        @intFromEnum(variant.payload_layout),
                    });
                }
                debugPrint("tag_ext={any}\n", .{source_desc.tag_ext_desc});
                hooks.debugDumpProc();
            }
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
        const ext_value = try hooks.materializeValue(tag_base.value, ext_payload_layout);
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
        const raw_payload = try self.readRawBoxyTagPayloadValue(hooks, tag_base, actual_payload_layout, payload_index);
        const payload_value = try hooks.coerceToLayout(raw_payload.value, raw_payload.layout, target_layout);
        return try hooks.materializeValue(payload_value, target_layout);
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
                    .value = try hooks.materializeValue(tag_base.offset(field_offset), actual_field_layout),
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
                    .value = try hooks.materializeValue(tag_base, actual_payload_layout),
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
            return try hooks.materializeValue(value, expected_layout);
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
                    if (payload_size == 0) return try hooks.materializeValue(Value.zst, expected_layout);
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
            const target = try hooks.allocTagValue(expected_layout);
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

        const coerced = try hooks.coerceToLayout(value, actual_layout, expected_layout);
        return try hooks.materializeValue(coerced, expected_layout);
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
                .value = try hooks.materializeValue(actual_base.value, actual_payload_layout),
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

        return try self.materializeBoxyPayloadToLayout(
            hooks,
            value,
            actual_layout,
            target_desc,
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
        if (source_list.len() == 0) {
            return try hooks.rocListToValue(canonicalZstList(0), expected_layout);
        }

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

        if (target_elem_size == 0) {
            return try hooks.rocListToValue(canonicalZstList(source_list.len()), expected_layout);
        }
        const source_bytes = source_list.bytes orelse {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: non-empty boxy list payload had null source bytes for layout {d}",
                .{@intFromEnum(actual_layout)},
            );
        };

        const total_elem_bytes = target_elem_size * source_list.len();
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

        const desc_for_element_materialization = if (target_elem_desc) |resolved_target_elem_desc| blk: {
            const target_elem_layout_val = self.layout_store.getLayout(target_elem_layout);
            if (target_elem_layout_val.tag == .box or target_elem_layout_val.tag == .box_of_zst) {
                const payload_desc = try self.boxyBoxAllocationPayloadDesc(hooks, target_elem_layout, resolved_target_elem_desc) orelse resolved_target_elem_desc;
                if (payload_desc == resolved_target_elem_desc and
                    resolved_target_elem_desc.payload_layout == target_elem_layout)
                {
                    break :blk source_elem_desc orelse payload_desc;
                }
                break :blk payload_desc;
            }
            break :blk resolved_target_elem_desc;
        } else source_elem_desc;

        var index: usize = 0;
        while (index < source_list.len()) : (index += 1) {
            const source_elem = if (source_elem_size == 0)
                Value.zst
            else
                Value{ .ptr = source_bytes + index * source_elem_size };
            try self.performBoxyLayoutDrop(hooks, source_elem, source_elem_layout, source_elem_desc, .incref, 1, .atomic);
            const materialized = try self.materializeBoxyPayloadToLayout(
                hooks,
                source_elem,
                source_elem_layout,
                desc_for_element_materialization,
                target_elem_layout,
            );
            @memcpy(target_bytes[index * target_elem_size ..][0..target_elem_size], materialized.readBytes(target_elem_size));
        }

        return try hooks.rocListToValue(.{
            .bytes = target_bytes,
            .length = source_list.len(),
            .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(source_list.len()),
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
                return try hooks.materializeValue(value, expected_layout);
            }
            if (actual_layout == expected_layout and
                source_payload_desc != null and
                target_payload_desc != null and
                source_payload_desc.?.payload_layout == target_payload_desc.?.payload_layout)
            {
                return try hooks.materializeValue(value, expected_layout);
            }

            const target_allocation_desc = target_payload_desc orelse {
                switch (expected_layout_val.tag) {
                    .box_of_zst => return try hooks.allocBoxOfZstValue(expected_layout),
                    .box => {
                        const target_payload_layout = expected_layout_val.getIdx();
                        const materialized_payload = try self.materializeBoxyPayloadToLayout(
                            hooks,
                            value,
                            actual_layout,
                            source_desc,
                            target_payload_layout,
                        );
                        return try hooks.boxBox(materialized_payload, expected_layout);
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
                .value = try hooks.materializeValue(actual_base.value, actual_payload_layout),
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
                trace_rc.log(
                    "debug_null_box_payload proc={d} layout={d} desc_payload={d} desc_nested={d}+{d} payload_desc_payload={d} payload_desc_nested={d}+{d} payload_desc_contains={}",
                    .{
                        hooks.traceProcId(),
                        @intFromEnum(layout_idx),
                        @intFromEnum(desc.payload_layout),
                        desc.nested_descs.start,
                        desc.nested_descs.len,
                        @intFromEnum(payload_desc.payload_layout),
                        payload_desc.nested_descs.start,
                        payload_desc.nested_descs.len,
                        payload_desc.contains_refcounted,
                    },
                );
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
        const expected_data = self.layout_store.getStructData(expected_struct_idx);
        const source_desc_refs = self.requireBoxyDescRefs(source_desc.nested_descs);
        const target_desc_refs = self.requireBoxyDescRefs(target_desc.nested_descs);
        var next_source_desc: usize = 0;
        var next_target_desc: usize = 0;

        const target = try hooks.allocValue(expected_layout);
        var original_index: u32 = 0;
        while (original_index < expected_data.fields.count) : (original_index += 1) {
            const expected_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(expected_struct_idx, original_index);
            const expected_field_size = self.helper.sizeOf(expected_field_layout);
            const actual_field_layout = self.layout_store.getStructFieldLayoutByOriginalIndex(actual_struct_idx, original_index);

            const source_field_desc = if (self.layoutNeedsBoxyStructuralDesc(actual_field_layout)) blk: {
                if (next_source_desc >= source_desc_refs.len) {
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: source boxy struct descriptor for layout {d} was missing nested descriptor {d}",
                        .{ @intFromEnum(actual_layout), next_source_desc },
                    );
                }
                const resolved = try hooks.resolveDescRef(source_desc_refs[next_source_desc]);
                next_source_desc += 1;
                break :blk resolved;
            } else null;
            const target_field_desc = if (self.layoutNeedsBoxyStructuralDesc(expected_field_layout)) blk: {
                if (next_target_desc >= target_desc_refs.len) {
                    if (builtin.mode == .Debug) {
                        debugPrint(
                            "debug_struct_target_missing actual={d} expected={d} field={d} expected_field={d} expected_field_tag={s} source_payload={d} source_nested={d}+{d} target_payload={d} target_nested={d}+{d} target_variants={d}+{d}\n",
                            .{
                                @intFromEnum(actual_layout),
                                @intFromEnum(expected_layout),
                                original_index,
                                @intFromEnum(expected_field_layout),
                                @tagName(self.layout_store.getLayout(expected_field_layout).tag),
                                @intFromEnum(source_desc.payload_layout),
                                source_desc.nested_descs.start,
                                source_desc.nested_descs.len,
                                @intFromEnum(target_desc.payload_layout),
                                target_desc.nested_descs.start,
                                target_desc.nested_descs.len,
                                target_desc.tag_variants.start,
                                target_desc.tag_variants.len,
                            },
                        );
                    }
                    return self.invariantFailedError(
                        "LIR/interpreter invariant violated: target boxy struct descriptor for layout {d} was missing nested descriptor {d}",
                        .{ @intFromEnum(expected_layout), next_target_desc },
                    );
                }
                const resolved = try hooks.resolveDescRef(target_desc_refs[next_target_desc]);
                next_target_desc += 1;
                break :blk resolved;
            } else null;

            if (expected_field_size == 0) continue;
            const actual_field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(actual_struct_idx, original_index);
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
        const target = try hooks.allocTagValue(expected_layout);
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
            return try hooks.materializeValue(value, expected_layout);
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
            const target = try hooks.allocTagValue(expected_layout);
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
            try hooks.writeVariantPayloadValue(target.base, ext_slot_layout, ext_value, ext_slot_layout);
            return target.outer;
        };

        const target = try hooks.allocTagValue(expected_layout);
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
};
