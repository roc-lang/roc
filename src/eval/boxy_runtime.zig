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

const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LirProgram = lir.Program;
const RocOps = builtins.host_abi.RocOps;
const Allocator = std.mem.Allocator;

const is_freestanding = builtin.target.os.tag == .freestanding;

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
/// dependencies. Callers resolve descriptor references to `*const BoxyTypeDesc`
/// pointers and pass them in; the runtime never resolves frame-local handles.
pub const BoxyRuntime = struct {
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    boxy_tables: BoxyTables,
    runtime_boxy_tag_variants: *const std.ArrayList(LirProgram.BoxyTagVariant),
    runtime_boxy_desc_refs: *const std.ArrayList(LirProgram.BoxyDescRef),
    runtime_boxy_tag_payload_descs: *const std.ArrayList(LirProgram.BoxyTagPayloadDesc),
    roc_ops: *RocOps,
    scratch: Allocator,

    fn invariantFailed(_: *const BoxyRuntime, comptime fmt: []const u8, args: anytype) noreturn {
        if (builtin.mode == .Debug) {
            debugPrint(fmt, args);
            debugPrint("\n", .{});
            std.debug.assert(false);
        }
        unreachable;
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
};
