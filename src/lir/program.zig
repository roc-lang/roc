//! LIR program result shared by post-check lowering, ARC, LirImage, glue, and
//! interpreter consumers.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const root = @import("root_metadata.zig");

const Allocator = std.mem.Allocator;
const names = check.CheckedNames;
const checked = check.CheckedModule;
const const_store = check.ConstStore;
const dispatch = check.StaticDispatchRegistry;

/// Layout requested for a checked value type digest.
pub const RequestedLayout = struct {
    ty: names.TypeDigest,
    checked_type: checked.CheckedTypeId,
    const_locator: ?checked.ConstLocator = null,
    layout_idx: layout.Idx,
    plan: ConstPlanId,
    /// Closed LIR procedure that constructs the exact target representation for
    /// a provided static data export. Plain layout-only requests leave this null.
    initializer: ?LIR.LirProcSpecId = null,
};

/// Identifier for a finite callable set in the LIR program.
pub const FnSetId = enum(u32) { _ };
/// Identifier for an erased callable entry set in the LIR program.
pub const ErasedFnsId = enum(u32) { _ };
/// Identifier for one finite callable variant.
pub const FnVariantId = enum(u32) { _ };

/// Callable lowering result used by const plans.
pub const FnResult = union(enum) {
    finite: FnSetId,
    erased: ErasedFnsId,
};

/// Checked function template and source type used to emit callable code.
pub const FnTemplate = struct {
    fn_def: const_store.FnDef,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    evidence: []const const_store.ConstFnEvidence = &.{},
    evidence_frames: []const const_store.ConstFnEvidenceFrame = &.{},
    evidence_frame_head: ?u32 = null,
};

/// Capture field copied from a checked binding into a callable payload. `id`
/// is checked-stage provenance for storing a compile-time result in
/// `ConstStore`; runtime capture joining was completed before LIR.
pub const CaptureSlot = struct {
    id: const_store.CaptureId,
    slot: u32,
    ty: const_store.ConstTypeId,
    plan: ConstPlanId,
    storage: CaptureSlotStorage,
};

/// Physical storage used by a callable capture slot while storing its value.
pub const CaptureSlotStorage = enum(u8) {
    value,
    recursive_box,
};

/// One runtime tag variant for a finite callable value.
pub const FnVariant = struct {
    id: FnVariantId,
    discriminant: u16,
    variant_index: u16,
    payload_layout: layout.Idx,
    template: FnTemplate,
    captures: []const CaptureSlot = &.{},
};

/// Runtime tag-union encoding for a finite callable set.
pub const FnSet = struct {
    layout: layout.Idx,
    variants: []const FnVariant = &.{},
};

/// One erased callable entry and its capture layout plan.
pub const ErasedFn = struct {
    entry: LIR.LirProcSpecId,
    capture_layout: layout.Idx = .zst,
    template: FnTemplate,
    captures: []const CaptureSlot = &.{},
};

/// Runtime encoding for an erased callable value type.
pub const ErasedFns = struct {
    layout: layout.Idx,
    entries: []const ErasedFn = &.{},
};

/// Identifier for a constant storage plan emitted with LIR.
pub const ConstPlanId = enum(u32) { _ };

/// Stable index of a Boxy type descriptor in the program side tables.
pub const BoxyTypeDescId = LIR.BoxyTypeDescId;
/// Stable index of a Boxy method dictionary in the program side tables.
pub const BoxyDictId = LIR.BoxyDictId;
/// Stable index of an explicit Boxy representation adapter.
pub const BoxyAdapterId = LIR.BoxyAdapterId;
/// Stable index of a method slot within a Boxy dictionary.
pub const BoxyMethodSlotId = enum(u32) { _ };
/// Explicit source for resolving a Boxy type descriptor.
pub const BoxyDescRef = LIR.BoxyDescRef;
/// Explicit source for resolving a Boxy dictionary.
pub const BoxyDictRef = LIR.BoxyDictRef;
/// Compact start and length pair into a Boxy side table.
pub const BoxySpan = LIR.BoxySpan;
/// Ownership transfer applied by one Boxy adaptation step.
pub const BoxyTransferMode = LIR.BoxyTransferMode;
/// One descriptor-guided representation adaptation step.
pub const BoxyAdaptStep = LIR.BoxyAdaptStep;
/// Operation performed by one descriptor payload traversal step.
pub const BoxyPayloadOp = LIR.BoxyPayloadOp;
/// One explicit descriptor payload traversal step.
pub const BoxyPayloadStep = LIR.BoxyPayloadStep;

/// Runtime metadata for one tag in a boxy tag-union descriptor.
pub const BoxyTagVariant = struct {
    name: base.StringLiteral.Idx,
    discriminant: u16,
    /// Number of source-language payloads carried by this tag. A single
    /// aggregate payload is distinct from a multi-payload tag whose runtime
    /// payload is also a struct.
    payload_count: u32 = 0,
    payload_layout: layout.Idx,
    payload_descs: BoxySpan = .{},
};

/// Descriptor metadata for one dynamic payload in a boxy tag variant.
pub const BoxyTagPayloadDesc = struct {
    payload_index: u32,
    desc: BoxyDescRef,
};

/// Purpose of one explicit boxy representation adapter.
pub const BoxyAdapterKind = enum {
    host_to_boxy,
    boxy_to_host,
    boxy_to_boxy,
    hosted_arg,
    hosted_ret,
    container_element,
    method_arg,
    method_ret,
};

/// Runtime operation selected when an adapter is built after layout planning.
pub const BoxyAdapterOperation = enum {
    relabel,
    materialize,
};

/// Explicit representation adaptation plan used by boxy LIR statements.
pub const BoxyAdapter = struct {
    kind: BoxyAdapterKind,
    operation: BoxyAdapterOperation = .materialize,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    steps: BoxySpan = .{},
    consumes_source: bool,
    produces_owned_result: bool,
};

/// Runtime data for representation and structural operations on a boxy value.
pub const BoxyTypeDesc = struct {
    payload_layout: layout.Idx,
    contains_refcounted: bool,
    nested_descs: BoxySpan = .{},
    tag_variants: BoxySpan = .{},
    tag_ext_desc: ?BoxyDescRef = null,
    /// Record field names in payload field order, one per field. Empty for
    /// non-record payloads (including tuples, which print positionally).
    field_names: BoxySpan = .{},
    /// Present-variant discriminant when these bytes use the canonical
    /// optional-field slot convention. This is compiler-produced semantic
    /// data, not a runtime inference from tags or layouts.
    presence_slot_present_discriminant: ?u16 = null,
    /// The described value is an opaque nominal type: inspect must not
    /// reveal its backing structure.
    inspect_opaque: bool = false,
    copy_plan: BoxySpan = .{},
    drop_plan: BoxySpan = .{},
    structural_eq: ?LIR.LirProcSpecId = null,
    structural_hash: ?LIR.LirProcSpecId = null,
    inspect_method: ?BoxyMethodSlotId = null,
    debug_checked_type: ?checked.CheckedTypeId = null,
};

/// Adapter metadata for one dictionary method slot.
pub const BoxyMethodAdapter = struct {
    arg_layouts: BoxySpan = .{},
    ret_layout: ?layout.Idx = null,
    arg_descs: BoxySpan = .{},
    /// Compact static descriptor references used by `call_desc_sources`.
    call_descs: BoxySpan = .{},
    /// Exact source for every descriptor-bearing position in the checked
    /// method requirement, in requirement traversal order. Empty means the
    /// legacy one-to-one static `call_descs` representation.
    call_desc_sources: BoxySpan = .{},
    ret_desc: ?BoxyDescRef = null,
    nested_dicts: BoxySpan = .{},
    hidden_desc_sources: BoxySpan = .{},
};

/// Origin of a hidden descriptor argument passed to a dictionary method.
pub const BoxyMethodHiddenDescSource = union(enum) {
    slot: u32,
    call: u32,
    argument: u32,
};

/// One callable slot in a boxy dictionary.
pub const BoxyMethodSlot = struct {
    /// False for an unimplemented program-wide method slot in a dictionary
    /// that requires only a subset of the program's semantic methods.
    present: bool = true,
    method: names.MethodNameId,
    proc: LIR.LirProcSpecId,
    hidden_descs: BoxySpan = .{},
    nested_dicts: BoxySpan = .{},
    adapter: BoxyMethodAdapter = .{},
    /// The slot is fulfilled by descriptor-guided structural equality of the
    /// two explicit arguments; `proc` is unused. Anonymous structural types
    /// have no method namespace, so their equality dictionary slots dispatch
    /// to the runtime's structural comparison instead of a worker.
    structural_eq: bool = false,
};

/// Runtime data for polymorphic behavior and static dispatch in boxy LIR.
pub const BoxyDict = struct {
    debug_dispatch_plan: ?dispatch.StaticDispatchPlanId = null,
    method_slots: BoxySpan = .{},
    hidden_descs: BoxySpan = .{},
    nested_dicts: BoxySpan = .{},
};

/// Tag variant in a constant storage plan.
pub const ConstTagVariant = struct {
    name: []const u8,
    checked_name: names.TagNameId,
    discriminant: u16,
    payloads: []const ConstPlanId = &.{},
};

/// Shape plan used to store an interpreted compile-time result in ConstStore.
pub const ConstPlan = union(enum) {
    pending,
    /// Layout-only request. This plan has no ConstStore materialization shape;
    /// consumers must use it only for requested layout metadata.
    layout_only,
    zst,
    scalar,
    str,
    list: ConstPlanId,
    box: ConstPlanId,
    tuple: []const ConstPlanId,
    record: []const ConstPlanId,
    tag_union: []const ConstTagVariant,
    named: struct {
        named_type: check.CheckedModule.ConstNamedType,
        backing: ConstPlanId,
    },
    fn_value: FnSetId,
    erased_fn: ErasedFnsId,
};

/// Constant root metadata needed after LIR interpretation finishes.
pub const ConstRootPlan = struct {
    root_order: u32,
    request: check.CheckedModule.RootRequest,
    proc: LIR.LirProcSpecId,
    ret_layout: layout.Idx,
    /// Exact producer-owned Monotype representation of the evaluated root.
    /// ConstStore restoration consumes this instead of reconstructing
    /// representation evidence from the public checked type.
    ret_type: const_store.ConstTypeId,
    plan: ConstPlanId,
};

/// One exact LIR value construction that is frozen as readonly target data.
pub const StaticDataValue = struct {
    initializer: LIR.LirProcSpecId,
};

/// Deterministic symbol name for an internal static-data value.
pub fn staticDataSymbolName(allocator: Allocator, id: LIR.StaticDataId) Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, "roc__static_const_value_{d}", .{@intFromEnum(id)});
}

/// Complete LIR program and side data consumed by ARC, backends, and eval.
pub const Result = struct {
    store: LirStore,
    layouts: layout.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    root_metadata: std.ArrayList(root.RootMetadata),
    requested_layouts: std.ArrayList(RequestedLayout),
    const_types: const_store.ConstTypeStore,
    const_type_names: names.NameStore,
    fn_sets: std.ArrayList(FnSet),
    erased_fns: std.ArrayList(ErasedFns),
    boxy_type_descs: std.ArrayList(BoxyTypeDesc),
    boxy_dicts: std.ArrayList(BoxyDict),
    boxy_adapters: std.ArrayList(BoxyAdapter),
    boxy_desc_refs: std.ArrayList(BoxyDescRef),
    boxy_dict_refs: std.ArrayList(BoxyDictRef),
    boxy_tag_variants: std.ArrayList(BoxyTagVariant),
    boxy_tag_payload_descs: std.ArrayList(BoxyTagPayloadDesc),
    boxy_field_names: std.ArrayList(base.StringLiteral.Idx),
    boxy_adapt_steps: std.ArrayList(BoxyAdaptStep),
    boxy_payload_steps: std.ArrayList(BoxyPayloadStep),
    boxy_method_slots: std.ArrayList(BoxyMethodSlot),
    boxy_method_arg_layouts: std.ArrayList(layout.Idx),
    boxy_method_hidden_desc_sources: std.ArrayList(BoxyMethodHiddenDescSource),
    boxy_erased_arg_layouts: std.ArrayList(layout.Idx),
    boxy_erased_arg_desc_keys: std.ArrayList(LIR.ErasedArgDescKey),
    boxy_erased_arg_desc_offsets: std.ArrayList(LIR.ErasedArgDescOffset),
    boxy_erased_arg_desc_params: std.ArrayList(LIR.ErasedArgDescParam),
    const_plans: std.ArrayList(ConstPlan),
    const_roots: std.ArrayList(ConstRootPlan),
    static_data_values: std.ArrayList(StaticDataValue),
    comptime_sites: std.ArrayList(LIR.ComptimeSite),

    pub fn init(allocator: Allocator, target_usize: @import("base").target.TargetUsize) Allocator.Error!Result {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout.Store.init(allocator, target_usize),
            .root_procs = .empty,
            .root_metadata = .empty,
            .requested_layouts = .empty,
            .const_types = const_store.ConstTypeStore.init(allocator),
            .const_type_names = names.NameStore.init(allocator),
            .fn_sets = .empty,
            .erased_fns = .empty,
            .boxy_type_descs = .empty,
            .boxy_dicts = .empty,
            .boxy_adapters = .empty,
            .boxy_desc_refs = .empty,
            .boxy_dict_refs = .empty,
            .boxy_tag_variants = .empty,
            .boxy_tag_payload_descs = .empty,
            .boxy_field_names = .empty,
            .boxy_adapt_steps = .empty,
            .boxy_payload_steps = .empty,
            .boxy_method_slots = .empty,
            .boxy_method_arg_layouts = .empty,
            .boxy_method_hidden_desc_sources = .empty,
            .boxy_erased_arg_layouts = .empty,
            .boxy_erased_arg_desc_keys = .empty,
            .boxy_erased_arg_desc_offsets = .empty,
            .boxy_erased_arg_desc_params = .empty,
            .const_plans = .empty,
            .const_roots = .empty,
            .static_data_values = .empty,
            .comptime_sites = .empty,
        };
    }

    pub fn deinit(self: *Result) void {
        const allocator = self.store.allocator;
        for (self.comptime_sites.items) |site| {
            allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(allocator);
        self.static_data_values.deinit(allocator);
        deinitConstPlans(allocator, self.const_plans.items);
        self.const_roots.deinit(allocator);
        self.const_plans.deinit(allocator);
        deinitFnSets(allocator, self.fn_sets.items);
        deinitErasedFns(allocator, self.erased_fns.items);
        self.boxy_erased_arg_desc_params.deinit(allocator);
        self.boxy_erased_arg_desc_offsets.deinit(allocator);
        self.boxy_erased_arg_desc_keys.deinit(allocator);
        self.boxy_erased_arg_layouts.deinit(allocator);
        self.boxy_method_hidden_desc_sources.deinit(allocator);
        self.boxy_method_arg_layouts.deinit(allocator);
        self.boxy_method_slots.deinit(allocator);
        self.boxy_payload_steps.deinit(allocator);
        self.boxy_adapt_steps.deinit(allocator);
        self.boxy_field_names.deinit(allocator);
        self.boxy_tag_payload_descs.deinit(allocator);
        self.boxy_tag_variants.deinit(allocator);
        self.boxy_dict_refs.deinit(allocator);
        self.boxy_desc_refs.deinit(allocator);
        self.boxy_adapters.deinit(allocator);
        self.boxy_dicts.deinit(allocator);
        self.boxy_type_descs.deinit(allocator);
        self.erased_fns.deinit(allocator);
        self.fn_sets.deinit(allocator);
        self.const_type_names.deinit();
        self.const_types.deinit();
        self.requested_layouts.deinit(allocator);
        self.root_metadata.deinit(allocator);
        self.root_procs.deinit(allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    pub fn requestedLayoutForType(self: *const Result, ty: names.TypeDigest) ?layout.Idx {
        for (self.requested_layouts.items) |entry| {
            if (std.mem.eql(u8, entry.ty.bytes[0..], ty.bytes[0..])) return entry.layout_idx;
        }
        return null;
    }

    pub fn addComptimeSite(
        self: *Result,
        kind: LIR.ComptimeSiteKind,
        region: base.Region,
        checked_site: ?LIR.CheckedExhaustivenessSiteId,
        proc: LIR.LirProcSpecId,
        branch_regions: []const base.Region,
    ) Allocator.Error!LIR.ComptimeSiteId {
        const owned_branch_regions = try self.store.allocator.dupe(base.Region, branch_regions);
        errdefer self.store.allocator.free(owned_branch_regions);
        const id: LIR.ComptimeSiteId = @enumFromInt(@as(u32, @intCast(self.comptime_sites.items.len)));
        try self.comptime_sites.append(self.store.allocator, .{
            .kind = kind,
            .region = region,
            .checked_site = checked_site,
            .proc = proc,
            .branch_regions = owned_branch_regions,
        });
        return id;
    }
};

/// Free slices owned by constant storage plans.
pub fn deinitConstPlans(allocator: Allocator, plans: []const ConstPlan) void {
    for (plans) |plan| {
        switch (plan) {
            .tuple => |items| allocator.free(items),
            .record => |fields| allocator.free(fields),
            .tag_union => |variants| {
                for (variants) |variant| {
                    allocator.free(variant.name);
                    allocator.free(variant.payloads);
                }
                allocator.free(variants);
            },
            .zst,
            .layout_only,
            .pending,
            .scalar,
            .str,
            .list,
            .box,
            .named,
            => {},
            .fn_value,
            .erased_fn,
            => {},
        }
    }
}

/// Free slices owned by finite callable sets.
pub fn deinitFnSets(allocator: Allocator, fn_sets: []const FnSet) void {
    for (fn_sets) |fn_set| {
        for (fn_set.variants) |variant| {
            if (variant.captures.len > 0) allocator.free(variant.captures);
            if (variant.template.evidence.len > 0) allocator.free(variant.template.evidence);
            if (variant.template.evidence_frames.len > 0) allocator.free(variant.template.evidence_frames);
        }
        if (fn_set.variants.len > 0) allocator.free(fn_set.variants);
    }
}

/// Free slices owned by erased callable entry sets.
pub fn deinitErasedFns(allocator: Allocator, erased_fns: []const ErasedFns) void {
    for (erased_fns) |set| {
        for (set.entries) |entry| {
            if (entry.captures.len > 0) allocator.free(entry.captures);
            if (entry.template.evidence.len > 0) allocator.free(entry.template.evidence);
            if (entry.template.evidence_frames.len > 0) allocator.free(entry.template.evidence_frames);
        }
        if (set.entries.len > 0) allocator.free(set.entries);
    }
}

/// Convert an intentional fixture-table position while preserving enum inference.
fn fixtureTableIndex(comptime index: u32) u32 {
    return index;
}

test "boxy side tables initialize empty and use flat pools" {
    const allocator = std.testing.allocator;
    var result = try Result.init(allocator, .u64);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 0), result.boxy_type_descs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_dicts.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_adapters.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_desc_refs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_dict_refs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_adapt_steps.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_payload_steps.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_method_slots.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_method_arg_layouts.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_method_hidden_desc_sources.items.len);

    const desc_refs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const desc_refs = BoxySpan{ .start = @intCast(desc_refs_start), .len = 1 };

    const copy_plan_start = result.boxy_payload_steps.items.len;
    try result.boxy_payload_steps.append(allocator, .{ .dynamic = .{
        .op = .copy,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    } });
    const copy_plan = BoxySpan{ .start = @intCast(copy_plan_start), .len = 1 };

    const drop_plan_start = result.boxy_payload_steps.items.len;
    try result.boxy_payload_steps.append(allocator, .{ .concrete = .{
        .op = .drop,
        .layout_idx = .zst,
    } });
    const drop_plan = BoxySpan{ .start = @intCast(drop_plan_start), .len = 1 };

    try result.boxy_type_descs.append(allocator, .{
        .payload_layout = .zst,
        .contains_refcounted = true,
        .nested_descs = desc_refs,
        .copy_plan = copy_plan,
        .drop_plan = drop_plan,
    });

    const arg_layouts_start = result.boxy_method_arg_layouts.items.len;
    try result.boxy_method_arg_layouts.append(allocator, .zst);
    const arg_layouts = BoxySpan{ .start = @intCast(arg_layouts_start), .len = 1 };

    const arg_descs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const arg_descs = BoxySpan{ .start = @intCast(arg_descs_start), .len = 1 };

    const nested_dicts_start = result.boxy_dict_refs.items.len;
    try result.boxy_dict_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const nested_dicts = BoxySpan{ .start = @intCast(nested_dicts_start), .len = 1 };

    const hidden_desc_sources_start = result.boxy_method_hidden_desc_sources.items.len;
    try result.boxy_method_hidden_desc_sources.append(allocator, .{ .slot = 0 });
    const hidden_desc_sources = BoxySpan{ .start = @intCast(hidden_desc_sources_start), .len = 1 };

    const method_slots_start = result.boxy_method_slots.items.len;
    try result.boxy_method_slots.append(allocator, .{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(fixtureTableIndex(0)),
        .adapter = .{
            .arg_layouts = arg_layouts,
            .arg_descs = arg_descs,
            .nested_dicts = nested_dicts,
            .hidden_desc_sources = hidden_desc_sources,
        },
    });
    const method_slots = BoxySpan{ .start = @intCast(method_slots_start), .len = 1 };

    const hidden_descs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const hidden_descs = BoxySpan{ .start = @intCast(hidden_descs_start), .len = 1 };

    try result.boxy_dicts.append(allocator, .{
        .method_slots = method_slots,
        .hidden_descs = hidden_descs,
    });

    const adapt_steps_start = result.boxy_adapt_steps.items.len;
    try result.boxy_adapt_steps.append(allocator, .{ .dynamic_payload = .{
        .source_offset = 0,
        .target_offset = 8,
        .source_desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .target_desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .mode = .copy,
    } });
    const adapt_steps = BoxySpan{ .start = @intCast(adapt_steps_start), .len = 1 };

    try result.boxy_adapters.append(allocator, .{
        .kind = .boxy_to_host,
        .source_layout = .str,
        .target_layout = .str,
        .steps = adapt_steps,
        .consumes_source = false,
        .produces_owned_result = true,
    });

    try std.testing.expectEqual(@as(usize, 1), result.boxy_type_descs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_dicts.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_adapters.items.len);
    try std.testing.expectEqual(@as(usize, 3), result.boxy_desc_refs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_dict_refs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_adapt_steps.items.len);
    try std.testing.expectEqual(@as(usize, 2), result.boxy_payload_steps.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_method_slots.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_method_arg_layouts.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_method_hidden_desc_sources.items.len);
}

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
