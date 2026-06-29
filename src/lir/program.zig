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
    layout_idx: layout.Idx,
    plan: ConstPlanId,
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
};

/// Capture field copied from a checked binder into a callable payload.
pub const CaptureSlot = struct {
    id: const_store.CaptureId,
    slot: u32,
    plan: ConstPlanId,
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

pub const BoxyTypeDescId = LIR.BoxyTypeDescId;
pub const BoxyDictId = LIR.BoxyDictId;
pub const BoxyAdapterId = LIR.BoxyAdapterId;
pub const BoxyDescRef = LIR.BoxyDescRef;
pub const BoxyDictRef = LIR.BoxyDictRef;
pub const BoxySpan = LIR.BoxySpan;
pub const BoxyTransferMode = LIR.BoxyTransferMode;
pub const BoxyAdaptStep = LIR.BoxyAdaptStep;
pub const BoxyPayloadOp = LIR.BoxyPayloadOp;
pub const BoxyPayloadStep = LIR.BoxyPayloadStep;

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

/// Explicit representation adaptation plan used by boxy LIR statements.
pub const BoxyAdapter = struct {
    kind: BoxyAdapterKind,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    source_desc: ?BoxyDescRef = null,
    target_desc: ?BoxyDescRef = null,
    steps: BoxySpan = .{},
    consumes_source: bool,
    produces_owned_result: bool,
};

/// Runtime data for representation and structural operations on a boxy value.
pub const BoxyTypeDesc = struct {
    payload_layout: layout.Idx,
    contains_refcounted: bool,
    nested_descs: BoxySpan = .{},
    copy_plan: BoxySpan = .{},
    drop_plan: BoxySpan = .{},
    structural_eq: ?LIR.LirProcSpecId = null,
    structural_hash: ?LIR.LirProcSpecId = null,
    debug_checked_type: ?checked.CheckedTypeId = null,
};

/// Adapter metadata for one dictionary method slot.
pub const BoxyMethodAdapter = struct {
    arg_layouts: BoxySpan = .{},
    ret_layout: ?layout.Idx = null,
    arg_descs: BoxySpan = .{},
    ret_desc: ?BoxyDescRef = null,
    nested_dicts: BoxySpan = .{},
};

/// One callable slot in a boxy dictionary.
pub const BoxyMethodSlot = struct {
    method: names.MethodNameId,
    proc: LIR.LirProcSpecId,
    adapter: BoxyMethodAdapter = .{},
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
    plan: ConstPlanId,
};

/// Complete LIR program and side data consumed by ARC, backends, and eval.
pub const Result = struct {
    store: LirStore,
    layouts: layout.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    root_metadata: std.ArrayList(root.RootMetadata),
    requested_layouts: std.ArrayList(RequestedLayout),
    fn_sets: std.ArrayList(FnSet),
    erased_fns: std.ArrayList(ErasedFns),
    boxy_type_descs: std.ArrayList(BoxyTypeDesc),
    boxy_dicts: std.ArrayList(BoxyDict),
    boxy_adapters: std.ArrayList(BoxyAdapter),
    boxy_desc_refs: std.ArrayList(BoxyDescRef),
    boxy_dict_refs: std.ArrayList(BoxyDictRef),
    boxy_adapt_steps: std.ArrayList(BoxyAdaptStep),
    boxy_payload_steps: std.ArrayList(BoxyPayloadStep),
    boxy_method_slots: std.ArrayList(BoxyMethodSlot),
    boxy_method_arg_layouts: std.ArrayList(layout.Idx),
    const_plans: std.ArrayList(ConstPlan),
    const_roots: std.ArrayList(ConstRootPlan),
    comptime_sites: std.ArrayList(LIR.ComptimeSite),

    pub fn init(allocator: Allocator, target_usize: @import("base").target.TargetUsize) Allocator.Error!Result {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout.Store.init(allocator, target_usize),
            .root_procs = .empty,
            .root_metadata = .empty,
            .requested_layouts = .empty,
            .fn_sets = .empty,
            .erased_fns = .empty,
            .boxy_type_descs = .empty,
            .boxy_dicts = .empty,
            .boxy_adapters = .empty,
            .boxy_desc_refs = .empty,
            .boxy_dict_refs = .empty,
            .boxy_adapt_steps = .empty,
            .boxy_payload_steps = .empty,
            .boxy_method_slots = .empty,
            .boxy_method_arg_layouts = .empty,
            .const_plans = .empty,
            .const_roots = .empty,
            .comptime_sites = .empty,
        };
    }

    pub fn deinit(self: *Result) void {
        const allocator = self.store.allocator;
        for (self.comptime_sites.items) |site| {
            allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(allocator);
        deinitConstPlans(allocator, self.const_plans.items);
        self.const_roots.deinit(allocator);
        self.const_plans.deinit(allocator);
        deinitFnSets(allocator, self.fn_sets.items);
        deinitErasedFns(allocator, self.erased_fns.items);
        self.boxy_method_arg_layouts.deinit(allocator);
        self.boxy_method_slots.deinit(allocator);
        self.boxy_payload_steps.deinit(allocator);
        self.boxy_adapt_steps.deinit(allocator);
        self.boxy_dict_refs.deinit(allocator);
        self.boxy_desc_refs.deinit(allocator);
        self.boxy_adapters.deinit(allocator);
        self.boxy_dicts.deinit(allocator);
        self.boxy_type_descs.deinit(allocator);
        self.erased_fns.deinit(allocator);
        self.fn_sets.deinit(allocator);
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
        }
        if (fn_set.variants.len > 0) allocator.free(fn_set.variants);
    }
}

/// Free slices owned by erased callable entry sets.
pub fn deinitErasedFns(allocator: Allocator, erased_fns: []const ErasedFns) void {
    for (erased_fns) |set| {
        for (set.entries) |entry| {
            if (entry.captures.len > 0) allocator.free(entry.captures);
        }
        if (set.entries.len > 0) allocator.free(set.entries);
    }
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

    const desc_refs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(0) });
    const desc_refs = BoxySpan{ .start = @intCast(desc_refs_start), .len = 1 };

    const copy_plan_start = result.boxy_payload_steps.items.len;
    try result.boxy_payload_steps.append(allocator, .{ .dynamic = .{
        .op = .copy,
        .desc = .{ .static = @enumFromInt(0) },
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
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(0) });
    const arg_descs = BoxySpan{ .start = @intCast(arg_descs_start), .len = 1 };

    const nested_dicts_start = result.boxy_dict_refs.items.len;
    try result.boxy_dict_refs.append(allocator, .{ .static = @enumFromInt(0) });
    const nested_dicts = BoxySpan{ .start = @intCast(nested_dicts_start), .len = 1 };

    const method_slots_start = result.boxy_method_slots.items.len;
    try result.boxy_method_slots.append(allocator, .{
        .method = @enumFromInt(0),
        .proc = @enumFromInt(0),
        .adapter = .{
            .arg_layouts = arg_layouts,
            .arg_descs = arg_descs,
            .nested_dicts = nested_dicts,
        },
    });
    const method_slots = BoxySpan{ .start = @intCast(method_slots_start), .len = 1 };

    const hidden_descs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(0) });
    const hidden_descs = BoxySpan{ .start = @intCast(hidden_descs_start), .len = 1 };

    try result.boxy_dicts.append(allocator, .{
        .method_slots = method_slots,
        .hidden_descs = hidden_descs,
    });

    const adapt_steps_start = result.boxy_adapt_steps.items.len;
    try result.boxy_adapt_steps.append(allocator, .{ .dynamic_payload = .{
        .source_offset = 0,
        .target_offset = 8,
        .source_desc = .{ .static = @enumFromInt(0) },
        .target_desc = .{ .static = @enumFromInt(0) },
        .mode = .copy,
    } });
    const adapt_steps = BoxySpan{ .start = @intCast(adapt_steps_start), .len = 1 };

    try result.boxy_adapters.append(allocator, .{
        .kind = .boxy_to_host,
        .source_layout = .str,
        .target_layout = .str,
        .source_desc = .{ .static = @enumFromInt(0) },
        .target_desc = .{ .static = @enumFromInt(0) },
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
}

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
