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

/// Identifier for one boxy runtime type descriptor owned by a LIR program.
pub const BoxyTypeDescId = enum(u32) { _ };

/// Identifier for one boxy runtime dictionary owned by a LIR program.
pub const BoxyDictId = enum(u32) { _ };

/// Reference to type-descriptor data available to boxy LIR.
pub const BoxyDescRef = union(enum) {
    static: BoxyTypeDescId,
    local: LIR.LocalId,
};

/// Reference to dictionary data available to boxy LIR.
pub const BoxyDictRef = union(enum) {
    static: BoxyDictId,
    local: LIR.LocalId,
};

/// Explicit runtime operation needed for a dynamic boxy payload.
pub const BoxyPayloadOp = enum {
    copy,
    incref,
    decref,
    drop,
    free,
};

/// One explicitly planned payload operation. It never asks a backend to infer
/// reference-counting behavior from a pointer-shaped value.
pub const BoxyPayloadStep = union(enum) {
    concrete: struct {
        op: BoxyPayloadOp,
        layout_idx: layout.Idx,
    },
    dynamic: struct {
        op: BoxyPayloadOp,
        desc: BoxyDescRef,
    },
};

/// Runtime data for representation and structural operations on a boxy value.
pub const BoxyTypeDesc = struct {
    payload_layout: layout.Idx,
    contains_refcounted: bool,
    nested_descs: []const BoxyDescRef = &.{},
    copy_plan: []const BoxyPayloadStep = &.{},
    drop_plan: []const BoxyPayloadStep = &.{},
    structural_eq: ?LIR.LirProcSpecId = null,
    structural_hash: ?LIR.LirProcSpecId = null,
    debug_checked_type: ?checked.CheckedTypeId = null,
};

/// Adapter metadata for one dictionary method slot.
pub const BoxyMethodAdapter = struct {
    arg_layouts: []const layout.Idx = &.{},
    ret_layout: ?layout.Idx = null,
    arg_descs: []const BoxyDescRef = &.{},
    ret_desc: ?BoxyDescRef = null,
    nested_dicts: []const BoxyDictRef = &.{},
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
    method_slots: []const BoxyMethodSlot = &.{},
    hidden_descs: []const BoxyDescRef = &.{},
    nested_dicts: []const BoxyDictRef = &.{},
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
        deinitBoxyTypeDescs(allocator, self.boxy_type_descs.items);
        deinitBoxyDicts(allocator, self.boxy_dicts.items);
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

/// Free slices owned by boxy runtime type descriptors.
pub fn deinitBoxyTypeDescs(allocator: Allocator, descs: []const BoxyTypeDesc) void {
    for (descs) |desc| {
        if (desc.nested_descs.len > 0) allocator.free(desc.nested_descs);
        if (desc.copy_plan.len > 0) allocator.free(desc.copy_plan);
        if (desc.drop_plan.len > 0) allocator.free(desc.drop_plan);
    }
}

/// Free slices owned by boxy runtime dictionaries.
pub fn deinitBoxyDicts(allocator: Allocator, dicts: []const BoxyDict) void {
    for (dicts) |dict| {
        for (dict.method_slots) |slot| {
            if (slot.adapter.arg_layouts.len > 0) allocator.free(slot.adapter.arg_layouts);
            if (slot.adapter.arg_descs.len > 0) allocator.free(slot.adapter.arg_descs);
            if (slot.adapter.nested_dicts.len > 0) allocator.free(slot.adapter.nested_dicts);
        }
        if (dict.method_slots.len > 0) allocator.free(dict.method_slots);
        if (dict.hidden_descs.len > 0) allocator.free(dict.hidden_descs);
        if (dict.nested_dicts.len > 0) allocator.free(dict.nested_dicts);
    }
}

test "boxy side tables initialize empty and own nested slices" {
    const allocator = std.testing.allocator;
    var result = try Result.init(allocator, .u64);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 0), result.boxy_type_descs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_dicts.items.len);

    const nested_descs = try allocator.dupe(BoxyDescRef, &[_]BoxyDescRef{.{ .static = @enumFromInt(0) }});
    var nested_descs_owned = false;
    errdefer if (!nested_descs_owned) allocator.free(nested_descs);
    const copy_plan = try allocator.dupe(BoxyPayloadStep, &[_]BoxyPayloadStep{.{ .dynamic = .{
        .op = .copy,
        .desc = .{ .static = @enumFromInt(0) },
    } }});
    var copy_plan_owned = false;
    errdefer if (!copy_plan_owned) allocator.free(copy_plan);
    const drop_plan = try allocator.dupe(BoxyPayloadStep, &[_]BoxyPayloadStep{.{ .concrete = .{
        .op = .drop,
        .layout_idx = .zst,
    } }});
    var drop_plan_owned = false;
    errdefer if (!drop_plan_owned) allocator.free(drop_plan);

    try result.boxy_type_descs.append(allocator, .{
        .payload_layout = .zst,
        .contains_refcounted = true,
        .nested_descs = nested_descs,
        .copy_plan = copy_plan,
        .drop_plan = drop_plan,
    });
    nested_descs_owned = true;
    copy_plan_owned = true;
    drop_plan_owned = true;

    const arg_layouts = try allocator.dupe(layout.Idx, &[_]layout.Idx{.zst});
    var arg_layouts_owned = false;
    errdefer if (!arg_layouts_owned) allocator.free(arg_layouts);
    const arg_descs = try allocator.dupe(BoxyDescRef, &[_]BoxyDescRef{.{ .static = @enumFromInt(0) }});
    var arg_descs_owned = false;
    errdefer if (!arg_descs_owned) allocator.free(arg_descs);
    const nested_dicts = try allocator.dupe(BoxyDictRef, &[_]BoxyDictRef{.{ .static = @enumFromInt(0) }});
    var nested_dicts_owned = false;
    errdefer if (!nested_dicts_owned) allocator.free(nested_dicts);
    const method_slots = try allocator.dupe(BoxyMethodSlot, &[_]BoxyMethodSlot{.{
        .method = @enumFromInt(0),
        .proc = @enumFromInt(0),
        .adapter = .{
            .arg_layouts = arg_layouts,
            .arg_descs = arg_descs,
            .nested_dicts = nested_dicts,
        },
    }});
    var method_slots_owned = false;
    errdefer if (!method_slots_owned) allocator.free(method_slots);
    const hidden_descs = try allocator.dupe(BoxyDescRef, &[_]BoxyDescRef{.{ .static = @enumFromInt(0) }});
    var hidden_descs_owned = false;
    errdefer if (!hidden_descs_owned) allocator.free(hidden_descs);

    try result.boxy_dicts.append(allocator, .{
        .method_slots = method_slots,
        .hidden_descs = hidden_descs,
    });
    arg_layouts_owned = true;
    arg_descs_owned = true;
    nested_dicts_owned = true;
    method_slots_owned = true;
    hidden_descs_owned = true;

    try std.testing.expectEqual(@as(usize, 1), result.boxy_type_descs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_dicts.items.len);
}

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
