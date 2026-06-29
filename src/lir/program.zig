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
    ty: const_store.ConstTypeId,
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
    const_types: const_store.ConstTypeStore,
    const_type_names: names.NameStore,
    fn_sets: std.ArrayList(FnSet),
    erased_fns: std.ArrayList(ErasedFns),
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
            .const_types = const_store.ConstTypeStore.init(allocator),
            .const_type_names = names.NameStore.init(allocator),
            .fn_sets = .empty,
            .erased_fns = .empty,
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

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
