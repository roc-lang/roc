//! LIR program result shared by post-check lowering, ARC, LirImage, glue, and
//! interpreter consumers.

const std = @import("std");
const check = @import("check");
const layout = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const root = @import("root_metadata.zig");

const Allocator = std.mem.Allocator;
const names = check.CheckedNames;
const checked = check.CheckedModule;
const const_store = check.ConstStore;

pub const RequestedLayout = struct {
    ty: names.ExecValueDigest,
    layout_idx: layout.Idx,
};

pub const FnSetId = enum(u32) { _ };
pub const ErasedFnsId = enum(u32) { _ };
pub const FnVariantId = enum(u32) { _ };

pub const FnResult = union(enum) {
    finite: FnSetId,
    erased: ErasedFnsId,
};

pub const FnTemplate = struct {
    fn_def: const_store.FnDef,
    source_fn_ty: checked.CheckedTypeId,
};

pub const CaptureSlot = struct {
    binder: checked.PatternBinderId,
    slot: u32,
    plan: ConstPlanId,
};

pub const FnVariant = struct {
    id: FnVariantId,
    discriminant: u16,
    variant_index: u16,
    payload_layout: layout.Idx,
    template: FnTemplate,
    captures: []const CaptureSlot = &.{},
};

pub const FnSet = struct {
    layout: layout.Idx,
    variants: []const FnVariant = &.{},
};

pub const ErasedFn = struct {
    entry: LIR.LirProcSpecId,
    template: FnTemplate,
    captures: []const CaptureSlot = &.{},
};

pub const ErasedFns = struct {
    layout: layout.Idx,
    entries: []const ErasedFn = &.{},
};

pub const ConstPlanId = enum(u32) { _ };

pub const ConstTagVariant = struct {
    name: names.TagNameId,
    discriminant: u16,
    payloads: []const ConstPlanId = &.{},
};

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

pub const ConstRootPlan = struct {
    root_order: u32,
    request: check.CheckedModule.RootRequest,
    proc: LIR.LirProcSpecId,
    ret_layout: layout.Idx,
    plan: ConstPlanId,
};

pub const Result = struct {
    store: LirStore,
    layouts: layout.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    root_metadata: std.ArrayList(root.RootMetadata),
    requested_layouts: std.ArrayList(RequestedLayout),
    fn_sets: std.ArrayList(FnSet),
    erased_fns: std.ArrayList(ErasedFns),
    const_plans: std.ArrayList(ConstPlan),
    const_roots: std.ArrayList(ConstRootPlan),

    pub fn init(allocator: Allocator, target_usize: @import("base").target.TargetUsize) Allocator.Error!Result {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout.Store.init(allocator, target_usize),
            .root_procs = .empty,
            .root_metadata = .empty,
            .requested_layouts = .empty,
            .fn_sets = .empty,
            .erased_fns = .empty,
            .const_plans = .empty,
            .const_roots = .empty,
        };
    }

    pub fn deinit(self: *Result) void {
        const allocator = self.store.allocator;
        deinitConstPlans(allocator, self.const_plans.items);
        self.const_roots.deinit(allocator);
        self.const_plans.deinit(allocator);
        deinitFnSets(allocator, self.fn_sets.items);
        deinitErasedFns(allocator, self.erased_fns.items);
        self.erased_fns.deinit(allocator);
        self.fn_sets.deinit(allocator);
        self.requested_layouts.deinit(allocator);
        self.root_metadata.deinit(allocator);
        self.root_procs.deinit(allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    pub fn requestedLayoutForType(self: *const Result, ty: names.ExecValueDigest) ?layout.Idx {
        for (self.requested_layouts.items) |entry| {
            if (std.mem.eql(u8, entry.ty.bytes[0..], ty.bytes[0..])) return entry.layout_idx;
        }
        return null;
    }
};

pub fn deinitConstPlans(allocator: Allocator, plans: []const ConstPlan) void {
    for (plans) |plan| {
        switch (plan) {
            .tuple => |items| allocator.free(items),
            .record => |fields| allocator.free(fields),
            .tag_union => |variants| {
                for (variants) |variant| allocator.free(variant.payloads);
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

pub fn deinitFnSets(allocator: Allocator, fn_sets: []const FnSet) void {
    for (fn_sets) |fn_set| {
        for (fn_set.variants) |variant| {
            if (variant.captures.len > 0) allocator.free(variant.captures);
        }
        if (fn_set.variants.len > 0) allocator.free(fn_set.variants);
    }
}

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
