//! Boxy checked-to-LIR lowerer.
//!
//! This lowerer consumes checked modules plus an explicit `Plan.ProgramPlan` and
//! produces ownership-neutral LIR. It is the only `.boxy` producer of LIR.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir_core = @import("lir_core");

const Common = @import("../common.zig");
const Layouts = @import("layouts.zig");
const Plan = @import("plan.zig");
const solved_lir_lower = @import("../solved_lir_lower.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const LirProgram = lir_core.Program;

pub const RuntimeSchemaStore = solved_lir_lower.RuntimeSchemaStore;

pub const Output = struct {
    lir_result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,

    pub fn deinit(self: *Output) void {
        self.runtime_schemas.deinit();
        self.lir_result.deinit();
    }
};

pub const Options = struct {
    target_usize: base.target.TargetUsize = .native,
};

pub fn run(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    plan: *const Plan.ProgramPlan,
    options: Options,
) Common.LowerError!Output {
    var result = try LirProgram.Result.init(allocator, options.target_usize);
    errdefer result.deinit();

    var layout_plan = try Layouts.build(allocator, plan, &result.layouts, .{});
    defer layout_plan.deinit();

    if (plan.roots.items.len != 0) {
        boxyLowerInvariant("boxy checked-to-LIR lowerer does not yet implement checked procedure lowering");
    }
    try appendRequestedLayouts(allocator, modules, roots, plan, &layout_plan, &result);

    return .{
        .lir_result = result,
        .runtime_schemas = RuntimeSchemaStore.init(allocator),
    };
}

fn appendRequestedLayouts(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    plan: *const Plan.ProgramPlan,
    layout_plan: *const Layouts.LayoutPlan,
    result: *LirProgram.Result,
) Allocator.Error!void {
    if (roots.layout_requests.len == 0) return;
    const root_rep_start = plan.roots.items.len;
    if (plan.root_reps.items.len < root_rep_start + roots.layout_requests.len) {
        boxyLowerInvariant("boxy plan root representations did not cover requested layouts");
    }

    var const_plans = try ConstPlanBuilder.init(allocator, modules, plan, result);
    defer const_plans.deinit();

    const root_types = modules.root.module.checked_types.view();
    for (roots.layout_requests, 0..) |checked_type, index| {
        const rep_id = plan.root_reps.items[root_rep_start + index];
        try result.requested_layouts.append(allocator, .{
            .ty = root_types.rootKey(checked_type),
            .checked_type = checked_type,
            .layout_idx = layout_plan.rep_layouts[@intFromEnum(rep_id)].host.layoutIdx(),
            .plan = try const_plans.constPlanForRep(rep_id),
        });
    }
}

const ConstPlanBuilder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    plan: *const Plan.ProgramPlan,
    result: *LirProgram.Result,
    by_rep: []?LirProgram.ConstPlanId,

    fn init(
        allocator: Allocator,
        modules: Common.CheckedModules,
        plan: *const Plan.ProgramPlan,
        result: *LirProgram.Result,
    ) Allocator.Error!ConstPlanBuilder {
        const by_rep = try allocator.alloc(?LirProgram.ConstPlanId, plan.representations.items.len);
        @memset(by_rep, null);
        return .{
            .allocator = allocator,
            .modules = modules,
            .plan = plan,
            .result = result,
            .by_rep = by_rep,
        };
    }

    fn deinit(self: *ConstPlanBuilder) void {
        self.allocator.free(self.by_rep);
        self.* = undefined;
    }

    fn constPlanForRep(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId) Allocator.Error!LirProgram.ConstPlanId {
        const index = @intFromEnum(rep_id);
        if (self.by_rep[index]) |existing| return existing;

        const rep = self.plan.representations.items[index];
        switch (rep.kind) {
            .alias => {
                const child = try self.constPlanForChild(rep_id, .alias_backing);
                self.by_rep[index] = child;
                return child;
            },
            .nominal => |kind| if (kind == .builtin_other) {
                const child = try self.constPlanForChild(rep_id, .nominal_backing);
                self.by_rep[index] = child;
                return child;
            },
            else => {},
        }

        const id: LirProgram.ConstPlanId = @enumFromInt(@as(u32, @intCast(self.result.const_plans.items.len)));
        try self.result.const_plans.append(self.allocator, .pending);
        self.by_rep[index] = id;

        const built = try self.buildConstPlan(rep_id);
        self.result.const_plans.items[@intFromEnum(id)] = built;
        return id;
    }

    fn buildConstPlan(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId) Allocator.Error!LirProgram.ConstPlan {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .in_progress => boxyLowerInvariant("in-progress boxy representation reached const plan output"),
            .dynamic => boxyLowerInvariant("dynamic boxy value reached const plan output before descriptor support"),
            .erased_callable => boxyLowerInvariant("erased callable reached const plan output before callable static-data support"),
            .primitive => |primitive| switch (primitive) {
                .str => .str,
                else => .scalar,
            },
            .bool_tag_union => .scalar,
            .empty_record,
            .empty_tag_union,
            => .zst,
            .alias => boxyLowerInvariant("alias representation was not redirected before const plan output"),
            .list => .{ .list = try self.constPlanForChild(rep_id, .list_elem) },
            .box => .{ .box = try self.constPlanForChild(rep_id, .box_payload) },
            .record,
            .record_unbound,
            => try self.structConstPlan(rep, .record_field, .record),
            .tuple => try self.structConstPlan(rep, .tuple_elem, .tuple),
            .tag_union => try self.tagUnionConstPlan(rep),
            .nominal => |kind| switch (kind) {
                .transparent => .{ .named = .{
                    .named_type = .{
                        .module = moduleDigestFromId(self.modules.root.module.key),
                        .ty = rep.source_type,
                    },
                    .backing = try self.constPlanForChild(rep_id, .nominal_backing),
                } },
                .opaque_nominal => boxyLowerInvariant("opaque nominal reached const plan output before opaque static-data support"),
                .builtin_other => boxyLowerInvariant("builtin nominal representation was not redirected before const plan output"),
            },
        };
    }

    fn constPlanForChild(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) Allocator.Error!LirProgram.ConstPlanId {
        return try self.constPlanForRep(self.requiredSingleChild(rep_id, role).rep);
    }

    const StructPlanKind = enum {
        tuple,
        record,
    };

    fn structConstPlan(
        self: *ConstPlanBuilder,
        rep: Plan.TypeRepresentation,
        comptime role_tag: std.meta.Tag(Plan.ChildRole),
        comptime kind: StructPlanKind,
    ) Allocator.Error!LirProgram.ConstPlan {
        var count: usize = 0;
        for (self.plan.childSlice(rep.children)) |child| {
            switch (child.role) {
                role_tag => count += 1,
                else => {},
            }
        }

        const plans = try self.allocator.alloc(LirProgram.ConstPlanId, count);
        errdefer self.allocator.free(plans);
        var cursor: usize = 0;
        for (self.plan.childSlice(rep.children)) |child| {
            switch (child.role) {
                role_tag => {
                    plans[cursor] = try self.constPlanForRep(child.rep);
                    cursor += 1;
                },
                else => {},
            }
        }

        return switch (kind) {
            .tuple => .{ .tuple = plans },
            .record => .{ .record = plans },
        };
    }

    fn tagUnionConstPlan(self: *ConstPlanBuilder, rep: Plan.TypeRepresentation) Allocator.Error!LirProgram.ConstPlan {
        const tag_variants = self.plan.tagVariantSlice(rep.tag_variants);

        const variants = try self.allocator.alloc(LirProgram.ConstTagVariant, tag_variants.len);
        var initialized: usize = 0;
        errdefer {
            for (variants[0..initialized]) |variant| {
                self.allocator.free(variant.name);
                self.allocator.free(variant.payloads);
            }
            self.allocator.free(variants);
        }

        for (tag_variants, variants, 0..) |tag_variant, *variant, discriminant| {
            variant.* = try self.buildTagVariant(tag_variant, discriminant);
            initialized += 1;
        }

        return .{ .tag_union = variants };
    }

    fn buildTagVariant(
        self: *ConstPlanBuilder,
        variant: Plan.TagVariant,
        discriminant: usize,
    ) Allocator.Error!LirProgram.ConstTagVariant {
        const root_names = &self.modules.root.module.canonical_names;
        const name = try self.allocator.dupe(u8, root_names.tagLabelText(variant.name));
        errdefer self.allocator.free(name);

        const payload_children = self.plan.childSlice(variant.payloads);
        const payloads = try self.allocator.alloc(LirProgram.ConstPlanId, payload_children.len);
        errdefer self.allocator.free(payloads);
        for (payload_children, payloads, 0..) |child, *payload_plan, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != variant.name or payload.index != index) {
                        boxyLowerInvariant("tag variant payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag variant payload span included a non-payload child"),
            }
            payload_plan.* = try self.constPlanForRep(child.rep);
        }

        return .{
            .name = name,
            .checked_name = variant.name,
            .discriminant = @intCast(discriminant),
            .payloads = payloads,
        };
    }

    fn requiredSingleChild(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) Plan.RepChild {
        var found: ?Plan.RepChild = null;
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        for (self.plan.childSlice(rep.children)) |child| {
            if (sameChildRole(child.role, role)) {
                if (found != null) boxyLowerInvariant("representation had duplicate required child role");
                found = child;
            }
        }
        return found orelse boxyLowerInvariant("representation was missing required child role");
    }
};

fn sameChildRole(a: Plan.ChildRole, b: Plan.ChildRole) bool {
    return switch (a) {
        .alias_backing => b == .alias_backing,
        .nominal_backing => b == .nominal_backing,
        .record_ext => b == .record_ext,
        .tag_ext => b == .tag_ext,
        .list_elem => b == .list_elem,
        .box_payload => b == .box_payload,
        else => false,
    };
}

fn moduleDigestFromId(key: checked.ModuleId) names.CheckedModuleDigest {
    return .{ .bytes = key.bytes };
}

fn boxyLowerInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("boxy lower invariant violated: {s}", .{message});
    }
    unreachable;
}

test "boxy lowerer returns an empty LIR program for an empty plan" {
    const gpa = std.testing.allocator;

    var plan = Plan.ProgramPlan.init(gpa);
    defer plan.deinit();

    var out = try run(gpa, .{ .root = undefined }, .{}, &plan, .{});
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 0), out.lir_result.store.proc_specs.items.len);
    try std.testing.expectEqual(@as(usize, 0), out.lir_result.root_procs.items.len);
}

test "boxy lowerer emits requested layout metadata for layout-only plans" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);

    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(0), .key = typeKey(1) });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });

    var plan = try Plan.analyzeProgram(gpa, .{
        .checked_types = artifact.checked_types.view(),
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(0))},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{ .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(0))} },
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.requested_layouts.items.len);
    const requested = out.lir_result.requested_layouts.items[0];
    try std.testing.expectEqual(typeKey(1), requested.ty);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), requested.checked_type);
    try std.testing.expectEqual(.u64, requested.layout_idx);
    try std.testing.expectEqual(LirProgram.ConstPlan.scalar, out.lir_result.const_plans.items[@intFromEnum(requested.plan)]);
    try std.testing.expectEqual(@as(usize, 0), out.lir_result.root_procs.items.len);
}

test "boxy lowerer emits const plans for zero-payload tag variants" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);

    const tag_a = try artifact.canonical_names.internTagLabel("A");
    const tag_b = try artifact.canonical_names.internTagLabel("B");

    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(0), .key = typeKey(0) });
    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(1), .key = typeKey(1) });
    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(2), .key = typeKey(2) });
    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_a, .args_start = 0, .args_len = 0 });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_b, .args_start = 0, .args_len = 1 });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_tag_union);
    try artifact.checked_types.payloads.append(gpa, .{
        .tag_union = .{ .tags = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(1) },
    });

    var plan = try Plan.analyzeProgram(gpa, .{
        .checked_types = artifact.checked_types.view(),
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(2))},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{ .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(2))} },
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.requested_layouts.items.len);
    const requested = out.lir_result.requested_layouts.items[0];
    const const_plan = out.lir_result.const_plans.items[@intFromEnum(requested.plan)];
    switch (const_plan) {
        .tag_union => |variants| {
            try std.testing.expectEqual(@as(usize, 2), variants.len);
            try std.testing.expectEqualStrings("A", variants[0].name);
            try std.testing.expectEqual(tag_a, variants[0].checked_name);
            try std.testing.expectEqual(@as(u16, 0), variants[0].discriminant);
            try std.testing.expectEqual(@as(usize, 0), variants[0].payloads.len);
            try std.testing.expectEqualStrings("B", variants[1].name);
            try std.testing.expectEqual(tag_b, variants[1].checked_name);
            try std.testing.expectEqual(@as(u16, 1), variants[1].discriminant);
            try std.testing.expectEqual(@as(usize, 1), variants[1].payloads.len);
        },
        else => return error.TestUnexpectedResult,
    }
}

fn minimalCheckedArtifact(allocator: Allocator) checked.CheckedModuleArtifact {
    return .{
        .key = moduleKey(1),
        .canonical_names = names.CanonicalNameStore.init(allocator),
        .module_identity = undefined,
        .checking_context_identity = undefined,
        .module_env = undefined,
        .exports = undefined,
        .provides_requires = undefined,
        .method_registry = undefined,
        .static_dispatch_plans = undefined,
        .resolved_value_refs = undefined,
        .checked_procedure_templates = undefined,
        .top_level_procedure_bindings = undefined,
        .root_requests = undefined,
        .hosted_procs = undefined,
        .platform_required_declarations = undefined,
        .platform_required_bindings = undefined,
        .interface_capabilities = .{},
        .compile_time_roots = undefined,
        .top_level_values = undefined,
        .hoisted_constants = undefined,
        .const_templates = undefined,
        .const_store = undefined,
    };
}

fn builtinNominal(
    builtin: checked.CheckedBuiltinNominal,
    backing: checked.CheckedTypeId,
    args: checked.CheckedTypeRange,
) checked.StoredNominal {
    return .{
        .name = @enumFromInt(0),
        .origin_module = @enumFromInt(0),
        .builtin = builtin,
        .is_opaque = false,
        .backing = backing,
        .representation = .{ .builtin = builtin },
        .args = args,
    };
}

fn moduleKey(byte: u8) checked.ModuleId {
    var key = checked.ModuleId{};
    key.bytes[0] = byte;
    return key;
}

fn typeKey(byte: u8) names.TypeDigest {
    var key = names.TypeDigest{};
    key.bytes[0] = byte;
    return key;
}
