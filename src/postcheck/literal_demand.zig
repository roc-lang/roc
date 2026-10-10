//! Which runtime specializations compile-time evaluation must still lower.
//!
//! Checking already published a compile-time root for every literal
//! conversion whose dispatch is closed. A specialization registers a further
//! literal root only where a conversion depends on its instance or where the
//! checked root is one no evaluation requests (design.md "Demand-Driven
//! Compile-Time Specialization"). Compile-time evaluation lowers a runtime
//! procedure body only when that body, or one it reaches, can do so; every
//! other runtime body is left unlowered.
//!
//! The answer is computed from explicit checked data, per checked module:
//!
//! - A module is a *source* when one of its checked expressions is a custom
//!   interpolation, or a literal whose conversion root no evaluation of this
//!   compilation requests and whose payload is not stored, or when one of its
//!   checked types names a custom literal type: a nominal with its own
//!   `from_numeral` or `from_quote` method.
//! - A module *reaches* a literal root when it is a source, or calls into a
//!   module that reaches one: through an import, or through a platform
//!   requirement the app fills.
//!
//! A specialization of a procedure whose module reaches no literal root, at
//! types that name no custom literal type and no nominal of a reaching
//! module, and with dispatch evidence that selects no procedure of a reaching
//! module, reaches no literal root either: every body it would lower belongs
//! to a non-reaching module and instantiates dependent conversions only at
//! types without a custom conversion, which convert their literals directly.

const std = @import("std");
const check = @import("check");

const Common = @import("common.zig");

const Allocator = std.mem.Allocator;
/// The checked modules a lowering sees.
pub const CheckedModules = Common.CheckedModules;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const canonical = check.CanonicalNames;

/// Builds without libc (the playground) never read the environment and
/// compile no trace output.
const trace_available = @import("builtin").link_libc;

/// `ROC_CTFE_DEMAND_TRACE` is set: print every demand decision, from the
/// module analysis to each discovery root and each parked, unparked, and
/// upgraded specialization.
pub fn traceEnabled() bool {
    return trace_available and std.c.getenv("ROC_CTFE_DEMAND_TRACE") != null;
}

/// A compile-time root this compilation evaluates.
pub const DeclaredRoot = struct {
    module: checked.ModuleId,
    root: checked.ComptimeRootId,
};

/// The nominal identity a checked type and a Monotype type share across
/// name stores: the declaring module's content identity, the declared name,
/// and its declaring statement.
const NominalKey = struct {
    module: [32]u8,
    name: []const u8,
    source_decl: ?u32,
};

const NominalKeyContext = struct {
    pub fn hash(_: NominalKeyContext, key: NominalKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(&key.module);
        hasher.update(key.name);
        if (key.source_decl) |decl| hasher.update(std.mem.asBytes(&decl));
        return hasher.final();
    }

    pub fn eql(_: NominalKeyContext, a: NominalKey, b: NominalKey) bool {
        return std.mem.eql(u8, &a.module, &b.module) and
            std.mem.eql(u8, a.name, b.name) and
            a.source_decl == b.source_decl;
    }
};

const NominalKeySet = std.HashMapUnmanaged(NominalKey, void, NominalKeyContext, std.hash_map.default_max_load_percentage);

/// Explicit work counts for the analysis and for the decisions made with it.
pub const Counters = struct {
    modules: u32 = 0,
    source_modules: u32 = 0,
    reaching_modules: u32 = 0,
    custom_literal_types: u32 = 0,
};

/// The modules whose procedures can lead a specialization to register a
/// literal root, and the custom literal types that make a dependent
/// conversion register one.
///
/// Borrows the name stores of the checked modules it was computed from; it
/// must not outlive them.
pub const LiteralDemand = struct {
    allocator: Allocator,
    /// Reaching modules, by checked module key.
    reaching_keys: std.AutoHashMapUnmanaged(checked.ModuleId, void) = .empty,
    /// Reaching modules, by declaring-module content identity, for types.
    reaching_identities: std.AutoHashMapUnmanaged([32]u8, void) = .empty,
    custom_types: NominalKeySet = .empty,
    counters: Counters = .{},

    pub fn deinit(self: *LiteralDemand) void {
        self.reaching_keys.deinit(self.allocator);
        self.reaching_identities.deinit(self.allocator);
        self.custom_types.deinit(self.allocator);
        self.* = undefined;
    }

    /// Whether a procedure of this checked module can lead a specialization
    /// to register a literal root.
    pub fn moduleReaches(self: *const LiteralDemand, module: checked.ModuleId) bool {
        return self.reaching_keys.contains(module);
    }

    /// Whether a nominal, named by its declaring module's content identity,
    /// its declared name, and its declaring statement, makes a specialization
    /// at it reach a literal root: it converts literals with its own method,
    /// or its methods belong to a reaching module.
    pub fn nominalReaches(self: *const LiteralDemand, module: *const [32]u8, name: []const u8, source_decl: ?u32) bool {
        if (self.reaching_identities.contains(module.*)) return true;
        return self.custom_types.containsContext(.{ .module = module.*, .name = name, .source_decl = source_decl }, .{});
    }

    /// Whether a checked type names a nominal that `nominalReaches`.
    pub fn checkedTypeReaches(
        self: *const LiteralDemand,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        types: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) Allocator.Error!bool {
        var pending = std.ArrayList(checked.CheckedTypeId).empty;
        defer pending.deinit(allocator);
        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(allocator);
        defer seen.deinit();
        try pending.append(allocator, root);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union => {},
                .alias => |alias| {
                    try pending.append(allocator, alias.backing);
                    try pending.appendSlice(allocator, alias.args);
                },
                .record => |record| {
                    for (record.fields) |field| try pending.append(allocator, field.ty);
                    try pending.append(allocator, record.ext);
                },
                .tuple => |elems| try pending.appendSlice(allocator, elems),
                .nominal => |nominal| {
                    if (self.nominalReaches(names.moduleIdentityBytes(nominal.origin_module), names.typeNameText(nominal.name), nominal.source_decl)) return true;
                    try pending.appendSlice(allocator, nominal.args);
                    try pending.appendSlice(allocator, nominal.padding_field_types);
                },
                .function => |function| {
                    try pending.appendSlice(allocator, function.args);
                    try pending.append(allocator, function.ret);
                },
                .tag_union => |tag_union| {
                    for (tag_union.tags) |tag| try pending.appendSlice(allocator, tag.argsSlice(types));
                    try pending.append(allocator, tag_union.ext);
                },
            }
        }
        return false;
    }

    /// Whether checked dispatch evidence selects a procedure of a reaching
    /// module, or selects its target in a way only a specialization decides.
    pub fn checkedEvidenceReaches(
        self: *const LiteralDemand,
        allocator: Allocator,
        plans: *const static_dispatch.StaticDispatchPlanTable,
        evidence: []const static_dispatch.CheckedEvidence,
    ) Allocator.Error!bool {
        var pending = std.ArrayList(static_dispatch.CheckedEvidence).empty;
        defer pending.deinit(allocator);
        try pending.appendSlice(allocator, evidence);
        while (pending.pop()) |entry| {
            try pending.appendSlice(allocator, plans.evidence_refs[entry.callable_contracts.start..][0..entry.callable_contracts.len]);
            switch (entry.resolution) {
                .direct => |node_id| {
                    const node = plans.evidenceNode(node_id);
                    switch (node.target.kind) {
                        .procedure => |procedure| if (self.moduleReaches(.{ .bytes = procedure.template.artifact.bytes })) return true,
                        .local_proc => return true,
                        .structural => {},
                    }
                    switch (node.nested) {
                        .resolved => try pending.appendSlice(allocator, plans.nestedEvidence(node)),
                        .from_callable => return true,
                    }
                },
                .structural, .checked_error, .unreachable_value => {},
                .constraint, .from_callable, .from_scheme => return true,
            }
        }
        return false;
    }
};

/// One checked module as the analysis reads it.
const ModuleInput = struct {
    view: checked.ImportedModuleView,
    reaches: bool = false,
};

/// Compute which modules reach a literal root. `declared` names every
/// compile-time root this compilation evaluates.
pub fn compute(
    allocator: Allocator,
    modules: Common.CheckedModules,
    declared: *const std.AutoHashMapUnmanaged(DeclaredRoot, void),
) Allocator.Error!LiteralDemand {
    var inputs = std.ArrayList(ModuleInput).empty;
    defer inputs.deinit(allocator);
    var positions = std.AutoHashMap(checked.ModuleId, usize).init(allocator);
    defer positions.deinit();
    try appendModule(allocator, &inputs, &positions, checked.importedView(modules.root.module));
    for (modules.root.relation_modules) |view| try appendModule(allocator, &inputs, &positions, view);
    for (modules.imports) |view| try appendModule(allocator, &inputs, &positions, view);

    var demand = LiteralDemand{ .allocator = allocator };
    errdefer demand.deinit();
    demand.counters.modules = @intCast(inputs.items.len);

    // Every nominal that converts literals with a method of its own. Builtin
    // owners convert their literals directly at every primitive.
    for (inputs.items) |input| {
        const names = input.view.canonical_names;
        for (input.view.method_registry.entries) |entry| {
            if (entry.target == null) continue;
            const nominal = switch (entry.key.owner) {
                .nominal => |nominal| nominal,
                .builtin => continue,
            };
            const method = names.methodNameText(entry.key.method);
            if (!std.mem.eql(u8, method, "from_numeral") and !std.mem.eql(u8, method, "from_quote")) continue;
            try demand.custom_types.putContext(allocator, .{
                .module = names.moduleIdentityBytes(nominal.module).*,
                .name = names.typeNameText(nominal.type_name),
                .source_decl = nominal.source_decl,
            }, {}, .{});
        }
    }
    demand.counters.custom_literal_types = demand.custom_types.count();

    // Sources.
    for (inputs.items) |*input| {
        if (try isSource(&demand, input.view, declared)) {
            input.reaches = true;
            demand.counters.source_modules += 1;
            if (traceEnabled()) std.debug.print("ctfe-demand source {s}\n", .{input.view.canonical_names.moduleNameText(input.view.module_identity.module_name)});
        }
    }

    // Reaching modules: a module reaches a literal root when a module it
    // calls into does. Callers are found through explicit import and
    // platform-requirement edges.
    var callers = std.ArrayList(std.ArrayList(usize)).empty;
    defer {
        for (callers.items) |*list| list.deinit(allocator);
        callers.deinit(allocator);
    }
    try callers.appendNTimes(allocator, .empty, inputs.items.len);
    for (inputs.items, 0..) |input, caller| {
        for (input.view.direct_import_artifact_keys) |key| {
            const callee = positions.get(key) orelse continue;
            try callers.items[callee].append(allocator, caller);
        }
        for (input.view.platform_required_bindings.bindings) |binding| {
            const callee = positions.get(.{ .bytes = binding.app_value.artifact.bytes }) orelse continue;
            try callers.items[callee].append(allocator, caller);
        }
        // A platform's requirements are filled by the app, whichever
        // relation names the filling.
        if (input.view.platform_required_declarations.declarations.len != 0) {
            for (inputs.items, 0..) |candidate, callee| {
                switch (candidate.view.module_identity.kind) {
                    .app, .default_app => try callers.items[callee].append(allocator, caller),
                    .type_module, .package, .platform, .hosted, .module, .malformed => {},
                }
            }
        }
    }
    var worklist = std.ArrayList(usize).empty;
    defer worklist.deinit(allocator);
    for (inputs.items, 0..) |input, index| {
        if (input.reaches) try worklist.append(allocator, index);
    }
    while (worklist.pop()) |callee| {
        for (callers.items[callee].items) |caller| {
            if (inputs.items[caller].reaches) continue;
            inputs.items[caller].reaches = true;
            try worklist.append(allocator, caller);
        }
    }
    if (traceEnabled()) {
        for (inputs.items) |input| {
            const names = input.view.canonical_names;
            std.debug.print("ctfe-demand module {s} {s}\n", .{
                names.moduleNameText(input.view.module_identity.module_name),
                if (input.reaches) "reaches" else "-",
            });
        }
        var types = demand.custom_types.keyIterator();
        while (types.next()) |key| std.debug.print("ctfe-demand custom-literal-type {s}\n", .{key.name});
    }
    for (inputs.items) |input| {
        if (!input.reaches) continue;
        try demand.reaching_keys.put(allocator, input.view.key, {});
        try demand.reaching_identities.put(allocator, input.view.module_identity.stable_hash, {});
    }
    demand.counters.reaching_modules = demand.reaching_keys.count();
    return demand;
}

fn appendModule(
    allocator: Allocator,
    inputs: *std.ArrayList(ModuleInput),
    positions: *std.AutoHashMap(checked.ModuleId, usize),
    view: checked.ImportedModuleView,
) Allocator.Error!void {
    const entry = try positions.getOrPut(view.key);
    if (entry.found_existing) return;
    entry.value_ptr.* = inputs.items.len;
    try inputs.append(allocator, .{ .view = view });
}

/// Whether a module's own checked data can make a specialization register a
/// literal root.
fn isSource(
    demand: *const LiteralDemand,
    view: checked.ImportedModuleView,
    declared: *const std.AutoHashMapUnmanaged(DeclaredRoot, void),
) Allocator.Error!bool {
    const plans = view.static_dispatch_plans;
    for (view.checked_bodies.stored_exprs) |expr| {
        const plan_id, const conversion_root = switch (expr.data) {
            .numeral => |numeral| .{ numeral.plan, numeral.conversion_root },
            .str_from_quote => |quote| .{ quote.plan, quote.conversion_root },
            .interpolation => |interpolation| .{ interpolation.plan, null },
            else => continue,
        };
        const plan = plan_id orelse continue;
        if (plans.plans[@backingInt(plan)].resolution == .checked_error) continue;
        if (expr.data == .interpolation) return true;
        // A conversion with no root of its own depends on its instance; the
        // instance's types decide it.
        const root_id = conversion_root orelse continue;
        const root = view.compile_time_roots.root(root_id);
        switch (root.payload) {
            .pending => if (!declared.contains(.{ .module = view.key, .root = root_id })) return true,
            .const_node, .fn_value, .discarded, .expect, .runtime => {},
        }
    }
    const names = view.canonical_names;
    for (view.checked_types.stored_payloads) |payload| {
        const nominal = switch (payload) {
            .nominal => |nominal| nominal,
            else => continue,
        };
        if (demand.custom_types.containsContext(.{
            .module = names.moduleIdentityBytes(nominal.origin_module).*,
            .name = names.typeNameText(nominal.name),
            .source_decl = nominal.source_decl,
        }, .{})) return true;
    }
    return false;
}

/// Select the runtime roots compile-time evaluation must specialize to find
/// the literal roots they reach. A root whose procedure belongs to a module
/// that reaches no literal root, whose checked type names no reaching
/// nominal, and whose root evidence selects no reaching procedure, reaches no
/// literal root, and is left to the runtime consumer.
pub fn selectDiscoveryRoots(
    allocator: Allocator,
    demand: *const LiteralDemand,
    modules: Common.CheckedModules,
    requests: []const checked.RootRequest,
    source_modules: []const checked.ModuleId,
    selected: *std.ArrayList(usize),
) Allocator.Error!void {
    for (requests, 0..) |request, index| {
        const owner = if (source_modules.len == 0) modules.root.module.key else source_modules[index];
        const view = findView(modules, owner) orelse Common.invariant("discovery root named a checked module outside its lowering set");
        const reaches = try rootReaches(allocator, demand, modules, view, request);
        if (traceEnabled()) std.debug.print("ctfe-demand root {d} {s} {s}\n", .{
            index,
            view.canonical_names.moduleNameText(view.module_identity.module_name),
            if (reaches) "discovery" else "skipped",
        });
        if (reaches) try selected.append(allocator, index);
    }
}

fn rootReaches(
    allocator: Allocator,
    demand: *const LiteralDemand,
    modules: Common.CheckedModules,
    view: checked.ImportedModuleView,
    request: checked.RootRequest,
) Allocator.Error!bool {
    const template = request.procedure_template orelse return true;
    if (demand.moduleReaches(.{ .bytes = template.artifact.bytes })) return true;
    if (demand.moduleReaches(view.key)) return true;
    if (try demand.checkedTypeReaches(allocator, view.canonical_names, view.checked_types, request.checked_type)) return true;
    if (request.root_evidence) |evidence| {
        const evidence_view = findView(modules, evidence.checked_module) orelse
            Common.invariant("discovery root evidence named a checked module outside its lowering set");
        const plans = evidence_view.static_dispatch_plans;
        if (try demand.checkedEvidenceReaches(allocator, plans, plans.evidence_refs[evidence.span.start..][0..evidence.span.len])) return true;
    }
    return false;
}

fn findView(modules: Common.CheckedModules, key: checked.ModuleId) ?checked.ImportedModuleView {
    if (std.meta.eql(modules.root.module.key, key)) return checked.importedView(modules.root.module);
    for (modules.root.relation_modules) |view| if (std.meta.eql(view.key, key)) return view;
    for (modules.imports) |view| if (std.meta.eql(view.key, key)) return view;
    return null;
}
