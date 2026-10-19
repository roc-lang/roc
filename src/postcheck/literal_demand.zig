//! Which runtime specializations compile-time evaluation must still lower.
//!
//! Checking already recorded a compile-time root for every literal
//! conversion whose dispatch is closed. A specialization registers a further
//! literal root only where a conversion depends on its instance or where the
//! checked root is one no evaluation requests (design.md "Demand-Driven
//! Compile-Time Specialization"). Compile-time evaluation lowers a runtime
//! procedure body only when that body, or one it reaches, can do so; every
//! other runtime body is left unlowered.
//!
//! The answer is computed from explicit checked data, per procedure. The
//! analysis is a graph whose nodes are checked procedure templates, the
//! checked types their bodies use, nominal types, and the builtin types, and
//! whose edges are the explicit references between them:
//!
//! - a template reaches the templates its resolved value references and its
//!   direct dispatch plans (with their nested evidence) select, and the
//!   types of its signature, references, and plans;
//! - a type reaches its component types and the nominals it names;
//! - a nominal, and the builtin types, reach the procedures of their methods
//!   whose names dispatch can select by type (a where-clause requirement, an
//!   evidence-dependent or structural plan, a generated codec, an iterator
//!   loop, or inspection); every other method is reached only through a plan
//!   that selects it.
//!
//! Each node carries flags for what it can contribute to a literal root:
//!
//! - *strict*: a custom interpolation, or a literal whose conversion root no
//!   evaluation of this compilation requests and whose payload is not stored,
//!   each of which registers a literal root whatever the instance; or a body
//!   no checked data bounds, such as a read of a constant whose computation
//!   lowers in place;
//! - *dependent numeral* (*quote*): a numeral (quote) whose conversion depends
//!   on its instance, which registers a literal root only at a custom numeral
//!   (quote) type;
//! - *custom numeral* (*quote*): a nominal with a `from_numeral`
//!   (`from_quote`) method of its own.
//!
//! A node's reach flags are its own flags and those of every node it reaches.
//! A specialization can register a literal root only when the reach flags of
//! its template, the types of its request, and its dispatch evidence include
//! *strict*, or a dependent kind together with the custom type of that kind:
//! every body it would lower is reached from its template, every type a
//! dependent conversion is instantiated at either names a custom type the
//! request or a reached body names, and every method dispatch can select is
//! reached through the types it dispatches on.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const check = @import("check");

const Common = @import("common.zig");

const Allocator = std.mem.Allocator;
/// The checked modules a lowering sees.
pub const CheckedModules = Common.CheckedModules;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const checked_names = check.CanonicalNames;

/// Builds without libc (the playground) and compilers built without
/// developer diagnostics never read the environment and compile no trace
/// output.
const trace_available = @import("builtin").link_libc and build_options.dev_diagnostics;

/// `ROC_CTFE_DEMAND_TRACE` is set: print every demand decision, from the
/// analysis to each discovery root and each parked, unparked, and upgraded
/// specialization. Inline, so a build without libc sees a compile-time false
/// and compiles none of the trace output.
pub inline fn traceEnabled() bool {
    return trace_available and std.c.getenv("ROC_CTFE_DEMAND_TRACE") != null;
}

/// A compile-time root this compilation evaluates.
pub const DeclaredRoot = struct {
    module: checked.ModuleId,
    root: checked.ComptimeRootId,
};

/// What a node can contribute to a literal root.
pub const Flags = packed struct(u8) {
    strict: bool = false,
    dependent_numeral: bool = false,
    dependent_quote: bool = false,
    custom_numeral: bool = false,
    custom_quote: bool = false,
    _padding: u3 = 0,

    pub fn merge(self: Flags, other: Flags) Flags {
        return @bitCast(@as(u8, @bitCast(self)) | @as(u8, @bitCast(other)));
    }

    fn eql(self: Flags, other: Flags) bool {
        return @as(u8, @bitCast(self)) == @as(u8, @bitCast(other));
    }

    /// Whether a specialization with these reach flags can register a
    /// literal root.
    pub fn reachesLiteralRoot(self: Flags) bool {
        return self.strict or
            (self.dependent_numeral and self.custom_numeral) or
            (self.dependent_quote and self.custom_quote);
    }

    const all: Flags = .{ .strict = true };
};

/// The nominal identity a checked type and a Monotype type share across
/// name stores: the declaring module's content identity and its declaring
/// statement (see `canonical_names.NominalTypeKey`).
const NominalKey = struct {
    module: [32]u8,
    source_decl: u32,
};

const TemplateKey = struct {
    module: [32]u8,
    template: u32,
};

const TypeKey = struct {
    module: [32]u8,
    ty: u32,
};

const NodeId = u32;

/// Explicit work counts for the analysis.
pub const Counters = struct {
    modules: u32 = 0,
    templates: u32 = 0,
    source_templates: u32 = 0,
    reaching_templates: u32 = 0,
    custom_literal_types: u32 = 0,
};

/// The reach flags of every checked procedure template, nominal, and the
/// builtin types.
///
/// Borrows the name stores of the checked modules it was computed from; it
/// must not outlive them.
pub const LiteralDemand = struct {
    allocator: Allocator,
    templates: std.AutoHashMapUnmanaged(TemplateKey, NodeId) = .empty,
    nominals: std.AutoHashMapUnmanaged(NominalKey, NodeId) = .empty,
    builtin: NodeId = 0,
    /// Reach flags, by node.
    flags: std.ArrayListUnmanaged(Flags) = .empty,
    counters: Counters = .{},

    pub fn deinit(self: *LiteralDemand) void {
        self.templates.deinit(self.allocator);
        self.nominals.deinit(self.allocator);
        self.flags.deinit(self.allocator);
        self.* = undefined;
    }

    /// The reach flags of a checked procedure template. A template the
    /// analysis never saw has no checked data to bound it.
    pub fn templateFlags(self: *const LiteralDemand, template: checked_names.ProcedureTemplateRef) Flags {
        const node = self.templates.get(.{ .module = template.artifact.bytes, .template = @backingInt(template.template) }) orelse return Flags.all;
        return self.flags.items[node];
    }

    /// The reach flags of a nominal, named by its declaring module's content
    /// identity and its declaring statement. A nominal with no methods and no
    /// custom conversion contributes nothing.
    pub fn nominalFlags(self: *const LiteralDemand, module: *const [32]u8, source_decl: u32) Flags {
        const node = self.nominals.get(.{ .module = module.*, .source_decl = source_decl }) orelse return .{};
        return self.flags.items[node];
    }

    /// The reach flags of the builtin types' methods.
    pub fn builtinFlags(self: *const LiteralDemand) Flags {
        return self.flags.items[self.builtin];
    }

    /// The reach flags of the nominals and builtin types a checked type
    /// names.
    pub fn checkedTypeFlags(
        self: *const LiteralDemand,
        allocator: Allocator,
        names: *const checked_names.CanonicalNameStore,
        types: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) Allocator.Error!Flags {
        var result: Flags = .{};
        var pending = std.ArrayList(checked.CheckedTypeId).empty;
        defer pending.deinit(allocator);
        var seen = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
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
                    result = result.merge(self.nominalFlags(names.moduleIdentityBytes(nominal.origin_module), nominal.source_decl));
                    if (nominal.builtin != null) result = result.merge(self.builtinFlags());
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
        return result;
    }

    /// The reach flags of the procedures checked dispatch evidence selects.
    /// Evidence that selects a local procedure, or that only a
    /// specialization decides, is not bounded by checked data.
    pub fn checkedEvidenceFlags(
        self: *const LiteralDemand,
        allocator: Allocator,
        plans: *const static_dispatch.StaticDispatchPlanTable,
        evidence: []const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Flags {
        var result: Flags = .{};
        var pending = std.ArrayList(static_dispatch.CheckedEvidence).empty;
        defer pending.deinit(allocator);
        try pending.appendSlice(allocator, evidence);
        while (pending.pop()) |entry| {
            try pending.appendSlice(allocator, plans.evidence_refs[entry.callable_contracts.start..][0..entry.callable_contracts.len]);
            switch (entry.resolution) {
                .direct => |node_id| {
                    const node = plans.evidenceNode(node_id);
                    switch (node.target.kind) {
                        .procedure => |procedure| result = result.merge(self.templateFlags(procedure.template)),
                        .local_proc => return Flags.all,
                        .structural => {},
                    }
                    switch (node.nested) {
                        .resolved => try pending.appendSlice(allocator, plans.nestedEvidence(node)),
                        .from_callable => return Flags.all,
                    }
                },
                .structural, .checked_error, .unreachable_value => {},
                .constraint, .from_callable, .from_scheme => return Flags.all,
            }
        }
        return result;
    }
};

/// The graph under construction.
const Builder = struct {
    allocator: Allocator,
    demand: *LiteralDemand,
    declared: *const std.AutoHashMapUnmanaged(DeclaredRoot, void),
    views: std.AutoHashMapUnmanaged([32]u8, checked.ImportedModuleView) = .empty,
    types: std.AutoHashMapUnmanaged(TypeKey, NodeId) = .empty,
    /// Own flags, by node.
    own: std.ArrayListUnmanaged(Flags) = .empty,
    /// Edges, from a node to a node it reaches.
    edges: std.ArrayListUnmanaged([2]NodeId) = .empty,
    /// The method names a dispatch can select without an explicit direct
    /// call naming the procedure: names a where-clause requirement, an
    /// evidence-dependent or structural plan, a generated codec, or an
    /// iterator loop dispatches by, and `to_inspect`, which inspection
    /// selects by type.
    implicit_methods: std.StringHashMapUnmanaged(void) = .empty,
    /// Type nodes whose components are still to be added.
    pending_types: std.ArrayListUnmanaged(struct { node: NodeId, view: checked.ImportedModuleView, ty: checked.CheckedTypeId }) = .empty,

    fn deinit(self: *Builder) void {
        self.views.deinit(self.allocator);
        self.implicit_methods.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.own.deinit(self.allocator);
        self.edges.deinit(self.allocator);
        self.pending_types.deinit(self.allocator);
    }

    fn addNode(self: *Builder, own: Flags) Allocator.Error!NodeId {
        const node: NodeId = @intCast(self.own.items.len);
        try self.own.append(self.allocator, own);
        return node;
    }

    fn addEdge(self: *Builder, from: NodeId, to: NodeId) Allocator.Error!void {
        if (from == to) return;
        try self.edges.append(self.allocator, .{ from, to });
    }

    fn markOwn(self: *Builder, node: NodeId, flags: Flags) void {
        self.own.items[node] = self.own.items[node].merge(flags);
    }

    fn markUnbounded(self: *Builder, node: NodeId, comptime reason: []const u8) void {
        if (traceEnabled()) std.debug.print("ctfe-demand unbounded {s} node={d}\n", .{ reason, node });
        self.markOwn(node, Flags.all);
    }

    /// The node of a template, or a node that reaches everything when no
    /// module in view declares it.
    fn templateNode(self: *Builder, template: checked_names.ProcedureTemplateRef) Allocator.Error!NodeId {
        const key: TemplateKey = .{ .module = template.artifact.bytes, .template = @backingInt(template.template) };
        if (self.demand.templates.get(key)) |node| return node;
        const node = try self.addNode(.{});
        self.markUnbounded(node, "template outside the lowering");
        try self.demand.templates.put(self.allocator, key, node);
        return node;
    }

    fn nominalNode(self: *Builder, key: NominalKey) Allocator.Error!NodeId {
        const entry = try self.demand.nominals.getOrPut(self.allocator, key);
        if (!entry.found_existing) entry.value_ptr.* = try self.addNode(.{});
        return entry.value_ptr.*;
    }

    fn typeNode(self: *Builder, view: checked.ImportedModuleView, ty: checked.CheckedTypeId) Allocator.Error!NodeId {
        const entry = try self.types.getOrPut(self.allocator, .{ .module = view.key.bytes, .ty = @backingInt(ty) });
        if (entry.found_existing) return entry.value_ptr.*;
        const node = try self.addNode(.{});
        entry.value_ptr.* = node;
        try self.pending_types.append(self.allocator, .{ .node = node, .view = view, .ty = ty });
        return node;
    }

    fn addTypeEdge(self: *Builder, from: NodeId, view: checked.ImportedModuleView, ty: checked.CheckedTypeId) Allocator.Error!void {
        try self.addEdge(from, try self.typeNode(view, ty));
    }

    /// Add the components of every type node created so far, and of those
    /// they create.
    fn expandTypes(self: *Builder) Allocator.Error!void {
        while (self.pending_types.pop()) |item| {
            const view = item.view;
            const types = view.checked_types;
            switch (types.payload(item.ty)) {
                .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union => {},
                .alias => |alias| {
                    try self.addTypeEdge(item.node, view, alias.backing);
                    for (alias.args) |arg| try self.addTypeEdge(item.node, view, arg);
                },
                .record => |record| {
                    for (record.fields) |field| try self.addTypeEdge(item.node, view, field.ty);
                    try self.addTypeEdge(item.node, view, record.ext);
                },
                .tuple => |elems| for (elems) |elem| try self.addTypeEdge(item.node, view, elem),
                .nominal => |nominal| {
                    const names = view.canonical_names;
                    try self.addEdge(item.node, try self.nominalNode(.{
                        .module = names.moduleIdentityBytes(nominal.origin_module).*,
                        .source_decl = nominal.source_decl,
                    }));
                    if (nominal.builtin != null) try self.addEdge(item.node, self.demand.builtin);
                    for (nominal.args) |arg| try self.addTypeEdge(item.node, view, arg);
                    for (nominal.padding_field_types) |field| try self.addTypeEdge(item.node, view, field);
                },
                .function => |function| {
                    for (function.args) |arg| try self.addTypeEdge(item.node, view, arg);
                    try self.addTypeEdge(item.node, view, function.ret);
                },
                .tag_union => |tag_union| {
                    for (tag_union.tags) |tag| for (tag.argsSlice(types)) |arg| try self.addTypeEdge(item.node, view, arg);
                    try self.addTypeEdge(item.node, view, tag_union.ext);
                },
            }
        }
    }

    /// The template a procedure use reaches, or null when it reaches no
    /// Roc body (a hosted procedure or a rejected binding).
    fn procedureUseTarget(self: *Builder, use: checked.ProcedureUseTemplate) ?TemplateOrUnbounded {
        return switch (use.binding) {
            .top_level => |top_level| blk: {
                const view = self.views.get(top_level.artifact.bytes) orelse break :blk .unbounded;
                break :blk self.bindingBodyTarget(view, view.top_level_procedure_bindings.get(top_level.binding).body);
            },
            .imported => |imported| blk: {
                const view = self.views.get(imported.artifact.bytes) orelse break :blk .unbounded;
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def != imported.def or binding.binding.pattern != imported.pattern) continue;
                    break :blk switch (binding.body) {
                        .direct_template => |direct| self.bindingBodyTarget(view, .{ .direct_template = direct }),
                        .callable_eval_template => |id| self.bindingBodyTarget(view, .{ .callable_eval_template = id }),
                        .checked_error => |expr| self.bindingBodyTarget(view, .{ .checked_error = expr }),
                    };
                }
                break :blk .unbounded;
            },
            .hosted => null,
            .platform_required => |required| blk: {
                const view = self.views.get(required.artifact.bytes) orelse break :blk .unbounded;
                break :blk self.bindingBodyTarget(view, view.top_level_procedure_bindings.get(required.procedure_binding).body);
            },
        };
    }

    const TemplateOrUnbounded = union(enum) {
        template: checked_names.ProcedureTemplateRef,
        unbounded,
    };

    fn bindingBodyTarget(_: *Builder, view: checked.ImportedModuleView, body: checked.ProcedureBindingBody) ?TemplateOrUnbounded {
        return switch (body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| .{ .template = template },
                .lifted, .synthetic => .unbounded,
            },
            .callable_eval_template => |id| blk: {
                const raw = @backingInt(id);
                if (raw >= view.callable_eval_templates.templates.len) break :blk .unbounded;
                const wrapper = view.entry_wrappers.lookupByRoot(view.callable_eval_templates.templates[raw].root) orelse break :blk .unbounded;
                break :blk .{ .template = wrapper.template };
            },
            .checked_error => null,
        };
    }

    /// Whether reading a constant can lower its computation in place: its
    /// value is neither stored nor evaluated by this compilation.
    fn constReadLowersBody(self: *Builder, use: checked.ConstUseTemplate) bool {
        const const_ref = use.const_ref;
        const view = self.views.get(const_ref.artifact.bytes) orelse return true;
        switch (view.const_templates.get(const_ref).state) {
            .stored_const, .unimplemented => return false,
            .reserved, .eval_template => {},
        }
        const root_id = switch (const_ref.owner) {
            .top_level_binding => |top_level| view.compile_time_roots.lookupIdByPattern(top_level.pattern),
            .hoisted_expr => |hoisted| if (view.hoisted_constants.lookupByExpr(hoisted.expr)) |entry| entry.root else null,
        } orelse return true;
        return !self.declared.contains(.{ .module = view.key.digest(), .root = root_id });
    }

    fn addTemplateTarget(self: *Builder, from: NodeId, target: ?TemplateOrUnbounded) Allocator.Error!void {
        switch (target orelse return) {
            .template => |template| try self.addEdge(from, try self.templateNode(template)),
            .unbounded => self.markUnbounded(from, "procedure use"),
        }
    }

    /// Add one template's own flags and edges.
    fn addTemplate(self: *Builder, view: checked.ImportedModuleView, node: NodeId, template: checked.CheckedProcedureTemplate) Allocator.Error!void {
        if (template.target == .hosted) return;
        try self.addTypeEdge(node, view, template.checked_fn_root);

        const refs = view.resolved_value_refs;
        for (refs.template_refs[template.resolved_value_refs.start..][0..template.resolved_value_refs.len]) |ref_id| {
            const record = refs.records[@backingInt(ref_id)];
            try self.addTypeEdge(node, view, record.checked_ty);
            switch (record.ref) {
                .top_level_proc, .imported_proc, .promoted_top_level_proc => |use| try self.addTemplateTarget(node, self.procedureUseTarget(use)),
                .platform_required_proc => |required| try self.addTemplateTarget(node, self.procedureUseTarget(required.procedure)),
                .top_level_const, .imported_const => |use| if (self.constReadLowersBody(use)) self.markUnbounded(node, "const read"),
                .selected_hoisted_const => |selected| if (self.constReadLowersBody(selected.const_use)) self.markUnbounded(node, "hoisted const read"),
                .platform_required_const => |required| if (self.constReadLowersBody(required.const_use)) self.markUnbounded(node, "required const read"),
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                .local_proc,
                .hosted_proc,
                .platform_required_declaration,
                .platform_required_checked_error,
                => {},
            }
        }

        const plans = view.static_dispatch_plans;
        for (plans.template_refs[template.static_dispatch_plans.start..][0..template.static_dispatch_plans.len]) |plan_id| {
            const plan = plans.plans[@backingInt(plan_id)];
            try self.addTypeEdge(node, view, plan.callable_ty);
            try self.addTypeEdge(node, view, plan.dispatcher_ty);
            self.markOwn(node, self.literalFlags(view, plan));
            switch (plan.resolution) {
                .direct_closed, .direct_parametric => |direct| try self.addEvidenceNode(node, plans, direct.evidence),
                .direct_pending => |evidence| try self.addEvidenceNode(node, plans, evidence),
                // The target comes from the specialization's evidence or is
                // derived from the types, which the instance's and this
                // body's type edges reach.
                .evidence_dependent, .structural, .checked_error, .@"unreachable" => {},
            }
        }
    }

    fn addEvidenceNode(self: *Builder, from: NodeId, plans: *const static_dispatch.StaticDispatchPlanTable, root: static_dispatch.EvidenceNodeId) Allocator.Error!void {
        var pending = std.ArrayList(static_dispatch.EvidenceNodeId).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, root);
        while (pending.pop()) |node_id| {
            const node = plans.evidenceNode(node_id);
            switch (node.target.kind) {
                .procedure => |procedure| try self.addEdge(from, try self.templateNode(procedure.template)),
                .local_proc => self.markUnbounded(from, "local procedure evidence"),
                .structural => {},
            }
            switch (node.nested) {
                .resolved => for (plans.nestedEvidence(node)) |nested| try self.addCheckedEvidence(from, plans, nested, &pending),
                .from_callable => self.markUnbounded(from, "callable nested evidence"),
            }
        }
    }

    fn addCheckedEvidence(
        self: *Builder,
        from: NodeId,
        plans: *const static_dispatch.StaticDispatchPlanTable,
        evidence: static_dispatch.CheckedEvidence,
        pending: *std.ArrayList(static_dispatch.EvidenceNodeId),
    ) Allocator.Error!void {
        for (plans.evidence_refs[evidence.callable_contracts.start..][0..evidence.callable_contracts.len]) |contract| {
            try self.addCheckedEvidence(from, plans, contract, pending);
        }
        switch (evidence.resolution) {
            .direct => |node_id| try pending.append(self.allocator, node_id),
            .structural, .checked_error, .unreachable_value, .constraint, .from_callable, .from_scheme => {},
        }
    }

    /// A plan's own contribution when it is a literal conversion.
    fn literalFlags(self: *Builder, view: checked.ImportedModuleView, plan: static_dispatch.StaticDispatchCallPlan) Flags {
        if (plan.resolution == .checked_error) return .{};
        const expr = view.checked_bodies.stored_exprs[@backingInt(plan.expr)];
        const conversion_root, const dependent: Flags = switch (expr.data) {
            .numeral => |numeral| .{ numeral.conversion_root, .{ .dependent_numeral = true } },
            .str_from_quote => |quote| .{ quote.conversion_root, .{ .dependent_quote = true } },
            .interpolation => {
                if (traceEnabled()) std.debug.print("ctfe-demand unbounded interpolation {s} byte={d}\n", .{ view.canonical_names.moduleNameText(view.module_identity.module_name), expr.source_region.start.offset });
                return .{ .strict = true };
            },
            .pending,
            .str_segment,
            .str,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .list,
            .empty_list,
            .tuple,
            .match_,
            .if_,
            .call,
            .record,
            .empty_record,
            .block,
            .tag,
            .nominal,
            .zero_argument_tag,
            .closure,
            .lambda,
            .binop,
            .unary_minus,
            .unary_not,
            .field_access,
            .dispatch_call,
            .structural_eq,
            .structural_hash,
            .method_eq,
            .type_dispatch_call,
            .tuple_access,
            .runtime_error,
            .crash,
            .dbg,
            .expect_err,
            .expect,
            .ellipsis,
            .anno_only,
            .break_,
            .return_,
            .row_coerce,
            .for_,
            .hosted_lambda,
            .run_low_level,
            => return .{},
        };
        // A conversion with no root of its own depends on its instance.
        const root_id = conversion_root orelse return dependent;
        return switch (view.compile_time_roots.root(root_id).payload) {
            .pending => if (self.declared.contains(.{ .module = view.key.digest(), .root = root_id })) .{} else unrequested: {
                if (traceEnabled()) std.debug.print("ctfe-demand unbounded unrequested-root {s} byte={d}\n", .{ view.canonical_names.moduleNameText(view.module_identity.module_name), expr.source_region.start.offset });
                break :unrequested .{ .strict = true };
            },
            // A root an earlier compilation left unevaluated is requested by
            // no evaluation here, like an undeclared pending root.
            .unevaluated => .{ .strict = true },
            .const_node, .fn_value, .discarded, .expect, .runtime => .{},
        };
    }

    /// Record every method name dispatch can select by type in this module.
    fn addImplicitMethods(self: *Builder, view: checked.ImportedModuleView) Allocator.Error!void {
        const names = view.canonical_names;
        for (view.checked_procedure_templates.evidence_params_pool) |param| {
            try self.implicit_methods.put(self.allocator, names.methodNameText(param.method), {});
        }
        const plans = view.static_dispatch_plans;
        for (plans.plans) |plan| switch (plan.resolution) {
            .evidence_dependent, .structural => try self.implicit_methods.put(self.allocator, names.methodNameText(plan.method), {}),
            .direct_pending, .direct_closed, .direct_parametric, .checked_error, .@"unreachable" => {},
        };
        for (plans.generated_codec_calls) |call| try self.implicit_methods.put(self.allocator, names.methodNameText(call.method), {});
        for (plans.iterator_for_plans) |plan| {
            try self.implicit_methods.put(self.allocator, names.methodNameText(plan.iter.method), {});
            try self.implicit_methods.put(self.allocator, names.methodNameText(plan.next.method), {});
        }
        try self.implicit_methods.put(self.allocator, "to_inspect", {});
    }

    /// A type reaches the methods dispatch can select by that type; an
    /// explicit call reaches its method through its own plan.
    fn addMethods(self: *Builder, view: checked.ImportedModuleView) Allocator.Error!void {
        const names = view.canonical_names;
        for (view.method_registry.entries) |entry| {
            const target = entry.target orelse continue;
            const implicit = self.implicit_methods.contains(names.methodNameText(entry.key.method));
            const owner: NodeId = switch (entry.key.owner) {
                .nominal => |nominal| owner: {
                    const node = try self.nominalNode(.{
                        .module = names.moduleIdentityBytes(nominal.module).*,
                        .source_decl = nominal.source_decl,
                    });
                    const method = names.methodNameText(entry.key.method);
                    if (std.mem.eql(u8, method, "from_numeral")) self.markOwn(node, .{ .custom_numeral = true });
                    if (std.mem.eql(u8, method, "from_quote")) self.markOwn(node, .{ .custom_quote = true });
                    break :owner node;
                },
                .builtin => self.demand.builtin,
            };
            if (!implicit) continue;
            switch (target.kind) {
                .procedure => |procedure| try self.addEdge(owner, try self.templateNode(procedure.template)),
                .local_proc => self.markUnbounded(owner, "local procedure method"),
                .structural => {},
            }
        }
    }

    /// Reach flags: each node's own flags and those of every node it
    /// reaches, propagated backwards along the edges to a fixed point.
    fn propagate(self: *Builder) Allocator.Error!void {
        const count = self.own.items.len;
        const flags = &self.demand.flags;
        try flags.appendSlice(self.allocator, self.own.items);
        const starts = try self.allocator.alloc(u32, count + 1);
        defer self.allocator.free(starts);
        @memset(starts, 0);
        for (self.edges.items) |edge| starts[edge[1] + 1] += 1;
        for (1..count + 1) |index| starts[index] += starts[index - 1];
        const callers = try self.allocator.alloc(NodeId, self.edges.items.len);
        defer self.allocator.free(callers);
        const fill = try self.allocator.dupe(u32, starts[0..count]);
        defer self.allocator.free(fill);
        for (self.edges.items) |edge| {
            callers[fill[edge[1]]] = edge[0];
            fill[edge[1]] += 1;
        }
        var worklist = std.ArrayList(NodeId).empty;
        defer worklist.deinit(self.allocator);
        for (flags.items, 0..) |node_flags, node| {
            if (!node_flags.eql(.{})) try worklist.append(self.allocator, @intCast(node));
        }
        while (worklist.pop()) |node| {
            const node_flags = flags.items[node];
            for (callers[starts[node]..starts[node + 1]]) |caller| {
                const merged = flags.items[caller].merge(node_flags);
                if (merged.eql(flags.items[caller])) continue;
                flags.items[caller] = merged;
                try worklist.append(self.allocator, caller);
            }
        }
    }
};

/// Compute the reach flags of every procedure template of the lowering's
/// modules. `declared` names every compile-time root this compilation
/// evaluates.
pub fn compute(
    allocator: Allocator,
    modules: Common.CheckedModules,
    declared: *const std.AutoHashMapUnmanaged(DeclaredRoot, void),
) Allocator.Error!LiteralDemand {
    var demand = LiteralDemand{ .allocator = allocator };
    errdefer demand.deinit();
    var builder = Builder{ .allocator = allocator, .demand = &demand, .declared = declared };
    defer builder.deinit();

    try addView(&builder, checked.importedView(modules.root.module));
    for (modules.root.relation_modules) |view| try addView(&builder, view);
    for (modules.imports) |view| try addView(&builder, view);
    demand.counters.modules = builder.views.count();
    demand.builtin = try builder.addNode(.{});

    // Every template has its node before any edge names one.
    var views = builder.views.valueIterator();
    while (views.next()) |view| {
        for (0..view.checked_procedure_templates.templates.items.len) |index| {
            const node = try builder.addNode(.{});
            try demand.templates.put(allocator, .{ .module = view.key.bytes, .template = @intCast(index) }, node);
        }
    }
    views = builder.views.valueIterator();
    while (views.next()) |view| try builder.addImplicitMethods(view.*);
    views = builder.views.valueIterator();
    while (views.next()) |view| try builder.addMethods(view.*);
    views = builder.views.valueIterator();
    while (views.next()) |view| {
        for (view.checked_procedure_templates.templates.items, 0..) |template, index| {
            const node = demand.templates.get(.{ .module = view.key.bytes, .template = @intCast(index) }).?;
            try builder.addTemplate(view.*, node, template);
            if (!builder.own.items[node].eql(.{})) demand.counters.source_templates += 1;
        }
    }
    try builder.expandTypes();
    try builder.propagate();

    demand.counters.templates = demand.templates.count();
    var nominals = demand.nominals.iterator();
    while (nominals.next()) |entry| {
        const own = builder.own.items[entry.value_ptr.*];
        if (own.custom_numeral or own.custom_quote) demand.counters.custom_literal_types += 1;
    }
    var templates = demand.templates.valueIterator();
    while (templates.next()) |node| {
        if (demand.flags.items[node.*].reachesLiteralRoot()) demand.counters.reaching_templates += 1;
    }
    if (traceEnabled()) {
        var strict: u32 = 0;
        var numerals: u32 = 0;
        var quotes: u32 = 0;
        var reach_nodes = demand.templates.valueIterator();
        while (reach_nodes.next()) |node| {
            const flags = demand.flags.items[node.*];
            if (flags.strict) strict += 1;
            if (flags.dependent_numeral and flags.custom_numeral) numerals += 1;
            if (flags.dependent_quote and flags.custom_quote) quotes += 1;
        }
        std.debug.print("ctfe-demand reach strict={d} numeral={d} quote={d} builtin={b:0>8}\n", .{ strict, numerals, quotes, @as(u8, @bitCast(demand.builtinFlags())) });
        var nominal_nodes = demand.nominals.iterator();
        while (nominal_nodes.next()) |entry| {
            const own = builder.own.items[entry.value_ptr.*];
            if (own.custom_numeral or own.custom_quote) std.debug.print("ctfe-demand custom-type decl={d} numeral={} quote={}\n", .{ entry.key_ptr.source_decl, own.custom_numeral, own.custom_quote });
        }
        std.debug.print("ctfe-demand analysis templates={d} sources={d} reaching={d} custom-types={d}\n", .{
            demand.counters.templates,
            demand.counters.source_templates,
            demand.counters.reaching_templates,
            demand.counters.custom_literal_types,
        });
    }
    return demand;
}

fn addView(builder: *Builder, view: checked.ImportedModuleView) Allocator.Error!void {
    const entry = try builder.views.getOrPut(builder.allocator, view.key.bytes);
    if (!entry.found_existing) entry.value_ptr.* = view;
}

/// Select the runtime roots compile-time evaluation must specialize to find
/// the literal roots they reach: a root whose template, checked type, and
/// root evidence together cannot register a literal root is left to the
/// runtime consumer.
pub fn selectDiscoveryRoots(
    allocator: Allocator,
    demand: *const LiteralDemand,
    modules: Common.CheckedModules,
    requests: []const checked.RootRequest,
    source_modules: []const checked.ModuleId,
    selected: *std.ArrayList(usize),
) Allocator.Error!void {
    for (requests, 0..) |request, index| {
        const owner = if (source_modules.len == 0) modules.root.module.key.digest() else source_modules[index];
        const view = findView(modules, owner) orelse Common.invariant("discovery root named a checked module outside its lowering set");
        const flags = try rootFlags(allocator, demand, modules, view, request);
        const reaches = flags.reachesLiteralRoot();
        if (traceEnabled()) std.debug.print("ctfe-demand root {d} {s} {s}\n", .{
            index,
            view.canonical_names.moduleNameText(view.module_identity.module_name),
            if (reaches) "discovery" else "skipped",
        });
        if (reaches) try selected.append(allocator, index);
    }
}

fn rootFlags(
    allocator: Allocator,
    demand: *const LiteralDemand,
    modules: Common.CheckedModules,
    view: checked.ImportedModuleView,
    request: checked.RootRequest,
) Allocator.Error!Flags {
    const template = request.procedure_template orelse return Flags.all;
    const template_flags = demand.templateFlags(template);
    const type_flags = try demand.checkedTypeFlags(allocator, view.canonical_names, view.checked_types, request.checked_type);
    var evidence_flags: Flags = .{};
    if (request.root_evidence) |evidence| {
        const evidence_view = findView(modules, evidence.checked_module) orelse
            Common.invariant("discovery root evidence named a checked module outside its lowering set");
        const plans = evidence_view.static_dispatch_plans;
        evidence_flags = try demand.checkedEvidenceFlags(allocator, plans, plans.evidence_refs[evidence.span.start..][0..evidence.span.len]);
    }
    if (traceEnabled()) std.debug.print("ctfe-demand root-flags template={b:0>8} type={b:0>8} evidence={b:0>8}\n", .{
        @as(u8, @bitCast(template_flags)),
        @as(u8, @bitCast(type_flags)),
        @as(u8, @bitCast(evidence_flags)),
    });
    return template_flags.merge(type_flags).merge(evidence_flags);
}

fn findView(modules: Common.CheckedModules, key: checked.ModuleId) ?checked.ImportedModuleView {
    if (modules.root.module.key.digest().eql(key)) return checked.importedView(modules.root.module);
    for (modules.root.relation_modules) |view| if (view.key.digest().eql(key)) return view;
    for (modules.imports) |view| if (view.key.digest().eql(key)) return view;
    return null;
}
