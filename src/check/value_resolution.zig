//! Value-reference resolution, recorded by checking once a module is checked
//! (design.md "Value-Reference Resolution").
//!
//! Every reached lookup is resolved to the binding it names, in checker-native
//! ids (`ModuleEnv.value_references`); every top-level binding of the module
//! records what evaluating it produces (`ModuleEnv.binding_resolutions`); the
//! expressions reference resolution rejects are retired
//! (`ModuleEnv.retired_exprs`); and the calls whose callee is a directly
//! callable procedure are recorded (`ModuleEnv.direct_procedure_calls`).
//!
//! The result is a function of the module and of the recorded binding
//! resolutions of the modules its lookups name: its direct imports, and the
//! modules that declare the types its associated lookups name. Those modules
//! are checked first, and their binding resolutions are part of their
//! checking interfaces.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types_mod = @import("types");
const body_analyses = @import("body_analyses.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const ModuleIndex = body_analyses.ModuleIndex;
const ValueReference = ModuleEnv.ValueReference;
const ValueReferenceTarget = ModuleEnv.ValueReferenceTarget;
const BindingResolution = ModuleEnv.BindingResolution;
const BindingResolutionBody = ModuleEnv.BindingResolutionBody;

/// The checked modules a module's lookups name.
pub const Dependencies = struct {
    /// Directly imported modules, by resolved import module index.
    imported_modules: []const *const ModuleEnv,
    context: *const anyopaque,
    /// The module that declares a type an associated lookup names.
    owner_env: *const fn (context: *const anyopaque, owner: base.ModuleIdentity.Idx) *const ModuleEnv,
};

/// Resolve every reached value reference of `env` and record the results in
/// its checked outputs.
pub fn record(gpa: Allocator, env: *ModuleEnv, dependencies: Dependencies) Allocator.Error!void {
    var index = try ModuleIndex.build(gpa, env, env.promoted_local_procedures.items.items);
    defer index.deinit(gpa);
    var resolver = try Resolver.init(gpa, env, &index, dependencies);
    defer resolver.deinit();
    try resolver.collectBindings();
    try resolver.collectLocalRoles();
    try resolver.resolveReferences();
    try resolver.resolveAliasTargets();
    resolver.forwardBindings();
    try resolver.retire();
    try resolver.write();
}

const StatementRole = union(enum) {
    mutable_version,
    local_procedure: struct { expr: CIR.Expr.Idx, is_alias: bool },
    local_value,
};

/// What a lookup names, for the decisions resolution makes about it.
const ReferencedBinding = union(enum) {
    /// No procedure: a local value, parameter or binder, a constant, or a
    /// platform requirement.
    none,
    /// A block-local function binding.
    local_procedure: ModuleEnv.LocalProcedureReference,
    /// A top-level binding of this module, by its index in `bindings`.
    local: u32,
    /// A top-level binding of another module.
    foreign: BindingResolution,
};

const Binding = struct {
    pattern: CIR.Pattern.Idx,
    body: BindingResolutionBody,
    forwarded_lookup: ?CIR.Expr.Idx = null,
};

const Resolver = struct {
    gpa: Allocator,
    env: *ModuleEnv,
    index: *const ModuleIndex,
    dependencies: Dependencies,
    node_count: usize,
    bindings: std.ArrayList(Binding) = .empty,
    /// Top-level value definitions by pattern.
    top_level_defs: collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    /// Bindings of top-level value definitions, by pattern.
    top_level_bindings: collections.DenseMap(CIR.Pattern.Idx, u32),
    /// Bindings of function-valued selected hoisted roots, by pattern.
    hoisted_procedures: collections.DenseMap(CIR.Pattern.Idx, u32),
    /// Bindings of promoted local procedures, by pattern.
    promoted_procedures: collections.DenseMap(CIR.Pattern.Idx, u32),
    /// Bindings of data-valued selected hoisted roots, by pattern.
    hoisted_constants: collections.DenseMap(CIR.Pattern.Idx, u32),
    lambda_args: []bool = &.{},
    statement_roles: []?StatementRole = &.{},
    references: std.ArrayList(ValueReference) = .empty,
    /// Per reference: whether it names an exact procedure alias whose alias
    /// target is not resolved yet.
    pending_aliases: std.ArrayList(bool) = .empty,
    retired: std.DynamicBitSetUnmanaged = .{},
    /// Whether resolution retired a use of a rejected binding.
    rejected: bool = false,
    direct_calls: std.ArrayList(CIR.Expr.Idx) = .empty,

    fn init(gpa: Allocator, env: *ModuleEnv, index: *const ModuleIndex, dependencies: Dependencies) Allocator.Error!Resolver {
        const node_count = env.store.nodes.len();
        return .{
            .gpa = gpa,
            .env = env,
            .index = index,
            .dependencies = dependencies,
            .node_count = node_count,
            .top_level_defs = collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx).init(gpa),
            .top_level_bindings = collections.DenseMap(CIR.Pattern.Idx, u32).init(gpa),
            .hoisted_procedures = collections.DenseMap(CIR.Pattern.Idx, u32).init(gpa),
            .promoted_procedures = collections.DenseMap(CIR.Pattern.Idx, u32).init(gpa),
            .hoisted_constants = collections.DenseMap(CIR.Pattern.Idx, u32).init(gpa),
            .retired = try std.DynamicBitSetUnmanaged.initEmpty(gpa, node_count),
        };
    }

    fn deinit(self: *Resolver) void {
        self.bindings.deinit(self.gpa);
        self.top_level_defs.deinit();
        self.top_level_bindings.deinit();
        self.hoisted_procedures.deinit();
        self.promoted_procedures.deinit();
        self.hoisted_constants.deinit();
        self.gpa.free(self.lambda_args);
        self.gpa.free(self.statement_roles);
        self.references.deinit(self.gpa);
        self.pending_aliases.deinit(self.gpa);
        self.retired.deinit(self.gpa);
        self.direct_calls.deinit(self.gpa);
    }

    fn isFunction(self: *const Resolver, var_: types_mod.Var) bool {
        return self.env.types.varResolvesToFunction(var_);
    }

    fn appendBinding(self: *Resolver, binding: Binding) Allocator.Error!u32 {
        const id: u32 = @intCast(self.bindings.items.len);
        try self.bindings.append(self.gpa, binding);
        return id;
    }

    /// Every top-level binding: the top-level value definitions, the bound
    /// selected hoisted roots and the promoted local procedures.
    fn collectBindings(self: *Resolver) Allocator.Error!void {
        const env = self.env;
        for (env.store.sliceDefs(env.value_binding_defs)) |def_idx| {
            const def = env.store.getDef(def_idx);
            if (env.store.getPattern(def.pattern) != .assign) continue;
            if (!self.index.reachesPattern(def.pattern)) unreachable; // every top-level value definition is reached
            const body: BindingResolutionBody = if (self.isProcedureDefinition(def_idx, def))
                .procedure
            else if (self.isFunction(ModuleEnv.varFrom(def_idx)))
                if (env.store.getExpr(def.expr) == .e_runtime_error)
                    .{ .checked_error = def.expr }
                else
                    .{ .evaluated = def.expr }
            else
                .constant;
            const id = try self.appendBinding(.{ .pattern = def.pattern, .body = body });
            try self.top_level_defs.put(def.pattern, def_idx);
            try self.top_level_bindings.put(def.pattern, id);
        }

        for (env.selected_hoisted_roots.items.items) |root| {
            switch (root.value_kind) {
                .discarded => continue,
                .data_constant => {
                    const pattern = hoistedRootPattern(root) orelse continue;
                    // A lookup of a top-level definition's pattern names the
                    // definition, even when checking also selected it as a
                    // hoisted root.
                    if (self.top_level_bindings.contains(pattern)) continue;
                    const id = try self.appendBinding(.{ .pattern = pattern, .body = .constant });
                    try self.hoisted_constants.put(pattern, id);
                },
                .callable_binding => {
                    const pattern = hoistedRootPattern(root) orelse unreachable; // a function-valued hoisted root binds a pattern
                    if (self.top_level_bindings.contains(pattern)) continue;
                    const evaluated: ?CIR.Expr.Idx = switch (root.body) {
                        .expr, .valueless_binding => root.expr,
                        .pattern_error, .pattern_extraction, .pattern_validation => null,
                    };
                    const id = try self.appendBinding(.{ .pattern = pattern, .body = .{ .evaluated = evaluated } });
                    try self.hoisted_procedures.put(pattern, id);
                },
            }
        }

        for (env.promoted_local_procedures.items.items) |promoted| {
            const id = try self.appendBinding(.{ .pattern = promoted.pattern, .body = .procedure });
            try self.promoted_procedures.put(promoted.pattern, id);
        }
    }

    /// Whether a top-level definition is a procedure of its own: a lambda,
    /// closure or hosted function, a compiler intrinsic, or an
    /// annotation-only function.
    fn isProcedureDefinition(self: *const Resolver, def_idx: CIR.Def.Idx, def: CIR.Def) bool {
        const expr = self.env.store.getExpr(def.expr);
        if (expr == .e_lambda or expr == .e_closure or expr == .e_hosted_lambda) return true;
        if (expr != .e_anno_only) return false;
        // A compiler intrinsic is a procedure whatever it declares: its body
        // is the intrinsic wrapper.
        if (can.BuiltinLowLevel.isBuiltinModule(self.env) and
            can.BuiltinLowLevel.intrinsicAnnotation(self.env, expr.e_anno_only.ident) != null) return true;
        return self.isFunction(ModuleEnv.varFrom(def_idx));
    }

    /// The roles of block-local patterns: lambda parameters and the patterns
    /// local statements bind.
    fn collectLocalRoles(self: *Resolver) Allocator.Error!void {
        const env = self.env;
        self.lambda_args = try self.gpa.alloc(bool, self.node_count);
        @memset(self.lambda_args, false);
        self.statement_roles = try self.gpa.alloc(?StatementRole, self.node_count);
        @memset(self.statement_roles, null);
        for (0..self.node_count) |raw| {
            const node: CIR.Node.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
            const tag = env.store.nodes.get(node).tag;
            if (tag == .statement_decl or tag == .statement_var or tag == .statement_var_uninitialized) {
                const statement: CIR.Statement.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
                if (!self.index.reachesStatement(statement)) continue;
                const data = env.store.getStatement(statement);
                if (data == .s_decl) {
                    const decl = data.s_decl;
                    const is_alias = self.index.isSchemeAlias(decl.pattern);
                    const role: StatementRole = if (is_alias or isLocalProcedureExpr(env, decl.expr))
                        .{ .local_procedure = .{ .expr = decl.expr, .is_alias = is_alias } }
                    else
                        .local_value;
                    self.putStatementRole(decl.pattern, role);
                } else if (data == .s_var) {
                    self.putStatementRole(data.s_var.pattern_idx, .mutable_version);
                } else if (data == .s_var_uninitialized) {
                    self.putStatementRole(data.s_var_uninitialized.pattern_idx, .mutable_version);
                } else unreachable; // these node tags decode as these statements
            } else if (tag == .expr_lambda) {
                const expr: CIR.Expr.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
                if (!self.index.reachesExpr(expr)) continue;
                const lambda = env.store.getExpr(expr).e_lambda;
                for (env.store.slicePatterns(lambda.args)) |arg| self.lambda_args[@backingInt(arg)] = true;
            }
        }
    }

    fn putStatementRole(self: *Resolver, pattern: CIR.Pattern.Idx, role: StatementRole) void {
        const slot = &self.statement_roles[@backingInt(pattern)];
        if (slot.*) |existing| {
            std.debug.assert(std.meta.eql(existing, role));
            return;
        }
        slot.* = role;
    }

    fn resolveReferences(self: *Resolver) Allocator.Error!void {
        const env = self.env;
        for (0..self.node_count) |raw| {
            const node: CIR.Node.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
            const tag = env.store.nodes.get(node).tag;
            if (tag != .expr_var and tag != .expr_external_lookup and
                tag != .expr_associated_lookup_resolved and tag != .expr_required_lookup) continue;
            const expr: CIR.Expr.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
            // Unreached source nodes can still hold unresolved lookups and
            // need no resolution.
            if (!self.index.reachesExpr(expr)) continue;
            var pending_alias = false;
            const data = env.store.getExpr(expr);
            const target: ValueReferenceTarget = if (data == .e_lookup_local)
                self.resolveLocal(data.e_lookup_local.pattern_idx, &pending_alias) orelse {
                    // A use of a rejected binding is retired.
                    self.retired.set(raw);
                    self.rejected = true;
                    continue;
                }
            else if (data == .e_lookup_external)
                .imported
            else if (data == .e_lookup_associated_resolved)
                if (data.e_lookup_associated_resolved.module_identity == env.selfModuleIdentity())
                    .{ .top_level = data.e_lookup_associated_resolved.target_def_idx }
                else
                    .associated
            else if (data == .e_lookup_required)
                .{ .required = data.e_lookup_required.requires_idx.toU32() }
            else
                unreachable; // the node tags above decode as these lookups
            try self.references.append(self.gpa, .{ .expr = expr, .target = target });
            try self.pending_aliases.append(self.gpa, pending_alias);
        }
    }

    /// The binding a local lookup names, or null when its binding was rejected.
    fn resolveLocal(self: *const Resolver, pattern: CIR.Pattern.Idx, pending_alias: *bool) ?ValueReferenceTarget {
        // A binder whose binding statement checking rejected is not
        // reached: the statement evaluates as a runtime error and binds
        // nothing, so a use of the binder names no binding.
        if (!self.index.reachesPattern(pattern)) return null;
        if (self.top_level_defs.get(pattern)) |def| return .{ .top_level = def };
        if (self.hoisted_procedures.contains(pattern)) return .{ .hoisted_procedure = pattern };
        if (self.promoted_procedures.contains(pattern)) return .{ .promoted_procedure = pattern };
        if (self.hoisted_constants.contains(pattern)) return .{ .hoisted_constant = pattern };
        const raw = @backingInt(pattern);
        if (self.lambda_args[raw]) return .{ .local_param = pattern };
        if (self.statement_roles[raw]) |role| return switch (role) {
            .mutable_version => .{ .local_mutable_version = pattern },
            .local_value => .{ .local_value = pattern },
            .local_procedure => |procedure| blk: {
                pending_alias.* = procedure.is_alias;
                break :blk .{ .local_procedure = .{ .binder = pattern, .expr = procedure.expr, .alias_target = null } };
            },
        };
        if (patternIsBinder(self.env, pattern)) return .{ .pattern_binder = pattern };
        unreachable; // every reached local lookup names a categorized binding
    }

    fn referenceIndex(self: *const Resolver, expr: CIR.Expr.Idx) ?usize {
        return std.sort.binarySearch(ValueReference, self.references.items, expr, struct {
            fn order(key: CIR.Expr.Idx, item: ValueReference) std.math.Order {
                return std.math.order(@backingInt(key), @backingInt(item.expr));
            }
        }.order);
    }

    /// Give each exact procedure alias the lookup its alias chain ends at.
    fn resolveAliasTargets(self: *Resolver) Allocator.Error!void {
        var path = std.ArrayList(usize).empty;
        defer path.deinit(self.gpa);
        for (self.pending_aliases.items, 0..) |pending, start| {
            if (!pending) continue;
            path.clearRetainingCapacity();
            var current = start;
            const target: CIR.Expr.Idx = while (true) {
                const reference = self.references.items[current];
                if (reference.target != .local_procedure or !self.pending_aliases.items[current]) break reference.expr;
                if (reference.target.local_procedure.alias_target) |done| break done;
                std.debug.assert(path.items.len < self.references.items.len); // alias declarations are acyclic
                try path.append(self.gpa, current);
                current = self.referenceIndex(reference.target.local_procedure.expr) orelse unreachable; // an alias's right-hand side is a reached lookup
            };
            for (path.items) |alias| self.references.items[alias].target.local_procedure.alias_target = target;
        }
    }

    fn referencedBinding(self: *const Resolver, reference: ValueReference) ReferencedBinding {
        return switch (reference.target) {
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .required => .none,
            .local_procedure => |procedure| .{ .local_procedure = procedure },
            .top_level => |def| .{ .local = self.top_level_bindings.get(self.env.store.getDef(def).pattern) orelse
                unreachable }, // a top-level lookup names a top-level value definition
            .hoisted_procedure => |pattern| .{ .local = self.hoisted_procedures.get(pattern).? },
            .promoted_procedure => |pattern| .{ .local = self.promoted_procedures.get(pattern).? },
            .hoisted_constant => |pattern| .{ .local = self.hoisted_constants.get(pattern).? },
            .imported => blk: {
                const imported = self.env.store.getExpr(reference.expr).e_lookup_external;
                const module_idx = self.env.imports.getResolvedModule(imported.module_idx) orelse unreachable; // reached lookups have resolved imports
                const module = self.dependencies.imported_modules[module_idx];
                break :blk .{ .foreign = module.bindingResolutionForNode(imported.target_node_idx) orelse unreachable }; // an imported value is a recorded top-level binding
            },
            .associated => blk: {
                const associated = self.env.store.getExpr(reference.expr).e_lookup_associated_resolved;
                const module = self.dependencies.owner_env(self.dependencies.context, associated.module_identity);
                break :blk .{ .foreign = module.bindingResolutionForNode(@backingInt(associated.target_def_idx)) orelse unreachable }; // an associated value is a recorded top-level binding
            },
        };
    }

    fn bindingBody(self: *const Resolver, binding: ReferencedBinding) ?BindingResolutionBody {
        return switch (binding) {
            .none, .local_procedure => null,
            .local => |id| self.bindings.items[id].body,
            .foreign => |foreign| foreign.body,
        };
    }

    fn bindingForwards(self: *const Resolver, binding: ReferencedBinding) bool {
        return switch (binding) {
            .none, .local_procedure => false,
            .local => |id| self.bindings.items[id].forwarded_lookup != null,
            .foreign => |foreign| foreign.forwarded_lookup != null,
        };
    }

    /// Whether a call through `target` calls a procedure directly. A call
    /// through an exact procedure alias is direct when the alias reaches a
    /// directly callable procedure.
    fn callableDirectly(self: *const Resolver, reference: ValueReference) bool {
        const binding = self.referencedBinding(reference);
        return switch (binding) {
            .none => false,
            .local_procedure => |procedure| if (procedure.alias_target) |alias_target|
                self.callableDirectly(self.references.items[self.referenceIndex(alias_target).?])
            else
                true,
            .local, .foreign => switch (self.bindingBody(binding).?) {
                .constant, .checked_error => false,
                .procedure => true,
                .evaluated => self.bindingForwards(binding),
            },
        };
    }

    /// Record every compile-time-evaluated binding whose root is a lookup of
    /// a directly callable procedure: a call through it forwards to that
    /// lookup. An alias of another alias forwards once that alias does, so
    /// this runs to a fixed point.
    fn forwardBindings(self: *Resolver) void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.bindings.items) |*binding| {
                if (binding.forwarded_lookup != null) continue;
                const root = switch (binding.body) {
                    .evaluated => |evaluated| evaluated orelse continue,
                    .constant, .procedure, .checked_error => continue,
                };
                const index = self.referenceIndex(root) orelse continue;
                const target = self.references.items[index].target;
                if (target == .local_procedure) continue;
                if (!self.callableDirectly(self.references.items[index])) continue;
                binding.forwarded_lookup = root;
                changed = true;
            }
        }
    }

    fn isError(self: *const Resolver, expr: CIR.Expr.Idx) bool {
        return self.retired.isSet(@backingInt(expr)) or self.env.store.getExpr(expr) == .e_runtime_error;
    }

    /// Retire the uses of rejected bindings and the calls of callees that
    /// are runtime errors, record each remaining call's directness, then
    /// propagate the rejections through call-to-callee edges and through
    /// each compile-time-evaluated binding's root; such a binding whose root
    /// is retired becomes a checked error.
    fn retire(self: *Resolver) Allocator.Error!void {
        const env = self.env;
        var rejected = self.rejected;
        for (self.references.items) |reference| {
            const body = self.bindingBody(self.referencedBinding(reference)) orelse continue;
            if (body == .checked_error) {
                self.retired.set(@backingInt(reference.expr));
                rejected = true;
            }
        }

        var candidate_calls = std.ArrayList(CIR.Expr.Idx).empty;
        defer candidate_calls.deinit(self.gpa);
        for (self.index.node_kinds, 0..) |kind, raw| {
            if (kind != .expr) continue;
            const expr: CIR.Expr.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
            const data = env.store.getExpr(expr);
            if (data != .e_call) continue;
            // The callee is evaluated before any argument; a callee that is
            // a runtime error produces neither a callable nor a result.
            if (self.isError(data.e_call.func)) {
                self.retired.set(raw);
                rejected = true;
                continue;
            }
            const index = self.referenceIndex(data.e_call.func) orelse continue;
            if (self.callableDirectly(self.references.items[index])) try candidate_calls.append(self.gpa, expr);
        }

        if (rejected) try self.propagate();

        for (candidate_calls.items) |call| {
            if (!self.retired.isSet(@backingInt(call))) try self.direct_calls.append(self.gpa, call);
        }
    }

    fn propagate(self: *Resolver) Allocator.Error!void {
        const env = self.env;
        var dependents = collections.DenseMap(u32, std.ArrayList(CIR.Expr.Idx)).init(self.gpa);
        defer {
            var lists = dependents.valueIterator();
            while (lists.next()) |list| list.deinit(self.gpa);
            dependents.deinit();
        }
        var work = std.ArrayList(CIR.Expr.Idx).empty;
        defer work.deinit(self.gpa);
        for (self.index.node_kinds, 0..) |kind, raw| {
            if (kind != .expr) continue;
            const expr: CIR.Expr.Idx = @fromBackingInt(@as(u32, @intCast(raw)));
            if (self.isError(expr)) {
                try work.append(self.gpa, expr);
                continue;
            }
            const data = env.store.getExpr(expr);
            if (data == .e_call) try addDependent(self.gpa, &dependents, data.e_call.func, expr);
        }
        for (self.references.items) |reference| {
            const id = switch (self.referencedBinding(reference)) {
                .local => |id| id,
                .none, .local_procedure, .foreign => continue,
            };
            const root = switch (self.bindings.items[id].body) {
                .evaluated => |evaluated| evaluated orelse continue,
                .constant, .procedure, .checked_error => continue,
            };
            try addDependent(self.gpa, &dependents, root, reference.expr);
        }
        while (work.pop()) |failed| {
            const waiting = dependents.get(@backingInt(failed)) orelse continue;
            for (waiting.items) |dependent| {
                if (self.isError(dependent)) continue;
                self.retired.set(@backingInt(dependent));
                try work.append(self.gpa, dependent);
            }
        }
        for (self.bindings.items) |*binding| {
            const root = switch (binding.body) {
                .evaluated => |evaluated| evaluated orelse continue,
                .constant, .procedure, .checked_error => continue,
            };
            if (self.isError(root)) binding.body = .{ .checked_error = root };
        }
    }

    fn write(self: *Resolver) Allocator.Error!void {
        const env = self.env;
        const gpa = env.gpa;
        env.value_references.items.clearRetainingCapacity();
        try env.value_references.items.appendSlice(gpa, self.references.items);

        env.retired_exprs.items.clearRetainingCapacity();
        try env.retired_exprs.items.ensureTotalCapacity(gpa, self.retired.count());
        var retired = self.retired.iterator(.{});
        while (retired.next()) |raw| env.retired_exprs.items.appendAssumeCapacity(@fromBackingInt(@as(u32, @intCast(raw))));

        env.direct_procedure_calls.items.clearRetainingCapacity();
        try env.direct_procedure_calls.items.appendSlice(gpa, self.direct_calls.items);

        env.binding_resolutions.items.clearRetainingCapacity();
        try env.binding_resolutions.items.ensureTotalCapacity(gpa, self.bindings.items.len);
        for (self.bindings.items) |binding| env.binding_resolutions.items.appendAssumeCapacity(.{
            .pattern = binding.pattern,
            .body = binding.body,
            .forwarded_lookup = binding.forwarded_lookup,
        });
        const sorted = env.binding_resolutions.items.items;
        std.mem.sort(BindingResolution, sorted, {}, struct {
            fn lessThan(_: void, a: BindingResolution, b: BindingResolution) bool {
                return @backingInt(a.pattern) < @backingInt(b.pattern);
            }
        }.lessThan);
        // A binding pattern names one top-level binding.
        for (sorted[0..sorted.len -| 1], 0..) |binding, i| std.debug.assert(@backingInt(binding.pattern) < @backingInt(sorted[i + 1].pattern));
    }
};

fn addDependent(
    gpa: Allocator,
    dependents: *collections.DenseMap(u32, std.ArrayList(CIR.Expr.Idx)),
    on: CIR.Expr.Idx,
    dependent: CIR.Expr.Idx,
) Allocator.Error!void {
    const entry = try dependents.getOrPut(@backingInt(on));
    if (!entry.found_existing) entry.value_ptr.* = .empty;
    try entry.value_ptr.append(gpa, dependent);
}

/// The pattern a selected hoisted root binds: its binding pattern, or the
/// result pattern of a destructure extraction.
fn hoistedRootPattern(root: ModuleEnv.SelectedHoistedRoot) ?CIR.Pattern.Idx {
    return switch (root.body) {
        .expr, .pattern_error => root.pattern,
        .pattern_extraction => |extraction| extraction.result_pattern,
        .pattern_validation => |validation| validation.scrutinee_pattern,
        .valueless_binding => null,
    };
}

fn isLocalProcedureExpr(env: *const ModuleEnv, expr: CIR.Expr.Idx) bool {
    const data = env.store.getExpr(expr);
    return data == .e_lambda or data == .e_closure;
}

fn patternIsBinder(env: *const ModuleEnv, pattern: CIR.Pattern.Idx) bool {
    const tag = env.store.nodes.get(ModuleEnv.nodeIdxFrom(pattern)).tag;
    return tag == .pattern_identifier or
        tag == .pattern_var_identifier or
        tag == .pattern_as or
        tag == .pattern_applied_tag or
        tag == .pattern_nominal or
        tag == .pattern_nominal_external or
        tag == .pattern_record_destructure or
        tag == .pattern_list or
        tag == .pattern_tuple;
}
