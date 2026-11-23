//! Read-only, scoped nominal substitution for structural type analyses.
//!
//! The reader has its own ID namespace: source IDs denote actual solver roots;
//! IDs above the source's frozen length denote (template root, environment)
//! views. Although descriptor payloads use Var's compact representation, view
//! IDs MUST NOT be passed to the solver. `sourceVar` is the only export route.
//! Opening a nested declaration replaces, rather than extends, its formal scope.
//! Lazy descriptors and arena-backed child spans stay valid across nested reads.
//! No solver variables, redirects, or descriptors change.

const std = @import("std");
const collections = @import("collections");
const types = @import("types");
const Var = types.Var;
const Allocator = std.mem.Allocator;
const Self = @This();

source: *const types.Store,
gpa: Allocator,
// Stable storage is part of this reader's representation, not merely scratch.
// Do not use SingleThreadArena here: its allocation-debug pass-through mode
// disables the bulk ownership these immutable spans and dependency graphs need.
arena: collections.SingleThreadArena,
source_len: u32,
vars_base: u32,
tags_base: u32,
fields_base: u32,
nodes: std.ArrayList(Node) = .empty,
scopes: std.ArrayList(Scope) = .empty,
environment_index: std.HashMapUnmanaged(Environment, u32, EnvironmentContext, std.hash_map.default_max_load_percentage) = .empty,
dependencies: collections.DenseMapUnmanaged(types.NominalDecl.Idx, *Dependencies) = .empty,
/// The backing of the nominal type each root holds, by root, as
/// `openNominalBackingOf` answered it: absent for an invalid declaration.
opened: collections.DenseMapUnmanaged(Var, OptVar) = .empty,
/// The backing of each valid declaration that takes no arguments, as
/// `openNominalBacking` answered it, by the declaration key its applications
/// carry (`nominalDeclarationKey`): every application of such a declaration opens to the
/// same view.
argless_backings: std.AutoHashMapUnmanaged(u64, Var) = .empty,
/// Where the graphs of final declarations live across readers, if anywhere.
shared: ?Shared = null,
argument_scratch: std.ArrayList(OptVar) = .empty,
// Projected Range.start values name immutable spans, not element offsets.
// Only this reader may interpret or index them. Reallocating these directories
// never moves or poisons the independently owned child spans.
vars: std.ArrayList([]const Var) = .empty,
tags: std.ArrayList(TagSlice) = .empty,
fields: std.ArrayList(FieldSlice) = .empty,

const TagSlice = struct {
    name: []const @FieldType(types.Tag, "name"),
    args: []const @FieldType(types.Tag, "args"),
    pub fn items(self: @This(), comptime field: types.Tag.SafeMultiList.Field) []const @FieldType(types.Tag, @tagName(field)) {
        return @field(self, @tagName(field));
    }
};
const FieldSlice = struct {
    name: []const @FieldType(types.RecordField, "name"),
    presence: []const types.RecordField.Presence,
    pub fn items(self: @This(), comptime field: types.RecordField.SafeMultiList.Field) []const @FieldType(types.RecordField, @tagName(field)) {
        return @field(self, @tagName(field));
    }
};

/// A template root in a scope: `dependency` is the root's node in the
/// scope's declaration graph.
const Node = struct { scope: u32, dependency: u32, descriptor: ?types.Descriptor = null };
/// An environment's argument is absent where the declaration's graph needs
/// none of it.
const Environment = struct { declaration: types.NominalDecl.Idx, args: []const OptVar };
/// An interned environment, its declaration's graph, and its views.
const Scope = struct {
    environment: Environment,
    dependencies: *const Dependencies,
    /// The view of each template root in this scope, indexed by the root's
    /// dependency node and absent until the root is first viewed here.
    /// Empty until the scope's first view.
    views: []OptVar = &.{},
};
const EnvironmentContext = struct {
    pub fn hash(_: @This(), env: Environment) u64 {
        var h = std.hash.Wyhash.init(0);
        std.hash.autoHash(&h, env.declaration);
        for (env.args) |arg| std.hash.autoHash(&h, arg);
        return h.final();
    }
    pub fn eql(_: @This(), a: Environment, b: Environment) bool {
        return a.declaration == b.declaration and optVarsEqual(a.args, b.args);
    }
};
pub const Resolved = struct { var_: Var, desc: types.Descriptor };

/// Exact substitution and unknown-owner dependencies, propagated backwards
/// through template edges.
/// Masking irrelevant arguments is essential for finite closed-argument resets
/// in otherwise recursive declarations; it is not decided by guessing from arity or groundness.
///
/// The graph also records what a projection needs of each template root, so a
/// view finds its children by position and never looks a variable up: the
/// declaration's backing is node zero, and every node lists its children in
/// the order `projectContent` views them.
const Dependencies = struct {
    nodes: std.ArrayList(DependencyNode) = .empty,
};
const DependencyNode = struct {
    root: Var,
    /// The first formal this root stands for, whose actual replaces it.
    formal: ?u32 = null,
    formals: []bool,
    /// Structural substitution dependencies, excluding private unknowns'
    /// application ownership. Only these edges can expose a source variable.
    source_formals: []bool,
    /// The nodes of the root's children, in projection order.
    children: []const u32 = &.{},
    parents: std.ArrayList(u32) = .empty,
};
/// A declaration formal, resolved once per graph.
const Formal = struct {
    root: Var,
    rigid_name: ?@FieldType(types.Rigid, "name"),

    fn init(resolved: types.ResolvedVarDesc) Formal {
        return .{
            .root = resolved.var_,
            .rigid_name = if (resolved.desc.content == .rigid) resolved.desc.content.rigid.name else null,
        };
    }

    fn matches(self: Formal, value: types.ResolvedVarDesc) bool {
        if (value.var_ == self.root) return true;
        const name = self.rigid_name orelse return false;
        return value.desc.content == .rigid and value.desc.content.rigid.name.eql(name);
    }
};
/// State of one `computeDependencies` run. Its scratch lives in the reader's
/// arena, whichever allocator owns the graph.
const DependencyBuilder = struct {
    /// The graph's owner.
    alloc: Allocator,
    scratch: Allocator,
    deps: *Dependencies,
    formal_count: usize,
    /// Each root's node.
    indices: std.AutoHashMapUnmanaged(Var, u32) = .empty,
    /// The children of the node being expanded.
    children: std.ArrayList(u32) = .empty,
};
const OptVar = collections.OptionalId(Var);

/// Formal-dependency graphs that outlive a reader. A declaration's graph is a
/// function of its formals and backing template alone, so once the template
/// can no longer change the first reader to open the declaration computes the
/// graph here and every later reader of the same source store reuses it.
pub const SharedDependencies = struct {
    gpa: Allocator,
    source: *const types.Store,
    arena: collections.SingleThreadArena,
    graphs: collections.DenseMapUnmanaged(types.NominalDecl.Idx, *Dependencies) = .empty,

    pub fn init(gpa: Allocator, source: *const types.Store) SharedDependencies {
        return .{ .gpa = gpa, .source = source, .arena = collections.SingleThreadArena.init(gpa) };
    }

    pub fn deinit(self: *SharedDependencies) void {
        self.graphs.deinit(self.gpa);
        self.arena.deinit();
    }
};

/// A reader's access to shared graphs: the storage, and the source store's
/// owner's explicit answer to whether a declaration's backing template is
/// final. Only a final declaration's graph is stored or reused.
pub const Shared = struct {
    graphs: *SharedDependencies,
    owner: *const anyopaque,
    isFinal: *const fn (owner: *const anyopaque, declaration: types.NominalDecl.Idx) bool,
};

pub fn init(gpa: Allocator, source: *const types.Store) Self {
    return .{
        .source = source,
        .gpa = gpa,
        .arena = collections.SingleThreadArena.init(gpa),
        .source_len = @intCast(source.len()),
        .vars_base = @intCast(source.vars.len()),
        .tags_base = source.tags.len(),
        .fields_base = source.record_fields.len(),
    };
}

/// A reader that reuses and extends `shared`'s graphs.
pub fn initShared(gpa: Allocator, source: *const types.Store, shared: Shared) Self {
    std.debug.assert(shared.graphs.source == source);
    var reader = init(gpa, source);
    reader.shared = shared;
    return reader;
}

pub fn deinit(self: *Self) void {
    std.debug.assert(self.source.len() == self.source_len);
    self.environment_index.deinit(self.gpa);
    self.dependencies.deinit(self.gpa);
    self.opened.deinit(self.gpa);
    self.argless_backings.deinit(self.gpa);
    self.nodes.deinit(self.gpa);
    self.scopes.deinit(self.gpa);
    self.argument_scratch.deinit(self.gpa);
    self.vars.deinit(self.gpa);
    self.tags.deinit(self.gpa);
    self.fields.deinit(self.gpa);
    self.arena.deinit();
}

/// Analysis-owned views have no mutable solver representative.
pub fn sourceVar(self: *const Self, view_id: Var) ?Var {
    if (@backingInt(view_id) >= self.source_len) {
        std.debug.assert(@backingInt(view_id) - self.source_len < self.nodes.items.len);
        return null;
    }
    return self.source.resolveVar(view_id).var_;
}

/// Whether this view's structural substitutions reach a source variable.
/// A false answer proves that blocker collection can export nothing, without
/// asking whether the template itself is inhabited. Source types always need
/// ordinary analysis: their descriptors can change between reads.
pub fn hasSourceDependencies(self: *const Self, view_id: Var) Allocator.Error!bool {
    if (self.sourceVar(view_id) != null) return true;
    var pending: std.ArrayList(Var) = .empty;
    defer pending.deinit(self.gpa);
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(self.gpa);
    var current = view_id;
    while (true) {
        const node = self.nodes.items[@backingInt(current) - self.source_len];
        const scope = self.scopes.items[node.scope];
        const required = scope.dependencies.nodes.items[node.dependency].source_formals;
        for (scope.environment.args, required) |actual, needed| {
            if (!needed) continue;
            const actual_var = actual.get().?;
            if (self.sourceVar(actual_var) != null) return true;
            if (!(try seen.getOrPut(self.gpa, actual_var)).found_existing) try pending.append(self.gpa, actual_var);
        }
        current = pending.pop() orelse return false;
    }
}

/// Resolve identity without projecting a descriptor or allocating.
pub fn root(self: *const Self, view_id: Var) Var {
    if (self.sourceVar(view_id)) |var_| return var_;
    std.debug.assert(@backingInt(view_id) - self.source_len < self.nodes.items.len);
    return view_id;
}

/// Resolve a descriptor without mutating the source solver graph.
pub fn resolveVar(self: *Self, view_id: Var) Allocator.Error!Resolved {
    if (@backingInt(view_id) < self.source_len) {
        const resolved = self.source.resolveVar(view_id);
        return .{ .var_ = resolved.var_, .desc = resolved.desc };
    }
    const index = @backingInt(view_id) - self.source_len;
    const node = self.nodes.items[index];
    if (node.descriptor) |descriptor| return .{ .var_ = view_id, .desc = descriptor };
    const dependencies = self.scopes.items[node.scope].dependencies;
    const template = dependencies.nodes.items[node.dependency];
    var descriptor = self.source.resolveVar(template.root).desc;
    var projection: Projection = .{ .dependencies = dependencies, .scope = node.scope, .children = template.children };
    descriptor.content = try self.projectContent(descriptor.content, &projection);
    std.debug.assert(projection.next == projection.children.len);
    // Projecting children can grow nodes; never retain a pointer into it.
    self.nodes.items[index].descriptor = descriptor;
    return .{ .var_ = view_id, .desc = descriptor };
}

/// Actuals are already in their caller's scope. Substitution returns them
/// directly and never interprets their rigid names in the callee's scope.
pub fn openNominalBacking(self: *Self, nominal: types.NominalType) Allocator.Error!?Var {
    const argless = self.sliceNominalArgs(nominal).len == 0;
    if (argless) {
        if (self.argless_backings.get(nominalDeclarationKey(nominal))) |backing| {
            if (std.debug.runtime_safety) std.debug.assert(try self.openDeclaredBacking(nominal) == backing);
            return backing;
        }
    }
    const backing = (try self.openDeclaredBacking(nominal)) orelse return null;
    if (argless) try self.argless_backings.put(self.gpa, nominalDeclarationKey(nominal), backing);
    return backing;
}

/// The declaration key of an application: its origin module and declaring
/// statement, which name one declaration of the source store.
pub fn nominalDeclarationKey(nominal: types.NominalType) u64 {
    return (@as(u64, @backingInt(nominal.origin_module)) << 32) | nominal.statement();
}

/// The view of the backing of `nominal`'s declaration under its arguments, or
/// null for an invalid declaration.
fn openDeclaredBacking(self: *Self, nominal: types.NominalType) Allocator.Error!?Var {
    const declaration = self.source.lookupNominalDecl(nominal) orelse
        @panic("type view referenced a missing nominal declaration");
    const decl = self.source.getNominalDecl(declaration);
    if (!decl.isValid()) return null;
    const args = self.sliceNominalArgs(nominal);
    std.debug.assert(args.len == decl.formals.count);
    return try self.viewBacking(declaration, args);
}

/// The view of a valid declaration's backing under `args`.
fn viewBacking(self: *Self, declaration: types.NominalDecl.Idx, args: []const Var) Allocator.Error!Var {
    const dependencies = try self.ensureDependencies(declaration);
    try self.argument_scratch.resize(self.gpa, args.len);
    const canonical = self.argument_scratch.items;
    for (args, canonical) |arg, *dest| dest.* = .some(self.root(arg));
    const scope = try self.internScope(.{ .declaration = declaration, .args = canonical }, dependencies);
    return try self.view(dependencies, backing_dependency, scope);
}

/// `openNominalBacking` for the nominal type that `nominal_root` holds,
/// answered once per root: a root's content is fixed for the reader's life,
/// and opening the same application always yields the same view.
pub fn openNominalBackingOf(self: *Self, nominal_root: Var, nominal: types.NominalType) Allocator.Error!?Var {
    if (std.debug.runtime_safety) std.debug.assert(self.root(nominal_root) == nominal_root);
    if (self.opened.get(nominal_root)) |backing| {
        if (std.debug.runtime_safety) {
            const reopened = try self.openNominalBacking(nominal);
            std.debug.assert(OptVar.of(reopened) == backing);
        }
        return backing.get();
    }
    const backing = try self.openNominalBacking(nominal);
    try self.opened.put(self.gpa, nominal_root, .of(backing));
    return backing;
}

fn internScope(self: *Self, environment: Environment, dependencies: *const Dependencies) Allocator.Error!u32 {
    if (self.environment_index.get(environment)) |existing| return existing;
    const owned = Environment{
        .declaration = environment.declaration,
        .args = try self.arena.allocator().dupe(OptVar, environment.args),
    };
    const index: u32 = @intCast(self.scopes.items.len);
    try self.scopes.append(self.gpa, .{ .environment = owned, .dependencies = dependencies });
    errdefer _ = self.scopes.pop();
    try self.environment_index.putNoClobber(self.gpa, owned, index);
    return index;
}

/// The view of the template root at `dependency` of `dependencies`, the graph
/// of `scope`'s declaration, in that scope.
fn view(self: *Self, dependencies: *const Dependencies, dependency: u32, scope: u32) Allocator.Error!Var {
    const template = &dependencies.nodes.items[dependency];
    const environment = self.scopes.items[scope].environment;
    if (template.formal) |formal| {
        return environment.args[formal].get().?;
    }
    var normalized = scope;
    for (environment.args, template.formals) |actual, needed| {
        if (!needed and actual != .none) {
            try self.argument_scratch.resize(self.gpa, environment.args.len);
            for (environment.args, template.formals, self.argument_scratch.items) |arg, used, *dest| {
                dest.* = if (used) arg else .none;
            }
            normalized = try self.internScope(.{ .declaration = environment.declaration, .args = self.argument_scratch.items }, dependencies);
            break;
        }
    }
    // Interning can grow scopes; take the pointer only now.
    const views = &self.scopes.items[normalized].views;
    if (views.*.len == 0) {
        const fresh = try self.arena.allocator().alloc(OptVar, dependencies.nodes.items.len);
        @memset(fresh, .none);
        views.* = fresh;
    }
    const slot = &views.*[dependency];
    if (slot.get()) |existing| return existing;
    // An optional Var (a view slot, a field's presence) stores the Var plus
    // one, so the Var's biased form must fit too.
    const index = try collections.nextBiasedId(try addOffset(self.source_len, @intCast(self.nodes.items.len)));
    const result: Var = @fromBackingInt(index);
    try self.nodes.append(self.gpa, .{ .scope = normalized, .dependency = dependency });
    slot.* = .some(result);
    return result;
}

/// The children of one template root, viewed in the order its dependency
/// node lists them.
const Projection = struct {
    dependencies: *const Dependencies,
    scope: u32,
    children: []const u32,
    next: usize = 0,
};

/// The view of `projection`'s next child, which is `source_var`.
fn viewChild(self: *Self, projection: *Projection, source_var: Var) Allocator.Error!Var {
    const dependency = projection.children[projection.next];
    projection.next += 1;
    if (std.debug.runtime_safety) {
        std.debug.assert(projection.dependencies.nodes.items[dependency].root == self.source.resolveVar(source_var).var_);
    }
    return self.view(projection.dependencies, dependency, projection.scope);
}

/// The declaration's backing is the first node of its graph.
const backing_dependency: u32 = 0;

fn dependencyNode(self: *Self, builder: *DependencyBuilder, var_: Var) Allocator.Error!u32 {
    const root_var = self.source.resolveVar(var_).var_;
    const entry = try builder.indices.getOrPut(builder.scratch, root_var);
    if (entry.found_existing) return entry.value_ptr.*;
    const index: u32 = @intCast(builder.deps.nodes.items.len);
    entry.value_ptr.* = index;
    const required = try builder.alloc.alloc(bool, builder.formal_count);
    @memset(required, false);
    const source_required = try builder.alloc.alloc(bool, builder.formal_count);
    @memset(source_required, false);
    try builder.deps.nodes.append(builder.alloc, .{ .root = root_var, .formals = required, .source_formals = source_required });
    return index;
}

/// Record `child` as the next child of `parent`, the node being expanded.
fn dependencyEdge(self: *Self, builder: *DependencyBuilder, parent: u32, child: Var) Allocator.Error!void {
    const index = try self.dependencyNode(builder, child);
    try builder.deps.nodes.items[index].parents.append(builder.alloc, parent);
    try builder.children.append(builder.scratch, index);
}

fn dependencyVars(self: *Self, builder: *DependencyBuilder, parent: u32, range: Var.SafeList.Range) Allocator.Error!void {
    for (self.source.sliceVars(range)) |child| try self.dependencyEdge(builder, parent, child);
}

fn ensureDependencies(self: *Self, declaration: types.NominalDecl.Idx) Allocator.Error!*const Dependencies {
    if (self.dependencies.get(declaration)) |existing| return existing;
    if (self.shared) |shared| {
        if (shared.isFinal(shared.owner, declaration)) {
            const graphs = shared.graphs;
            const deps = graphs.graphs.get(declaration) orelse computed: {
                const deps = try self.computeDependencies(graphs.arena.allocator(), declaration);
                try graphs.graphs.put(graphs.gpa, declaration, deps);
                break :computed deps;
            };
            try self.dependencies.put(self.gpa, declaration, deps);
            return deps;
        }
    }
    const deps = try self.computeDependencies(self.arena.allocator(), declaration);
    try self.dependencies.put(self.gpa, declaration, deps);
    return deps;
}

fn computeDependencies(self: *Self, alloc: Allocator, declaration: types.NominalDecl.Idx) Allocator.Error!*Dependencies {
    const deps = try alloc.create(Dependencies);
    deps.* = .{};
    const decl = self.source.getNominalDecl(declaration);
    const formal_vars = self.source.sliceVars(decl.formals);
    const scratch = self.arena.allocator();
    const formals = try scratch.alloc(Formal, formal_vars.len);
    for (formal_vars, formals) |formal, *dest| dest.* = .init(self.source.resolveVar(formal));
    var builder: DependencyBuilder = .{ .alloc = alloc, .scratch = scratch, .deps = deps, .formal_count = formals.len };
    const backing = try self.dependencyNode(&builder, decl.backing);
    std.debug.assert(backing == backing_dependency);
    var pending: std.ArrayList(u32) = .empty;
    defer pending.deinit(self.gpa);
    var offset: u32 = 0;
    while (offset < deps.nodes.items.len) : (offset += 1) {
        const resolved = self.source.resolveVar(deps.nodes.items[offset].root);
        var substituted: ?u32 = null;
        for (formals, 0..) |formal, formal_index| {
            if (formal.matches(resolved)) {
                deps.nodes.items[offset].formals[formal_index] = true;
                deps.nodes.items[offset].source_formals[formal_index] = true;
                if (substituted == null) substituted = @intCast(formal_index);
            }
        }
        if (substituted != null) {
            deps.nodes.items[offset].formal = substituted;
            try pending.append(self.gpa, offset);
            continue;
        }
        // Children are recorded in the order `projectContent` views them.
        builder.children.clearRetainingCapacity();
        switch (resolved.desc.content) {
            .flex, .rigid => {
                // Unknowns belong to an application, not just their template
                // root. Retain the entire application scope even when their
                // descriptor has no structural references to its formals.
                @memset(deps.nodes.items[offset].formals, true);
                try pending.append(self.gpa, offset);
            },
            .field_presence, .err => {},
            .alias => |alias| try self.dependencyVars(&builder, offset, alias.vars.nonempty),
            .structure => |flat| switch (flat) {
                .empty_record, .empty_tag_union => {},
                .tuple => |tuple| try self.dependencyVars(&builder, offset, tuple.elems),
                .nominal_type => |nominal| try self.dependencyVars(&builder, offset, nominal.args),
                .record => |record| {
                    for (self.source.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                        try self.dependencyEdge(&builder, offset, presence.typeVar());
                        if (presence.presenceVar()) |p| try self.dependencyEdge(&builder, offset, p);
                    }
                    try self.dependencyEdge(&builder, offset, record.ext);
                },
                .tag_union => |tag_union| {
                    for (self.source.getTagsSlice(tag_union.tags).items(.args)) |args| {
                        try self.dependencyVars(&builder, offset, args);
                    }
                    try self.dependencyEdge(&builder, offset, tag_union.ext);
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    try self.dependencyVars(&builder, offset, func.args);
                    try self.dependencyEdge(&builder, offset, func.ret);
                    try self.dependencyVars(&builder, offset, func.effect_deps);
                },
            },
        }
        deps.nodes.items[offset].children = try alloc.dupe(u32, builder.children.items);
    }
    // Monotone dependency sets converge on cyclic templates as well as DAGs.
    // Each node is queued only when its finite dependency set grows.
    while (pending.pop()) |child_index| {
        const child = deps.nodes.items[child_index];
        for (child.parents.items) |parent_index| {
            const parent = &deps.nodes.items[parent_index];
            var changed = false;
            for (child.formals, parent.formals) |needed, *known| {
                if (needed and !known.*) {
                    known.* = true;
                    changed = true;
                }
            }
            for (child.source_formals, parent.source_formals) |needed, *known| {
                if (needed and !known.*) {
                    known.* = true;
                    changed = true;
                }
            }
            if (changed) try pending.append(self.gpa, parent_index);
        }
    }
    return deps;
}

fn projectVars(self: *Self, range: Var.SafeList.Range, projection: *Projection) Allocator.Error!Var.SafeList.Range {
    const source = self.source.sliceVars(range);
    const projected = try self.arena.allocator().alloc(Var, source.len);
    for (source, projected) |var_, *dest| dest.* = try self.viewChild(projection, var_);
    const start = try addOffset(@intCast(self.vars.items.len), self.vars_base);
    try self.vars.append(self.gpa, projected);
    return .{ .start = @fromBackingInt(start), .count = @intCast(source.len) };
}

fn addOffset(index: u32, offset: u32) Allocator.Error!u32 {
    return std.math.add(u32, index, offset) catch error.OutOfMemory;
}

fn projectContent(self: *Self, content: types.Content, projection: *Projection) Allocator.Error!types.Content {
    return switch (content) {
        // Constraints are not traversed here. Source spans preserve their
        // empty/nonempty distinction for unknown-payload classification.
        .flex, .rigid, .field_presence, .err => content,
        .alias => |alias| blk: {
            var projected = alias;
            projected.vars.nonempty = try self.projectVars(alias.vars.nonempty, projection);
            break :blk .{ .alias = projected };
        },
        .structure => |flat| .{ .structure = switch (flat) {
            .empty_record, .empty_tag_union => flat,
            .nominal_type => |nominal| blk: {
                var projected = nominal;
                projected.args = try self.projectVars(nominal.args, projection);
                break :blk .{ .nominal_type = projected };
            },
            .tuple => |tuple| .{ .tuple = .{ .elems = try self.projectVars(tuple.elems, projection) } },
            .record => |record| blk: {
                const source = self.source.getRecordFieldsSlice(record.fields);
                const projected = try self.arena.allocator().alloc(types.RecordField.Presence, record.fields.count);
                for (source.items(.presence), projected) |presence, *dest| {
                    const value = try self.viewChild(projection, presence.typeVar());
                    dest.* = if (presence.presenceVar()) |p|
                        .unknown(try self.viewChild(projection, p), value)
                    else
                        .required(value);
                }
                const fields: types.RecordField.SafeMultiList.Range = .{
                    .start = @fromBackingInt(try addOffset(@intCast(self.fields.items.len), self.fields_base)),
                    .count = record.fields.count,
                };
                try self.fields.append(self.gpa, .{ .name = source.items(.name), .presence = projected });
                break :blk .{ .record = .{ .fields = fields, .ext = try self.viewChild(projection, record.ext) } };
            },
            .tag_union => |tag_union| blk: {
                const source = self.source.getTagsSlice(tag_union.tags);
                const projected = try self.arena.allocator().alloc(Var.SafeList.Range, tag_union.tags.count);
                for (source.items(.args), projected) |args, *dest| {
                    dest.* = try self.projectVars(args, projection);
                }
                const tags: types.Tag.SafeMultiList.Range = .{
                    .start = @fromBackingInt(try addOffset(@intCast(self.tags.items.len), self.tags_base)),
                    .count = tag_union.tags.count,
                };
                try self.tags.append(self.gpa, .{ .name = source.items(.name), .args = projected });
                break :blk .{ .tag_union = .{ .tags = tags, .ext = try self.viewChild(projection, tag_union.ext) } };
            },
            inline .fn_pure, .fn_effectful, .fn_unbound => |func, tag| blk: {
                const projected: @TypeOf(func) = .{
                    .args = try self.projectVars(func.args, projection),
                    .ret = try self.viewChild(projection, func.ret),
                    .effect_deps = try self.projectVars(func.effect_deps, projection),
                };
                break :blk @unionInit(@TypeOf(flat), @tagName(tag), projected);
            },
        } },
    };
}

/// Read source or projected variable spans through the same view boundary.
pub fn sliceVars(self: *const Self, range: Var.SafeList.Range) []const Var {
    if (range.count == 0) return &.{};
    if (@backingInt(range.start) < self.vars_base) return self.source.sliceVars(range);
    const span = self.vars.items[@backingInt(range.start) - self.vars_base];
    std.debug.assert(span.len == range.count);
    return span;
}
/// Read a variable without exposing which store owns the span.
pub fn getVarAt(self: *const Self, range: Var.SafeList.Range, offset: u32) Var {
    return self.sliceVars(range)[offset];
}
/// Read nominal arguments with their projected substitution identities.
pub fn sliceNominalArgs(self: *const Self, nominal: types.NominalType) []const Var {
    return self.sliceVars(nominal.args);
}
/// Preserve view identity when following an alias backing.
pub fn getAliasBackingVar(self: *const Self, alias: types.Alias) Var {
    return self.sliceVars(alias.vars.nonempty)[0];
}
/// Read tag rows without exposing source versus projected ownership.
pub fn getTagsSlice(self: *const Self, range: types.Tag.SafeMultiList.Range) TagSlice {
    if (range.count == 0) return .{ .name = &.{}, .args = &.{} };
    if (@backingInt(range.start) < self.tags_base) {
        const source = self.source.getTagsSlice(range);
        return .{ .name = source.items(.name), .args = source.items(.args) };
    }
    const span = self.tags.items[@backingInt(range.start) - self.tags_base];
    std.debug.assert(span.name.len == range.count);
    return span;
}
/// Read one tag while preserving its projected payload span.
pub fn getTagAt(self: *const Self, range: types.Tag.SafeMultiList.Range, offset: u32) types.Tag {
    const slice = self.getTagsSlice(range);
    return .{ .name = slice.items(.name)[offset], .args = slice.items(.args)[offset] };
}
/// Read fields with presence evidence retained by the view.
pub fn getRecordFieldsSlice(self: *const Self, range: types.RecordField.SafeMultiList.Range) FieldSlice {
    if (range.count == 0) return .{ .name = &.{}, .presence = &.{} };
    if (@backingInt(range.start) < self.fields_base) {
        const source = self.source.getRecordFieldsSlice(range);
        return .{ .name = source.items(.name), .presence = source.items(.presence) };
    }
    const span = self.fields.items[@backingInt(range.start) - self.fields_base];
    std.debug.assert(span.name.len == range.count);
    return span;
}
/// Read one field without discarding projected presence evidence.
pub fn getRecordFieldAt(self: *const Self, range: types.RecordField.SafeMultiList.Range, offset: u32) types.RecordField {
    const slice = self.getRecordFieldsSlice(range);
    return .{ .name = slice.items(.name)[offset], .presence = slice.items(.presence)[offset] };
}

fn optVarsEqual(a: []const OptVar, b: []const OptVar) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left != right) return false;
    }
    return true;
}
