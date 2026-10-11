//! Read-only, scoped nominal substitution for exhaustiveness analysis.
//!
//! The reader has its own ID namespace: source IDs denote actual solver roots;
//! IDs above the source's frozen length denote (template root, environment)
//! views. Although descriptor payloads use Var's compact representation, view
//! IDs MUST NOT be passed to the solver. `sourceVar` is the only export route.
//! Opening a nested declaration replaces, rather than extends, its formal scope.
//! Lazy descriptors and arena-backed child spans stay valid across nested reads.
//! No solver variables, redirects, or descriptors change.

const std = @import("std");
const types = @import("types");
const Var = types.Var;
const Allocator = std.mem.Allocator;
const Self = @This();

source: *const types.Store,
gpa: Allocator,
// Stable storage is part of this reader's representation, not merely scratch.
// Do not use SingleThreadArena here: its allocation-debug pass-through mode
// disables the bulk ownership these immutable spans and dependency graphs need.
arena: std.heap.ArenaAllocator,
source_len: u32,
vars_base: u32,
tags_base: u32,
fields_base: u32,
nodes: std.ArrayList(Node) = .empty,
node_index: std.AutoHashMapUnmanaged(NodeKey, Var) = .empty,
environments: std.ArrayList(Environment) = .empty,
environment_index: std.HashMapUnmanaged(Environment, u32, EnvironmentContext, std.hash_map.default_max_load_percentage) = .empty,
dependencies: std.AutoHashMapUnmanaged(types.NominalDecl.Idx, *Dependencies) = .empty,
argument_scratch: std.ArrayList(Var) = .empty,
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

const NodeKey = struct { root: Var, environment: u32 };
const Node = struct { key: NodeKey, descriptor: ?types.Descriptor = null };
const Environment = struct { declaration: types.NominalDecl.Idx, args: []const Var };
const EnvironmentContext = struct {
    pub fn hash(_: @This(), env: Environment) u64 {
        var h = std.hash.Wyhash.init(0);
        std.hash.autoHash(&h, env.declaration);
        for (env.args) |arg| std.hash.autoHash(&h, arg);
        return h.final();
    }
    pub fn eql(_: @This(), a: Environment, b: Environment) bool {
        return a.declaration == b.declaration and varsEqual(a.args, b.args);
    }
};
pub const Resolved = struct { var_: Var, desc: types.Descriptor };

/// Exact substitution and unknown-owner dependencies, propagated backwards
/// through template edges.
/// Masking irrelevant arguments is essential for finite closed-argument resets
/// in otherwise recursive declarations; it is not decided by guessing from arity or groundness.
const Dependencies = struct {
    indices: std.AutoHashMapUnmanaged(Var, u32) = .empty,
    nodes: std.ArrayList(DependencyNode) = .empty,
};
const DependencyNode = struct {
    root: Var,
    formals: []bool,
    parents: std.ArrayList(u32) = .empty,
};
const absent_argument: Var = @fromBackingInt(std.math.maxInt(u32));

pub fn init(gpa: Allocator, source: *const types.Store) Self {
    return .{
        .source = source,
        .gpa = gpa,
        .arena = std.heap.ArenaAllocator.init(gpa),
        .source_len = @intCast(source.len()),
        .vars_base = @intCast(source.vars.len()),
        .tags_base = source.tags.len(),
        .fields_base = source.record_fields.len(),
    };
}

pub fn deinit(self: *Self) void {
    std.debug.assert(self.source.len() == self.source_len);
    self.node_index.deinit(self.gpa);
    self.environment_index.deinit(self.gpa);
    self.dependencies.deinit(self.gpa);
    self.nodes.deinit(self.gpa);
    self.environments.deinit(self.gpa);
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
    var descriptor = self.source.resolveVar(node.key.root).desc;
    descriptor.content = try self.projectContent(descriptor.content, node.key.environment);
    // Projecting children can grow nodes; never retain a pointer into it.
    self.nodes.items[index].descriptor = descriptor;
    return .{ .var_ = view_id, .desc = descriptor };
}

/// Actuals are already in their caller's scope. Substitution returns them
/// directly and never interprets their rigid names in the callee's scope.
pub fn openNominalBacking(self: *Self, nominal: types.NominalType) Allocator.Error!?Var {
    const declaration = self.source.lookupNominalDecl(nominal) orelse
        @panic("type view referenced a missing nominal declaration");
    const decl = self.source.getNominalDecl(declaration);
    if (!decl.isValid()) return null;
    try self.ensureDependencies(declaration);
    const args = self.sliceNominalArgs(nominal);
    std.debug.assert(args.len == decl.formals.count);
    try self.argument_scratch.resize(self.gpa, args.len);
    const canonical = self.argument_scratch.items;
    for (args, canonical) |arg, *dest| dest.* = self.root(arg);
    const environment = try self.internEnvironment(.{ .declaration = declaration, .args = canonical });
    return try self.view(decl.backing, environment);
}

fn internEnvironment(self: *Self, environment: Environment) Allocator.Error!u32 {
    if (self.environment_index.get(environment)) |existing| return existing;
    const owned = Environment{
        .declaration = environment.declaration,
        .args = try self.arena.allocator().dupe(Var, environment.args),
    };
    const index: u32 = @intCast(self.environments.items.len);
    try self.environments.append(self.gpa, owned);
    errdefer _ = self.environments.pop();
    try self.environment_index.putNoClobber(self.gpa, owned, index);
    return index;
}

fn view(self: *Self, source_var: Var, environment: u32) Allocator.Error!Var {
    std.debug.assert(@backingInt(source_var) < self.source_len);
    const resolved = self.source.resolveVar(source_var);
    const env = self.environments.items[environment];
    const decl = self.source.getNominalDecl(env.declaration);
    for (self.source.sliceVars(decl.formals), env.args) |formal, actual| {
        const f = self.source.resolveVar(formal);
        if (matchesFormal(resolved, f)) {
            std.debug.assert(actual != absent_argument);
            return actual;
        }
    }
    const deps = self.dependencies.get(env.declaration).?;
    const required = deps.nodes.items[deps.indices.get(resolved.var_).?].formals;
    var normalized = environment;
    for (env.args, required) |actual, needed| {
        if (!needed and actual != absent_argument) {
            try self.argument_scratch.resize(self.gpa, env.args.len);
            for (env.args, required, self.argument_scratch.items) |arg, used, *dest| {
                dest.* = if (used) arg else absent_argument;
            }
            normalized = try self.internEnvironment(.{ .declaration = env.declaration, .args = self.argument_scratch.items });
            break;
        }
    }
    const key = NodeKey{ .root = resolved.var_, .environment = normalized };
    if (self.node_index.get(key)) |existing| return existing;
    const index = try addOffset(self.source_len, @intCast(self.nodes.items.len));
    // All-ones Var is RecordField.Presence's absent-presence sentinel.
    if (index == std.math.maxInt(u32)) return error.OutOfMemory;
    const result: Var = @fromBackingInt(index);
    try self.nodes.append(self.gpa, .{ .key = key });
    errdefer _ = self.nodes.pop();
    try self.node_index.putNoClobber(self.gpa, key, result);
    return result;
}

fn matchesFormal(value: types.ResolvedVarDesc, formal: types.ResolvedVarDesc) bool {
    return value.var_ == formal.var_ or
        (value.desc.content == .rigid and formal.desc.content == .rigid and
            value.desc.content.rigid.name.eql(formal.desc.content.rigid.name));
}

fn dependencyNode(self: *Self, deps: *Dependencies, var_: Var, formal_count: usize) Allocator.Error!u32 {
    const root_var = self.source.resolveVar(var_).var_;
    if (deps.indices.get(root_var)) |index| return index;
    const alloc = self.arena.allocator();
    const index: u32 = @intCast(deps.nodes.items.len);
    const required = try alloc.alloc(bool, formal_count);
    @memset(required, false);
    try deps.nodes.append(alloc, .{ .root = root_var, .formals = required });
    try deps.indices.put(alloc, root_var, index);
    return index;
}

fn dependencyEdge(self: *Self, deps: *Dependencies, parent: u32, child: Var, count: usize) Allocator.Error!void {
    const index = try self.dependencyNode(deps, child, count);
    try deps.nodes.items[index].parents.append(self.arena.allocator(), parent);
}

fn dependencyVars(self: *Self, deps: *Dependencies, parent: u32, range: Var.SafeList.Range, count: usize) Allocator.Error!void {
    for (self.source.sliceVars(range)) |child| try self.dependencyEdge(deps, parent, child, count);
}

fn ensureDependencies(self: *Self, declaration: types.NominalDecl.Idx) Allocator.Error!void {
    if (self.dependencies.contains(declaration)) return;
    const alloc = self.arena.allocator();
    const deps = try alloc.create(Dependencies);
    deps.* = .{};
    const decl = self.source.getNominalDecl(declaration);
    const formals = self.source.sliceVars(decl.formals);
    _ = try self.dependencyNode(deps, decl.backing, formals.len);
    var pending: std.ArrayList(u32) = .empty;
    var offset: u32 = 0;
    while (offset < deps.nodes.items.len) : (offset += 1) {
        const resolved = self.source.resolveVar(deps.nodes.items[offset].root);
        var substituted = false;
        for (formals, 0..) |formal, formal_index| {
            if (matchesFormal(resolved, self.source.resolveVar(formal))) {
                deps.nodes.items[offset].formals[formal_index] = true;
                substituted = true;
            }
        }
        if (substituted) {
            try pending.append(alloc, offset);
            continue;
        }
        switch (resolved.desc.content) {
            .flex, .rigid => {
                // Unknowns belong to an application, not just their template
                // root. Retain the entire application scope even when their
                // descriptor has no structural references to its formals.
                @memset(deps.nodes.items[offset].formals, true);
                try pending.append(alloc, offset);
            },
            .field_presence, .err => {},
            .alias => |alias| try self.dependencyVars(deps, offset, alias.vars.nonempty, formals.len),
            .structure => |flat| switch (flat) {
                .empty_record, .empty_tag_union => {},
                .tuple => |tuple| try self.dependencyVars(deps, offset, tuple.elems, formals.len),
                .nominal_type => |nominal| try self.dependencyVars(deps, offset, nominal.args, formals.len),
                .record => |record| {
                    try self.dependencyEdge(deps, offset, record.ext, formals.len);
                    for (self.source.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                        try self.dependencyEdge(deps, offset, presence.typeVar(), formals.len);
                        if (presence.presenceVar()) |p| try self.dependencyEdge(deps, offset, p, formals.len);
                    }
                },
                .tag_union => |tag_union| {
                    try self.dependencyEdge(deps, offset, tag_union.ext, formals.len);
                    for (self.source.getTagsSlice(tag_union.tags).items(.args)) |args| {
                        try self.dependencyVars(deps, offset, args, formals.len);
                    }
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    try self.dependencyVars(deps, offset, func.args, formals.len);
                    try self.dependencyVars(deps, offset, func.effect_deps, formals.len);
                    try self.dependencyEdge(deps, offset, func.ret, formals.len);
                },
            },
        }
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
            if (changed) try pending.append(alloc, parent_index);
        }
    }
    try self.dependencies.put(self.gpa, declaration, deps);
}

fn projectVars(self: *Self, range: Var.SafeList.Range, environment: u32) Allocator.Error!Var.SafeList.Range {
    const source = self.source.sliceVars(range);
    const projected = try self.arena.allocator().alloc(Var, source.len);
    for (source, projected) |var_, *dest| dest.* = try self.view(var_, environment);
    const start = try addOffset(@intCast(self.vars.items.len), self.vars_base);
    try self.vars.append(self.gpa, projected);
    return .{ .start = @fromBackingInt(start), .count = @intCast(source.len) };
}

fn addOffset(index: u32, offset: u32) Allocator.Error!u32 {
    return std.math.add(u32, index, offset) catch error.OutOfMemory;
}

fn projectContent(self: *Self, content: types.Content, environment: u32) Allocator.Error!types.Content {
    return switch (content) {
        // Constraints are not traversed here. Source spans preserve their
        // empty/nonempty distinction for unknown-payload classification.
        .flex, .rigid, .field_presence, .err => content,
        .alias => |alias| blk: {
            var projected = alias;
            projected.vars.nonempty = try self.projectVars(alias.vars.nonempty, environment);
            break :blk .{ .alias = projected };
        },
        .structure => |flat| .{ .structure = switch (flat) {
            .empty_record, .empty_tag_union => flat,
            .nominal_type => |nominal| blk: {
                var projected = nominal;
                projected.args = try self.projectVars(nominal.args, environment);
                break :blk .{ .nominal_type = projected };
            },
            .tuple => |tuple| .{ .tuple = .{ .elems = try self.projectVars(tuple.elems, environment) } },
            .record => |record| blk: {
                const source = self.source.getRecordFieldsSlice(record.fields);
                const projected = try self.arena.allocator().alloc(types.RecordField.Presence, record.fields.count);
                for (source.items(.presence), projected) |presence, *dest| {
                    const value = try self.view(presence.typeVar(), environment);
                    dest.* = if (presence.presenceVar()) |p|
                        .unknown(try self.view(p, environment), value)
                    else
                        .required(value);
                }
                const fields: types.RecordField.SafeMultiList.Range = .{
                    .start = @fromBackingInt(try addOffset(@intCast(self.fields.items.len), self.fields_base)),
                    .count = record.fields.count,
                };
                try self.fields.append(self.gpa, .{ .name = source.items(.name), .presence = projected });
                break :blk .{ .record = .{ .fields = fields, .ext = try self.view(record.ext, environment) } };
            },
            .tag_union => |tag_union| blk: {
                const source = self.source.getTagsSlice(tag_union.tags);
                const projected = try self.arena.allocator().alloc(Var.SafeList.Range, tag_union.tags.count);
                for (source.items(.args), projected) |args, *dest| {
                    dest.* = try self.projectVars(args, environment);
                }
                const tags: types.Tag.SafeMultiList.Range = .{
                    .start = @fromBackingInt(try addOffset(@intCast(self.tags.items.len), self.tags_base)),
                    .count = tag_union.tags.count,
                };
                try self.tags.append(self.gpa, .{ .name = source.items(.name), .args = projected });
                break :blk .{ .tag_union = .{ .tags = tags, .ext = try self.view(tag_union.ext, environment) } };
            },
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                const projected: @TypeOf(func) = .{
                    .args = try self.projectVars(func.args, environment),
                    .ret = try self.view(func.ret, environment),
                    .effect_deps = try self.projectVars(func.effect_deps, environment),
                };
                break :blk switch (flat) {
                    .fn_pure => .{ .fn_pure = projected },
                    .fn_effectful => .{ .fn_effectful = projected },
                    .fn_unbound => .{ .fn_unbound = projected },
                    .record, .tuple, .nominal_type, .empty_record, .tag_union, .empty_tag_union => unreachable,
                };
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

/// Whether two type-variable lists name the same variables in the same order.
fn varsEqual(a: []const Var, b: []const Var) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left != right) return false;
    }
    return true;
}
