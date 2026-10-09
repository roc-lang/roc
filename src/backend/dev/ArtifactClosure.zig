//! Closed native artifact graphs shared by pack publication and CTFE binding.
//!
//! A serving entry owns its transitive code/data closure; unrelated offers must
//! not add source-context requirements or claim complete-module availability.

const std = @import("std");
const lir = @import("lir");
const ProcArtifact = @import("ProcArtifact.zig");
const Allocator = std.mem.Allocator;
const Self = @This();

allocator: Allocator,
set: *const ProcArtifact.Set,
procs: std.AutoHashMap(lir.ProcIdentity, u32),
thunks: std.AutoHashMap(lir.ProcIdentity, u32),
helpers: std.StringHashMap(u32),
/// One resolution per symbolic edge, shared by admission and placement.
symbolic_offsets: []usize = &.{},
symbolic_targets: []?u32 = &.{},
symbolic_lookups: usize = 0,
/// Same-kind definitions whose symbolic name cannot prove one callable contract.
ambiguous_definitions: std.AutoHashMap(u32, void),
/// Preparation-only dependencies from an alias to its selected definition.
aliases: []const u32 = &.{},
seen: std.AutoHashMap(u32, void),
order: std.ArrayList(u32) = .empty,
complete: bool = true,

pub fn init(allocator: Allocator, set: *const ProcArtifact.Set) Allocator.Error!Self {
    var self = Self{
        .allocator = allocator,
        .set = set,
        .procs = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator),
        .thunks = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator),
        .helpers = std.StringHashMap(u32).init(allocator),
        .ambiguous_definitions = std.AutoHashMap(u32, void).init(allocator),
        .seen = std.AutoHashMap(u32, void).init(allocator),
    };
    errdefer self.deinit();
    for (set.artifacts, 0..) |artifact, index| {
        switch (artifact.kind) {
            .proc => |identity| try self.indexDefinition(&self.procs, identity, @intCast(index)),
            .boxy_thunk => |identity| try self.indexDefinition(&self.thunks, identity, @intCast(index)),
            .rc_helper => |name| try self.indexDefinition(&self.helpers, name, @intCast(index)),
            .entrypoint, .message_pool_run, .branch_island => {},
        }
    }
    self.symbolic_offsets = try allocator.alloc(usize, set.artifacts.len + 1);
    self.symbolic_offsets[0] = 0;
    for (set.artifacts, 0..) |artifact, index| {
        self.symbolic_offsets[index + 1] = self.symbolic_offsets[index] + artifact.symbolic_refs.len;
    }
    self.symbolic_targets = try allocator.alloc(?u32, self.symbolic_offsets[set.artifacts.len]);
    for (set.artifacts, 0..) |artifact, index| {
        for (artifact.symbolic_refs, self.symbolic_targets[self.symbolic_offsets[index]..self.symbolic_offsets[index + 1]]) |ref, *target| {
            self.symbolic_lookups += 1;
            target.* = switch (ref.target) {
                .proc => |identity| self.procs.get(identity),
                .boxy_thunk => |identity| self.thunks.get(identity),
                .rc_helper => |name| self.helpers.get(name),
            };
            if (target.*) |definition| {
                if (self.ambiguous_definitions.contains(definition)) target.* = null;
            }
        }
    }
    return self;
}

fn indexDefinition(self: *Self, map: anytype, key: anytype, index: u32) Allocator.Error!void {
    const entry = try map.getOrPut(key);
    if (!entry.found_existing) {
        entry.value_ptr.* = index;
        return;
    }
    const previous = entry.value_ptr.*;
    const selected = if (providerRank(self.set.artifacts[index]) > providerRank(self.set.artifacts[previous])) index else previous;
    const other = if (selected == index) previous else index;
    if (self.ambiguous_definitions.contains(previous) or
        !compatibleDefinition(self.set.artifacts[other], self.set.artifacts[selected]))
    {
        try self.ambiguous_definitions.put(selected, {});
    }
    entry.value_ptr.* = selected;
}

pub fn deinit(self: *Self) void {
    self.ambiguous_definitions.deinit();
    self.allocator.free(self.symbolic_targets);
    self.allocator.free(self.symbolic_offsets);
    self.order.deinit(self.allocator);
    self.seen.deinit();
    self.helpers.deinit();
    self.thunks.deinit();
    self.procs.deinit();
}

/// Null preserves an external symbolic reference; it is never guessed to name
/// a local definition. Serving-closure admission rejects unresolved dependencies.
pub fn symbolicTargets(self: *const Self, artifact: u32) []const ?u32 {
    return self.symbolic_targets[self.symbolic_offsets[artifact]..self.symbolic_offsets[artifact + 1]];
}

/// Artifact indices valid until the next query. Missing symbolic definitions
/// make this an explicitly incomplete closure, not an external-code guess.
pub fn of(self: *Self, root: u32) Allocator.Error![]const u32 {
    return self.ofMany(&.{root});
}

pub fn ofMany(self: *Self, roots: []const u32) Allocator.Error![]const u32 {
    self.seen.clearRetainingCapacity();
    self.order.clearRetainingCapacity();
    self.complete = true;
    for (roots) |root| try self.visit(root);
    var cursor: usize = 0;
    while (cursor < self.order.items.len) : (cursor += 1) {
        const node = self.order.items[cursor];
        const artifact = self.set.artifacts[node];
        for (artifact.refs) |ref| try self.visit(ref.target);
        for (self.symbolicTargets(node)) |target| {
            if (target) |index| {
                try self.visit(index);
            } else {
                self.complete = false;
            }
        }
    }
    return self.order.items;
}

fn visit(self: *Self, index: u32) Allocator.Error!void {
    if ((try self.seen.getOrPut(index)).found_existing) return;
    try self.order.append(self.allocator, index);
}

/// One immutable condensation of the exact serving graph. Components are
/// numbered caller-before-callee, so consumers union callee facts once in
/// reverse order. Recursive procedures share a component, not provisional facts.
pub const Components = struct {
    arena: std.heap.ArenaAllocator,
    nodes: []const u32,
    callees: []const []const u32,

    pub fn deinit(self: *Components) void {
        self.arena.deinit();
    }
};

pub fn components(self: *const Self) Allocator.Error!Components {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    const count = self.set.artifacts.len;
    const callers = try a.alloc(std.ArrayList(u32), count);
    @memset(callers, .empty);
    const callees = try a.alloc(std.ArrayList(u32), count);
    @memset(callees, .empty);
    for (self.set.artifacts, 0..) |artifact, index| {
        for (artifact.refs) |ref| {
            try callers[ref.target].append(a, @intCast(index));
            try callees[index].append(a, ref.target);
        }
        for (self.symbolicTargets(@intCast(index))) |target| {
            if (target) |callee| {
                try callers[callee].append(a, @intCast(index));
                try callees[index].append(a, callee);
            }
        }
    }
    const visited = try a.alloc(bool, count);
    @memset(visited, false);
    const Frame = struct { node: u32, next: usize = 0 };
    var frames = std.ArrayList(Frame).empty;
    var order = std.ArrayList(u32).empty;
    for (0..count) |start| {
        if (visited[start]) continue;
        visited[start] = true;
        try frames.append(a, .{ .node = @intCast(start) });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next == callees[frame.node].items.len) {
                try order.append(a, frame.node);
                _ = frames.pop();
                continue;
            }
            const callee = callees[frame.node].items[frame.next];
            frame.next += 1;
            if (visited[callee]) continue;
            visited[callee] = true;
            try frames.append(a, .{ .node = callee });
        }
    }
    const nodes = try a.alloc(u32, count);
    @memset(nodes, std.math.maxInt(u32));
    var component_count: u32 = 0;
    var pending = std.ArrayList(u32).empty;
    while (order.pop()) |start| {
        if (nodes[start] != std.math.maxInt(u32)) continue;
        nodes[start] = component_count;
        try pending.append(a, start);
        while (pending.pop()) |node| for (callers[node].items) |caller| {
            if (nodes[caller] != std.math.maxInt(u32)) continue;
            nodes[caller] = component_count;
            try pending.append(a, caller);
        };
        component_count += 1;
    }
    const edges = try a.alloc(std.AutoHashMap(u32, void), component_count);
    for (edges) |*set| set.* = std.AutoHashMap(u32, void).init(a);
    for (callees, 0..) |targets, node| {
        for (targets.items) |callee| {
            if (nodes[callee] != nodes[node]) try edges[nodes[node]].put(nodes[callee], {});
        }
    }
    const output = try a.alloc([]const u32, component_count);
    for (edges, output, 0..) |*set, *owned, component| {
        const targets = try a.alloc(u32, set.count());
        var iterator = set.keyIterator();
        var index: usize = 0;
        while (iterator.next()) |callee| : (index += 1) {
            std.debug.assert(callee.* > component);
            targets[index] = callee.*;
        }
        std.mem.sort(u32, targets, {}, std.sort.asc(u32));
        owned.* = targets;
    }
    return .{ .arena = arena, .nodes = nodes, .callees = output };
}

pub const ContextRequirements = struct {
    dependencies: @import("LirCodeGen.zig").FragmentContextDependencies = .{},
    /// Unknown producer facts or unresolved edges are never a neutral context.
    complete: bool = true,

    fn merge(self: *ContextRequirements, other: ContextRequirements) void {
        self.dependencies.merge(other.dependencies);
        self.complete = self.complete and other.complete;
    }
};

/// Producer emission facts flow from callees to callers. Unrelated siblings
/// cannot add requirements to a serving offer. The returned array is owned by
/// the graph allocator; it is session data, not a reconstructed pack contract.
pub fn contextRequirements(self: *const Self, groups: *const Components) Allocator.Error![]ContextRequirements {
    const a = self.allocator;
    const summaries = try a.alloc(ContextRequirements, groups.callees.len);
    defer a.free(summaries);
    @memset(summaries, .{});
    for (self.set.artifacts, groups.nodes, 0..) |artifact, component, node| {
        var direct = ContextRequirements{
            .dependencies = artifact.context_dependencies orelse .{},
            .complete = artifact.context_complete and artifact.context_dependencies != null and artifact.context_contract != null,
        };
        for (self.symbolicTargets(@intCast(node))) |target| {
            if (target == null) direct.complete = false;
        }
        summaries[component].merge(direct);
    }
    var cursor = summaries.len;
    while (cursor != 0) {
        cursor -= 1;
        for (groups.callees[cursor]) |callee| summaries[cursor].merge(summaries[callee]);
    }
    const output = try a.alloc(ContextRequirements, groups.nodes.len);
    for (groups.nodes, output) |component, *summary| summary.* = summaries[component];
    return output;
}

test "artifact context requirements combine recursion and flow only to callers" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testContextRequirements, .{});
}

fn testContextRequirements(allocator: Allocator) !void {
    var artifacts: [6]ProcArtifact.Artifact = undefined;
    for (&artifacts, 0..) |*artifact, index| artifact.* = .{
        .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index)) },
        .code = "",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .context_contract = .{
            .target = .x64musl,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
            .dict_seed_mode = .comptime_zero,
            .hooks_enabled = true,
            .initialize_boxy_runtime = false,
            .static_data_readonly = false,
        },
        .context_dependencies = .{},
    };
    artifacts[0].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    artifacts[0].context_dependencies = .{ .dict_seed = true };
    artifacts[1].refs = &.{
        .{ .site = 0, .form = .call, .target = 0, .delta = 0 },
        .{ .site = 0, .form = .call, .target = 2, .delta = 0 },
    };
    artifacts[2].context_dependencies = .{ .comptime_hooks = true };
    artifacts[3].context_dependencies = .{ .boxy_runtime = true };
    artifacts[4].context_dependencies = null;
    artifacts[5].refs = &.{.{ .site = 0, .form = .call, .target = 4, .delta = 0 }};
    const set = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &artifacts };
    var graph = try Self.init(allocator, &set);
    defer graph.deinit();
    var groups = try graph.components();
    defer groups.deinit();
    const summaries = try graph.contextRequirements(&groups);
    defer allocator.free(summaries);
    try std.testing.expectEqual(groups.nodes[0], groups.nodes[1]);
    try std.testing.expectEqualDeep(summaries[0], summaries[1]);
    try std.testing.expect(summaries[0].dependencies.dict_seed);
    try std.testing.expect(summaries[0].dependencies.comptime_hooks);
    try std.testing.expect(!summaries[0].dependencies.boxy_runtime);
    try std.testing.expect(summaries[0].complete);
    try std.testing.expect(!summaries[2].dependencies.dict_seed);
    try std.testing.expect(summaries[2].dependencies.comptime_hooks);
    try std.testing.expect(summaries[3].dependencies.boxy_runtime);
    try std.testing.expect(!summaries[3].dependencies.comptime_hooks);
    try std.testing.expect(!summaries[4].complete);
    try std.testing.expect(!summaries[5].complete);
}

/// Admit all roots in graph-proportional work. Invalid node facts propagate
/// backwards through exact dependencies; recursive closed graphs need no
/// provisional "hit" or repeated transitive walk.
pub fn admit(self: *const Self, context: *anyopaque, accepts: *const fn (*anyopaque, ProcArtifact.Artifact) bool) Allocator.Error![]bool {
    const allocator = self.allocator;
    const valid = try allocator.alloc(bool, self.set.artifacts.len);
    errdefer allocator.free(valid);
    for (self.set.artifacts, 0..) |artifact, index| valid[index] = accepts(context, artifact);
    try self.propagateInvalid(valid);
    return valid;
}

/// Propagate caller-supplied node facts over the same resolved edges used by
/// closure selection and placement.
pub fn propagateInvalid(self: *const Self, valid: []bool) Allocator.Error!void {
    const allocator = self.allocator;
    std.debug.assert(valid.len == self.set.artifacts.len);
    const Edge = struct { from: u32, to: u32 };
    var edges = std.ArrayList(Edge).empty;
    defer edges.deinit(allocator);
    for (self.set.artifacts, 0..) |artifact, index| {
        if (self.aliases.len != 0 and self.aliases[index] != index) {
            try edges.append(allocator, .{ .from = @intCast(index), .to = self.aliases[index] });
        }
        for (artifact.refs) |ref| try edges.append(allocator, .{ .from = @intCast(index), .to = ref.target });
        for (self.symbolicTargets(@intCast(index))) |target| {
            if (target) |to| {
                try edges.append(allocator, .{ .from = @intCast(index), .to = to });
            } else {
                valid[index] = false;
            }
        }
    }
    const offsets = try allocator.alloc(usize, valid.len + 1);
    defer allocator.free(offsets);
    @memset(offsets, 0);
    for (edges.items) |edge| offsets[edge.to + 1] += 1;
    for (1..offsets.len) |index| offsets[index] += offsets[index - 1];
    const cursors = try allocator.dupe(usize, offsets[0..valid.len]);
    defer allocator.free(cursors);
    const predecessors = try allocator.alloc(u32, edges.items.len);
    defer allocator.free(predecessors);
    for (edges.items) |edge| {
        predecessors[cursors[edge.to]] = edge.from;
        cursors[edge.to] += 1;
    }
    var invalid = std.ArrayList(u32).empty;
    defer invalid.deinit(allocator);
    for (valid, 0..) |accept, index| if (!accept) {
        try invalid.append(allocator, @intCast(index));
    };
    var cursor: usize = 0;
    while (cursor < invalid.items.len) : (cursor += 1) {
        const node = invalid.items[cursor];
        for (predecessors[offsets[node]..offsets[node + 1]]) |predecessor| {
            if (!valid[predecessor]) continue;
            valid[predecessor] = false;
            try invalid.append(allocator, predecessor);
        }
    }
}

pub const Selection = struct {
    set: ProcArtifact.Set,
    /// Borrowed from `set`'s arena.
    old_to_new: []const ?u32,

    pub fn deinit(self: *Selection) void {
        self.set.deinit();
    }
};

fn sameValue(comptime T: type, left: T, right: T) bool {
    return switch (@typeInfo(T)) {
        .pointer => |info| blk: {
            if (info.size != .slice) @compileError("persistent contracts cannot contain pointers");
            if (left.len != right.len) break :blk false;
            for (left, right) |a, b| if (!sameValue(info.child, a, b)) break :blk false;
            break :blk true;
        },
        .@"struct" => |info| blk: {
            inline for (info.fields) |field| {
                if (!sameValue(field.type, @field(left, field.name), @field(right, field.name))) break :blk false;
            }
            break :blk true;
        },
        .@"union" => |info| blk: {
            if (std.meta.activeTag(left) != std.meta.activeTag(right)) break :blk false;
            inline for (info.fields) |field| {
                if (std.mem.eql(u8, @tagName(left), field.name)) {
                    break :blk sameValue(field.type, @field(left, field.name), @field(right, field.name));
                }
            }
            unreachable;
        },
        .optional => |info| if (left) |a| if (right) |b| sameValue(info.child, a, b) else false else right == null,
        .array => |info| blk: {
            for (left, right) |a, b| if (!sameValue(info.child, a, b)) break :blk false;
            break :blk true;
        },
        else => std.meta.eql(left, right),
    };
}

/// Only a CTFE image may select an instrumented definition. Hooks are additive
/// observations there; ownership/ABI and every context-dependent emission
/// choice remain exact. Two instrumented providers must retain the same facts.
pub fn compatibleDefinition(original: ProcArtifact.Artifact, selected: ProcArtifact.Artifact) bool {
    const left = original.callable_contract orelse return false;
    const right = selected.callable_contract orelse return false;
    if (!std.mem.eql(u8, &left, &right)) return false;
    const a = original.context_contract orelse return false;
    const b = selected.context_contract orelse return false;
    const da = original.context_dependencies orelse return false;
    const db = selected.context_dependencies orelse return false;
    if (a.target != b.target or a.cpu_level != b.cpu_level or a.hot_reload != b.hot_reload) return false;
    if ((da.dict_seed or db.dict_seed) and a.dict_seed_mode != b.dict_seed_mode) return false;
    if ((da.static_data or db.static_data) and a.static_data_readonly != b.static_data_readonly) return false;
    if ((da.static_data or db.static_data) and da.static_data_access != db.static_data_access) return false;
    if ((da.boxy_runtime or db.boxy_runtime or da.boxy_runtime_entry or db.boxy_runtime_entry) and
        (a.default_platform_runtime != b.default_platform_runtime or a.initialize_boxy_runtime != b.initialize_boxy_runtime)) return false;
    if (original.domain == .ctfe) {
        if (selected.domain != .ctfe) return false;
        if (!sameValue([]const @import("CtfeContext.zig").Binding, original.context_bindings, selected.context_bindings)) return false;
    }
    return true;
}

/// A CTFE-only immutable image plan. Missing mappings are rejected offers,
/// established before a cache reader can authorize source-body elision.
pub const Canonical = struct {
    set: ProcArtifact.Set,
    source_offsets: []const u32,
    old_to_new: []const ?u32,
    /// Physical source-set provenance of each actually selected definition.
    new_to_source: []const u32,
    /// Transitive producer context facts for each physical serving definition.
    requirements: []const ContextRequirements,

    pub fn deinit(self: *Canonical) void {
        self.set.deinit();
    }
};

fn preferredRank(artifact: ProcArtifact.Artifact, preference: Preference) u2 {
    if (artifact.context_contract) |contract| {
        if (preference == .complete_runtime) {
            if (artifact.domain == .runtime and contract.static_data_readonly and !contract.hooks_enabled) return 3;
        } else {
            // Neutral CTFE definitions can still depend on zero dict seeds or
            // another definition's instrumented/static contract. Keep the
            // original CTFE namespace coherent, not only hook-bearing nodes.
            if (contract.hooks_enabled and !contract.static_data_readonly) return 3;
        }
    }
    return providerRank(artifact);
}

fn providerRank(artifact: ProcArtifact.Artifact) u2 {
    if (artifact.domain == .ctfe) return 2;
    // Neutral CTFE fragments do not displace a runtime-capable definition
    // merely because hook emission was enabled elsewhere in their producer.
    if (artifact.context_contract) |contract| {
        if (contract.static_data_readonly and !contract.hooks_enabled) return 1;
    }
    return 0;
}

pub fn canonicalize(
    allocator: Allocator,
    sources: []const *const ProcArtifact.Set,
    context: *anyopaque,
    accepts: *const fn (*anyopaque, ProcArtifact.Artifact) bool,
) Allocator.Error!Canonical {
    return canonicalizePreferred(allocator, sources, context, accepts, .ctfe);
}

pub const Preference = enum { ctfe, complete_runtime };

/// A shared runtime provision must not lose its independently valid serving
/// closure merely because instrumented siblings were also captured by checking.
pub fn canonicalizePreferred(
    allocator: Allocator,
    sources: []const *const ProcArtifact.Set,
    context: *anyopaque,
    accepts: *const fn (*anyopaque, ProcArtifact.Artifact) bool,
    preference: Preference,
) Allocator.Error!Canonical {
    // Preparation borrows payloads; only the accepted canonical graph is
    // deep-owned. Avoid cloning whole packs twice on every warm preparation.
    var combined = ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{},
    };
    defer combined.deinit();
    const a = combined.arena.allocator();
    var joined = std.ArrayList(ProcArtifact.Artifact).empty;
    for (sources) |source| {
        const base: u32 = @intCast(joined.items.len);
        for (source.artifacts) |original| {
            var artifact = original;
            const refs = try a.dupe(ProcArtifact.Reference, original.refs);
            for (refs) |*ref| ref.target += base;
            artifact.refs = refs;
            try joined.append(a, artifact);
        }
    }
    combined.artifacts = joined.items;
    const artifacts = @constCast(combined.artifacts);
    const valid = try a.alloc(bool, artifacts.len);
    @memset(valid, true);
    const offsets = try a.alloc(u32, sources.len + 1);
    offsets[0] = 0;
    // Resolve within each original namespace before sources are combined.
    // Otherwise a symbolic CTFE edge could acquire a runtime sibling's ABI.
    for (sources, 0..) |source, source_index| {
        offsets[source_index + 1] = offsets[source_index] + @as(u32, @intCast(source.artifacts.len));
        var local = try Self.init(allocator, source);
        defer local.deinit();
        for (source.artifacts, 0..) |original, index| {
            const artifact = &artifacts[offsets[source_index] + index];
            var refs = std.ArrayList(ProcArtifact.Reference).empty;
            try refs.appendSlice(a, artifact.refs);
            var external = std.ArrayList(ProcArtifact.SymbolicReference).empty;
            for (original.symbolic_refs, local.symbolicTargets(@intCast(index)), 0..) |ref, target, ref_index| {
                if (target) |node| {
                    try refs.append(a, .{
                        .site = ref.site,
                        .form = ref.form,
                        .target = offsets[source_index] + node,
                        .delta = source.artifacts[node].entry,
                        .veneer = ref.veneer,
                    });
                } else {
                    // Preparation borrows this name; selection deep-owns it.
                    try external.append(a, artifact.symbolic_refs[ref_index]);
                    valid[offsets[source_index] + index] = false;
                }
            }
            artifact.refs = try refs.toOwnedSlice(a);
            artifact.symbolic_refs = try external.toOwnedSlice(a);
        }
    }
    var graph = try Self.init(allocator, &combined);
    defer graph.deinit();
    for (artifacts, 0..) |artifact, index| valid[index] = valid[index] and accepts(context, artifact);
    // An unusable CTFE sibling must not displace an otherwise complete runtime
    // closure. Establish provider eligibility before choosing representatives.
    try graph.propagateInvalid(valid);
    // Preference is global over definitions, not dependent on root splice order.
    for (artifacts, 0..) |artifact, index| {
        if (!valid[index]) continue;
        const representative = switch (artifact.kind) {
            .proc => |identity| graph.procs.getPtr(identity).?,
            .boxy_thunk => |identity| graph.thunks.getPtr(identity).?,
            .rc_helper => |name| graph.helpers.getPtr(name).?,
            .entrypoint, .message_pool_run, .branch_island => continue,
        };
        if (!valid[representative.*] or preferredRank(artifact, preference) > preferredRank(artifacts[representative.*], preference)) representative.* = @intCast(index);
    }
    const aliases = try a.alloc(u32, artifacts.len);
    for (artifacts, 0..) |artifact, index| {
        aliases[index] = switch (artifact.kind) {
            .proc => |identity| graph.procs.get(identity).?,
            .boxy_thunk => |identity| graph.thunks.get(identity).?,
            .rc_helper => |name| graph.helpers.get(name).?,
            .entrypoint, .message_pool_run, .branch_island => @intCast(index),
        };
    }
    for (artifacts, 0..) |artifact, index| {
        valid[index] = valid[index] and
            (aliases[index] == index or compatibleDefinition(artifact, artifacts[aliases[index]]));
        for (artifact.refs) |ref| {
            if (aliases[ref.target] != ref.target and ref.delta != artifacts[ref.target].entry) valid[index] = false;
        }
    }
    graph.aliases = aliases;
    try graph.propagateInvalid(valid);
    const marked = try a.alloc(bool, artifacts.len);
    for (artifacts, 0..) |*artifact, index| {
        marked[index] = valid[index] and aliases[index] == index;
        if (!marked[index]) continue;
        const refs = @constCast(artifact.refs);
        for (refs) |*ref| {
            const target = aliases[ref.target];
            if (target != ref.target) ref.delta = artifacts[target].entry;
            ref.target = target;
        }
    }
    var selected = try selectMarked(allocator, &combined, marked);
    errdefer selected.deinit();
    const owned = selected.set.arena.allocator();
    const mapping = try owned.alloc(?u32, artifacts.len);
    for (mapping, 0..) |*mapped, index| mapped.* = if (valid[index]) selected.old_to_new[aliases[index]] else null;
    const owned_offsets = try owned.dupe(u32, offsets);
    const provenance = try owned.alloc(u32, selected.set.artifacts.len);
    for (sources, 0..) |_, source| {
        for (offsets[source]..offsets[source + 1]) |original| {
            if (selected.old_to_new[original]) |index| provenance[index] = @intCast(source);
        }
    }
    var selected_graph = try Self.init(allocator, &selected.set);
    defer selected_graph.deinit();
    var groups = try selected_graph.components();
    defer groups.deinit();
    const requirements = try selected_graph.contextRequirements(&groups);
    defer allocator.free(requirements);
    return .{
        .set = selected.set,
        .source_offsets = owned_offsets,
        .old_to_new = mapping,
        .new_to_source = provenance,
        .requirements = try owned.dupe(ContextRequirements, requirements),
    };
}

test "canonical CTFE closure preserves typed hooks in both source and splice orders" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCanonicalMixed, .{false});
}

test "canonical CTFE closure owns an empty cold provider plan" {
    const Attempt = struct {
        fn accepts(_: *anyopaque, _: ProcArtifact.Artifact) bool {
            unreachable;
        }
        fn run(allocator: Allocator) !void {
            var context: u8 = 0;
            var canonical = try canonicalize(allocator, &.{}, &context, accepts);
            defer canonical.deinit();
            try std.testing.expectEqual(@as(usize, 0), canonical.set.artifacts.len);
            try std.testing.expectEqualSlices(u32, &.{0}, canonical.source_offsets);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Attempt.run, .{});
}

test "consumer preference preserves mixed static and neutral-seed serving closures" {
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |symbolic| {
        for ([_]bool{ false, true }) |reverse| {
            var runtime: [3]ProcArtifact.Artifact = undefined;
            for (&runtime, 0..) |*artifact, index| artifact.* = .{
                .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index + 1)) },
                .code = "\xe8\x00\x00\x00\x00\xc3",
                .entry = 0,
                .frame = null,
                .refs = &.{},
                .relocations = &.{},
                .data = &.{},
                .callable_contract = [_]u8{11} ** 32,
                .context_contract = .{
                    .target = .x64linux,
                    .cpu_level = .default,
                    .hot_reload = false,
                    .default_platform_runtime = false,
                    .dict_seed_mode = .runtime,
                    .hooks_enabled = false,
                    .initialize_boxy_runtime = false,
                    .static_data_readonly = true,
                },
                .context_dependencies = .{},
            };
            runtime[1].context_dependencies = .{ .dict_seed = true };
            runtime[2].context_dependencies = .{ .static_data = true, .static_data_access = .readonly_symbols };
            if (symbolic) {
                runtime[0].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
                runtime[1].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(3) } }};
                runtime[2].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
            } else {
                runtime[0].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
                runtime[1].refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
                runtime[2].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
            }
            var ctfe = runtime;
            for (&ctfe) |*artifact| {
                artifact.context_contract.?.hooks_enabled = true;
                artifact.context_contract.?.static_data_readonly = false;
                artifact.context_contract.?.dict_seed_mode = .comptime_zero;
            }
            ctfe[0].domain = .ctfe;
            ctfe[0].context_dependencies = .{ .comptime_hooks = true };
            // This neutral definition must retain its CTFE namespace: its
            // seed and static callee are not interchangeable with runtime.
            ctfe[2].domain = .ctfe;
            ctfe[2].context_dependencies = .{ .comptime_hooks = true, .static_data = true, .static_data_access = .current_context };
            try std.testing.expect(!compatibleDefinition(runtime[2], ctfe[2]));
            try std.testing.expect(!compatibleDefinition(ctfe[1], runtime[1]));
            const rt = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &runtime };
            const cf = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &ctfe };
            const sources = if (reverse) [_]*const ProcArtifact.Set{ &cf, &rt } else [_]*const ProcArtifact.Set{ &rt, &cf };
            const accepts = struct {
                fn node(_: *anyopaque, _: ProcArtifact.Artifact) bool {
                    return true;
                }
            }.node;
            var context: u8 = 0;
            for ([_]Preference{ .ctfe, .complete_runtime }) |preference| {
                var image = try canonicalizePreferred(allocator, &sources, &context, accepts, preference);
                defer image.deinit();
                const wanted_source: usize = if ((preference == .ctfe) == reverse) 0 else 1;
                const root = image.old_to_new[image.source_offsets[wanted_source]].?;
                var graph = try Self.init(allocator, &image.set);
                defer graph.deinit();
                const placed = try graph.of(root);
                try std.testing.expect(graph.complete);
                try std.testing.expectEqual(@as(usize, 3), placed.len);
                for (placed) |index| try std.testing.expectEqual(@as(u32, @intCast(wanted_source)), image.new_to_source[index]);
            }
        }
    }
}

test "canonical CTFE closure rejects conflicting ARC and emitter contracts before offers" {
    try testCanonicalMixed(std.testing.allocator, true);
}

fn testCanonicalMixed(allocator: Allocator, conflict: bool) !void {
    const Context = @import("CtfeContext.zig");
    const CG = @import("LirCodeGen.zig").LirCodeGen(@import("roc_target").RocTarget.x64linux);
    const b_identity = lir.ProcIdentity.forTest(2);
    var branches = [_]@import("base").Region{.from_raw_offsets(4, 8)};
    var runtime = [_]ProcArtifact.Artifact{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
            .code = "\xe8\x00\x00\x00\x00\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }},
            .relocations = &.{},
            .data = &.{},
        },
        .{
            .kind = .{ .proc = b_identity },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
    };
    for (&runtime) |*artifact| {
        artifact.callable_contract = [_]u8{1} ** 32;
        artifact.context_contract = .{
            .target = .x64linux,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
            .dict_seed_mode = .runtime,
            .hooks_enabled = false,
            .initialize_boxy_runtime = false,
            .static_data_readonly = true,
        };
        artifact.context_dependencies = .{};
    }
    var ctfe = runtime;
    ctfe[0].kind = .{ .proc = lir.ProcIdentity.forTest(3) };
    ctfe[1].domain = .ctfe;
    ctfe[1].code = "\x48\xb8" ++ "\x00" ** 8 ++ "\x48\xb8" ++ "\x00" ** 8 ++ "\xc3";
    ctfe[1].context_dependencies = .{ .comptime_hooks = true };
    ctfe[1].context_bindings = &.{
        Context.Binding{ .failure = .{
            .source = .{
                .checked_module = [_]u8{2} ** 32,
                .source_identity = [_]u8{3} ** 32,
                .region = .from_raw_offsets(1, 3),
                .line = 1,
                .column = 2,
                .has_location = true,
            },
            .checked_error = true,
            .literal_rejection = null,
            .guard_root = null,
        } },
        Context.Binding{ .site = .{
            .checked_module = [_]u8{2} ** 32,
            .checked_site = 7,
            .procedure_identity = b_identity.bytes,
            .kind = .match,
            .region = .from_raw_offsets(4, 8),
            .branch_regions = &branches,
        } },
    };
    ctfe[1].context_relocations = &.{
        .{ .offset = 0, .binding = 0, .encoding = .x86_movabs },
        .{ .offset = 10, .binding = 1, .encoding = .x86_movabs },
    };
    for (&ctfe) |*artifact| {
        artifact.context_contract.?.hooks_enabled = true;
        artifact.context_contract.?.dict_seed_mode = .comptime_zero;
        artifact.context_contract.?.static_data_readonly = false;
    }
    if (conflict) runtime[1].callable_contract = [_]u8{9} ** 32;
    var runtime_set = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &runtime };
    defer runtime_set.deinit();
    var ctfe_set = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &ctfe };
    defer ctfe_set.deinit();
    const Accept = struct {
        fn node(_: *anyopaque, _: ProcArtifact.Artifact) bool {
            return true;
        }
    };
    var context: u8 = 0;
    for ([_]bool{ false, true }) |reverse_sources| {
        const sources = if (reverse_sources) [_]*const ProcArtifact.Set{ &ctfe_set, &runtime_set } else [_]*const ProcArtifact.Set{ &runtime_set, &ctfe_set };
        var canonical = try canonicalize(allocator, &sources, &context, Accept.node);
        defer canonical.deinit();
        const runtime_source: usize = if (reverse_sources) 1 else 0;
        const ctfe_source: usize = if (reverse_sources) 0 else 1;
        const a = canonical.old_to_new[canonical.source_offsets[runtime_source]];
        const c = canonical.old_to_new[canonical.source_offsets[ctfe_source]].?;
        try std.testing.expectEqual(conflict, a == null);
        const b = canonical.set.artifacts[c].refs[0].target;
        try std.testing.expectEqual(Context.Domain.ctfe, canonical.set.artifacts[b].domain);
        try std.testing.expectEqual(@as(usize, 2), canonical.set.artifacts[b].context_bindings.len);
        try std.testing.expect(canonical.set.artifacts[b].context_bindings[0].failure.checked_error);
        try std.testing.expectEqual(@as(?u32, 7), canonical.set.artifacts[b].context_bindings[1].site.checked_site);
        try std.testing.expectEqual(@as(u32, 1), runtime[0].refs[0].target);
        try std.testing.expectEqual(@as(u32, 1), ctfe[0].refs[0].target);
        // Binding graphs are deep-owned, including branch-region slices.
        branches[0] = .from_raw_offsets(20, 30);
        try std.testing.expectEqual(@as(u32, 4), canonical.set.artifacts[b].context_bindings[1].site.branch_regions[0].start.offset);
        branches[0] = .from_raw_offsets(4, 8);
        if (conflict) continue;
        try std.testing.expectEqual(b, canonical.set.artifacts[a.?].refs[0].target);
        inline for (.{ false, true }) |reverse_roots| {
            var store = lir.LirStore.init(allocator);
            defer store.deinit();
            var layouts = try @import("layout").Store.init(allocator, .u64);
            defer layouts.deinit();
            var receiver = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
            defer receiver.deinit();
            var graph = try Self.init(allocator, &canonical.set);
            defer graph.deinit();
            var procs = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
            defer procs.deinit();
            var placed = std.AutoHashMap(u32, usize).init(allocator);
            defer placed.deinit();
            var data = std.ArrayList(ProcArtifact.DataItem).empty;
            defer data.deinit(allocator);
            const roots = if (reverse_roots) [_]u32{ c, a.? } else [_]u32{ a.?, c };
            for (roots) |root| try ProcArtifact.spliceIndexed(CG, allocator, &receiver, &graph, &.{root}, &procs, &placed, &data);
            try std.testing.expectEqual(@as(usize, 3), placed.count());
            try std.testing.expectEqual(@as(usize, 2), receiver.context_bindings.items.len);
            try std.testing.expect(receiver.context_bindings.items[0].failure.checked_error);
            try std.testing.expectEqual(@as(?u32, 7), receiver.context_bindings.items[1].site.checked_site);
            try receiver.finishImage();
            var recaptured = try ProcArtifact.extract(CG, allocator, &receiver, store.getProcSpecs(), &layouts, &.{}, &.{}, &.{});
            defer recaptured.deinit();
            var found_hook_definition = false;
            for (recaptured.artifacts) |artifact| {
                if (artifact.kind != .proc or !std.meta.eql(artifact.kind.proc, b_identity)) continue;
                try std.testing.expectEqualDeep(canonical.set.artifacts[b], artifact);
                found_hook_definition = true;
            }
            try std.testing.expect(found_hook_definition);
        }
    }
    // A seed-dependent mismatch is independently incompatible, even with the
    // same ARC/ABI stamp. Unrelated definitions are not blanket-declined.
    runtime[1].callable_contract = ctfe[1].callable_contract;
    runtime[1].context_dependencies.?.dict_seed = true;
    try std.testing.expect(!compatibleDefinition(runtime[1], ctfe[1]));
    // A declared runtime consumer retains the historical full-native provision.
    // Preserve it in either physical load order and reject only the CTFE
    // caller whose selected callee contract is incompatible.
    ctfe[1].domain = .runtime;
    ctfe[1].code = "\xc3";
    ctfe[1].context_dependencies = .{ .dict_seed = true };
    ctfe[1].context_bindings = &.{};
    ctfe[1].context_relocations = &.{};
    for ([_]bool{ false, true }) |seed_dependent| {
        runtime[1].context_dependencies.?.dict_seed = seed_dependent;
        ctfe[1].context_dependencies.?.dict_seed = seed_dependent;
        for ([_]bool{ false, true }) |reverse_sources| {
            const sources = if (reverse_sources) [_]*const ProcArtifact.Set{ &ctfe_set, &runtime_set } else [_]*const ProcArtifact.Set{ &runtime_set, &ctfe_set };
            var canonical = try canonicalizePreferred(allocator, &sources, &context, Accept.node, .complete_runtime);
            defer canonical.deinit();
            const runtime_source: usize = if (reverse_sources) 1 else 0;
            const ctfe_source: usize = if (reverse_sources) 0 else 1;
            const root = canonical.old_to_new[canonical.source_offsets[runtime_source]].?;
            const b = canonical.set.artifacts[root].refs[0].target;
            const neutral_root = canonical.old_to_new[canonical.source_offsets[ctfe_source]];
            try std.testing.expectEqual(seed_dependent, neutral_root == null);
            if (neutral_root) |callee| try std.testing.expectEqual(b, canonical.set.artifacts[callee].refs[0].target);
            try std.testing.expectEqual(@as(u32, @intCast(runtime_source)), canonical.new_to_source[b]);
            try std.testing.expect(canonical.set.artifacts[b].context_contract.?.static_data_readonly);
        }
    }
}

test "artifact admission evaluates each chain node once and propagates invalid ancestors" {
    const count = 128;
    var artifacts: [count]ProcArtifact.Artifact = undefined;
    var refs: [count - 1]ProcArtifact.Reference = undefined;
    for (&artifacts, 0..) |*artifact, index| {
        artifact.* = .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index)) },
            .code = "",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        };
        if (index + 1 < count) {
            refs[index] = .{ .site = 0, .form = .call, .target = @intCast(index + 1), .delta = 0 };
            artifact.refs = refs[index..][0..1];
        }
    }
    var source = ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(std.testing.allocator),
        .artifacts = &artifacts,
    };
    defer source.deinit();
    var graph = try Self.init(std.testing.allocator, &source);
    defer graph.deinit();
    const Probe = struct {
        calls: usize = 0,
        fn accepts(context: *anyopaque, artifact: ProcArtifact.Artifact) bool {
            const self: *@This() = @ptrCast(@alignCast(context));
            self.calls += 1;
            return !std.meta.eql(artifact.kind.proc, lir.ProcIdentity.forTest(count - 1));
        }
    };
    var probe = Probe{};
    const eligible = try graph.admit(&probe, Probe.accepts);
    defer std.testing.allocator.free(eligible);
    try std.testing.expectEqual(count, probe.calls);
    for (eligible) |valid| try std.testing.expect(!valid);
}

test "symbolic index resolves a chain once and preserves cycles external names and kind namespaces" {
    const count = 128;
    var artifacts: [count + 2]ProcArtifact.Artifact = undefined;
    var refs: [count]ProcArtifact.SymbolicReference = undefined;
    for (&artifacts, 0..) |*artifact, index| {
        artifact.* = .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index)) },
            .code = "",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        };
        if (index < count) {
            refs[index] = .{
                .site = 0,
                .form = .call,
                .target = .{ .proc = lir.ProcIdentity.forTest(@intCast((index + 1) % count)) },
            };
            artifact.symbolic_refs = refs[index..][0..1];
        }
    }
    artifacts[count].kind = .{ .boxy_thunk = lir.ProcIdentity.forTest(0) };
    const external = ProcArtifact.SymbolicReference{
        .site = 0,
        .form = .call,
        .target = .{ .rc_helper = "external_helper" },
    };
    artifacts[count + 1].symbolic_refs = &.{external};
    var source = ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(std.testing.allocator),
        .artifacts = &artifacts,
    };
    defer source.deinit();
    var graph = try Self.init(std.testing.allocator, &source);
    defer graph.deinit();
    try std.testing.expectEqual(count + 1, graph.symbolic_lookups);
    try std.testing.expectEqual(@as(?u32, 0), graph.procs.get(lir.ProcIdentity.forTest(0)));
    try std.testing.expectEqual(@as(?u32, count), graph.thunks.get(lir.ProcIdentity.forTest(0)));
    for (0..count) |root| {
        const closure = try graph.of(@intCast(root));
        try std.testing.expectEqual(count, closure.len);
        try std.testing.expect(graph.complete);
    }
    try std.testing.expectEqual(count + 1, graph.symbolic_lookups);
    try std.testing.expectEqual(@as(?u32, null), graph.symbolicTargets(count + 1)[0]);
    _ = try graph.of(count + 1);
    try std.testing.expect(!graph.complete);
}

test "symbolic index declines ambiguous same-identity contracts in either definition order" {
    const identity = lir.ProcIdentity.forTest(2);
    const reference = ProcArtifact.SymbolicReference{
        .site = 0,
        .form = .call,
        .target = .{ .proc = identity },
    };
    const runtime = ProcArtifact.Artifact{
        .kind = .{ .proc = identity },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .callable_contract = [_]u8{1} ** 32,
        .context_dependencies = .{},
        .context_contract = .{
            .target = .x64linux,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
            .dict_seed_mode = .runtime,
            .hooks_enabled = false,
            .initialize_boxy_runtime = false,
            .static_data_readonly = true,
        },
    };
    const Variant = enum { compatible, conflicting, unsupported };
    for ([_]Variant{ .compatible, .conflicting, .unsupported }) |variant| {
        const conflict = variant != .compatible;
        var ctfe = runtime;
        ctfe.domain = .ctfe;
        ctfe.context_contract.?.hooks_enabled = true;
        ctfe.context_contract.?.static_data_readonly = false;
        ctfe.context_dependencies = .{ .comptime_hooks = true };
        if (variant == .conflicting) ctfe.callable_contract = [_]u8{9} ** 32;
        if (variant == .unsupported) ctfe.callable_contract = null;
        for ([_]bool{ false, true }) |reverse| {
            var artifacts = if (reverse) [_]ProcArtifact.Artifact{ runtime, ctfe, runtime, runtime } else [_]ProcArtifact.Artifact{ runtime, runtime, ctfe, runtime };
            artifacts[0].kind = .{ .proc = lir.ProcIdentity.forTest(1) };
            artifacts[0].symbolic_refs = &.{reference};
            const runtime_index: u32 = if (reverse) 2 else 1;
            const exact = ProcArtifact.Reference{ .site = 0, .form = .call, .target = runtime_index, .delta = 0 };
            artifacts[3].kind = .{ .proc = lir.ProcIdentity.forTest(3) };
            artifacts[3].refs = &.{exact};
            var source = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(std.testing.allocator), .artifacts = &artifacts };
            defer source.deinit();
            var graph = try Self.init(std.testing.allocator, &source);
            defer graph.deinit();
            try std.testing.expectEqual(conflict, graph.symbolicTargets(0)[0] == null);
            _ = try graph.of(0);
            try std.testing.expectEqual(!conflict, graph.complete);
            if (!conflict) {
                const target = graph.symbolicTargets(0)[0].?;
                try std.testing.expectEqual(@import("CtfeContext.zig").Domain.ctfe, artifacts[target].domain);
            }
            _ = try graph.of(3);
            try std.testing.expect(graph.complete);
            const Accept = struct {
                fn node(_: *anyopaque, _: ProcArtifact.Artifact) bool {
                    return true;
                }
            };
            var context: u8 = 0;
            const admitted = try graph.admit(&context, Accept.node);
            defer std.testing.allocator.free(admitted);
            try std.testing.expectEqual(!conflict, admitted[0]);
            try std.testing.expect(admitted[3]);
            try std.testing.expect(admitted[runtime_index]);
        }
    }
}

/// Deep-own an explicitly selected closed graph, preserving stable identities
/// and rebasing only set-local references. Order is the producer's set order.
pub fn selectMarked(allocator: Allocator, source: *const ProcArtifact.Set, marked: []const bool) Allocator.Error!Selection {
    std.debug.assert(marked.len == source.artifacts.len);
    var temporary = std.heap.ArenaAllocator.init(allocator);
    defer temporary.deinit();
    const a = temporary.allocator();
    const remap = try a.alloc(?u32, marked.len);
    @memset(remap, null);
    var artifacts = std.ArrayList(ProcArtifact.Artifact).empty;
    for (source.artifacts, marked, 0..) |artifact, keep, index| {
        if (!keep) continue;
        remap[index] = @intCast(artifacts.items.len);
        try artifacts.append(a, artifact);
    }
    for (artifacts.items) |*artifact| {
        const refs = try a.dupe(ProcArtifact.Reference, artifact.refs);
        for (refs) |*ref| ref.target = remap[ref.target].?;
        artifact.refs = refs;
    }
    const view = ProcArtifact.Set{ .arena = temporary, .artifacts = artifacts.items };
    var set = try ProcArtifact.combine(allocator, &.{&view});
    errdefer set.deinit();
    const owned_remap = try set.arena.allocator().dupe(?u32, remap);
    return .{ .set = set, .old_to_new = owned_remap };
}

test "artifact closure selection owns context graphs and rebases cyclic references" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testSelection, .{});
}

fn testSelection(allocator: Allocator) !void {
    var branches = [_]@import("base").Region{
        .from_raw_offsets(1, 2),
        .from_raw_offsets(3, 4),
    };
    const helper = lir.ProcIdentity.forTest(3);
    const artifacts = [_]ProcArtifact.Artifact{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(2) },
            .code = "\xe8\x00\x00\x00\x00\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }},
            .relocations = &.{},
            .data = &.{},
        },
        .{
            .kind = .{ .proc = helper },
            .code = "\x48\xb8" ++ "\x00" ** 8 ++ "\xe8\x00\x00\x00\x00\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 10, .form = .call, .target = 2, .delta = 0 }},
            .relocations = &.{},
            .data = &.{},
            .domain = .ctfe,
            .context_contract = .{
                .target = .x64musl,
                .cpu_level = .default,
                .hot_reload = false,
                .default_platform_runtime = false,
                .dict_seed_mode = .comptime_zero,
                .hooks_enabled = true,
                .initialize_boxy_runtime = false,
                .static_data_readonly = false,
            },
            .context_dependencies = .{ .comptime_hooks = true },
            .context_bindings = &.{.{ .site = .{
                .checked_module = @splat(7),
                .checked_site = null,
                .procedure_identity = helper.bytes,
                .kind = .if_,
                .region = .from_raw_offsets(1, 4),
                .branch_regions = &branches,
            } }},
            .context_relocations = &.{.{ .offset = 0, .binding = 0, .encoding = .x86_movabs }},
        },
    };
    const source = ProcArtifact.Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &artifacts };
    var closure = try init(allocator, &source);
    defer closure.deinit();
    const placed = try closure.of(1);
    try std.testing.expect(closure.complete);
    try std.testing.expectEqualSlices(u32, &.{ 1, 2 }, placed);
    const Accept = struct {
        fn node(_: *anyopaque, artifact: ProcArtifact.Artifact) bool {
            return switch (artifact.kind) {
                .proc => |identity| !std.meta.eql(identity, lir.ProcIdentity.forTest(1)),
                else => true,
            };
        }
    };
    var context: u8 = 0;
    const valid = try closure.admit(&context, Accept.node);
    defer allocator.free(valid);
    try std.testing.expectEqualSlices(bool, &.{ false, true, true }, valid);
    var selected = try selectMarked(allocator, &source, &.{ false, true, true });
    defer selected.deinit();
    branches[0] = .from_raw_offsets(99, 100);
    try std.testing.expectEqual(@as(u32, 1), selected.set.artifacts[1].context_bindings[0].site.branch_regions[0].start.offset);
    try std.testing.expectEqual(@as(u32, 1), selected.set.artifacts[0].refs[0].target);
    try std.testing.expectEqual(@as(u32, 1), selected.set.artifacts[1].refs[0].target);
    const PackFile = @import("PackFile.zig");
    const bytes = try PackFile.write(allocator, &selected.set, &.{});
    defer allocator.free(bytes);
    var decoded = try PackFile.read(allocator, bytes);
    defer decoded.deinit();
    try std.testing.expectEqualDeep(selected.set.artifacts, decoded.set.artifacts);
}
