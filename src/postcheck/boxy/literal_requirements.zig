//! Demand propagation for Boxy's compile-time literal values.
//!
//! The checked-to-Boxy planner supplies interned type constructors, variables,
//! and call substitutions. This module never inspects source syntax or layouts.
//! Requirements name a literal site and a type term, never a call path.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Index of a hash-consed type term in `Terms`.
pub const TermId = enum(u32) { _ };
/// A Boxy worker that owns literal demands and incoming call edges.
pub const WorkerId = enum(u32) { _ };
/// A call edge from one worker (or a root) into another worker.
pub const EdgeId = enum(u32) { _ };
/// A literal site: one compile-time conversion read in a worker body.
pub const SiteId = enum(u32) { _ };
/// A requirement to produce one literal site's value at one type term.
pub const RequirementId = enum(u32) { _ };

/// A type term: a worker-local variable or a constructor applied to terms.
pub const Term = union(enum) {
    variable: u64,
    application: struct { constructor: u64, args: []const TermId },
};

const TermContext = struct {
    pub fn hash(_: TermContext, term: Term) u64 {
        var state = std.hash.Wyhash.init(@intFromEnum(std.meta.activeTag(term)));
        switch (term) {
            .variable => |variable| state.update(std.mem.asBytes(&variable)),
            .application => |application| {
                state.update(std.mem.asBytes(&application.constructor));
                state.update(std.mem.sliceAsBytes(application.args));
            },
        }
        return state.final();
    }

    pub fn eql(_: TermContext, a: Term, b: Term) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .variable => |variable| variable == b.variable,
            .application => |application| application.constructor == b.application.constructor and
                std.mem.eql(TermId, application.args, b.application.args),
        };
    }
};

/// One substitution of a callee variable by a caller term on a call edge.
pub const Binding = struct { variable: u64, term: TermId };

/// Hash-consed type terms. Constructor identity comes from the planner's
/// checked type authority and includes nominal identity and structural labels.
pub const Terms = struct {
    allocator: Allocator,
    entries: std.ArrayList(Term) = .empty,
    closed: std.ArrayList(bool) = .empty,
    by_term: std.HashMapUnmanaged(Term, TermId, TermContext, 80) = .empty,

    pub fn init(allocator: Allocator) Terms {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Terms) void {
        self.by_term.deinit(self.allocator);
        for (self.entries.items) |term| switch (term) {
            .variable => {},
            .application => |application| self.allocator.free(application.args),
        };
        self.entries.deinit(self.allocator);
        self.closed.deinit(self.allocator);
    }

    pub fn intern(self: *Terms, term: Term) Allocator.Error!TermId {
        if (self.by_term.get(term)) |id| return id;
        try self.entries.ensureUnusedCapacity(self.allocator, 1);
        try self.closed.ensureUnusedCapacity(self.allocator, 1);
        try self.by_term.ensureUnusedCapacity(self.allocator, 1);
        var owned = term;
        const is_closed = switch (term) {
            .variable => false,
            .application => |application| blk: {
                owned.application.args = try self.allocator.dupe(TermId, application.args);
                for (application.args) |arg| {
                    if (!self.isClosed(arg)) break :blk false;
                }
                break :blk true;
            },
        };
        const id: TermId = @enumFromInt(self.entries.items.len);
        self.entries.appendAssumeCapacity(owned);
        self.closed.appendAssumeCapacity(is_closed);
        self.by_term.putAssumeCapacity(owned, id);
        return id;
    }

    pub fn isClosed(self: *const Terms, term: TermId) bool {
        return self.closed.items[@intFromEnum(term)];
    }

    /// Substitute `bindings` into `root`, rebuilding each open application
    /// after its arguments, in the order a depth-first walk reaches them.
    pub fn substitute(
        self: *Terms,
        root: TermId,
        bindings: []const Binding,
        memo: *std.AutoHashMapUnmanaged(TermId, TermId),
    ) Allocator.Error!TermId {
        const Frame = struct { term: TermId, args: []TermId, next: usize = 0 };
        var frames: std.ArrayList(Frame) = .empty;
        defer {
            for (frames.items) |frame| self.allocator.free(frame.args);
            frames.deinit(self.allocator);
        }
        var delivered: ?TermId = null;
        var pending: ?TermId = root;
        while (true) {
            if (pending) |term| {
                pending = null;
                if (self.isClosed(term)) {
                    delivered = term;
                } else if (memo.get(term)) |result| {
                    delivered = result;
                } else switch (self.entries.items[@intFromEnum(term)]) {
                    .variable => |variable| {
                        // A lexical variable not quantified by the callee
                        // stays in its enclosing scope. Only recorded
                        // substitutions apply.
                        var result = term;
                        for (bindings) |binding| {
                            if (binding.variable == variable) {
                                result = binding.term;
                                break;
                            }
                        }
                        try memo.put(self.allocator, term, result);
                        delivered = result;
                    },
                    .application => |application| {
                        const args = try self.allocator.alloc(TermId, application.args.len);
                        frames.append(self.allocator, .{ .term = term, .args = args }) catch |err| {
                            self.allocator.free(args);
                            return err;
                        };
                    },
                }
            }
            const top = if (frames.items.len == 0) return delivered.? else &frames.items[frames.items.len - 1];
            if (delivered) |result| {
                top.args[top.next] = result;
                top.next += 1;
                delivered = null;
            }
            const application = self.entries.items[@intFromEnum(top.term)].application;
            if (top.next < application.args.len) {
                pending = application.args[top.next];
                continue;
            }
            const frame = frames.pop().?;
            defer self.allocator.free(frame.args);
            const result = try self.intern(.{ .application = .{ .constructor = application.constructor, .args = frame.args } });
            try memo.put(self.allocator, frame.term, result);
            delivered = result;
        }
    }
};

/// A literal site demanded at a type term, with the evidence term its
/// conversion dispatches through when that evidence is not the value type.
pub const Requirement = struct { site: SiteId, ty: TermId, evidence: ?TermId = null };

/// A closed requirement is a compile-time initializer, and an open requirement
/// is supplied by the enclosing worker's evidence. Neither source executes a
/// conversion when an ordinary runtime call forwards it.
pub const Source = union(enum) {
    closed: RequirementId,
    parameter: RequirementId,
};

/// A range in one of the `Abi` arrays.
pub const Span = struct { start: u32 = 0, len: u32 = 0 };

/// Dense lowering input. Parameter ordinals are local to the caller; result
/// ordinals address `closed_requirements`. No hash lookup or requirement
/// solving remains on the body-lowering path.
pub const Argument = union(enum) {
    result: u32,
    parameter: u32,
};

/// Frozen per-worker literal parameters and per-edge arguments, in the dense
/// form lowering consumes.
pub const Abi = struct {
    allocator: Allocator,
    workers: []Span,
    edges: []Span,
    parameters: []RequirementId,
    arguments: []Argument,
    closed_requirements: []RequirementId,

    pub fn deinit(self: *Abi) void {
        self.allocator.free(self.workers);
        self.allocator.free(self.edges);
        self.allocator.free(self.parameters);
        self.allocator.free(self.arguments);
        self.allocator.free(self.closed_requirements);
    }

    pub fn workerParameters(self: *const Abi, worker: WorkerId) []const RequirementId {
        const span = self.workers[@intFromEnum(worker)];
        return self.parameters[span.start..][0..span.len];
    }

    pub fn edgeArguments(self: *const Abi, edge: EdgeId) []const Argument {
        const span = self.edges[@intFromEnum(edge)];
        return self.arguments[span.start..][0..span.len];
    }
};

const Worker = struct {
    incoming: std.ArrayList(EdgeId) = .empty,
    requirements: std.AutoHashMapUnmanaged(RequirementId, void) = .empty,
};

const Edge = struct {
    caller: ?WorkerId,
    callee: WorkerId,
    bindings: []const Binding,
    bindings_ready: bool = true,
    /// Freeze-environment bindings instantiate runtime literal sites only.
    site_limit: ?u32 = null,
    memo: std.AutoHashMapUnmanaged(TermId, TermId) = .empty,
    args: std.AutoHashMapUnmanaged(RequirementId, Source) = .empty,
};

const Pending = struct { edge: EdgeId, requirement: RequirementId };

/// Incremental closure over the explicit call/evidence graph. Adding an edge
/// processes existing demands; adding a demand processes existing incoming
/// edges. A pair is processed once, irrespective of discovery order.
pub const Graph = struct {
    allocator: Allocator,
    terms: Terms,
    workers: std.ArrayList(Worker) = .empty,
    edges: std.ArrayList(Edge) = .empty,
    requirements: std.ArrayList(Requirement) = .empty,
    by_requirement: std.AutoHashMapUnmanaged(Requirement, RequirementId) = .empty,
    pending: std.ArrayList(Pending) = .empty,
    cursor: usize = 0,
    site_count: u32 = 0,
    observations: std.AutoHashMapUnmanaged(SiteId, void) = .empty,

    pub fn init(allocator: Allocator) Graph {
        return .{ .allocator = allocator, .terms = Terms.init(allocator) };
    }

    pub fn deinit(self: *Graph) void {
        for (self.workers.items) |*worker| {
            worker.incoming.deinit(self.allocator);
            worker.requirements.deinit(self.allocator);
        }
        for (self.edges.items) |*edge| {
            self.allocator.free(edge.bindings);
            edge.memo.deinit(self.allocator);
            edge.args.deinit(self.allocator);
        }
        self.workers.deinit(self.allocator);
        self.edges.deinit(self.allocator);
        self.requirements.deinit(self.allocator);
        self.by_requirement.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        self.terms.deinit();
        self.observations.deinit(self.allocator);
    }

    /// Allocate the identity of one source literal. Its checked source is
    /// retained by the planner, independently of its instantiated demands.
    pub fn addSite(self: *Graph) SiteId {
        const site: SiteId = @enumFromInt(self.site_count);
        self.site_count += 1;
        return site;
    }

    /// Observe closed environments on call edges without making runtime-only
    /// open environments into literal ABI parameters. A freezer must still
    /// match every value it actually exports to an observed closed recipe.
    pub fn addObservationSite(self: *Graph) Allocator.Error!SiteId {
        const site = self.addSite();
        try self.observations.put(self.allocator, site, {});
        return site;
    }

    pub fn addWorker(self: *Graph) Allocator.Error!WorkerId {
        const id: WorkerId = @enumFromInt(self.workers.items.len);
        try self.workers.append(self.allocator, .{});
        return id;
    }

    pub fn addEdge(self: *Graph, caller: ?WorkerId, callee: WorkerId, bindings: []const Binding) Allocator.Error!EdgeId {
        const id: EdgeId = @enumFromInt(self.edges.items.len);
        const owned = try self.allocator.dupe(Binding, bindings);
        self.edges.append(self.allocator, .{ .caller = caller, .callee = callee, .bindings = owned }) catch |err| {
            self.allocator.free(owned);
            return err;
        };
        // The edge owns `owned` after append, including on subsequent errors.
        return try self.connectEdge(id);
    }

    /// Call graph discovery need not build type terms for calls that never
    /// carry literal evidence. The planner supplies their substitutions only
    /// if a propagated demand reaches the edge.
    pub fn addDeferredEdge(self: *Graph, caller: ?WorkerId, callee: WorkerId) Allocator.Error!EdgeId {
        const id = try self.addEdge(caller, callee, &.{});
        self.edges.items[@intFromEnum(id)].bindings_ready = false;
        return id;
    }

    pub fn bindEdge(self: *Graph, id: EdgeId, bindings: []const Binding) Allocator.Error!void {
        const edge = &self.edges.items[@intFromEnum(id)];
        std.debug.assert(!edge.bindings_ready);
        const owned = try self.allocator.dupe(Binding, bindings);
        self.allocator.free(edge.bindings);
        edge.bindings = owned;
        edge.bindings_ready = true;
    }

    pub fn pendingEdge(self: *const Graph) EdgeId {
        return self.pending.items[self.cursor].edge;
    }

    fn connectEdge(self: *Graph, id: EdgeId) Allocator.Error!EdgeId {
        const worker = &self.workers.items[@intFromEnum(self.edges.items[@intFromEnum(id)].callee)];
        try worker.incoming.append(self.allocator, id);
        var requirements = worker.requirements.keyIterator();
        while (requirements.next()) |requirement| {
            try self.pending.append(self.allocator, .{ .edge = id, .requirement = requirement.* });
        }
        return id;
    }

    fn internRequirement(self: *Graph, requirement: Requirement) Allocator.Error!RequirementId {
        if (self.by_requirement.get(requirement)) |id| return id;
        try self.requirements.ensureUnusedCapacity(self.allocator, 1);
        const entry = try self.by_requirement.getOrPut(self.allocator, requirement);
        const id: RequirementId = @enumFromInt(self.requirements.items.len);
        entry.value_ptr.* = id;
        self.requirements.appendAssumeCapacity(requirement);
        return id;
    }

    pub fn isClosed(self: *const Graph, requirement: Requirement) bool {
        return self.terms.isClosed(requirement.ty) and (if (requirement.evidence) |evidence| self.terms.isClosed(evidence) else true);
    }

    pub fn demand(self: *Graph, worker: WorkerId, requirement: Requirement) Allocator.Error!Source {
        const id = try self.internRequirement(requirement);
        if (self.isClosed(requirement)) return .{ .closed = id };
        const owner = &self.workers.items[@intFromEnum(worker)];
        if (!(try owner.requirements.getOrPut(self.allocator, id)).found_existing) {
            for (owner.incoming.items) |edge| try self.pending.append(self.allocator, .{ .edge = edge, .requirement = id });
        }
        return .{ .parameter = id };
    }

    /// Root edges must close every requirement. An unbound root is a planner
    /// invariant violation; consumers must not choose runtime conversion.
    pub fn solve(self: *Graph) (Allocator.Error || error{ UnboundRootRequirement, MissingSubstitution })!void {
        while (self.cursor < self.pending.items.len) : (self.cursor += 1) {
            const pending = self.pending.items[self.cursor];
            const edge = &self.edges.items[@intFromEnum(pending.edge)];
            if (edge.args.contains(pending.requirement)) continue;
            if (!edge.bindings_ready) return error.MissingSubstitution;
            const requirement = self.requirements.items[@intFromEnum(pending.requirement)];
            if (edge.site_limit) |limit| if (@intFromEnum(requirement.site) >= limit) continue;
            const ty = try self.terms.substitute(requirement.ty, edge.bindings, &edge.memo);
            const instantiated = Requirement{
                .site = requirement.site,
                .ty = ty,
                .evidence = if (requirement.evidence) |evidence| try self.terms.substitute(evidence, edge.bindings, &edge.memo) else null,
            };
            const source: Source = if (self.isClosed(instantiated))
                .{ .closed = try self.internRequirement(instantiated) }
            else if (edge.caller) |caller|
                try self.demand(caller, instantiated)
            else if (self.observations.contains(requirement.site))
                continue
            else
                return error.UnboundRootRequirement;
            try edge.args.put(self.allocator, pending.requirement, source);
        }
    }

    pub fn argument(self: *const Graph, edge: EdgeId, requirement: RequirementId) ?Source {
        return self.edges.items[@intFromEnum(edge)].args.get(requirement);
    }

    pub fn freezeAbi(self: *const Graph) Allocator.Error!Abi {
        return self.freezeAbiForSites(null);
    }

    /// A site used only at builtin numeric types keeps descriptor-based
    /// lowering. Removing it here removes its entire propagated ABI demand.
    pub fn freezeAbiForSites(self: *const Graph, active_sites: ?[]const bool) Allocator.Error!Abi {
        std.debug.assert(self.cursor == self.pending.items.len);
        const allocator = self.allocator;
        const workers = try allocator.alloc(Span, self.workers.items.len);
        errdefer allocator.free(workers);
        const edges = try allocator.alloc(Span, self.edges.items.len);
        errdefer allocator.free(edges);
        var parameters = std.ArrayList(RequirementId).empty;
        defer parameters.deinit(allocator);
        var arguments = std.ArrayList(Argument).empty;
        defer arguments.deinit(allocator);
        var closed = std.ArrayList(RequirementId).empty;
        defer closed.deinit(allocator);
        const ParameterKey = struct { worker: WorkerId, requirement: RequirementId };
        var slots = std.AutoHashMap(ParameterKey, u32).init(allocator);
        defer slots.deinit();
        // Result ordinal of each closed requirement, by requirement id.
        const results = try allocator.alloc(?u32, self.requirements.items.len);
        defer allocator.free(results);
        @memset(results, null);
        for (self.requirements.items, 0..) |requirement, index| {
            if (self.observations.contains(requirement.site)) continue;
            if (!self.isClosed(requirement)) continue;
            if (active_sites) |active| if (!active[@intFromEnum(requirement.site)]) continue;
            const id: RequirementId = @enumFromInt(index);
            results[@intFromEnum(id)] = @intCast(closed.items.len);
            try closed.append(allocator, id);
        }
        for (self.workers.items, workers, 0..) |worker, *span, index| {
            span.* = .{ .start = @intCast(parameters.items.len) };
            var required = worker.requirements.keyIterator();
            while (required.next()) |requirement| {
                if (self.observations.contains(self.requirements.items[@intFromEnum(requirement.*)].site)) continue;
                if (active_sites) |active| if (!active[@intFromEnum(self.requirements.items[@intFromEnum(requirement.*)].site)]) continue;
                try parameters.append(allocator, requirement.*);
            }
            span.len = @intCast(parameters.items.len - span.start);
            const params = parameters.items[span.start..][0..span.len];
            std.mem.sort(RequirementId, params, {}, struct {
                fn lessThan(_: void, a: RequirementId, b: RequirementId) bool {
                    return @intFromEnum(a) < @intFromEnum(b);
                }
            }.lessThan);
            for (params, 0..) |requirement, ordinal| try slots.put(.{ .worker = @enumFromInt(index), .requirement = requirement }, @intCast(ordinal));
        }
        for (self.edges.items, edges) |edge, *span| {
            const callee = workers[@intFromEnum(edge.callee)];
            span.* = .{ .start = @intCast(arguments.items.len), .len = callee.len };
            for (parameters.items[callee.start..][0..callee.len]) |requirement| {
                const source = edge.args.get(requirement).?;
                try arguments.append(allocator, switch (source) {
                    .closed => |id| .{ .result = results[@intFromEnum(id)].? },
                    .parameter => |id| .{ .parameter = slots.get(.{ .worker = edge.caller.?, .requirement = id }).? },
                });
            }
        }
        const owned_parameters = try parameters.toOwnedSlice(allocator);
        errdefer allocator.free(owned_parameters);
        const owned_arguments = try arguments.toOwnedSlice(allocator);
        errdefer allocator.free(owned_arguments);
        const owned_closed = try closed.toOwnedSlice(allocator);
        return .{
            .allocator = allocator,
            .workers = workers,
            .edges = edges,
            .parameters = owned_parameters,
            .arguments = owned_arguments,
            .closed_requirements = owned_closed,
        };
    }
};

test "boxy literal demands close composite types across recursive forwarding" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const caller = try graph.addWorker();
    const a = try graph.terms.intern(.{ .variable = 0 });
    const b = try graph.terms.intern(.{ .variable = 1 });
    const x = try graph.terms.intern(.{ .variable = 2 });
    const y = try graph.terms.intern(.{ .variable = 3 });
    const word = try graph.terms.intern(.{ .application = .{ .constructor = 0, .args = &.{} } });
    const number = try graph.terms.intern(.{ .application = .{ .constructor = 1, .args = &.{} } });
    const pair = try graph.terms.intern(.{ .application = .{ .constructor = 2, .args = &.{ a, b } } });
    const recursive = try graph.addEdge(worker, worker, &.{ .{ .variable = 0, .term = a }, .{ .variable = 1, .term = b } });
    const call = try graph.addEdge(caller, worker, &.{ .{ .variable = 0, .term = x }, .{ .variable = 1, .term = y } });
    const root = try graph.addEdge(null, caller, &.{ .{ .variable = 2, .term = word }, .{ .variable = 3, .term = number } });
    const site = graph.addSite();
    const required = try graph.demand(worker, .{ .site = site, .ty = pair });
    try graph.solve();
    try std.testing.expectEqual(required, graph.argument(recursive, required.parameter).?);
    const forwarded = graph.argument(call, required.parameter).?;
    const result = graph.argument(root, forwarded.parameter).?;
    const expected = try graph.terms.intern(.{ .application = .{ .constructor = 2, .args = &.{ word, number } } });
    try std.testing.expectEqual(expected, graph.requirements.items[@intFromEnum(result.closed)].ty);
    const processed = graph.cursor;
    _ = try graph.demand(worker, .{ .site = site, .ty = pair });
    try graph.solve();
    try std.testing.expectEqual(processed, graph.cursor);
    var abi = try graph.freezeAbi();
    defer abi.deinit();
    try std.testing.expectEqualSlices(RequirementId, &.{required.parameter}, abi.workerParameters(worker));
    try std.testing.expectEqualSlices(Argument, &.{.{ .parameter = 0 }}, abi.edgeArguments(recursive));
    try std.testing.expectEqualSlices(Argument, &.{.{ .parameter = 0 }}, abi.edgeArguments(call));
    try std.testing.expectEqualSlices(Argument, &.{.{ .result = 0 }}, abi.edgeArguments(root));
    try std.testing.expectEqualSlices(RequirementId, &.{result.closed}, abi.closed_requirements);
}

test "boxy literal demands added before call edges retain distinct sites and types" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const a = try graph.terms.intern(.{ .variable = 0 });
    const word = try graph.terms.intern(.{ .application = .{ .constructor = 0, .args = &.{} } });
    const other = try graph.terms.intern(.{ .application = .{ .constructor = 1, .args = &.{} } });
    const first = try graph.demand(worker, .{ .site = graph.addSite(), .ty = a });
    const second = try graph.demand(worker, .{ .site = graph.addSite(), .ty = a });
    const left = try graph.addEdge(null, worker, &.{.{ .variable = 0, .term = word }});
    const right = try graph.addEdge(null, worker, &.{.{ .variable = 0, .term = other }});
    try graph.solve();
    try std.testing.expect(graph.argument(left, first.parameter).?.closed != graph.argument(left, second.parameter).?.closed);
    try std.testing.expect(graph.argument(left, first.parameter).?.closed != graph.argument(right, first.parameter).?.closed);
}

test "boxy literal demands reject an unbound root instead of converting at runtime" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const a = try graph.terms.intern(.{ .variable = 0 });
    _ = try graph.demand(worker, .{ .site = graph.addSite(), .ty = a });
    _ = try graph.addEdge(null, worker, &.{});
    try std.testing.expectError(error.UnboundRootRequirement, graph.solve());
}

test "boxy literal demands close a finite polymorphic recursive permutation" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const a = try graph.terms.intern(.{ .variable = 0 });
    const b = try graph.terms.intern(.{ .variable = 1 });
    const first_type = try graph.terms.intern(.{ .application = .{ .constructor = 0, .args = &.{} } });
    const second_type = try graph.terms.intern(.{ .application = .{ .constructor = 1, .args = &.{} } });
    const pair = try graph.terms.intern(.{ .application = .{ .constructor = 2, .args = &.{ a, b } } });
    const requirement = try graph.demand(worker, .{ .site = graph.addSite(), .ty = pair });
    const recursive = try graph.addEdge(worker, worker, &.{ .{ .variable = 0, .term = b }, .{ .variable = 1, .term = a } });
    const root = try graph.addEdge(null, worker, &.{ .{ .variable = 0, .term = first_type }, .{ .variable = 1, .term = second_type } });
    try graph.solve();
    const swapped = graph.argument(recursive, requirement.parameter).?;
    try std.testing.expect(swapped.parameter != requirement.parameter);
    try std.testing.expectEqual(requirement, graph.argument(recursive, swapped.parameter).?);
    try std.testing.expect(graph.argument(root, requirement.parameter).?.closed != graph.argument(root, swapped.parameter).?.closed);
    try std.testing.expectEqual(@as(usize, 2), graph.workers.items[@intFromEnum(worker)].requirements.count());
}

test "boxy literal demands only request substitutions on demanded edges" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const needed = try graph.addWorker();
    const unused = try graph.addWorker();
    const a = try graph.terms.intern(.{ .variable = 0 });
    const fixed = try graph.terms.intern(.{ .application = .{ .constructor = 0, .args = &.{} } });
    _ = try graph.addDeferredEdge(null, unused);
    const root = try graph.addDeferredEdge(null, needed);
    const demand = try graph.demand(needed, .{ .site = graph.addSite(), .ty = a });
    try std.testing.expectError(error.MissingSubstitution, graph.solve());
    try std.testing.expectEqual(root, graph.pendingEdge());
    try graph.bindEdge(root, &.{.{ .variable = 0, .term = fixed }});
    try graph.solve();
    try std.testing.expectEqual(fixed, graph.requirements.items[@intFromEnum(graph.argument(root, demand.parameter).?.closed)].ty);
}

fn allocationFailureFixture(allocator: Allocator) (Allocator.Error || error{ UnboundRootRequirement, MissingSubstitution })!void {
    var graph = Graph.init(allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const caller = try graph.addWorker();
    const a = try graph.terms.intern(.{ .variable = 0 });
    const b = try graph.terms.intern(.{ .variable = 1 });
    const fixed = try graph.terms.intern(.{ .application = .{ .constructor = 0, .args = &.{} } });
    const nested = try graph.terms.intern(.{ .application = .{ .constructor = 1, .args = &.{a} } });
    _ = try graph.demand(worker, .{ .site = graph.addSite(), .ty = nested });
    _ = try graph.addEdge(caller, worker, &.{.{ .variable = 0, .term = b }});
    _ = try graph.addEdge(null, caller, &.{.{ .variable = 1, .term = fixed }});
    try graph.solve();
    var abi = try graph.freezeAbi();
    defer abi.deinit();
}

test "boxy literal demands release partial graphs on allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationFailureFixture, .{});
}

test "boxy literal evidence closes independently of its value type" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const caller = try graph.addWorker();
    const evidence_variable: u64 = @as(u64, 1) << 32;
    const value_type = try graph.terms.intern(.{ .application = .{ .constructor = 7, .args = &.{} } });
    const error_row = try graph.terms.intern(.{ .variable = 3 });
    const evidence = try graph.terms.intern(.{ .variable = evidence_variable });
    const method = try graph.terms.intern(.{ .application = .{ .constructor = 8, .args = &.{error_row} } });
    const empty_row = try graph.terms.intern(.{ .application = .{ .constructor = 9, .args = &.{} } });
    const other_row = try graph.terms.intern(.{ .application = .{ .constructor = 10, .args = &.{} } });
    const demand = try graph.demand(worker, .{ .site = graph.addSite(), .ty = value_type, .evidence = evidence });
    try std.testing.expect(demand == .parameter);
    const call = try graph.addEdge(caller, worker, &.{.{ .variable = evidence_variable, .term = method }});
    const first = try graph.addEdge(null, caller, &.{.{ .variable = 3, .term = empty_row }});
    const second = try graph.addEdge(null, caller, &.{.{ .variable = 3, .term = other_row }});
    try graph.solve();
    const forwarded = graph.argument(call, demand.parameter).?.parameter;
    try std.testing.expect(graph.argument(first, forwarded).?.closed != graph.argument(second, forwarded).?.closed);
    var abi = try graph.freezeAbi();
    defer abi.deinit();
    try std.testing.expectEqual(@as(usize, 2), abi.closed_requirements.len);
}

test "boxy literal ABI omits builtin-only sites throughout forwarding" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const caller = try graph.addWorker();
    const custom_site = graph.addSite();
    const builtin_site = graph.addSite();
    const variable = try graph.terms.intern(.{ .variable = 3 });
    const scalar = try graph.terms.intern(.{ .application = .{ .constructor = 4, .args = &.{} } });
    _ = try graph.demand(worker, .{ .site = builtin_site, .ty = variable });
    const custom = try graph.demand(worker, .{ .site = custom_site, .ty = variable });
    const forward = try graph.addEdge(caller, worker, &.{});
    const root = try graph.addEdge(null, caller, &.{.{ .variable = 3, .term = scalar }});
    try graph.solve();
    var abi = try graph.freezeAbiForSites(&.{ true, false });
    defer abi.deinit();
    try std.testing.expectEqualSlices(RequirementId, &.{custom.parameter}, abi.workerParameters(worker));
    try std.testing.expectEqualSlices(Argument, &.{.{ .parameter = 0 }}, abi.edgeArguments(forward));
    try std.testing.expectEqualSlices(Argument, &.{.{ .result = 0 }}, abi.edgeArguments(root));
    try std.testing.expectEqual(@as(usize, 1), abi.closed_requirements.len);
}

test "boxy freeze observations collect closed environments without imposing runtime obligations" {
    var graph = Graph.init(std.testing.allocator);
    defer graph.deinit();
    const worker = try graph.addWorker();
    const site = try graph.addObservationSite();
    const variable = try graph.terms.intern(.{ .variable = 0 });
    const concrete = try graph.terms.intern(.{ .application = .{ .constructor = 7, .args = &.{} } });
    _ = try graph.addEdge(null, worker, &.{});
    const closed_edge = try graph.addEdge(null, worker, &.{.{ .variable = 0, .term = concrete }});
    const demand = try graph.demand(worker, .{ .site = site, .ty = variable });
    try graph.solve();
    const observed = graph.argument(closed_edge, demand.parameter).?.closed;
    try std.testing.expectEqual(concrete, graph.requirements.items[@intFromEnum(observed)].ty);
    var abi = try graph.freezeAbi();
    defer abi.deinit();
    try std.testing.expectEqual(@as(usize, 0), abi.parameters.len);
    try std.testing.expectEqual(@as(usize, 0), abi.closed_requirements.len);
}
