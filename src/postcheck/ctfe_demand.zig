//! Producer-owned expression demand data retained independently of lowered bodies.
//!
//! Evaluation follows applications, not function addresses or representation
//! witnesses. Keeping those relations distinct permits recursive callables
//! without turning their construction into an eager value cycle.

const std = @import("std");
const base = @import("base");
const Common = @import("common.zig");

/// A node of the demand graph: an expression, statement, or function.
pub const NodeId = enum(u32) { _ };

/// How the source of an `Edge` depends on its target.
pub const Relation = enum {
    evaluation,
    application,
    address_capture,
    representation,
};

/// A demand relation from one node to another.
pub const Edge = struct {
    from: NodeId,
    to: NodeId,
    relation: Relation,
};

/// A node's read of a compile-time root's value, with its source location.
pub const Read = struct {
    node: NodeId,
    root: Common.ComptimeValueRoot,
    region: base.Region,
    loc: base.SourceLoc,
};

/// A late application retains the already-produced callable type slot until
/// lambda solving closes its exact members. It is not a function-type guess.
pub const LateApplication = struct {
    node: NodeId,
    callee_expr: u32,
};

/// Only touched keys are enumerated. An empty component never scans the
/// capacity retained from a previous broad component.
const ScratchSet = struct {
    allocator: std.mem.Allocator,
    marks: []u32,
    generation: u32 = 0,
    keys: std.ArrayList(u32) = .empty,
    work: u64,

    fn init(allocator: std.mem.Allocator, count: usize) std.mem.Allocator.Error!ScratchSet {
        const marks = try allocator.alloc(u32, count);
        @memset(marks, std.math.maxInt(u32));
        return .{ .allocator = allocator, .marks = marks, .work = count };
    }

    fn deinit(self: *ScratchSet) void {
        self.allocator.free(self.marks);
        self.keys.deinit(self.allocator);
    }

    fn reset(self: *ScratchSet, generation: u32) void {
        self.generation = generation;
        self.keys.clearRetainingCapacity();
        self.work += 1;
    }

    fn put(self: *ScratchSet, key: u32) std.mem.Allocator.Error!void {
        self.work += 1;
        if (self.marks[key] == self.generation) return;
        try self.keys.append(self.allocator, key);
        self.marks[key] = self.generation;
    }

    fn sortedKeys(self: *ScratchSet) std.mem.Allocator.Error![]u32 {
        self.work += self.keys.items.len;
        const keys = try self.allocator.dupe(u32, self.keys.items);
        std.mem.sort(u32, keys, {}, std.sort.asc(u32));
        return keys;
    }
};

/// Read-only view of a graph's demand data. Edges and reads are grouped by source
/// node; `edge_starts` and `read_starts` index each node's group.
pub const View = struct {
    node_count: u32,
    edges: []const Edge,
    reads: []const Read,
    late_applications: []const LateApplication,
    unavailable: []const NodeId,
    edge_starts: []const u32,
    read_starts: []const u32,
};

/// The solving traversal records demand data as it processes expressions and
/// statements. Consumers never need to inspect a retired lowered body.
pub const Graph = struct {
    allocator: std.mem.Allocator,
    node_count: u32,
    edges: std.ArrayList(Edge) = .empty,
    reads: std.ArrayList(Read) = .empty,
    late_applications: std.ArrayList(LateApplication) = .empty,
    unavailable: std.ArrayList(NodeId) = .empty,
    edge_starts: []u32 = &.{},
    read_starts: []u32 = &.{},
    components: []u32 = &.{},
    summaries: std.ArrayList(Summary) = .empty,
    /// Each component's dependency components, grouped by component; a
    /// component whose summary is not materialized is resolved through them
    /// when a summarizable component that reaches it is materialized.
    component_dependencies: std.ArrayList(u32) = .empty,
    component_dependency_starts: []u32 = &.{},
    /// Nodes at or past this index may be summarized; the rest (a program's
    /// expressions and statements) only carry demand data for them.
    summary_start: u32 = 0,
    closure_scratch_work: u64 = 0,

    pub fn init(allocator: std.mem.Allocator, node_count: u32) Graph {
        return .{ .allocator = allocator, .node_count = node_count };
    }

    /// A graph whose nodes at or past `summary_start` are the only ones
    /// `summarize` is asked about. The transitive reads of every other node
    /// are never materialized on their own.
    pub fn initWithSummaryStart(allocator: std.mem.Allocator, node_count: u32, summary_start: u32) Graph {
        return .{ .allocator = allocator, .node_count = node_count, .summary_start = summary_start };
    }

    pub fn deinit(self: *Graph) void {
        self.edges.deinit(self.allocator);
        self.reads.deinit(self.allocator);
        self.late_applications.deinit(self.allocator);
        self.unavailable.deinit(self.allocator);
        self.allocator.free(self.edge_starts);
        self.allocator.free(self.read_starts);
        self.allocator.free(self.components);
        for (self.summaries.items) |*summary| summary.deinit(self.allocator);
        self.summaries.deinit(self.allocator);
        self.component_dependencies.deinit(self.allocator);
        self.allocator.free(self.component_dependency_starts);
        self.* = undefined;
    }

    pub fn clone(self: *const Graph, allocator: std.mem.Allocator) std.mem.Allocator.Error!Graph {
        var result = Graph.initWithSummaryStart(allocator, self.node_count, self.summary_start);
        errdefer result.deinit();
        try result.component_dependencies.appendSlice(allocator, self.component_dependencies.items);
        result.component_dependency_starts = try allocator.dupe(u32, self.component_dependency_starts);
        try result.edges.appendSlice(allocator, self.edges.items);
        try result.reads.appendSlice(allocator, self.reads.items);
        try result.late_applications.appendSlice(allocator, self.late_applications.items);
        try result.unavailable.appendSlice(allocator, self.unavailable.items);
        result.edge_starts = try allocator.dupe(u32, self.edge_starts);
        result.read_starts = try allocator.dupe(u32, self.read_starts);
        result.components = try allocator.dupe(u32, self.components);
        result.closure_scratch_work = self.closure_scratch_work;
        for (self.summaries.items) |summary| {
            const reads = try allocator.dupe(u32, summary.reads);
            errdefer allocator.free(reads);
            try result.summaries.append(allocator, .{
                .reads = reads,
                .complete = summary.complete,
                .materialized = summary.materialized,
            });
        }
        return result;
    }

    pub fn view(self: *const Graph) View {
        return .{
            .node_count = self.node_count,
            .edges = self.edges.items,
            .reads = self.reads.items,
            .late_applications = self.late_applications.items,
            .unavailable = self.unavailable.items,
            .edge_starts = self.edge_starts,
            .read_starts = self.read_starts,
        };
    }

    pub fn addEdge(self: *Graph, from: NodeId, to: NodeId, relation: Relation) std.mem.Allocator.Error!void {
        self.requireOpen();
        self.validate(from);
        self.validate(to);
        try self.edges.append(self.allocator, .{ .from = from, .to = to, .relation = relation });
    }

    pub fn addRead(self: *Graph, read: Read) std.mem.Allocator.Error!void {
        self.requireOpen();
        self.validate(read.node);
        try self.reads.append(self.allocator, read);
    }

    pub fn addLateApplication(self: *Graph, application: LateApplication) std.mem.Allocator.Error!void {
        self.requireOpen();
        self.validate(application.node);
        try self.late_applications.append(self.allocator, application);
    }

    /// A retired cached body must supply its recorded summary. Missing data
    /// are explicit incompleteness, never an empty dependency proof.
    pub fn addUnavailable(self: *Graph, node: NodeId) std.mem.Allocator.Error!void {
        self.requireOpen();
        self.validate(node);
        try self.unavailable.append(self.allocator, node);
    }

    pub fn seal(self: *Graph) std.mem.Allocator.Error!void {
        self.requireOpen();
        const seal_started = if (@import("builtin").os.tag != .freestanding and std.c.getenv("ROC_PACK_STATS") != null) statsNowNs() else 0;
        defer if (seal_started != 0) std.debug.print("ctfe demand seal: nodes={d} edges={d} reads={d} {d} ms\n", .{ self.node_count, self.edges.items.len, self.reads.items.len, @divTrunc(statsNowNs() - seal_started, std.time.ns_per_ms) });
        const edge_starts = try self.allocator.alloc(u32, @as(usize, self.node_count) + 1);
        errdefer self.allocator.free(edge_starts);
        const read_starts = try self.allocator.alloc(u32, @as(usize, self.node_count) + 1);
        errdefer self.allocator.free(read_starts);
        std.mem.sort(Edge, self.edges.items, {}, struct {
            fn less(_: void, a: Edge, b: Edge) bool {
                if (a.from != b.from) return @backingInt(a.from) < @backingInt(b.from);
                if (a.to != b.to) return @backingInt(a.to) < @backingInt(b.to);
                return @backingInt(a.relation) < @backingInt(b.relation);
            }
        }.less);
        std.mem.sort(Read, self.reads.items, {}, struct {
            fn less(_: void, a: Read, b: Read) bool {
                return @backingInt(a.node) < @backingInt(b.node);
            }
        }.less);
        var edge_cursor: u32 = 0;
        var read_cursor: u32 = 0;
        for (0..self.node_count) |node| {
            edge_starts[node] = edge_cursor;
            read_starts[node] = read_cursor;
            while (edge_cursor < self.edges.items.len and @backingInt(self.edges.items[edge_cursor].from) == node) edge_cursor += 1;
            while (read_cursor < self.reads.items.len and @backingInt(self.reads.items[read_cursor].node) == node) read_cursor += 1;
        }
        edge_starts[self.node_count] = edge_cursor;
        read_starts[self.node_count] = read_cursor;
        self.edge_starts = edge_starts;
        self.read_starts = read_starts;
        errdefer {
            self.edge_starts = &.{};
            self.read_starts = &.{};
        }
        try self.buildSharedSummaries();
    }

    pub const Summary = struct {
        /// Every retained read cause, not merely a root-deduplicated set.
        reads: []u32,
        complete: bool,
        /// Whether `reads` and `complete` cover everything the component
        /// reaches; a component no summarizable node belongs to keeps only
        /// its own reads and availability.
        materialized: bool = true,

        pub fn deinit(self: *Summary, allocator: std.mem.Allocator) void {
            allocator.free(self.reads);
            self.* = undefined;
        }
    };

    /// Copy the already-computed component demand data for a callable/root owner.
    /// No consumer performs a separate graph traversal per owner.
    pub fn summarize(self: *const Graph, allocator: std.mem.Allocator, owner: NodeId) std.mem.Allocator.Error!Summary {
        self.validate(owner);
        if (self.edge_starts.len == 0) Common.invariant("CTFE demand summary requested before producer seal");
        if (@backingInt(owner) < self.summary_start) Common.invariant("CTFE demand summary requested for a node the graph does not summarize");
        const summary = self.summaries.items[self.components[@backingInt(owner)]];
        if (!summary.materialized) Common.invariant("CTFE demand summary of a summarizable node was not materialized");
        return .{
            .reads = try allocator.dupe(u32, summary.reads),
            .complete = summary.complete,
        };
    }

    /// Iterative Tarjan visits each execution edge once. Component closures
    /// are then shared by all recursive members and merged dependency-first.
    /// Work beyond SCC discovery is proportional to propagated demand data, not
    /// repeated walks of chains with no demand data.
    fn buildSharedSummaries(self: *Graph) std.mem.Allocator.Error!void {
        const unseen = std.math.maxInt(u32);
        const State = struct { index: u32 = unseen, low: u32 = unseen, on_stack: bool = false, unavailable: bool = false };
        const Frame = struct { node: u32, edge: u32 };
        const states = try self.allocator.alloc(State, self.node_count);
        defer self.allocator.free(states);
        @memset(states, .{});
        for (self.unavailable.items) |node| states[@backingInt(node)].unavailable = true;
        const components = try self.allocator.alloc(u32, self.node_count);
        errdefer self.allocator.free(components);
        @memset(components, unseen);
        var summaries = std.ArrayList(Summary).empty;
        errdefer {
            for (summaries.items) |*summary| summary.deinit(self.allocator);
            summaries.deinit(self.allocator);
        }
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(self.allocator);
        var stack = std.ArrayList(u32).empty;
        defer stack.deinit(self.allocator);
        var members = std.ArrayList(u32).empty;
        defer members.deinit(self.allocator);
        var read_set = try ScratchSet.init(self.allocator, self.reads.items.len);
        defer read_set.deinit();
        var dependency_set = try ScratchSet.init(self.allocator, self.node_count);
        defer dependency_set.deinit();
        var visited_set = try ScratchSet.init(self.allocator, self.node_count);
        defer visited_set.deinit();
        var pending = std.ArrayList(u32).empty;
        defer pending.deinit(self.allocator);
        var dependency_starts = std.ArrayList(u32).empty;
        errdefer dependency_starts.deinit(self.allocator);
        var next_index: u32 = 0;
        for (0..self.node_count) |start_index| {
            const start: u32 = @intCast(start_index);
            if (states[start].index != unseen) continue;
            states[start].index = next_index;
            states[start].low = next_index;
            states[start].on_stack = true;
            next_index += 1;
            try stack.append(self.allocator, start);
            try frames.append(self.allocator, .{ .node = start, .edge = self.edge_starts[start] });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                if (frame.edge < self.edge_starts[node + 1]) {
                    const edge = self.edges.items[frame.edge];
                    frame.edge += 1;
                    if (edge.relation != .evaluation and edge.relation != .application) continue;
                    const target = @backingInt(edge.to);
                    if (states[target].index == unseen) {
                        states[target].index = next_index;
                        states[target].low = next_index;
                        states[target].on_stack = true;
                        next_index += 1;
                        try stack.append(self.allocator, target);
                        try frames.append(self.allocator, .{ .node = target, .edge = self.edge_starts[target] });
                    } else if (states[target].on_stack) {
                        states[node].low = @min(states[node].low, states[target].index);
                    }
                    continue;
                }
                _ = frames.pop();
                if (frames.items.len != 0) {
                    const parent = frames.items[frames.items.len - 1].node;
                    states[parent].low = @min(states[parent].low, states[node].low);
                }
                if (states[node].low != states[node].index) continue;
                members.clearRetainingCapacity();
                const component: u32 = @intCast(summaries.items.len);
                while (true) {
                    const member = stack.pop() orelse Common.invariant("CTFE demand component stack underflow");
                    states[member].on_stack = false;
                    components[member] = component;
                    try members.append(self.allocator, member);
                    if (member == node) break;
                }
                read_set.reset(component);
                dependency_set.reset(component);
                var complete = true;
                var summarizable = false;
                for (members.items) |member| {
                    if (states[member].unavailable) complete = false;
                    if (member >= self.summary_start) summarizable = true;
                    for (self.read_starts[member]..self.read_starts[member + 1]) |read| try read_set.put(@intCast(read));
                    for (self.edge_starts[member]..self.edge_starts[member + 1]) |edge_index| {
                        const edge = self.edges.items[edge_index];
                        switch (edge.relation) {
                            .address_capture, .representation => {},
                            .evaluation, .application => {
                                const dependency = components[@backingInt(edge.to)];
                                if (dependency == unseen) Common.invariant("CTFE demand component consumed an unfinished predecessor");
                                if (dependency != component) try dependency_set.put(dependency);
                            },
                        }
                    }
                }
                dependency_set.work += dependency_set.keys.items.len;
                try dependency_starts.append(self.allocator, @intCast(self.component_dependencies.items.len));
                try self.component_dependencies.appendSlice(self.allocator, dependency_set.keys.items);
                if (summarizable) {
                    // The closure over the dependency components: a
                    // materialized one stands for everything it reaches, an
                    // unmaterialized one contributes its own demand data and is
                    // followed through its dependencies.
                    visited_set.reset(component);
                    pending.clearRetainingCapacity();
                    try pending.appendSlice(self.allocator, dependency_set.keys.items);
                    while (pending.pop()) |dependency| {
                        if (visited_set.marks[dependency] == visited_set.generation) continue;
                        try visited_set.put(dependency);
                        const summary = summaries.items[dependency];
                        complete = complete and summary.complete;
                        for (summary.reads) |read| try read_set.put(read);
                        if (summary.materialized) continue;
                        const dependency_start = dependency_starts.items[dependency];
                        const dependency_end = if (dependency + 1 < dependency_starts.items.len) dependency_starts.items[dependency + 1] else self.component_dependencies.items.len;
                        try pending.appendSlice(self.allocator, self.component_dependencies.items[dependency_start..dependency_end]);
                    }
                }
                const reads = try read_set.sortedKeys();
                errdefer self.allocator.free(reads);
                try summaries.append(self.allocator, .{ .reads = reads, .complete = complete, .materialized = summarizable });
            }
        }
        // Nothing may fail once the graph owns the results, or its deinit
        // would free them a second time.
        const owned_dependency_starts = try dependency_starts.toOwnedSlice(self.allocator);
        self.components = components;
        self.summaries = summaries;
        self.component_dependency_starts = owned_dependency_starts;
        self.closure_scratch_work = read_set.work + dependency_set.work + visited_set.work;
    }

    fn requireOpen(self: *const Graph) void {
        if (self.edge_starts.len != 0) Common.invariant("sealed CTFE demand producer was mutated");
    }

    fn validate(self: *const Graph, node: NodeId) void {
        if (@backingInt(node) >= self.node_count) Common.invariant("CTFE demand referenced an unrecorded producer node");
    }
};

/// The dense node ids of a test graph, in recording order.
fn testNodes(comptime count: usize) [count]NodeId {
    var nodes: [count]NodeId = undefined;
    for (&nodes, 0..) |*node, index| node.* = @fromBackingInt(@intCast(index));
    return nodes;
}

test "CTFE demand recursive applications exclude captured and representation-only bodies" {
    const allocator = std.testing.allocator;
    const nodes = testNodes(5);
    const owner, const callee, const captured, const representation, const evaluated = nodes;
    var graph = Graph.init(allocator, nodes.len);
    defer graph.deinit();
    try graph.addEdge(owner, callee, .application);
    try graph.addEdge(callee, owner, .application);
    try graph.addEdge(owner, captured, .address_capture);
    try graph.addEdge(callee, representation, .representation);
    try graph.addEdge(callee, evaluated, .evaluation);
    const root: Common.ComptimeValueRoot = .{
        .module = .{ .bytes = @as([32]u8, @splat(5)) },
        .root = .{ .literal = @fromBackingInt(@intCast(2)) },
        .const_locator = null,
    };
    for ([_]NodeId{ captured, representation, evaluated }) |node| {
        try graph.addRead(.{ .node = node, .root = root, .region = .zero(), .loc = .none });
    }
    try graph.addUnavailable(captured);
    try graph.seal();
    var summary = try graph.summarize(allocator, owner);
    defer summary.deinit(allocator);
    try std.testing.expect(summary.complete);
    try std.testing.expectEqual(@as(usize, 1), summary.reads.len);
    try std.testing.expectEqual(evaluated, graph.reads.items[summary.reads[0]].node);
    var incomplete = try graph.summarize(allocator, captured);
    defer incomplete.deinit(allocator);
    try std.testing.expect(!incomplete.complete);
}

test "CTFE demand retired comparator propagates incompleteness only through application" {
    const allocator = std.testing.allocator;
    const nodes = testNodes(4);
    const sort_owner, const box_owner, const dispatch_object, const comparator = nodes;
    var graph = Graph.init(allocator, nodes.len);
    defer graph.deinit();
    // Both owners evaluate the same boxed dispatch object. Only sorting
    // invokes its comparator, whose retired body has no complete contract.
    try graph.addEdge(sort_owner, dispatch_object, .evaluation);
    try graph.addEdge(box_owner, dispatch_object, .evaluation);
    try graph.addEdge(dispatch_object, comparator, .address_capture);
    try graph.addEdge(sort_owner, comparator, .application);
    try graph.addUnavailable(comparator);
    try graph.seal();
    var sort = try graph.summarize(allocator, sort_owner);
    defer sort.deinit(allocator);
    var box = try graph.summarize(allocator, box_owner);
    defer box.deinit(allocator);
    try std.testing.expect(!sort.complete);
    try std.testing.expect(box.complete);
}

test "CTFE demand shares component closure facts along long sorted chains" {
    const allocator = std.testing.allocator;
    const count = 20_000;
    var graph = Graph.init(allocator, count);
    defer graph.deinit();
    for (0..count - 1) |index| {
        try graph.addEdge(@fromBackingInt(@intCast(index)), @fromBackingInt(@intCast(index + 1)), .evaluation);
    }
    try graph.addRead(.{
        .node = @fromBackingInt(@intCast(count - 1)),
        .root = .{
            .module = .{ .bytes = @as([32]u8, @splat(11)) },
            .root = .{ .literal = @fromBackingInt(@intCast(3)) },
            .const_locator = null,
        },
        .region = .zero(),
        .loc = .none,
    });
    try graph.seal();
    try std.testing.expectEqual(@as(usize, count), graph.summaries.items.len);
    for (0..count) |index| {
        const summary = graph.summaries.items[graph.components[index]];
        try std.testing.expect(summary.complete);
        try std.testing.expectEqualSlices(u32, &.{0}, summary.reads);
        if (index + 1 < count) try std.testing.expect(graph.components[index] > graph.components[index + 1]);
    }
}

test "CTFE demand shared summaries and clone release partial allocation failures" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, exerciseDemandAllocations, .{});
}

test "CTFE demand scratch work stays linear after fanout followed by isolated nodes" {
    for ([_]u32{ 128, 1024, 8192 }) |width| {
        const allocator = std.testing.allocator;
        var graph = Graph.init(allocator, 2 * width + 1);
        defer graph.deinit();
        const hub, const first_target = testNodes(2);
        for (0..width) |offset| {
            try graph.addEdge(hub, @fromBackingInt(@intCast(@backingInt(first_target) + offset)), .application);
        }
        try graph.seal();
        // Two generation resets per component, domain initialization, and
        // one insertion plus one dense visit per outgoing dependency.
        try std.testing.expectEqual(@as(u64, 13) * width + 5, graph.closure_scratch_work);
    }
}

fn exerciseDemandAllocations(allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    const nodes = testNodes(4);
    const owner, const callee, const evaluated, const captured = nodes;
    var graph = Graph.init(allocator, nodes.len);
    defer graph.deinit();
    try graph.addEdge(owner, callee, .application);
    try graph.addEdge(callee, owner, .application);
    try graph.addEdge(callee, evaluated, .evaluation);
    try graph.addEdge(callee, captured, .address_capture);
    try graph.addRead(.{
        .node = evaluated,
        .root = .{
            .module = .{ .bytes = @as([32]u8, @splat(12)) },
            .root = .{ .checked = @fromBackingInt(@intCast(3)) },
            .const_locator = null,
        },
        .region = .zero(),
        .loc = .none,
    });
    try graph.addUnavailable(evaluated);
    try graph.seal();
    var copy = try graph.clone(allocator);
    defer copy.deinit();
    var summary = try copy.summarize(allocator, owner);
    defer summary.deinit(allocator);
}

/// Monotonic nanoseconds for the `ROC_PACK_STATS` stage reports.
fn statsNowNs() i128 {
    var ts: std.c.timespec = undefined;
    _ = std.c.clock_gettime(.MONOTONIC, &ts);
    return @as(i128, ts.sec) * 1_000_000_000 + @as(i128, ts.nsec);
}
