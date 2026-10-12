//! Deterministic serial root scheduling from sealed producer demand facts.
//! Callable recursion and address capture are not root value dependencies.

const std = @import("std");
const base = @import("base");
const Program = @import("program.zig");
const LIR = @import("LIR.zig");
const Allocator = std.mem.Allocator;

/// Identifies a compile-time root by its module and producer.
pub const Key = struct {
    module: [32]u8,
    kind: enum { checked, literal },
    id: u32,

    pub fn init(module: [32]u8, producer: LIR.ComptimeProducer) Key {
        return switch (producer) {
            .checked => |id| .{ .module = module, .kind = .checked, .id = @backingInt(id) },
            .literal => |id| .{ .module = module, .kind = .literal, .id = @backingInt(id) },
        };
    }
};

/// A root to schedule, with the demand facts its producer sealed.
pub const Node = struct {
    key: Key,
    guarded: bool,
    region: base.Region,
    loc: ?base.SourceLoc,
    demand: Program.CtfeDemandFacts,
};

/// A value dependency of one node on the node at `target`.
pub const Edge = struct {
    target: u32,
    cause: Program.CtfeDemandRead,
};

/// Why `Schedule.build` could not schedule the given nodes.
pub const BuildError = Allocator.Error || error{
    MissingDemandProof,
    MissingRootProducer,
    DuplicateRootProducer,
};

/// The roots' dependency graph, condensed into components in evaluation order.
pub const Schedule = struct {
    allocator: Allocator,
    /// Borrowed source-owned nodes also retain the address-only nondependency
    /// map; scheduling never traverses those captures.
    nodes: []const Node,
    edge_starts: []u32,
    edges: []Edge,
    components: []u32,
    /// Components and their members are dependency-first. Members within a
    /// component are sorted by the producer's deterministic root ordinal.
    component_starts: []u32,
    members: []u32,
    cyclic: []bool,

    pub fn deinit(self: *Schedule) void {
        self.allocator.free(self.edge_starts);
        self.allocator.free(self.edges);
        self.allocator.free(self.components);
        self.allocator.free(self.component_starts);
        self.allocator.free(self.members);
        self.allocator.free(self.cyclic);
        self.* = undefined;
    }

    pub fn predecessors(self: *const Schedule, node: u32) []const Edge {
        return self.edges[self.edge_starts[node]..self.edge_starts[node + 1]];
    }

    pub fn componentMembers(self: *const Schedule, component: usize) []const u32 {
        return self.members[self.component_starts[component]..self.component_starts[component + 1]];
    }

    /// All failure headers for cycles become terminal before any executable
    /// root runs. The executor can publish/report, but cannot demand a root.
    pub fn run(self: *const Schedule, executor: anytype, comptime ExecutionError: type) (Allocator.Error || ExecutionError)!void {
        for (self.cyclic, 0..) |cyclic, component| {
            if (!cyclic) continue;
            for (self.componentMembers(component)) |node| {
                const source = self.nodes[node];
                var origin: LIR.ComptimeFailureOrigin = .{
                    .loc = if (source.loc) |loc| if (loc.hasLocation()) loc else null else null,
                    .region = source.region,
                };
                for (self.predecessors(node)) |edge| {
                    if (self.components[edge.target] != component) continue;
                    origin = .{ .loc = if (edge.cause.loc.hasLocation()) edge.cause.loc else null, .region = edge.cause.region };
                    break;
                }
                try executor.failCycle(node, origin);
            }
        }
        const done = try self.allocator.alloc(bool, self.nodes.len);
        defer self.allocator.free(done);
        for (self.components, done) |component, *completed| completed.* = self.cyclic[component];
        for (self.cyclic, 0..) |cyclic, component| {
            if (cyclic) continue;
            for (self.componentMembers(component)) |node| {
                for (self.predecessors(node)) |edge| {
                    if (!done[edge.target]) @panic("CTFE schedule read an unfinished predecessor");
                }
                try executor.execute(node);
                done[node] = true;
            }
        }
    }

    /// Completed declarations belong to the producer's imported-value manifest,
    /// not to a dependency guessed from a static-data symbol or erased body.
    pub fn build(allocator: Allocator, nodes: []const Node, completed: []const Key) BuildError!Schedule {
        var index = std.AutoHashMap(Key, u32).init(allocator);
        defer index.deinit();
        var terminal = std.AutoHashMap(Key, void).init(allocator);
        defer terminal.deinit();
        for (completed) |key| try terminal.put(key, {});
        for (nodes, 0..) |node, ordinal| {
            if (!node.demand.complete) return error.MissingDemandProof;
            const entry = try index.getOrPut(node.key);
            if (entry.found_existing) return error.DuplicateRootProducer;
            entry.value_ptr.* = @intCast(ordinal);
        }
        const starts = try allocator.alloc(u32, nodes.len + 1);
        errdefer allocator.free(starts);
        var edges = std.ArrayList(Edge).empty;
        errdefer edges.deinit(allocator);
        for (nodes, 0..) |node, ordinal| {
            starts[ordinal] = @intCast(edges.items.len);
            for (node.demand.reads) |read| {
                const key = Key.init(read.module.bytes, read.producer);
                if (index.get(key)) |target| {
                    try edges.append(allocator, .{ .target = target, .cause = read });
                } else if (!read.completed and !terminal.contains(key)) return error.MissingRootProducer;
            }
        }
        starts[nodes.len] = @intCast(edges.items.len);

        const unseen = std.math.maxInt(u32);
        const State = struct { index: u32 = unseen, low: u32 = unseen, on_stack: bool = false };
        const Frame = struct { node: u32, edge: u32 };
        const states = try allocator.alloc(State, nodes.len);
        defer allocator.free(states);
        @memset(states, .{});
        const components = try allocator.alloc(u32, nodes.len);
        errdefer allocator.free(components);
        @memset(components, unseen);
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(allocator);
        var stack = std.ArrayList(u32).empty;
        defer stack.deinit(allocator);
        var members = std.ArrayList(u32).empty;
        errdefer members.deinit(allocator);
        var component_starts = std.ArrayList(u32).empty;
        errdefer component_starts.deinit(allocator);
        var cyclic = std.ArrayList(bool).empty;
        errdefer cyclic.deinit(allocator);
        var next: u32 = 0;
        for (0..nodes.len) |start_index| {
            const start: u32 = @intCast(start_index);
            if (states[start].index != unseen) continue;
            states[start] = .{ .index = next, .low = next, .on_stack = true };
            next += 1;
            try stack.append(allocator, start);
            try frames.append(allocator, .{ .node = start, .edge = starts[start] });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                if (frame.edge < starts[node + 1]) {
                    const target = edges.items[frame.edge].target;
                    frame.edge += 1;
                    if (states[target].index == unseen) {
                        states[target] = .{ .index = next, .low = next, .on_stack = true };
                        next += 1;
                        try stack.append(allocator, target);
                        try frames.append(allocator, .{ .node = target, .edge = starts[target] });
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
                const member_start = members.items.len;
                const component: u32 = @intCast(cyclic.items.len);
                try component_starts.append(allocator, @intCast(member_start));
                while (true) {
                    const member = stack.pop() orelse unreachable;
                    states[member].on_stack = false;
                    components[member] = component;
                    try members.append(allocator, member);
                    if (member == node) break;
                }
                std.mem.sort(u32, members.items[member_start..], {}, std.sort.asc(u32));
                var is_cycle = members.items.len - member_start > 1;
                if (!is_cycle) {
                    for (edges.items[starts[node]..starts[node + 1]]) |edge| {
                        if (edge.target == node) is_cycle = true;
                    }
                }
                try cyclic.append(allocator, is_cycle);
            }
        }
        try component_starts.append(allocator, @intCast(members.items.len));
        const owned_edges = try edges.toOwnedSlice(allocator);
        errdefer allocator.free(owned_edges);
        const owned_members = try members.toOwnedSlice(allocator);
        errdefer allocator.free(owned_members);
        const owned_starts = try component_starts.toOwnedSlice(allocator);
        errdefer allocator.free(owned_starts);
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .edge_starts = starts,
            .edges = owned_edges,
            .components = components,
            .component_starts = owned_starts,
            .members = owned_members,
            .cyclic = try cyclic.toOwnedSlice(allocator),
        };
    }
};

fn testNode(module: u8, producer: LIR.ComptimeProducer, reads: []const Program.CtfeDemandRead) Node {
    return .{
        .key = Key.init(@splat(module), producer),
        .guarded = false,
        .region = .zero(),
        .loc = .none,
        .demand = .{ .complete = true, .reads = reads },
    };
}

fn testRead(module: u8, producer: LIR.ComptimeProducer) Program.CtfeDemandRead {
    return .{ .module = .{ .bytes = @splat(module) }, .producer = producer, .region = .zero(), .loc = .none };
}

test "CTFE schedule crosses modules and literal roots dependency first" {
    const a: LIR.ComptimeProducer = .{ .checked = @fromBackingInt(@intCast(4)) };
    const b: LIR.ComptimeProducer = .{ .literal = @fromBackingInt(@intCast(4)) };
    const nodes = [_]Node{
        testNode(1, a, &.{testRead(2, a)}),
        testNode(2, a, &.{testRead(2, b)}),
        testNode(2, b, &.{}),
    };
    var schedule = try Schedule.build(std.testing.allocator, &nodes, &.{});
    defer schedule.deinit();
    try std.testing.expectEqualSlices(u32, &.{ 2, 1, 0 }, schedule.members);
    try std.testing.expectEqualSlices(bool, &.{ false, false, false }, schedule.cyclic);
}

test "CTFE schedule classifies cycles before execution and ignores address captures" {
    const a: LIR.ComptimeProducer = .{ .checked = @fromBackingInt(@intCast(2)) };
    const b: LIR.ComptimeProducer = .{ .checked = @fromBackingInt(@intCast(1)) };
    var nodes = [_]Node{
        testNode(1, a, &.{testRead(1, b)}),
        testNode(1, b, &.{testRead(1, a)}),
        testNode(2, a, &.{}),
    };
    nodes[2].demand.address_captures = &.{ 0, 1 };
    var schedule = try Schedule.build(std.testing.allocator, &nodes, &.{});
    defer schedule.deinit();
    try std.testing.expectEqualSlices(bool, &.{ true, false }, schedule.cyclic);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1 }, schedule.componentMembers(0));
    try std.testing.expectEqual(@as(usize, 0), schedule.predecessors(2).len);
}

test "CTFE schedule rejects missing proof and undeclared producers" {
    const root: LIR.ComptimeProducer = .{ .checked = @fromBackingInt(@intCast(4)) };
    var node = testNode(1, root, &.{testRead(2, root)});
    try std.testing.expectError(error.MissingRootProducer, Schedule.build(std.testing.allocator, &.{node}, &.{}));
    var schedule = try Schedule.build(std.testing.allocator, &.{node}, &.{Key.init(@splat(2), root)});
    schedule.deinit();
    node.demand.complete = false;
    try std.testing.expectError(error.MissingDemandProof, Schedule.build(std.testing.allocator, &.{node}, &.{}));
}

test "CTFE schedule publishes all cycle failures before serial execution" {
    const root = struct {
        fn of(id: u32) LIR.ComptimeProducer {
            return .{ .checked = @fromBackingInt(@intCast(id)) };
        }
    }.of;
    const nodes = [_]Node{
        testNode(1, root(0), &.{}),
        testNode(1, root(1), &.{testRead(1, root(2))}),
        testNode(1, root(2), &.{testRead(1, root(1))}),
        testNode(1, root(3), &.{testRead(1, root(1))}),
    };
    var schedule = try Schedule.build(std.testing.allocator, &nodes, &.{});
    defer schedule.deinit();
    const Recorder = struct {
        events: [4]u32 = undefined,
        count: usize = 0,

        pub fn failCycle(self: *@This(), node: u32, _: LIR.ComptimeFailureOrigin) error{TestUnexpectedResult}!void {
            try std.testing.expect(node == 1 or node == 2);
            self.events[self.count] = 100 + node;
            self.count += 1;
        }

        pub fn execute(self: *@This(), node: u32) error{TestUnexpectedResult}!void {
            try std.testing.expect(node == 0 or node == 3);
            try std.testing.expect(self.count >= 2);
            self.events[self.count] = node;
            self.count += 1;
        }
    };
    var recorder = Recorder{};
    try schedule.run(&recorder, error{TestUnexpectedResult});
    try std.testing.expectEqualSlices(u32, &.{ 101, 102, 0, 3 }, &recorder.events);
}
