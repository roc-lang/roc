//! Immutable source-type graph facts shared by checked publication and key encoding.
//! Iterative Tarjan traversal computes facts for whole strongly connected
//! components before publishing them, including identities reached after a back edge.
const std = @import("std");
const types = @import("types");
const Allocator = std.mem.Allocator;
const Var = types.Var;

/// Properties of the graph reachable from one resolved source variable.
pub const Facts = struct {
    contains_identity_variables: bool = false,
    contains_cycle: bool = false,

    /// Only closed acyclic graphs have context-independent encodings.
    pub fn isComposable(self: Facts) bool {
        return !self.contains_identity_variables and !self.contains_cycle;
    }

    fn merge(self: *Facts, other: Facts) void {
        self.contains_identity_variables = self.contains_identity_variables or other.contains_identity_variables;
        self.contains_cycle = self.contains_cycle or other.contains_cycle;
    }
};

/// Retained scratch and completed facts for one immutable source store.
pub const Analysis = struct {
    const Node = struct { index: u32, low: u32, facts: Facts, complete: bool = false };
    const Frame = struct { root: Var, children_base: usize, next: usize, end: usize, facts: Facts = .{} };

    allocator: Allocator,
    nodes: std.AutoHashMap(Var, Node),
    members: std.ArrayList(Var) = .empty,
    frames: std.ArrayList(Frame) = .empty,
    children: std.ArrayList(Var) = .empty,
    next_index: u32 = 0,

    /// Initialize an analysis whose facts live only as long as the store is immutable.
    pub fn init(allocator: Allocator) Analysis {
        return .{ .allocator = allocator, .nodes = std.AutoHashMap(Var, Node).init(allocator) };
    }

    /// Release graph facts and traversal scratch.
    pub fn deinit(self: *Analysis) void {
        self.nodes.deinit();
        self.members.deinit(self.allocator);
        self.frames.deinit(self.allocator);
        self.children.deinit(self.allocator);
    }

    /// Start a new immutable-store query scope, retaining allocation capacity.
    pub fn resetRetainingCapacity(self: *Analysis) void {
        self.clearPending();
        self.nodes.clearRetainingCapacity();
    }

    fn clearPending(self: *Analysis) void {
        for (self.members.items) |member| _ = self.nodes.remove(member);
        self.members.clearRetainingCapacity();
        self.frames.clearRetainingCapacity();
        self.children.clearRetainingCapacity();
        self.next_index = 0;
    }

    /// Analyze without native-stack recursion. Failed queries discard unfinished
    /// components; completed components remain valid for this immutable store.
    pub fn analyze(self: *Analysis, store: *const types.Store, var_: Var) Allocator.Error!Facts {
        const root = store.resolveVar(var_).var_;
        if (self.nodes.get(root)) |node| {
            std.debug.assert(node.complete);
            return node.facts;
        }
        std.debug.assert(self.frames.items.len == 0);
        errdefer self.clearPending();
        if (try self.push(store, root)) |facts| return facts;
        while (self.frames.items.len != 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            if (frame.next < frame.end) {
                const child = store.resolveVar(self.children.items[frame.next]).var_;
                frame.next += 1;
                if (self.nodes.get(child)) |node| {
                    if (node.complete) {
                        frame.facts.merge(node.facts);
                    } else {
                        const parent = self.nodes.getPtr(frame.root).?;
                        parent.low = @min(parent.low, node.index);
                    }
                } else {
                    if (try self.push(store, child)) |facts| frame.facts.merge(facts);
                }
                continue;
            }
            const finished = self.frames.pop().?;
            self.children.items.len = finished.children_base;
            const node = self.nodes.getPtr(finished.root).?;
            node.facts = finished.facts;
            const low = node.low;
            const index = node.index;
            if (low == index) {
                var first = self.members.items.len - 1;
                while (self.members.items[first] != finished.root) first -= 1;
                var facts = finished.facts;
                facts.contains_cycle = facts.contains_cycle or self.members.items.len - first > 1;
                for (self.members.items[first..]) |member| facts.merge(self.nodes.get(member).?.facts);
                for (self.members.items[first..]) |member| {
                    const completed = self.nodes.getPtr(member).?;
                    completed.facts = facts;
                    completed.complete = true;
                }
                self.members.items.len = first;
            }
            if (self.frames.items.len != 0) {
                const parent_frame = &self.frames.items[self.frames.items.len - 1];
                const finished_node = self.nodes.get(finished.root).?;
                if (finished_node.complete) {
                    parent_frame.facts.merge(finished_node.facts);
                } else {
                    const parent = self.nodes.getPtr(parent_frame.root).?;
                    parent.low = @min(parent.low, low);
                }
            }
        }
        self.next_index = 0;
        return self.nodes.get(root).?.facts;
    }

    fn push(self: *Analysis, types_store: *const types.Store, root: Var) Allocator.Error!?Facts {
        const resolved = types_store.resolveVar(root);
        var frame = Frame{ .root = root, .children_base = self.children.items.len, .next = self.children.items.len, .end = undefined };
        try self.collect(types_store, resolved, &frame);
        frame.end = self.children.items.len;
        // A node without child edges is already a complete component. Publish
        // it directly instead of opening and immediately closing traversal state.
        if (frame.end == frame.children_base) {
            try self.nodes.put(root, .{
                .index = self.next_index,
                .low = self.next_index,
                .facts = frame.facts,
                .complete = true,
            });
            return frame.facts;
        }
        for (self.children.items[frame.children_base..frame.end]) |child| {
            if (types_store.resolveVar(child).var_ == root) frame.facts.contains_cycle = true;
        }
        try self.nodes.put(root, .{ .index = self.next_index, .low = self.next_index, .facts = .{} });
        errdefer _ = self.nodes.remove(root);
        self.next_index += 1;
        try self.members.append(self.allocator, root);
        errdefer _ = self.members.pop();
        try self.frames.append(self.allocator, frame);
        return null;
    }

    fn collect(self: *Analysis, types_store: *const types.Store, resolved: anytype, frame: *Frame) Allocator.Error!void {
        if (resolved.desc.flags.empty_tag_union_is_default) {
            frame.facts.contains_identity_variables = true;
            return;
        }

        switch (resolved.desc.content) {
            .err, .field_presence => {},
            .flex, .rigid => frame.facts.contains_identity_variables = true,
            .alias => |alias| {
                try self.children.append(self.allocator, types_store.getAliasBackingVar(alias));
                try self.children.appendSlice(self.allocator, types_store.sliceAliasArgs(alias));
            },
            .structure => |structure| switch (structure) {
                .empty_record, .empty_tag_union => {},
                .tuple => |tuple| try self.children.appendSlice(self.allocator, types_store.sliceVars(tuple.elems)),
                .nominal_type => |nominal| try self.children.appendSlice(self.allocator, types_store.sliceNominalArgs(nominal)),
                .fn_pure, .fn_effectful, .fn_unbound => |function| {
                    try self.children.appendSlice(self.allocator, types_store.sliceVars(function.args));
                    try self.children.append(self.allocator, function.ret);
                },
                .record => |record| {
                    for (types_store.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                        switch (presence.decode()) {
                            .required => |type_var| try self.children.append(self.allocator, type_var),
                            .unknown => |unknown| {
                                try self.children.append(self.allocator, unknown.presence);
                                try self.children.append(self.allocator, unknown.var_);
                            },
                        }
                    }
                    try self.children.append(self.allocator, record.ext);
                },
                .record_unbound => |fields| {
                    for (types_store.getRecordFieldsSlice(fields).items(.presence)) |presence| {
                        switch (presence.decode()) {
                            .required => |type_var| try self.children.append(self.allocator, type_var),
                            .unknown => |unknown| {
                                try self.children.append(self.allocator, unknown.presence);
                                try self.children.append(self.allocator, unknown.var_);
                            },
                        }
                    }
                },
                .tag_union => |tag_union| {
                    const tags = types_store.getTagsSlice(tag_union.tags);
                    for (tags.items(.args)) |args| {
                        try self.children.appendSlice(self.allocator, types_store.sliceVars(args));
                    }
                    try self.children.append(self.allocator, tag_union.ext);
                },
            },
        }
    }
};

test "source graph facts finish whole components before publishing identities" {
    const allocator = std.testing.allocator;
    var store = try types.Store.initCapacity(allocator, 16, 16);
    defer store.deinit();
    const a = try store.fresh();
    const b = try store.fresh();
    const identity = try store.fresh();
    try store.setVarContent(a, .{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ b, identity }) } } });
    try store.setVarContent(b, .{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{a}) } } });
    var analysis = Analysis.init(allocator);
    defer analysis.deinit();
    const expected = Facts{ .contains_cycle = true, .contains_identity_variables = true };
    try std.testing.expectEqualDeep(expected, try analysis.analyze(&store, a));
    try std.testing.expectEqualDeep(expected, try analysis.analyze(&store, b));
    analysis.resetRetainingCapacity();
    try std.testing.expectEqualDeep(expected, try analysis.analyze(&store, b));
    try std.testing.expectEqualDeep(expected, try analysis.analyze(&store, a));
}

test "source graph composability handles deep shared spines without native recursion" {
    const allocator = std.testing.allocator;
    var store = try types.Store.initCapacity(allocator, 40008, 8);
    defer store.deinit();
    var root = try store.freshFromContent(.{ .structure = .empty_record });
    for (0..40000) |_| {
        root = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ root, root }) } } });
    }
    var analysis = Analysis.init(allocator);
    defer analysis.deinit();
    try std.testing.expect((try analysis.analyze(&store, root)).isComposable());
    try std.testing.expectEqual(@as(u32, 40001), analysis.nodes.count());
}

test "source graph analysis recovers after each allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAnalysisFailure, .{});
}

fn testAnalysisFailure(allocator: Allocator) !void {
    var store = try types.Store.initCapacity(std.testing.allocator, 16, 16);
    defer store.deinit();
    const leaf = try store.freshFromContent(.{ .structure = .empty_record });
    const root = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ leaf, leaf }) } } });
    var analysis = Analysis.init(allocator);
    defer analysis.deinit();
    const result = analysis.analyze(&store, root) catch |err| {
        try std.testing.expect((try analysis.analyze(&store, root)).isComposable());
        return err;
    };
    try std.testing.expect(result.isComposable());
}
