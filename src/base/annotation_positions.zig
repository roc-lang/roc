//! Shared declaration-position equations for canonical checking and parse-only formatting.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// Exact occurrence classes of one source formal.
pub const Positions = packed struct(u8) {
    inherited: bool = false,
    input: bool = false,
    output: bool = false,
    padding: u5 = 0,

    fn join(self: Positions, other: Positions) Positions {
        return @bitCast(@as(u8, @bitCast(self)) | @as(u8, @bitCast(other)));
    }

    pub fn isEmpty(self: Positions) bool {
        return @as(u8, @bitCast(self)) == 0;
    }

    fn compose(self: Positions, reference: Positions) Positions {
        var result: Positions = if (self.inherited) reference else .{};
        result.input = result.input or self.input;
        result.output = result.output or self.output;
        return result;
    }

    pub fn polarity(self: Positions, reference: anytype) @TypeOf(reference) {
        if (self.input) return .neg;
        if (self.inherited or self.isEmpty()) return reference;
        std.debug.assert(self.output);
        return .pos;
    }
};

/// Finite formal-flow solver; adapters supply only declaration and syntax facts.
pub fn Solver(comptime Adapter: type) type {
    return struct {
        const Self = @This();
        /// Resolved application target, or explicit frontend uncertainty.
        pub const Reference = union(enum) { builtin, declaration: Adapter.Key, invalid };
        /// The syntax distinctions that affect formal positions.
        pub const Node = union(enum) {
            leaf,
            invalid,
            formal: usize,
            children: []const Adapter.Annotation,
            function: struct { args: []const Adapter.Annotation, ret: Adapter.Annotation },
            apply: struct { reference: Reference, args: []const Adapter.Annotation },
        };
        const Declaration = struct {
            key: Adapter.Key,
            body: Adapter.Annotation,
            formal_count: usize,
            positions: []Positions,
            used: []bool,
            nominal: bool,
            formal_base: u32,
        };
        const Pending = struct { annotation: Adapter.Annotation, positions: Positions, dependency: ?usize };
        const Dependency = struct { formal: u32, parent: ?usize };
        const Edge = struct { from: u32, to: u32 };

        allocator: Allocator,
        adapter: Adapter,
        declarations: std.ArrayList(Declaration) = .empty,
        by_declaration: std.AutoHashMapUnmanaged(Adapter.Key, usize) = .empty,
        pending: std.ArrayList(Pending) = .empty,
        next_inherited: std.ArrayList(bool) = .empty,
        next_positions: std.ArrayList(Positions) = .empty,
        dependencies: std.ArrayList(Dependency) = .empty,
        edges: std.ArrayList(Edge) = .empty,
        active_dependency: ?usize = null,
        formal_count: u32 = 0,
        components: []const u32 = &.{},
        active_component: u32 = 0,
        invalid: bool = false,

        pub fn deinit(self: *Self) void {
            for (self.declarations.items) |decl| {
                self.allocator.free(decl.positions);
                self.allocator.free(decl.used);
            }
            self.declarations.deinit(self.allocator);
            self.by_declaration.deinit(self.allocator);
            self.pending.deinit(self.allocator);
            self.next_inherited.deinit(self.allocator);
            self.next_positions.deinit(self.allocator);
            self.dependencies.deinit(self.allocator);
            self.edges.deinit(self.allocator);
        }

        fn register(self: *Self, key: Adapter.Key) Allocator.Error!?usize {
            if (self.by_declaration.get(key)) |index| return index;
            const declaration = (try self.adapter.declaration(key)) orelse return null;
            const count = declaration.formal_count;
            const positions = try self.allocator.alloc(Positions, count);
            errdefer self.allocator.free(positions);
            @memset(positions, .{});
            const used = try self.allocator.alloc(bool, count);
            errdefer self.allocator.free(used);
            @memset(used, false);
            const index = self.declarations.items.len;
            try self.by_declaration.put(self.allocator, key, index);
            try self.declarations.append(self.allocator, .{
                .key = key,
                .body = declaration.body,
                .formal_count = count,
                .positions = positions,
                .used = used,
                .nominal = declaration.nominal,
                .formal_base = self.formal_count,
            });
            self.formal_count += @intCast(count);
            return index;
        }

        fn push(self: *Self, annotation: Adapter.Annotation, positions: Positions) Allocator.Error!void {
            // Even a temporarily empty equation retains its argument syntax; a
            // nested function establishes positions independently of that equation.
            try self.pending.append(self.allocator, .{ .annotation = annotation, .positions = positions, .dependency = self.active_dependency });
        }

        fn pushSlice(self: *Self, annotations: []const Adapter.Annotation, positions: Positions) Allocator.Error!void {
            for (annotations) |annotation| try self.push(annotation, positions);
        }

        const Phase = enum { discover, inheritance, dependencies, witnesses, settle };

        fn evaluate(self: *Self, index: usize, phase: Phase) Allocator.Error!bool {
            const decl = self.declarations.items[index];
            if (phase == .witnesses or phase == .settle) {
                var participates = false;
                for (0..decl.formal_count) |formal| {
                    participates = participates or self.components[decl.formal_base + formal] == self.active_component;
                }
                if (!participates) return false;
            }
            var changed = false;
            if (phase == .inheritance) {
                try self.next_inherited.resize(self.allocator, decl.formal_count);
                @memset(self.next_inherited.items, false);
            }
            if (phase == .settle) {
                try self.next_positions.resize(self.allocator, decl.formal_count);
                @memset(self.next_positions.items, .{});
            }
            self.active_dependency = null;
            self.pending.clearRetainingCapacity();
            try self.push(decl.body, .{ .inherited = true });
            while (self.pending.pop()) |item| {
                self.active_dependency = item.dependency;
                const node = try self.adapter.node(decl.key, item.annotation);
                const formal = switch (node) {
                    .formal => |formal_index| formal_index,
                    .leaf, .invalid, .children, .function, .apply => null,
                };
                if (formal) |formal_index| {
                    if (phase == .discover) {
                        decl.used[formal_index] = true;
                        continue;
                    }
                    if (phase == .inheritance) {
                        self.next_inherited.items[formal_index] = self.next_inherited.items[formal_index] or item.positions.inherited;
                        continue;
                    }
                    const formal_id = decl.formal_base + @as(u32, @intCast(formal_index));
                    if (phase == .dependencies) {
                        var dependency = item.dependency;
                        while (dependency) |dep| {
                            const edge = self.dependencies.items[dep];
                            try self.edges.append(self.allocator, .{ .from = formal_id, .to = edge.formal });
                            dependency = edge.parent;
                        }
                        continue;
                    }
                    if (self.components[formal_id] != self.active_component) {
                        continue;
                    }
                    if (phase == .settle) {
                        self.next_positions.items[formal_index] = self.next_positions.items[formal_index].join(item.positions);
                        continue;
                    }
                    const old = decl.positions[formal_index];
                    const joined = old.join(item.positions);
                    changed = changed or @as(u8, @bitCast(old)) != @as(u8, @bitCast(joined));
                    decl.positions[formal_index] = joined;
                    continue;
                }
                switch (node) {
                    .children => |children| try self.pushSlice(children, item.positions),
                    .function => |func| {
                        self.active_dependency = null;
                        try self.pushSlice(func.args, .{ .input = true });
                        try self.push(func.ret, .{ .output = true });
                    },
                    .apply => |apply| {
                        const args = apply.args;
                        switch (apply.reference) {
                            .builtin => try self.pushSlice(args, item.positions),
                            .invalid => self.invalid = true,
                            .declaration => |key| {
                                const target_index = (try self.register(key)) orelse {
                                    self.invalid = true;
                                    continue;
                                };
                                const target = self.declarations.items[target_index];
                                if (target.positions.len != args.len) {
                                    self.invalid = true;
                                    continue;
                                }
                                for (args, target.positions, target.used, 0..) |arg, positions, used, target_formal| {
                                    // Source argument storage retains a phantom
                                    // actual, including functions inside it. Usedness
                                    // is complete before solving positions: an empty
                                    // equation mid-iteration does not prove unusedness.
                                    const target_id = target.formal_base + @as(u32, @intCast(target_formal));
                                    self.active_dependency = item.dependency;
                                    if (phase == .dependencies and used) {
                                        const dependency = self.dependencies.items.len;
                                        try self.dependencies.append(self.allocator, .{ .formal = target_id, .parent = item.dependency });
                                        self.active_dependency = dependency;
                                    }
                                    var transfer = positions;
                                    if (phase == .witnesses and target.nominal and self.components[target_id] == self.active_component) transfer.inherited = used;
                                    const argument_positions = if (phase == .discover or !used) item.positions else transfer.compose(item.positions);
                                    try self.push(arg, argument_positions);
                                }
                            },
                        }
                    },
                    .formal, .leaf => {},
                    .invalid => self.invalid = true,
                }
            }
            if (phase == .inheritance) {
                for (decl.positions, self.next_inherited.items) |*positions, inherited| {
                    std.debug.assert(positions.inherited or !inherited);
                    changed = changed or positions.inherited != inherited;
                    positions.inherited = inherited;
                }
            }
            if (phase == .settle) {
                for (decl.positions, self.next_positions.items, 0..) |*positions, next, formal_index| {
                    if (self.components[decl.formal_base + formal_index] != self.active_component) continue;
                    const old_bits: u8 = @bitCast(positions.*);
                    const next_bits: u8 = @bitCast(next);
                    std.debug.assert((old_bits | next_bits) == old_bits);
                    changed = changed or old_bits != next_bits;
                    positions.* = next;
                }
            }
            return changed;
        }

        fn solve(self: *Self) Allocator.Error!void {
            // Discover every declaration and every retained source-formal
            // occurrence without depending on the position equations' initial bottom.
            var index: usize = 0;
            while (index < self.declarations.items.len and !self.invalid) : (index += 1) {
                _ = try self.evaluate(index, .discover);
            }
            for (self.declarations.items) |decl| {
                for (decl.positions, decl.used) |*positions, used| positions.inherited = used;
            }
            // Pure recursive argument storage preserves inheritance. Function
            // boundaries remove it, so compute this bit by descending from true.
            var inheritance_changed = true;
            while (inheritance_changed and !self.invalid) {
                inheritance_changed = false;
                for (0..self.declarations.items.len) |decl_index| inheritance_changed = (try self.evaluate(decl_index, .inheritance)) or inheritance_changed;
            }
            if (self.invalid) return;
            const inherited = try self.allocator.alloc(bool, self.formal_count);
            defer self.allocator.free(inherited);
            for (self.declarations.items) |decl| {
                for (decl.positions, 0..) |positions, formal| inherited[decl.formal_base + formal] = positions.inherited;
            }
            for (0..self.declarations.items.len) |decl_index| _ = try self.evaluate(decl_index, .dependencies);
            const adjacency = try self.allocator.alloc(std.ArrayListUnmanaged(u32), self.formal_count);
            defer {
                for (adjacency) |*edges| edges.deinit(self.allocator);
                self.allocator.free(adjacency);
            }
            @memset(adjacency, .empty);
            for (self.edges.items) |edge| try adjacency[edge.from].append(self.allocator, edge.to);
            const components = try self.allocator.alloc(u32, self.formal_count);
            defer self.allocator.free(components);
            try stronglyConnectedComponents(self.allocator, adjacency, components);
            self.components = components;
            var component_count: u32 = 0;
            for (components) |component| component_count = @max(component_count, component + 1);
            for (0..component_count) |component| {
                self.active_component = @intCast(component);
                for (self.declarations.items) |decl| {
                    for (decl.positions, 0..) |*positions, formal| {
                        if (components[decl.formal_base + formal] == component) positions.* = .{};
                    }
                }
                // Witness only actual contexts at recurring nominal boundaries.
                // Transparent aliases still compose their own function resets.
                var changed = true;
                while (changed) {
                    changed = false;
                    for (0..self.declarations.items.len) |decl_index| changed = (try self.evaluate(decl_index, .witnesses)) or changed;
                }
                for (self.declarations.items) |decl| {
                    for (decl.positions, 0..) |*positions, formal| {
                        if (components[decl.formal_base + formal] == component) positions.inherited = inherited[decl.formal_base + formal];
                    }
                }
                // The witness closure is an upper bound. Ordinary composition now
                // removes contexts overwritten before the next recurring boundary.
                changed = true;
                while (changed) {
                    changed = false;
                    for (0..self.declarations.items.len) |decl_index| changed = (try self.evaluate(decl_index, .settle)) or changed;
                }
            }
        }

        /// Analyze one resolved declaration. The caller owns the returned slice.
        pub fn analyze(self: *Self, key: Adapter.Key) Allocator.Error!?[]Positions {
            const root = (try self.register(key)) orelse return null;
            try self.solve();
            if (self.invalid) return null;
            return try self.allocator.dupe(Positions, self.declarations.items[root].positions);
        }

        /// Create an analysis using a frontend's explicit declaration adapter.
        pub fn init(allocator: Allocator, adapter: Adapter) Self {
            return .{ .allocator = allocator, .adapter = adapter };
        }
    };
}

// Components are emitted after all components they depend on.
fn stronglyConnectedComponents(
    allocator: Allocator,
    adjacency: []const std.ArrayListUnmanaged(u32),
    component_of: []u32,
) std.mem.Allocator.Error!void {
    const node_count = adjacency.len;
    const unvisited = std.math.maxInt(u32);
    const index_of = try allocator.alloc(u32, node_count);
    defer allocator.free(index_of);
    @memset(index_of, unvisited);
    const low_of = try allocator.alloc(u32, node_count);
    defer allocator.free(low_of);
    const on_stack = try allocator.alloc(bool, node_count);
    defer allocator.free(on_stack);
    @memset(on_stack, false);

    var component_stack: std.ArrayListUnmanaged(u32) = .empty;
    defer component_stack.deinit(allocator);
    const WalkFrame = struct { node: u32, edge: u32 };
    var walk_frames: std.ArrayListUnmanaged(WalkFrame) = .empty;
    defer walk_frames.deinit(allocator);

    var next_index: u32 = 0;
    var next_component: u32 = 0;
    for (0..node_count) |start| {
        if (index_of[start] != unvisited) continue;
        index_of[start] = next_index;
        low_of[start] = next_index;
        next_index += 1;
        try component_stack.append(allocator, @intCast(start));
        on_stack[start] = true;
        try walk_frames.append(allocator, .{ .node = @intCast(start), .edge = 0 });
        while (walk_frames.items.len != 0) {
            const frame = &walk_frames.items[walk_frames.items.len - 1];
            const node = frame.node;
            if (frame.edge < adjacency[node].items.len) {
                const next = adjacency[node].items[frame.edge];
                frame.edge += 1;
                if (index_of[next] == unvisited) {
                    index_of[next] = next_index;
                    low_of[next] = next_index;
                    next_index += 1;
                    try component_stack.append(allocator, next);
                    on_stack[next] = true;
                    try walk_frames.append(allocator, .{ .node = next, .edge = 0 });
                } else if (on_stack[next]) {
                    low_of[node] = @min(low_of[node], index_of[next]);
                }
                continue;
            }
            walk_frames.items.len -= 1;
            if (walk_frames.items.len != 0) {
                const parent = walk_frames.items[walk_frames.items.len - 1].node;
                low_of[parent] = @min(low_of[parent], low_of[node]);
            }
            if (low_of[node] == index_of[node]) {
                while (true) {
                    const member = component_stack.pop().?;
                    on_stack[member] = false;
                    component_of[member] = next_component;
                    if (member == node) break;
                }
                next_component += 1;
            }
        }
    }
}

/// Only the entire signature's function return is adapter-reachable.
pub fn functionReturnReach(reach: anytype) @TypeOf(reach) {
    return if (reach == .signature) .result else .nested;
}

/// Existing adapters descend only into the error argument of a direct Try result.
pub fn nominalArgumentReach(reach: anytype, builtin_try: bool, index: usize) @TypeOf(reach) {
    return if (builtin_try and reach == .result and index == 1) .try_row else .nested;
}
