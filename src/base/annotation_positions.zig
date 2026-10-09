//! Shared declaration-position equations for canonical checking and parse-only formatting.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// Where one source formal occurs relative to its declaration's root. A
/// function's argument positions flip the surrounding polarity and its
/// return keeps it, so every occurrence stands either at the polarity of the
/// reference to the declaration (`same`) or at its flip (`opposite`).
pub const Positions = packed struct(u8) {
    /// The declaration body names this formal, including inside the retained
    /// argument storage of another reference.
    used: bool = false,
    /// An occurrence stands at the reference's own polarity.
    same: bool = false,
    /// An occurrence stands at the reference's flipped polarity.
    opposite: bool = false,
    padding: u5 = 0,

    fn join(self: Positions, other: Positions) Positions {
        return @bitCast(@as(u8, @bitCast(self)) | @as(u8, @bitCast(other)));
    }

    fn hasSide(self: Positions) bool {
        return self.same or self.opposite;
    }

    fn swap(self: Positions) Positions {
        return .{ .used = self.used, .same = self.opposite, .opposite = self.same };
    }

    /// Where an argument substituted for this formal stands, given where the
    /// reference itself stands.
    fn compose(self: Positions, reference: Positions) Positions {
        var result: Positions = .{};
        if (self.same) result = result.join(reference);
        if (self.opposite) result = result.join(reference.swap());
        return result;
    }

    /// Whether the declaration body never names this formal.
    pub fn isUnused(self: Positions) bool {
        return !self.used;
    }

    /// Whether occurrences stand on both sides. One row cannot be open on the
    /// output side and closed on the input side, and every row nested in an
    /// argument substituted for such a formal stands on both sides too, so
    /// the whole argument is generated as written, at every depth: a closing
    /// polarity alone would reopen one function argument position further in.
    pub fn isInvariant(self: Positions) bool {
        return self.same and self.opposite;
    }

    /// The polarity an argument substituted for this formal is generated at,
    /// given the polarity of the reference. An invariant formal's argument is
    /// generated closed (see `isInvariant` for its nested rows). A formal
    /// with no side at all (unused, or only carried through its own
    /// recursion) keeps the reference's polarity.
    pub fn polarity(self: Positions, reference: anytype) @TypeOf(reference) {
        if (self.isInvariant()) return .neg;
        if (self.opposite) return switch (reference) {
            .pos => .neg,
            .neg => .pos,
        };
        return reference;
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
            positions: []Positions,
            /// Declarations whose bodies reference this one, so they are
            /// re-solved whenever its positions grow.
            users: std.ArrayList(usize) = .empty,
            queued: bool = false,
        };
        const Pending = struct { annotation: Adapter.Annotation, positions: Positions };
        const Phase = enum { discover, solve };

        allocator: Allocator,
        adapter: Adapter,
        declarations: std.ArrayList(Declaration) = .empty,
        by_declaration: std.AutoHashMapUnmanaged(Adapter.Key, usize) = .empty,
        pending: std.ArrayList(Pending) = .empty,
        worklist: std.ArrayList(usize) = .empty,
        invalid: bool = false,

        pub fn deinit(self: *Self) void {
            for (self.declarations.items) |*decl| {
                self.allocator.free(decl.positions);
                decl.users.deinit(self.allocator);
            }
            self.declarations.deinit(self.allocator);
            self.by_declaration.deinit(self.allocator);
            self.pending.deinit(self.allocator);
            self.worklist.deinit(self.allocator);
        }

        fn register(self: *Self, key: Adapter.Key) Allocator.Error!?usize {
            if (self.by_declaration.get(key)) |index| return index;
            const declaration = (try self.adapter.declaration(key)) orelse return null;
            const positions = try self.allocator.alloc(Positions, declaration.formal_count);
            errdefer self.allocator.free(positions);
            @memset(positions, .{});
            const index = self.declarations.items.len;
            try self.by_declaration.put(self.allocator, key, index);
            try self.declarations.append(self.allocator, .{ .key = key, .body = declaration.body, .positions = positions });
            return index;
        }

        fn push(self: *Self, annotation: Adapter.Annotation, positions: Positions) Allocator.Error!void {
            try self.pending.append(self.allocator, .{ .annotation = annotation, .positions = positions });
        }

        fn pushSlice(self: *Self, annotations: []const Adapter.Annotation, positions: Positions) Allocator.Error!void {
            for (annotations) |annotation| try self.push(annotation, positions);
        }

        /// Walk one declaration body. Discovery records usedness and the
        /// declarations each body references; solving joins every
        /// occurrence's position into the formal's equation and reports
        /// whether the declaration's positions grew.
        fn evaluate(self: *Self, index: usize, phase: Phase) Allocator.Error!bool {
            const key = self.declarations.items[index].key;
            var changed = false;
            self.pending.clearRetainingCapacity();
            try self.push(self.declarations.items[index].body, .{ .same = true });
            while (self.pending.pop()) |item| {
                // Below an argument whose formal has no side yet, nothing
                // occurs; discovery still visits it for usedness.
                if (phase == .solve and !item.positions.hasSide()) continue;
                switch (try self.adapter.node(key, item.annotation)) {
                    .formal => |formal| {
                        const positions = self.declarations.items[index].positions;
                        const old = positions[formal];
                        const joined = switch (phase) {
                            .discover => old.join(.{ .used = true }),
                            .solve => old.join(item.positions),
                        };
                        changed = changed or @as(u8, @bitCast(old)) != @as(u8, @bitCast(joined));
                        positions[formal] = joined;
                    },
                    .children => |children| try self.pushSlice(children, item.positions),
                    .function => |func| {
                        try self.pushSlice(func.args, item.positions.swap());
                        try self.push(func.ret, item.positions);
                    },
                    .apply => |apply| switch (apply.reference) {
                        .builtin => try self.pushSlice(apply.args, item.positions),
                        .invalid => self.invalid = true,
                        .declaration => |target_key| {
                            const target_index = (try self.register(target_key)) orelse {
                                self.invalid = true;
                                continue;
                            };
                            if (phase == .discover) {
                                const users = &self.declarations.items[target_index].users;
                                // Discovery walks one body at a time, so a
                                // repeated reference repeats the last user.
                                if (users.items.len == 0 or users.items[users.items.len - 1] != index) try users.append(self.allocator, index);
                            }
                            const target_positions = self.declarations.items[target_index].positions;
                            if (target_positions.len != apply.args.len) {
                                self.invalid = true;
                                continue;
                            }
                            for (apply.args, target_positions) |arg, target| {
                                // An unused formal's actual is retained in the
                                // reference's argument storage at the
                                // reference's own position. Usedness is
                                // complete before solving begins.
                                const argument_positions = if (phase == .discover or target.isUnused()) item.positions else target.compose(item.positions);
                                try self.push(arg, argument_positions);
                            }
                        },
                    },
                    .leaf => {},
                    .invalid => self.invalid = true,
                }
            }
            return changed;
        }

        fn enqueue(self: *Self, index: usize) Allocator.Error!void {
            const decl = &self.declarations.items[index];
            if (decl.queued) return;
            decl.queued = true;
            try self.worklist.append(self.allocator, index);
        }

        fn solve(self: *Self) Allocator.Error!void {
            // Discover every declaration and every source-formal occurrence
            // before any position equation is solved, so an equation that is
            // still empty never reads as an unused formal.
            var index: usize = 0;
            while (index < self.declarations.items.len and !self.invalid) : (index += 1) {
                _ = try self.evaluate(index, .discover);
            }
            if (self.invalid) return;
            // Least fixed point: positions only grow, each formal has two
            // sides, and a declaration is re-solved only when one it
            // references grew.
            for (0..self.declarations.items.len) |decl_index| try self.enqueue(decl_index);
            while (self.worklist.pop()) |decl_index| {
                self.declarations.items[decl_index].queued = false;
                if (!try self.evaluate(decl_index, .solve)) continue;
                for (self.declarations.items[decl_index].users.items) |user| try self.enqueue(user);
            }
            std.debug.assert(!self.invalid);
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

/// Only the entire signature's function return is adapter-reachable.
pub fn functionReturnReach(reach: anytype) @TypeOf(reach) {
    return if (reach == .signature) .result else .nested;
}

/// Existing adapters descend only into the error argument of a direct Try result.
pub fn nominalArgumentReach(reach: anytype, builtin_try: bool, index: usize) @TypeOf(reach) {
    return if (builtin_try and reach == .result and index == 1) .try_row else .nested;
}
