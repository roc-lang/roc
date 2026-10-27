//! Least fixed point of the producer's emptiness AND/OR equations.
const std = @import("std");
const AnyAll = @import("collections").AnyAll;
const FixedPoint = @import("fixed_point.zig");
const Allocator = std.mem.Allocator;

/// Evaluate producer-authored emptiness equations and commit converged leaves.
pub fn Evaluation(comptime Leaf: type, comptime Context: type) type {
    return struct {
        const Tree = AnyAll.Evaluation(Leaf, Context);
        /// The same explicit AND/OR item format used by tree evaluation.
        pub const Items = Tree.Items;
        /// A constant or the Boolean operator combining the emitted children.
        pub const Expansion = Tree.Expansion;
        /// Reusable graph and worklist storage for one non-reentrant evaluation.
        pub const Scratch = struct {
            leaves: std.ArrayList(Leaf) = .empty,
            indices: std.AutoHashMapUnmanaged(Leaf, usize) = .empty,
            equations: FixedPoint = .{},
            items: std.ArrayList(Tree.Item) = .empty,
            /// Release every scratch allocation.
            pub fn deinit(self: *Scratch, gpa: Allocator) void {
                self.leaves.deinit(gpa);
                self.indices.deinit(gpa);
                self.equations.deinit(gpa);
                self.items.deinit(gpa);
            }
            fn intern(self: *Scratch, gpa: Allocator, leaf: Leaf) Allocator.Error!usize {
                const entry = try self.indices.getOrPut(gpa, leaf);
                if (entry.found_existing) return entry.value_ptr.*;
                const index = try self.equations.add(gpa);
                entry.value_ptr.* = index;
                try self.leaves.append(gpa, leaf);
                return index;
            }
        };
        /// Expand each leaf once, converge, then deliver settled outcomes to exit.
        pub fn runWith(gpa: Allocator, scratch: *Scratch, context: *Context, root: Leaf) Allocator.Error!bool {
            defer {
                scratch.leaves.clearRetainingCapacity();
                scratch.indices.clearRetainingCapacity();
                scratch.equations.clear();
                scratch.items.clearRetainingCapacity();
            }
            const root_index = try scratch.intern(gpa, context.representative(root));
            var cursor: usize = 0;
            while (cursor < scratch.leaves.items.len) : (cursor += 1) {
                const leaf = scratch.leaves.items[cursor];
                const index = scratch.indices.get(leaf).?;
                scratch.items.clearRetainingCapacity();
                const expansion = try context.enter(.{ .allocator = gpa, .list = &scratch.items }, leaf);
                switch (expansion) {
                    .value => |value| scratch.equations.set(index, @intFromBool(value)),
                    .group => |op| {
                        if (op == .all) scratch.equations.all(index);
                        var item_index: usize = 0;
                        while (item_index < scratch.items.items.len) : (item_index += 1) {
                            switch (scratch.items.items[item_index]) {
                                .leaf => |child| {
                                    const child_index = try scratch.intern(gpa, context.representative(child));
                                    try scratch.equations.edge(gpa, index, child_index, .identity);
                                },
                                .group => |group| {
                                    const nested = try scratch.equations.add(gpa);
                                    if (group.op == .all) scratch.equations.all(nested);
                                    try scratch.equations.edge(gpa, index, nested, .identity);
                                    for (scratch.items.items[item_index + 1 ..][0..group.count]) |item| {
                                        const child_index = try scratch.intern(gpa, context.representative(item.leaf));
                                        try scratch.equations.edge(gpa, nested, child_index, .identity);
                                    }
                                    item_index += group.count;
                                },
                            }
                        }
                    },
                }
            }
            try scratch.equations.solve(gpa);
            for (scratch.leaves.items) |leaf| {
                try context.exit(leaf, scratch.equations.value(scratch.indices.get(leaf).?) != 0);
            }
            return scratch.equations.value(root_index) != 0;
        }
    };
}
