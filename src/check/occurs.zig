//! Checks whether a type variable is recursively defined.
//!
//! This module implements an "occurs check" to detect:
//! - structurally infinite types (e.g., `a = List(a)`)
//! - recursion through unnamed constructs (anonymous recursion)
//! - recursion through named nominal types (permitted in Roc under some rules)
//!
//! The main entrypoint is `occurs()`. It analyzes the type graph rooted at a variable
//! and reports whether the structure is recursive, and if so, what kind of recursion.
//!
//! The traversal uses a shared `Scratch` value to track visited nodes and recursion
//! chains. This is reset between runs. The check does not mutate the `Store`.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const types = @import("types");

const Ident = base.Ident;
const MkSafeList = collections.SafeList;
const Store = types.Store;
const DescStoreIdx = types.DescStoreIdx;
const Content = types.Content;
const Var = types.Var;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

/// The result of checking for recursion
///
/// When the result is recursive/infinite, `scratch.err_var` holds the var to
/// report in the resulting diagnostic.
pub const Result = enum {
    valid,
    recursive_anonymous,
    infinite,
};

/// Check if a variable is recursive
///
/// This uses `Scratch` as to hold intermediate values. `occurs` will reset it
/// before each run.
///
/// When the result is recursive/infinite, `scratch.err_var` holds the var to
/// report in the resulting diagnostic.
///
/// This function does not modify the `Store`.
pub fn occurs(types_store: *Store, scratch: *Scratch, var_: Var) std.mem.Allocator.Error!Result {
    scratch.reset();

    // Check for recursion. The root has no incoming edge, so it starts with the
    // empty edge. Whether the recursion is nominal/anonymous/infinite is decided
    // from the edges *within* the detected cycle, not from the root downward.
    var check_occurs = CheckOccurs.init(types_store, scratch);
    return try check_occurs.occurs(var_, Edge.none);
}

/// Performs an occurs check on a type variable.
///
/// This struct encapsulates the iterative traversal logic used to detect
/// whether a variable is recursively defined through it's children.
///
/// It uses a scratch space to track visited nodes and maintain the explicit
/// work stack that drives the traversal. It is intended for one-time use per
/// `occurs()` call.
///
/// Ownership: `CheckOccurs` does not allocate or deallocate memory. It borrows
/// both the `types_store` and `scratch` passed during initialization. These
/// outlive the `CheckOccurs` value.
const CheckOccurs = struct {
    const Self = @This();

    types_store: *Store,
    scratch: *Scratch,

    /// Init CheckOccurs
    ///
    /// Note that this struct does not own any of it's fields
    fn init(types_store: *Store, scratch: *Scratch) Self {
        return .{ .types_store = types_store, .scratch = scratch };
    }

    /// Iteratively check if a type is referenced by it's children
    ///
    /// On detecting a cycle, sets `scratch.err_var` to the var to report.
    fn occurs(self: *Self, var_: Var, edge: Edge) std.mem.Allocator.Error!Result {
        // Push the first frame
        // Since we know that Scratch.init initializes capacity, we can append
        // assuming capacity here
        std.debug.assert(self.scratch.stack.items.capacity > 0);
        _ = self.scratch.stack.appendAssumeCapacity(Frame{ .process_var = .{ .var_ = var_, .edge = edge } });

        // Process frames
        return try self.occursIter();
    }

    /// Iteratively process frames
    fn occursIter(self: *Self) std.mem.Allocator.Error!Result {
        while (self.scratch.stack.items.pop()) |frame| {
            switch (frame) {
                .process_var => |start| {
                    const root = self.types_store.resolveVar(start.var_);
                    const root_var = root.var_;

                    if (self.scratch.hasVisited(root.desc_idx)) {
                        // If we've already visited this var and not returned, then it's not recursive
                        continue;
                    } else if (self.scratch.hasSeenVar(root_var)) |match_idx| {
                        // Recursion point!

                        // We've already seen this var during traversal. `edge`
                        // is the edge that closes the cycle (from the current
                        // parent back to `root_var`); `match_idx` is where
                        // `root_var` sits on the seen stack.

                        // Classify using only the edges inside that cycle.
                        const classified = self.classifyCycle(match_idx, start.edge);
                        switch (classified) {
                            .valid => {
                                // It the recursion was valid (ie passed thru
                                // nominal) then continue processing
                                continue;
                            },
                            else => {
                                // If the recursion observed is invalid (infinite or
                                // anonymous) then return the error immediately.

                                // Report the deepest var on the seen stack (the
                                // parent of the cycle-closing edge) as the error
                                // var.
                                const seen_len: usize = @intCast(self.scratch.seen.len());
                                std.debug.assert(seen_len > 0);
                                self.scratch.err_var = self.scratch.seen.items.items[seen_len - 1].var_;

                                return classified;
                            },
                        }
                    } else {
                        // Push this var to the seen stack
                        try self.scratch.pushSeen(root_var, start.edge);

                        // Schedule the finish frame to run after processing children
                        _ = try self.scratch.stack.append(self.scratch.gpa, Frame{ .finish_process_var = .{
                            .desc_idx = root.desc_idx,
                        } });

                        // Process this frame & schedule children
                        switch (root.desc.content) {
                            .structure => |flat_type| {
                                switch (flat_type) {
                                    .tuple => |tuple| {
                                        const elems = self.types_store.sliceVars(tuple.elems);
                                        try self.pushVarsToProcess(elems, Edge.none);
                                    },
                                    .nominal_type => |nominal_type| {
                                        const backing_var = self.types_store.getNominalBackingVar(nominal_type);
                                        try self.pushVarToProcess(backing_var, Edge.nominal);

                                        // Arguments are ordinary positions; only the backing
                                        // var is "through" the nominal.
                                        var arg_iter = self.types_store.iterNominalArgs(nominal_type);
                                        while (arg_iter.next()) |arg_var| {
                                            try self.pushVarToProcess(arg_var, Edge.none);
                                        }
                                    },
                                    .fn_pure => |func| {
                                        try self.pushVarToProcess(func.ret, Edge.none);
                                        const args = self.types_store.sliceVars(func.args);
                                        try self.pushVarsToProcess(args, Edge.none);
                                    },
                                    .fn_effectful => |func| {
                                        try self.pushVarToProcess(func.ret, Edge.none);
                                        const args = self.types_store.sliceVars(func.args);
                                        try self.pushVarsToProcess(args, Edge.none);
                                    },
                                    .fn_unbound => |func| {
                                        try self.pushVarToProcess(func.ret, Edge.none);
                                        const args = self.types_store.sliceVars(func.args);
                                        try self.pushVarsToProcess(args, Edge.none);
                                    },
                                    .record => |record| {
                                        try self.pushVarToProcess(record.ext, Edge.none);
                                        const fields = self.types_store.getRecordFieldsSlice(record.fields);
                                        try self.pushVarsToProcess(fields.items(.var_), Edge.recursion);
                                    },
                                    .record_unbound => |fields| {
                                        const fields_slice = self.types_store.getRecordFieldsSlice(fields);
                                        try self.pushVarsToProcess(fields_slice.items(.var_), Edge.recursion);
                                    },
                                    .tag_union => |tag_union| {
                                        try self.pushVarToProcess(tag_union.ext, Edge.none);
                                        const tags = self.types_store.getTagsSlice(tag_union.tags);
                                        for (tags.items(.args)) |tag_args| {
                                            const args = self.types_store.sliceVars(tag_args);
                                            try self.pushVarsToProcess(args, Edge.recursion);
                                        }
                                    },
                                    .empty_record => {},
                                    .empty_tag_union => {},
                                }
                            },
                            .alias => |alias| {
                                const backing_var = self.types_store.getAliasBackingVar(alias);
                                try self.pushVarToProcess(backing_var, Edge.none);

                                var arg_iter = self.types_store.iterAliasArgs(alias);
                                while (arg_iter.next()) |arg_var| {
                                    try self.pushVarToProcess(arg_var, Edge.none);
                                }
                            },
                            .flex => {
                                // Flex variables are not checked for cycles - they are allowed to have
                                // self-referential constraints. Only structural content is checked.
                            },
                            .rigid => {},
                            .err => {},
                        }
                    }
                },
                .finish_process_var => |end| {
                    self.scratch.popSeen();
                    try self.scratch.appendVisited(end.desc_idx);
                },
            }
        }

        return .valid;
    }

    /// Classify a detected cycle using only the edges that lie *within* it.
    ///
    /// The cycle consists of the seen-stack frames `match_idx+1 ..= top` (each
    /// carrying the edge that pushed it) plus the `closing` edge from the current
    /// parent back to the cycle head at `match_idx`. Edges above the cycle (the
    /// head's own incoming edge and anything before it) are intentionally ignored
    /// so an enclosing nominal/container can't reclassify an unrelated cycle.
    fn classifyCycle(self: *Self, match_idx: usize, closing: Edge) Result {
        var recursion_allowed = closing.recursion_allowed;
        var nominal = closing.nominal_backing;

        const entries = self.scratch.seen.items.items;
        var k = match_idx + 1;
        while (k < entries.len) : (k += 1) {
            recursion_allowed = recursion_allowed or entries[k].edge.recursion_allowed;
            nominal = nominal or entries[k].edge.nominal_backing;
        }

        if (recursion_allowed and nominal) {
            // The cycle passes through a recursion-allowed position (record, tag
            // union) AND a nominal's backing: valid recursion through a nominal.
            return .valid;
        } else if (recursion_allowed) {
            // Through a recursion-allowed position but no nominal: anonymous
            // recursion, which Roc rejects.
            return .recursive_anonymous;
        } else {
            // No recursion-allowed position in the cycle: structurally infinite.
            return .infinite;
        }
    }

    /// Push a single sub var onto the work stack to be processed later.
    fn pushVarToProcess(self: *Self, sub_var: Var, edge: Edge) std.mem.Allocator.Error!void {
        _ = try self.scratch.stack.append(self.scratch.gpa, Frame{ .process_var = .{
            .var_ = sub_var,
            .edge = edge,
        } });
    }

    /// Push a slice of sub vars onto the work stack to be processed later.
    fn pushVarsToProcess(self: *Self, sub_vars: []Var, edge: Edge) std.mem.Allocator.Error!void {
        try self.scratch.stack.items.ensureUnusedCapacity(self.scratch.gpa, sub_vars.len);
        for (sub_vars) |sub_var| {
            _ = self.scratch.stack.appendAssumeCapacity(Frame{ .process_var = .{
                .var_ = sub_var,
                .edge = edge,
            } });
        }
    }
};

/// A single parent→child edge in the type graph traversal.
///
/// An `Edge` describes exactly one step and never carries state down from above:
/// at most one flag is set per edge. Each `seen`-stack frame stores the edge that
/// pushed it (see `SeenEntry`), and a detected cycle is classified from the edges
/// inside the cycle only. This is what keeps an enclosing nominal/container from
/// reclassifying a cycle it isn't actually part of.
const Edge = struct {
    /// The child sits in a record field or tag-union argument: a position that
    /// makes recursion through it well-formed (vs. structurally infinite).
    recursion_allowed: bool,
    /// The child is a nominal type's backing var (recursion "through" a nominal).
    nominal_backing: bool,

    /// No flag set: tuple elems, fn args/ret, nominal args, alias args/backing,
    /// record/tag-union ext.
    const none: Edge = .{ .recursion_allowed = false, .nominal_backing = false };
    /// Edge into a record field or tag-union argument.
    const recursion: Edge = .{ .recursion_allowed = true, .nominal_backing = false };
    /// Edge into a nominal type's backing var.
    const nominal: Edge = .{ .recursion_allowed = false, .nominal_backing = true };
};

/// A frame on the `seen` traversal stack: a resolved var plus the edge that
/// pushed it onto the stack.
const SeenEntry = struct {
    var_: Var,
    edge: Edge,
};

/// A single iterative frame
const Frame = union(enum) {
    process_var: struct {
        var_: Var,
        edge: Edge,
    },
    finish_process_var: struct {
        desc_idx: DescStoreIdx,
    },
};

/// Struct to hold intermediate values used during occurs check
pub const Scratch = struct {
    const Self = @This();

    gpa: std.mem.Allocator,

    stack: MkSafeList(Frame),
    seen: MkSafeList(SeenEntry),
    visited: MkSafeList(DescStoreIdx),

    /// The var to report when a cycle is detected: the deepest var on the seen
    /// stack (the parent of the cycle-closing edge). Null until a cycle is found.
    err_var: ?Var = null,

    pub fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        // Initial capacities are conservative estimates. Lists grow dynamically as needed.
        // Rust compiler uses 1024, but that's likely overkill for typical Roc code.
        // These values handle common cases:
        // - seen: 32 - typical type depth is much shallower
        // - visited: 64 - covers most type traversals without reallocation
        // Future optimization: profile real codebases to tune these values.
        return .{
            .gpa = gpa,
            .stack = try MkSafeList(Frame).initCapacity(gpa, 32),
            .seen = try MkSafeList(SeenEntry).initCapacity(gpa, 32),
            .visited = try MkSafeList(DescStoreIdx).initCapacity(gpa, 64),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit(self.gpa);
        self.seen.deinit(self.gpa);
        self.visited.deinit(self.gpa);
    }

    pub fn reset(self: *Self) void {
        self.stack.items.clearRetainingCapacity();
        self.seen.items.clearRetainingCapacity();
        self.visited.items.clearRetainingCapacity();
        self.err_var = null;
    }

    /// Returns the index of `var_` on the seen stack if it's currently being
    /// traversed (i.e. a cycle), else null. The index marks the cycle head.
    fn hasSeenVar(self: *const Self, var_: Var) ?usize {
        for (self.seen.items.items, 0..) |entry, i| {
            if (entry.var_ == var_) return i;
        }
        return null;
    }

    fn hasVisited(self: *const Self, desc_idx: DescStoreIdx) bool {
        for (self.visited.items.items) |visited_idx| {
            if (visited_idx == desc_idx) return true;
        }
        return false;
    }

    fn pushSeen(self: *Self, var_: Var, edge: Edge) std.mem.Allocator.Error!void {
        _ = try self.seen.append(self.gpa, .{ .var_ = var_, .edge = edge });
    }

    fn popSeen(self: *Self) void {
        _ = self.seen.items.pop();
    }

    fn appendVisited(self: *Self, desc_idx: DescStoreIdx) std.mem.Allocator.Error!void {
        _ = try self.visited.append(self.gpa, desc_idx);
    }
};

test "occurs: no recurcion (v = Str)" {
    const gpa = std.testing.allocator;

    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const str_var = try types_store.freshFromContent(Content{ .structure = .empty_record });

    const result = occurs(&types_store, &scratch, str_var);
    try std.testing.expectEqual(.valid, result);
}

test "occurs: no recursion through two levels (v1 = Box(v2), v2 = Str)" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const v1 = try types_store.fresh();
    const v2 = try types_store.fresh();

    // Create a nominal Box type wrapping v2
    const backing_var = try types_store.freshFromContent(Content{ .structure = .empty_record });
    try types_store.setVarContent(v1, try types_store.mkNominal(
        undefined,
        backing_var,
        &.{v2},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));
    try types_store.setRootVarContent(v2, Content{ .structure = .empty_record });

    const result = occurs(&types_store, &scratch, v1);
    try std.testing.expectEqual(.valid, result);
}

test "occurs: tuple recursion (v = Tuple(v, Str))" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const v = try types_store.fresh();
    const str_var = try types_store.freshFromContent(Content{ .structure = .empty_record });

    const elems_range = try types_store.appendVars(&[_]Var{ v, str_var });
    const tuple = types.Tuple{ .elems = elems_range };

    try types_store.setRootVarContent(v, Content{ .structure = .{ .tuple = tuple } });

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.infinite, result);

    try std.testing.expectEqual(v, scratch.err_var.?);
}

test "occurs: tuple not recursive (v = Tuple(Str, Str))" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const str_var = try types_store.freshFromContent(Content{ .structure = .empty_record });

    const elems_range = try types_store.appendVars(&[_]Var{ str_var, str_var });
    const tuple = types.Tuple{ .elems = elems_range };

    const v = try types_store.freshFromContent(Content{ .structure = .{ .tuple = tuple } });

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.valid, result);

    try std.testing.expectEqual(2, scratch.visited.len());
}

test "occurs: recursive alias (v = Alias(List v))" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const v = try types_store.fresh();
    const backing_var = try types_store.fresh(); // backing var at v+1
    const arg = try types_store.freshRedirect(v); // arg at v+2 redirecting to v (creating infinite recursion on purpose for the test)

    try types_store.setRootVarContent(v, try types_store.mkAlias(
        types.TypeIdent{ .ident_idx = undefined },
        backing_var,
        &.{arg},
        Ident.Idx.NONE,
    ));

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.infinite, result);

    try std.testing.expectEqual(v, scratch.err_var.?);
}

test "occurs: alias with no recursion (v = Alias Str)" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const alias_var = try types_store.fresh();
    const backing_var = try types_store.freshFromContent(Content{ .structure = .empty_record });
    const arg_var = try types_store.freshFromContent(Content{ .structure = .empty_record });

    try types_store.setRootVarContent(alias_var, try types_store.mkAlias(
        types.TypeIdent{ .ident_idx = undefined },
        backing_var,
        &.{arg_var},
        Ident.Idx.NONE,
    ));

    const result = occurs(&types_store, &scratch, alias_var);
    try std.testing.expectEqual(.valid, result);
}

test "occurs: recursive tag union (v = [ Cons(elem, v), Nil ]" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const linked_list = try types_store.fresh();
    const elem = try types_store.fresh();
    const ext = try types_store.fresh();

    const cons_tag_args = try types_store.appendVars(&[_]Var{ elem, linked_list });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };

    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };

    const tags = try types_store.appendTags(&[_]Tag{ cons_tag, nil_tag });

    const tag_union = TagUnion{ .tags = tags, .ext = ext };

    try types_store.setRootVarContent(linked_list, .{ .structure = .{ .tag_union = tag_union } });

    const result = occurs(&types_store, &scratch, linked_list);
    try std.testing.expectEqual(.recursive_anonymous, result);

    try std.testing.expectEqual(linked_list, scratch.err_var.?);
}
test "occurs: nested recursive tag union (v = [ Cons(elem, Box(v)) ] )" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const linked_list = try types_store.fresh();
    const elem = try types_store.fresh();

    // Wrap the recursive var in a nominal Box to simulate nesting
    const boxed_linked_list = try types_store.fresh();
    const box_backing_var = try types_store.freshFromContent(.{ .structure = .empty_record });
    try types_store.setVarContent(boxed_linked_list, try types_store.mkNominal(
        undefined,
        box_backing_var,
        &.{linked_list},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // Build tag args: (elem, Box(linked_list))
    const cons_tag_args = try types_store.appendVars(&[_]Var{ elem, boxed_linked_list });

    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };

    const tags = try types_store.appendTags(&[_]Tag{ cons_tag, nil_tag });

    const tag_union = TagUnion{ .tags = tags, .ext = try types_store.fresh() };

    try types_store.setRootVarContent(linked_list, .{ .structure = .{ .tag_union = tag_union } });

    const result = occurs(&types_store, &scratch, linked_list);
    try std.testing.expectEqual(.recursive_anonymous, result);

    try std.testing.expectEqual(boxed_linked_list, scratch.err_var.?);
}

test "occurs: recursive tag union (v = List: [ Cons(Elem, List), Nil ])" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    const nominal_type = try types_store.fresh();

    const elem = try types_store.fresh();
    const ext = try types_store.fresh();

    const cons_tag_args = try types_store.appendVars(&[_]Var{ elem, nominal_type });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };
    const backing_var = try types_store.freshFromContent(try types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));
    try types_store.setVarContent(nominal_type, try types_store.mkNominal(
        undefined,
        backing_var,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // assert that starting from the nominal type, it works

    const result1 = occurs(&types_store, &scratch, nominal_type);
    try std.testing.expectEqual(.valid, result1);

    // assert that starting from the the tag union, it works

    const result2 = occurs(&types_store, &scratch, backing_var);
    try std.testing.expectEqual(.valid, result2);
}

test "occurs: recursive tag union with multiple nominals (TypeA := TypeB, TypeB := [ Cons(Elem, TypeA), Nil ])" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    // Create vars in the required order for adjacency to work out
    const type_b_nominal = try types_store.fresh();
    const type_a_nominal = try types_store.fresh();
    const elem = try types_store.fresh();
    const ext = try types_store.fresh();

    // Create the tag union content that references type_a_nominal
    const cons_tag_args = try types_store.appendVars(&[_]Var{ elem, type_a_nominal });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };
    const type_b_backing = try types_store.freshFromContent(try types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));

    // Set up TypeB = [ Cons(Elem, TypeA), Nil ]
    try types_store.setVarContent(type_b_nominal, try types_store.mkNominal(
        undefined,
        type_b_backing,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // Set up TypeA = Type B
    try types_store.setVarContent(type_a_nominal, try types_store.mkNominal(
        undefined,
        type_b_nominal,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // assert that starting from the `TypeA` nominal, it works
    const result1 = occurs(&types_store, &scratch, type_a_nominal);
    try std.testing.expectEqual(.valid, result1);

    // assert that starting from the `TypeB` nominal, it works

    const result2 = occurs(&types_store, &scratch, type_b_nominal);
    try std.testing.expectEqual(.valid, result2);

    // assert that starting from the the tag union, it works

    const result3 = occurs(&types_store, &scratch, type_b_backing);
    try std.testing.expectEqual(.valid, result3);
}

test "occurs: valid nominal recursion does not hide later invalid recursion" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    // Invalid branch: Inner = (Inner,), which is structurally infinite.
    const invalid_inner = try types_store.fresh();
    const invalid_tuple_elems = try types_store.appendVars(&[_]Var{invalid_inner});
    try types_store.setRootVarContent(invalid_inner, .{ .structure = .{ .tuple = .{ .elems = invalid_tuple_elems } } });

    // Valid branch: List := [ Cons(List), Nil ].
    const list_nominal = try types_store.fresh();
    const ext = try types_store.fresh();
    const cons_tag_args = try types_store.appendVars(&[_]Var{list_nominal});
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };
    const list_backing = try types_store.freshFromContent(try types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));
    try types_store.setVarContent(list_nominal, try types_store.mkNominal(
        undefined,
        list_backing,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // The tuple elements are pushed in order and popped LIFO, so the valid branch
    // is observed first. The traversal must keep going and still report the
    // invalid branch.
    const root_elems = try types_store.appendVars(&[_]Var{ invalid_inner, list_nominal });
    const root = try types_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = root_elems } } });

    const result = occurs(&types_store, &scratch, root);
    try std.testing.expectEqual(.infinite, result);
    try std.testing.expectEqual(invalid_inner, scratch.err_var.?);
}

test "occurs: valid nominal return recursion does not hide invalid argument recursion" {
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    // Invalid argument branch: Arg = (Arg,), which is structurally infinite.
    const invalid_arg = try types_store.fresh();
    const invalid_arg_tuple_elems = try types_store.appendVars(&[_]Var{invalid_arg});
    try types_store.setRootVarContent(invalid_arg, .{ .structure = .{ .tuple = .{ .elems = invalid_arg_tuple_elems } } });

    // Valid return branch: List := [ Cons(List), Nil ].
    const list_nominal = try types_store.fresh();
    const ext = try types_store.fresh();
    const cons_tag_args = try types_store.appendVars(&[_]Var{list_nominal});
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };
    const list_backing = try types_store.freshFromContent(try types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));
    try types_store.setVarContent(list_nominal, try types_store.mkNominal(
        undefined,
        list_backing,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // This pins the same behavior for function traversal. Even if scheduling is
    // changed so the return is observed before the args, valid recursion in the
    // return must not hide an invalid argument cycle.
    const args = try types_store.appendVars(&.{invalid_arg});
    const root = try types_store.freshFromContent(.{ .structure = .{ .fn_pure = types.Func{
        .args = args,
        .ret = list_nominal,
        .needs_instantiation = false,
    } } });

    const result = occurs(&types_store, &scratch, root);
    try std.testing.expectEqual(.infinite, result);
    try std.testing.expectEqual(invalid_arg, scratch.err_var.?);
}

test "occurs: anonymous recursion in a nominal's type argument is not valid (regression)" {
    // Wrapper(Inner) := {}   where   Inner = [ Cons(Inner), Nil ]
    //
    // The recursion cycle is `Inner -> Cons -> Inner`. It lives entirely inside
    // the *type argument* of `Wrapper` and never passes through the nominal
    // `Wrapper` itself, so this is anonymous recursion (which Roc rejects), NOT
    // legal recursion-through-a-nominal.
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    // Inner = [ Cons(Inner), Nil ]  -- an anonymous, self-recursive tag union
    const inner = try types_store.fresh();
    const ext = try types_store.fresh();
    const cons_tag_args = try types_store.appendVars(&[_]Var{inner});
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };
    try types_store.setRootVarContent(inner, try types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));

    // Wrapper(Inner) := {}  -- nominal with `inner` as its only type argument
    const wrapper_backing = try types_store.freshFromContent(.{ .structure = .empty_record });
    const wrapper = try types_store.fresh();
    try types_store.setVarContent(wrapper, try types_store.mkNominal(
        undefined,
        wrapper_backing,
        &.{inner},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    const result = occurs(&types_store, &scratch, wrapper);
    try std.testing.expectEqual(.recursive_anonymous, result);
}

test "occurs: anonymous recursion below a buried nominal is not valid (regression)" {
    // Root = ( N, )   where   N := Inner   and   Inner = [ Cons(Inner), Nil ]
    //
    // `Root` is a tuple, so no nominal edge is crossed at the root. The only
    // nominal is `N`, buried one level down. The cycle is `Inner -> Cons ->
    // Inner`, which does NOT pass through `N`. A buggy occurs check that lets the
    // nominal marking from N's backing leak downward would wrongly classify this
    // as valid. Correct answer: recursive_anonymous.
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    // Inner = [ Cons(Inner), Nil ]
    const inner = try types_store.fresh();
    const ext = try types_store.fresh();
    const cons_tag_args = try types_store.appendVars(&[_]Var{inner});
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty() };
    try types_store.setRootVarContent(inner, try types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));

    // N := Inner  -- nominal whose backing is the anonymous recursive tag union
    const n = try types_store.fresh();
    try types_store.setVarContent(n, try types_store.mkNominal(
        undefined,
        inner,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
        false,
    ));

    // Root = ( N, )  -- a tuple so the root is NOT a nominal type
    const elems_range = try types_store.appendVars(&[_]Var{n});
    const root = try types_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems_range } } });

    const result = occurs(&types_store, &scratch, root);
    try std.testing.expectEqual(.recursive_anonymous, result);
}

test "occurs: tuple self-cycle below a tag union stays infinite (regression)" {
    // Outer = [ Wrap(Inner) ]   where   Inner = ( Inner, )
    //
    // The cycle is the tuple `Inner = (Inner,)`, which has no recursion-allowed
    // constructor in it, so it is an infinite type. But `recursion_allowed`, set
    // when descending into the outer tag union, leaks down into the tuple cycle
    // and downgrades the result to recursive_anonymous. Correct answer: infinite.
    const gpa = std.testing.allocator;
    var types_store = try Store.init(gpa);
    defer types_store.deinit();

    var scratch = try Scratch.init(gpa);
    defer scratch.deinit();

    // Inner = ( Inner, )  -- a tuple that directly contains itself
    const inner = try types_store.fresh();
    const tuple_elems = try types_store.appendVars(&[_]Var{inner});
    try types_store.setRootVarContent(inner, .{ .structure = .{ .tuple = .{ .elems = tuple_elems } } });

    // Outer = [ Wrap(Inner) ]  -- a tag union wrapping the tuple
    const ext = try types_store.fresh();
    const wrap_args = try types_store.appendVars(&[_]Var{inner});
    const wrap_tag = types.Tag{ .name = undefined, .args = wrap_args };
    const outer = try types_store.freshFromContent(try types_store.mkTagUnion(&.{wrap_tag}, ext));

    const result = occurs(&types_store, &scratch, outer);
    try std.testing.expectEqual(.infinite, result);
}
