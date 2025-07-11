//! Checks whether a type variable is recursively defined.
//!
//! This module implements an "occurs check" to detect:
//! - structurally infinite types (e.g., `a = List a`)
//! - recursion through unnamed constructs (anonymous recursion)
//! - recursion through named nominal types (permitted in Roc under some rules)
//!
//! The main entrypoint is `occurs()`. It analyzes the type graph rooted at a variable
//! and reports whether the structure is recursive, and if so, what kind of recursion.
//!
//! The traversal uses a shared `Scratch` value to track visited nodes, recursion
//! chains, and to temporarily mark variables during analysis. This is reset between runs.
//!
//! The check only mutates the `Mark` field of descriptors in `Store`, and all marks
//! are reset before returning. Other descriptor state remains unchanged.

const std = @import("std");

const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const types = @import("../../types/types.zig");
const store = @import("../../types/store.zig");

const Ident = base.Ident;

const MkSafeList = collections.SafeList;
const exitOnOutOfMemory = collections.utils.exitOnOom;

const Store = store.Store;
const DescStoreIdx = store.DescStoreIdx;
const ResolvedVarDesc = store.ResolvedVarDesc;

const Desc = types.Descriptor;
const Content = types.Content;
const Mark = types.Mark;
const Var = types.Var;
const NominalType = types.NominalType;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

/// The result of checking for recursion
///
/// If the result is `recursive_nominal`, you can use `scratch.err_chain_nominal_vars`
/// to check what nominal vars were encountered
pub const Result = enum {
    not_recursive,
    recursive_nominal,
    recursive_anonymous,
    infinite,
};

/// Check if a variable is recursive
///
/// This uses `Scratch` as to hold intermediate values. `occurs` will reset it
/// before each run.
///
/// If the result is `recursive_nominal`, you can use `scratch.err_chain_nominal_vars`
/// to check what nominal vars were encountered
///
/// This function accepts a mutable reference to `Store`, but guarantees that it
/// _only_ modifies a variable's `Mark`. Before returning, all visited nodes'
/// `Mark`s will be reset to `none`.
///
/// TODO: See if there's a way to represent this ^ in the type system? If we
/// switch the types_store descriptors to use a multi list (which we should do
/// anyway), maybe we can only pass in only a mutable ref to the backing `Mark`s
/// array?
pub fn occurs(types_store: *Store, scratch: *Scratch, var_: Var) Result {
    scratch.reset();

    var result: Result = .not_recursive;

    // Check if we're starting from a nominal type
    const root = types_store.resolveVar(var_);
    const initial_context = if (root.desc.content == .structure and root.desc.content.structure == .nominal_type)
        Context.init().markNominal()
    else
        Context.init();

    // Check for recursion
    var check_occurs = CheckOccurs.init(types_store, scratch);
    check_occurs.occurs(var_, initial_context) catch |err| switch (err) {
        error.InfiniteType => {
            result = .infinite;
        },
        error.RecursiveAnonymous => {
            result = .recursive_anonymous;
        },
        error.RecursiveNominal => {
            // We we somehow threw `RecursiveNominal` but didn't find any
            // nominal vars, this is a bug!
            std.debug.assert(scratch.err_chain_nominal_vars.len() > 0);

            result = .recursive_nominal;
        },
    };

    // Reset the marks for all visited nodes
    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        types_store.setDescMark(visited_desc_idx, Mark.none);
    }

    return result;
}

/// Performs an occurs check on a type variable.
///
/// This struct encapsulates the recursive traversal logic used to detect
/// whether a variable is recursively defined through it's children.
///
/// It uses a scratch space to track visited nodes and maintain state across
/// recursive calls. It is intended for one-time use per `occurs()` call.
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
    ///
    /// This function accepts a mutable reference to `Store`, and _must_ only
    /// modify a var's `Mark`
    fn init(types_store: *Store, scratch: *Scratch) Self {
        return .{ .types_store = types_store, .scratch = scratch };
    }

    const Error = error{ InfiniteType, RecursiveAnonymous, RecursiveNominal };

    /// Recursively check if a type is referenced by it's children
    ///
    /// This method appends intermediate error chain information to the `scratch`,
    /// which can be queried after the run.
    fn occurs(self: *Self, var_: Var, ctx: Context) Error!void {
        const root = self.types_store.resolveVar(var_);

        if (root.desc.mark == .visited) {
            // If we've already visited this var and not errored, then it's not recursive
            return;
        } else if (self.scratch.hasSeenVar(root.var_)) {
            // Recursion point! We've already seen this var during traversal.
            if (ctx.recursion_allowed and ctx.seen_nominal) {
                // If recursion is allowed (we've passed through a Box, List or
                // Tag Union) AND at somepoint in the chain we've seen a nominal
                // type, then this recursion is okay
                return error.RecursiveNominal;
            } else if (ctx.recursion_allowed and !ctx.seen_nominal) {
                // If recursion is allowed (we've passed through a Box, List or
                // Tag Union) BUT we haven't seen any nominal types, then this
                // recursion is anonymous and not okay
                return error.RecursiveAnonymous;
            } else {
                // Otherwise, this is an infinite type
                return error.InfiniteType;
            }
        } else {
            self.scratch.appendSeen(var_);
            switch (root.desc.content) {
                .structure => |flat_type| {
                    switch (flat_type) {
                        .str => {},
                        .box => |sub_var| {
                            try self.occursSubVar(root, sub_var, ctx.allowRecursion());
                        },
                        .list => |sub_var| {
                            try self.occursSubVar(root, sub_var, ctx.allowRecursion());
                        },
                        .list_unbound => {
                            // list_unbound has no sub-variables to check
                        },
                        .tuple => |tuple| {
                            const elems = self.types_store.getTupleElemsSlice(tuple.elems);
                            try self.occursSubVars(root, elems, ctx);
                        },
                        .num => {},
                        .nominal_type => |nominal_type| {
                            // Check all argument vars using iterator
                            var arg_iter = self.types_store.iterNominalArgs(nominal_type);
                            while (arg_iter.next()) |arg_var| {
                                try self.occursSubVar(root, arg_var, ctx);
                            }
                            // Check backing var using helper method
                            const backing_var = self.types_store.getNominalBackingVar(nominal_type);
                            try self.occursSubVar(root, backing_var, ctx.markNominal());
                        },
                        .fn_pure => |func| {
                            const args = self.types_store.getFuncArgsSlice(func.args);
                            try self.occursSubVars(root, args, ctx);
                            try self.occursSubVar(root, func.ret, ctx);
                        },
                        .fn_effectful => |func| {
                            const args = self.types_store.getFuncArgsSlice(func.args);
                            try self.occursSubVars(root, args, ctx);
                            try self.occursSubVar(root, func.ret, ctx);
                        },
                        .fn_unbound => |func| {
                            const args = self.types_store.getFuncArgsSlice(func.args);
                            try self.occursSubVars(root, args, ctx);
                            try self.occursSubVar(root, func.ret, ctx);
                        },
                        .record => |record| {
                            const fields = self.types_store.getRecordFieldsSlice(record.fields);
                            try self.occursSubVars(root, fields.items(.var_), ctx.allowRecursion());
                            try self.occursSubVar(root, record.ext, ctx);
                        },
                        .record_unbound => |fields| {
                            const fields_slice = self.types_store.getRecordFieldsSlice(fields);
                            try self.occursSubVars(root, fields_slice.items(.var_), ctx.allowRecursion());
                        },
                        .record_poly => |poly| {
                            const fields = self.types_store.getRecordFieldsSlice(poly.record.fields);
                            try self.occursSubVars(root, fields.items(.var_), ctx.allowRecursion());
                            try self.occursSubVar(root, poly.record.ext, ctx);
                            try self.occursSubVar(root, poly.var_, ctx);
                        },
                        .tag_union => |tag_union| {
                            const tags = self.types_store.getTagsSlice(tag_union.tags);
                            for (tags.items(.args)) |tag_args| {
                                const args = self.types_store.getTagArgsSlice(tag_args);
                                try self.occursSubVars(root, args, ctx.allowRecursion());
                            }
                            try self.occursSubVar(root, tag_union.ext, ctx);
                        },
                        .empty_record => {},
                        .empty_tag_union => {},
                    }
                },
                .alias => |alias| {
                    // Check all argument vars using iterator
                    var arg_iter = self.types_store.iterAliasArgs(alias);
                    while (arg_iter.next()) |arg_var| {
                        try self.occursSubVar(root, arg_var, ctx);
                    }
                    const backing_var = self.types_store.getAliasBackingVar(alias);
                    try self.occursSubVar(root, backing_var, ctx);
                },
                .flex_var => {},
                .rigid_var => {},
                .err => {},
            }
            self.scratch.popSeen();

            self.scratch.appendVisited(root.desc_idx);
            self.types_store.setDescMark(root.desc_idx, Mark.visited);
        }
    }

    /// Check if a sub var is recursive
    /// In the event of an error, append the root var to the chain
    fn occursSubVar(self: *Self, root: ResolvedVarDesc, sub_var: Var, ctx: Context) Error!void {
        self.occurs(sub_var, ctx) catch |err| {
            self.scratch.appendErrChain(root.var_);
            if (root.desc.content.unwrapNominalType() != null) {
                self.scratch.appendErrChainNominalVar(root.var_);
            }
            return err;
        };
    }

    /// Check if a slice of sub vars are recursive
    /// In the event of an error, append the root var to the chain
    fn occursSubVars(self: *Self, root_var: ResolvedVarDesc, sub_vars: []Var, ctx: Context) Error!void {
        for (sub_vars) |sub_var| {
            try self.occursSubVar(root_var, sub_var, ctx);
        }
    }
};

/// This type represents the context of a recursive branch in the occurs check
///
/// This types is passed down through the occurs check and is modified to keep
/// track of whatever we've seen so far in this branch.
const Context = struct {
    recursion_allowed: bool,
    seen_nominal: bool,

    fn init() Context {
        return .{ .recursion_allowed = false, .seen_nominal = false };
    }

    /// Mark that recursion is allowed
    /// ie the chain passes through a Box, List or Tag Union
    fn allowRecursion(self: Context) Context {
        return .{
            .recursion_allowed = true,
            .seen_nominal = self.seen_nominal,
        };
    }

    /// Mark that we have seen a nominal type
    fn markNominal(self: Context) Context {
        return .{
            .recursion_allowed = self.recursion_allowed,
            .seen_nominal = true,
        };
    }
};

/// Struct to hold intermediate values used during occurs check
pub const Scratch = struct {
    const Self = @This();

    gpa: std.mem.Allocator,

    seen: Var.SafeList,
    err_chain: Var.SafeList,
    err_chain_nominal_vars: Var.SafeList,
    visited: MkSafeList(DescStoreIdx),

    pub fn init(gpa: std.mem.Allocator) Self {
        // TODO: eventually use herusitics here to determine sensible defaults
        // Rust compiler inits with 1024 capacity. But that feels like a lot.
        // Typical recursion cases should only be a few layers deep?
        return .{
            .gpa = gpa,
            .seen = Var.SafeList.initCapacity(gpa, 32),
            .err_chain = Var.SafeList.initCapacity(gpa, 32),
            .err_chain_nominal_vars = Var.SafeList.initCapacity(gpa, 8),
            .visited = MkSafeList(DescStoreIdx).initCapacity(gpa, 64),
        };
    }

    pub fn deinit(self: *Self) void {
        self.seen.deinit(self.gpa);
        self.err_chain.deinit(self.gpa);
        self.err_chain_nominal_vars.deinit(self.gpa);
        self.visited.deinit(self.gpa);
    }

    pub fn reset(self: *Self) void {
        self.seen.items.clearRetainingCapacity();
        self.err_chain.items.clearRetainingCapacity();
        self.err_chain_nominal_vars.items.clearRetainingCapacity();
        self.visited.items.clearRetainingCapacity();
    }

    fn hasSeenVar(self: *const Self, var_: Var) bool {
        for (self.seen.items.items) |seen_var| {
            if (seen_var == var_) return true;
        }
        return false;
    }

    fn appendSeen(self: *Self, var_: Var) void {
        _ = self.seen.append(self.gpa, var_);
    }

    fn popSeen(self: *Self) void {
        _ = self.seen.items.pop();
    }

    fn appendVisited(self: *Self, desc_idx: DescStoreIdx) void {
        _ = self.visited.append(self.gpa, desc_idx);
    }

    fn appendErrChain(self: *Self, var_: Var) void {
        _ = self.err_chain.append(self.gpa, var_);
    }

    fn appendErrChainNominalVar(self: *Self, var_: Var) void {
        _ = self.err_chain_nominal_vars.append(self.gpa, var_);
    }

    fn errChainSlice(self: *const Scratch) []const Var {
        return self.err_chain.items.items;
    }

    fn errChainNominalVarsSlice(self: *const Scratch) []const Var {
        return self.err_chain_nominal_vars.items.items;
    }
};

test "occurs: no recurcion (v = Str)" {
    const gpa = std.testing.allocator;

    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const str_var = types_store.freshFromContent(Content{ .structure = .str });

    const result = occurs(&types_store, &scratch, str_var);
    try std.testing.expectEqual(.not_recursive, result);
}

test "occurs: direct recursion (v = List v)" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const list_var = types_store.fresh();
    const list_content = Content{
        .structure = .{ .list = list_var },
    };
    try types_store.setRootVarContent(list_var, list_content);

    const result = occurs(&types_store, &scratch, list_var);
    try std.testing.expectEqual(.recursive_anonymous, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expectEqual(1, err_chain.len);
    try std.testing.expectEqual(list_var, err_chain[0]);
}

test "occurs: indirect recursion (v1 = Box v2, v2 = List v1)" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const v1 = types_store.fresh();
    const v2 = types_store.fresh();

    try types_store.setRootVarContent(v1, Content{ .structure = .{ .box = v2 } });
    try types_store.setRootVarContent(v2, Content{ .structure = .{ .list = v1 } });

    const result = occurs(&types_store, &scratch, v1);
    try std.testing.expectEqual(.recursive_anonymous, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expectEqual(2, err_chain.len);
    try std.testing.expectEqual(v2, err_chain[0]);
    try std.testing.expectEqual(v1, err_chain[1]);
}

test "occurs: no recursion through two levels (v1 = Box v2, v2 = Str)" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const v1 = types_store.fresh();
    const v2 = types_store.fresh();

    try types_store.setRootVarContent(v1, Content{ .structure = .{ .box = v2 } });
    try types_store.setRootVarContent(v2, Content{ .structure = .str });

    const result = occurs(&types_store, &scratch, v1);
    try std.testing.expectEqual(.not_recursive, result);
}

test "occurs: tuple recursion (v = Tuple(v, Str))" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const v = types_store.fresh();
    const str_var = types_store.freshFromContent(Content{ .structure = .str });

    const elems_range = types_store.appendTupleElems(&[_]Var{ v, str_var });
    const tuple = types.Tuple{ .elems = elems_range };

    try types_store.setRootVarContent(v, Content{ .structure = .{ .tuple = tuple } });

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.infinite, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expectEqual(1, err_chain.len);
    try std.testing.expectEqual(v, err_chain[0]);
}

test "occurs: tuple not recursive (v = Tuple(Str, Str))" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const str_var = types_store.freshFromContent(Content{ .structure = .str });

    const elems_range = types_store.appendTupleElems(&[_]Var{ str_var, str_var });
    const tuple = types.Tuple{ .elems = elems_range };

    const v = types_store.freshFromContent(Content{ .structure = .{ .tuple = tuple } });

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.not_recursive, result);

    try std.testing.expectEqual(2, scratch.visited.len());
}

test "occurs: recursive alias (v = Alias(List v))" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const v = types_store.fresh();
    const backing_var = types_store.fresh(); // backing var at v+1
    const arg = types_store.freshRedirect(v); // arg at v+2 redirecting to v (creating infinite recursion on purpose for the test)

    try types_store.setRootVarContent(v, types_store.mkAlias(
        types.TypeIdent{ .ident_idx = undefined },
        backing_var,
        &.{arg},
    ));

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.infinite, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expectEqual(1, err_chain.len);
    try std.testing.expectEqual(v, err_chain[0]);
}

test "occurs: alias with no recursion (v = Alias Str)" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const alias_var = types_store.fresh();
    const backing_var = types_store.freshFromContent(Content{ .structure = .str });
    const arg_var = types_store.freshFromContent(Content{ .structure = .str });

    try types_store.setRootVarContent(alias_var, types_store.mkAlias(
        types.TypeIdent{ .ident_idx = undefined },
        backing_var,
        &.{arg_var},
    ));

    const result = occurs(&types_store, &scratch, alias_var);
    try std.testing.expectEqual(.not_recursive, result);
}

test "occurs: recursive tag union (v = [ Cons(elem, v), Nil ]" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const linked_list = types_store.fresh();
    const elem = types_store.fresh();
    const ext = types_store.fresh();

    const cons_tag_args = types_store.appendTagArgs(&[_]Var{ elem, linked_list });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };

    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty };

    const tags = types_store.appendTags(&[_]Tag{ cons_tag, nil_tag });

    const tag_union = TagUnion{ .tags = tags, .ext = ext };

    try types_store.setRootVarContent(linked_list, .{ .structure = .{ .tag_union = tag_union } });

    const result = occurs(&types_store, &scratch, linked_list);
    try std.testing.expectEqual(.recursive_anonymous, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expectEqual(1, err_chain.len);
    try std.testing.expectEqual(linked_list, err_chain[0]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }
}
test "occurs: nested recursive tag union (v = [ Cons(elem, Box(v)) ] )" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const linked_list = types_store.fresh();
    const elem = types_store.fresh();

    // Wrap the recursive var in a Box to simulate nesting
    const boxed_linked_list = types_store.fresh();
    try types_store.setRootVarContent(boxed_linked_list, .{ .structure = .{ .box = linked_list } });

    // Build tag args: (elem, Box(linked_list))
    const cons_tag_args = types_store.appendTagArgs(&[_]Var{ elem, boxed_linked_list });

    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty };

    const tags = types_store.appendTags(&[_]Tag{ cons_tag, nil_tag });

    const tag_union = TagUnion{ .tags = tags, .ext = types_store.fresh() };

    try types_store.setRootVarContent(linked_list, .{ .structure = .{ .tag_union = tag_union } });

    const result = occurs(&types_store, &scratch, linked_list);
    try std.testing.expectEqual(.recursive_anonymous, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expect(err_chain.len == 2);
    try std.testing.expectEqual(err_chain[0], boxed_linked_list);
    try std.testing.expectEqual(err_chain[1], linked_list);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }
}

test "occurs: recursive tag union (v = List: [ Cons(Elem, List), Nil ])" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const nominal_type = types_store.fresh();

    const elem = types_store.fresh();
    const ext = types_store.fresh();

    const cons_tag_args = types_store.appendTagArgs(&[_]Var{ elem, nominal_type });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty };
    const backing_var = types_store.freshFromContent(types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));
    try types_store.setVarContent(nominal_type, types_store.mkNominal(
        undefined,
        backing_var,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
    ));

    // assert that starting from the nominal type, it works

    const result1 = occurs(&types_store, &scratch, nominal_type);
    try std.testing.expectEqual(.recursive_nominal, result1);

    const err_chain1 = scratch.errChainSlice();
    try std.testing.expectEqual(2, err_chain1.len);
    try std.testing.expectEqual(backing_var, err_chain1[0]);
    try std.testing.expectEqual(nominal_type, err_chain1[1]);

    const err_chain_nominal1 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(1, err_chain_nominal1.len);
    try std.testing.expectEqual(nominal_type, err_chain_nominal1[0]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }

    // assert that starting from the the tag union, it works

    const result2 = occurs(&types_store, &scratch, backing_var);
    try std.testing.expectEqual(.recursive_nominal, result2);

    const err_chain2 = scratch.errChainSlice();
    try std.testing.expectEqual(2, err_chain2.len);
    try std.testing.expectEqual(nominal_type, err_chain2[0]);
    try std.testing.expectEqual(backing_var, err_chain2[1]);

    const err_chain_nominal2 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(1, err_chain_nominal2.len);
    try std.testing.expectEqual(nominal_type, err_chain_nominal2[0]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }
}

test "occurs: recursive tag union with multiple nominals (TypeA := TypeB, TypeB := [ Cons(Elem, TypeA), Nil ])" {
    const gpa = std.testing.allocator;
    var types_store = Store.init(gpa);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    // Create vars in the required order for adjacency to work out
    const type_b_nominal = types_store.fresh();
    const type_a_nominal = types_store.fresh();
    const elem = types_store.fresh();
    const ext = types_store.fresh();

    // Create the tag union content that references type_a_nominal
    const cons_tag_args = types_store.appendTagArgs(&[_]Var{ elem, type_a_nominal });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };
    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty };
    const type_b_backing = types_store.freshFromContent(types_store.mkTagUnion(&.{ cons_tag, nil_tag }, ext));

    // Set up TypeB = [ Cons(Elem, TypeA), Nil ]
    try types_store.setVarContent(type_b_nominal, types_store.mkNominal(
        undefined,
        type_b_backing,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
    ));

    // Set up TypeA = Type B
    try types_store.setVarContent(type_a_nominal, types_store.mkNominal(
        undefined,
        type_b_nominal,
        &.{},
        Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
    ));

    // assert that starting from the `TypeA` nominal, it works
    const result1 = occurs(&types_store, &scratch, type_a_nominal);
    try std.testing.expectEqual(.recursive_nominal, result1);

    const err_chain1 = scratch.errChainSlice();
    try std.testing.expectEqual(3, err_chain1.len);
    try std.testing.expectEqual(type_b_backing, err_chain1[0]);
    try std.testing.expectEqual(type_b_nominal, err_chain1[1]);
    try std.testing.expectEqual(type_a_nominal, err_chain1[2]);

    const err_chain_nominal1 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(2, err_chain_nominal1.len);
    try std.testing.expectEqual(type_b_nominal, err_chain_nominal1[0]);
    try std.testing.expectEqual(type_a_nominal, err_chain_nominal1[1]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }

    // assert that starting from the `TypeB` nominal, it works

    const result2 = occurs(&types_store, &scratch, type_b_nominal);
    try std.testing.expectEqual(.recursive_nominal, result2);

    const err_chain2 = scratch.errChainSlice();
    try std.testing.expectEqual(3, err_chain2.len);
    try std.testing.expectEqual(type_a_nominal, err_chain2[0]);
    try std.testing.expectEqual(type_b_backing, err_chain2[1]);
    try std.testing.expectEqual(type_b_nominal, err_chain2[2]);

    const err_chain_nominal2 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(2, err_chain_nominal2.len);
    try std.testing.expectEqual(type_a_nominal, err_chain_nominal2[0]);
    try std.testing.expectEqual(type_b_nominal, err_chain_nominal2[1]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }

    // assert that starting from the the tag union, it works

    const result3 = occurs(&types_store, &scratch, type_b_backing);
    try std.testing.expectEqual(.recursive_nominal, result3);

    const err_chain3 = scratch.errChainSlice();
    try std.testing.expectEqual(3, err_chain3.len);
    try std.testing.expectEqual(type_b_nominal, err_chain3[0]);
    try std.testing.expectEqual(type_a_nominal, err_chain3[1]);
    try std.testing.expectEqual(type_b_backing, err_chain3[2]);

    const err_chain_nominal3 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(2, err_chain_nominal3.len);
    try std.testing.expectEqual(type_b_nominal, err_chain_nominal3[0]);
    try std.testing.expectEqual(type_a_nominal, err_chain_nominal3[1]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }
}
