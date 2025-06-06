//! This module contains the logic for checking if a variable is recursive

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

const Desc = types.Descriptor;
const Content = types.Content;
const Mark = types.Mark;
const Var = types.Var;
const CustomType = types.CustomType;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

const Result = enum {
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

    // Check for recursion
    var check_occurs = CheckOccurs.init(types_store, scratch);
    check_occurs.occurs(var_, Context.init()) catch |err| switch (err) {
        error.InfiniteType => {
            result = .infinite;
        },
        error.RecursiveAnonymous => {
            result = .recursive_anonymous;
        },
        error.RecursiveNominal => {
            result = .recursive_nominal;

            // If we recieved this error, then the we should've seen at least 1
            // nominal type in the chain of errors that lead to recursion.
            //
            // We iterate over the error chain, checking which were nominal
            // types. This is an extra iteration, but this should only happen in
            // rare cases so the perf hit should be negligible
            var iter = scratch.err_chain.iterIndices();
            while (iter.next()) |err_chain_idx| {
                const err_chain_var = scratch.err_chain.get(err_chain_idx).*;
                const root = types_store.resolveVar(err_chain_var);
                if (root.desc.content.unwrapCustomType() != null) {
                    scratch.appendErrChainNominalVar(root.var_);
                }
            }

            // We we somehow threw `RecursiveNominal` but didn't find any
            // nominal var,s this is a bug!
            std.debug.assert(scratch.err_chain_nominal_vars.len() > 0);
        },
    };

    // Reset the marks for all visited nodex
    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        types_store.setDescMark(visited_desc_idx, Mark.none);
    }

    return result;
}

/// This is an intermediate struct used when checking occurrences.
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
    fn occurs(self: *Self, var_: Var, ctx: Context) error{ InfiniteType, RecursiveAnonymous, RecursiveNominal }!void {
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
            defer self.scratch.popSeen();

            switch (root.desc.content) {
                .structure => |flat_type| {
                    switch (flat_type) {
                        .str => {},
                        .box => |sub_var| {
                            try self.occursSubVar(root.var_, sub_var, ctx.allowRecursion());
                        },
                        .list => |sub_var| {
                            try self.occursSubVar(root.var_, sub_var, ctx.allowRecursion());
                        },
                        .tuple => |tuple| {
                            const elems = self.types_store.getTupleElemsSlice(tuple.elems);
                            try self.occursSubVars(root.var_, elems, ctx);
                        },
                        .num => {},
                        .custom_type => |custom_type| {
                            const args = self.types_store.getCustomTypeArgsSlice(custom_type.args);
                            try self.occursSubVars(root.var_, args, ctx);
                            try self.occursSubVar(root.var_, custom_type.backing_var, ctx.markNominal());
                        },
                        .func => |func| {
                            const args = self.types_store.getFuncArgsSlice(func.args);
                            try self.occursSubVars(root.var_, args, ctx);
                            try self.occursSubVar(root.var_, func.ret, ctx);
                            try self.occursSubVar(root.var_, func.eff, ctx);
                        },
                        .record => |record| {
                            const fields = self.types_store.getRecordFieldsSlice(record.fields);
                            try self.occursSubVars(root.var_, fields.items(.var_), ctx.allowRecursion());
                            try self.occursSubVar(root.var_, record.ext, ctx);
                        },
                        .tag_union => |tag_union| {
                            const tags = self.types_store.getTagsSlice(tag_union.tags);
                            for (tags.items(.args)) |tag_args| {
                                const args = self.types_store.getTagArgsSlice(tag_args);
                                try self.occursSubVars(root.var_, args, ctx.allowRecursion());
                            }
                            try self.occursSubVar(root.var_, tag_union.ext, ctx);
                        },
                        .empty_record => {},
                        .empty_tag_union => {},
                    }
                },
                .alias => |alias| {
                    const args = self.types_store.getAliasArgsSlice(alias.args);
                    try self.occursSubVars(root.var_, args, ctx);
                    try self.occursSubVar(root.var_, alias.backing_var, ctx);
                },
                .flex_var => {},
                .rigid_var => {},
                .effectful => {},
                .pure => {},
                .err => {},
            }

            self.scratch.appendVisited(root.desc_idx);
            self.types_store.setDescMark(root.desc_idx, Mark.visited);
        }
    }

    /// Check if a sub var is recursive
    /// In the event of an error, append the root var to the chain
    fn occursSubVar(self: *Self, root_var: Var, sub_var: Var, ctx: Context) error{ InfiniteType, RecursiveAnonymous, RecursiveNominal }!void {
        self.occurs(sub_var, ctx) catch |err| {
            self.scratch.appendErrChain(root_var);
            return err;
        };
    }

    /// Check if a slice of sub vars are recursive
    /// In the event of an error, append the root var to the chain
    fn occursSubVars(self: *Self, root_var: Var, sub_vars: []Var, ctx: Context) error{ InfiniteType, RecursiveAnonymous, RecursiveNominal }!void {
        for (sub_vars) |sub_var| {
            try self.occursSubVar(root_var, sub_var, ctx);
        }
    }
};

/// This type represents the context of a recursive branch in the occurs check
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
const Scratch = struct {
    const Self = @This();

    gpa: std.mem.Allocator,

    seen: Var.SafeList,
    err_chain: Var.SafeList,
    err_chain_nominal_vars: Var.SafeList,
    visited: MkSafeList(DescStoreIdx),

    fn init(gpa: std.mem.Allocator) Self {
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

    fn deinit(self: *Self) void {
        self.seen.deinit(self.gpa);
        self.err_chain.deinit(self.gpa);
        self.err_chain_nominal_vars.deinit(self.gpa);
        self.visited.deinit(self.gpa);
    }

    fn reset(self: *Self) void {
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

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const str_var = types_store.freshFromContent(Content{ .structure = .str });

    const result = occurs(&types_store, &scratch, str_var);
    try std.testing.expectEqual(.not_recursive, result);
}

test "occurs: direct recursion (v = List v)" {
    const gpa = std.testing.allocator;
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const backing_var = types_store.fresh();
    const v = types_store.fresh();

    const args = types_store.appendAliasArgs(&[_]Var{v});

    try types_store.setRootVarContent(v, Content{
        .alias = .{
            .ident = types.TypeIdent{ .ident_idx = undefined },
            .args = args,
            .backing_var = backing_var,
        },
    });

    const result = occurs(&types_store, &scratch, v);
    try std.testing.expectEqual(.infinite, result);

    const err_chain = scratch.errChainSlice();
    try std.testing.expectEqual(1, err_chain.len);
    try std.testing.expectEqual(v, err_chain[0]);
}

test "occurs: alias with no recursion (v = Alias Str)" {
    const gpa = std.testing.allocator;
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const backing_var = types_store.fresh();
    const str_var = types_store.freshFromContent(Content{ .structure = .str });
    const args = types_store.appendAliasArgs(&[_]Var{str_var});

    const alias_var = types_store.fresh();
    try types_store.setRootVarContent(alias_var, Content{ .alias = .{
        .ident = types.TypeIdent{ .ident_idx = undefined },
        .args = args,
        .backing_var = backing_var,
    } });

    const result = occurs(&types_store, &scratch, alias_var);
    try std.testing.expectEqual(.not_recursive, result);
}

test "occurs: recursive tag union (v = [ Cons(elem, v), Nil ]" {
    const gpa = std.testing.allocator;
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
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
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const custom_type = types_store.fresh();

    const elem = types_store.fresh();
    const ext = types_store.fresh();

    const cons_tag_args = types_store.appendTagArgs(&[_]Var{ elem, custom_type });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };

    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty };

    const tags = types_store.appendTags(&[_]Tag{ cons_tag, nil_tag });

    const tag_union = types_store.freshFromContent(.{ .structure = .{ .tag_union = TagUnion{ .tags = tags, .ext = ext } } });

    try types_store.setRootVarContent(custom_type, .{ .structure = .{ .custom_type = CustomType{
        .ident = undefined,
        .args = Var.SafeList.Range.empty,
        .backing_var = tag_union,
    } } });

    // assert that starting from the nominal type, it works

    const result1 = occurs(&types_store, &scratch, custom_type);
    try std.testing.expectEqual(.recursive_nominal, result1);

    const err_chain1 = scratch.errChainSlice();
    try std.testing.expectEqual(2, err_chain1.len);
    try std.testing.expectEqual(tag_union, err_chain1[0]);
    try std.testing.expectEqual(custom_type, err_chain1[1]);

    const err_chain_nominal1 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(1, err_chain_nominal1.len);
    try std.testing.expectEqual(custom_type, err_chain_nominal1[0]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }

    // assert that starting from the the tag union, it works

    const result2 = occurs(&types_store, &scratch, tag_union);
    try std.testing.expectEqual(.recursive_nominal, result2);

    const err_chain2 = scratch.errChainSlice();
    try std.testing.expectEqual(2, err_chain2.len);
    try std.testing.expectEqual(custom_type, err_chain2[0]);
    try std.testing.expectEqual(tag_union, err_chain2[1]);

    const err_chain_nominal2 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(1, err_chain_nominal2.len);
    try std.testing.expectEqual(custom_type, err_chain_nominal2[0]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }
}

test "occurs: recursive tag union with multiple nominals (List : Other, Other : [ Cons(Elem, List), Nil ])" {
    const gpa = std.testing.allocator;
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var types_store = Store.init(&module_env);
    defer types_store.deinit();

    var scratch = Scratch.init(gpa);
    defer scratch.deinit();

    const list_nominal = types_store.fresh(); // List
    const other_nominal = types_store.fresh(); // Other

    const elem = types_store.fresh();
    const ext = types_store.fresh();

    const cons_tag_args = types_store.appendTagArgs(&[_]Var{ elem, list_nominal });
    const cons_tag = types.Tag{ .name = undefined, .args = cons_tag_args };

    const nil_tag = types.Tag{ .name = undefined, .args = Var.SafeList.Range.empty };

    const tags = types_store.appendTags(&[_]Tag{ cons_tag, nil_tag });

    const tag_union = types_store.freshFromContent(.{ .structure = .{ .tag_union = TagUnion{ .tags = tags, .ext = ext } } });

    // Other = [ Cons(Elem, List), Nil ]
    try types_store.setRootVarContent(other_nominal, .{ .structure = .{ .custom_type = CustomType{
        .ident = undefined,
        .args = Var.SafeList.Range.empty,
        .backing_var = tag_union,
    } } });

    // List = Other
    try types_store.setRootVarContent(list_nominal, .{ .structure = .{ .custom_type = CustomType{
        .ident = undefined,
        .args = Var.SafeList.Range.empty,
        .backing_var = other_nominal,
    } } });

    // assert that starting from the nominal type, it works

    const result1 = occurs(&types_store, &scratch, list_nominal);
    try std.testing.expectEqual(.recursive_nominal, result1);

    const err_chain1 = scratch.errChainSlice();
    try std.testing.expectEqual(3, err_chain1.len);
    try std.testing.expectEqual(tag_union, err_chain1[0]);
    try std.testing.expectEqual(other_nominal, err_chain1[1]);
    try std.testing.expectEqual(list_nominal, err_chain1[2]);

    const err_chain_nominal1 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(2, err_chain_nominal1.len);
    try std.testing.expectEqual(other_nominal, err_chain_nominal1[0]);
    try std.testing.expectEqual(list_nominal, err_chain_nominal1[1]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }

    // assert that starting from the the tag union, it works

    const result2 = occurs(&types_store, &scratch, tag_union);
    try std.testing.expectEqual(.recursive_nominal, result2);

    const err_chain2 = scratch.errChainSlice();
    try std.testing.expectEqual(3, err_chain2.len);
    try std.testing.expectEqual(other_nominal, err_chain2[0]);
    try std.testing.expectEqual(list_nominal, err_chain2[1]);
    try std.testing.expectEqual(tag_union, err_chain2[2]);

    const err_chain_nominal2 = scratch.errChainNominalVarsSlice();
    try std.testing.expectEqual(2, err_chain_nominal2.len);
    try std.testing.expectEqual(other_nominal, err_chain_nominal2[0]);
    try std.testing.expectEqual(list_nominal, err_chain_nominal2[1]);

    for (scratch.visited.items.items[0..]) |visited_desc_idx| {
        try std.testing.expectEqual(Mark.none, types_store.getDesc(visited_desc_idx).mark);
    }
}
