const std = @import("std");
const base = @import("../../base.zig");
const problem_mod = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Alias = @import("./Alias.zig");

const Ident = base.Ident;
const Region = base.Region;
const Module = base.Module;
const Problem = problem_mod.Problem;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

env: *base.ModuleEnv,
/// The custom alias that this file is centered around, if one has been defined.
focused_custom_alias: ?Alias.Idx = null,
// TODO: handle renaming, e.g. `CustomType := [ExportedName as LocalName]`
custom_tags: std.AutoHashMapUnmanaged(Ident.Idx, Alias.Idx) = .{},
/// Identifiers/aliases that are in scope, and defined in the current module.
levels: Levels,

/// Initialize a new scope.
pub fn init(
    env: *base.ModuleEnv,
    builtin_aliases: []const struct { alias: Alias.Idx, name: Ident.Idx },
    builtin_idents: []const Ident.Idx,
) Self {
    var scope = Self{ .env = env, .levels = Levels{ .env = env } };

    scope.levels.enter();

    for (builtin_idents) |builtin_ident| {
        _ = scope.levels.introduce(.ident, .{
            .scope_name = builtin_ident,
            .ident = builtin_ident,
        });
    }

    for (builtin_aliases) |builtin_alias| {
        _ = scope.levels.introduce(.alias, .{
            .scope_name = builtin_alias.name,
            .alias = builtin_alias.alias,
        });
    }

    return scope;
}

/// Deinitialize a scope's memory
pub fn deinit(self: *Self) void {
    self.custom_tags.deinit(self.env.gpa);
    self.levels.deinit(self.env.gpa);
}

/// Generates a unique ident like "1" or "5" in the home module.
///
/// This is used, for example, during canonicalization of an Expr::Closure
/// to generate a unique ident to refer to that closure.
pub fn genUnique(self: *Self) Ident.Idx {
    const unique_idx = self.env.idents.genUnique();

    _ = self.levels.introduce(.ident, .{
        .scope_name = unique_idx,
        .ident = unique_idx,
    });

    return unique_idx;
}

/// todo
pub fn Contains(item_kind: Level.ItemKind) type {
    return union(enum) {
        InScope: Level.Name(item_kind),
        NotInScope: Level.Name(item_kind),
        NotPresent,
    };
}

/// todo
pub fn LookupResult(item_kind: Level.ItemKind) type {
    return union(enum) {
        InScope: Level.Name(item_kind),
        Problem: Problem,
    };
}

/// todo
pub const Level = struct {
    idents: std.ArrayListUnmanaged(IdentInScope) = .{},
    aliases: std.ArrayListUnmanaged(AliasInScope) = .{},
    /// todo
    pub const ItemKind = enum { ident, alias };
    /// todo
    pub fn Item(comptime item_kind: ItemKind) type {
        return switch (item_kind) {
            .ident => IdentInScope,
            .alias => AliasInScope,
        };
    }

    /// todo
    pub fn ItemName(comptime item_kind: ItemKind) type {
        return switch (item_kind) {
            .ident => Ident.Idx,
            .alias => Ident.Idx,
        };
    }

    /// todo
    pub fn items(level: *Level, comptime item_kind: ItemKind) *std.ArrayListUnmanaged(Item(item_kind)) {
        return switch (item_kind) {
            .ident => &level.idents,
            .alias => &level.aliases,
        };
    }
    /// todo
    pub const IdentInScope = struct {
        scope_name: Ident.Idx,
        ident: Ident.Idx,
    };
    /// todo
    pub const AliasInScope = struct {
        scope_name: Ident.Idx,
        alias: Alias.Idx,
    };
    /// todo
    pub fn deinit(self: *Level, gpa: std.mem.Allocator) void {
        self.idents.deinit(gpa);
        self.aliases.deinit(gpa);
    }
};

/// Bucket sizes: 1B, 2B, 3B, 4B, 8B, 16B, 17B+
const num_buckets = 7;

const max_small_str_len: usize = 16;

pub const ScopeStack = struct {
    /// For each level in the stack, we track the lengths of each bucket.
    levels: std.ArrayListUnmanaged([num_buckets]u32),

    /// Each bucket holds small strings of one particular sizes.
    /// They each have different sizes (e.g. 4B small strings vs 8B small strings),
    /// but we use 128-bit SIMD operations on all of them, so they all need 128-bit
    /// alignment. It's num_buckets - 1 because large strings are stored separately.
    small_str_buckets: [num_buckets - 1](*const @Vector(1, u128)),

    /// The flag is just a bit for whether it's a var or a const.
    small_str_flags: [num_buckets - 1](*const ConstOrVar),

    /// The capacities for each of these allocations. (The levels determine the lengths.)
    small_str_capacities: [num_buckets - 1]u32,

    /// The CanId values for each of these declarations
    can_ids: [num_buckets]CanId,

    large_str_buckets: std.ArrayListUnmanaged(std.AutoHashMap([]u8, ConstOrVar)),

    /// Assign the given name to the given value.
    /// If there was a `var` before this, then `is_var_decl` is true.
    fn assign(self: *ScopeStack, str: []u8, can_id: CanId, is_var_decl: bool) Assigned!AssignErr {
        const len = str.len;

        if (len <= max_small_str_len) {
            // 1-4 => 0-3
            const small_bucket_index = len - 1;

            // 5-8 => 4
            // 9-16 => 5
            const big_bucket_index = (len << 3) + 3;

            const bucket_index = if (len <= 4) small_bucket_index else big_bucket_index;
            const bucket = &self.buckets[bucket_index];

            switch (bucket_index) {
                0 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    // TODO bump capacity, copy the u8 over
                },
                1 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    // TODO bump capacity, make a u16, copy it in
                },
                2 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    // TODO bump capacity, make a u32 (zero-padded), copy it in
                },
                3 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    // TODO bump capacity, make a u32, copy it in
                },
                4 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    // TODO bump capacity, make a u64 (zero-padded), copy it in
                },
                5 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    // TODO bump capacity, make a u128 (zero-padded), copy it in
                },
            }
        } else {
            // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
            // It's big! Bump capacity, then put it into the hashmap at the current level.
        }
    }
};

/// Whether this assignment turned out to be a new const declaration or
/// a valid reassignment of a `var` that was in scope.
pub const Assigned = union(enum) {
    /// This `=` is assigning a new constant.
    NewConst: void,
    /// This `=` is reassigning an existing `var` declaration,
    /// and the id is the id of that var declaration.
    VarReassign: CanId,
};

pub const AssignErr = union(enum) {
    /// This is an attempt to assign (with or without being a `var` decl)
    /// to a value that already has a constant with that name declared in scope.
    ShadowingConst: CanId,
    /// This is an attempt to use `var` to define something that already
    /// has a `var` by that name declared in scope.
    ShadowingVar: CanId,
};

pub const ConstOrVar = enum {
    Const,
    Var,
};

/// todo
pub const Levels = struct {
    env: *base.ModuleEnv,
    levels: std.ArrayListUnmanaged(Level) = .{},
    /// todo
    pub fn deinit(self: *Levels) void {
        self.levels.deinit(self.env.gpa);
    }
    /// todo
    pub fn enter(self: *Levels) void {
        self.levels.append(self.env.gpa, .{}) catch |err| exitOnOom(err);
    }
    /// todo
    pub fn exit(self: *Levels) void {
        if (self.levels.items.len <= 1) {
            self.env.problems.append(self.env.gpa, Problem.Compiler.make(.{
                .canonicalize = .exited_top_scope_level,
            })) catch |err| exitOnOom(err);
        } else {
            _ = self.levels.pop();
        }
    }
    /// todo
    pub fn iter(self: *Levels, comptime item_kind: Level.ItemKind) Iterator(item_kind) {
        return Iterator(item_kind).new(self);
    }
    fn contains(
        self: *Levels,
        comptime item_kind: Level.ItemKind,
        name: Level.ItemName(item_kind),
    ) ?Level.Item(item_kind) {
        var items_in_scope = Iterator(item_kind).new(self);
        while (items_in_scope.nextData()) |item_in_scope| {
            if (self.env.idents.identsHaveSameText(name, item_in_scope.scope_name)) {
                return item_in_scope;
            }
        }

        return null;
    }
    /// todo
    pub fn lookup(
        self: *Levels,
        comptime item_kind: Level.ItemKind,
        name: Level.ItemName(item_kind),
    ) Contains(item_kind) {
        if (self.contains(name)) |item_in_scope| {
            return Contains{ .InScope = item_in_scope };
        }

        const problem = undefined;
        switch (item_kind) {
            .ident => {
                const all_idents_in_scope = self.levels.iter(.ident);
                const options = self.env.ident_ids_for_slicing.extendFromIter(all_idents_in_scope);

                problem = Problem.Canonicalize.make(.{ .IdentNotInScope = .{
                    .ident = name,
                    .suggestions = options,
                } });
            },
            .alias => {
                const all_aliases_in_scope = self.levels.iter(.alias);
                const options = self.env.ident_ids_for_slicing.extendFromIter(all_aliases_in_scope);

                problem = Problem.Canonicalize.make(.{ .AliasNotInScope = .{
                    .name = name,
                    .suggestions = options,
                } });
            },
        }

        self.env.problems.append(problem) catch |err| exitOnOom(err);
        return LookupResult{ .Problem = problem };
    }
    /// todo
    pub fn introduce(
        self: *Levels,
        comptime item_kind: Level.ItemKind,
        scope_item: Level.Item(item_kind),
    ) Level.Item(item_kind) {
        if (self.contains(item_kind, scope_item.scope_name)) |item_in_scope| {
            const can_problem: Problem.Canonicalize = switch (item_kind) {
                .ident => .{ .IdentAlreadyInScope = .{
                    .original_ident = item_in_scope.scope_name,
                    .shadow = scope_item.scope_name,
                } },
                .alias => .{ .AliasAlreadyInScope = .{
                    .original_name = item_in_scope.scope_name,
                    .shadow = scope_item.scope_name,
                } },
            };

            _ = self.env.problems.append(self.env.gpa, Problem.Canonicalize.make(can_problem));
            // TODO: is this correct for shadows?
            return scope_item;
        }

        var last_level = self.levels.getLast();
        last_level.items(item_kind).append(self.env.gpa, scope_item) catch |err| exitOnOom(err);

        return scope_item;
    }
    /// todo
    pub fn Iterator(comptime item_kind: Level.ItemKind) type {
        return struct {
            levels: *Levels,
            level_index: usize,
            prior_item_index: usize,
            /// todo
            pub fn empty(levels: *Levels) Iterator(item_kind) {
                return Iterator(item_kind){
                    .levels = levels,
                    .level_index = 0,
                    .prior_item_index = 0,
                };
            }
            /// todo
            pub fn new(scope_levels: *Levels) Iterator(item_kind) {
                if (scope_levels.levels.items.len == 0) {
                    return empty(scope_levels);
                }

                const levels = scope_levels.levels.items;

                var level_index = levels.len -| 1;
                while (level_index > 0 and levels[level_index].items(item_kind).items.len == 0) {
                    level_index -= 1;
                }

                const prior_item_index = levels[level_index].items(item_kind).items.len;

                return Iterator(item_kind){
                    .levels = scope_levels,
                    .level_index = level_index,
                    .prior_item_index = prior_item_index,
                };
            }
            /// todo
            pub fn next(
                self: *Iterator(item_kind),
            ) ?Level.ItemName(item_kind) {
                if (self.prior_item_index == 0) {
                    return null;
                }

                const levels = self.levels.levels.items;
                var level = levels[self.level_index];
                const next_item = level.items(item_kind).items[self.prior_item_index - 1];

                self.prior_item_index -= 1;

                if (self.prior_item_index == 0) {
                    self.level_index -|= 1;

                    while (self.level_index > 0 and levels[self.level_index].items(item_kind).items.len == 0) {
                        self.level_index -= 1;
                    }
                }

                return next_item.scope_name;
            }
            /// todo
            pub fn nextData(
                self: *Iterator(item_kind),
            ) ?Level.Item(item_kind) {
                if (self.prior_item_index == 0) {
                    return null;
                }

                const levels = self.levels.levels.items;
                var level = levels[self.level_index];
                const next_item = level.items(item_kind).items[self.prior_item_index - 1];

                self.prior_item_index -= 1;

                if (self.prior_item_index == 0) {
                    self.level_index -|= 1;

                    while (self.level_index > 0 and levels[self.level_index].items(item_kind).items.len == 0) {
                        self.level_index -= 1;
                    }
                }

                return next_item;
            }
        };
    }
};

fn createTestScope(idents: [][]Level.IdentInScope, aliases: [][]Level.AliasInScope) Self {
    const gpa = std.testing.allocator;
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var scope = Self{
        .env = &env,
        .focused_custom_alias = null,
        .custom_tags = std.AutoHashMap(Ident.Idx, Alias.Idx).init(gpa),
        .levels = Levels.init(&env, gpa),
        .gpa = gpa,
    };
    scope.deinit();

    const max_level = @min(idents.len, aliases.len);
    for (0..max_level) |_| {
        scope.levels.enter();
    }

    if (idents.len > 0) {
        for (idents, 0..) |ident_level, level_index| {
            var level = scope.levels.levels.items[level_index];
            for (ident_level) |ident_in_scope| {
                level.idents.append(ident_in_scope) catch |err| exitOnOom(err);
            }
        }
    }

    if (aliases.len > 0) {
        for (aliases, 0..) |alias_level, level_index| {
            var level = scope.levels.levels.items[level_index];
            for (alias_level) |aliases_in_scope| {
                level.aliases.append(aliases_in_scope) catch |err| exitOnOom(err);
            }
        }
    }

    return scope;
}

// test "empty scope has no items" {
//     var scope = createTestScope(&.{}, &.{});
//     defer scope.env.deinit();
//     defer scope.deinit();

//     var ident_iter = scope.levels.iter(.ident);

//     try std.testing.expectEqual(null, ident_iter.next());
// }
