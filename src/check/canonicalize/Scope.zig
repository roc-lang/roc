const std = @import("std");
const base = @import("../../base.zig");
const problem_mod = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Alias = @import("./Alias.zig");
const CanId = @import("./CanId.zig");

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

/// Bucket sizes: 1B, 2B, 3B, 4B
const max_small_str_len: usize = 4;
const num_small_string_buckets: usize = max_small_str_len;

/// Small string buckets need to be 128-bit aligned because we do 128-bit SIMD
/// operations on them.
pub const BucketEntry = u128;

const initial_bucket_capacity = 16; // Starting capacity for each bucket

/// Top-level scope has different rules and tradeoffs compared to nested scopes.
/// - Top-level scope does not allow `var` declarations.
/// - For top-level scope, all declarations are added, and then sorted, before any lookups happen.
/// - Duplicates are detected during sorting instead of at declaration time.
/// - Binary search can be used for lookups, because lookups only happen after sorting has finished.
///
/// Note that this scope does not distinguish between modules, types, tags, and lowercase identifiers.
/// Callers should make separate scopes for each of these, because this will shrink the search space
/// for each of them.
pub const TopLevelScope = struct {
    /// Each bucket holds small strings of one particular sizes. They each have
    /// different sizes (e.g. 2B small strings vs 3B small strings), but we use
    /// 128-bit SIMD operations on all of them, so they all need 128-bit alignment.
    small_str_buckets: [num_small_string_buckets](*BucketEntry),
    bucket_lens: [num_small_string_buckets]u32,
    bucket_capacities: [num_small_string_buckets]u32,
    /// The CanId of the declaration node which caused this to be added to scope.
    bucket_can_ids: [num_small_string_buckets](*CanId),

    large_strings: std.AutoHashMap(InternedStrId, CanId),

    /// This is simpler than ScopeStack's `assign` because:
    /// - It does not support `var`
    /// - It does not check for duplicate declarations (that is, shadowing). That's done later, during sorting.
    pub fn add(self: *TopLevelScope, allocator: *Allocator, str: []u8, can_id: CanId) void {
        // TODO add
    }

    /// Sort each bucket by string name, allowing future lookups to use binary searches.
    /// Appends duplicates to the given ArrayList, which the caller can then use to report shadowing.
    /// This should only ever be called once, after all entries have been added. No entries should
    /// be added after sort() has been called. (Also, lookup() should only be called after sort() has run.)
    pub fn sort(self: *TopLevelScope, allocator: *Allocator, duplicates: *ArrayListUnmanaged(CanId)) Allocator.Error!void {
        // TODO do a manual sort (mergesort? quicksort? timsort?) where we also rearrange the can_ids at the same time,
        // and also whenever there's a tie, we report a duplicate and drop whichever CanId is lower. (This has the effect
        // that the one which is furthest down in the file "wins" - essentially, shadowing.) To preserve the maximally-fast
        // sorting algorithm (e.g. not having to compact things), we don't actually *remove* the duplicate entry; rather,
        // we set its CanId to be equal to the other's. In this way, all lookups will resolve to the other CanId, so it's
        // as if the other one had been removed, but we don't have to slow down sorting for this error case.
    }

    /// This must only be called after sort() has been called, because it does a binary search
    /// which relies on the assumption that everything has been sorted.
    pub fn lookup(self: *TopLevelScope, name: []const u8) ?CanId {
        // TODO if it's a small string, do a binary search of the buckets.
        // TODO if it's a large string, do a hash lookup.
    }

    pub fn init(allocator: Allocator) Allocator.Error!TopLevelScope {
        var small_str_buckets: [num_small_string_buckets](*BucketEntry) = undefined;
        var small_str_flags: [num_small_string_buckets](*ConstOrVar) = undefined;
        var small_str_capacities: [num_small_string_buckets]u32 = undefined;
        var can_ids: [num_small_string_buckets](*CanId) = undefined;

        // Small string buckets
        for (0..num_small_string_buckets) |i| {
            small_str_buckets[i] = try allocator.alignedAlloc(BucketEntry, @alignOf(BucketEntry), initial_bucket_capacity);
            small_str_capacities[i] = @intCast(initial_bucket_capacity);
            can_ids[i] = try allocator.alloc(CanId, initial_bucket_capacity);
        }

        // Large string bucket
        can_ids[num_small_string_buckets] = try allocator.alloc(CanId, 16);
        var large_str_buckets = std.ArrayListUnmanaged(std.AutoHashMap([]u8, ConstOrVar)){};
        try large_str_buckets.append(allocator, std.AutoHashMap([]u8, ConstOrVar).init(allocator));

        return TopLevelScope{
            .small_str_buckets = small_str_buckets,
            .small_str_flags = small_str_flags,
            .bucket_capacities = small_str_capacities,
            .can_ids = can_ids,
            .large_str_buckets = large_str_buckets,
        };
    }

    pub fn deinit(self: *TopLevelScope, allocator: std.mem.Allocator) void {
        for (0..num_small_string_buckets) |i| {
            allocator.free(self.small_str_buckets[i]);
            allocator.free(self.small_str_flags[i]);
            allocator.free(self.can_ids[i]);
        }

        allocator.free(self.can_ids[num_small_string_buckets]);

        for (self.large_str_buckets.items) |*hashmap| {
            hashmap.deinit();
        }
        self.large_str_buckets.deinit(allocator);
    }
};

/// A stack of nested scopes. This should be used for all non-top-level scopes, such as inside functions
/// or top-level constant blocks.
///
/// Note that this scope does not distinguish between modules, types, tags, and lowercase identifiers.
/// Callers should make separate scopes for each of these, because this will shrink the search space
/// for each of them.
pub const ScopeStack = struct {
    /// For each level in the stack, we track the lengths of each bucket.
    levels: std.ArrayListUnmanaged([num_small_string_buckets]u32),

    /// Each bucket holds small strings of one particular sizes.
    /// They each have different sizes (e.g. 4B small strings vs 8B small strings),
    /// but we use 128-bit SIMD operations on all of them, so they all need 128-bit
    /// alignment. It's num_small_string_buckets because large strings are stored separately.
    small_str_buckets: [num_small_string_buckets](*BucketEntry),

    /// The flag is just a bit for whether it's a var or a const.
    small_str_flags: [num_small_string_buckets](*ConstOrVar),

    /// The capacities for each of these allocations. (The levels determine the lengths.)
    small_str_capacities: [num_small_string_buckets]u32,

    /// The CanId of the declaration node which caused this to be added to scope.
    can_ids: [num_small_string_buckets](*CanId),

    large_str_buckets: std.ArrayListUnmanaged(std.AutoHashMap([]u8, ConstOrVar)),

    fn init(allocator: *Allocator) Self {
        const levels = std.ArrayListUnmanaged.initCapacity(allocator, 8);

        // We should *always* have at least 1 level, for the top-level scope.
        levels.append(allocator, [_]u32{0} ** num_small_string_buckets) catch unreachable;

        return Self{
            levels,
        };
    }

    fn deinit(self: *Self, allocator: *Allocator) void {
        // TODO
    }

    /// Enter a new scope. For every `enter` call, there must be a corresponding `exit` call.
    fn enter(self: *Self, allocator: *Allocator) Allocator.Error!void {
        std.debug.assert(self.levels.items.len > 0); // Scope stack should never be empty
        const current_level = self.levels.items[self.levels.items.len - 1];
        try self.levels.append(allocator, [_]u32{0} ** num_small_string_buckets);
    }

    /// Exit the current scope. This must never be called more times than `enter` has called,
    /// as doing so would eliminate the top-level scope. That would result in UB.
    fn exit(self: *Self) void {
        self.levels.pop();
        std.debug.assert(self.levels.items.len > 0); // Scope stack should never be empty
    }

    /// Assign the given name to the given value.
    /// const_or_var is Var if there was a `var` keyword preceding this `=`,
    /// and Const otherwise.
    fn assign(self: *ScopeStack, allocator: *Allocator, str: []u8, can_id: CanId, const_or_var: ConstOrVar) Assignment {
        const len = str.len;
        const bucket_index = undefined;
        const bucket_len = undefined;
        const index_in_bucket = undefined;
        const answer = undefined;

        if (len <= max_small_str_len) {
            bucket_index = bucket_index_from_len(len);
            bucket_len = &self.levels.getLast()[bucket_index];
            index_in_bucket = *bucket_len;

            switch (bucket_index) {
                0 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    self.ensure_bucket_capacity(allocator, bucket_index);
                    // TODO copy the u8 over
                },
                1 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    self.ensure_bucket_capacity(allocator, bucket_index);
                    // TODO make a u16, copy it in
                },
                2 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    self.ensure_bucket_capacity(allocator, bucket_index);
                    // TODO make a u32 (zero-padded), copy it in
                },
                3 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    self.ensure_bucket_capacity(allocator, bucket_index);
                    // TODO make a u32, copy it in
                },
                4 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    self.ensure_bucket_capacity(allocator, bucket_index);
                    // TODO make a u64 (zero-padded), copy it in
                },
                5 => {
                    // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
                    self.ensure_bucket_capacity(allocator, bucket_index);
                    // TODO make a u128 (zero-padded), copy it in
                },
            }

            small_str_flags[bucket_index][index_in_bucket] = const_or_var;
        } else {
            bucket_index = num_small_string_buckets;
            bucket_len = &self.levels.getLast()[bucket_index];
            index_in_bucket = *bucket_len;
            // TODO check if shadowing (it's ok if this is not a var and the old decl was a var)
            // It's big! Bump capacity (don't use ensure_bucket_capacity; this works differently),
            // then put it into the hashmap at the current level.
        }

        // Capacity should already have been bumped by this point if needed,
        // so we can set these without checking.
        *bucket_len = index_in_bucket + 1;
        self.can_ids[bucket_index] = can_id;

        return answer;
    }

    /// This must only be called after sort() has been called, because it does a binary search
    /// which relies on the assumption that everything has been sorted.
    pub fn lookup(self: *const ScopeStack, name: []const u8, top_level: *const TopLevelScope) ?CanId {
        // TODO if it's a small string, do a *reverse* search of the buckets, and then a top level scope lookup
        // TODO if it's a large string, do a hash lookup, and then a top level scope lookup if that fails.
    }

    /// Ensure that the given bucket has at least this much capacity available.
    /// If it does not, make a new allocation (grow capacity by 1.5x), copy
    /// over the existing data, and deinit the existing allocation.
    fn ensure_bucket_capacity(self: *ScopeStack, allocator: *Allocator, capacity: usize) void {
        const bucket_capacity = self.bucket_capacity;
        const index_in_bucket = self.index_in_bucket;

        if (index_in_bucket >= bucket_capacity) {
            // TODO alloc new, copy everything over, dealloc old, set new bucket_len
        }
    }
};

pub const Assignment = union(enum) {
    /// This `=` is successfully declaring a new constant.
    NewConst: void,
    /// This `=` is successfully reassigning an existing `var` declaration,
    /// and the id is the id of that var declaration.
    VarReassign: CanId,
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

fn bucket_index_from_len(len: usize) usize {
    // 1-4 => 0-3
    const small_bucket_index = len - 1;

    // 5-8 => 4
    // 9-16 => 5
    const big_bucket_index = (len << 3) + 3;

    // Branchlessly get the bucket index.
    return if (len <= 4) small_bucket_index else big_bucket_index;
}

// test "empty scope has no items" {
//     var scope = createTestScope(&.{}, &.{});
//     defer scope.env.deinit();
//     defer scope.deinit();

//     var ident_iter = scope.levels.iter(.ident);

//     try std.testing.expectEqual(null, ident_iter.next());
// }
