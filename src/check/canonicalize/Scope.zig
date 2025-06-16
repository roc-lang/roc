//! Scope management for identifier resolution during canonicalization.
//!
//! This module provides a hierarchical scope structure for tracking identifiers and aliases
//! during the canonicalization phase of compilation. It supports:
//! - Nested scopes with shadowing semantics
//! - Separate namespaces for identifiers and type aliases
//! - Lookups that search through nested scopes from innermost to outermost
//! - Error reporting for duplicate and missing identifiers
//!
//! The scope hierarchy works like a stack of levels, where each level represents a lexical
//! scope (e.g., function body, let-binding, pattern match). When looking up an identifier,
//! the search proceeds from the innermost scope outward until the identifier is found or
//! all scopes are exhausted.

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;
const Region = base.Region;
const Module = base.Module;
const Pattern = @import("CIR.zig").Pattern;
const exitOnOom = collections.utils.exitOnOom;

const Scope = @This();

/// Identifiers/aliases that are in scope, and defined in the current module.
levels: Levels,

/// Errors that can occur during scope operations
pub const Error = error{
    NotInScope,
    AlreadyInScope,
    ExitedTopScopeLevel,
};

/// Initialize a new scope.
pub fn init(gpa: std.mem.Allocator) Scope {
    var scope = Scope{ .levels = Levels{} };

    // ensure we have a top-level scope
    scope.levels.enter(gpa);

    return scope;
}

/// Deinitialize a scope's memory
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    Levels.deinit(gpa, &self.levels);
}

/// Level in the scope hierarchy
pub const Level = struct {
    /// Maps an Ident to a Pattern in the Can IR
    idents: std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx),
    aliases: std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx),

    /// Initialize the level
    pub fn init() Level {
        return Level{
            .idents = std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx).empty,
            .aliases = std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx).empty,
        };
    }

    /// Deinitialize the level
    pub fn deinit(self: *Level, gpa: std.mem.Allocator) void {
        if (self.idents.count() > 0) {
            self.idents.deinit(gpa);
        }
        if (self.aliases.count() > 0) {
            self.aliases.deinit(gpa);
        }
    }

    /// Item kinds in a level
    pub const ItemKind = enum { ident, alias };

    /// Get the appropriate map for the given item kind
    pub fn items(level: *Level, comptime item_kind: ItemKind) *std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx) {
        return switch (item_kind) {
            .ident => &level.idents,
            .alias => &level.aliases,
        };
    }

    /// Get the appropriate map for the given item kind (const version)
    pub fn itemsConst(level: *const Level, comptime item_kind: ItemKind) *const std.AutoHashMapUnmanaged(Ident.Idx, Pattern.Idx) {
        return switch (item_kind) {
            .ident => &level.idents,
            .alias => &level.aliases,
        };
    }

    /// Put an item in the level, panics on OOM
    pub fn put(level: *Level, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, pattern: Pattern.Idx) void {
        level.items(item_kind).put(gpa, name, pattern) catch |err| exitOnOom(err);
    }
};

/// Manages multiple levels of scope
pub const Levels = struct {
    levels: std.ArrayListUnmanaged(Level) = .{},

    /// Deinitialize all levels
    pub fn deinit(gpa: std.mem.Allocator, self: *Levels) void {
        for (0..self.levels.items.len) |i| {
            var level = &self.levels.items[i];
            level.deinit(gpa);
        }
        self.levels.deinit(gpa);
    }

    /// Enter a new scope level
    pub fn enter(self: *Levels, gpa: std.mem.Allocator) void {
        const level = Level.init();
        self.levels.append(gpa, level) catch |err| exitOnOom(err);
    }

    /// Exit the current scope level
    pub fn exit(self: *Levels, gpa: std.mem.Allocator) error{ExitedTopScopeLevel}!void {
        if (self.levels.items.len <= 1) {
            return Error.ExitedTopScopeLevel;
        }
        var level: Level = self.levels.pop().?;
        level.deinit(gpa);
    }

    /// Check if an identifier is in scope
    fn contains(
        self: *const Levels,
        ident_store: *const Ident.Store,
        comptime item_kind: Level.ItemKind,
        name: Ident.Idx,
    ) ?Pattern.Idx {
        var level_idx = self.levels.items.len;
        while (level_idx > 0) {
            level_idx -= 1;
            const level = &self.levels.items[level_idx];
            const map = level.itemsConst(item_kind);

            var iter = map.iterator();
            while (iter.next()) |entry| {
                if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
                    return entry.value_ptr.*;
                }
            }
        }
        return null;
    }

    /// Look up an identifier in the scope
    pub fn lookup(
        self: *const Levels,
        ident_store: *const Ident.Store,
        comptime item_kind: Level.ItemKind,
        name: Ident.Idx,
    ) ?Pattern.Idx {
        if (self.contains(ident_store, item_kind, name)) |pattern| {
            return pattern;
        }
        return null;
    }

    /// Introduce a new identifier to the current scope level
    pub fn introduce(
        self: *Levels,
        gpa: std.mem.Allocator,
        ident_store: *const Ident.Store,
        comptime item_kind: Level.ItemKind,
        ident_idx: Ident.Idx,
        pattern_idx: Pattern.Idx,
    ) error{AlreadyInScope}!void {
        // Only check the current level for duplicates to allow shadowing in nested scopes
        const current_level = &self.levels.items[self.levels.items.len - 1];
        const map = current_level.itemsConst(item_kind);

        var iter = map.iterator();
        while (iter.next()) |entry| {
            if (ident_store.identsHaveSameText(ident_idx, entry.key_ptr.*)) {
                return Error.AlreadyInScope;
            }
        }

        self.levels.items[self.levels.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
    }

    /// Get all identifiers in scope
    pub fn getAllIdentsInScope(self: *const Levels, gpa: std.mem.Allocator, comptime item_kind: Level.ItemKind) []Ident.Idx {
        var result = std.ArrayList(Ident.Idx).init(gpa);

        for (self.levels.items) |level| {
            const map = level.itemsConst(item_kind);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                result.append(entry.key_ptr.*) catch |err| exitOnOom(err);
            }
        }

        return result.toOwnedSlice() catch |err| exitOnOom(err);
    }
};

test "empty scope has no items" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const foo_ident = ident_store.insert(gpa, Ident.for_text("foo"), Region.zero());
    const result = scope.levels.lookup(&ident_store, .ident, foo_ident);

    try std.testing.expectEqual(null, result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const foo_ident = ident_store.insert(gpa, Ident.for_text("foo"), Region.zero());
    const bar_ident = ident_store.insert(gpa, Ident.for_text("bar"), Region.zero());
    const foo_pattern: Pattern.Idx = @enumFromInt(1);
    const bar_pattern: Pattern.Idx = @enumFromInt(2);

    // Add identifiers
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, foo_ident, foo_pattern);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, bar_ident, bar_pattern);

    // Lookup should find them
    const foo_result = scope.levels.lookup(&ident_store, .ident, foo_ident);
    const bar_result = scope.levels.lookup(&ident_store, .ident, bar_ident);

    try std.testing.expectEqual(foo_pattern, foo_result);
    try std.testing.expectEqual(bar_pattern, bar_result);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const x_ident = ident_store.insert(gpa, Ident.for_text("x"), Region.zero());
    const outer_pattern: Pattern.Idx = @enumFromInt(1);
    const inner_pattern: Pattern.Idx = @enumFromInt(2);

    // Add x to outer scope
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, x_ident, outer_pattern);

    // Enter new scope
    scope.levels.enter(gpa);

    // x from outer scope should still be visible
    const outer_result = scope.levels.lookup(&ident_store, .ident, x_ident);
    try std.testing.expectEqual(outer_pattern, outer_result);

    // Add x to inner scope (shadows outer)
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, x_ident, inner_pattern);

    // Now x should resolve to inner scope
    const inner_result = scope.levels.lookup(&ident_store, .ident, x_ident);
    try std.testing.expectEqual(inner_pattern, inner_result);

    // Exit inner scope
    try scope.levels.exit(gpa);

    // x should resolve to outer scope again
    const after_exit_result = scope.levels.lookup(&ident_store, .ident, x_ident);
    try std.testing.expectEqual(outer_pattern, after_exit_result);
}

test "cannot introduce duplicate identifier in same scope" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const x_ident = ident_store.insert(gpa, Ident.for_text("x"), Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // First introduction should succeed
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, x_ident, pattern1);

    // Second introduction should fail
    const result = scope.levels.introduce(gpa, &ident_store, .ident, x_ident, pattern2);
    try std.testing.expectError(Error.AlreadyInScope, result);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const foo = ident_store.insert(gpa, Ident.for_text("Foo"), Region.zero());
    const ident_pattern: Pattern.Idx = @enumFromInt(1);
    const alias_pattern: Pattern.Idx = @enumFromInt(2);

    // Add as both ident and alias (they're in separate namespaces)
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, foo, ident_pattern);
    _ = try scope.levels.introduce(gpa, &ident_store, .alias, foo, alias_pattern);

    // Both should be found in their respective namespaces
    const ident_result = scope.levels.lookup(&ident_store, .ident, foo);
    const alias_result = scope.levels.lookup(&ident_store, .alias, foo);

    try std.testing.expectEqual(ident_pattern, ident_result);
    try std.testing.expectEqual(alias_pattern, alias_result);
}

test "cannot exit top scope level" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    // Should fail to exit the only level
    const result = scope.levels.exit(gpa);
    try std.testing.expectError(Error.ExitedTopScopeLevel, result);
}

test "multiple nested scopes work correctly" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const a = ident_store.insert(gpa, Ident.for_text("a"), Region.zero());
    const b = ident_store.insert(gpa, Ident.for_text("b"), Region.zero());
    const c = ident_store.insert(gpa, Ident.for_text("c"), Region.zero());

    const pattern_a: Pattern.Idx = @enumFromInt(1);
    const pattern_b1: Pattern.Idx = @enumFromInt(2);
    const pattern_b2: Pattern.Idx = @enumFromInt(3);
    const pattern_c: Pattern.Idx = @enumFromInt(4);

    // Level 1: add a
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, a, pattern_a);

    // Enter level 2: add b
    scope.levels.enter(gpa);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, b, pattern_b1);

    // Enter level 3: shadow b, add c
    scope.levels.enter(gpa);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, b, pattern_b2);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, c, pattern_c);

    // Check all are visible with correct values
    try std.testing.expectEqual(pattern_a, scope.levels.lookup(&ident_store, .ident, a));
    try std.testing.expectEqual(pattern_b2, scope.levels.lookup(&ident_store, .ident, b));
    try std.testing.expectEqual(pattern_c, scope.levels.lookup(&ident_store, .ident, c));

    // Exit level 3
    try scope.levels.exit(gpa);

    // c should be gone, b should be from level 2
    try std.testing.expectEqual(pattern_a, scope.levels.lookup(&ident_store, .ident, a));
    try std.testing.expectEqual(pattern_b1, scope.levels.lookup(&ident_store, .ident, b));
    try std.testing.expectEqual(null, scope.levels.lookup(&ident_store, .ident, c));

    // Exit level 2
    try scope.levels.exit(gpa);

    // Only a should remain
    try std.testing.expectEqual(pattern_a, scope.levels.lookup(&ident_store, .ident, a));
    try std.testing.expectEqual(null, scope.levels.lookup(&ident_store, .ident, b));
    try std.testing.expectEqual(null, scope.levels.lookup(&ident_store, .ident, c));
}

test "getAllIdentsInScope returns all identifiers" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const a = ident_store.insert(gpa, Ident.for_text("a"), Region.zero());
    const b = ident_store.insert(gpa, Ident.for_text("b"), Region.zero());
    const c = ident_store.insert(gpa, Ident.for_text("c"), Region.zero());

    _ = try scope.levels.introduce(gpa, &ident_store, .ident, a, @enumFromInt(1));
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, b, @enumFromInt(2));

    // Get all idents in scope
    const all_idents_1 = scope.levels.getAllIdentsInScope(gpa, .ident);
    defer gpa.free(all_idents_1);

    // Should only have 2 identifiers
    try std.testing.expectEqual(@as(usize, 2), all_idents_1.len);

    scope.levels.enter(gpa);

    _ = try scope.levels.introduce(gpa, &ident_store, .ident, c, @enumFromInt(3));

    // Get all idents in scope
    const all_idents_2 = scope.levels.getAllIdentsInScope(gpa, .ident);
    defer gpa.free(all_idents_2);

    // Should have all 3 identifiers
    try std.testing.expectEqual(@as(usize, 3), all_idents_2.len);

    // Also test for aliases (should be empty)
    const all_aliases = scope.levels.getAllIdentsInScope(gpa, .alias);
    defer gpa.free(all_aliases);

    try std.testing.expectEqual(@as(usize, 0), all_aliases.len);
}

test "identifiers with same text are treated as duplicates" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    // Create two different Ident.Idx with the same text
    const foo1 = ident_store.insert(gpa, Ident.for_text("foo"), Region.zero());
    const foo2 = ident_store.insert(gpa, Ident.for_text("foo"), Region.zero());

    // They should have different indices
    try std.testing.expect(foo1 != foo2);

    const pattern_1_idx: Pattern.Idx = @enumFromInt(1);
    const pattern_2_idx: Pattern.Idx = @enumFromInt(1);

    // Add the first one
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, foo1, pattern_1_idx);

    // Adding the second should fail because it has the same text
    const result = scope.levels.introduce(gpa, &ident_store, .ident, foo2, pattern_2_idx);
    try std.testing.expectError(Error.AlreadyInScope, result);

    // But looking up either should find the first one
    const lookup1 = scope.levels.lookup(&ident_store, .ident, foo1);
    const lookup2 = scope.levels.lookup(&ident_store, .ident, foo2);

    try std.testing.expectEqual(pattern_1_idx, lookup1);
    try std.testing.expectEqual(pattern_1_idx, lookup2);
}

test "cannot introduce duplicate alias in same scope" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const list_alias = ident_store.insert(gpa, Ident.for_text("List"), Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // First introduction should succeed
    _ = try scope.levels.introduce(gpa, &ident_store, .alias, list_alias, pattern1);

    // Second introduction should fail
    const result = scope.levels.introduce(gpa, &ident_store, .alias, list_alias, pattern2);
    try std.testing.expectError(Error.AlreadyInScope, result);
}

test "shadowing works correctly for aliases" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const my_type = ident_store.insert(gpa, Ident.for_text("MyType"), Region.zero());
    const outer_pattern: Pattern.Idx = @enumFromInt(1);
    const inner_pattern: Pattern.Idx = @enumFromInt(2);

    // Add alias to outer scope
    _ = try scope.levels.introduce(gpa, &ident_store, .alias, my_type, outer_pattern);

    // Enter new scope and shadow the alias
    scope.levels.enter(gpa);
    _ = try scope.levels.introduce(gpa, &ident_store, .alias, my_type, inner_pattern);

    // Should resolve to inner scope
    const inner_result = scope.levels.lookup(&ident_store, .alias, my_type);
    try std.testing.expectEqual(inner_pattern, inner_result);

    // Exit inner scope
    try scope.levels.exit(gpa);

    // Should resolve to outer scope again
    const outer_result = scope.levels.lookup(&ident_store, .alias, my_type);
    try std.testing.expectEqual(outer_pattern, outer_result);
}

test "deeply nested scopes maintain proper visibility" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa);
    defer scope.deinit(gpa);

    const x = ident_store.insert(gpa, Ident.for_text("x"), Region.zero());
    const patterns = [_]Pattern.Idx{
        @enumFromInt(1),
        @enumFromInt(2),
        @enumFromInt(3),
        @enumFromInt(4),
        @enumFromInt(5),
    };

    // Create 5 nested scopes, each shadowing x
    for (patterns) |pattern| {
        scope.levels.enter(gpa);
        _ = try scope.levels.introduce(gpa, &ident_store, .ident, x, pattern);

        // Verify it resolves to the current pattern
        const result = scope.levels.lookup(&ident_store, .ident, x);
        try std.testing.expectEqual(pattern, result);
    }

    // Exit all scopes and verify x resolves correctly at each level
    var i: usize = patterns.len;
    while (i > 1) : (i -= 1) {
        try scope.levels.exit(gpa);
        const expected = patterns[i - 2];
        const result = scope.levels.lookup(&ident_store, .ident, x);
        try std.testing.expectEqual(expected, result);
    }

    // Exit the last scope - x should not be found
    try scope.levels.exit(gpa);
    const final_result = scope.levels.lookup(&ident_store, .ident, x);
    try std.testing.expectEqual(null, final_result);
}
