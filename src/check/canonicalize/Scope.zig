//! Scope management for identifier resolution during canonicalization.
//!
//! This module provides a hierarchical scope structure for tracking identifiers and aliases
//! during the canonicalization phase of compilation. It supports:
//! - Nested scopes with proper shadowing semantics
//! - Separate namespaces for identifiers and type aliases
//! - Efficient O(1) lookups using hash maps
//! - Error reporting for duplicate and missing identifiers
//!
//! The scope hierarchy works like a stack of levels, where each level represents a lexical
//! scope (e.g., function body, let-binding, pattern match). When looking up an identifier,
//! the search proceeds from the innermost scope outward until the identifier is found or
//! all scopes are exhausted.

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

const Alias = @import("./Alias.zig");

const Ident = base.Ident;
const Region = base.Region;
const Module = base.Module;
const Pattern = @import("CIR.zig").Pattern;
const exitOnOom = collections.utils.exitOnOom;

const Scope = @This();

const TagMap = std.AutoHashMapUnmanaged(Ident.Idx, Alias.Idx);

/// The custom alias that this file is centered around, if one has been defined.
focused_custom_alias: ?Alias.Idx = null,
// TODO: handle renaming, e.g. `CustomType := [ExportedName as LocalName]`
custom_tags: TagMap = TagMap.empty,
/// Identifiers/aliases that are in scope, and defined in the current module.
levels: Levels,

/// Errors that can occur during scope operations
pub const Error = error{
    IdentNotInScope,
    IdentAlreadyInScope,
    AliasNotInScope,
    AliasAlreadyInScope,
    ExitedTopScopeLevel,
};

/// Error details for when an identifier is not in scope
pub const IdentNotInScopeError = struct {
    ident: Ident.Idx,
    suggestions: []const Ident.Idx,
};

/// Error details for when an identifier is already in scope
pub const IdentAlreadyInScopeError = struct {
    original_ident: Ident.Idx,
    shadow: Ident.Idx,
};

/// Error details for when an alias is not in scope
pub const AliasNotInScopeError = struct {
    name: Ident.Idx,
    suggestions: []const Ident.Idx,
};

/// Error details for when an alias is already in scope
pub const AliasAlreadyInScopeError = struct {
    original_name: Ident.Idx,
    shadow: Ident.Idx,
};

/// Initialize a new scope.
pub fn init(
    gpa: std.mem.Allocator,
    ident_store: *const Ident.Store,
    builtin_aliases: []const struct { alias: Ident.Idx, name: Ident.Idx },
    builtin_idents: []const Ident.Idx,
) Scope {
    var scope = Scope{
        .levels = Levels{},
    };

    scope.levels.enter(gpa);

    for (builtin_idents) |builtin_ident| {
        _ = scope.levels.introduce(gpa, ident_store, .ident, builtin_ident, @enumFromInt(0)) catch {};
    }

    for (builtin_aliases) |builtin_alias| {
        _ = scope.levels.introduce(gpa, ident_store, .alias, builtin_alias.name, @enumFromInt(0)) catch {};
    }

    return scope;
}

/// Deinitialize a scope's memory
pub fn deinit(self: *Scope, gpa: std.mem.Allocator) void {
    if (self.custom_tags.count() > 0) {
        self.custom_tags.deinit(gpa);
    }
    Levels.deinit(gpa, &self.levels);
}

/// Generates a unique ident like "1" or "5" in the home module.
///
/// This is used, for example, during canonicalization of an Expr::Closure
/// to generate a unique ident to refer to that closure.
pub fn genUnique(self: *Scope, gpa: std.mem.Allocator, ident_store: *Ident.Store, pattern: Pattern.Idx) !Ident.Idx {
    const unique_idx = ident_store.genUnique();

    try self.levels.introduce(gpa, ident_store, .ident, unique_idx, pattern);

    return unique_idx;
}

/// Result of a lookup operation
pub const LookupResult = union(enum) {
    Found: Pattern.Idx,
    NotFound: void,
};

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

    /// Put an item in the level
    pub fn put(level: *Level, gpa: std.mem.Allocator, comptime item_kind: ItemKind, name: Ident.Idx, pattern: Pattern.Idx) !void {
        try level.items(item_kind).put(gpa, name, pattern);
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
    pub fn exit(self: *Levels, gpa: std.mem.Allocator) Error!void {
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
    ) LookupResult {
        if (self.contains(ident_store, item_kind, name)) |pattern| {
            return .{ .Found = pattern };
        }
        return .NotFound;
    }

    /// Introduce a new identifier to the current scope level
    pub fn introduce(
        self: *Levels,
        gpa: std.mem.Allocator,
        ident_store: *const Ident.Store,
        comptime item_kind: Level.ItemKind,
        name: Ident.Idx,
        pattern: Pattern.Idx,
    ) !Pattern.Idx {
        if (self.contains(ident_store, item_kind, name)) |existing_pattern| {
            _ = existing_pattern;
            return switch (item_kind) {
                .ident => Error.IdentAlreadyInScope,
                .alias => Error.AliasAlreadyInScope,
            };
        }

        self.levels.items[self.levels.items.len - 1].put(gpa, item_kind, name, pattern) catch |err| exitOnOom(err);
        return pattern;
    }

    /// Get all identifiers in scope
    pub fn getAllIdentsInScope(self: *const Levels, gpa: std.mem.Allocator, comptime item_kind: Level.ItemKind) ![]Ident.Idx {
        var result = std.ArrayList(Ident.Idx).init(gpa);

        for (self.levels.items) |level| {
            const map = level.itemsConst(item_kind);
            var iter = map.iterator();
            while (iter.next()) |entry| {
                try result.append(entry.key_ptr.*);
            }
        }

        return result.toOwnedSlice();
    }
};

// Tests

test "empty scope has no items" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
    defer scope.deinit(gpa);

    const foo_ident = ident_store.insert(gpa, Ident.for_text("foo"), Region.zero());
    const result = scope.levels.lookup(&ident_store, .ident, foo_ident);

    try std.testing.expectEqual(LookupResult.NotFound, result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
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

    try std.testing.expectEqual(LookupResult{ .Found = foo_pattern }, foo_result);
    try std.testing.expectEqual(LookupResult{ .Found = bar_pattern }, bar_result);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
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
    try std.testing.expectEqual(LookupResult{ .Found = outer_pattern }, outer_result);

    // Add x to inner scope (shadows outer)
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, x_ident, inner_pattern);

    // Now x should resolve to inner scope
    const inner_result = scope.levels.lookup(&ident_store, .ident, x_ident);
    try std.testing.expectEqual(LookupResult{ .Found = inner_pattern }, inner_result);

    // Exit inner scope
    try scope.levels.exit(gpa);

    // x should resolve to outer scope again
    const after_exit_result = scope.levels.lookup(&ident_store, .ident, x_ident);
    try std.testing.expectEqual(LookupResult{ .Found = outer_pattern }, after_exit_result);
}

test "cannot introduce duplicate identifier in same scope" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
    defer scope.deinit(gpa);

    const x_ident = ident_store.insert(gpa, Ident.for_text("x"), Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // First introduction should succeed
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, x_ident, pattern1);

    // Second introduction should fail
    const result = scope.levels.introduce(gpa, &ident_store, .ident, x_ident, pattern2);
    try std.testing.expectError(Error.IdentAlreadyInScope, result);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
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

    try std.testing.expectEqual(LookupResult{ .Found = ident_pattern }, ident_result);
    try std.testing.expectEqual(LookupResult{ .Found = alias_pattern }, alias_result);
}

test "cannot exit top scope level" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
    defer scope.deinit(gpa);

    // Should fail to exit the only level
    const result = scope.levels.exit(gpa);
    try std.testing.expectError(Error.ExitedTopScopeLevel, result);
}

test "multiple nested scopes work correctly" {
    const gpa = std.testing.allocator;
    var ident_store = Ident.Store.initCapacity(gpa, 100);
    defer ident_store.deinit(gpa);

    var scope = init(gpa, &ident_store, &.{}, &.{});
    defer scope.deinit(gpa);

    const a = ident_store.insert(gpa, Ident.for_text("a"), Region.zero());
    const b = ident_store.insert(gpa, Ident.for_text("b"), Region.zero());
    const c = ident_store.insert(gpa, Ident.for_text("c"), Region.zero());

    const pattern_a1: Pattern.Idx = @enumFromInt(1);
    const pattern_b1: Pattern.Idx = @enumFromInt(2);
    const pattern_b2: Pattern.Idx = @enumFromInt(3);
    const pattern_c: Pattern.Idx = @enumFromInt(4);

    // Level 1: add a
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, a, pattern_a1);

    // Enter level 2: add b
    scope.levels.enter(gpa);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, b, pattern_b1);

    // Enter level 3: shadow b, add c
    scope.levels.enter(gpa);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, b, pattern_b2);
    _ = try scope.levels.introduce(gpa, &ident_store, .ident, c, pattern_c);

    // Check all are visible with correct values
    try std.testing.expectEqual(LookupResult{ .Found = pattern_a1 }, scope.levels.lookup(&ident_store, .ident, a));
    try std.testing.expectEqual(LookupResult{ .Found = pattern_b2 }, scope.levels.lookup(&ident_store, .ident, b));
    try std.testing.expectEqual(LookupResult{ .Found = pattern_c }, scope.levels.lookup(&ident_store, .ident, c));

    // Exit level 3
    try scope.levels.exit(gpa);

    // c should be gone, b should be from level 2
    try std.testing.expectEqual(LookupResult{ .Found = pattern_a1 }, scope.levels.lookup(&ident_store, .ident, a));
    try std.testing.expectEqual(LookupResult{ .Found = pattern_b1 }, scope.levels.lookup(&ident_store, .ident, b));
    try std.testing.expectEqual(LookupResult.NotFound, scope.levels.lookup(&ident_store, .ident, c));

    // Exit level 2
    try scope.levels.exit(gpa);

    // Only a should remain
    try std.testing.expectEqual(LookupResult{ .Found = pattern_a1 }, scope.levels.lookup(&ident_store, .ident, a));
    try std.testing.expectEqual(LookupResult.NotFound, scope.levels.lookup(&ident_store, .ident, b));
    try std.testing.expectEqual(LookupResult.NotFound, scope.levels.lookup(&ident_store, .ident, c));
}
