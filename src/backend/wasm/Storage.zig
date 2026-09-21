//! Tracks where LIR locals live in wasm locals.
//!
//! The active statement-only LIR path uses compact `LocalId`s everywhere.
//! Wasm codegen therefore binds executable values by local id, not by symbol.
//!
//! Bindings live in one store-indexed column that every function scope reuses.
//! Each row carries the scope stamp that wrote it, so a row is live exactly for
//! the scope that bound it. Entering a scope allocates no table and clears
//! nothing, and every write records the row it replaced, so leaving a scope
//! undoes exactly the rows that scope bound: both costs are proportional to the
//! locals that scope bound.
//!
//! The column itself is grown by the first bind that reaches past its current
//! length, which initializes every row up to that `LocalId`. That is a one-time
//! cost per row over the whole module rather than a per-scope one: a procedure
//! that first reaches a high `LocalId` pays for the rows below it too, and no
//! later procedure pays for them again. Total initialization over a module is
//! therefore the highest `LocalId` any procedure binds, and the per-procedure
//! work on top of that is the locals that procedure emits.
//!
//! Nested helper compilation (RC helpers, capture-drop helpers, dictionary
//! thunks, host adapters) takes a fresh stamp, so a helper that binds the same
//! `LocalId` as its caller neither sees the caller's binding nor destroys it.
//! Specialized procedures likewise share `LocalId`s without sharing wasm slots.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const lir = @import("lir");
const LocalId = lir.LIR.LocalId;
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

const Self = @This();

/// Info about a wasm local variable: its index and value type.
pub const LocalInfo = struct {
    idx: u32,
    val_type: ValType,
};

/// One row of the store-indexed binding column.
const Binding = struct {
    /// The scope that wrote `info`. A row is live exactly when this equals the
    /// active scope; `unbound_scope` is issued to no scope, so a freshly
    /// initialized row is unbound in every scope.
    scope: u64,
    info: LocalInfo,
};

/// Stamp of a row that no scope has written.
const unbound_scope: u64 = 0;

/// The outer scope a nested function scope must restore on exit.
pub const Scope = struct {
    scope: u64,
    types_base: u32,
    shadow_base: u32,
};

/// A binding row as it stood before the active scope overwrote it.
const ShadowedBinding = struct {
    row_index: u32,
    row: Binding,
};

/// LIR local → wasm local binding, indexed directly by the `LocalId` ordinal.
bindings: std.ArrayList(Binding),
/// Local types for every live scope. The active scope owns `types_base..`.
local_types: std.ArrayList(ValType),
/// Where the active scope's locals begin in `local_types`.
types_base: u32,
/// Replaced rows, innermost last. `endScope` unwinds this back to the scope's
/// base, which restores every row the scope bound to what it displaced.
shadow: std.ArrayList(ShadowedBinding),
/// Stamp that makes a binding row live.
active_scope: u64,
/// Highest stamp issued so far. Stamps are never reused, so a stale row from
/// an earlier scope can never be mistaken for a live one.
next_scope: u64,
allocator: Allocator,
/// Deterministic regression metric for binding work: every row read, written,
/// or initialized. Allocation accounting alone cannot detect a consumer that
/// rescans the whole column once per procedure.
rows_visited: if (builtin.is_test) usize else void,

pub fn init(allocator: Allocator) Self {
    return .{
        .bindings = .empty,
        .local_types = .empty,
        .types_base = 0,
        .shadow = .empty,
        .active_scope = unbound_scope + 1,
        .next_scope = unbound_scope + 1,
        .allocator = allocator,
        .rows_visited = if (builtin.is_test) 0 else {},
    };
}

pub fn deinit(self: *Self) void {
    self.bindings.deinit(self.allocator);
    self.local_types.deinit(self.allocator);
    self.shadow.deinit(self.allocator);
}

/// Begin a function scope, returning the outer scope for `endScope`.
pub fn beginScope(self: *Self) Scope {
    const outer: Scope = .{
        .scope = self.active_scope,
        .types_base = self.types_base,
        .shadow_base = @intCast(self.shadow.items.len),
    };
    self.next_scope += 1;
    self.active_scope = self.next_scope;
    self.types_base = @intCast(self.local_types.items.len);
    return outer;
}

/// End the active function scope and restore the outer one.
pub fn endScope(self: *Self, outer: Scope) void {
    while (self.shadow.items.len > outer.shadow_base) {
        const shadowed = self.shadow.pop().?;
        if (builtin.is_test) self.rows_visited += 1;
        self.bindings.items[shadowed.row_index] = shadowed.row;
    }
    self.local_types.shrinkRetainingCapacity(self.types_base);
    self.types_base = outer.types_base;
    self.active_scope = outer.scope;
}

/// Allocate a new wasm local for the given LIR local id.
pub fn allocLocal(self: *Self, local_id: LocalId, val_type: ValType) Allocator.Error!u32 {
    const row_index = try self.reserveBindingRow(local_id);
    if (builtin.is_test) self.rows_visited += 1;
    try self.shadow.append(self.allocator, .{
        .row_index = @intCast(row_index),
        .row = self.bindings.items[row_index],
    });
    const idx = try self.allocAnonymousLocal(val_type);
    self.bindings.items[row_index] = .{
        .scope = self.active_scope,
        .info = .{ .idx = idx, .val_type = val_type },
    };
    return idx;
}

/// Allocate an anonymous local (not bound to any LIR local).
pub fn allocAnonymousLocal(self: *Self, val_type: ValType) Allocator.Error!u32 {
    const idx = self.nextLocalIdx();
    try self.local_types.append(self.allocator, val_type);
    return idx;
}

/// Look up the wasm-local binding a previous emission site made in this scope.
pub fn getLocalInfo(self: *Self, local_id: LocalId) ?LocalInfo {
    const row_index: usize = @intFromEnum(local_id);
    if (row_index >= self.bindings.items.len) return null;
    if (builtin.is_test) self.rows_visited += 1;
    const row = self.bindings.items[row_index];
    if (row.scope != self.active_scope) return null;
    return row.info;
}

/// Index the next local allocated in this scope will receive.
pub fn nextLocalIdx(self: *const Self) u32 {
    return @intCast(self.local_types.items.len - self.types_base);
}

/// The active scope's local types, in allocation order.
pub fn currentTypes(self: *const Self) []const ValType {
    return self.local_types.items[self.types_base..];
}

/// Grow the column so `local_id` addresses a row, initializing only new rows.
fn reserveBindingRow(self: *Self, local_id: LocalId) Allocator.Error!usize {
    const row_index: usize = @intFromEnum(local_id);
    const old_len = self.bindings.items.len;
    if (row_index >= old_len) {
        try self.bindings.resize(self.allocator, row_index + 1);
        for (self.bindings.items[old_len..]) |*row| {
            if (builtin.is_test) self.rows_visited += 1;
            row.* = .{ .scope = unbound_scope, .info = .{ .idx = 0, .val_type = .i32 } };
        }
    }
    return row_index;
}

test "a nested scope neither sees nor leaks wasm local bindings" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const shared: LocalId = @enumFromInt(7);
    const outer_idx = try storage.allocLocal(shared, .i64);
    try std.testing.expectEqual(@as(u32, 0), outer_idx);
    try std.testing.expectEqual(LocalInfo{ .idx = outer_idx, .val_type = .i64 }, storage.getLocalInfo(shared).?);

    const outer = storage.beginScope();
    try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(shared));
    const inner_idx = try storage.allocLocal(shared, .i32);
    try std.testing.expectEqual(@as(u32, 0), inner_idx);
    try std.testing.expectEqual(@as(usize, 1), storage.currentTypes().len);
    storage.endScope(outer);

    try std.testing.expectEqual(LocalInfo{ .idx = outer_idx, .val_type = .i64 }, storage.getLocalInfo(shared).?);
    try std.testing.expectEqual(@as(usize, 1), storage.currentTypes().len);
    try std.testing.expectEqual(ValType.i64, storage.currentTypes()[0]);
}

test "entering and leaving a wasm local scope visits no binding rows" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    for (0..64) |i| {
        _ = try storage.allocLocal(@enumFromInt(@as(u32, @intCast(i))), .i32);
    }

    const before = storage.rows_visited;
    for (0..1000) |_| {
        const outer = storage.beginScope();
        storage.endScope(outer);
    }
    try std.testing.expectEqual(before, storage.rows_visited);
}

test "a wasm local scope reuses local indices without clearing the column" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const first = storage.beginScope();
    _ = try storage.allocLocal(@enumFromInt(3), .i32);
    _ = try storage.allocAnonymousLocal(.i64);
    try std.testing.expectEqual(@as(u32, 2), storage.nextLocalIdx());
    storage.endScope(first);

    const second = storage.beginScope();
    try std.testing.expectEqual(@as(u32, 0), storage.nextLocalIdx());
    try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(@enumFromInt(3)));
    const rows_before = storage.rows_visited;
    const reused = try storage.allocLocal(@enumFromInt(3), .f64);
    try std.testing.expectEqual(@as(u32, 0), reused);
    // The row already exists, so binding it again initializes nothing new:
    // one write, no initialization.
    try std.testing.expectEqual(rows_before + 1, storage.rows_visited);
    storage.endScope(second);
}

test "three nesting levels each restore the enclosing binding of one local" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const shared: LocalId = @enumFromInt(4);
    const types = [_]ValType{ .i64, .i32, .f64 };
    var scopes: [types.len]Scope = undefined;
    var bindings: [types.len]LocalInfo = undefined;

    for (types, 0..) |val_type, level| {
        scopes[level] = storage.beginScope();
        try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(shared));
        // Give each level a different index as well as a different type.
        for (0..level) |_| _ = try storage.allocAnonymousLocal(.i32);
        const idx = try storage.allocLocal(shared, val_type);
        try std.testing.expectEqual(@as(u32, @intCast(level)), idx);
        bindings[level] = .{ .idx = idx, .val_type = val_type };
    }

    var level = types.len;
    while (level > 0) {
        level -= 1;
        try std.testing.expectEqual(bindings[level], storage.getLocalInfo(shared).?);
        storage.endScope(scopes[level]);
    }

    try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(shared));
    try std.testing.expectEqual(@as(usize, 0), storage.local_types.items.len);
}

test "a nested scope that fails to allocate still restores the enclosing binding" {
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0 });
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const shared: LocalId = @enumFromInt(2);
    const outer_scope = storage.beginScope();
    const outer = try storage.allocLocal(shared, .i64);
    const outer_next = storage.nextLocalIdx();

    const inner_scope = storage.beginScope();
    _ = try storage.allocLocal(shared, .i32);
    // The rest of the nested scope runs out of memory partway through.
    storage.allocator = failing.allocator();
    // A LocalId far past the column's capacity, so growing it must allocate.
    try std.testing.expectError(error.OutOfMemory, storage.allocLocal(@enumFromInt(1000), .f32));
    storage.allocator = std.testing.allocator;
    storage.endScope(inner_scope);

    try std.testing.expectEqual(LocalInfo{ .idx = outer, .val_type = .i64 }, storage.getLocalInfo(shared).?);
    try std.testing.expectEqual(outer_next, storage.nextLocalIdx());
    try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(@enumFromInt(1000)));
    storage.endScope(outer_scope);
}
