//! Tracks where LIR locals live in wasm locals, and which wasm locals a
//! function has to spare.
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
//! Nested helper compilation (RC helpers, host adapters) takes a fresh stamp,
//! so a helper that binds the same `LocalId` as its caller neither sees the
//! caller's binding nor destroys it.
//! Specialized procedures likewise share `LocalId`s without sharing wasm slots.
//!
//! A binding names either a wasm local or a slot in the procedure's frame
//! (`Home`). Frame-slot bindings belong to a procedure emitted as a family of
//! wasm functions that share one frame (see design.md, "Wasm Function
//! Limits"); a fragment scope starts a fresh wasm local numbering for one such
//! function while keeping the family's bindings.
//!
//! Wasm locals that hold no LIR local are anonymous. Most are temporaries one
//! statement's emission uses and finishes with, so each function scope keeps a
//! free list: `releaseTempsSince` returns every anonymous local allocated
//! since a mark to it, and `allocAnonymousLocal` reuses a returned local of
//! the same type before declaring a new one. A LIR local or a persistent local
//! never takes a returned index: it is live across statements, and a loop's
//! remainder is emitted after the body that runs after it, so a temporary
//! that looks finished in emission order may still clobber it at runtime.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const lir = @import("lir");
const LocalId = lir.LIR.LocalId;
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

const Self = @This();

/// Where a bound value lives.
pub const Home = enum {
    /// A wasm local of the function being emitted.
    wasm_local,
    /// Bytes in the procedure's frame, at a byte offset from the frame pointer.
    frame_slot,
};

/// Info about a bound value: its wasm local index, or for a frame slot its
/// byte offset in the frame, and its wasm value type.
pub const LocalInfo = struct {
    idx: u32,
    val_type: ValType,
    home: Home = .wasm_local,
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
    free_base: u32,
    temps_base: u32,
};

/// The enclosing function's local numbering a fragment scope restores on exit.
pub const FragmentScope = struct {
    types_base: u32,
    free_base: u32,
    temps_base: u32,
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
/// Anonymous locals released for reuse. The active function owns `free_base..`.
free: std.ArrayList(LocalInfo),
free_base: u32,
/// Anonymous locals allocated since the last release point, in order. The
/// active function owns `temps_base..`.
temps: std.ArrayList(LocalInfo),
temps_base: u32,
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
        .free = .empty,
        .free_base = 0,
        .temps = .empty,
        .temps_base = 0,
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
    self.free.deinit(self.allocator);
    self.temps.deinit(self.allocator);
}

/// Begin a function scope, returning the outer scope for `endScope`.
pub fn beginScope(self: *Self) Scope {
    const outer: Scope = .{
        .scope = self.active_scope,
        .types_base = self.types_base,
        .shadow_base = @intCast(self.shadow.items.len),
        .free_base = self.free_base,
        .temps_base = self.temps_base,
    };
    self.next_scope += 1;
    self.active_scope = self.next_scope;
    self.types_base = @intCast(self.local_types.items.len);
    self.free_base = @intCast(self.free.items.len);
    self.temps_base = @intCast(self.temps.items.len);
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
    self.free.shrinkRetainingCapacity(self.free_base);
    self.temps.shrinkRetainingCapacity(self.temps_base);
    self.types_base = outer.types_base;
    self.free_base = outer.free_base;
    self.temps_base = outer.temps_base;
    self.active_scope = outer.scope;
}

/// Begin numbering wasm locals for a fragment of the active function scope:
/// a separate wasm function that shares the scope's bindings.
pub fn beginFragment(self: *Self) FragmentScope {
    const outer: FragmentScope = .{
        .types_base = self.types_base,
        .free_base = self.free_base,
        .temps_base = self.temps_base,
    };
    self.types_base = @intCast(self.local_types.items.len);
    self.free_base = @intCast(self.free.items.len);
    self.temps_base = @intCast(self.temps.items.len);
    return outer;
}

/// End a fragment's numbering and resume the enclosing function's.
pub fn endFragment(self: *Self, outer: FragmentScope) void {
    self.local_types.shrinkRetainingCapacity(self.types_base);
    self.free.shrinkRetainingCapacity(self.free_base);
    self.temps.shrinkRetainingCapacity(self.temps_base);
    self.types_base = outer.types_base;
    self.free_base = outer.free_base;
    self.temps_base = outer.temps_base;
}

/// Bind the LIR local to a new wasm local of the given type.
pub fn allocLocal(self: *Self, local_id: LocalId, val_type: ValType) Allocator.Error!u32 {
    const idx = try self.allocPersistentLocal(val_type);
    try self.bind(local_id, .{ .idx = idx, .val_type = val_type });
    return idx;
}

/// Bind the LIR local to the frame slot at `offset`.
pub fn bindLocalSlot(self: *Self, local_id: LocalId, val_type: ValType, offset: u32) Allocator.Error!void {
    try self.bind(local_id, .{ .idx = offset, .val_type = val_type, .home = .frame_slot });
}

fn bind(self: *Self, local_id: LocalId, info: LocalInfo) Allocator.Error!void {
    const row_index = try self.reserveBindingRow(local_id);
    if (builtin.is_test) self.rows_visited += 1;
    try self.shadow.append(self.allocator, .{
        .row_index = @intCast(row_index),
        .row = self.bindings.items[row_index],
    });
    self.bindings.items[row_index] = .{ .scope = self.active_scope, .info = info };
}

/// Move a LIR local the active scope bound to a wasm local into the frame
/// slot at `offset`, for every later emission site that names it.
pub fn rebindLocalToSlot(self: *Self, local_id: LocalId, offset: u32) void {
    const row_index: usize = @backingInt(local_id);
    if (builtin.is_test) self.rows_visited += 1;
    const row = &self.bindings.items[row_index];
    std.debug.assert(row.scope == self.active_scope and row.info.home == .wasm_local);
    row.info = .{ .idx = offset, .val_type = row.info.val_type, .home = .frame_slot };
}

/// The LIR locals the active scope has bound since `scope` began, with their
/// current bindings, in binding order.
pub fn boundSince(self: *const Self, scope: Scope) BoundIterator {
    return .{ .storage = self, .next = scope.shadow_base };
}

/// Iterates `boundSince`.
pub const BoundIterator = struct {
    storage: *const Self,
    next: u32,

    pub fn nextBinding(self: *BoundIterator) ?struct { local_id: LocalId, info: LocalInfo } {
        if (self.next >= self.storage.shadow.items.len) return null;
        const row_index = self.storage.shadow.items[self.next].row_index;
        self.next += 1;
        return .{ .local_id = @fromBackingInt(row_index), .info = self.storage.bindings.items[row_index].info };
    }
};

/// Allocate an anonymous wasm local for one statement's use: a released local
/// of the same type if there is one, else a new one. `releaseTempsSince`
/// returns it to the free list.
pub fn allocAnonymousLocal(self: *Self, val_type: ValType) Allocator.Error!u32 {
    const idx = try self.allocScratchLocal(val_type);
    try self.temps.append(self.allocator, .{ .idx = idx, .val_type = val_type });
    return idx;
}

/// Allocate an anonymous wasm local that the caller releases itself with
/// `releaseScratchLocal` once the code reading it has been emitted.
pub fn allocScratchLocal(self: *Self, val_type: ValType) Allocator.Error!u32 {
    var i = self.free.items.len;
    while (i > self.free_base) {
        i -= 1;
        if (self.free.items[i].val_type != val_type) continue;
        const idx = self.free.items[i].idx;
        _ = self.free.swapRemove(i);
        return idx;
    }
    return self.allocPersistentLocal(val_type);
}

/// Allocate a wasm local that stays allocated for the rest of the function.
pub fn allocPersistentLocal(self: *Self, val_type: ValType) Allocator.Error!u32 {
    const idx = self.nextLocalIdx();
    try self.local_types.append(self.allocator, val_type);
    return idx;
}

/// Return a scratch local to the free list.
pub fn releaseScratchLocal(self: *Self, info: LocalInfo) Allocator.Error!void {
    std.debug.assert(info.home == .wasm_local);
    try self.free.append(self.allocator, info);
}

/// The number of anonymous locals the active function has allocated since its
/// last release point, as a mark for `releaseTempsSince`.
pub fn tempMark(self: *const Self) u32 {
    return @intCast(self.temps.items.len);
}

/// Return every anonymous local allocated since `mark` to the free list.
pub fn releaseTempsSince(self: *Self, mark: u32) Allocator.Error!void {
    std.debug.assert(mark >= self.temps_base);
    try self.free.appendSlice(self.allocator, self.temps.items[mark..]);
    self.temps.shrinkRetainingCapacity(mark);
}

/// Look up the binding a previous emission site made in this scope.
pub fn getLocalInfo(self: *Self, local_id: LocalId) ?LocalInfo {
    const row_index: usize = @backingInt(local_id);
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
    const row_index: usize = @backingInt(local_id);
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

    const shared: LocalId = @fromBackingInt(@intCast(7));
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
        _ = try storage.allocLocal(@fromBackingInt(@intCast(@as(u32, @intCast(i)))), .i32);
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
    _ = try storage.allocLocal(@fromBackingInt(@intCast(3)), .i32);
    _ = try storage.allocAnonymousLocal(.i64);
    try std.testing.expectEqual(@as(u32, 2), storage.nextLocalIdx());
    storage.endScope(first);

    const second = storage.beginScope();
    try std.testing.expectEqual(@as(u32, 0), storage.nextLocalIdx());
    try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(@fromBackingInt(@intCast(3))));
    const rows_before = storage.rows_visited;
    const reused = try storage.allocLocal(@fromBackingInt(@intCast(3)), .f64);
    try std.testing.expectEqual(@as(u32, 0), reused);
    // The row already exists, so binding it again initializes nothing new:
    // one write, no initialization.
    try std.testing.expectEqual(rows_before + 1, storage.rows_visited);
    storage.endScope(second);
}

test "three nesting levels each restore the enclosing binding of one local" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const shared: LocalId = @fromBackingInt(@intCast(4));
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

    const shared: LocalId = @fromBackingInt(@intCast(2));
    const outer_scope = storage.beginScope();
    const outer = try storage.allocLocal(shared, .i64);
    const outer_next = storage.nextLocalIdx();

    const inner_scope = storage.beginScope();
    _ = try storage.allocLocal(shared, .i32);
    // The rest of the nested scope runs out of memory partway through.
    storage.allocator = failing.allocator();
    // A LocalId far past the column's capacity, so growing it must allocate.
    try std.testing.expectError(error.OutOfMemory, storage.allocLocal(@fromBackingInt(@intCast(1000)), .f32));
    storage.allocator = std.testing.allocator;
    storage.endScope(inner_scope);

    try std.testing.expectEqual(LocalInfo{ .idx = outer, .val_type = .i64 }, storage.getLocalInfo(shared).?);
    try std.testing.expectEqual(outer_next, storage.nextLocalIdx());
    try std.testing.expectEqual(@as(?LocalInfo, null), storage.getLocalInfo(@fromBackingInt(@intCast(1000))));
    storage.endScope(outer_scope);
}

test "released temporaries are reused by type and never by bound or persistent locals" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const scope = storage.beginScope();
    defer storage.endScope(scope);

    const mark = storage.tempMark();
    const first = try storage.allocAnonymousLocal(.i32);
    const second = try storage.allocAnonymousLocal(.i64);
    try std.testing.expectEqual(@as(u32, 0), first);
    try std.testing.expectEqual(@as(u32, 1), second);
    try storage.releaseTempsSince(mark);

    // A bound local and a persistent local take fresh indices.
    try std.testing.expectEqual(@as(u32, 2), try storage.allocLocal(@fromBackingInt(@intCast(5)), .i32));
    try std.testing.expectEqual(@as(u32, 3), try storage.allocPersistentLocal(.i64));
    // An anonymous local of each type reuses the released one of that type.
    try std.testing.expectEqual(@as(u32, 1), try storage.allocAnonymousLocal(.i64));
    try std.testing.expectEqual(@as(u32, 0), try storage.allocAnonymousLocal(.i32));
    // Nothing of this type is free, so a new index is declared.
    try std.testing.expectEqual(@as(u32, 4), try storage.allocAnonymousLocal(.f64));
    try std.testing.expectEqual(@as(usize, 5), storage.currentTypes().len);
}

test "a scratch local is reused only once released, and a scope's free list ends with it" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const outer = storage.beginScope();
    const scratch = try storage.allocScratchLocal(.i32);
    const mark = storage.tempMark();
    _ = try storage.allocAnonymousLocal(.i32);
    try storage.releaseTempsSince(mark);
    // The scratch local was not in the temporaries, so it is not reused here.
    try std.testing.expectEqual(@as(u32, 1), try storage.allocAnonymousLocal(.i32));
    try storage.releaseScratchLocal(.{ .idx = scratch, .val_type = .i32 });

    const inner = storage.beginScope();
    // The outer scope's free list is invisible to the nested function.
    try std.testing.expectEqual(@as(u32, 0), try storage.allocAnonymousLocal(.i32));
    storage.endScope(inner);

    try std.testing.expectEqual(scratch, try storage.allocAnonymousLocal(.i32));
    storage.endScope(outer);
}

test "a fragment numbers its wasm locals afresh while sharing the scope's bindings" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const scope = storage.beginScope();
    defer storage.endScope(scope);

    const slot_local: LocalId = @fromBackingInt(@intCast(9));
    _ = try storage.allocPersistentLocal(.i32);
    try storage.bindLocalSlot(slot_local, .i64, 16);
    const mark = storage.tempMark();
    _ = try storage.allocAnonymousLocal(.i32);
    try storage.releaseTempsSince(mark);

    const fragment = storage.beginFragment();
    try std.testing.expectEqual(@as(u32, 0), storage.nextLocalIdx());
    // The enclosing function's free list is not the fragment's.
    try std.testing.expectEqual(@as(u32, 0), try storage.allocAnonymousLocal(.i32));
    try std.testing.expectEqual(LocalInfo{ .idx = 16, .val_type = .i64, .home = .frame_slot }, storage.getLocalInfo(slot_local).?);
    const later: LocalId = @fromBackingInt(@intCast(10));
    try storage.bindLocalSlot(later, .i32, 24);
    try std.testing.expectEqual(@as(usize, 1), storage.currentTypes().len);
    storage.endFragment(fragment);

    // Back in the enclosing function: its numbering, its free list, and a
    // binding the fragment made.
    try std.testing.expectEqual(@as(u32, 2), storage.nextLocalIdx());
    try std.testing.expectEqual(@as(u32, 1), try storage.allocAnonymousLocal(.i32));
    try std.testing.expectEqual(LocalInfo{ .idx = 24, .val_type = .i32, .home = .frame_slot }, storage.getLocalInfo(later).?);
}

test "bound locals are listed in binding order and can move into frame slots" {
    var storage = Self.init(std.testing.allocator);
    defer storage.deinit();

    const scope = storage.beginScope();
    defer storage.endScope(scope);

    const a: LocalId = @fromBackingInt(@intCast(3));
    const b: LocalId = @fromBackingInt(@intCast(1));
    _ = try storage.allocLocal(a, .i64);
    _ = try storage.allocLocal(b, .i32);
    storage.rebindLocalToSlot(a, 8);

    var it = storage.boundSince(scope);
    const first = it.nextBinding().?;
    try std.testing.expectEqual(a, first.local_id);
    try std.testing.expectEqual(LocalInfo{ .idx = 8, .val_type = .i64, .home = .frame_slot }, first.info);
    const second = it.nextBinding().?;
    try std.testing.expectEqual(b, second.local_id);
    try std.testing.expectEqual(LocalInfo{ .idx = 1, .val_type = .i32 }, second.info);
    try std.testing.expect(it.nextBinding() == null);
}
