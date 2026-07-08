//! Debug-checked growable lists for mutable compiler stores.
//!
//! A guarded list has the same release representation as `std.ArrayList(T)`,
//! but Debug builds track physical backing moves so stale span and pointer
//! borrows fail deterministically at the point of guarded access.

const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const debug_guards = builtin.mode == .Debug;

/// Growable list wrapper that checks stale borrows in Debug and erases to `std.ArrayList` otherwise.
pub fn List(comptime T: type, comptime list_name: []const u8) type {
    return struct {
        const Self = @This();
        const Generation = if (debug_guards) u64 else void;

        __guarded_backing: std.ArrayList(T) = .empty,
        __guarded_generation: Generation = if (debug_guards) 0 else {},

        pub const empty: Self = .{};
        pub const Slice = std.ArrayList(T).Slice;

        comptime {
            if (!debug_guards) {
                std.debug.assert(@sizeOf(Self) == @sizeOf(std.ArrayList(T)));
                std.debug.assert(@alignOf(Self) == @alignOf(std.ArrayList(T)));
            }
        }

        pub fn init() Self {
            return .empty;
        }

        pub fn initCapacity(allocator: Allocator, capacity_: usize) Allocator.Error!Self {
            return .{ .__guarded_backing = try std.ArrayList(T).initCapacity(allocator, capacity_) };
        }

        pub fn fromArrayList(backing: std.ArrayList(T)) Self {
            return .{ .__guarded_backing = backing };
        }

        pub fn takeArrayList(self: *Self) std.ArrayList(T) {
            self.invalidateIfElementsCouldBeBorrowed();
            const backing = self.__guarded_backing;
            self.__guarded_backing = .empty;
            return backing;
        }

        pub fn fromOwnedSlice(slice: []T) Self {
            return .{ .__guarded_backing = std.ArrayList(T).fromOwnedSlice(slice) };
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.invalidateIfElementsCouldBeBorrowed();
            self.__guarded_backing.deinit(allocator);
            self.__guarded_backing = .empty;
        }

        pub fn len(self: *const Self) usize {
            return self.__guarded_backing.items.len;
        }

        pub fn capacity(self: *const Self) usize {
            return self.__guarded_backing.capacity;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len() == 0;
        }

        pub fn append(self: *Self, allocator: Allocator, item: T) Allocator.Error!void {
            const old = self.moveStateBeforeGrowth();
            try self.__guarded_backing.append(allocator, item);
            self.bumpIfMoved(old);
        }

        pub fn appendAssumeCapacity(self: *Self, item: T) void {
            self.__guarded_backing.appendAssumeCapacity(item);
        }

        pub fn appendSlice(self: *Self, allocator: Allocator, values: []const T) Allocator.Error!void {
            const old = self.moveStateBeforeGrowth();
            try self.__guarded_backing.appendSlice(allocator, values);
            self.bumpIfMoved(old);
        }

        pub fn appendSliceAssumeCapacity(self: *Self, values: []const T) void {
            self.__guarded_backing.appendSliceAssumeCapacity(values);
        }

        pub fn pop(self: *Self) ?T {
            return self.__guarded_backing.pop();
        }

        pub fn ensureUnusedCapacity(self: *Self, allocator: Allocator, additional_count: usize) Allocator.Error!void {
            const old = self.moveStateBeforeGrowth();
            try self.__guarded_backing.ensureUnusedCapacity(allocator, additional_count);
            self.bumpIfMoved(old);
        }

        pub fn ensureTotalCapacity(self: *Self, allocator: Allocator, new_capacity: usize) Allocator.Error!void {
            const old = self.moveStateBeforeGrowth();
            try self.__guarded_backing.ensureTotalCapacity(allocator, new_capacity);
            self.bumpIfMoved(old);
        }

        pub fn ensureTotalCapacityPrecise(self: *Self, allocator: Allocator, new_capacity: usize) Allocator.Error!void {
            const old = self.moveStateBeforeGrowth();
            try self.__guarded_backing.ensureTotalCapacityPrecise(allocator, new_capacity);
            self.bumpIfMoved(old);
        }

        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            self.__guarded_backing.shrinkRetainingCapacity(new_len);
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            self.__guarded_backing.clearRetainingCapacity();
        }

        pub fn clearAndFree(self: *Self, allocator: Allocator) void {
            self.invalidateIfElementsCouldBeBorrowed();
            self.__guarded_backing.clearAndFree(allocator);
        }

        pub fn shrinkAndFree(self: *Self, allocator: Allocator, new_len: usize) void {
            const old = self.moveStateBeforeGrowth();
            self.__guarded_backing.shrinkAndFree(allocator, new_len);
            self.bumpIfMoved(old);
        }

        pub fn toOwnedSlice(self: *Self, allocator: Allocator) Allocator.Error![]T {
            self.invalidateIfElementsCouldBeBorrowed();
            return self.__guarded_backing.toOwnedSlice(allocator);
        }

        pub fn toOwnedSliceAssert(self: *Self) []T {
            self.invalidateIfElementsCouldBeBorrowed();
            return self.__guarded_backing.toOwnedSliceAssert();
        }

        pub fn get(self: *const Self, index: usize) T {
            return self.__guarded_backing.items[index];
        }

        pub fn getPtrImmediate(self: *Self, index: usize) *T {
            return &self.__guarded_backing.items[index];
        }

        pub fn set(self: *Self, index: usize, value: T) void {
            self.__guarded_backing.items[index] = value;
        }

        pub fn replace(self: *Self, index: usize, value: T) T {
            const old = self.__guarded_backing.items[index];
            self.__guarded_backing.items[index] = value;
            return old;
        }

        pub fn borrowSpan(self: *const Self, start: usize, span_len: usize) BorrowSpan(T, list_name) {
            assertRangeInBounds(list_name, self.__guarded_backing.items.len, start, span_len);
            if (debug_guards) {
                return .{
                    .list = self,
                    .start = start,
                    .len = span_len,
                    .generation = self.currentGeneration(),
                };
            } else {
                return self.__guarded_backing.items[start..][0..span_len];
            }
        }

        pub fn borrowSpanMut(self: *Self, start: usize, span_len: usize) BorrowSpanMut(T, list_name) {
            assertRangeInBounds(list_name, self.__guarded_backing.items.len, start, span_len);
            if (debug_guards) {
                return .{
                    .list = self,
                    .start = start,
                    .len = span_len,
                    .generation = self.currentGeneration(),
                };
            } else {
                return self.__guarded_backing.items[start..][0..span_len];
            }
        }

        pub fn borrowPtr(self: *Self, index: usize) BorrowPtr(T, list_name) {
            assertIndexInBounds(list_name, self.__guarded_backing.items.len, index);
            if (debug_guards) {
                return .{
                    .list = self,
                    .index = index,
                    .generation = self.currentGeneration(),
                };
            } else {
                return &self.__guarded_backing.items[index];
            }
        }

        pub fn borrowPtrConst(self: *const Self, index: usize) BorrowPtrConst(T, list_name) {
            assertIndexInBounds(list_name, self.__guarded_backing.items.len, index);
            if (debug_guards) {
                return .{
                    .list = self,
                    .index = index,
                    .generation = self.currentGeneration(),
                };
            } else {
                return &self.__guarded_backing.items[index];
            }
        }

        pub fn dupeSpan(self: *const Self, allocator: Allocator, start: usize, span_len: usize) Allocator.Error![]T {
            assertRangeInBounds(list_name, self.__guarded_backing.items.len, start, span_len);
            return allocator.dupe(T, self.__guarded_backing.items[start..][0..span_len]);
        }

        pub fn markLen(self: *const Self) usize {
            return self.__guarded_backing.items.len;
        }

        pub fn restoreLen(self: *Self, mark: usize) void {
            self.shrinkRetainingCapacity(mark);
        }

        pub fn unsafeRawItemsForView(self: *const Self) []const T {
            return self.__guarded_backing.items;
        }

        pub fn unsafeRawItemsMutForStore(self: *Self) []T {
            return self.__guarded_backing.items;
        }

        pub fn unsafeBackingForClone(self: *const Self) *const std.ArrayList(T) {
            return &self.__guarded_backing;
        }

        pub fn unsafeBackingForStore(self: *Self) *std.ArrayList(T) {
            return &self.__guarded_backing;
        }

        fn currentGeneration(self: *const Self) u64 {
            if (debug_guards) return self.__guarded_generation;
            return 0;
        }

        fn invalidateIfElementsCouldBeBorrowed(self: *Self) void {
            if (!debug_guards) return;
            if (self.__guarded_backing.items.len != 0 or self.__guarded_backing.capacity != 0) {
                self.__guarded_generation +%= 1;
            }
        }

        const MoveState = struct {
            ptr: [*]T,
            len: usize,
        };

        fn moveStateBeforeGrowth(self: *const Self) MoveState {
            return .{
                .ptr = self.__guarded_backing.items.ptr,
                .len = self.__guarded_backing.items.len,
            };
        }

        fn bumpIfMoved(self: *Self, old: MoveState) void {
            if (!debug_guards) return;
            if (old.len == 0) return;
            if (old.ptr != self.__guarded_backing.items.ptr) {
                self.__guarded_generation +%= 1;
            }
        }
    };
}

/// Read-only span borrow type for a guarded list.
pub fn BorrowSpan(comptime T: type, comptime list_name: []const u8) type {
    if (!debug_guards) return []const T;

    return struct {
        const Self = @This();

        list: *const List(T, list_name),
        start: usize,
        len: usize,
        generation: u64,

        pub fn at(self: Self, index: usize) T {
            self.assertElementAccess(index);
            return self.list.__guarded_backing.items[self.start + index];
        }

        pub fn assertValid(self: Self) void {
            if (self.len == 0) return;
            self.assertCurrent("span validation");
        }

        fn assertElementAccess(self: Self, index: usize) void {
            if (index >= self.len) {
                std.debug.panic(
                    "guarded list invalidated: {s}: span access index={d} exceeds borrowed len={d}",
                    .{ list_name, index, self.len },
                );
            }
            self.assertCurrent("span element access");
        }

        fn assertCurrent(self: Self, operation: []const u8) void {
            assertRangeInBounds(list_name, self.list.__guarded_backing.items.len, self.start, self.len);
            if (self.list.__guarded_generation != self.generation) {
                std.debug.panic(
                    "guarded list invalidated: {s}: {s}; borrowed start={d} len={d} generation={d} current_generation={d} current_len={d}",
                    .{
                        list_name,
                        operation,
                        self.start,
                        self.len,
                        self.generation,
                        self.list.__guarded_generation,
                        self.list.__guarded_backing.items.len,
                    },
                );
            }
        }
    };
}

/// Mutable span borrow type for a guarded list.
pub fn BorrowSpanMut(comptime T: type, comptime list_name: []const u8) type {
    if (!debug_guards) return []T;

    return struct {
        const Self = @This();

        list: *List(T, list_name),
        start: usize,
        len: usize,
        generation: u64,

        pub fn at(self: Self, index: usize) T {
            self.assertElementAccess(index);
            return self.list.__guarded_backing.items[self.start + index];
        }

        pub fn atPtr(self: Self, index: usize) *T {
            self.assertElementAccess(index);
            return &self.list.__guarded_backing.items[self.start + index];
        }

        pub fn set(self: Self, index: usize, value: T) void {
            self.assertElementAccess(index);
            self.list.__guarded_backing.items[self.start + index] = value;
        }

        pub fn assertValid(self: Self) void {
            if (self.len == 0) return;
            self.assertCurrent("mutable span validation");
        }

        fn assertElementAccess(self: Self, index: usize) void {
            if (index >= self.len) {
                std.debug.panic(
                    "guarded list invalidated: {s}: mutable span access index={d} exceeds borrowed len={d}",
                    .{ list_name, index, self.len },
                );
            }
            self.assertCurrent("mutable span element access");
        }

        fn assertCurrent(self: Self, operation: []const u8) void {
            assertRangeInBounds(list_name, self.list.__guarded_backing.items.len, self.start, self.len);
            if (self.list.__guarded_generation != self.generation) {
                std.debug.panic(
                    "guarded list invalidated: {s}: {s}; borrowed start={d} len={d} generation={d} current_generation={d} current_len={d}",
                    .{
                        list_name,
                        operation,
                        self.start,
                        self.len,
                        self.generation,
                        self.list.__guarded_generation,
                        self.list.__guarded_backing.items.len,
                    },
                );
            }
        }
    };
}

/// Mutable element pointer borrow type for a guarded list.
pub fn BorrowPtr(comptime T: type, comptime list_name: []const u8) type {
    if (!debug_guards) return *T;

    return struct {
        const Self = @This();

        list: *List(T, list_name),
        index: usize,
        generation: u64,

        pub fn get(self: Self) T {
            self.assertCurrent("pointer get");
            return self.list.__guarded_backing.items[self.index];
        }

        pub fn set(self: Self, value: T) void {
            self.assertCurrent("pointer set");
            self.list.__guarded_backing.items[self.index] = value;
        }

        pub fn ptrImmediate(self: Self) *T {
            self.assertCurrent("immediate pointer access");
            return &self.list.__guarded_backing.items[self.index];
        }

        fn assertCurrent(self: Self, operation: []const u8) void {
            assertIndexInBounds(list_name, self.list.__guarded_backing.items.len, self.index);
            if (self.list.__guarded_generation != self.generation) {
                std.debug.panic(
                    "guarded list invalidated: {s}: {s}; borrowed index={d} generation={d} current_generation={d} current_len={d}",
                    .{
                        list_name,
                        operation,
                        self.index,
                        self.generation,
                        self.list.__guarded_generation,
                        self.list.__guarded_backing.items.len,
                    },
                );
            }
        }
    };
}

/// Read-only element pointer borrow type for a guarded list.
pub fn BorrowPtrConst(comptime T: type, comptime list_name: []const u8) type {
    if (!debug_guards) return *const T;

    return struct {
        const Self = @This();

        list: *const List(T, list_name),
        index: usize,
        generation: u64,

        pub fn get(self: Self) T {
            self.assertCurrent("const pointer get");
            return self.list.__guarded_backing.items[self.index];
        }

        fn assertCurrent(self: Self, operation: []const u8) void {
            assertIndexInBounds(list_name, self.list.__guarded_backing.items.len, self.index);
            if (self.list.__guarded_generation != self.generation) {
                std.debug.panic(
                    "guarded list invalidated: {s}: {s}; borrowed index={d} generation={d} current_generation={d} current_len={d}",
                    .{
                        list_name,
                        operation,
                        self.index,
                        self.generation,
                        self.list.__guarded_generation,
                        self.list.__guarded_backing.items.len,
                    },
                );
            }
        }
    };
}

/// Returns the length of a slice or guarded span borrow.
pub fn borrowLen(borrow: anytype) usize {
    const Borrow = @TypeOf(borrow);
    return switch (@typeInfo(Borrow)) {
        .pointer => |ptr| switch (ptr.size) {
            .slice => borrow.len,
            .one => switch (@typeInfo(ptr.child)) {
                .array => |array| array.len,
                else => 1,
            },
            else => @compileError("borrowLen expected a slice, array pointer, or guarded span"),
        },
        .@"struct" => borrow.len,
        else => @compileError("borrowLen expected a slice or guarded span"),
    };
}

/// Reads one element from a slice or guarded span borrow.
pub fn at(borrow: anytype, index: usize) BorrowElement(@TypeOf(borrow)) {
    const Borrow = @TypeOf(borrow);
    return switch (@typeInfo(Borrow)) {
        .pointer => |ptr| switch (ptr.size) {
            .slice => borrow[index],
            .one => switch (@typeInfo(ptr.child)) {
                .array => |array| if (array.len == 0) unreachable else borrow[index],
                else => blk: {
                    std.debug.assert(index == 0);
                    break :blk borrow.*;
                },
            },
            else => @compileError("at expected a slice, array pointer, or guarded span"),
        },
        .@"struct" => borrow.at(index),
        else => @compileError("at expected a slice or guarded span"),
    };
}

/// Copies a slice or guarded span borrow into owned memory.
pub fn dupe(allocator: Allocator, comptime T: type, borrow: anytype) Allocator.Error![]T {
    const len = borrowLen(borrow);
    const copied = try allocator.alloc(T, len);
    for (0..len) |index| {
        copied[index] = at(borrow, index);
    }
    return copied;
}

/// Returns a mutable element pointer from a slice or guarded mutable span borrow.
pub fn atPtr(borrow: anytype, index: usize) *BorrowElement(@TypeOf(borrow)) {
    const Borrow = @TypeOf(borrow);
    return switch (@typeInfo(Borrow)) {
        .pointer => |ptr| switch (ptr.size) {
            .slice => &borrow[index],
            .one => switch (@typeInfo(ptr.child)) {
                .array => &borrow[index],
                else => blk: {
                    std.debug.assert(index == 0);
                    break :blk borrow;
                },
            },
            else => @compileError("atPtr expected a mutable slice, array pointer, or guarded mutable span"),
        },
        .@"struct" => borrow.atPtr(index),
        else => @compileError("atPtr expected a mutable slice or guarded mutable span"),
    };
}

/// Reads the element behind a raw pointer or guarded pointer borrow.
pub fn ptrGet(ptr_borrow: anytype) PtrBorrowElement(@TypeOf(ptr_borrow)) {
    const PtrBorrow = @TypeOf(ptr_borrow);
    return switch (@typeInfo(PtrBorrow)) {
        .pointer => ptr_borrow.*,
        .@"struct" => ptr_borrow.get(),
        else => @compileError("ptrGet expected a pointer or guarded pointer"),
    };
}

/// Writes the element behind a raw pointer or guarded pointer borrow.
pub fn ptrSet(ptr_borrow: anytype, value: PtrBorrowElement(@TypeOf(ptr_borrow))) void {
    const PtrBorrow = @TypeOf(ptr_borrow);
    switch (@typeInfo(PtrBorrow)) {
        .pointer => ptr_borrow.* = value,
        .@"struct" => ptr_borrow.set(value),
        else => @compileError("ptrSet expected a pointer or guarded pointer"),
    }
}

fn BorrowElement(comptime Borrow: type) type {
    return switch (@typeInfo(Borrow)) {
        .pointer => |ptr| blk: {
            switch (ptr.size) {
                .slice => break :blk ptr.child,
                .one => switch (@typeInfo(ptr.child)) {
                    .array => |array| break :blk array.child,
                    else => break :blk ptr.child,
                },
                else => @compileError("expected slice, array pointer, or guarded span"),
            }
        },
        .@"struct" => @typeInfo(@TypeOf(Borrow.at)).@"fn".return_type.?,
        else => @compileError("expected slice or guarded span"),
    };
}

fn PtrBorrowElement(comptime PtrBorrow: type) type {
    return switch (@typeInfo(PtrBorrow)) {
        .pointer => |ptr| ptr.child,
        .@"struct" => @typeInfo(@TypeOf(PtrBorrow.get)).@"fn".return_type.?,
        else => @compileError("expected pointer or guarded pointer"),
    };
}

fn assertRangeInBounds(comptime list_name: []const u8, current_len: usize, start: usize, len_: usize) void {
    if (start > current_len or len_ > current_len - start) {
        std.debug.panic(
            "guarded list invalidated: {s}: span start={d} len={d} exceeds current_len={d}",
            .{ list_name, start, len_, current_len },
        );
    }
}

fn assertIndexInBounds(comptime list_name: []const u8, current_len: usize, index: usize) void {
    if (index >= current_len) {
        std.debug.panic(
            "guarded list invalidated: {s}: index={d} exceeds current_len={d}",
            .{ list_name, index, current_len },
        );
    }
}

test "GuardedList no-move append keeps existing borrow valid" {
    var list = try List(u32, "test.no_move").initCapacity(std.testing.allocator, 4);
    defer list.deinit(std.testing.allocator);

    try list.append(std.testing.allocator, 10);
    const borrow = list.borrowSpan(0, 1);
    try list.append(std.testing.allocator, 20);

    try std.testing.expectEqual(@as(usize, 1), borrowLen(borrow));
    try std.testing.expectEqual(@as(u32, 10), at(borrow, 0));
}

test "GuardedList no-move append keeps existing pointer borrow valid" {
    var list = try List(u32, "test.ptr_no_move").initCapacity(std.testing.allocator, 4);
    defer list.deinit(std.testing.allocator);

    try list.append(std.testing.allocator, 10);
    const borrow = list.borrowPtr(0);
    try list.append(std.testing.allocator, 20);

    try std.testing.expectEqual(@as(u32, 10), ptrGet(borrow));
    ptrSet(borrow, 30);
    try std.testing.expectEqual(@as(u32, 30), list.get(0));
}

test "GuardedList no-move reserve keeps existing borrow valid" {
    var list = try List(u32, "test.reserve_no_move").initCapacity(std.testing.allocator, 4);
    defer list.deinit(std.testing.allocator);

    try list.append(std.testing.allocator, 10);
    const borrow = list.borrowSpan(0, 1);
    try list.ensureUnusedCapacity(std.testing.allocator, 1);

    try std.testing.expectEqual(@as(u32, 10), at(borrow, 0));
}

test "GuardedList no-move appendSlice keeps existing borrow valid" {
    var list = try List(u32, "test.append_slice_no_move").initCapacity(std.testing.allocator, 4);
    defer list.deinit(std.testing.allocator);

    try list.append(std.testing.allocator, 10);
    const borrow = list.borrowSpan(0, 1);
    try list.appendSlice(std.testing.allocator, &.{ 20, 30 });

    try std.testing.expectEqual(@as(usize, 1), borrowLen(borrow));
    try std.testing.expectEqual(@as(u32, 10), at(borrow, 0));
}

test "GuardedList range-precise restore keeps prefix borrow valid" {
    var list = try List(u32, "test.restore_prefix").initCapacity(std.testing.allocator, 8);
    defer list.deinit(std.testing.allocator);

    try list.appendSlice(std.testing.allocator, &.{ 1, 2, 3, 4 });
    const borrow = list.borrowSpan(0, 2);
    list.restoreLen(2);

    try std.testing.expectEqual(@as(usize, 2), borrowLen(borrow));
    try std.testing.expectEqual(@as(u32, 1), at(borrow, 0));
    try std.testing.expectEqual(@as(u32, 2), at(borrow, 1));
}

test "GuardedList zero-length borrow does not false-positive after clear" {
    var list = try List(u32, "test.empty_borrow").initCapacity(std.testing.allocator, 4);
    defer list.deinit(std.testing.allocator);

    try list.append(std.testing.allocator, 10);
    const borrow = list.borrowSpan(1, 0);
    list.clearRetainingCapacity();

    try std.testing.expectEqual(@as(usize, 0), borrowLen(borrow));
    if (debug_guards) borrow.assertValid();
}

test "GuardedList release representation and borrow types are raw" {
    if (!debug_guards) {
        const U32List = List(u32, "test.release_layout");
        try std.testing.expectEqual(@sizeOf(std.ArrayList(u32)), @sizeOf(U32List));
        try std.testing.expectEqual(@alignOf(std.ArrayList(u32)), @alignOf(U32List));
        try std.testing.expect(BorrowSpan(u32, "test.release_span") == []const u32);
        try std.testing.expect(BorrowSpanMut(u32, "test.release_span_mut") == []u32);
        try std.testing.expect(BorrowPtr(u32, "test.release_ptr") == *u32);
        try std.testing.expect(BorrowPtrConst(u32, "test.release_ptr_const") == *const u32);
    }
}
