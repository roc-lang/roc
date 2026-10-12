//! Test allocator adapter whose allocation sequence depends only on its caller.
//!
//! Growth helpers such as `std.ArrayList` first ask the allocator to remap a
//! block in place. Whether a backing allocator can do that depends on its heap
//! state, so the same code can perform a different number of allocations on
//! each run. `std.testing.checkAllAllocationFailures` requires every run to
//! allocate identically, so it reports `NondeterministicMemoryUsage` for such
//! code. This adapter forwards `alloc` and `free` and refuses `resize` and
//! `remap`, which makes the sequence a function of the code under test.
const std = @import("std");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const DeterministicAllocator = @This();

backing: Allocator,

/// Wraps `backing`, usually `std.testing.allocator`.
pub fn init(backing: Allocator) DeterministicAllocator {
    return .{ .backing = backing };
}

/// Returns the allocator interface. The adapter must outlive every use of it.
pub fn allocator(self: *DeterministicAllocator) Allocator {
    return .{ .ptr = self, .vtable = &vtable };
}

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn alloc(context: *anyopaque, len: usize, alignment: Alignment, return_address: usize) ?[*]u8 {
    const self: *DeterministicAllocator = @ptrCast(@alignCast(context));
    return self.backing.rawAlloc(len, alignment, return_address);
}

fn resize(_: *anyopaque, _: []u8, _: Alignment, _: usize, _: usize) bool {
    return false;
}

fn remap(_: *anyopaque, _: []u8, _: Alignment, _: usize, _: usize) ?[*]u8 {
    return null;
}

fn free(context: *anyopaque, memory: []u8, alignment: Alignment, return_address: usize) void {
    const self: *DeterministicAllocator = @ptrCast(@alignCast(context));
    self.backing.rawFree(memory, alignment, return_address);
}

test "growth allocates identically on every run" {
    const Probe = struct {
        fn run(gpa: Allocator) Allocator.Error!usize {
            var failing = std.testing.FailingAllocator.init(gpa, .{});
            var list: std.ArrayList(u64) = .empty;
            defer list.deinit(failing.allocator());
            for (0..300) |value| try list.append(failing.allocator(), value);
            return failing.alloc_index;
        }
    };
    var deterministic = DeterministicAllocator.init(std.testing.allocator);
    const first = try Probe.run(deterministic.allocator());
    for (0..8) |_| try std.testing.expectEqual(first, try Probe.run(deterministic.allocator()));
}

test "resize and remap are refused so growth cannot depend on heap layout" {
    var deterministic = DeterministicAllocator.init(std.testing.allocator);
    const gpa = deterministic.allocator();
    const block = try gpa.alloc(u8, 64);
    defer gpa.free(block);
    try std.testing.expect(!gpa.resize(block, 32));
    try std.testing.expectEqual(@as(?[]u8, null), gpa.remap(block, 128));
}
