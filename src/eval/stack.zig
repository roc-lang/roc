//! The stack (as in stack memory) used when evaluating Roc IR.
//!
//! This has a lot in common with a thread's stack memory: it's fixed-size, bumps a
//! pointer to allocate, and can pop. But this stack's memory comes from an Allocator.
//!
//! When a scope begins, we write down the current stack pointer (at runtime), and then
//! proceed with letting things do `stack.alloca()` as often as necessary. Then, when
//! that scope ends, we reset the stack back to where it was when the scope began,
//! and everything is effectively wiped out.
//!
//! When it's a function being called, or an expression that's going to evaluate to
//! a value that has its own scope (e.g. `x = { ... }`), it's the job of the caller
//! (or the code preceding the expression) to allocate enough space for the return value
//! before entering the scope, and to hand a pointer to that space to the scope (or function).
//! That way, the return value can be written to a location where the caller can access it
//! even after the stack has been reset due to the scope ending.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

/// Error when an alloca is attempted that's too big for the stack
pub const StackOverflow = error{
    StackOverflow,
};

/// Fixed-size stack memory allocator to be used when evaluating Roc IR
pub const Stack = struct {
    allocator: std.mem.Allocator,

    /// The original allocation
    start: [*]u8,

    /// We only allow u32 for these, both because the stack is never going to be
    /// so big that they would overflow, but also because we don't want compile-time
    /// evaluation of constants to succeed on 64-bit targets and fail on 32-bit targets
    /// (e.g. when compiling on wasm32). It should consistently work (or not) on both!
    capacity: u32,

    /// How many bytes are currently in use on the stack.
    //
    // If this is 0, self.next() will return self.start
    used: u32,

    pub fn initCapacity(allocator: std.mem.Allocator, capacity: u32) std.mem.Allocator.Error!Stack {
        const memory = try allocator.alloc(u8, capacity);
        return .{
            .allocator = allocator,
            .start = memory.ptr,
            .capacity = capacity,
            .used = 0,
        };
    }

    /// IMPORTANT: Before calling this, it is critical to ensure that nothing which
    /// was allocated on the stack is still being referenced anywhere!
    pub fn deinit(self: *Stack) void {
        self.allocator.free(self.start[0..self.capacity]);
    }

    /// Advance the stack pointer.
    ///
    /// This is called `alloca` because that's what libc calls this operation.
    pub fn alloca(self: *Stack, bytes: u32, alignment: u32) StackOverflow!*anyopaque {
        if (bytes == 0) return self.next();

        // Calculate padding needed for alignment
        const current_addr = @intFromPtr(self.next());
        const aligned_addr = std.mem.alignForward(usize, current_addr, alignment);
        const padding = @as(u32, @intCast(aligned_addr - current_addr));

        // Check for overflow when adding padding
        const bytes_with_padding = @addWithOverflow(bytes, padding);
        if (bytes_with_padding[1] != 0) {
            return StackOverflow.StackOverflow;
        }

        // Check for overflow when adding to used
        const new_used = @addWithOverflow(self.used, bytes_with_padding[0]);
        if (new_used[1] != 0 or new_used[0] > self.capacity) {
            return StackOverflow.StackOverflow;
        }

        // Advance the pointer
        const result = self.start + self.used + padding;
        self.used = new_used[0];

        return @ptrCast(result);
    }

    /// Restore the stack pointer to an earlier state.
    pub fn restore(self: *Stack, ptr: *anyopaque) void {
        const ptr_addr = @intFromPtr(ptr);
        const start_addr = @intFromPtr(self.start);

        // Debug assertion to validate that the pointer is within our allocation
        std.debug.assert(ptr_addr >= start_addr and ptr_addr <= start_addr + self.capacity);

        // Calculate new used bytes
        const new_used = ptr_addr - start_addr;
        std.debug.assert(new_used <= self.capacity);

        self.used = @intCast(new_used);
    }

    pub fn next(self: *const Stack) *anyopaque {
        return @ptrCast(self.start + self.used);
    }

    /// Check if the stack is empty
    pub fn isEmpty(self: *const Stack) bool {
        return self.used == 0;
    }

    /// Helper function to get the number of bytes available for allocation
    fn available(self: *const Stack) u32 {
        return self.capacity - self.used;
    }
};

test "Stack.initCapacity and deinit" {
    var stack = try Stack.initCapacity(testing.allocator, 1024);
    defer stack.deinit();

    try testing.expectEqual(@as(u32, 1024), stack.capacity);
    try testing.expectEqual(@as(u32, 0), stack.used);
    try testing.expect(stack.isEmpty());
}

test "Stack.alloca basic allocation" {
    var stack = try Stack.initCapacity(testing.allocator, 1024);
    defer stack.deinit();

    const ptr1 = try stack.alloca(10, 1);
    try testing.expectEqual(@as(u32, 10), stack.used);

    const ptr2 = try stack.alloca(20, 1);
    try testing.expectEqual(@as(u32, 30), stack.used);

    // The pointers should be different
    try testing.expect(@intFromPtr(ptr1) != @intFromPtr(ptr2));
    try testing.expectEqual(@as(usize, 10), @intFromPtr(ptr2) - @intFromPtr(ptr1));
}

test "Stack.alloca with alignment" {
    var stack = try Stack.initCapacity(testing.allocator, 4096);
    defer stack.deinit();

    // Test alignments from 1 to 16
    const alignments = [_]u32{ 1, 2, 4, 8, 16 };

    // Test different misalignments
    for (0..17) |misalign| {
        // Reset stack for each misalignment test
        stack.used = 0;

        // Create initial misalignment
        if (misalign > 0) {
            _ = try stack.alloca(@intCast(misalign), 1);
        }

        // Test each alignment with the current misalignment
        for (alignments) |alignment| {
            const start_used = stack.used;
            const allocation_size: u32 = 32; // Use a consistent size for testing

            const aligned_ptr = try stack.alloca(allocation_size, alignment);

            // Verify the pointer is properly aligned
            try testing.expectEqual(@as(usize, 0), @intFromPtr(aligned_ptr) % alignment);

            // Calculate expected padding
            const start_addr = @intFromPtr(stack.start) + start_used;
            const aligned_addr = std.mem.alignForward(usize, start_addr, alignment);
            const expected_padding = @as(u32, @intCast(aligned_addr - start_addr));

            // Verify the stack used the expected amount of space
            const expected_used = start_used + expected_padding + allocation_size;
            try testing.expectEqual(expected_used, stack.used);
        }
    }

    // Additional test: allocate with various alignments in sequence
    stack.used = 0;
    for (alignments) |alignment| {
        // Create some misalignment
        _ = try stack.alloca(3, 1);

        const before_used = stack.used;
        const ptr = try stack.alloca(alignment * 2, alignment);

        // Verify alignment
        try testing.expectEqual(@as(usize, 0), @intFromPtr(ptr) % alignment);

        // Verify we used at least the requested amount
        try testing.expect(stack.used >= before_used + alignment * 2);
    }
}

test "Stack.alloca overflow" {
    var stack = try Stack.initCapacity(testing.allocator, 100);
    defer stack.deinit();

    // This should succeed
    _ = try stack.alloca(50, 1);

    // This should fail (would total 150 bytes)
    try testing.expectError(StackOverflow.StackOverflow, stack.alloca(100, 1));

    // Stack should still be in valid state
    try testing.expectEqual(@as(u32, 50), stack.used);
}

test "Stack.restore" {
    var stack = try Stack.initCapacity(testing.allocator, 1024);
    defer stack.deinit();

    const checkpoint = stack.next();
    _ = try stack.alloca(100, 1);
    try testing.expectEqual(@as(u32, 100), stack.used);

    stack.restore(checkpoint);
    try testing.expectEqual(@as(u32, 0), stack.used);

    // Allocate again after restore
    const ptr1 = try stack.alloca(50, 1);
    try testing.expectEqual(@intFromPtr(checkpoint), @intFromPtr(ptr1));
}

test "Stack.isEmpty" {
    var stack = try Stack.initCapacity(testing.allocator, 100);
    defer stack.deinit();

    try testing.expect(stack.isEmpty());
    try testing.expectEqual(@as(u32, 100), stack.available());

    _ = try stack.alloca(30, 1);
    try testing.expect(!stack.isEmpty());
    try testing.expectEqual(@as(u32, 70), stack.available());
}

test "Stack zero-size allocation" {
    var stack = try Stack.initCapacity(testing.allocator, 100);
    defer stack.deinit();

    const ptr1 = try stack.alloca(0, 1);
    const ptr2 = try stack.alloca(0, 1);

    // Zero-size allocations should return the same pointer
    try testing.expectEqual(@intFromPtr(ptr1), @intFromPtr(ptr2));
    try testing.expectEqual(@as(u32, 0), stack.used);
}
