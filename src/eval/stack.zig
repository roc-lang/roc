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
const collections = @import("collections");

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
        // Allocate the backing memory aligned to max_roc_alignment,
        // in case the first `alloca` happens to need that alignment.
        if (allocator.rawAlloc(
            capacity,
            collections.max_roc_alignment,
            @returnAddress(),
        )) |allocation| {
            return .{
                .allocator = allocator,
                .start = allocation,
                .capacity = capacity,
                .used = 0,
            };
        } else {
            return std.mem.Allocator.Error.OutOfMemory;
        }
    }

    /// IMPORTANT: Before calling this, it is critical to ensure that nothing which
    /// was allocated on the stack is still being referenced anywhere!
    pub fn deinit(self: *Stack) void {
        self.allocator.rawFree(
            self.start[0..self.capacity],
            collections.max_roc_alignment,
            @returnAddress(),
        );
    }

    /// Advance the stack pointer.
    ///
    /// This is called `alloca` because that's what libc calls this operation.
    pub fn alloca(self: *Stack, bytes: u32, alignment: std.mem.Alignment) StackOverflow!*anyopaque {
        if (bytes == 0) return self.next();

        // Calculate padding needed for alignment
        const current_addr = @intFromPtr(self.next());
        const alignment_bytes = alignment.toByteUnits();

        // If this ever fails, either we have a bug or else max_roc_alignment must be increased!
        std.debug.assert(alignment_bytes <= collections.max_roc_alignment.toByteUnits());

        const aligned_addr = std.mem.alignForward(usize, current_addr, alignment_bytes);
        const padding = @as(u32, @intCast(aligned_addr - current_addr));

        // Check for overflow when adding padding
        const bytes_with_padding, const padding_overflowed = @addWithOverflow(bytes, padding);
        if (padding_overflowed != 0) {
            return StackOverflow.StackOverflow;
        }

        // Check for overflow when adding to used
        const new_used, const used_overflowed = @addWithOverflow(self.used, bytes_with_padding);
        if (used_overflowed != 0 or new_used > self.capacity) {
            return StackOverflow.StackOverflow;
        }

        // Advance the pointer
        const result = self.start + self.used + padding;
        self.used = new_used;

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
    pub fn available(self: *const Stack) u32 {
        return self.capacity - self.used;
    }
};
