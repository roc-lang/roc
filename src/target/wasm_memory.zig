//! The sizes a WebAssembly module's linear memory and stack can be configured
//! to. A platform header's targets section and the `roc build` flags that
//! override it accept exactly these sizes, so every size that reaches a link
//! is one each linker can express and lay out the same way.

/// Linear memory is sized in pages of this many bytes.
pub const page_size: u64 = 64 * 1024;
/// Linear memory is at most 4 GiB.
pub const limit: u64 = 1 << 32;
/// The stack pointer stays a multiple of this many bytes, so a stack's size is one too.
pub const stack_alignment: u64 = 16;

/// The sizes a setting accepts: the multiples of `multiple_of` from `min` to `max`.
pub const SizeRule = struct {
    min: u64,
    max: u64,
    multiple_of: u64,

    pub fn accepts(self: SizeRule, bytes: u64) bool {
        return bytes >= self.min and bytes <= self.max and bytes % self.multiple_of == 0;
    }
};

/// An initial or maximum memory size: a whole number of pages, and at least
/// one, since neither linker can be told to declare a memory of no pages.
pub const memory_size: SizeRule = .{ .min = page_size, .max = limit, .multiple_of = page_size };

/// A stack size. The largest leaves the stack's alignment free below 4 GiB,
/// the least a module that keeps anything else in memory needs.
pub const stack_size: SizeRule = .{ .min = stack_alignment, .max = limit - stack_alignment, .multiple_of = stack_alignment };

test "size rules accept whole multiples within their bounds" {
    const std = @import("std");
    try std.testing.expect(memory_size.accepts(page_size));
    try std.testing.expect(memory_size.accepts(limit));
    try std.testing.expect(!memory_size.accepts(0));
    try std.testing.expect(!memory_size.accepts(page_size + 1));
    try std.testing.expect(!memory_size.accepts(limit + page_size));
    try std.testing.expect(stack_size.accepts(16));
    try std.testing.expect(stack_size.accepts(limit - 16));
    try std.testing.expect(!stack_size.accepts(0));
    try std.testing.expect(!stack_size.accepts(24));
    try std.testing.expect(!stack_size.accepts(limit));
}
