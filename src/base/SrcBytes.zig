//! Roc code source bytes, in the format our tokenizer requires - namely:
//! 1. The total number of bytes fits in an u31 (so, 2GiB max)
//! 2. The base address of the bytes is aligned to 64B
//! 3. The bytes end in a newline (for tokenizing perf) followed by 15 zeros (for SIMD)

const std = @import("std");
const Allocator = std.mem.Allocator;

const SrcBytes = @This();

// All source bytes must have this alignment and suffix.
//
// The alignment lets us do SIMD on it starting from index 0. The suffix having a newline means
// we can do newline-terminted tokenization logic (e.g. line comments) in a loop that only checks
// for newlines and doesn't need to separately check for out-of-bounds, because we'll always find a newline.
//
// The trailing zero bytes are for two purposes: one, knowing the file ends in zeros (when zero-bytes
// are defined to not be allowed in valid Roc source code) means that we can use the normal `switch` branch
// on the current byte to detect if the file has ended, rather than needing a separate conditional check.
//
// Having the guarantee that there are always 15 bytes after the last non-EOF character (the newline) means
// that we can safely use 128-bit SIMD operations from start to finish on the source bytes, without needing
// to do any separate checks other than the usual "did we hit a zero byte, which means we're done" check.
//
// (We store the "real" length separately from the zero-terminator marker of EOF, so that if we hit a zero
// byte, we can check if it was at the end of the actual known length; if not, then we know there was an
// invalid zero byte in the middle of the file and can report that. This check only happens once per file.)
pub const alignment = 16;
pub const suffix: [16]u8 = .{ '\n', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

ptr: [*]align(alignment) const u8,
len: u31,

pub fn lenWithoutZeros(self: SrcBytes) usize {
    return @as(usize, @intCast(self.len)) + 1 - suffix.len;
}

/// Get the bytes as a slice
pub fn bytes(self: SrcBytes) []const u8 {
    return self.ptr[0..@as(usize, @intCast(self.len))];
}

pub fn fromSlice(
    slice: [:suffix]align(alignment) const u8,
) error{TooBig}!SrcBytes {
    if (slice.len > @as(usize, @intCast(std.math.maxInt(@sizeOf(SrcBytes.len))))) {
        return error.TooBig;
    }

    @memcpy(slice.ptr[slice.len - suffix.len ..], &suffix);

    return .{ .ptr = slice.ptr, .len = @intCast(slice.len) };
}

pub const Testing = struct {
    src: SrcBytes,

    pub fn initFromSlice(gpa: Allocator, slice: []const u8) (error{TooBig} || Allocator.Error)!SrcBytes.Testing {
        const allocation = try gpa.allocWithOptions(u8, slice.len + suffix.len, alignment, null);

        if (allocation.len > @as(usize, @intCast(std.math.maxInt(u31)))) {
            return error.TooBig;
        }

        @memcpy(allocation[0..slice.len], slice);
        @memcpy(allocation[slice.len..], &suffix);

        return .{ .src = SrcBytes{ .ptr = allocation.ptr, .len = @intCast(allocation.len) } };
    }

    pub fn deinit(self: *SrcBytes.Testing, gpa: Allocator) void {
        gpa.free(self.src.ptr[0..@as(usize, @intCast(self.src.len))]);
    }
};
