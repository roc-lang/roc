//! Minimal read-only view of `builtins.str.RocStr` for the freestanding
//! default-platform runtimes.
//!
//! `c_runtime.zig` and `linux_runtime.zig` are compiled as standalone objects
//! (freestanding, no libc, no compiler-rt) without the `builtins` module, so
//! they cannot import `builtins.str.RocStr` directly the way `echo_platform` and
//! the glue host do. This file is the one place the host-boundary `RocStr`
//! encoding those runtimes depend on is written down. `src/builtins/str.zig`
//! carries a test asserting this view's size, alignment, and field layout match
//! the canonical `RocStr`, so the two definitions cannot drift.

const seamless_slice_tag: usize = 1;

/// Read-only view over a host-boundary `RocStr`, exposing only the byte access
/// and reference-count release that the default-platform runtimes need. The
/// field layout mirrors `builtins.str.RocStr`.
pub const RocStr = extern struct {
    bytes: ?[*]u8,
    capacity_or_alloc_ptr: usize,
    length: usize,

    fn isSmallStr(self: RocStr) bool {
        return @as(isize, @bitCast(self.length)) < 0;
    }

    fn isSeamlessSlice(self: RocStr) bool {
        return !self.isSmallStr() and (self.capacity_or_alloc_ptr & seamless_slice_tag) == seamless_slice_tag;
    }

    fn len(self: RocStr) usize {
        if (self.isSmallStr()) {
            const raw: *const [@sizeOf(RocStr)]u8 = @ptrCast(&self);
            return raw.*[@sizeOf(RocStr) - 1] ^ 0b1000_0000;
        }
        return self.length;
    }

    fn allocationPtr(self: RocStr) ?[*]u8 {
        if (self.isSmallStr()) return null;
        if (self.isSeamlessSlice()) {
            return @ptrFromInt(self.capacity_or_alloc_ptr & ~seamless_slice_tag);
        }
        return self.bytes;
    }

    /// The string's bytes, borrowed for the lifetime of the view.
    pub fn asSlice(self: *const RocStr) []const u8 {
        const ptr: [*]const u8 = if (self.isSmallStr())
            @ptrCast(self)
        else
            @ptrCast(self.bytes.?);
        return ptr[0..self.len()];
    }

    /// Release one reference to the string, freeing the backing allocation with
    /// `deallocFn` (the runtime's `roc_dealloc`) when the last reference drops.
    /// Small strings and static-lifetime allocations (refcount 0) are no-ops.
    pub fn decref(self: *RocStr, deallocFn: *const fn (*anyopaque, usize) callconv(.c) void) void {
        const data = self.allocationPtr() orelse return;
        const refcount_ptr: *isize = @ptrCast(@alignCast(data - @sizeOf(usize)));
        const refcount = refcount_ptr.*;
        if (refcount == 0) return;

        const last = @atomicRmw(isize, refcount_ptr, .Sub, 1, .monotonic);
        if (last == 1) {
            deallocFn(data - @sizeOf(usize), @alignOf(usize));
        }
    }
};
