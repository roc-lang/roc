//! Minimal ABI views of Roc values for the freestanding
//! default-platform runtimes.
//!
//! `c_runtime.zig` and `linux_runtime.zig` are compiled as standalone objects
//! (freestanding, no libc, no compiler-rt) without the `builtins` module, so
//! they cannot import the canonical builtins types directly. The builtins tests
//! assert these views stay byte-for-byte identical to their canonical structs.

const seamless_slice_tag: usize = 1;

pub const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,
    capacity_or_alloc_ptr: usize,

    pub fn empty() RocList {
        return .{ .bytes = null, .length = 0, .capacity_or_alloc_ptr = 0 };
    }
};

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
