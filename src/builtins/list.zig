//! Runtime implementation of Roc's List type with reference counting and memory optimization.
//!
//! Lists use copy-on-write semantics to minimize allocations when shared across contexts.
//! Seamless slice optimization reduces memory overhead for substring operations.
const std = @import("std");

const builtin = @import("builtin");
const utils = @import("utils.zig");
const UpdateMode = utils.UpdateMode;
const TestEnv = utils.TestEnv;
const RocOps = @import("host_abi.zig").RocOps;
const RocStr = @import("str.zig").RocStr;
const erased_callable = @import("erased_callable.zig");
const sort = @import("sort.zig");

/// Pointer to the bytes of a list element or similar data
pub const Opaque = ?[*]u8;
/// Function copying data between 2 Opaques with a slot for the element's width
pub const CopyFallbackFn = *const fn (Opaque, Opaque, usize) callconv(.c) void;

/// Retains one list element through an opaque layout-specific context.
pub const Inc = *const fn (?*anyopaque, ?[*]u8) callconv(.c) void;
/// Releases one list element through an opaque layout-specific context.
pub const Dec = *const fn (?*anyopaque, ?[*]u8) callconv(.c) void;

/// The low bit tags whether a List is a seamless slice.
pub const SEAMLESS_SLICE_TAG: usize = 1;
/// Runtime representation of Roc's List type with reference counting and seamless slice optimization.
pub const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,
    // For normal lists, contains the capacity shifted left by one.
    // For seamless slices contains the pointer to the original allocation with the low bit set.
    // This pointer is to the first element of the original list.
    capacity_or_alloc_ptr: usize,

    /// Number of pointer-sized words in a RocList's in-memory layout. The layout
    /// is target-width parameterized: its byte size is `word_count` times the
    /// target pointer width. Sites that build a RocList for a target of a
    /// different width than the host multiply this by the target word size.
    pub const word_count = 3;

    /// Big-list capacities are stored shifted left by this many bits so the low
    /// bit stays free for the seamless-slice tag.
    pub const capacity_shift = 1;

    comptime {
        std.debug.assert(word_count * @sizeOf(usize) == @sizeOf(RocList));
    }

    fn encodeCapacityGeneric(comptime T: type, capacity: T) T {
        return capacity << capacity_shift;
    }

    pub inline fn encodeCapacity(capacity: usize) usize {
        return encodeCapacityGeneric(usize, capacity);
    }

    /// Encode a big-list capacity for a target whose pointer width may differ
    /// from the host's. Applies the same shift as the host-width
    /// `encodeCapacity`, but on a `u64` so it can hold any target word's value.
    pub fn encodeCapacityForWidth(capacity: u64) u64 {
        return encodeCapacityGeneric(u64, capacity);
    }

    pub inline fn decodeCapacity(encoded_capacity: usize) usize {
        return encoded_capacity >> capacity_shift;
    }

    pub inline fn encodeSliceAllocationPtr(alloc_ptr: [*]u8) usize {
        return @intFromPtr(alloc_ptr) | SEAMLESS_SLICE_TAG;
    }

    pub inline fn decodeSliceAllocationPtr(encoded_alloc_ptr: usize) usize {
        return encoded_alloc_ptr & ~SEAMLESS_SLICE_TAG;
    }

    /// Returns the number of elements in the list.
    pub inline fn len(self: RocList) usize {
        return self.length;
    }

    /// Returns the total capacity of the list.
    pub fn getCapacity(self: RocList) usize {
        const list_capacity = decodeCapacity(self.capacity_or_alloc_ptr);
        const slice_capacity = self.length;
        const slice_mask = self.seamlessSliceMask();
        const capacity = (list_capacity & ~slice_mask) | (slice_capacity & slice_mask);
        return capacity;
    }

    /// Returns true if this list is a seamless slice.
    pub fn isSeamlessSlice(self: RocList) bool {
        return (self.capacity_or_alloc_ptr & SEAMLESS_SLICE_TAG) == SEAMLESS_SLICE_TAG;
    }

    // This returns all ones if the list is a seamless slice.
    // Otherwise, it returns all zeros.
    // This is done without branching for optimization purposes.
    pub fn seamlessSliceMask(self: RocList) usize {
        return 0 -% (self.capacity_or_alloc_ptr & SEAMLESS_SLICE_TAG);
    }

    pub fn isEmpty(self: RocList) bool {
        return self.len() == 0;
    }

    pub fn empty() RocList {
        return RocList{ .bytes = null, .length = 0, .capacity_or_alloc_ptr = 0 };
    }

    pub fn fromSlice(
        comptime T: type,
        slice: []const T,
        elements_refcounted: bool,
        roc_ops: *RocOps,
    ) RocList {
        if (slice.len == 0) {
            return RocList.empty();
        }

        const list = list_allocate(@alignOf(T), slice.len, @sizeOf(T), elements_refcounted, roc_ops);

        if (slice.len > 0) {
            const dest = list.bytes orelse unreachable;
            const src = @as([*]const u8, @ptrCast(slice.ptr));
            const num_bytes = slice.len * @sizeOf(T);

            @memcpy(dest[0..num_bytes], src[0..num_bytes]);
        }

        return list;
    }

    // returns a pointer to the original allocation.
    // This pointer points to the first element of the allocation.
    // The pointer is to just after the refcount.
    // For big lists, it just returns their bytes pointer.
    // For seamless slices, it returns the pointer stored in capacity_or_alloc_ptr.
    pub fn getAllocationDataPtr(self: RocList, _: *RocOps) ?[*]u8 {
        const list_alloc_ptr = @intFromPtr(self.bytes);
        const slice_alloc_ptr = decodeSliceAllocationPtr(self.capacity_or_alloc_ptr);
        const slice_mask = self.seamlessSliceMask();
        const alloc_ptr = (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);

        return @as(?[*]u8, @ptrFromInt(alloc_ptr));
    }

    // Returns the number of elements to decref when freeing this list's allocation.
    // For seamless slices with refcounted elements, this reads the original allocation size from the heap.
    // For non-refcounted elements or non-slices, just returns the list length.
    pub fn getAllocationElementCount(self: RocList, elements_refcounted: bool, roc_ops: *RocOps) usize {
        // Only read from heap (-2) for seamless slices with refcounted elements.
        // The count is only written by setAllocationElementCount when elements_refcounted=true.
        if (self.isSeamlessSlice() and elements_refcounted) {
            // Seamless slices always refer to an underlying allocation.
            const alloc_ptr = self.getAllocationDataPtr(roc_ops) orelse unreachable;
            // - 1 is refcount.
            // - 2 is size on heap.
            const ptr: [*]usize = utils.alignedPtrCast([*]usize, alloc_ptr, @src());
            return (ptr - 2)[0];
        } else {
            return self.length;
        }
    }

    // This needs to be called when creating seamless slices from unique list.
    // It will put the allocation size on the heap to enable the seamless slice to free the underlying allocation.
    fn setAllocationElementCount(self: RocList, elements_refcounted: bool, roc_ops: *RocOps) void {
        if (elements_refcounted and !self.isSeamlessSlice()) {
            if (self.getAllocationDataPtr(roc_ops)) |alloc_ptr| {
                // - 1 is refcount.
                // - 2 is size on heap.
                const ptr: [*]usize = utils.alignedPtrCast([*]usize, alloc_ptr, @src());
                (ptr - 2)[0] = self.length;
            }
        }
    }

    /// Increments the list's refcount using the given count-update atomicity.
    pub fn increfWithAtomicity(self: RocList, amount: isize, elements_refcounted: bool, atomicity: utils.RcAtomicity, roc_ops: *RocOps) void {
        // Seamless slices of refcounted lists need the original allocation's element
        // count recorded in the heap header. Once a non-slice list becomes shared,
        // that count must already be present because later slice teardown will read it
        // from the shared allocation.
        // This writes whole-allocation element count metadata before a normal
        // list becomes shared. Slices already point at an allocation whose
        // teardown metadata must have been established by the original owner.
        if (elements_refcounted and self.canReuseAllocation(.Immutable, roc_ops)) {
            if (self.getAllocationDataPtr(roc_ops)) |source| {
                // - 1 is refcount.
                // - 2 is size on heap.
                const ptr: [*]usize = utils.alignedPtrCast([*]usize, source, @src());
                (ptr - 2)[0] = self.length;
            }
        }
        utils.increfDataPtr(self.getAllocationDataPtr(roc_ops), amount, atomicity, roc_ops);
    }

    /// Increments the list's refcount with atomic count updates.
    pub fn incref(self: RocList, amount: isize, elements_refcounted: bool, roc_ops: *RocOps) void {
        self.increfWithAtomicity(amount, elements_refcounted, .atomic, roc_ops);
    }

    /// Walk every element in the list's backing allocation and apply `dec` to
    /// each. This is the single definition of the "a dying unique refcounted
    /// list decrefs its children first" traversal: the compiled `decref` path
    /// and the interpreter's list teardown both route through it, so they
    /// cannot disagree about which elements are visited. For seamless slices,
    /// `getAllocationElementCount` reads the heap-stored whole-allocation
    /// element count, so teardown visits elements outside the visible slice
    /// window too. Callers own the uniqueness gate before invoking this.
    pub fn decrefElements(
        self: RocList,
        element_width: usize,
        dec_context: ?*anyopaque,
        dec: Dec,
        roc_ops: *RocOps,
    ) void {
        if (self.getAllocationDataPtr(roc_ops)) |source| {
            const count = self.getAllocationElementCount(true, roc_ops);

            var i: usize = 0;
            while (i < count) : (i += 1) {
                const element = source + i * element_width;
                dec(dec_context, element);
            }
        }
    }

    /// Always uses atomic count updates: this entry serves primitive-internal
    /// RC inside runtime-checked list ops, which serve both modes and make no
    /// thread-confinement claim. Single-thread statement teardown instead goes
    /// through `roc_builtins_list_decref_with_single_thread` (whose caller
    /// passes element callbacks matching the statement's atomicity) or the
    /// interpreter's `decrefListElements`.
    pub fn decref(
        self: RocList,
        alignment: u32,
        element_width: usize,
        elements_refcounted: bool,
        dec_context: ?*anyopaque,
        dec: Dec,
        roc_ops: *RocOps,
    ) void {
        // If unique, decref will free the list. Before that happens, all elements must be decremented.
        if (elements_refcounted and self.isUnique(roc_ops)) {
            self.decrefElements(element_width, dec_context, dec, roc_ops);
        }

        // We use the raw capacity to ensure we always decrement the refcount of seamless slices.
        utils.decref(
            self.getAllocationDataPtr(roc_ops),
            self.capacity_or_alloc_ptr,
            alignment,
            elements_refcounted,
            .atomic,
            roc_ops,
        );
    }

    fn decrefAfterMovingSliceElements(
        self: RocList,
        alignment: u32,
        element_width: usize,
        elements_refcounted: bool,
        dec_context: ?*anyopaque,
        dec: Dec,
        roc_ops: *RocOps,
    ) void {
        std.debug.assert(self.isSeamlessSlice());
        std.debug.assert(self.isUnique(roc_ops));

        if (elements_refcounted) {
            const alloc_ptr = self.getAllocationDataPtr(roc_ops) orelse unreachable;
            const slice_ptr = self.bytes orelse unreachable;
            std.debug.assert(element_width > 0);

            const moved_start_bytes = @intFromPtr(slice_ptr) - @intFromPtr(alloc_ptr);
            std.debug.assert(moved_start_bytes % element_width == 0);

            const moved_start = moved_start_bytes / element_width;
            const moved_end = moved_start + self.len();
            const count = self.getAllocationElementCount(true, roc_ops);
            std.debug.assert(moved_end <= count);

            var i: usize = 0;
            while (i < moved_start) : (i += 1) {
                dec(dec_context, alloc_ptr + i * element_width);
            }
            i = moved_end;
            while (i < count) : (i += 1) {
                dec(dec_context, alloc_ptr + i * element_width);
            }
        }

        utils.decref(
            self.getAllocationDataPtr(roc_ops),
            self.capacity_or_alloc_ptr,
            alignment,
            elements_refcounted,
            .atomic,
            roc_ops,
        );
    }

    pub fn elements(self: RocList, comptime T: type) ?[*]T {
        if (self.bytes) |bytes| {
            return utils.alignedPtrCast([*]T, bytes, @src());
        }
        return null;
    }

    pub fn isUnique(self: RocList, roc_ops: *RocOps) bool {
        return utils.rcUnique(@bitCast(self.refcount(roc_ops)));
    }

    /// Returns true when this value is the only live reference to its allocation,
    /// either because the caller proved that statically (`.InPlace`) or because
    /// the runtime refcount is 1. This permits consuming the value and mutating
    /// elements inside its visible window, but it does not mean the allocation
    /// can be resized, freed with partial element teardown, or reused wholesale:
    /// a seamless slice may be exclusive while still pointing into a larger
    /// allocation. Empty lists are vacuously exclusive because they have no
    /// allocation and no other possible owner.
    pub inline fn isExclusive(self: RocList, update_mode: UpdateMode, roc_ops: *RocOps) bool {
        if (update_mode == .InPlace) {
            // `.InPlace` is the compiler's claim that nothing else holds this
            // allocation, and it is the one path where the count is never
            // consulted. A wrong claim writes through a shared allocation and
            // corrupts whatever else points at it, with nothing downstream able
            // to notice. Debug builds hold the claim against the runtime truth
            // so a mistaken proof fails a test instead of silently corrupting
            // memory.
            if (comptime builtin.mode == .Debug) {
                if (!self.isUnique(roc_ops)) {
                    roc_ops.crash("List written in place while another reference to it was live");
                }
            }
            return true;
        }
        return self.isUnique(roc_ops);
    }

    /// Returns true when treating this value's allocation as exclusively owned
    /// by the visible list is safe. For seamless slices, refcount 1 means sole
    /// ownership of the whole backing allocation, not ownership of only the
    /// slice window, so slices must not take allocation-reuse paths. Empty lists
    /// return true vacuously; callers already guard on `bytes` before touching
    /// memory.
    pub inline fn canReuseAllocation(self: RocList, update_mode: UpdateMode, roc_ops: *RocOps) bool {
        return !self.isSeamlessSlice() and self.isExclusive(update_mode, roc_ops);
    }

    fn refcount(self: RocList, roc_ops: *RocOps) usize {
        // Reduced debug output - only print on potential issues
        if (self.getCapacity() == 0 and !self.isSeamlessSlice()) {
            // the zero-capacity is Clone, copying it will not leak memory
            return 1;
        }

        if (self.getAllocationDataPtr(roc_ops)) |non_null_ptr| {
            const ptr: [*]usize = utils.alignedPtrCast([*]usize, non_null_ptr, @src());
            return (ptr - 1)[0];
        } else {
            unreachable;
        }
    }

    pub fn makeUnique(
        self: RocList,
        alignment: u32,
        element_width: usize,
        elements_refcounted: bool,
        inc_context: ?*anyopaque,
        inc: Inc,
        dec_context: ?*anyopaque,
        dec: Dec,
        roc_ops: *RocOps,
    ) RocList {
        // `makeUnique` guarantees a refcount-1 value that is safe for in-window
        // element writes. It intentionally may return a seamless slice; callers
        // that need to resize or reuse the allocation must use
        // canReuseAllocation/reallocate instead.
        if (self.isUnique(roc_ops)) {
            return self;
        }

        if (self.isEmpty()) {
            // Empty is not necessarily unique on it's own.
            // The list could have capacity and be shared.
            self.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
            return RocList.empty();
        }

        // unfortunately, we have to clone
        const new_list = RocList.list_allocate(alignment, self.length, element_width, elements_refcounted, roc_ops);

        var old_bytes: [*]u8 = @as([*]u8, @ptrCast(self.bytes));
        var new_bytes: [*]u8 = @as([*]u8, @ptrCast(new_list.bytes));

        const number_of_bytes = self.len() * element_width;
        @memcpy(new_bytes[0..number_of_bytes], old_bytes[0..number_of_bytes]);

        // Increment refcount of all elements now in a new list.
        if (elements_refcounted) {
            var i: usize = 0;
            while (i < self.len()) : (i += 1) {
                inc(inc_context, new_bytes + i * element_width);
            }
        }

        self.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);

        return new_list;
    }

    pub fn list_allocate(
        elem_alignment: u32,
        length: usize,
        element_width: usize,
        elements_refcounted: bool,
        roc_ops: *RocOps,
    ) RocList {
        if (length == 0) {
            return empty();
        }

        return RocList{
            .bytes = utils.allocateWithRefcount(
                length * element_width,
                elem_alignment,
                elements_refcounted,
                roc_ops,
            ),
            .length = length,
            .capacity_or_alloc_ptr = encodeCapacity(length),
        };
    }

    pub fn reallocate(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
        elements_refcounted: bool,
        inc_context: ?*anyopaque,
        inc: Inc,
        dec_context: ?*anyopaque,
        dec: Dec,
        update_mode: UpdateMode,
        roc_ops: *RocOps,
    ) RocList {
        if (self.bytes) |source_ptr| {
            if (self.canReuseAllocation(update_mode, roc_ops)) {
                const capacity = decodeCapacity(self.capacity_or_alloc_ptr);
                if (capacity >= new_length) {
                    const result = RocList{ .bytes = self.bytes, .length = new_length, .capacity_or_alloc_ptr = self.capacity_or_alloc_ptr };
                    return result;
                } else {
                    const new_source = utils.unsafeReallocate(source_ptr, alignment, capacity, new_length, element_width, elements_refcounted, roc_ops);
                    const result = RocList{ .bytes = new_source, .length = new_length, .capacity_or_alloc_ptr = encodeCapacity(new_length) };
                    return result;
                }
            }
            return self.reallocateFresh(alignment, new_length, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
        }
        return RocList.list_allocate(alignment, new_length, element_width, elements_refcounted, roc_ops);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
        elements_refcounted: bool,
        inc_context: ?*anyopaque,
        inc: Inc,
        dec_context: ?*anyopaque,
        dec: Dec,
        roc_ops: *RocOps,
    ) RocList {
        const old_length = self.length;

        const result = RocList.list_allocate(alignment, new_length, element_width, elements_refcounted, roc_ops);
        // A unique seamless slice can move its visible elements into `result`
        // without inc/dec traffic for those elements. The old backing allocation
        // is then consumed by decrefAfterMovingSliceElements, which decrefs only
        // the out-of-window elements before freeing the raw allocation.
        const move_slice_elements = self.isSeamlessSlice() and self.isUnique(roc_ops);

        if (self.bytes) |source_ptr| {
            // transfer the memory
            const dest_ptr = result.bytes orelse unreachable;

            @memcpy(dest_ptr[0..(old_length * element_width)], source_ptr[0..(old_length * element_width)]);

            if (elements_refcounted and !move_slice_elements) {
                var i: usize = 0;
                while (i < old_length) : (i += 1) {
                    inc(inc_context, dest_ptr + i * element_width);
                }
            }
        }

        if (move_slice_elements) {
            self.decrefAfterMovingSliceElements(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        } else {
            self.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        }

        return result;
    }
};

/// Increment the reference count.
pub fn listIncref(list: RocList, amount: isize, elements_refcounted: bool, roc_ops: *RocOps) callconv(.c) void {
    list.incref(amount, elements_refcounted, roc_ops);
}

/// Decrement reference count and deallocate when no longer shared.
pub fn listDecref(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    dec_context: ?*anyopaque,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.c) void {
    list.decref(
        alignment,
        element_width,
        elements_refcounted,
        dec_context,
        dec,
        roc_ops,
    );
}

/// Create an empty list with pre-allocated capacity to avoid reallocation during growth.
pub fn listWithCapacity(
    capacity: u64,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    return listReserve(
        RocList.empty(),
        alignment,
        capacity,
        element_width,
        elements_refcounted,
        inc_context,
        inc,
        null,
        rcNone,
        .InPlace,
        roc_ops,
    );
}

/// Ensure the list has capacity for additional elements to prevent reallocation.
pub fn listReserve(
    list: RocList,
    alignment: u32,
    spare: u64,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const original_len = list.len();

    const cap = @as(u64, @intCast(list.getCapacity()));

    // A list's length never exceeds its capacity, so the slack subtraction
    // cannot wrap and the hot no-growth check needs no saturating add.
    std.debug.assert(original_len <= cap);
    if (list.isExclusive(update_mode, roc_ops) and spare <= cap - @as(u64, @intCast(original_len))) {
        // For seamless slices, getCapacity() is the visible window length. This
        // branch can therefore only fire for a slice when no growth was
        // requested, so returning the unchanged slice touches no allocation.
        std.debug.assert(!list.isSeamlessSlice() or spare == 0);
        return list;
    } else {
        const desired_cap = @as(u64, @intCast(original_len)) +| spare;
        // Make sure on 32-bit targets we don't accidentally wrap when we cast our U64 desired capacity to U32.
        const reserve_size: u64 = @min(desired_cap, @as(u64, @intCast(std.math.maxInt(usize))));

        var output = list.reallocate(
            alignment,
            @as(usize, @intCast(reserve_size)),
            element_width,
            elements_refcounted,
            inc_context,
            inc,
            dec_context,
            dec,
            update_mode,
            roc_ops,
        );
        output.length = original_len;
        return output;
    }
}

/// The capacity to give `list` so it holds `needed` elements while keeping a
/// run of growing operations on it amortized-linear. An allocation that
/// already fits is kept as-is; otherwise the result is at least one geometric
/// step. Only an allocation that grows in place carries its slack into future
/// operations, so only that path steps from the capacity. A fresh copy steps
/// from the length instead: its capacity then tracks the elements it holds,
/// rather than compounding on every operation on a list that stays shared.
fn amortizedCapacity(list: RocList, needed: usize, element_width: usize, update_mode: UpdateMode, roc_ops: *RocOps) usize {
    const reuses_allocation = list.canReuseAllocation(update_mode, roc_ops);
    const capacity = list.getCapacity();
    if (reuses_allocation and needed <= capacity) return needed;
    const growth_base: usize = if (reuses_allocation) capacity else list.len();
    return @max(needed, utils.geometricGrowth(growth_base, element_width));
}

/// Ensure capacity for `spare` more elements ahead of an append. Unlike
/// `listReserve`—the explicit user reserve, which trusts the request and
/// sizes the allocation exactly—growth here takes at least the geometric
/// step, so a loop of appends stays amortized-linear instead of reallocating
/// on every call once the list runs tight.
pub fn listReserveForAppend(
    list: RocList,
    alignment: u32,
    spare: u64,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const original_len = list.len();
    const cap = @as(u64, @intCast(list.getCapacity()));

    std.debug.assert(original_len <= cap);
    if (list.isExclusive(update_mode, roc_ops) and spare <= cap - @as(u64, @intCast(original_len))) {
        std.debug.assert(!list.isSeamlessSlice() or spare == 0);
        return list;
    }

    const needed = @as(u64, @intCast(original_len)) +| spare;
    const clamped: usize = @intCast(@min(needed, @as(u64, @intCast(std.math.maxInt(usize)))));

    var output = list.reallocate(
        alignment,
        amortizedCapacity(list, clamped, element_width, update_mode, roc_ops),
        element_width,
        elements_refcounted,
        inc_context,
        inc,
        dec_context,
        dec,
        update_mode,
        roc_ops,
    );
    output.length = original_len;
    return output;
}

/// Append `count` elements copied from the list itself beginning at `start`.
/// The copy reads through its own freshly appended elements, so a range past
/// the original end repeats the elements from `start` onward. The caller has
/// already verified `start < list.len()` and `count > 0`.
pub fn listAppendRangeWithin(
    list: RocList,
    start_u64: u64,
    count_u64: u64,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const start: usize = @intCast(start_u64);
    const count: usize = @intCast(@min(count_u64, @as(u64, @intCast(std.math.maxInt(usize)))));

    // Reserve scratch beyond the appended range so every copy below may run
    // in bursts of whole-word stores that overshoot the range. The scratch
    // stays within capacity and outside the length.
    const slop_elements: u64 = (append_range_within_scratch_bytes + element_width - 1) / element_width;
    var output = listReserveForAppend(
        list,
        alignment,
        count_u64 +| slop_elements,
        element_width,
        elements_refcounted,
        inc_context,
        inc,
        dec_context,
        dec,
        update_mode,
        roc_ops,
    );
    appendRangeWithinCore(&output, start, count, element_width, elements_refcounted, inc_context, inc);
    return output;
}

/// The bytes past an appended range that a range-within append's whole-word
/// stores may overshoot into, which its capacity must cover beyond the range.
pub const append_range_within_scratch_bytes: usize = 40;

/// Append `count` elements copied from the list itself beginning at `start`,
/// with every check already discharged by the caller: the list uniquely owns
/// a non-slice allocation whose capacity covers the appended range plus the
/// word-copy scratch. The loop-append promotion pass emits this on its hot
/// path after proving exactly those facts through its slack counter.
pub fn listAppendRangeWithinUnsafe(
    list: RocList,
    start_u64: u64,
    count_u64: u64,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const start: usize = @intCast(start_u64);
    const count: usize = @intCast(count_u64);

    std.debug.assert(!list.isSeamlessSlice());
    std.debug.assert(list.isUnique(roc_ops));
    std.debug.assert(list.getCapacity() * element_width >=
        (list.len() + count) * element_width + append_range_within_scratch_bytes);

    var output = list;
    appendRangeWithinCore(&output, start, count, element_width, elements_refcounted, inc_context, inc);
    return output;
}

/// Copy `count` elements within the list from `src_index` onward to
/// `dest_index` onward, overwriting the destination range. The caller has
/// already verified both whole ranges lie inside the list. The ranges may
/// overlap; every source element is read as it was before any destination
/// element was overwritten, like a memmove.
pub fn listCopyRangeWithin(
    list: RocList,
    dest_index_u64: u64,
    src_index_u64: u64,
    count_u64: u64,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const dest_index: usize = @intCast(dest_index_u64);
    const src_index: usize = @intCast(src_index_u64);
    const count: usize = @intCast(count_u64);
    if (count == 0 or element_width == 0 or dest_index == src_index) return list;

    const output = list.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
    const base = output.bytes.?;

    if (elements_refcounted) {
        // Retain every copied element before releasing any overwritten one,
        // so an element present in both ranges never reaches a zero count
        // mid-copy. The overwritten elements must be released before the
        // move clobbers their bytes.
        var i: usize = 0;
        while (i < count) : (i += 1) {
            inc(inc_context, base + (src_index + i) * element_width);
        }
        i = 0;
        while (i < count) : (i += 1) {
            dec(dec_context, base + (dest_index + i) * element_width);
        }
    }

    const total = count * element_width;
    const src = base[src_index * element_width ..][0..total];
    const dest = base[dest_index * element_width ..][0..total];
    @memmove(dest, src);
    return output;
}

/// The copy-and-lengthen half of a range-within append. The capacity for the
/// range plus the overshoot scratch is already reserved.
inline fn appendRangeWithinCore(
    output: *RocList,
    start: usize,
    count: usize,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
) void {
    const original_len = output.len();
    const base = output.bytes.?;

    var src = base + start * element_width;
    var dst = base + original_len * element_width;
    const distance = (original_len - start) * element_width;
    const total = count * element_width;
    const end = dst + total;
    if (distance >= 8) {
        // A word read at offset i touches src[i..i+8), which stays at or
        // behind the write cursor, so every byte read is already
        // materialized. An unconditional five-word burst covers most ranges
        // without a branch; longer ranges continue in five-word strides.
        inline for (0..5) |_| {
            dst[0..8].* = src[0..8].*;
            src += 8;
            dst += 8;
        }
        if (@intFromPtr(dst) < @intFromPtr(end)) {
            // Only ranges longer than the burst reach here, so this branch
            // costs short copies nothing. Distances of sixteen or more
            // stream through vector registers; shorter distances stay on
            // word stores, whose reads overlap the just-written words
            // exactly and forward without stalling, where a doubled-reach
            // vector read would overlap them partially and stall.
            if (distance >= 16) {
                while (@intFromPtr(dst) < @intFromPtr(end)) {
                    dst[0..16].* = src[0..16].*;
                    dst[16..32].* = src[16..32].*;
                    dst[32..40].* = src[32..40].*;
                    src += 40;
                    dst += 40;
                }
            } else {
                while (@intFromPtr(dst) < @intFromPtr(end)) {
                    inline for (0..5) |_| {
                        dst[0..8].* = src[0..8].*;
                        src += 8;
                        dst += 8;
                    }
                }
            }
        }
    } else if (distance == 1) {
        // A run of one repeated byte: broadcast it and store whole words,
        // no loads at all.
        const v: u64 = @as(u64, 0x0101010101010101) *% src[0];
        inline for (0..4) |_| {
            dst[0..8].* = @bitCast(v);
            dst += 8;
        }
        while (@intFromPtr(dst) < @intFromPtr(end)) {
            inline for (0..4) |_| {
                dst[0..8].* = @bitCast(v);
                dst += 8;
            }
        }
    } else {
        // The range repeats with a period of 2-7 bytes. First materialize a
        // whole word of repeats with stores that advance by the period: each
        // store writes a full word but only its leading `distance` bytes are
        // final, so bytes before the cursor never change once written. The
        // trailing garbage of the last store sits at or past the cursor and
        // is overwritten below or left in the slop.
        var materialized: usize = 0;
        while (materialized < 8) {
            dst[0..8].* = src[0..8].*;
            src += distance;
            dst += distance;
            materialized += distance;
        }
        // Everything before the cursor is now final and repeats with a
        // word-sized period, so the rest runs in the same five-word bursts
        // as the long-distance case, reading one period multiple back.
        if (@intFromPtr(dst) < @intFromPtr(end)) {
            src = dst - materialized;
            while (true) {
                inline for (0..5) |_| {
                    dst[0..8].* = src[0..8].*;
                    src += 8;
                    dst += 8;
                }
                if (@intFromPtr(dst) >= @intFromPtr(end)) break;
            }
        }
    }

    if (elements_refcounted) {
        var i: usize = 0;
        while (i < count) : (i += 1) {
            inc(inc_context, base + (original_len + i) * element_width);
        }
    }

    output.length = original_len + count;
}

/// Append `len` elements of `src` beginning at `start` to `list`. The caller
/// has already clamped the range to `src`'s length. `src` is borrowed: its
/// refcount is untouched, and copied refcounted elements gain a reference.
pub fn listAppendSublist(
    list: RocList,
    src: RocList,
    start_u64: u64,
    len_u64: u64,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const count: usize = @intCast(len_u64);
    if (count == 0) return list;
    const original_len = list.len();
    const start: usize = @intCast(start_u64);

    var output = listReserveForAppend(
        list,
        alignment,
        len_u64,
        element_width,
        elements_refcounted,
        inc_context,
        inc,
        dec_context,
        dec,
        update_mode,
        roc_ops,
    );
    const base = output.bytes.?;
    // When the source aliases the destination, the reserve above may have
    // moved (and freed) the destination's old allocation. The old contents
    // survive as the output's prefix, so read the range from there.
    const src_ptr = if (src.bytes == list.bytes) base else src.bytes.?;
    @memcpy(
        (base + original_len * element_width)[0 .. count * element_width],
        (src_ptr + start * element_width)[0 .. count * element_width],
    );

    if (elements_refcounted) {
        var i: usize = 0;
        while (i < count) : (i += 1) {
            inc(inc_context, base + (original_len + i) * element_width);
        }
    }

    output.length = original_len + count;
    return output;
}

/// The number of elements that can be appended in place without any further
/// ownership or capacity check: capacity minus length when this list uniquely
/// owns a non-slice allocation, and zero otherwise (so the caller's next
/// append takes the checked path, which clones or grows as needed).
/// One when this list uniquely owns a non-slice allocation, so element
/// overwrites may skip their per-call ownership check; zero otherwise.
pub fn listOwnedUnique(
    list: RocList,
    roc_ops: *RocOps,
) callconv(.c) u64 {
    if (list.isSeamlessSlice()) return 0;
    if (list.bytes == null) return 0;
    if (!list.isUnique(roc_ops)) return 0;
    return 1;
}

/// Number of elements appendable in place without any further ownership or
/// capacity check: the unused capacity when the list is uniquely owned and
/// not a seamless slice, zero otherwise.
pub fn listSlackUnique(
    list: RocList,
    roc_ops: *RocOps,
) callconv(.c) u64 {
    if (list.isSeamlessSlice()) return 0;
    if (!list.isUnique(roc_ops)) return 0;
    return @intCast(list.getCapacity() - list.len());
}

/// Append the low `count` bytes of `value` to a byte list, least significant
/// byte first. The caller has already verified `count <= 8`. One uniqueness
/// and capacity check covers the whole write.
pub fn listAppendLeBytes(
    list: RocList,
    value: u64,
    count_u64: u64,
    alignment: u32,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const count: usize = @intCast(count_u64);
    if (count == 0) return list;
    const original_len = list.len();

    // A unique list with a full word of spare capacity takes one unaligned
    // little-endian store and a length bump: the low `count` bytes become the
    // appended data and the rest of the word lands in capacity slack, which
    // nothing can observe. Bit-writer flushes hit this path on every call.
    if (!list.isSeamlessSlice() and list.getCapacity() >= original_len + 8 and
        list.isExclusive(update_mode, roc_ops))
    {
        if (list.bytes) |bytes| {
            std.mem.writeInt(u64, bytes[original_len..][0..8], value, .little);
            var output = list;
            output.length = original_len + count;
            return output;
        }
    }

    var output = listReserveForAppend(
        list,
        alignment,
        count_u64,
        1,
        false,
        null,
        @ptrCast(&utils.rcNone),
        null,
        @ptrCast(&utils.rcNone),
        update_mode,
        roc_ops,
    );
    const base = output.bytes.?;
    var i: usize = 0;
    var word = value;
    while (i < count) : (i += 1) {
        base[original_len + i] = @truncate(word);
        word >>= 8;
    }
    output.length = original_len + count;
    return output;
}

/// Reduce memory usage by trimming unused capacity when list has shrunk significantly.
pub fn listReleaseExcessCapacity(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const old_length = list.len();

    if (list.canReuseAllocation(update_mode, roc_ops) and list.getCapacity() == old_length) {
        return list;
    } else if (old_length == 0) {
        list.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        return RocList.empty();
    } else {
        // TODO: This can be made more efficient, but has to work around the `decref`.
        // If the list is unique, we can avoid incrementing and decrementing the live items.
        // We can just decrement the dead elements and free the old list.
        // This pattern is also like true in other locations like listConcat and listDropAt.
        const output = RocList.list_allocate(alignment, old_length, element_width, elements_refcounted, roc_ops);
        if (list.bytes) |source_ptr| {
            const dest_ptr = output.bytes orelse unreachable;

            @memcpy(dest_ptr[0..(old_length * element_width)], source_ptr[0..(old_length * element_width)]);
            if (elements_refcounted) {
                var i: usize = 0;
                while (i < old_length) : (i += 1) {
                    const element = source_ptr + i * element_width;
                    inc(inc_context, element);
                }
            }
        }
        list.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        return output;
    }
}

/// Add element to end of list. Caller must ensure sufficient capacity exists.
pub fn listAppendUnsafe(
    list: RocList,
    element: Opaque,
    element_width: usize,
    copy: CopyFallbackFn,
) callconv(.c) RocList {
    const old_length = list.len();
    var output = list;
    output.length += 1;

    // The caller has discharged every check: the list uniquely owns an
    // allocation with a spare slot, so the data pointer cannot be null.
    // Zero-sized elements have no bytes to copy (and may pass null).
    if (element_width > 0) {
        const target = output.bytes.? + old_length * element_width;
        copy(target, element.?, element_width);
    }

    return output;
}

/// List.prepend - adds an element to the beginning of a list.
///
/// ## Ownership
/// - `list`: **consumes** - caller loses ownership
/// - `element`: **borrows** - copied into list, caller retains original
/// - Returns: **copy-on-write** - may be same allocation if unique with capacity
///
/// Reserves capacity if needed, shifts existing elements, then inserts element
/// at the front. If the list is unique with sufficient capacity, modifies in
/// place and returns same pointer.
///
/// An `.InPlace` update mode means the caller proved the list unique, so the
/// runtime uniqueness check inside the capacity reservation is skipped.
pub fn listPrepend(
    list: RocList,
    alignment: u32,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    copy: CopyFallbackFn,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    // An exclusive seamless slice whose window starts after the start of its
    // backing allocation owns the slot just before the window, so the new
    // element can go there without moving the window's elements. For
    // refcounted elements that slot still holds a live element which the
    // slice's whole-allocation teardown would otherwise release, so it is
    // released here before being overwritten.
    if (list.isSeamlessSlice() and element_width > 0 and list.isExclusive(update_mode, roc_ops)) {
        const window_ptr = list.bytes orelse unreachable;
        const alloc_ptr = list.getAllocationDataPtr(roc_ops) orelse unreachable;
        if (@intFromPtr(window_ptr) - @intFromPtr(alloc_ptr) >= element_width) {
            const target = window_ptr - element_width;
            if (elements_refcounted) {
                dec(dec_context, target);
            }
            if (element) |source| {
                copy(target, source, element_width);
            }
            var result = list;
            result.bytes = target;
            result.length += 1;
            return result;
        }
    }

    const old_length = list.len();
    var with_capacity = listReserveForAppend(
        list,
        alignment,
        1,
        element_width,
        elements_refcounted,
        inc_context,
        inc,
        dec_context,
        dec,
        update_mode,
        roc_ops,
    );
    with_capacity.length += 1;

    // can't use one memcpy here because source and target overlap
    if (with_capacity.bytes) |target| {
        const from = target;
        const to = target + element_width;
        const size = element_width * old_length;
        std.mem.copyBackwards(u8, to[0..size], from[0..size]);

        // finally copy in the new first element
        if (element) |source| {
            copy(target, source, element_width);
        }
    }

    return with_capacity;
}

/// Exchange elements at two positions within the list.
pub fn listSwap(
    list: RocList,
    alignment: u32,
    element_width: usize,
    index_1: u64,
    index_2: u64,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    copy: CopyFallbackFn,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    // Early exit to avoid swapping the same element.
    if (index_1 == index_2)
        return list;

    const size = @as(u64, @intCast(list.len()));
    if (index_1 == index_2 or index_1 >= size or index_2 >= size) {
        // Either one index was out of bounds, or both indices were the same; just return
        return list;
    }

    const newList = blk: {
        if (update_mode == .InPlace) {
            break :blk list;
        } else {
            break :blk list.makeUnique(
                alignment,
                element_width,
                elements_refcounted,
                inc_context,
                inc,
                dec_context,
                dec,
                roc_ops,
            );
        }
    };

    const source_ptr = @as([*]u8, @ptrCast(newList.bytes));

    swapElements(source_ptr, element_width, @as(usize,
        // We already verified that both indices are less than the stored list length,
        // which is usize, so casting them to usize will definitely be lossless.
        @intCast(index_1)), @as(usize, @intCast(index_2)), copy);

    return newList;
}

/// The `rest` of a numeric prefix parse (`T.from_utf8_prefix`): the bytes of
/// `list` after its first `consumed` bytes.
///
/// ## Ownership
/// - `list`: **borrows** - caller retains ownership
/// - Returns: **owned** - a retained seamless slice of `list`, or empty
pub fn listFromUtf8PrefixRest(list: RocList, consumed: usize, roc_ops: *RocOps) RocList {
    const list_len = list.len();
    std.debug.assert(consumed <= list_len);
    if (consumed == list_len) return RocList.empty();

    list.incref(1, false, roc_ops);
    return listSublistBorrowed(list, @sizeOf(u8), consumed, list_len - consumed, false, roc_ops);
}

/// Construct a sublist view borrowed from `list`.
///
/// This operation never changes a reference count or consumes `list`. ARC
/// keeps `list` live for the complete lifetime of the returned view. For
/// refcounted elements it initializes whole-allocation teardown metadata while
/// the source is exclusive, so a later owned occurrence can safely retain the
/// view. Shared sources already had that metadata initialized before sharing.
pub fn listSublistBorrowed(
    list: RocList,
    element_width: usize,
    start_u64: u64,
    len_u64: u64,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const size = list.len();
    if (size == 0 or len_u64 == 0 or start_u64 >= @as(u64, @intCast(size))) {
        return RocList.empty();
    }

    const source_ptr = list.bytes orelse return RocList.empty();
    const start: usize = @intCast(start_u64);
    const size_minus_start = size - start;
    const keep_len: usize = @intCast(@min(len_u64, @as(u64, @intCast(size_minus_start))));
    if (elements_refcounted and list.canReuseAllocation(.Immutable, roc_ops)) {
        list.setAllocationElementCount(elements_refcounted, roc_ops);
    }
    const list_alloc_ptr = RocList.encodeSliceAllocationPtr(source_ptr);
    const slice_alloc_ptr = list.capacity_or_alloc_ptr;
    const slice_mask = list.seamlessSliceMask();
    const alloc_ptr = (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);

    return .{
        .bytes = source_ptr + start * element_width,
        .length = keep_len,
        .capacity_or_alloc_ptr = alloc_ptr,
    };
}

/// List.sublist - returns a sublist of the given list.
///
/// ## Ownership
/// - `list`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** or **seamless-slice** depending on input
///
/// If list is empty, or sublist range is empty/out-of-bounds:
/// - If unique: shrinks length to 0, returns same allocation
/// - Otherwise: decrefs original, returns empty list
///
/// If sublist starts at index 0 and list is unique:
/// - Shrinks length in place, returns same allocation
///
/// Otherwise: creates a seamless slice pointing into the original allocation.
///
/// An `.InPlace` update mode means the caller proved the list unique, so the
/// runtime uniqueness checks are skipped.
pub fn listSublist(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    start_u64: u64,
    len_u64: u64,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const size = list.len();
    const can_reuse_allocation = list.canReuseAllocation(update_mode, roc_ops);
    if (size == 0 or len_u64 == 0 or start_u64 >= @as(u64, @intCast(size))) {
        if (can_reuse_allocation) {
            // Decrement the reference counts of all elements.
            if (list.bytes) |source_ptr| {
                if (elements_refcounted) {
                    var i: usize = 0;
                    while (i < size) : (i += 1) {
                        const element = source_ptr + i * element_width;
                        dec(dec_context, element);
                    }
                }
            }

            var output = list;
            output.length = 0;
            return output;
        }
        list.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        return RocList.empty();
    }

    if (list.bytes) |source_ptr| {
        // This cast is lossless because we would have early-returned already
        // if `start_u64` were greater than `size`, and `size` fits in usize.
        const start: usize = @intCast(start_u64);

        // (size - start) can't overflow because we would have early-returned already
        // if `start` were greater than `size`.
        const size_minus_start = size - start;

        // This outer cast to usize is lossless. size, start, and size_minus_start all fit in usize,
        // and @min guarantees that if `len_u64` gets returned, it's because it was smaller
        // than something that fit in usize.
        const keep_len = @as(usize, @intCast(@min(len_u64, @as(u64, @intCast(size_minus_start)))));

        if (start == 0 and can_reuse_allocation) {
            // The list is unique, we actually have to decrement refcounts to elements we aren't keeping around.
            // Decrement the reference counts of elements after `start + keep_len`.
            if (elements_refcounted) {
                const drop_end_len = size_minus_start - keep_len;
                var i: usize = 0;
                while (i < drop_end_len) : (i += 1) {
                    const element = source_ptr + (start + keep_len + i) * element_width;
                    dec(dec_context, element);
                }
            }

            var output = list;
            output.length = keep_len;
            return output;
        } else {
            if (can_reuse_allocation) {
                // Store original element count for proper cleanup when the slice is freed.
                // When the seamless slice is later decreffed, it will decref ALL elements
                // starting from the original allocation pointer, not just the slice elements.
                list.setAllocationElementCount(elements_refcounted, roc_ops);
            }
            const list_alloc_ptr = RocList.encodeSliceAllocationPtr(source_ptr);
            const slice_alloc_ptr = list.capacity_or_alloc_ptr;
            const slice_mask = list.seamlessSliceMask();
            const alloc_ptr = (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);

            return RocList{
                .bytes = source_ptr + start * element_width,
                .length = keep_len,
                .capacity_or_alloc_ptr = alloc_ptr,
            };
        }
    }

    return RocList.empty();
}

/// Remove element at specified index, shifting remaining elements.
///
/// An `.InPlace` update mode means the caller proved the list unique, so the
/// runtime uniqueness check is skipped.
pub fn listDropAt(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    drop_index_u64: u64,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const size = list.len();
    const size_u64 = @as(u64, @intCast(size));

    // Empty lists lower to the canonical null-pointer representation. Since
    // listDropAt consumes its input, spend the ownership token and return the
    // canonical empty result.
    if (size == 0) {
        list.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        return RocList.empty();
    }

    if (drop_index_u64 >= size_u64) {
        return list;
    }

    if (size == 1) {
        list.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        return RocList.empty();
    }

    // If dropping the first or last element, return a seamless slice.
    // For simplicity, do this by calling listSublist.
    // In the future, we can test if it is faster to manually inline the important parts here.
    if (drop_index_u64 == 0) {
        return listSublist(
            list,
            alignment,
            element_width,
            elements_refcounted,
            1,
            size -| 1,
            dec_context,
            dec,
            update_mode,
            roc_ops,
        );
    } else if (drop_index_u64 == size_u64 - 1) { // It's fine if (size - 1) wraps on size == 0 here,
        // because if size is 0 then it's always fine for this branch to be taken; no
        // matter what drop_index was, we're size == 0, so empty list will always be returned.
        return listSublist(
            list,
            alignment,
            element_width,
            elements_refcounted,
            0,
            size -| 1,
            dec_context,
            dec,
            update_mode,
            roc_ops,
        );
    }

    if (list.bytes) |source_ptr| {
        if (drop_index_u64 >= size_u64) {
            return list;
        }

        // This cast must be lossless, because we would have just early-returned if drop_index
        // were >= than `size`, and we know `size` fits in usize.
        const drop_index: usize = @intCast(drop_index_u64);

        const can_reuse_allocation = list.canReuseAllocation(update_mode, roc_ops);
        if (can_reuse_allocation) {
            if (elements_refcounted) {
                const element = source_ptr + drop_index * element_width;
                dec(dec_context, element);
            }

            const copy_target = source_ptr + (drop_index * element_width);
            const copy_source = copy_target + element_width;
            const copy_size = (size - drop_index - 1) * element_width;
            std.mem.copyForwards(u8, copy_target[0..copy_size], copy_source[0..copy_size]);

            var new_list = list;

            new_list.length -= 1;
            return new_list;
        }

        const output = RocList.list_allocate(
            alignment,
            size - 1,
            element_width,
            elements_refcounted,
            roc_ops,
        );
        const target_ptr = output.bytes orelse unreachable;

        const head_size = drop_index * element_width;
        @memcpy(target_ptr[0..head_size], source_ptr[0..head_size]);

        const tail_target = target_ptr + drop_index * element_width;
        const tail_source = source_ptr + (drop_index + 1) * element_width;
        const tail_size = (size - drop_index - 1) * element_width;
        @memcpy(tail_target[0..tail_size], tail_source[0..tail_size]);

        if (elements_refcounted) {
            var i: usize = 0;
            while (i < output.len()) : (i += 1) {
                const cloned_elem = target_ptr + i * element_width;
                inc(inc_context, cloned_elem);
            }
        }

        list.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);

        return output;
    } else {
        return RocList.empty();
    }
}

// SWAP ELEMENTS

fn swap(
    element_width: usize,
    p1: [*]u8,
    p2: [*]u8,
    copy: CopyFallbackFn,
) void {
    const threshold: usize = 64;

    var buffer_actual: [threshold]u8 = undefined;
    const buffer: [*]u8 = buffer_actual[0..];

    if (element_width <= threshold) {
        copy(buffer, p1, element_width);
        copy(p1, p2, element_width);
        copy(p2, buffer, element_width);
        return;
    }

    var width = element_width;

    var ptr1 = p1;
    var ptr2 = p2;
    while (true) {
        if (width < threshold) {
            @memcpy(buffer[0..width], ptr1[0..width]);
            @memcpy(ptr1[0..width], ptr2[0..width]);
            @memcpy(ptr2[0..width], buffer[0..width]);
            return;
        } else {
            @memcpy(buffer[0..threshold], ptr1[0..threshold]);
            @memcpy(ptr1[0..threshold], ptr2[0..threshold]);
            @memcpy(ptr2[0..threshold], buffer[0..threshold]);

            ptr1 += threshold;
            ptr2 += threshold;

            width -= threshold;
        }
    }
}

fn swapElements(
    source_ptr: [*]u8,
    element_width: usize,
    index_1: usize,
    index_2: usize,
    copy: CopyFallbackFn,
) void {
    const element_at_i = source_ptr + (index_1 * element_width);
    const element_at_j = source_ptr + (index_2 * element_width);

    return swap(element_width, element_at_i, element_at_j, copy);
}

/// List.reverse - reverses the order of a list's elements.
///
/// ## Ownership
/// - `list`: **consumes** - caller loses ownership
/// - Returns: **copy-on-write** - same allocation when unique, fresh copy when shared
///
/// An `.InPlace` update mode means the caller proved the list unique, so the
/// runtime uniqueness check is skipped and the elements are reversed in place.
pub fn listReverse(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    copy: CopyFallbackFn,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    if (list.len() <= 1) {
        return list;
    }

    const new_list = if (update_mode == .InPlace)
        list
    else
        list.makeUnique(
            alignment,
            element_width,
            elements_refcounted,
            inc_context,
            inc,
            dec_context,
            dec,
            roc_ops,
        );

    if (new_list.bytes) |source_ptr| {
        var lo: usize = 0;
        var hi: usize = new_list.len() - 1;
        while (lo < hi) {
            swapElements(source_ptr, element_width, lo, hi, copy);
            lo += 1;
            hi -= 1;
        }
    }

    return new_list;
}

const SortWithContext = struct {
    callable: [*]u8,
    args: [*]u8,
    second_offset: usize,
    element_width: usize,
    inc_context: ?*anyopaque,
    inc: Inc,
    elements_refcounted: bool,
    in_process: bool,
    test_context: ?*anyopaque,
    roc_ops: *RocOps,
    invoke_context: ?*anyopaque,
    invoke: *const fn (?*anyopaque, [*]u8, [*]u8) callconv(.c) u8,
};

fn compareErasedCallable(context_bytes: Opaque, first: Opaque, second: Opaque) callconv(.c) u8 {
    const context: *SortWithContext = @ptrCast(@alignCast(context_bytes orelse unreachable));
    const first_ptr = first orelse unreachable;
    const second_ptr = second orelse unreachable;
    @memcpy(context.args[0..context.element_width], first_ptr[0..context.element_width]);
    @memcpy(context.args[context.second_offset..][0..context.element_width], second_ptr[0..context.element_width]);
    if (context.elements_refcounted) {
        // Erased callable arguments are owned: the generated callable consumes
        // and decrefs them. These increfs create the ownership transferred by
        // the copied argument values, so no matching decrefs belong here.
        context.inc(context.inc_context, context.args);
        context.inc(context.inc_context, context.args + context.second_offset);
    }
    return context.invoke(context.invoke_context, context.callable, context.args);
}

fn invokeErasedCallable(context_bytes: ?*anyopaque, callable_bytes: [*]u8, args: [*]u8) callconv(.c) u8 {
    const context: *SortWithContext = @ptrCast(@alignCast(context_bytes orelse unreachable));
    var ordering: u8 = undefined;
    var result_desc: ?*const anyopaque = null;
    const payload = erased_callable.payloadPtr(callable_bytes);
    if (context.in_process) {
        const InProcessFn = *const fn (*RocOps, ?*anyopaque, ?[*]u8, ?[*]const u8, ?[*]u8, ?[*]u8, *?*const anyopaque) callconv(.c) void;
        const callable: InProcessFn = @ptrCast(@alignCast(payload.callable_fn_ptr));
        callable(context.roc_ops, context.test_context, @ptrCast(&ordering), args, erased_callable.capturePtr(callable_bytes), null, &result_desc);
    } else {
        payload.callable_fn_ptr(context.roc_ops, @ptrCast(&ordering), args, erased_callable.capturePtr(callable_bytes), null, &result_desc);
    }
    return ordering;
}

/// Stable Fluxsort through the uniform boxed erased-callable ABI.
pub fn listSortWith(list: RocList, callable: [*]u8, alignment: u32, element_width: usize, elements_refcounted: bool, inc_context: ?*anyopaque, inc: Inc, dec_context: ?*anyopaque, dec: Dec, update_mode: UpdateMode, in_process: bool, test_context: ?*anyopaque, roc_ops: *RocOps) RocList {
    if (list.len() < 2 or element_width == 0) return list;
    var result = if (update_mode == .InPlace) list else list.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
    const second_offset = std.mem.alignForward(usize, element_width, alignment);
    const args = roc_ops.alloc(alignment, second_offset + element_width);
    defer roc_ops.dealloc(args, alignment);
    var context = SortWithContext{ .callable = callable, .args = @ptrCast(args), .second_offset = second_offset, .element_width = element_width, .inc_context = inc_context, .inc = inc, .elements_refcounted = elements_refcounted, .in_process = in_process, .test_context = test_context, .roc_ops = roc_ops, .invoke_context = undefined, .invoke = &invokeErasedCallable };
    context.invoke_context = @ptrCast(&context);
    sort.fluxsort(result.bytes.?, result.len(), &compareErasedCallable, @ptrCast(&context), false, null, @ptrCast(&utils.rcNone), element_width, alignment, &copy_fallback, roc_ops);
    return result;
}

/// Stable Fluxsort with comparator invocation supplied by the boxy ABI.
pub fn listSortWithInvoker(list: RocList, callable: [*]u8, alignment: u32, element_width: usize, elements_refcounted: bool, inc_context: ?*anyopaque, inc: Inc, dec_context: ?*anyopaque, dec: Dec, update_mode: UpdateMode, invoke_context: ?*anyopaque, invoke: *const fn (?*anyopaque, [*]u8, [*]u8) callconv(.c) u8, roc_ops: *RocOps) RocList {
    if (list.len() < 2 or element_width == 0) return list;
    var result = if (update_mode == .InPlace) list else list.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
    const second_offset = std.mem.alignForward(usize, element_width, alignment);
    const args = roc_ops.alloc(alignment, second_offset + element_width);
    defer roc_ops.dealloc(args, alignment);
    var context = SortWithContext{ .callable = callable, .args = @ptrCast(args), .second_offset = second_offset, .element_width = element_width, .inc_context = inc_context, .inc = inc, .elements_refcounted = elements_refcounted, .in_process = false, .test_context = null, .roc_ops = roc_ops, .invoke_context = invoke_context, .invoke = invoke };
    sort.fluxsort(result.bytes.?, result.len(), &compareErasedCallable, @ptrCast(&context), false, null, @ptrCast(&utils.rcNone), element_width, alignment, &copy_fallback, roc_ops);
    return result;
}

/// List.concat - concatenates two lists into one.
///
/// ## Ownership
/// - `list_a`: **consumes** - caller loses ownership
/// - `list_b`: **consumes** - caller loses ownership
/// - Returns: **independent** or **copy-on-write** - new allocation or extended list_a
///
/// This function handles cleanup of both input lists internally.
/// If list_a has capacity, may extend it and return (copy-on-write).
/// Otherwise allocates new list containing elements from both.
///
/// An `.InPlace` update mode for either argument means the caller proved that
/// argument unique, so its runtime uniqueness check is skipped.
pub fn listConcat(
    list_a: RocList,
    list_b: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode_a: UpdateMode,
    update_mode_b: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    // Early return for empty lists - avoid unnecessary allocations.
    //
    // The surviving side is handed back as the result, so it has to satisfy the
    // op's `result_unique` claim (`base/LowLevel.zig`): `makeUnique` returns it
    // as-is when nobody else holds its allocation, and clones it when they do.
    // Returning a shared allocation here would let ARC treat it as freshly
    // owned and hand a later op a static in-place path into memory someone else
    // is reading.
    if (list_a.isEmpty()) {
        if (list_b.isEmpty()) {
            // Both are empty, return list_a and clean up list_b
            list_b.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
            return list_a.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
        } else {
            // list_a is empty, list_b has elements - return list_b
            // list_a might still need decref if it has capacity
            list_a.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
            return list_b.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
        }
    } else if (list_b.isEmpty()) {
        // list_b is empty, list_a has elements - return list_a
        // list_b might still need decref if it has capacity
        list_b.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
        return list_a.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
    }

    // Check if both lists share the same underlying allocation.
    // This can happen when the same list is passed as both arguments (e.g., in repeat_helper).
    const same_allocation = blk: {
        const alloc_a = list_a.getAllocationDataPtr(roc_ops);
        const alloc_b = list_b.getAllocationDataPtr(roc_ops);
        break :blk (alloc_a != null and alloc_a == alloc_b);
    };

    // If they share the same allocation, we must:
    // 1. NOT use the unique paths (reallocate might free/move the allocation)
    // 2. Only decref once at the end (to avoid double-free)
    // Instead, fall through to the general path that allocates a new list.
    const can_consume_a = !same_allocation and list_a.isExclusive(update_mode_a, roc_ops);
    const can_consume_b = !same_allocation and list_b.isExclusive(update_mode_b, roc_ops);
    const a_reuses_allocation = !same_allocation and list_a.canReuseAllocation(update_mode_a, roc_ops);
    const b_reuses_allocation = !same_allocation and list_b.canReuseAllocation(update_mode_b, roc_ops);
    const use_a_path = a_reuses_allocation or (can_consume_a and !b_reuses_allocation);

    const total_length: usize = list_a.len() + list_b.len();

    if (use_a_path) {
        var resized_list_a = list_a.reallocate(
            alignment,
            amortizedCapacity(list_a, total_length, element_width, update_mode_a, roc_ops),
            element_width,
            elements_refcounted,
            inc_context,
            inc,
            dec_context,
            dec,
            update_mode_a,
            roc_ops,
        );
        resized_list_a.length = total_length;

        // These must exist, otherwise, the lists would have been empty.
        const source_a = resized_list_a.bytes orelse unreachable;
        const source_b = list_b.bytes orelse unreachable;

        // Use @memmove instead of @memcpy to handle potential aliasing
        const dest_slice = source_a[(list_a.len() * element_width)..(total_length * element_width)];
        const src_slice = source_b[0..(list_b.len() * element_width)];
        @memmove(dest_slice, src_slice);

        // Increment refcount of all cloned elements.
        if (elements_refcounted) {
            var i: usize = 0;
            while (i < list_b.len()) : (i += 1) {
                const cloned_elem = source_b + i * element_width;
                inc(inc_context, cloned_elem);
            }
        }

        // decrement list b.
        list_b.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);

        return resized_list_a;
    } else if (can_consume_b) {
        var resized_list_b = list_b.reallocate(
            alignment,
            amortizedCapacity(list_b, total_length, element_width, update_mode_b, roc_ops),
            element_width,
            elements_refcounted,
            inc_context,
            inc,
            dec_context,
            dec,
            update_mode_b,
            roc_ops,
        );
        resized_list_b.length = total_length;

        // These must exist, otherwise, the lists would have been empty.
        const source_a = list_a.bytes orelse unreachable;
        const source_b = resized_list_b.bytes orelse unreachable;

        // This is a bit special, we need to first copy the elements of list_b to the end,
        // then copy the elements of list_a to the beginning.
        // This first call must use mem.copy because the slices might overlap.
        const byte_count_a = list_a.len() * element_width;
        const byte_count_b = list_b.len() * element_width;
        std.mem.copyBackwards(u8, source_b[byte_count_a .. byte_count_a + byte_count_b], source_b[0..byte_count_b]);
        @memcpy(source_b[0..byte_count_a], source_a[0..byte_count_a]);

        // Increment refcount of all cloned elements.
        if (elements_refcounted) {
            var i: usize = 0;
            while (i < list_a.len()) : (i += 1) {
                const cloned_elem = source_a + i * element_width;
                inc(inc_context, cloned_elem);
            }
        }

        // decrement list a.
        list_a.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);

        return resized_list_b;
    }

    const output = RocList.list_allocate(alignment, total_length, element_width, elements_refcounted, roc_ops);

    // These must exist, otherwise, the lists would have been empty.
    const target = output.bytes orelse unreachable;
    const source_a = list_a.bytes orelse unreachable;
    const source_b = list_b.bytes orelse unreachable;

    @memcpy(target[0..(list_a.len() * element_width)], source_a[0..(list_a.len() * element_width)]);
    @memcpy(target[(list_a.len() * element_width)..(total_length * element_width)], source_b[0..(list_b.len() * element_width)]);

    // Increment refcount of all cloned elements.
    if (elements_refcounted) {
        var i: usize = 0;
        while (i < list_a.len()) : (i += 1) {
            const cloned_elem = source_a + i * element_width;
            inc(inc_context, cloned_elem);
        }
        i = 0;
        while (i < list_b.len()) : (i += 1) {
            const cloned_elem = source_b + i * element_width;
            inc(inc_context, cloned_elem);
        }
    }

    // Decrement both consumed lists. Even if both values share an allocation, they
    // are separate owned references at this call boundary.
    list_a.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);
    list_b.decref(alignment, element_width, elements_refcounted, dec_context, dec, roc_ops);

    return output;
}

/// Replace element at index, returning original value. No allocation when unique.
pub fn listReplaceInPlace(
    list: RocList,
    index: u64,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
    copy: CopyFallbackFn,
) callconv(.c) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds,
    // and it's always safe to cast index to usize.
    return listReplaceInPlaceHelp(list, @as(usize, @intCast(index)), element, element_width, out_element, copy);
}

/// Replace element at index, ensuring list uniqueness through copy-on-write.
pub fn listReplace(
    list: RocList,
    alignment: u32,
    index: u64,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    out_element: ?[*]u8,
    copy: CopyFallbackFn,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds,
    // and it's always safe to cast index to usize.
    // because inserting into an empty list is always out of bounds
    return listReplaceInPlaceHelp(
        list.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops),
        @as(usize, @intCast(index)),
        element,
        element_width,
        out_element,
        copy,
    );
}

/// Replace an element and release the displaced value.
///
/// `listReplace` transfers the displaced element to its caller. `listSet`
/// instead implements the ownership contract for `List.set`, whose result does
/// not contain the old element.
pub fn listSet(
    list: RocList,
    alignment: u32,
    index: u64,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    update_mode: UpdateMode,
    copy: CopyFallbackFn,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const output = if (update_mode == .InPlace)
        list
    else
        list.makeUnique(
            alignment,
            element_width,
            elements_refcounted,
            inc_context,
            inc,
            dec_context,
            dec,
            roc_ops,
        );

    const element_at_index = (output.bytes orelse unreachable) + (@as(usize, @intCast(index)) * element_width);
    if (elements_refcounted) dec(dec_context, element_at_index);
    copy(element_at_index, element orelse unreachable, element_width);
    return output;
}

inline fn listReplaceInPlaceHelp(
    list: RocList,
    index: usize,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
    copy: CopyFallbackFn,
) RocList {
    // the element we will replace
    const element_at_index = (list.bytes orelse unreachable) + (index * element_width);

    // copy out the old element
    copy((out_element orelse unreachable), element_at_index, element_width);

    // copy in the new element
    copy(element_at_index, (element orelse unreachable), element_width);

    return list;
}

/// Whether List.map may overwrite this list's elements in place: the list
/// must uniquely own its allocation and must not be a seamless slice into a
/// larger allocation (a slice's buffer start and header bookkeeping cover
/// the whole underlying allocation, not just the slice window).
pub fn listMapCanReuse(
    list: RocList,
    roc_ops: *RocOps,
) callconv(.c) bool {
    return list.canReuseAllocation(.Immutable, roc_ops);
}

/// Create independent copy for safe mutation when list is shared.
pub fn listClone(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc_context: ?*anyopaque,
    inc: Inc,
    dec_context: ?*anyopaque,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    return list.makeUnique(alignment, element_width, elements_refcounted, inc_context, inc, dec_context, dec, roc_ops);
}

/// No-op reference counting function for non-refcounted types
pub fn rcNone(_: ?*anyopaque, _: ?[*]u8) callconv(.c) void {}

/// Test helper: compare two lists' raw bytes (only valid for flat scalar elements).
fn testBytesEqual(a: RocList, b: RocList) bool {
    if (a.len() != b.len()) return false;
    if (a.isEmpty()) return true;
    const a_bytes = a.bytes orelse return false;
    const b_bytes = b.bytes orelse return false;
    return std.mem.eql(u8, a_bytes[0..a.len()], b_bytes[0..b.len()]);
}

/// Specialized copy fn which takes pointers as pointers to u8 and copies from src to dest.
pub fn copy_fallback(dest: Opaque, source: Opaque, width: usize) callconv(.c) void {
    const src: []u8 = source.?[0..width];
    const dst: []u8 = dest.?[0..width];
    @memmove(dst, src);
}

test "listConcat: non-unique with unique overlapping" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const nonUnique = RocList.fromSlice(u8, ([_]u8{1})[0..], false, test_env.getOps());
    const bytes: [*]u8 = @as([*]u8, @ptrCast(nonUnique.bytes));
    const refcount_ptr: [*]isize = utils.alignedPtrCast([*]isize, bytes - @sizeOf(usize), @src());
    utils.increfRcPtrC(&refcount_ptr[0], 1, test_env.getOps());
    // NOTE: nonUnique will be consumed by listConcat, so no defer decref needed

    const unique = RocList.fromSlice(u8, ([_]u8{ 2, 3, 4 })[0..], false, test_env.getOps());
    // NOTE: unique will be consumed by listConcat, so no defer decref needed

    var concatted = listConcat(nonUnique, unique, 1, 1, false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());
    defer concatted.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());
    var wanted = RocList.fromSlice(u8, ([_]u8{ 1, 2, 3, 4 })[0..], false, test_env.getOps());
    defer wanted.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(testBytesEqual(concatted, wanted));
}

test "listConcat onto a unique full list takes a geometric growth step" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list_a = RocList.fromSlice(u64, ([_]u64{ 1, 2, 3 })[0..], false, test_env.getOps());
    const list_b = RocList.fromSlice(u64, ([_]u64{ 4, 5 })[0..], false, test_env.getOps());

    const concatted = listConcat(list_a, list_b, @alignOf(u64), @sizeOf(u64), false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());
    defer concatted.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqualSlices(u64, &.{ 1, 2, 3, 4, 5 }, concatted.elements(u64).?[0..concatted.len()]);
    try std.testing.expectEqual(utils.geometricGrowth(3, @sizeOf(u64)), concatted.getCapacity());
}

test "listConcat prepending onto a unique full list takes a geometric growth step" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const shared_a = RocList.fromSlice(u64, ([_]u64{ 1, 2 })[0..], false, test_env.getOps());
    shared_a.incref(1, false, test_env.getOps());
    defer shared_a.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
    const list_b = RocList.fromSlice(u64, ([_]u64{ 3, 4, 5 })[0..], false, test_env.getOps());

    const concatted = listConcat(shared_a, list_b, @alignOf(u64), @sizeOf(u64), false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());
    defer concatted.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqualSlices(u64, &.{ 1, 2, 3, 4, 5 }, concatted.elements(u64).?[0..concatted.len()]);
    try std.testing.expectEqual(utils.geometricGrowth(3, @sizeOf(u64)), concatted.getCapacity());
}

test "listConcat refcounted seamless slice releases backing allocation when reused" {
    const Counter = struct {
        fn inc(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }

        fn dec(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }
    };

    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const left_data = [_]u8{ 1, 2, 3, 4 };
    const right_data = [_]u8{ 9, 10 };
    const left = RocList.fromSlice(u8, left_data[0..], true, test_env.getOps());
    const right = RocList.fromSlice(u8, right_data[0..], true, test_env.getOps());
    var inc_count: usize = 0;
    var dec_count: usize = 0;

    const slice = listSublist(
        left,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        1,
        2,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );
    try std.testing.expect(slice.isSeamlessSlice());

    const result = listConcat(
        slice,
        right,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        &inc_count,
        Counter.inc,
        &dec_count,
        Counter.dec,
        .InPlace,
        .InPlace,
        test_env.getOps(),
    );
    defer result.decref(@alignOf(u8), @sizeOf(u8), true, &dec_count, Counter.dec, test_env.getOps());

    try std.testing.expect(!result.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(usize, right_data.len), inc_count);
    try std.testing.expectEqual(@as(usize, left_data.len - slice.len() + right_data.len), dec_count);

    const elements = result.elements(u8).?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 3), elements[1]);
    try std.testing.expectEqual(@as(u8, 9), elements[2]);
    try std.testing.expectEqual(@as(u8, 10), elements[3]);
}

test "listConcat reuses non-slice allocation before cloning seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const left_data = [_]u8{ 1, 2, 3, 4 };
    const right_data = [_]u8{ 9, 10 };
    const left = RocList.fromSlice(u8, left_data[0..], false, test_env.getOps());
    var right = RocList.fromSlice(u8, right_data[0..], false, test_env.getOps());
    right = listReserve(right, @alignOf(u8), 2, @sizeOf(u8), false, null, rcNone, null, rcNone, .InPlace, test_env.getOps());
    const right_bytes = right.bytes;

    const slice = listSublist(
        left,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        1,
        2,
        null,
        rcNone,
        .InPlace,
        test_env.getOps(),
    );
    try std.testing.expect(slice.isSeamlessSlice());

    const result = listConcat(
        slice,
        right,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        null,
        rcNone,
        null,
        rcNone,
        .InPlace,
        .InPlace,
        test_env.getOps(),
    );
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(right_bytes, result.bytes);
    try std.testing.expect(!result.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 4), result.len());

    const elements = result.elements(u8).?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 3), elements[1]);
    try std.testing.expectEqual(@as(u8, 9), elements[2]);
    try std.testing.expectEqual(@as(u8, 10), elements[3]);
}

test "RocList empty list creation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    defer empty_list.decref(1, 1, false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), empty_list.len());
    try std.testing.expect(empty_list.isEmpty());
}

test "default-platform RocList view matches canonical RocList layout" {
    const View = @import("roc_str_view").RocList;

    try std.testing.expectEqual(@sizeOf(RocList), @sizeOf(View));
    try std.testing.expectEqual(@alignOf(RocList), @alignOf(View));

    const canonical_fields = @typeInfo(RocList).@"struct".fields;
    const view_fields = @typeInfo(View).@"struct".fields;
    try std.testing.expectEqual(canonical_fields.len, view_fields.len);
    inline for (canonical_fields, view_fields) |canonical, view| {
        try std.testing.expect(std.mem.eql(u8, canonical.name, view.name));
        try std.testing.expectEqual(canonical.type, view.type);
        try std.testing.expectEqual(@offsetOf(RocList, canonical.name), @offsetOf(View, view.name));
    }
}

test "RocList fromSlice basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), list.len());
    try std.testing.expect(!list.isEmpty());
}

test "RocList elements access" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4, 5 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
    try std.testing.expectEqual(@as(u8, 4), elements[3]);
    try std.testing.expectEqual(@as(u8, 5), elements[4]);
}

test "RocList capacity operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i16{ 100, 200 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(i16), @sizeOf(i16), false, null, rcNone, test_env.getOps());

    const capacity = list.getCapacity();
    try std.testing.expect(capacity >= list.len());
    try std.testing.expect(capacity >= 2);
}

test "RocList equality operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data1 = [_]u8{ 1, 2, 3 };
    const data2 = [_]u8{ 1, 2, 3 };
    const data3 = [_]u8{ 1, 2, 4 };

    const list1 = RocList.fromSlice(u8, data1[0..], false, test_env.getOps());
    defer list1.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    const list2 = RocList.fromSlice(u8, data2[0..], false, test_env.getOps());
    defer list2.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    const list3 = RocList.fromSlice(u8, data3[0..], false, test_env.getOps());
    defer list3.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Equal lists should be equal
    try std.testing.expect(testBytesEqual(list1, list2));
    try std.testing.expect(testBytesEqual(list2, list1));

    // Different lists should not be equal
    try std.testing.expect(!testBytesEqual(list1, list3));
    try std.testing.expect(!testBytesEqual(list3, list1));

    // Empty lists should be equal
    const empty1 = RocList.empty();
    defer empty1.decref(1, 1, false, null, rcNone, test_env.getOps());
    const empty2 = RocList.empty();
    defer empty2.decref(1, 1, false, null, rcNone, test_env.getOps());
    try std.testing.expect(testBytesEqual(empty1, empty2));
}

test "RocList uniqueness and cloning" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // A freshly created list should be unique
    try std.testing.expect(list.isUnique(test_env.getOps()));

    // Make the list non-unique by incrementing reference count
    list.incref(1, false, test_env.getOps());
    defer list.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());
    try std.testing.expect(!list.isUnique(test_env.getOps()));

    // Clone the list (this will consume one reference to the original)
    const cloned = listClone(list, @alignOf(i32), @sizeOf(i32), false, null, rcNone, null, rcNone, test_env.getOps());
    defer cloned.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    // Both should be equal but different objects (since list was not unique)
    try std.testing.expect(testBytesEqual(list, cloned));
    try std.testing.expect(list.bytes != cloned.bytes);
}

test "listWithCapacity basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const capacity: usize = 10;
    const list = listWithCapacity(capacity, @alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());
    defer list.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    // Should have the requested capacity
    try std.testing.expect(list.getCapacity() >= capacity);
    // Should be empty initially
    try std.testing.expectEqual(@as(usize, 0), list.len());
    try std.testing.expect(list.isEmpty());
}

test "listReserve functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    const reserved_list = listReserve(list, @alignOf(u8), 20, @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer reserved_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Should have at least the requested capacity
    try std.testing.expect(reserved_list.getCapacity() >= 20);
    // Should preserve the original content
    try std.testing.expectEqual(@as(usize, 3), reserved_list.len());

    const elements_ptr = reserved_list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..reserved_list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
}

test "listReserve is exact when the request is one past the capacity" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Length 3 and capacity 3: reserving 1 asks for capacity + 1.
    const full = RocList.fromSlice(u64, ([_]u64{ 1, 2, 3 })[0..], false, test_env.getOps());
    const reserved_one = listReserve(full, @alignOf(u64), 1, @sizeOf(u64), false, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 4), reserved_one.getCapacity());

    // Length 2 and capacity 4: reserving 3 also lands on capacity + 1.
    var partial = listReserve(
        RocList.fromSlice(u64, ([_]u64{ 1, 2 })[0..], false, test_env.getOps()),
        @alignOf(u64),
        2,
        @sizeOf(u64),
        false,
        null,
        rcNone,
        null,
        rcNone,
        .Immutable,
        test_env.getOps(),
    );
    try std.testing.expectEqual(@as(usize, 4), partial.getCapacity());
    partial = listReserve(partial, @alignOf(u64), 3, @sizeOf(u64), false, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer partial.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 5), partial.getCapacity());

    reserved_one.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
}

test "listReserveForAppend takes a geometric growth step" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_grown = listReserveForAppend(RocList.empty(), @alignOf(u64), 1, @sizeOf(u64), false, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer empty_grown.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqual(utils.geometricGrowth(0, @sizeOf(u64)), empty_grown.getCapacity());

    const full = RocList.fromSlice(u64, ([_]u64{ 1, 2, 3 })[0..], false, test_env.getOps());
    const grown = listReserveForAppend(full, @alignOf(u64), 2, @sizeOf(u64), false, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer grown.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 3), grown.len());
    try std.testing.expectEqual(utils.geometricGrowth(3, @sizeOf(u64)), grown.getCapacity());
}

test "listReserve refcounted seamless slice releases backing allocation when growing" {
    const Counter = struct {
        fn inc(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }

        fn dec(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }
    };

    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], true, test_env.getOps());
    var inc_count: usize = 0;
    var dec_count: usize = 0;

    const slice = listSublist(
        list,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        1,
        2,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );
    try std.testing.expect(slice.isSeamlessSlice());

    const reserved = listReserve(
        slice,
        @alignOf(u8),
        1,
        @sizeOf(u8),
        true,
        &inc_count,
        Counter.inc,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );
    defer reserved.decref(@alignOf(u8), @sizeOf(u8), true, &dec_count, Counter.dec, test_env.getOps());

    try std.testing.expect(!reserved.isSeamlessSlice());
    try std.testing.expect(reserved.getCapacity() >= 3);
    try std.testing.expectEqual(@as(usize, 2), reserved.len());
    try std.testing.expectEqual(@as(usize, 0), inc_count);
    try std.testing.expectEqual(@as(usize, data.len - slice.len()), dec_count);

    const elements = reserved.elements(u8).?[0..reserved.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 3), elements[1]);
}

test "RocList canReuseAllocation excludes seamless slices" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    try std.testing.expect(RocList.empty().isExclusive(.Immutable, test_env.getOps()));
    try std.testing.expect(RocList.empty().canReuseAllocation(.Immutable, test_env.getOps()));

    const plain_data = [_]u8{ 1, 2, 3 };
    const plain = RocList.fromSlice(u8, plain_data[0..], false, test_env.getOps());
    defer plain.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(plain.isExclusive(.Immutable, test_env.getOps()));
    try std.testing.expect(plain.canReuseAllocation(.Immutable, test_env.getOps()));
    try std.testing.expect(listMapCanReuse(plain, test_env.getOps()));

    const unique_data = [_]u8{ 4, 5, 6, 7 };
    const unique_source = RocList.fromSlice(u8, unique_data[0..], false, test_env.getOps());
    const unique_slice = listSublist(
        unique_source,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        1,
        2,
        null,
        rcNone,
        .InPlace,
        test_env.getOps(),
    );
    defer unique_slice.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(unique_slice.isSeamlessSlice());
    try std.testing.expect(unique_slice.isExclusive(.Immutable, test_env.getOps()));
    try std.testing.expect(!unique_slice.canReuseAllocation(.Immutable, test_env.getOps()));
    try std.testing.expect(!listMapCanReuse(unique_slice, test_env.getOps()));

    const shared_data = [_]u8{ 8, 9, 10, 11 };
    const shared_source = RocList.fromSlice(u8, shared_data[0..], false, test_env.getOps());
    shared_source.incref(1, false, test_env.getOps());
    defer shared_source.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    const shared_slice = listSublist(
        shared_source,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        1,
        2,
        null,
        rcNone,
        .Immutable,
        test_env.getOps(),
    );
    defer shared_slice.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(shared_slice.isSeamlessSlice());
    try std.testing.expect(!shared_slice.isExclusive(.Immutable, test_env.getOps()));
    try std.testing.expect(!shared_slice.canReuseAllocation(.Immutable, test_env.getOps()));
}

test "listReserve keeps zero-spare seamless slice unchanged" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const slice = listSublist(
        list,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        1,
        2,
        null,
        rcNone,
        .InPlace,
        test_env.getOps(),
    );

    const slice_bytes = slice.bytes;
    const slice_alloc = slice.getAllocationDataPtr(test_env.getOps());
    const reserved = listReserve(
        slice,
        @alignOf(u8),
        0,
        @sizeOf(u8),
        false,
        null,
        rcNone,
        null,
        rcNone,
        .InPlace,
        test_env.getOps(),
    );
    defer reserved.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(reserved.isSeamlessSlice());
    try std.testing.expect(reserved.bytes == slice_bytes);
    try std.testing.expect(reserved.getAllocationDataPtr(test_env.getOps()) == slice_alloc);
    try std.testing.expectEqual(@as(usize, 2), reserved.len());
}

test "RocList list_allocate sizes the allocation exactly" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    for ([_]usize{ 1, 5 }) |exact_size| {
        const list = RocList.list_allocate(@alignOf(u64), exact_size, @sizeOf(u64), false, test_env.getOps());
        defer list.decref(@alignOf(u64), @sizeOf(u64), false, null, rcNone, test_env.getOps());

        try std.testing.expectEqual(exact_size, list.getCapacity());
        try std.testing.expectEqual(exact_size, list.len());
        try std.testing.expect(!list.isEmpty());
    }
}

test "listReleaseExcessCapacity functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with some data
    const data = [_]u8{ 1, 2, 3 };
    const list_with_data = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve excess capacity for it
    const list_with_excess = listReserve(list_with_data, @alignOf(u8), 100, @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());

    // Verify it has excess capacity
    try std.testing.expect(list_with_excess.getCapacity() >= 100);
    try std.testing.expectEqual(@as(usize, 3), list_with_excess.len());

    // Release the excess capacity
    const released_list = listReleaseExcessCapacity(list_with_excess, @alignOf(u8), @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer released_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // The released list should have capacity close to its length and preserve the data
    try std.testing.expectEqual(@as(usize, 3), released_list.len());
    try std.testing.expect(released_list.getCapacity() >= released_list.len());
    try std.testing.expect(released_list.getCapacity() < 100); // Much less than the original excess

    // Verify data is preserved
    const elements_ptr = released_list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..released_list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
}

test "listReleaseExcessCapacity clones seamless slice before releasing backing allocation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const slice = listSublist(
        list,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        1,
        2,
        null,
        rcNone,
        .InPlace,
        test_env.getOps(),
    );
    const slice_bytes = slice.bytes;

    const released = listReleaseExcessCapacity(
        slice,
        @alignOf(u8),
        @sizeOf(u8),
        false,
        null,
        rcNone,
        null,
        rcNone,
        .InPlace,
        test_env.getOps(),
    );
    defer released.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(!released.isSeamlessSlice());
    try std.testing.expect(released.bytes != slice_bytes);
    try std.testing.expectEqual(@as(usize, 2), released.len());

    const elements = released.elements(u8).?[0..released.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 3), elements[1]);
}

test "randomized refcounted list slice operations match reference model" {
    const StrList = struct {
        fn inc(ctx: ?*anyopaque, elem: ?[*]u8) callconv(.c) void {
            const ops: *RocOps = @ptrCast(@alignCast(ctx.?));
            const str: *RocStr = @ptrCast(@alignCast(elem.?));
            str.incref(1, ops);
        }

        fn dec(ctx: ?*anyopaque, elem: ?[*]u8) callconv(.c) void {
            const ops: *RocOps = @ptrCast(@alignCast(ctx.?));
            const str: *RocStr = @ptrCast(@alignCast(elem.?));
            str.decref(ops);
        }

        fn bytesForId(id: u8, buf: []u8) []const u8 {
            const prefix = "item-";
            const suffix = "-abcdefghijklmnopqrstuvwxyz";
            const len = prefix.len + 3 + suffix.len;
            std.debug.assert(buf.len >= len);

            @memcpy(buf[0..prefix.len], prefix);
            buf[prefix.len] = '0' + (id / 100);
            buf[prefix.len + 1] = '0' + ((id / 10) % 10);
            buf[prefix.len + 2] = '0' + (id % 10);
            @memcpy(buf[prefix.len + 3 .. len], suffix);

            return buf[0..len];
        }

        fn strForId(id: u8, roc_ops: *RocOps) RocStr {
            var buf: [48]u8 = undefined;
            const bytes = bytesForId(id, buf[0..]);
            return RocStr.fromSlice(bytes, roc_ops);
        }

        fn listFromIds(ids: []const u8, roc_ops: *RocOps) RocList {
            var strs: [8]RocStr = undefined;
            std.debug.assert(ids.len <= strs.len);
            for (ids, 0..) |id, i| {
                strs[i] = strForId(id, roc_ops);
            }
            return RocList.fromSlice(RocStr, strs[0..ids.len], true, roc_ops);
        }

        fn expectList(list: RocList, expected: []const u8) error{ TestExpectedEqual, TestUnexpectedResult }!void {
            try std.testing.expectEqual(expected.len, list.len());
            if (expected.len == 0) {
                return;
            }

            const elems = list.elements(RocStr).?;
            for (expected, 0..) |id, i| {
                var buf: [48]u8 = undefined;
                const expected_bytes = bytesForId(id, buf[0..]);
                try std.testing.expect(elems[i].eqlSlice(expected_bytes));
            }
        }
    };

    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var prng = std.Random.DefaultPrng.init(0x9742_cafe_babe);
    const random = prng.random();

    var expected: [96]u8 = undefined;
    var expected_len: usize = 8;
    for (expected[0..expected_len], 0..) |*slot, i| {
        slot.* = @intCast(i);
    }

    var list = StrList.listFromIds(expected[0..expected_len], test_env.getOps());

    var step: usize = 0;
    while (step < 80) : (step += 1) {
        const share_before_op = random.boolean();
        const shared = list;
        if (share_before_op) {
            list.incref(1, true, test_env.getOps());
        }

        const update_mode: UpdateMode = if (!share_before_op and random.boolean()) .InPlace else .Immutable;
        const op = random.intRangeLessThan(u8, 0, 5);

        switch (op) {
            0 => {
                const old_len = expected_len;
                const start = if (old_len == 0) 0 else random.intRangeLessThan(usize, 0, old_len + 2);
                const keep = random.intRangeLessThan(usize, 0, old_len + 3);

                list = listSublist(
                    list,
                    @alignOf(RocStr),
                    @sizeOf(RocStr),
                    true,
                    @intCast(start),
                    @intCast(keep),
                    test_env.getOps(),
                    StrList.dec,
                    update_mode,
                    test_env.getOps(),
                );

                if (old_len == 0 or keep == 0 or start >= old_len) {
                    expected_len = 0;
                } else {
                    const new_len = @min(keep, old_len - start);
                    std.mem.copyForwards(u8, expected[0..new_len], expected[start .. start + new_len]);
                    expected_len = new_len;
                }
            },
            1 => {
                const old_len = expected_len;
                const index = if (old_len == 0) 0 else random.intRangeLessThan(usize, 0, old_len + 2);

                list = listDropAt(
                    list,
                    @alignOf(RocStr),
                    @sizeOf(RocStr),
                    true,
                    @intCast(index),
                    test_env.getOps(),
                    StrList.inc,
                    test_env.getOps(),
                    StrList.dec,
                    update_mode,
                    test_env.getOps(),
                );

                if (index < old_len) {
                    std.mem.copyForwards(u8, expected[index .. old_len - 1], expected[index + 1 .. old_len]);
                    expected_len = old_len - 1;
                }
            },
            2 => {
                if (expected_len + 3 < expected.len) {
                    var tail_ids: [3]u8 = undefined;
                    const tail_len = random.intRangeLessThan(usize, 0, tail_ids.len + 1);
                    for (tail_ids[0..tail_len], 0..) |*slot, i| {
                        slot.* = @intCast(40 + ((step * 3 + i) % 180));
                    }
                    const tail = StrList.listFromIds(tail_ids[0..tail_len], test_env.getOps());

                    list = listConcat(
                        list,
                        tail,
                        @alignOf(RocStr),
                        @sizeOf(RocStr),
                        true,
                        test_env.getOps(),
                        StrList.inc,
                        test_env.getOps(),
                        StrList.dec,
                        update_mode,
                        .InPlace,
                        test_env.getOps(),
                    );

                    @memcpy(expected[expected_len .. expected_len + tail_len], tail_ids[0..tail_len]);
                    expected_len += tail_len;
                } else {
                    list = listReleaseExcessCapacity(
                        list,
                        @alignOf(RocStr),
                        @sizeOf(RocStr),
                        true,
                        test_env.getOps(),
                        StrList.inc,
                        test_env.getOps(),
                        StrList.dec,
                        update_mode,
                        test_env.getOps(),
                    );
                }
            },
            3 => {
                const spare = random.intRangeLessThan(u64, 0, 4);
                list = listReserve(
                    list,
                    @alignOf(RocStr),
                    spare,
                    @sizeOf(RocStr),
                    true,
                    test_env.getOps(),
                    StrList.inc,
                    test_env.getOps(),
                    StrList.dec,
                    update_mode,
                    test_env.getOps(),
                );
            },
            else => {
                list = listReleaseExcessCapacity(
                    list,
                    @alignOf(RocStr),
                    @sizeOf(RocStr),
                    true,
                    test_env.getOps(),
                    StrList.inc,
                    test_env.getOps(),
                    StrList.dec,
                    update_mode,
                    test_env.getOps(),
                );
            },
        }

        if (share_before_op) {
            shared.decref(@alignOf(RocStr), @sizeOf(RocStr), true, test_env.getOps(), StrList.dec, test_env.getOps());
        }

        try StrList.expectList(list, expected[0..expected_len]);
    }

    list.decref(@alignOf(RocStr), @sizeOf(RocStr), true, test_env.getOps(), StrList.dec, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "listSublist basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    // Note: listSublist consumes the original list

    // Extract middle portion
    const sublist = listSublist(list, @alignOf(u8), @sizeOf(u8), false, 2, 4, null, rcNone, .Immutable, test_env.getOps());
    defer sublist.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), sublist.len());

    const elements_ptr = sublist.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..sublist.len()];
    try std.testing.expectEqual(@as(u8, 3), elements[0]); // data[2]
    try std.testing.expectEqual(@as(u8, 4), elements[1]); // data[3]
    try std.testing.expectEqual(@as(u8, 5), elements[2]); // data[4]
    try std.testing.expectEqual(@as(u8, 6), elements[3]); // data[5]
}

test "borrowed sublist leaves source unique and initializes teardown metadata" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, &.{ 10, 20, 30, 40 }, true, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), true, null, rcNone, test_env.getOps());

    const sublist = listSublistBorrowed(list, @sizeOf(u8), 1, 2, true, test_env.getOps());

    try std.testing.expect(list.isUnique(test_env.getOps()));
    try std.testing.expectEqual(@as(usize, 4), sublist.getAllocationElementCount(true, test_env.getOps()));
    try std.testing.expect(sublist.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 2), sublist.len());
    try std.testing.expectEqualSlices(u8, &.{ 20, 30 }, sublist.elements(u8).?[0..sublist.len()]);
}

test "listSublist edge cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Take empty sublist
    const empty_sublist = listSublist(list, @alignOf(i32), @sizeOf(i32), false, 1, 0, null, rcNone, .Immutable, test_env.getOps());
    defer empty_sublist.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), empty_sublist.len());
    try std.testing.expect(empty_sublist.isEmpty());
}

test "listSublist empty result from seamless slice releases backing allocation once" {
    const Counter = struct {
        fn dec(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }
    };

    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], true, test_env.getOps());
    var dec_count: usize = 0;

    const suffix = listSublist(
        list,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        1,
        2,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );
    try std.testing.expect(suffix.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 0), dec_count);

    const empty = listSublist(
        suffix,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        0,
        0,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );

    try std.testing.expect(empty.isEmpty());
    try std.testing.expect(!empty.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, data.len), dec_count);
}

test "listSwap basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u16{ 100, 200, 300, 400 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Swap elements at indices 1 and 3
    // Proper copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const swapped_list = listSwap(list, @alignOf(u16), @sizeOf(u16), 1, 3, false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, copy_fn, test_env.getOps());
    defer swapped_list.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), swapped_list.len());

    // Verify the swap actually worked
    const elements_ptr = swapped_list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..swapped_list.len()];
    try std.testing.expectEqual(@as(u16, 100), elements[0]); // unchanged
    try std.testing.expectEqual(@as(u16, 400), elements[1]); // was 200, now 400
    try std.testing.expectEqual(@as(u16, 300), elements[2]); // unchanged
    try std.testing.expectEqual(@as(u16, 200), elements[3]); // was 400, now 200
}

test "listAppendUnsafe basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with some capacity
    var list = listWithCapacity(10, @alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Add some initial elements using listAppendUnsafe
    const element1: u8 = 42;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u8), &copy_fallback);

    const element2: u8 = 84;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u8), &copy_fallback);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), list.len());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 42), elements[0]);
    try std.testing.expectEqual(@as(u8, 84), elements[1]);
}

test "listReserve followed by listAppendUnsafe reuses reserved allocation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();
    list = listReserve(list, @alignOf(u16), 2, @sizeOf(u16), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());

    const reserved_ptr = list.bytes;
    try std.testing.expect(list.getCapacity() >= 2);

    const first: u16 = 11;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&first))), @sizeOf(u16), &copy_fallback);
    try std.testing.expectEqual(reserved_ptr, list.bytes);

    const second: u16 = 22;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&second))), @sizeOf(u16), &copy_fallback);
    try std.testing.expectEqual(reserved_ptr, list.bytes);
    try std.testing.expectEqual(@as(usize, 2), list.len());

    const elements_ptr = list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u16, 11), elements[0]);
    try std.testing.expectEqual(@as(u16, 22), elements[1]);

    defer list.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());
}

test "listAppendSublist on a shared list sizes each copy by its length" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const ops = test_env.getOps();

    const src = RocList.fromSlice(u8, ([_]u8{7})[0..], false, ops);
    defer src.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, ops);

    var items = RocList.empty();
    var step: usize = 0;
    while (step < 200) : (step += 1) {
        const kept = items;
        kept.incref(1, false, ops);
        items = listAppendSublist(items, src, 0, 1, @alignOf(u8), @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, ops);
        kept.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, ops);

        try std.testing.expectEqual(step + 1, items.len());
        try std.testing.expect(items.getCapacity() <= @max(64, 2 * items.len()));
    }

    const elements = items.elements(u8).?[0..items.len()];
    for (elements) |byte| try std.testing.expectEqual(@as(u8, 7), byte);
    items.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, ops);
}

test "listAppendSublist on an exclusive list grows geometrically in place" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const ops = test_env.getOps();

    const src = RocList.fromSlice(u8, ([_]u8{7})[0..], false, ops);
    defer src.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, ops);

    var items = RocList.empty();
    var reallocations: usize = 0;
    var step: usize = 0;
    while (step < 1000) : (step += 1) {
        const capacity_before = items.getCapacity();
        items = listAppendSublist(items, src, 0, 1, @alignOf(u8), @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, ops);
        if (items.getCapacity() != capacity_before) reallocations += 1;
    }

    try std.testing.expectEqual(@as(usize, 1000), items.len());
    try std.testing.expect(reallocations <= 8);
    items.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, ops);
}

test "listPrepend basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a list containing some elements
    const initial_data = [_]u8{ 2, 3, 4 };
    const list = RocList.fromSlice(u8, initial_data[0..], false, test_env.getOps());

    // Prepend an element
    const element: u8 = 1;
    const result = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]); // prepended element
    try std.testing.expectEqual(@as(u8, 2), elements[1]); // original first
    try std.testing.expectEqual(@as(u8, 3), elements[2]); // original second
    try std.testing.expectEqual(@as(u8, 4), elements[3]); // original third
}

test "listPrepend to empty list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for i32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with an empty list
    const empty_list = RocList.empty();

    // Prepend an element
    const element: i32 = 42;
    const result = listPrepend(empty_list, @alignOf(i32), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(i32), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());
    defer result.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), result.len());
    try std.testing.expect(!result.isEmpty());

    const elements_ptr = result.elements(i32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i32, 42), elements[0]);
}

test "listPrepend multiple elements" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a single element
    const initial_data = [_]u16{100};
    var list = RocList.fromSlice(u16, initial_data[0..], false, test_env.getOps());

    // Prepend first element
    const element1: u16 = 200;
    list = listPrepend(list, @alignOf(u16), @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u16), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());

    // Prepend second element
    const element2: u16 = 300;
    list = listPrepend(list, @alignOf(u16), @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u16), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());

    defer list.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), list.len());

    const elements_ptr = list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u16, 300), elements[0]); // last prepended (most recent)
    try std.testing.expectEqual(@as(u16, 200), elements[1]); // first prepended
    try std.testing.expectEqual(@as(u16, 100), elements[2]); // original element
}

test "listDropAt basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with multiple elements
    const data = [_]u8{ 10, 20, 30, 40, 50 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Drop element at index 2 (value 30)
    const result = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 2, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 40), elements[2]); // 30 was dropped
    try std.testing.expectEqual(@as(u8, 50), elements[3]);
}

test "listDropAt middle element from seamless slice clones before shifting" {
    const Counter = struct {
        fn inc(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }

        fn dec(ctx: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const count: *usize = @ptrCast(@alignCast(ctx.?));
            count.* += 1;
        }
    };

    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], true, test_env.getOps());
    var inc_count: usize = 0;
    var dec_count: usize = 0;

    const suffix = listSublist(
        list,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        1,
        3,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );
    try std.testing.expect(suffix.isSeamlessSlice());

    const dropped = listDropAt(
        suffix,
        @alignOf(u8),
        @sizeOf(u8),
        true,
        1,
        &inc_count,
        Counter.inc,
        &dec_count,
        Counter.dec,
        .InPlace,
        test_env.getOps(),
    );
    defer dropped.decref(@alignOf(u8), @sizeOf(u8), true, &dec_count, Counter.dec, test_env.getOps());

    try std.testing.expect(!dropped.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 2), dropped.len());
    try std.testing.expectEqual(@as(usize, 2), inc_count);
    try std.testing.expectEqual(@as(usize, data.len), dec_count);

    const elements = dropped.elements(u8).?[0..dropped.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 4), elements[1]);
}

test "listDropAt first element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with multiple elements
    const data = [_]i32{ 100, 200, 300 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Drop first element (index 0)
    const result = listDropAt(list, @alignOf(i32), @sizeOf(i32), false, 0, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer result.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), result.len());

    const elements_ptr = result.elements(i32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i32, 200), elements[0]); // first element was dropped
    try std.testing.expectEqual(@as(i32, 300), elements[1]);
}

test "listDropAt last element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with multiple elements
    const data = [_]u16{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Drop last element (index 3)
    const result = listDropAt(list, @alignOf(u16), @sizeOf(u16), false, 3, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result.len());

    const elements_ptr = result.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 1), elements[0]);
    try std.testing.expectEqual(@as(u16, 2), elements[1]);
    try std.testing.expectEqual(@as(u16, 3), elements[2]); // last element (4) was dropped
}

test "listDropAt single element list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with a single element
    const data = [_]u8{42};
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Drop the only element (index 0)
    const result = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 0, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), result.len());
    try std.testing.expect(result.isEmpty());
}

test "listDropAt out of bounds" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with 3 elements
    const data = [_]i16{ 10, 20, 30 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());

    // Try to drop at index 5 (out of bounds)
    const result = listDropAt(list, @alignOf(i16), @sizeOf(i16), false, 5, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    defer result.decref(@alignOf(i16), @sizeOf(i16), false, null, rcNone, test_env.getOps());

    // Should return the original list unchanged
    try std.testing.expectEqual(@as(usize, 3), result.len());

    const elements_ptr = result.elements(i16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i16, 10), elements[0]);
    try std.testing.expectEqual(@as(i16, 20), elements[1]);
    try std.testing.expectEqual(@as(i16, 30), elements[2]);
}

test "listReplace basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]u8{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Replace element at index 2 (value 30) with 99
    const new_element: u8 = 99;
    var out_element: u8 = 0;
    const result = listReplace(list, @alignOf(u8), 2, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u8), false, null, rcNone, null, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(u8, 30), out_element); // original value

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 99), elements[2]); // replaced value
    try std.testing.expectEqual(@as(u8, 40), elements[3]);
}

test "listSet releases displaced refcounted element after copy-on-write" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const Counters = struct {
        increments: usize = 0,
        decrements: usize = 0,

        fn incref(context: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const self: *@This() = @ptrCast(@alignCast(context orelse unreachable));
            self.increments += 1;
        }

        fn decref(context: ?*anyopaque, _: ?[*]u8) callconv(.c) void {
            const self: *@This() = @ptrCast(@alignCast(context orelse unreachable));
            self.decrements += 1;
        }
    };

    var counters = Counters{};
    const data = [_]usize{ 10, 20, 30 };
    const list = RocList.fromSlice(usize, data[0..], true, test_env.getOps());
    list.incref(1, true, test_env.getOps());

    const replacement: usize = 99;
    const result = listSet(
        list,
        @alignOf(usize),
        1,
        @ptrCast(@constCast(&replacement)),
        @sizeOf(usize),
        true,
        @ptrCast(&counters),
        &Counters.incref,
        @ptrCast(&counters),
        &Counters.decref,
        .Immutable,
        &copy_fallback,
        test_env.getOps(),
    );
    defer list.decref(@alignOf(usize), @sizeOf(usize), true, @ptrCast(&counters), &Counters.decref, test_env.getOps());
    defer result.decref(@alignOf(usize), @sizeOf(usize), true, @ptrCast(&counters), &Counters.decref, test_env.getOps());

    try std.testing.expect(result.bytes != list.bytes);
    try std.testing.expectEqual(@as(usize, data.len), counters.increments);
    try std.testing.expectEqual(@as(usize, 1), counters.decrements);
    try std.testing.expectEqual(replacement, result.elements(usize).?[1]);
}

test "listReplace first element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for i32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]i32{ 100, 200, 300 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Replace first element (index 0)
    const new_element: i32 = -999;
    var out_element: i32 = 0;
    const result = listReplace(list, @alignOf(i32), 0, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(i32), false, null, rcNone, null, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(i32), @sizeOf(i32), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result.len());
    try std.testing.expectEqual(@as(i32, 100), out_element); // original value

    const elements_ptr = result.elements(i32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i32, -999), elements[0]); // replaced value
    try std.testing.expectEqual(@as(i32, 200), elements[1]);
    try std.testing.expectEqual(@as(i32, 300), elements[2]);
}

test "listReplace last element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]u16{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Replace last element (index 3)
    const new_element: u16 = 9999;
    var out_element: u16 = 0;
    const result = listReplace(list, @alignOf(u16), 3, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u16), false, null, rcNone, null, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(u16, 4), out_element); // original value

    const elements_ptr = result.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 1), elements[0]);
    try std.testing.expectEqual(@as(u16, 2), elements[1]);
    try std.testing.expectEqual(@as(u16, 3), elements[2]);
    try std.testing.expectEqual(@as(u16, 9999), elements[3]); // replaced value
}

test "listReplace single element list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with a single element
    const data = [_]u8{42};
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Replace the only element (index 0)
    const new_element: u8 = 84;
    var out_element: u8 = 0;
    const result = listReplace(list, @alignOf(u8), 0, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u8), false, null, rcNone, null, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), result.len());
    try std.testing.expectEqual(@as(u8, 42), out_element); // original value

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 84), elements[0]); // replaced value
}
// Regression: prior to threading element_width through CopyFn, listReplace via
// the dev wrappers cast a 3-arg copy_fallback into a 2-arg pointer, leaving
// `width` to read garbage from an unpopulated argument register. Wide element
// types (records, anything reaching the copy_fallback branch in
// selectCopyFallbackFn) were silently corrupted. This test exercises that path
// end-to-end through listReplace + copy_fallback with a wide element to verify
// the contents are preserved.
test "listReplace with wide element through copy_fallback" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const Elem = [3]u64; // 24-byte element type—must use copy_fallback (not a specialized helper)
    const elem_align: u32 = @alignOf(Elem);
    const elem_width: usize = @sizeOf(Elem);

    const initial = [_]Elem{ .{ 1, 2, 3 }, .{ 4, 5, 6 }, .{ 7, 8, 9 } };
    const list = RocList.fromSlice(Elem, initial[0..], false, test_env.getOps());

    var out_element: Elem = .{ 0, 0, 0 };
    const new_element: Elem = .{ 100, 200, 300 };

    const result = listReplace(
        list,
        elem_align,
        1, // index
        @as(?[*]u8, @ptrCast(@constCast(&new_element))),
        elem_width,
        false,
        null,
        rcNone,
        null,
        rcNone,
        @as(?[*]u8, @ptrCast(&out_element)),
        &copy_fallback,
        test_env.getOps(),
    );
    defer result.decref(elem_align, elem_width, false, null, rcNone, test_env.getOps());

    // Old element should be fully copied out (all three u64s)
    try std.testing.expectEqual(@as(u64, 4), out_element[0]);
    try std.testing.expectEqual(@as(u64, 5), out_element[1]);
    try std.testing.expectEqual(@as(u64, 6), out_element[2]);

    // New element should be fully copied in (all three u64s)
    const elements_ptr = result.elements(Elem);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u64, 1), elements[0][0]);
    try std.testing.expectEqual(@as(u64, 100), elements[1][0]);
    try std.testing.expectEqual(@as(u64, 200), elements[1][1]);
    try std.testing.expectEqual(@as(u64, 300), elements[1][2]);
    try std.testing.expectEqual(@as(u64, 7), elements[2][0]);
}

test "edge case: listConcat with empty lists" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty1 = RocList.empty();
    const empty2 = RocList.empty();

    const result = listConcat(empty1, empty2, 1, 1, false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), result.len());
    try std.testing.expect(result.isEmpty());
}

test "edge case: listConcat one empty one non-empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    const data = [_]u8{ 1, 2, 3 };
    const non_empty = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Empty + non-empty
    const result1 = listConcat(empty_list, non_empty, 1, 1, false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());
    defer result1.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result1.len());

    // Non-empty + empty
    const empty2 = RocList.empty();
    const non_empty2 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const result2 = listConcat(non_empty2, empty2, 1, 1, false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());
    defer result2.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result2.len());
}

test "edge case: listSublist entire list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i16{ 10, 20, 30 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());

    // Extract entire list as sublist
    const sublist = listSublist(list, @alignOf(i16), @sizeOf(i16), false, 0, 3, null, rcNone, .Immutable, test_env.getOps());
    defer sublist.decref(@alignOf(i16), @sizeOf(i16), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), sublist.len());

    const elements_ptr = sublist.elements(i16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..sublist.len()];
    try std.testing.expectEqual(@as(i16, 10), elements[0]);
    try std.testing.expectEqual(@as(i16, 20), elements[1]);
    try std.testing.expectEqual(@as(i16, 30), elements[2]);
}

test "listSublist transfers ownership from a non-unique source to the returned slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    list.incref(1, false, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    const sublist = listSublist(list, @alignOf(u8), @sizeOf(u8), false, 1, 2, null, rcNone, .Immutable, test_env.getOps());
    try std.testing.expect(sublist.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    sublist.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "edge case: listPrepend to large list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a larger list
    var large_data: [100]u8 = undefined;
    for (large_data[0..], 0..) |*elem, i| {
        elem.* = @as(u8, @intCast(i % 256));
    }
    const list = RocList.fromSlice(u8, large_data[0..], false, test_env.getOps());

    // Prepend an element
    const element: u8 = 255;
    const result = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 101), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 255), elements[0]); // prepended element
    try std.testing.expectEqual(@as(u8, 0), elements[1]); // original first element
    try std.testing.expectEqual(@as(u8, 1), elements[2]); // original second element
}

test "edge case: listWithCapacity zero capacity" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = listWithCapacity(0, @alignOf(u32), @sizeOf(u32), false, null, rcNone, test_env.getOps());
    defer list.decref(@alignOf(u32), @sizeOf(u32), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), list.len());
    try std.testing.expect(list.isEmpty());
}

test "edge case: RocList equality with different capacities" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create two lists with same content but different capacities
    const data = [_]u8{ 1, 2, 3 };
    const list1 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer list1.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Create list with larger capacity
    var list2 = listWithCapacity(10, @alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());
    // Manually set the same content
    list2.length = 3;
    if (list2.bytes) |bytes| {
        for (data, 0..) |val, i| {
            bytes[i] = val;
        }
    }
    defer list2.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Should be equal despite different capacities
    try std.testing.expect(testBytesEqual(list1, list2));
    try std.testing.expect(testBytesEqual(list2, list1));
}

test "seamless slice: seamlessSliceMask functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Regular list should have mask of all zeros
    const data = [_]u8{ 1, 2, 3 };
    const regular_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer regular_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), regular_list.seamlessSliceMask());

    // Empty list should have mask of all zeros
    const empty_list = RocList.empty();
    defer empty_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), empty_list.seamlessSliceMask());
}

test "seamless slice: low-bit encoding matches RocStr convention" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const alloc_ptr: [*]u8 = @ptrFromInt(0x1000);

    const owned_list = RocList{
        .bytes = alloc_ptr,
        .length = 4,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(8),
    };
    try std.testing.expect(!owned_list.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 8), owned_list.getCapacity());

    const slice_list = RocList{
        .bytes = alloc_ptr + 2,
        .length = 2,
        .capacity_or_alloc_ptr = RocList.encodeSliceAllocationPtr(alloc_ptr),
    };
    try std.testing.expect(slice_list.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 2), slice_list.getCapacity());
    try std.testing.expectEqual(@intFromPtr(alloc_ptr), @intFromPtr(slice_list.getAllocationDataPtr(test_env.getOps()).?));
}

test "seamless slice: manual creation and detection" {
    // Test creating a seamless slice manually by setting the low bit
    var seamless_list = RocList{
        .bytes = null,
        .length = 0,
        .capacity_or_alloc_ptr = SEAMLESS_SLICE_TAG,
    };

    try std.testing.expect(seamless_list.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, std.math.maxInt(usize)), seamless_list.seamlessSliceMask());
}

test "complex reference counting: clone behavior" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 5, 10, 15 };
    const original_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Clone should create a new independent copy
    const cloned_list = listClone(original_list, @alignOf(u8), @sizeOf(u8), false, null, rcNone, null, rcNone, test_env.getOps());
    defer cloned_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Cloned list should be unique and have same content
    try std.testing.expect(cloned_list.isUnique(test_env.getOps()));
    try std.testing.expect(testBytesEqual(cloned_list, original_list));
}

test "complex reference counting: empty list operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    defer empty_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Empty lists should handle basic operations gracefully
    try std.testing.expect(empty_list.isUnique(test_env.getOps()));
    try std.testing.expect(empty_list.isEmpty());
    try std.testing.expectEqual(@as(usize, 0), empty_list.len());
}

test "listReplaceInPlace basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]u8{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Replace element at index 2 (value 30) with 99
    const new_element: u8 = 99;
    var out_element: u8 = 0;
    const result = listReplaceInPlace(list, 2, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u8), @as(?[*]u8, @ptrCast(&out_element)), copy_fn);
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(u8, 30), out_element); // original value

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 99), elements[2]); // replaced value
    try std.testing.expectEqual(@as(u8, 40), elements[3]);
}

test "listReplaceInPlace vs listReplace comparison" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const data = [_]u8{ 1, 2, 3, 4, 5 };

    // Test listReplaceInPlace
    const list1 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const new_element1: u8 = 99;
    var out_element1: u8 = 0;
    const result1 = listReplaceInPlace(list1, 2, @as(?[*]u8, @ptrCast(@constCast(&new_element1))), @sizeOf(u8), @as(?[*]u8, @ptrCast(&out_element1)), copy_fn);
    defer result1.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Test listReplace with same parameters
    const list2 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const new_element2: u8 = 99;
    var out_element2: u8 = 0;
    const result2 = listReplace(list2, @alignOf(u8), 2, @as(?[*]u8, @ptrCast(@constCast(&new_element2))), @sizeOf(u8), false, null, rcNone, null, rcNone, @as(?[*]u8, @ptrCast(&out_element2)), copy_fn, test_env.getOps());
    defer result2.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Both should produce the same result
    try std.testing.expect(testBytesEqual(result1, result2));
    try std.testing.expectEqual(out_element1, out_element2);
}

test "listIncref and listDecref public functions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 10, 20, 30 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Should be unique initially
    try std.testing.expect(list.isUnique(test_env.getOps()));

    // Use public listIncref function
    listIncref(list, 1, false, test_env.getOps());

    // Should no longer be unique
    try std.testing.expect(!list.isUnique(test_env.getOps()));

    // Use public listDecref function
    listDecref(list, @alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Should be unique again
    try std.testing.expect(list.isUnique(test_env.getOps()));

    // Final cleanup
    listDecref(list, @alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());
}

test "integration: prepend then drop operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a basic list
    const initial_data = [_]u8{ 5, 10, 15 };
    var list = RocList.fromSlice(u8, initial_data[0..], false, test_env.getOps());

    // Prepend multiple elements
    const element1: u8 = 1;
    list = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());

    const element2: u8 = 2;
    list = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, copy_fn, test_env.getOps());

    // Now we should have [2, 1, 5, 10, 15]
    try std.testing.expectEqual(@as(usize, 5), list.len());

    // Drop the middle element (index 2, value 5)
    list = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 2, null, rcNone, null, rcNone, .Immutable, test_env.getOps());

    // Now we should have [2, 1, 10, 15]
    try std.testing.expectEqual(@as(usize, 4), list.len());

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 1), elements[1]);
    try std.testing.expectEqual(@as(u8, 10), elements[2]);
    try std.testing.expectEqual(@as(u8, 15), elements[3]);
}

test "integration: concat then sublist operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create two lists to concatenate
    const data1 = [_]i16{ 100, 200 };
    const list1 = RocList.fromSlice(i16, data1[0..], false, test_env.getOps());

    const data2 = [_]i16{ 300, 400, 500 };
    const list2 = RocList.fromSlice(i16, data2[0..], false, test_env.getOps());

    // Concatenate them
    const concatenated = listConcat(list1, list2, @alignOf(i16), @sizeOf(i16), false, null, rcNone, null, rcNone, .Immutable, .Immutable, test_env.getOps());

    // Should have [100, 200, 300, 400, 500]
    try std.testing.expectEqual(@as(usize, 5), concatenated.len());

    // Extract a sublist from the middle
    const sublist = listSublist(concatenated, @alignOf(i16), @sizeOf(i16), false, 1, 3, null, rcNone, .Immutable, test_env.getOps());
    defer sublist.decref(@alignOf(i16), @sizeOf(i16), false, null, rcNone, test_env.getOps());

    // Should have [200, 300, 400]
    try std.testing.expectEqual(@as(usize, 3), sublist.len());

    const elements_ptr = sublist.elements(i16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..sublist.len()];
    try std.testing.expectEqual(@as(i16, 200), elements[0]);
    try std.testing.expectEqual(@as(i16, 300), elements[1]);
    try std.testing.expectEqual(@as(i16, 400), elements[2]);
}

test "integration: replace then swap operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a list
    const data = [_]u32{ 10, 20, 30, 40 };
    var list = RocList.fromSlice(u32, data[0..], false, test_env.getOps());

    // Replace element at index 1 (20 -> 99)
    const new_element: u32 = 99;
    var out_element: u32 = 0;
    list = listReplace(list, @alignOf(u32), 1, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u32), false, null, rcNone, null, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());

    try std.testing.expectEqual(@as(u32, 20), out_element);

    // Now we should have [10, 99, 30, 40]
    // Swap elements at indices 0 and 2 (10 <-> 30)
    list = listSwap(list, @alignOf(u32), @sizeOf(u32), 0, 2, false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, copy_fn, test_env.getOps());

    defer list.decref(@alignOf(u32), @sizeOf(u32), false, null, rcNone, test_env.getOps());

    // Now we should have [30, 99, 10, 40]
    try std.testing.expectEqual(@as(usize, 4), list.len());

    const elements_ptr = list.elements(u32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u32, 30), elements[0]); // swapped from index 2
    try std.testing.expectEqual(@as(u32, 99), elements[1]); // replaced value
    try std.testing.expectEqual(@as(u32, 10), elements[2]); // swapped from index 0
    try std.testing.expectEqual(@as(u32, 40), elements[3]); // unchanged
}

test "memory management: capacity boundary conditions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with exact capacity
    const exact_capacity: usize = 10;
    var list = listWithCapacity(exact_capacity, @alignOf(u32), @sizeOf(u32), false, null, rcNone, test_env.getOps());

    try std.testing.expect(list.getCapacity() >= exact_capacity);
    try std.testing.expectEqual(@as(usize, 0), list.len());

    // Use listReserve to ensure we have exactly the capacity we want
    list = listReserve(list, @alignOf(u32), exact_capacity, @sizeOf(u32), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer list.decref(@alignOf(u32), @sizeOf(u32), false, null, rcNone, test_env.getOps());

    // Verify capacity management functions work correctly
    const initial_capacity = list.getCapacity();
    try std.testing.expect(initial_capacity >= exact_capacity);
}

test "memory management: release excess capacity edge cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with minimal data but large capacity
    const data = [_]u8{42};
    const small_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve much more capacity than needed
    const oversized_list = listReserve(small_list, @alignOf(u8), 1000, @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), oversized_list.len());
    try std.testing.expect(oversized_list.getCapacity() >= 1000);

    // Release excess capacity
    const trimmed_list = listReleaseExcessCapacity(oversized_list, @alignOf(u8), @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer trimmed_list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Should maintain content but reduce capacity
    try std.testing.expectEqual(@as(usize, 1), trimmed_list.len());
    try std.testing.expect(trimmed_list.getCapacity() < 1000);
    try std.testing.expect(trimmed_list.getCapacity() >= trimmed_list.len());

    // Verify content is preserved
    const elements_ptr = trimmed_list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..trimmed_list.len()];
    try std.testing.expectEqual(@as(u8, 42), elements[0]);
}

test "boundary conditions: swap with identical indices" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8, _: usize) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const data = [_]u8{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Swap element with itself (index 2 with index 2)
    const swapped = listSwap(list, @alignOf(u8), @sizeOf(u8), 2, 2, false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, copy_fn, test_env.getOps());
    defer swapped.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Should be unchanged
    try std.testing.expectEqual(@as(usize, 4), swapped.len());
    const elements_ptr = swapped.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..swapped.len()];

    for (data, 0..) |expected, i| {
        try std.testing.expectEqual(expected, elements[i]);
    }
}

test "memory management: multiple reserve operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Start with a small list
    const data = [_]u8{ 1, 2 };
    var list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve capacity multiple times, each time increasing
    list = listReserve(list, @alignOf(u8), 10, @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    try std.testing.expect(list.getCapacity() >= 12); // 2 existing + 10 spare

    list = listReserve(list, @alignOf(u8), 20, @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    try std.testing.expect(list.getCapacity() >= 22); // 2 existing + 20 spare

    list = listReserve(list, @alignOf(u8), 5, @sizeOf(u8), false, null, rcNone, null, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    // Should not decrease capacity, so still >= 22
    try std.testing.expect(list.getCapacity() >= 22);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    // Verify content is preserved through all operations
    try std.testing.expectEqual(@as(usize, 2), list.len());
    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
}

test "RocList single-thread incref pairs with single-thread decref" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, ([_]u8{ 1, 2, 3 })[0..], false, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    list.increfWithAtomicity(1, false, .single_thread, test_env.getOps());
    try std.testing.expect(!list.isUnique(test_env.getOps()));

    utils.decref(list.getAllocationDataPtr(test_env.getOps()), list.capacity_or_alloc_ptr, @alignOf(u8), false, .single_thread, test_env.getOps());
    try std.testing.expect(list.isUnique(test_env.getOps()));
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    utils.decref(list.getAllocationDataPtr(test_env.getOps()), list.capacity_or_alloc_ptr, @alignOf(u8), false, .single_thread, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "listSublist InPlace shrinks the unique allocation without a uniqueness check" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4, 5 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const original_bytes = list.bytes;

    const sublist = listSublist(list, @alignOf(u8), @sizeOf(u8), false, 0, 3, null, rcNone, .InPlace, test_env.getOps());
    defer sublist.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(original_bytes, sublist.bytes);
    try std.testing.expect(!sublist.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 3), sublist.len());
    const elements = sublist.elements(u8).?[0..sublist.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
}

test "listPrepend InPlace reuses the unique allocation without a uniqueness check" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 2, 3, 4 };
    var list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    list = listReserve(list, @alignOf(u8), 1, @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
    const original_bytes = list.bytes;

    const element: u8 = 1;
    const result = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, null, rcNone, null, rcNone, .InPlace, &copy_fallback, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(original_bytes, result.bytes);
    try std.testing.expectEqual(@as(usize, 4), result.len());
    const elements = result.elements(u8).?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
    try std.testing.expectEqual(@as(u8, 4), elements[3]);
}

test "listPrepend Immutable copies a shared allocation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const original_bytes = list.bytes;

    // Hold a second reference so the checked path must copy.
    list.incref(1, false, test_env.getOps());

    const element: u8 = 1;
    const result = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, &copy_fallback, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(result.bytes != original_bytes);
    try std.testing.expectEqual(@as(usize, 4), result.len());
    const shared_elements = list.elements(u8).?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 2), shared_elements[0]);
}

test "listPrepend writes into the open slot before a unique seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u16{ 10, 20, 30, 40 };
    var list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const alloc_ptr = list.bytes;

    // Repeatedly pop the front and push a new front, as a stack would.
    var i: u16 = 0;
    while (i < 100) : (i += 1) {
        list = listDropAt(list, @alignOf(u16), @sizeOf(u16), false, 0, null, rcNone, null, rcNone, .Immutable, test_env.getOps());
        try std.testing.expect(list.isSeamlessSlice());
        const element: u16 = i;
        list = listPrepend(list, @alignOf(u16), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u16), false, null, rcNone, null, rcNone, .Immutable, &copy_fallback, test_env.getOps());
        try std.testing.expectEqual(alloc_ptr, list.bytes);
        try std.testing.expectEqual(@as(usize, 4), list.len());
    }
    defer list.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    const elements = list.elements(u16).?[0..list.len()];
    try std.testing.expectEqual(@as(u16, 99), elements[0]);
    try std.testing.expectEqual(@as(u16, 20), elements[1]);
    try std.testing.expectEqual(@as(u16, 30), elements[2]);
    try std.testing.expectEqual(@as(u16, 40), elements[3]);
}

test "listPrepend into a unique seamless slice releases the overwritten refcounted element" {
    const Counter = struct {
        fn dec(ctx: ?*anyopaque, elem: ?[*]u8) callconv(.c) void {
            const seen: *std.ArrayList(u8) = @ptrCast(@alignCast(ctx.?));
            seen.append(std.testing.allocator, elem.?[0]) catch unreachable;
        }
    };

    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var decremented: std.ArrayList(u8) = .empty;
    defer decremented.deinit(std.testing.allocator);

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], true, test_env.getOps());
    const alloc_ptr = list.bytes;

    const slice = listDropAt(list, @alignOf(u8), @sizeOf(u8), true, 0, null, rcNone, &decremented, Counter.dec, .Immutable, test_env.getOps());
    try std.testing.expect(slice.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 0), decremented.items.len);

    const element: u8 = 9;
    const result = listPrepend(slice, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), true, null, rcNone, &decremented, Counter.dec, .Immutable, &copy_fallback, test_env.getOps());
    try std.testing.expectEqual(alloc_ptr, result.bytes);
    try std.testing.expectEqualSlices(u8, &.{1}, decremented.items);

    result.decref(@alignOf(u8), @sizeOf(u8), true, &decremented, Counter.dec, test_env.getOps());
    try std.testing.expectEqualSlices(u8, &.{ 1, 9, 2, 3 }, decremented.items);
}

test "listPrepend copies a shared seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const slice = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 0, null, rcNone, null, rcNone, .Immutable, test_env.getOps());

    // Hold a second reference so the front slot is not the slice's to reuse.
    slice.incref(1, false, test_env.getOps());
    defer slice.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    const element: u8 = 9;
    const result = listPrepend(slice, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, null, rcNone, null, rcNone, .Immutable, &copy_fallback, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, null, rcNone, test_env.getOps());

    try std.testing.expect(!result.isSeamlessSlice());
    try std.testing.expectEqualSlices(u8, &.{ 9, 2, 3 }, result.elements(u8).?[0..result.len()]);
    try std.testing.expectEqual(@as(u8, 1), (slice.bytes.? - 1)[0]);
}

test "listReverse InPlace reverses the unique allocation without a uniqueness check" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u16{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const original_bytes = list.bytes;

    const result = listReverse(list, @alignOf(u16), @sizeOf(u16), false, null, rcNone, null, rcNone, .InPlace, &copy_fallback, test_env.getOps());
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    try std.testing.expectEqual(original_bytes, result.bytes);
    const elements = result.elements(u16).?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 4), elements[0]);
    try std.testing.expectEqual(@as(u16, 3), elements[1]);
    try std.testing.expectEqual(@as(u16, 2), elements[2]);
    try std.testing.expectEqual(@as(u16, 1), elements[3]);
}

test "listReverse Immutable copies a shared allocation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u16{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const original_bytes = list.bytes;

    // Hold a second reference so the checked path must copy.
    list.incref(1, false, test_env.getOps());

    const result = listReverse(list, @alignOf(u16), @sizeOf(u16), false, null, rcNone, null, rcNone, .Immutable, &copy_fallback, test_env.getOps());
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());
    defer list.decref(@alignOf(u16), @sizeOf(u16), false, null, rcNone, test_env.getOps());

    try std.testing.expect(result.bytes != original_bytes);
    const reversed = result.elements(u16).?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 4), reversed[0]);
    try std.testing.expectEqual(@as(u16, 1), reversed[3]);
    const original = list.elements(u16).?[0..list.len()];
    try std.testing.expectEqual(@as(u16, 1), original[0]);
    try std.testing.expectEqual(@as(u16, 4), original[3]);
}
