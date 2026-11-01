//! The Boxy runtime table numbers a arranged dev image uses (design.md
//! "Deterministic Dev Images").
//!
//! A program numbers its layouts, and the runs of erased argument layouts its
//! erased procedures take, in the order lowering happened to reach them, and
//! which procedures a build lowered depends on what the object cache served
//! it. The image's code passes those numbers to the Boxy runtime, which reads
//! the sidecar tables by them, so a arranged image renumbers what its placed
//! code uses: each number in the order the arranged code first loads it.
//! The sidecar is then built from exactly those entries
//! (`LirImage.buildArrangedSidecarBlob`).

const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");

const LIR = core.LIR;
const Allocator = std.mem.Allocator;

/// Layouts below this index are the store's fixed layouts, the same in every
/// program and never renumbered.
pub const fixed_layout_count: u32 = @backingInt(layout.Idx.i64x2) + 1;

/// What a arranged image's code loads, in arranged order.
pub const Order = struct {
    /// The program layout each canonical layout index past the fixed ones
    /// names, by canonical index minus `fixed_layout_count`.
    layouts: std.ArrayList(layout.Idx) = .empty,
    /// The program's erased argument layout runs, in arranged order; each
    /// one's canonical start is the sum of the lengths before it.
    erased_arg_layouts: std.ArrayList(LIR.BoxySpan) = .empty,

    pub fn deinit(self: *Order, allocator: Allocator) void {
        self.layouts.deinit(allocator);
        self.erased_arg_layouts.deinit(allocator);
        self.* = undefined;
    }
};

/// Gives each program number the code loads its canonical number, at its
/// first load.
pub const Numbering = struct {
    order: Order = .{},
    layout_ids: std.AutoHashMapUnmanaged(layout.Idx, u32) = .empty,
    erased_starts: std.AutoHashMapUnmanaged(LIR.BoxySpan, u32) = .empty,
    erased_len: u32 = 0,

    pub fn deinit(self: *Numbering, allocator: Allocator) void {
        self.order.deinit(allocator);
        self.layout_ids.deinit(allocator);
        self.erased_starts.deinit(allocator);
        self.* = undefined;
    }

    /// The canonical index of a program layout.
    pub fn layoutId(self: *Numbering, allocator: Allocator, program: layout.Idx) Allocator.Error!u32 {
        if (@backingInt(program) < fixed_layout_count) return @backingInt(program);
        const gop = try self.layout_ids.getOrPut(allocator, program);
        if (!gop.found_existing) {
            gop.value_ptr.* = fixed_layout_count + @as(u32, @intCast(self.order.layouts.items.len));
            try self.order.layouts.append(allocator, program);
        }
        return gop.value_ptr.*;
    }

    /// The canonical start of a program run of erased argument layouts.
    pub fn erasedArgLayoutsStart(self: *Numbering, allocator: Allocator, program: LIR.BoxySpan) Allocator.Error!u32 {
        const gop = try self.erased_starts.getOrPut(allocator, program);
        if (!gop.found_existing) {
            gop.value_ptr.* = self.erased_len;
            self.erased_len += program.len;
            try self.order.erased_arg_layouts.append(allocator, program);
        }
        return gop.value_ptr.*;
    }

    /// Give up the order, leaving the numbering empty.
    pub fn takeOrder(self: *Numbering) Order {
        const order = self.order;
        self.order = .{};
        return order;
    }
};

test "runtime table numbers follow first use and keep the fixed layouts" {
    const allocator = std.testing.allocator;
    var numbering: Numbering = .{};
    defer numbering.deinit(allocator);
    const first: layout.Idx = @fromBackingInt(40);
    const second: layout.Idx = @fromBackingInt(31);
    try std.testing.expectEqual(@as(u32, @backingInt(layout.Idx.str)), try numbering.layoutId(allocator, .str));
    try std.testing.expectEqual(fixed_layout_count, try numbering.layoutId(allocator, first));
    try std.testing.expectEqual(fixed_layout_count + 1, try numbering.layoutId(allocator, second));
    try std.testing.expectEqual(fixed_layout_count, try numbering.layoutId(allocator, first));
    try std.testing.expectEqualSlices(layout.Idx, &.{ first, second }, numbering.order.layouts.items);

    try std.testing.expectEqual(@as(u32, 0), try numbering.erasedArgLayoutsStart(allocator, .{ .start = 7, .len = 2 }));
    try std.testing.expectEqual(@as(u32, 2), try numbering.erasedArgLayoutsStart(allocator, .{ .start = 1, .len = 3 }));
    try std.testing.expectEqual(@as(u32, 0), try numbering.erasedArgLayoutsStart(allocator, .{ .start = 7, .len = 2 }));
    try std.testing.expectEqual(@as(usize, 2), numbering.order.erased_arg_layouts.items.len);
}
