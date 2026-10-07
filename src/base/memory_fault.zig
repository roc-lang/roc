//! Classification and formatting of memory faults for crash handlers.
//!
//! Everything here is a pure function of its arguments: no globals, no
//! thread-locals, no operating-system calls. The compiler's crash handler
//! (`signal_handler.zig`) and the freestanding default-platform runtime both
//! decide "stack overflow or stray access?" through `classifyFault`, so the two
//! can never disagree about what counts as a stack overflow.

const std = @import("std");

/// Stack address range for one thread, including guard-page bounds when known.
pub const StackBounds = struct {
    low: usize,
    high: usize,
    page_size: usize,
    guard_low: ?usize = null,
    guard_high: ?usize = null,

    pub fn init(low: usize, high: usize, page_size: usize, guard_low: ?usize, guard_high: ?usize) ?StackBounds {
        if (low == 0 or high <= low or !std.math.isPowerOfTwo(page_size)) return null;
        if (guard_low == null or guard_high == null) {
            return .{ .low = low, .high = high, .page_size = page_size };
        }
        if (guard_high.? <= guard_low.?) return null;
        return .{
            .low = low,
            .high = high,
            .page_size = page_size,
            .guard_low = guard_low,
            .guard_high = guard_high,
        };
    }

    pub fn containsGuardAddress(self: StackBounds, addr: usize) bool {
        const guard_low = self.guard_low orelse return false;
        const guard_high = self.guard_high orelse return false;
        return addr >= guard_low and addr < guard_high;
    }

    pub fn containsLowerBoundaryAddress(self: StackBounds, fault_addr: usize) bool {
        const boundary_low = self.low -| self.page_size;
        const boundary_high = self.low;

        return fault_addr >= boundary_low and fault_addr < boundary_high;
    }
};

/// Classification for a memory fault observed by a crash handler.
pub const FaultKind = enum {
    stack_overflow,
    access_violation,
};

/// Distance from the stack pointer within which a fault counts as a stack
/// overflow. A genuine overflow faults inside the frame being set up—the
/// stack-probe/push that ran past the guard—so the fault address is close to
/// the stack pointer. A null or wild write faults far from it. This proximity
/// test is the primary, bounds-independent signal, because the reported stack
/// bounds are not always trustworthy: `pthread_getattr_np` on a static-musl
/// main thread reports a region that does not contain the real stack pointer,
/// and a rule built on those bounds alone would call any low-address (e.g.
/// null) write an overflow. The window is far larger than any real frame yet
/// far smaller than the gap between a high stack and a null/low pointer.
pub const stack_overflow_proximity: usize = 16 * 1024 * 1024;

/// Classify a fault from its address, the interrupted stack pointer, and the
/// faulting thread's stack range. A caller passes `null` for whichever of the
/// stack pointer and the bounds it cannot obtain exactly.
pub fn classifyFault(fault_addr: usize, stack_pointer: ?usize, bounds: ?StackBounds) FaultKind {
    // Primary signal (bounds-independent): a fault adjacent to the stack
    // pointer is the overflowing access itself.
    if (stack_pointer) |sp| {
        const distance = if (fault_addr >= sp) fault_addr - sp else sp - fault_addr;
        if (distance <= stack_overflow_proximity) return .stack_overflow;
    }

    // Secondary signals, used only when we trust the reported bounds: the stack
    // pointer or the fault sits in the guard page / just below the stack.
    const stack_bounds = bounds orelse return .access_violation;

    if (stack_pointer) |sp| {
        if (stack_bounds.containsGuardAddress(sp)) return .stack_overflow;
        if (stack_bounds.containsLowerBoundaryAddress(sp)) return .stack_overflow;
    }

    if (stack_bounds.containsGuardAddress(fault_addr)) return .stack_overflow;
    if (stack_bounds.containsLowerBoundaryAddress(fault_addr)) return .stack_overflow;
    return .access_violation;
}

/// Format a pointer-sized integer as lowercase hexadecimal into caller storage.
pub fn formatHex(value: usize, buf: []u8) []const u8 {
    const hex_chars = "0123456789abcdef";
    var i: usize = buf.len;

    if (value == 0) {
        i -= 1;
        buf[i] = '0';
    } else {
        var v = value;
        while (v > 0 and i > 2) {
            i -= 1;
            buf[i] = hex_chars[v & 0xf];
            v >>= 4;
        }
    }

    i -= 1;
    buf[i] = 'x';
    i -= 1;
    buf[i] = '0';

    return buf[i..];
}

test "formatHex" {
    var buf: [18]u8 = undefined;

    const zero = formatHex(0, &buf);
    try std.testing.expectEqualStrings("0x0", zero);

    const small = formatHex(0xff, &buf);
    try std.testing.expectEqualStrings("0xff", small);

    const medium = formatHex(0xdeadbeef, &buf);
    try std.testing.expectEqualStrings("0xdeadbeef", medium);
}

test "classifyFault uses only exact stack data" {
    const bounds = StackBounds.init(0x7000, 0x9000, 0x1000, 0x6000, 0x7000).?;
    const unrelated_addr: usize = 0x5000_0000;

    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x9000, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6800, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5000, null, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6800, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, null, null));
}

test "classifyFault treats a lower stack boundary fault as overflow" {
    const bounds = StackBounds.init(0x7000, 0x9000, 0x1000, null, null).?;

    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6ff0, null, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6000, null, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6ff0, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x1000_1000, 0x6ff0, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5ff0, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x1000_1000, 0x8000, bounds));
}

test "classifyFault reports a null/wild write as access violation, not overflow" {
    // A JIT'd eval object that stores through an unrelocated GOT slot writes to
    // address 0 while the stack pointer is healthy and deep inside the stack.
    // That is an access violation; calling it a stack overflow would send a
    // one-line relocation bug down a stack-overflow investigation.
    const bounds = StackBounds.init(0x7000_0000, 0x7080_0000, 0x1000, 0x6fff_f000, 0x7000_0000).?;
    const healthy_sp: usize = 0x7040_0000;

    // Null write.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0, healthy_sp, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0, null, bounds));
    // Arbitrary wild write far from the stack.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0xdead_0000, healthy_sp, bounds));
    // A genuine overflow (fault just below the stack) still classifies as overflow.
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6fff_f800, healthy_sp, bounds));
}

test "classifyFault: a stack pointer below reported bounds is not overflow on its own" {
    // When the reported bounds are unreliable (a real hazard: pthread_getattr_np
    // on a static-musl main thread reports a stack the real sp isn't in), a
    // "stack pointer below low" must not be treated as overflow by itself—the
    // fault has to corroborate by being near the stack pointer.
    const bounds = StackBounds.init(0x7000, 0x9000, 0x1000, null, null).?;

    // Fault far from the (below-bounds) sp: a null/wild write, not an overflow.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5000_0000, 0x5000, bounds));
    if (comptime @bitSizeOf(usize) >= 64) {
        try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0xffff_fd1f_fea0, 0x5000, bounds));
    }
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5000_0000, 0x9000, bounds));
    // Fault adjacent to the sp: a genuine overflow, even with untrustworthy bounds.
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x4040, 0x5000, bounds));
}

test "classifyFault without stack bounds decides by distance from the stack pointer" {
    // The default-platform Linux runtime has no libc to ask for its stack
    // range, so it classifies with the interrupted stack pointer alone.
    if (comptime @bitSizeOf(usize) < 64) return error.SkipZigTest;

    const sp: usize = 0x7ffd_4000_0000;

    // The push or store that ran off the end of the stack lands beside sp.
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(sp - 8, sp, null));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(sp + 0x1000, sp, null));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(sp - stack_overflow_proximity, sp, null));

    // A null dereference, a read through an uninitialized thread pointer, and a
    // wild heap pointer all fault far from a healthy sp.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0, sp, null));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x10, sp, null));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5555_5555_0000, sp, null));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(sp - stack_overflow_proximity - 1, sp, null));

    // With neither a stack pointer nor bounds there is no evidence of an overflow.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(sp - 8, null, null));
}
