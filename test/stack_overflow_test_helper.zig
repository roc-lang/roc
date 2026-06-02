//! Helper executable for validating the compiler stack overflow handler output.

const std = @import("std");
const stack_overflow = @import("base").stack_overflow;

pub const std_options: std.Options = .{
    .enable_segfault_handler = false,
};

/// Install the compiler crash handler and trigger the requested crash mode so
/// tests can validate the emitted message in a child process.
pub fn main(init: std.process.Init) noreturn {
    var args = std.process.Args.Iterator.initAllocator(init.minimal.args, std.heap.page_allocator) catch {
        std.debug.print("Failed to read stack overflow helper arguments\n", .{});
        std.process.exit(99);
    };
    _ = args.skip();
    const mode = args.next() orelse "stack-overflow";

    if (std.mem.eql(u8, mode, "thread-stack-overflow")) {
        triggerThreadStackOverflow();
    }

    if (!stack_overflow.installForCurrentThread()) {
        std.debug.print("Failed to install stack overflow handler in helper process\n", .{});
        std.process.exit(98);
    }

    if (std.mem.eql(u8, mode, "stack-overflow")) {
        stack_overflow.triggerStackOverflowForTest();
    } else if (std.mem.eql(u8, mode, "high-access-violation")) {
        triggerHighAccessViolation();
    } else {
        std.debug.print("Unknown stack overflow test helper mode: {s}\n", .{mode});
        std.process.exit(97);
    }
}

fn triggerHighAccessViolation() noreturn {
    var bad_addr: usize = if (comptime @bitSizeOf(usize) >= 64) 0x1_0000_1000 else 0x1000;
    // Read the address back through a volatile pointer so it is a runtime value
    // in a register. A comptime-constant absolute address makes the Zig 0.16
    // x86_64 backend emit `mov [moffs], imm`, which it currently fails to encode.
    const addr_ptr: *volatile usize = &bad_addr;
    const ptr: *volatile u8 = @ptrFromInt(addr_ptr.*);
    ptr.* = 1;
    std.process.exit(96);
}

fn triggerThreadStackOverflow() noreturn {
    const thread = std.Thread.spawn(.{}, struct {
        fn run() void {
            if (!stack_overflow.installForCurrentThread()) {
                std.debug.print("Failed to install stack overflow handler in helper worker thread\n", .{});
                std.process.exit(98);
            }
            stack_overflow.triggerStackOverflowForTest();
        }
    }.run, .{}) catch {
        std.debug.print("Failed to spawn stack overflow helper worker thread\n", .{});
        std.process.exit(95);
    };

    thread.join();
    std.process.exit(94);
}
