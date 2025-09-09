//! An implementation of RocOps for testing purposes.

const std = @import("std");
const builtins = @import("builtins");
const eval = @import("eval");

const Interpreter = eval.Interpreter;
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

/// An implementation of RocOps for testing purposes.
pub const TestEnv = struct {
    allocator: std.mem.Allocator,
    roc_ops: RocOps,
    interpreter: ?*Interpreter,

    pub fn init(allocator: std.mem.Allocator) TestEnv {
        return TestEnv{
            .allocator = allocator,
            .interpreter = null,
            .roc_ops = RocOps{
                .env = undefined, // set below
                .roc_alloc = testRocAlloc,
                .roc_dealloc = testRocDealloc,
                .roc_realloc = testRocRealloc,
                .roc_dbg = testRocDbg,
                .roc_expect_failed = testRocExpectFailed,
                .roc_crashed = testRocCrashed,
                .host_fns = undefined, // Not used in tests
            },
        };
    }

    pub fn setInterpreter(self: *TestEnv, interp: *Interpreter) void {
        self.interpreter = interp;
    }

    pub fn deinit(self: *TestEnv) void {
        // Clean up crash message if we allocated it
        if (self.interpreter) |interp| {
            if (interp.crash_message) |msg| {
                // Only free if we allocated it (not a string literal)
                if (std.mem.eql(u8, msg, "Failed to store crash message")) {
                    // Don't free string literals
                } else {
                    self.allocator.free(msg);
                }
            }
        }
    }

    pub fn get_ops(self: *TestEnv) *RocOps {
        self.roc_ops.env = @ptrCast(self);
        return &self.roc_ops;
    }
};

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));

    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = test_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        @panic("Out of memory during REPL test allocation");
    };

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));

    // Read the total size from metadata
    const total_size = size_ptr.*;

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);

    // Calculate alignment
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    test_env.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = realloc_args.new_length + size_storage_bytes;

    // Perform reallocation
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = test_env.allocator.realloc(old_slice, new_total_size) catch {
        // On reallocation failure, keep the original allocation unchanged
        // The caller should handle this by checking if the pointer changed
        return;
    };

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = env; // Environment pointer not needed for this implementation

    // Extract debug output from RocDbg struct
    const debug_output = dbg_args.utf8_bytes[0..dbg_args.len];

    // In REPL context, debug output should be displayed to the user
    // Print to stdout so it appears in the REPL output stream
    const stdout = std.io.getStdOut().writer();
    stdout.print("[REPL DBG] {s}\n", .{debug_output}) catch {
        // If we can't write to stdout, try stderr as fallback
        std.io.getStdErr().writer().print("[REPL DBG] {s}\n", .{debug_output}) catch {
            // If all output fails, silently continue - debug output is non-critical
            // We've made best effort to output the debug information
        };
    };
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = env; // Environment pointer not needed for this implementation

    // Extract expect failure message from RocExpectFailed struct
    const expect_output = expect_args.utf8_bytes[0..expect_args.len];

    // In REPL context, expect failures should be displayed to the user as errors
    // Print to stderr with clear indication this is an expect failure
    const stderr = std.io.getStdErr().writer();
    stderr.print("[REPL EXPECT FAILED] {s}\n", .{expect_output}) catch {
        // If we can't write to stderr, try stdout as fallback
        std.io.getStdOut().writer().print("[REPL EXPECT FAILED] {s}\n", .{expect_output}) catch {
            // If all output fails, silently continue - expect output is important but non-fatal
            // The test/REPL will likely fail anyway due to the expect failure
        };
    };

    // In REPL context, expect failures are user errors to be displayed, not fatal
    // Don't exit - let the REPL continue running so the user can fix their code
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];

    // Set crash state on the interpreter if it's available
    if (test_env.interpreter) |interp| {
        interp.has_crashed = true;
        // Store the crash message - we need to allocate and copy it since the original may be temporary
        const owned_msg = test_env.allocator.dupe(u8, msg_slice) catch |err| {
            std.log.err("Failed to allocate crash message: {}", .{err});
            interp.crash_message = "Failed to store crash message";
            return;
        };
        interp.crash_message = owned_msg;
    }
}
