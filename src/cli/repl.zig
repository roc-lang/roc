//! CLI REPL implementation
//!
//! Provides the interactive Read-Eval-Print-Loop for the Roc CLI.

const std = @import("std");
const builtins = @import("builtins");
const eval = @import("eval");
const repl_mod = @import("repl");
const Repl = repl_mod.Repl;

const cli_context = @import("CliContext.zig");
const CliContext = cli_context.CliContext;

/// An implementation of RocOps for the CLI REPL.
const ReplOps = struct {
    allocator: std.mem.Allocator,
    crash: eval.CrashContext,
    roc_ops: builtins.host_abi.RocOps,

    const RocOps = builtins.host_abi.RocOps;
    const RocAlloc = builtins.host_abi.RocAlloc;
    const RocDealloc = builtins.host_abi.RocDealloc;
    const RocRealloc = builtins.host_abi.RocRealloc;
    const RocDbg = builtins.host_abi.RocDbg;
    const RocExpectFailed = builtins.host_abi.RocExpectFailed;
    const RocCrashed = builtins.host_abi.RocCrashed;

    pub fn init(allocator: std.mem.Allocator) ReplOps {
        return ReplOps{
            .allocator = allocator,
            .crash = eval.CrashContext.init(allocator),
            .roc_ops = builtins.host_abi.RocOps{
                .env = undefined, // set in get_ops()
                .roc_alloc = replRocAlloc,
                .roc_dealloc = replRocDealloc,
                .roc_realloc = replRocRealloc,
                .roc_dbg = replRocDbg,
                .roc_expect_failed = replRocExpectFailed,
                .roc_crashed = replRocCrashed,
                .hosted_fns = .{ .count = 0, .fns = undefined },
            },
        };
    }

    pub fn deinit(self: *ReplOps) void {
        self.crash.deinit();
    }

    pub fn get_ops(self: *ReplOps) *builtins.host_abi.RocOps {
        self.roc_ops.env = @ptrCast(self);
        self.crash.reset();
        return &self.roc_ops;
    }

    pub fn crashContextPtr(self: *ReplOps) *eval.CrashContext {
        return &self.crash;
    }

    fn replRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const repl_env: *ReplOps = @ptrCast(@alignCast(env));

        const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));

        // Calculate additional bytes needed to store the size
        const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
        const total_size = alloc_args.length + size_storage_bytes;

        // Allocate memory including space for size metadata
        const result = repl_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());

        const base_ptr = result orelse {
            std.debug.panic("Out of memory during replRocAlloc", .{});
        };

        // Store the total size (including metadata) right before the user data
        const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
        size_ptr.* = total_size;

        // Return pointer to the user data (after the size metadata)
        alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
    }

    fn replRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
        const repl_env: *ReplOps = @ptrCast(@alignCast(env));

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
        repl_env.allocator.rawFree(slice, align_enum, @returnAddress());
    }

    fn replRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const repl_env: *ReplOps = @ptrCast(@alignCast(env));

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
        const new_slice = repl_env.allocator.realloc(old_slice, new_total_size) catch {
            std.debug.panic("Out of memory during replRocRealloc", .{});
        };

        // Store the new total size in the metadata
        const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
        new_size_ptr.* = new_total_size;

        // Return pointer to the user data (after the size metadata)
        realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
    }

    fn replRocDbg(dbg_args: *const RocDbg, _: *anyopaque) callconv(.c) void {
        const message = dbg_args.utf8_bytes[0..dbg_args.len];
        std.debug.print("[dbg] {s}\n", .{message});
    }

    fn replRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
        const repl_env: *ReplOps = @ptrCast(@alignCast(env));
        const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
        const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
        // Format and record the message
        const formatted = std.fmt.allocPrint(repl_env.allocator, "Expect failed: {s}", .{trimmed}) catch {
            std.debug.panic("failed to allocate REPL expect failure message", .{});
        };
        repl_env.crash.recordCrash(formatted) catch |err| {
            repl_env.allocator.free(formatted);
            std.debug.panic("failed to store REPL expect failure: {}", .{err});
        };
    }

    fn replRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
        const repl_env: *ReplOps = @ptrCast(@alignCast(env));
        repl_env.crash.recordCrash(crashed_args.utf8_bytes[0..crashed_args.len]) catch |err| {
            std.debug.panic("failed to store REPL crash message: {}", .{err});
        };
    }
};

/// Run the interactive REPL
pub fn run(ctx: *CliContext) !void {
    const stdout = ctx.io.stdout();

    // Print welcome banner
    stdout.print("Roc REPL\n", .{}) catch {};
    stdout.print("Type :help for help, :exit to quit\n\n", .{}) catch {};

    // Initialize ReplOps and REPL
    var repl_ops = ReplOps.init(ctx.gpa);
    defer repl_ops.deinit();

    var repl_instance = Repl.init(ctx.gpa, repl_ops.get_ops(), repl_ops.crashContextPtr()) catch |err| {
        ctx.io.stderr().print("Failed to initialize REPL: {}\n", .{err}) catch {};
        return error.NotImplemented;
    };
    defer repl_instance.deinit();

    // Read-eval-print loop
    const stdin_file = std.fs.File.stdin();
    var read_buffer: [4096]u8 = undefined;
    var stdin = stdin_file.reader(&read_buffer);
    const stdin_reader = stdin.interface.adaptToOldInterface();
    var line_buffer: [4096]u8 = undefined;

    while (true) {
        // Print prompt
        stdout.print("» ", .{}) catch {};
        ctx.io.flush();

        // Read line
        const line = stdin_reader.readUntilDelimiterOrEof(&line_buffer, '\n') catch |err| {
            ctx.io.stderr().print("Error reading input: {}\n", .{err}) catch {};
            break;
        } orelse break; // EOF (Ctrl+D)

        // Evaluate and print result
        const result = repl_instance.step(line) catch |err| {
            ctx.io.stderr().print("Error: {}\n", .{err}) catch {};
            continue;
        };
        defer ctx.gpa.free(result);

        if (result.len > 0) {
            stdout.print("{s}\n", .{result}) catch {};
        }

        // Check for quit command (handled internally by step returning "Goodbye!")
        if (std.mem.eql(u8, result, "Goodbye!")) {
            break;
        }
    }
}
