const std = @import("std");
const builtins = @import("builtins");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc =  builtins.host_abi.RocAlloc;
const RocDealloc =  builtins.host_abi.RocDealloc;
const RocRealloc =  builtins.host_abi.RocRealloc;
const RocDbg =  builtins.host_abi.RocDbg;
const RocExpectFailed =  builtins.host_abi.RocExpectFailed;
const RocCrashed =  builtins.host_abi.RocCrashed;

pub const TestEnv = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TestEnv {
        return TestEnv {
            allocator = allocator,
        };
    }

    pub fn roc_ops(self: *TestEnv) RocOps {
        return RocOps{
            .env = self,
            .roc_alloc = testRocAlloc,
            .roc_dealloc = testRocDealloc,
            .roc_realloc = testRocRealloc,
            .roc_dbg = testRocDbg,
            .roc_expect_failed = testRocExpectFailed,
            .roc_crashed = testRocCrashed,
            .host_fns = undefined, // Not used in tests
        };
    }
};

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
    const test_env : *TestEnv = @ptrCast(env);

    const slice = test_env.allocator.alloc(u8, alloc_args.length) catch {
        @panic("Test allocation failed");
    };
    alloc_args.answer = slice.ptr;
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
    const test_env : *TestEnv = @ptrCast(env);

    test_env.allocator.free(dealloc_args.ptr);
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
    _ = env;
    _ = realloc_args;
    @panic("testRocRealloc not implemented yet - our current tests don't need realloc");
}

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = dbg_args;
    _ = env;
    @panic("testRocDbg not implemented yet");
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = expect_args;
    _ = env;
    @panic("testRocExpectFailed not implemented yet");
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    _ = env;
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    std.log.err("Test program crashed: {s}", .{msg_slice});
}
