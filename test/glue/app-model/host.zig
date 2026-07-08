const std = @import("std");
const abi = @import("roc_platform_abi.zig");

extern fn malloc(size: usize) callconv(.c) ?*anyopaque;
extern fn free(ptr: ?*anyopaque) callconv(.c) void;
extern fn write(fd: c_int, buf: [*]const u8, count: usize) callconv(.c) isize;
extern fn exit(code: c_int) callconv(.c) noreturn;

const max_allocations = 512;
const canary_size = 16;
const canary_byte: u8 = 0xA5;
const poison_byte: u8 = 0xCC;

const Allocation = struct {
    raw: ?[*]u8 = null,
    user: ?[*]u8 = null,
    length: usize = 0,
    total: usize = 0,
    alignment: usize = 0,
    live: bool = false,
};

const ContractEnv = struct {
    allocations: [max_allocations]Allocation = [_]Allocation{.{}} ** max_allocations,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    live_alloc_count: usize = 0,
    allocator_error_count: usize = 0,
    failure_count: usize = 0,
    report: [1024]u8 = [_]u8{0} ** 1024,
    report_len: usize = 0,

    fn fail(self: *ContractEnv, comptime fmt: []const u8, args: anytype) void {
        if (self.failure_count == 0) {
            const text = std.fmt.bufPrint(&self.report, "FAIL app-model: " ++ fmt, args) catch {
                const overflow = "FAIL app-model: report overflow";
                @memcpy(self.report[0..overflow.len], overflow);
                self.report_len = overflow.len;
                self.failure_count += 1;
                return;
            };
            self.report_len = text.len;
        }
        self.failure_count += 1;
    }

    fn allocatorFail(self: *ContractEnv, comptime fmt: []const u8, args: anytype) void {
        self.allocator_error_count += 1;
        if (self.failure_count == 0) {
            const text = std.fmt.bufPrint(&self.report, "FAIL app-model: allocator: " ++ fmt, args) catch {
                const overflow = "FAIL app-model: allocator report overflow";
                @memcpy(self.report[0..overflow.len], overflow);
                self.report_len = overflow.len;
                self.failure_count += 1;
                return;
            };
            self.report_len = text.len;
        }
        self.failure_count += 1;
    }

    fn findAllocation(self: *ContractEnv, ptr: *anyopaque) ?*Allocation {
        const needle = @intFromPtr(ptr);
        for (&self.allocations) |*allocation| {
            if (allocation.live) {
                if (@intFromPtr(allocation.user.?) == needle) return allocation;
            }
        }
        return null;
    }

    fn checkCanaries(self: *ContractEnv, allocation: *const Allocation) bool {
        const user = allocation.user.?;
        for (0..canary_size) |i| {
            if ((user - canary_size)[i] != canary_byte) {
                self.allocatorFail("prefix canary changed", .{});
                return false;
            }
            if ((user + allocation.length)[i] != canary_byte) {
                self.allocatorFail("suffix canary changed", .{});
                return false;
            }
        }
        return true;
    }

    fn alloc(self: *ContractEnv, length: usize, alignment: usize) ?*anyopaque {
        if (alignment == 0 or (alignment & (alignment - 1)) != 0) {
            self.allocatorFail("invalid alignment {}", .{alignment});
            return null;
        }
        if (length > std.math.maxInt(usize) - canary_size - canary_size - alignment) {
            self.allocatorFail("allocation size overflow length={} alignment={}", .{ length, alignment });
            return null;
        }

        const total = canary_size + alignment - 1 + length + canary_size;
        const raw_any = malloc(total) orelse {
            self.allocatorFail("malloc failed length={} alignment={}", .{ length, alignment });
            return null;
        };
        const raw: [*]u8 = @ptrCast(raw_any);
        const user_addr = std.mem.alignForward(usize, @intFromPtr(raw) + canary_size, alignment);
        const user: [*]u8 = @ptrFromInt(user_addr);
        if (user_addr % alignment != 0) {
            free(raw_any);
            self.allocatorFail("returned pointer is not aligned to {}", .{alignment});
            return null;
        }

        var slot: ?*Allocation = null;
        for (&self.allocations) |*allocation| {
            if (!allocation.live) {
                slot = allocation;
                break;
            }
        }
        const allocation = slot orelse {
            free(raw_any);
            self.allocatorFail("allocation table exhausted", .{});
            return null;
        };

        @memset((user - canary_size)[0..canary_size], canary_byte);
        @memset(user[0..length], poison_byte);
        @memset((user + length)[0..canary_size], canary_byte);

        allocation.* = .{
            .raw = raw,
            .user = user,
            .length = length,
            .total = total,
            .alignment = alignment,
            .live = true,
        };
        self.alloc_count += 1;
        self.live_alloc_count += 1;
        return @ptrCast(user);
    }

    fn dealloc(self: *ContractEnv, ptr: ?*anyopaque, alignment: usize) void {
        const raw_ptr = ptr orelse return;
        const allocation = self.findAllocation(raw_ptr) orelse {
            self.allocatorFail("unknown or double free for {*}", .{raw_ptr});
            return;
        };
        if (allocation.alignment != alignment) {
            self.allocatorFail("dealloc alignment mismatch allocated={} freed={}", .{ allocation.alignment, alignment });
        }
        _ = self.checkCanaries(allocation);
        @memset(allocation.user.?[0..allocation.length], 0xDD);
        free(@ptrCast(allocation.raw.?));
        allocation.live = false;
        self.dealloc_count += 1;
        self.live_alloc_count -= 1;
    }

    fn realloc(self: *ContractEnv, ptr: ?*anyopaque, new_length: usize, alignment: usize) ?*anyopaque {
        const raw_ptr = ptr orelse return self.alloc(new_length, alignment);
        const old = self.findAllocation(raw_ptr) orelse {
            self.allocatorFail("realloc unknown pointer {*}", .{raw_ptr});
            return null;
        };
        if (old.alignment != alignment) {
            self.allocatorFail("realloc alignment mismatch allocated={} requested={}", .{ old.alignment, alignment });
            return null;
        }
        if (!self.checkCanaries(old)) return null;

        const old_user = old.user.?;
        const copy_length = @min(old.length, new_length);
        const new_ptr = self.alloc(new_length, alignment) orelse return null;
        const new_user: [*]u8 = @ptrCast(new_ptr);
        @memcpy(new_user[0..copy_length], old_user[0..copy_length]);
        if (!std.mem.eql(u8, new_user[0..copy_length], old_user[0..copy_length])) {
            self.allocatorFail("realloc did not preserve old bytes", .{});
        }
        self.dealloc(raw_ptr, alignment);
        return new_ptr;
    }
};

var contract_env: ContractEnv = .{};
var roc_host = abi.RocHost{
    .env = @ptrCast(&contract_env),
    .roc_alloc = &hostAlloc,
    .roc_dealloc = &hostDealloc,
    .roc_realloc = &hostRealloc,
    .roc_dbg = &hostDbg,
    .roc_expect_failed = &hostExpectFailed,
    .roc_crashed = &hostCrashed,
};

fn writeStderr(bytes: []const u8) void {
    if (bytes.len == 0) return;
    _ = write(2, bytes.ptr, bytes.len);
}

fn writeStderrLine(bytes: []const u8) void {
    writeStderr(bytes);
    writeStderr("\n");
}

fn stderrPrint(comptime fmt: []const u8, args: anytype) void {
    var buffer: [512]u8 = undefined;
    const text = std.fmt.bufPrint(&buffer, fmt, args) catch "stderr format overflow\n";
    writeStderr(text);
}

fn hostAlloc(roc_host_ptr: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    return env.alloc(length, alignment);
}

fn hostDealloc(roc_host_ptr: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    env.dealloc(ptr, alignment);
}

fn hostRealloc(roc_host_ptr: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    return env.realloc(ptr, new_length, alignment);
}

fn hostDbg(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderrLine(bytes[0..len]);
}

fn hostExpectFailed(roc_host_ptr: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderrLine(bytes[0..len]);
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    env.fail("roc_expect_failed", .{});
}

fn hostCrashed(roc_host_ptr: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderrLine(bytes[0..len]);
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    env.fail("roc_crashed", .{});
    exit(1);
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return contract_env.alloc(length, alignment);
}

export fn roc_dealloc(ptr: ?*anyopaque, alignment: usize) callconv(.c) void {
    contract_env.dealloc(ptr, alignment);
}

export fn roc_realloc(ptr: ?*anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return contract_env.realloc(ptr, new_length, alignment);
}

export fn roc_dbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderrLine(bytes[0..len]);
}

export fn roc_expect_failed(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderrLine(bytes[0..len]);
    contract_env.fail("roc_expect_failed", .{});
}

export fn roc_crashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderrLine(bytes[0..len]);
    contract_env.fail("roc_crashed", .{});
    exit(1);
}

fn resetMsg() abi.Msg {
    var msg = std.mem.zeroes(abi.Msg);
    msg.tag = .Reset;
    return msg;
}

fn rocStrSlice(str: *const abi.RocStr) ?[]const u8 {
    if (str.isSmallStr()) {
        const bytes: [*]const u8 = @ptrCast(str);
        return bytes[0..str.len()];
    }
    const bytes = str.bytes orelse return null;
    return bytes[0..str.len()];
}

fn runAppModelContract() void {
    const initial = abi.roc_init();
    const updated = abi.roc_update(initial, resetMsg());
    const view = abi.roc_render(updated);

    const title = view.@"title";
    const title_slice = rocStrSlice(&title) orelse {
        contract_env.fail("render title has null bytes len={}", .{title.len()});
        view.decref(&roc_host);
        return;
    };
    if (!std.mem.eql(u8, title_slice, "ready")) {
        contract_env.fail("render title mismatch len={}", .{title_slice.len});
    }
    if (view.@"lifecycle".tag != .Ready) {
        contract_env.fail("render lifecycle expected Ready got {}", .{@intFromEnum(view.@"lifecycle".tag)});
    }
    if (view.@"messages".length != 0) {
        contract_env.fail("render messages expected empty got {}", .{view.@"messages".length});
    }
    view.decref(&roc_host);
}

export fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = .{ argc, argv };
    runAppModelContract();
    if (contract_env.allocator_error_count != 0) {
        contract_env.fail("allocator recorded {} errors", .{contract_env.allocator_error_count});
    }
    if (contract_env.live_alloc_count != 0) {
        contract_env.fail("live allocations after scenario: {}", .{contract_env.live_alloc_count});
    }

    if (contract_env.failure_count != 0) {
        const message = if (contract_env.report_len == 0) "FAIL app-model: unknown failure" else contract_env.report[0..contract_env.report_len];
        stderrPrint("{s}\nalloc_count={} dealloc_count={} live={} allocator_errors={}\n", .{
            message,
            contract_env.alloc_count,
            contract_env.dealloc_count,
            contract_env.live_alloc_count,
            contract_env.allocator_error_count,
        });
        return 1;
    }

    stderrPrint("PASS glue-runtime app-model ZigGlue native alloc={} dealloc={}\n", .{ contract_env.alloc_count, contract_env.dealloc_count });
    return 0;
}
