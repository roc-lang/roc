const std = @import("std");
const abi = @import("roc_platform_abi.zig");

const wasm_allocator = std.heap.wasm_allocator;

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

    fn reset(self: *ContractEnv) void {
        self.* = .{};
    }

    fn setReport(self: *ContractEnv, comptime prefix: []const u8, comptime fmt: []const u8, args: anytype) void {
        if (self.failure_count != 0) return;
        const text = std.fmt.bufPrint(&self.report, prefix ++ fmt, args) catch {
            const overflow = prefix ++ "report overflow";
            @memcpy(self.report[0..overflow.len], overflow);
            self.report_len = overflow.len;
            return;
        };
        self.report_len = text.len;
    }

    fn fail(self: *ContractEnv, comptime fmt: []const u8, args: anytype) void {
        self.setReport("FAIL app-model wasm32: ", fmt, args);
        self.failure_count += 1;
    }

    fn allocatorFail(self: *ContractEnv, comptime fmt: []const u8, args: anytype) void {
        self.allocator_error_count += 1;
        self.setReport("FAIL app-model wasm32 allocator: ", fmt, args);
        self.failure_count += 1;
    }

    fn finishPass(self: *ContractEnv) void {
        const text = std.fmt.bufPrint(
            &self.report,
            "PASS glue-runtime app-model ZigGlue wasm32 alloc={} dealloc={}",
            .{ self.alloc_count, self.dealloc_count },
        ) catch "PASS glue-runtime app-model ZigGlue wasm32";
        self.report_len = text.len;
    }

    fn findAllocation(self: *ContractEnv, ptr: *anyopaque) ?*Allocation {
        const needle = @intFromPtr(ptr);
        for (&self.allocations) |*allocation| {
            if (allocation.live and @intFromPtr(allocation.user.?) == needle) return allocation;
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
        const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));
        const raw = wasm_allocator.rawAlloc(total, align_log2, @returnAddress()) orelse {
            self.allocatorFail("wasm allocation failed length={} alignment={}", .{ length, alignment });
            return null;
        };
        const user_addr = std.mem.alignForward(usize, @intFromPtr(raw) + canary_size, alignment);
        const user: [*]u8 = @ptrFromInt(user_addr);
        if (user_addr % alignment != 0) {
            wasm_allocator.rawFree(raw[0..total], align_log2, @returnAddress());
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
            wasm_allocator.rawFree(raw[0..total], align_log2, @returnAddress());
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
        const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, allocation.alignment));
        wasm_allocator.rawFree(allocation.raw.?[0..allocation.total], align_log2, @returnAddress());
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
    _ = .{ bytes, len };
}

fn hostExpectFailed(roc_host_ptr: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = .{ bytes, len };
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    env.fail("roc_expect_failed", .{});
}

fn hostCrashed(roc_host_ptr: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = .{ bytes, len };
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    env.fail("roc_crashed", .{});
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
    _ = .{ bytes, len };
}

export fn roc_expect_failed(bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = .{ bytes, len };
    contract_env.fail("roc_expect_failed", .{});
}

export fn roc_crashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = .{ bytes, len };
    contract_env.fail("roc_crashed", .{});
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

fn runContract() void {
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

    if (contract_env.allocator_error_count != 0) {
        contract_env.fail("allocator recorded {} errors", .{contract_env.allocator_error_count});
    }
    if (contract_env.live_alloc_count != 0) {
        contract_env.fail("live allocations after scenario: {}", .{contract_env.live_alloc_count});
    }

    if (contract_env.failure_count == 0) {
        contract_env.finishPass();
    } else if (contract_env.report_len == 0) {
        const unknown = "FAIL app-model wasm32: unknown failure";
        @memcpy(contract_env.report[0..unknown.len], unknown);
        contract_env.report_len = unknown.len;
    }
}

export fn wasm_main() [*]const u8 {
    contract_env.reset();
    runContract();
    return &contract_env.report;
}

export fn wasm_result_len() usize {
    return contract_env.report_len;
}

export fn wasm_alloc_count() usize {
    return contract_env.alloc_count;
}

export fn wasm_dealloc_count() usize {
    return contract_env.dealloc_count;
}
