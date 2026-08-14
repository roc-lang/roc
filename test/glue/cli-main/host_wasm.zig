const abi = @import("roc_platform_abi.zig");

const max_allocations = 512;
const canary_size = 16;
const canary_byte: u8 = 0xA5;
const poison_byte: u8 = 0xCC;
const wasm_page_size = 65_536;

const Allocation = struct {
    user: ?[*]u8 = null,
    length: usize = 0,
    alignment: usize = 0,
    live: bool = false,
};

const ContractEnv = struct {
    allocations: [max_allocations]Allocation = [_]Allocation{.{}} ** max_allocations,
    heap_cursor: usize = 0,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    live_alloc_count: usize = 0,
    allocator_error_count: usize = 0,
    failure_count: usize = 0,
    log_count: usize = 0,
    report: [1024]u8 = [_]u8{0} ** 1024,
    report_len: usize = 0,

    fn reset(self: *ContractEnv) void {
        self.* = .{};
    }

    fn setReport(self: *ContractEnv, comptime prefix: []const u8, comptime message: []const u8) void {
        if (self.failure_count != 0) return;
        const text = prefix ++ message;
        @memcpy(self.report[0..text.len], text);
        self.report_len = text.len;
    }

    fn fail(self: *ContractEnv, comptime message: []const u8) void {
        self.setReport("FAIL cli-main wasm32: ", message);
        self.failure_count += 1;
    }

    fn allocatorFail(self: *ContractEnv, comptime message: []const u8) void {
        self.allocator_error_count += 1;
        self.setReport("FAIL cli-main wasm32 allocator: ", message);
        self.failure_count += 1;
    }

    fn finishPass(self: *ContractEnv) void {
        const message = "PASS glue-runtime cli-main ZigGlue wasm32";
        @memcpy(self.report[0..message.len], message);
        self.report_len = message.len;
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
                self.allocatorFail("prefix canary changed");
                return false;
            }
            if ((user + allocation.length)[i] != canary_byte) {
                self.allocatorFail("suffix canary changed");
                return false;
            }
        }
        return true;
    }

    fn bumpAlloc(self: *ContractEnv, total: usize, alignment: usize) ?[*]u8 {
        if (self.heap_cursor == 0) self.heap_cursor = @wasmMemorySize(0) * wasm_page_size;
        const raw = alignForward(self.heap_cursor, alignment);
        const end = raw + total;
        if (end < raw) {
            self.allocatorFail("bump allocation overflow");
            return null;
        }
        const required_pages = (end + wasm_page_size - 1) / wasm_page_size;
        const current_pages = @wasmMemorySize(0);
        if (required_pages > current_pages and @wasmMemoryGrow(0, required_pages - current_pages) == -1) {
            self.allocatorFail("wasm memory grow failed");
            return null;
        }
        self.heap_cursor = end;
        return @ptrFromInt(raw);
    }

    fn alloc(self: *ContractEnv, length: usize, alignment: usize) ?*anyopaque {
        if (alignment == 0 or (alignment & (alignment - 1)) != 0) {
            self.allocatorFail("invalid alignment");
            return null;
        }
        if (length > ~@as(usize, 0) - canary_size - canary_size - alignment) {
            self.allocatorFail("allocation size overflow");
            return null;
        }

        const total = canary_size + alignment - 1 + length + canary_size;
        const raw = self.bumpAlloc(@max(total, 1), alignment) orelse return null;
        const user_addr = alignForward(@intFromPtr(raw) + canary_size, alignment);
        const user: [*]u8 = @ptrFromInt(user_addr);
        if (user_addr % alignment != 0) {
            self.allocatorFail("returned pointer is not aligned");
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
            self.allocatorFail("allocation table exhausted");
            return null;
        };

        @memset((user - canary_size)[0..canary_size], canary_byte);
        @memset(user[0..length], poison_byte);
        @memset((user + length)[0..canary_size], canary_byte);

        allocation.* = .{
            .user = user,
            .length = length,
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
            self.allocatorFail("unknown or double free");
            return;
        };
        if (allocation.alignment != alignment) {
            self.allocatorFail("dealloc alignment mismatch");
        }
        _ = self.checkCanaries(allocation);
        @memset(allocation.user.?[0..allocation.length], 0xDD);
        allocation.live = false;
        self.dealloc_count += 1;
        self.live_alloc_count -= 1;
    }

    fn realloc(self: *ContractEnv, ptr: ?*anyopaque, new_length: usize, alignment: usize) ?*anyopaque {
        const raw_ptr = ptr orelse return self.alloc(new_length, alignment);
        const old = self.findAllocation(raw_ptr) orelse {
            self.allocatorFail("realloc unknown pointer");
            return null;
        };
        if (old.alignment != alignment) {
            self.allocatorFail("realloc alignment mismatch");
            return null;
        }
        if (!self.checkCanaries(old)) return null;

        const old_user = old.user.?;
        const copy_length = @min(old.length, new_length);
        const new_ptr = self.alloc(new_length, alignment) orelse return null;
        const new_user: [*]u8 = @ptrCast(new_ptr);
        @memcpy(new_user[0..copy_length], old_user[0..copy_length]);
        if (!bytesEqual(new_user[0..copy_length], old_user[0..copy_length])) {
            self.allocatorFail("realloc did not preserve old bytes");
        }
        self.dealloc(raw_ptr, alignment);
        return new_ptr;
    }
};

fn alignForward(value: usize, alignment: usize) usize {
    return (value + alignment - 1) & ~(alignment - 1);
}

fn bytesEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |a_byte, b_byte| if (a_byte != b_byte) return false;
    return true;
}

fn zeroed(comptime T: type) T {
    var value: T = undefined;
    const bytes: [*]u8 = @ptrCast(&value);
    @memset(bytes[0..@sizeOf(T)], 0);
    return value;
}

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
    env.fail("roc_expect_failed");
}

fn hostCrashed(roc_host_ptr: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = .{ bytes, len };
    const env: *ContractEnv = @ptrCast(@alignCast(roc_host_ptr.env));
    env.fail("roc_crashed");
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
    contract_env.fail("roc_expect_failed");
}

export fn roc_crashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = .{ bytes, len };
    contract_env.fail("roc_crashed");
}

export fn roc_cli_read() callconv(.c) abi.RocStr {
    return abi.RocStr.fromSlice("contract-input", &roc_host);
}

export fn roc_cli_log(arg0: abi.RocStr) callconv(.c) void {
    const expected = "roc saw contract-input argc=2 first=alpha";
    var owned = arg0;
    const actual = owned.asSlice();
    if (!bytesEqual(actual, expected)) {
        contract_env.fail("unexpected log payload");
    }
    contract_env.log_count += 1;
    owned.decref(&roc_host);
}

export fn roc_cli_many(
    arg0: u8,
    arg1: u16,
    arg2: u32,
    arg3: u64,
    arg4: u128,
    arg5: i8,
    arg6: i16,
    arg7: i32,
    arg8: i64,
    arg9: i128,
    arg10: f32,
    arg11: f64,
    arg12: abi.RocDec,
    arg13: bool,
    arg14: abi.RocStr,
) callconv(.c) abi.CliHostManyResult {
    _ = .{ arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13 };
    var owned = arg14;
    owned.decref(&roc_host);
    contract_env.fail("roc_cli_many was called");
    return zeroed(abi.CliHostManyResult);
}

export fn roc_cli_shape(arg0: abi.CircleOrEmptyOrRect, arg1: abi.CliHostShapeArg1) callconv(.c) abi.CliHostNamedRecord {
    _ = .{ arg0, arg1 };
    contract_env.fail("roc_cli_shape was called");
    return zeroed(abi.CliHostNamedRecord);
}

export fn roc_cli_wide(arg0: abi.RocDec, arg1: i128, arg2: u128) callconv(.c) abi.CliHostWide {
    _ = .{ arg0, arg1, arg2 };
    contract_env.fail("roc_cli_wide was called");
    return zeroed(abi.CliHostWide);
}

fn validateRefcountedListHeader(list: abi.RocList(abi.RocStr)) void {
    const elements = list.elements_ptr orelse {
        contract_env.fail("argument list has null elements");
        return;
    };
    const count_ptr: *const usize = @ptrFromInt(@intFromPtr(elements) - (2 * @sizeOf(usize)));
    if (count_ptr.* != list.length) {
        contract_env.fail("refcounted list element count header mismatch");
    }
}

fn makeArgs() abi.RocList(abi.RocStr) {
    var items = [_]abi.RocStr{
        abi.RocStr.fromSlice("alpha", &roc_host),
        abi.RocStr.fromSlice("beta", &roc_host),
    };
    const list = abi.RocList(abi.RocStr).fromSlice(items[0..], &roc_host);
    validateRefcountedListHeader(list);
    return list;
}

fn runContract() void {
    const args = makeArgs();
    const result = abi.roc_main(args);
    if (result.tag != .Ok) {
        contract_env.fail("roc_main returned Err");
    }
    result.decref(&roc_host);

    if (contract_env.log_count != 1) {
        contract_env.fail("expected one log call");
    }
    if (contract_env.allocator_error_count != 0) {
        contract_env.fail("allocator recorded errors");
    }
    if (contract_env.live_alloc_count != 0) {
        contract_env.fail("live allocations after scenario");
    }

    if (contract_env.failure_count == 0) {
        contract_env.finishPass();
    } else if (contract_env.report_len == 0) {
        const unknown = "FAIL cli-main wasm32: unknown failure";
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
