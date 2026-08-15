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
        self.setReport("FAIL app-model wasm32: ", message);
        self.failure_count += 1;
    }

    fn allocatorFail(self: *ContractEnv, comptime message: []const u8) void {
        self.allocator_error_count += 1;
        self.setReport("FAIL app-model wasm32 allocator: ", message);
        self.failure_count += 1;
    }

    fn finishPass(self: *ContractEnv) void {
        const message = "PASS glue-runtime app-model ZigGlue wasm32";
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
        if (self.heap_cursor == 0) {
            const heap_start = @mulWithOverflow(@wasmMemorySize(0), wasm_page_size);
            if (heap_start[1] != 0) {
                self.allocatorFail("wasm memory exhausted");
                return null;
            }
            self.heap_cursor = heap_start[0];
        }
        const raw = alignForward(self.heap_cursor, alignment) orelse {
            self.allocatorFail("bump alignment overflow");
            return null;
        };
        const end_result = @addWithOverflow(raw, total);
        if (end_result[1] != 0) {
            self.allocatorFail("bump allocation overflow");
            return null;
        }
        const end = end_result[0];
        const required_pages = wasmPagesForBytes(end);
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
        const user_start = @addWithOverflow(@intFromPtr(raw), canary_size);
        if (user_start[1] != 0) {
            self.allocatorFail("user pointer overflow");
            return null;
        }
        const user_addr = alignForward(user_start[0], alignment) orelse {
            self.allocatorFail("user pointer alignment overflow");
            return null;
        };
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

fn alignForward(value: usize, alignment: usize) ?usize {
    const sum = @addWithOverflow(value, alignment - 1);
    if (sum[1] != 0) return null;
    return sum[0] & ~(alignment - 1);
}

fn wasmPagesForBytes(byte_count: usize) usize {
    return byte_count / wasm_page_size + @intFromBool(byte_count % wasm_page_size != 0);
}

comptime {
    const max_usize = ~@as(usize, 0);
    if (wasmPagesForBytes(max_usize) != max_usize / wasm_page_size + 1) {
        @compileError("wasm page rounding must handle the usize limit");
    }
}

fn bytesEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |a_byte, b_byte| if (a_byte != b_byte) return false;
    return true;
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

fn resetMsg() abi.Msg {
    var msg: abi.Msg = undefined;
    const bytes: [*]u8 = @ptrCast(&msg);
    @memset(bytes[0..@sizeOf(abi.Msg)], 0);
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

    const title = view.title;
    const title_slice = rocStrSlice(&title) orelse {
        contract_env.fail("render title has null bytes");
        view.decref(&roc_host);
        return;
    };
    if (!bytesEqual(title_slice, "ready")) {
        contract_env.fail("render title mismatch");
    }
    if (view.lifecycle.tag != .Ready) {
        contract_env.fail("render lifecycle mismatch");
    }
    if (view.messages.length != 0) {
        contract_env.fail("render messages expected empty");
    }
    view.decref(&roc_host);

    if (contract_env.allocator_error_count != 0) {
        contract_env.fail("allocator recorded errors");
    }
    if (contract_env.live_alloc_count != 0) {
        contract_env.fail("live allocations after scenario");
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
