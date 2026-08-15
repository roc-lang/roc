const abi = @import("roc_platform_abi.zig");

const wasm_page_size = 65_536;
var failure_count: usize = 0;
var report: [512]u8 = [_]u8{0} ** 512;
var report_len: usize = 0;
var alloc_count: usize = 0;
var dealloc_count: usize = 0;
var heap_cursor: usize = 0;

fn fail(comptime message: []const u8) void {
    if (failure_count == 0) {
        const text = "FAIL type-catalog ZigGlue wasm32: " ++ message;
        @memcpy(report[0..text.len], text);
        report_len = text.len;
    }
    failure_count += 1;
}

fn finishPass() void {
    const message = "PASS glue-runtime type-catalog ZigGlue wasm32";
    @memcpy(report[0..message.len], message);
    report_len = message.len;
}

fn allocRaw(length: usize, alignment: usize) ?*anyopaque {
    if (alignment == 0 or (alignment & (alignment - 1)) != 0) {
        fail("invalid allocation alignment");
        return null;
    }
    if (heap_cursor == 0) {
        const heap_start = @mulWithOverflow(@wasmMemorySize(0), wasm_page_size);
        if (heap_start[1] != 0) {
            fail("wasm memory exhausted");
            return null;
        }
        heap_cursor = heap_start[0];
    }
    const aligned = @addWithOverflow(heap_cursor, alignment - 1);
    if (aligned[1] != 0) {
        fail("allocation alignment overflow");
        return null;
    }
    const ptr = aligned[0] & ~(alignment - 1);
    const end_result = @addWithOverflow(ptr, length);
    if (end_result[1] != 0) {
        fail("allocation overflow");
        return null;
    }
    const end = end_result[0];
    const required_pages = wasmPagesForBytes(end);
    const current_pages = @wasmMemorySize(0);
    if (required_pages > current_pages and @wasmMemoryGrow(0, required_pages - current_pages) == -1) {
        fail("memory grow failed");
        return null;
    }
    heap_cursor = end;
    alloc_count += 1;
    return @ptrFromInt(ptr);
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

fn deallocRaw(ptr: ?*anyopaque, _: usize, _: usize) void {
    _ = ptr orelse return;
    dealloc_count += 1;
}

fn hostAlloc(_: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}

fn hostDealloc(_: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    deallocRaw(ptr, 0, alignment);
}

fn hostRealloc(_: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = ptr;
    return allocRaw(new_length, alignment);
}

fn hostDbg(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {}
fn hostExpectFailed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed");
}
fn hostCrashed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed");
}

var roc_host = abi.RocHost{
    .env = @ptrCast(&failure_count),
    .roc_alloc = &hostAlloc,
    .roc_dealloc = &hostDealloc,
    .roc_realloc = &hostRealloc,
    .roc_dbg = &hostDbg,
    .roc_expect_failed = &hostExpectFailed,
    .roc_crashed = &hostCrashed,
};

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}
export fn roc_dealloc(ptr: ?*anyopaque, alignment: usize) callconv(.c) void {
    deallocRaw(ptr, 0, alignment);
}
export fn roc_realloc(ptr: ?*anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = ptr;
    return allocRaw(new_length, alignment);
}
export fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}
export fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed");
}
export fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed");
}

export fn roc_catalog_roundtrip(arg0: abi.EmptyOrPairOrPayloadOrRecursive) callconv(.c) abi.EmptyOrPairOrPayloadOrRecursive {
    return arg0;
}
export fn roc_catalog_single_no_payload() callconv(.c) *anyopaque {
    return undefined;
}
export fn roc_catalog_single_payload_roundtrip(arg0: abi.CatalogPayload) callconv(.c) abi.CatalogPayload {
    return arg0;
}

fn expectStr(str: *const abi.RocStr, expected: []const u8, comptime message: []const u8) void {
    const actual = str.asSlice();
    if (actual.len != expected.len) return fail(message);
    for (actual, expected) |actual_byte, expected_byte| if (actual_byte != expected_byte) return fail(message);
}

fn runContract() void {
    const point = abi.roc_point();
    if (point.x != -17 or point.y != 42) fail("point mismatch");
    const structural = abi.roc_structural();
    if (structural.count != 19) fail("structural count mismatch");
    expectStr(&structural.name, "catalog", "structural name");
    if (structural.nested.byte != 7 or structural.nested.flag != true) fail("structural nested mismatch");
    const result_a = abi.roc_result_a();
    if (result_a.tag != .Ok) fail("A.Result tag mismatch");
    var a_payload = result_a.payload_ok();
    expectStr(&a_payload, "alpha", "A.Result payload");
    const result_b = abi.roc_result_b();
    if (result_b.tag != .Err) fail("B.Result tag mismatch");
    const b_payload = result_b.payload_err();
    if (b_payload.code != 5) fail("B.Result code mismatch");
    expectStr(&b_payload.message, "bravo", "B.Result message");
    const dec = abi.RocDec{ .num = 1_250_000_000_000_000_000 };
    if (abi.roc_dec(dec).num != dec.num) fail("Dec identity mismatch");
    if (abi.roc_i128(-123456789) != -123456789) fail("I128 identity mismatch");
    if (abi.roc_u128(123456789) != 123456789) fail("U128 identity mismatch");
    result_a.decref(&roc_host);
    result_b.decref(&roc_host);
    structural.decref(&roc_host);
}

export fn wasm_main() [*]const u8 {
    failure_count = 0;
    report_len = 0;
    runContract();
    if (failure_count == 0) finishPass();
    return &report;
}

export fn wasm_result_len() usize {
    return report_len;
}

export fn wasm_alloc_count() usize {
    return alloc_count;
}

export fn wasm_dealloc_count() usize {
    return dealloc_count;
}
