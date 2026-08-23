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
        const text = "FAIL duplicate-tags ZigGlue wasm32: " ++ message;
        @memcpy(report[0..text.len], text);
        report_len = text.len;
    }
    failure_count += 1;
}

fn finishPass() void {
    const message = "PASS glue-runtime duplicate-tags ZigGlue wasm32";
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

fn okResult(comptime T: type) T {
    var result: T = undefined;
    const bytes: [*]u8 = @ptrCast(&result);
    @memset(bytes[0..@sizeOf(T)], 0);
    result.tag = .Ok;
    return result;
}

export fn roc_a_unit(arg0: abi.RocStr) callconv(.c) abi.AUnitResult {
    _ = arg0;
    return okResult(abi.AUnitResult);
}
export fn roc_a_str(arg0: abi.RocStr) callconv(.c) abi.AStrResult {
    _ = arg0;
    return okResult(abi.AStrResult);
}
export fn roc_a_bytes(arg0: abi.RocStr) callconv(.c) abi.ABytesResult {
    _ = arg0;
    return okResult(abi.ABytesResult);
}
export fn roc_a_record(arg0: abi.RocStr) callconv(.c) abi.ARecordResult {
    _ = arg0;
    return okResult(abi.ARecordResult);
}
export fn roc_a_nested(arg0: abi.RocStr) callconv(.c) abi.ANestedResult {
    _ = arg0;
    return okResult(abi.ANestedResult);
}

export fn roc_b_unit(arg0: abi.RocStr) callconv(.c) abi.BUnitResult {
    _ = arg0;
    return okResult(abi.BUnitResult);
}
export fn roc_b_str(arg0: abi.RocStr) callconv(.c) abi.BStrResult {
    _ = arg0;
    return okResult(abi.BStrResult);
}
export fn roc_b_bytes(arg0: abi.RocStr) callconv(.c) abi.BBytesResult {
    _ = arg0;
    return okResult(abi.BBytesResult);
}
export fn roc_b_record(arg0: abi.RocStr) callconv(.c) abi.BRecordResult {
    _ = arg0;
    return okResult(abi.BRecordResult);
}
export fn roc_b_nested(arg0: abi.RocStr) callconv(.c) abi.BNestedResult {
    _ = arg0;
    return okResult(abi.BNestedResult);
}

export fn roc_c_unit(arg0: abi.RocStr) callconv(.c) abi.CUnitResult {
    _ = arg0;
    return okResult(abi.CUnitResult);
}
export fn roc_c_str(arg0: abi.RocStr) callconv(.c) abi.CStrResult {
    _ = arg0;
    return okResult(abi.CStrResult);
}
export fn roc_c_bytes(arg0: abi.RocStr) callconv(.c) abi.CBytesResult {
    _ = arg0;
    return okResult(abi.CBytesResult);
}
export fn roc_c_record(arg0: abi.RocStr) callconv(.c) abi.CRecordResult {
    _ = arg0;
    return okResult(abi.CRecordResult);
}
export fn roc_c_nested(arg0: abi.RocStr) callconv(.c) abi.CNestedResult {
    _ = arg0;
    return okResult(abi.CNestedResult);
}

export fn roc_d_unit(arg0: abi.RocStr) callconv(.c) abi.DUnitResult {
    _ = arg0;
    return okResult(abi.DUnitResult);
}
export fn roc_d_str(arg0: abi.RocStr) callconv(.c) abi.DStrResult {
    _ = arg0;
    return okResult(abi.DStrResult);
}
export fn roc_d_bytes(arg0: abi.RocStr) callconv(.c) abi.DBytesResult {
    _ = arg0;
    return okResult(abi.DBytesResult);
}
export fn roc_d_record(arg0: abi.RocStr) callconv(.c) abi.DRecordResult {
    _ = arg0;
    return okResult(abi.DRecordResult);
}
export fn roc_d_nested(arg0: abi.RocStr) callconv(.c) abi.DNestedResult {
    _ = arg0;
    return okResult(abi.DNestedResult);
}

export fn roc_fallible_unit(arg0: abi.RocStr) callconv(.c) abi.HostFallible_unitResult {
    _ = arg0;
    return okResult(abi.HostFallible_unitResult);
}
export fn roc_fallible_str(arg0: abi.RocStr) callconv(.c) abi.HostFallible_strResult {
    _ = arg0;
    return okResult(abi.HostFallible_strResult);
}
export fn roc_fallible_bytes(arg0: abi.RocStr) callconv(.c) abi.HostFallible_bytesResult {
    _ = arg0;
    return okResult(abi.HostFallible_bytesResult);
}
export fn roc_fallible_record(arg0: abi.RocStr) callconv(.c) abi.HostFallible_recordResult {
    _ = arg0;
    return okResult(abi.HostFallible_recordResult);
}
export fn roc_fallible_nested(arg0: abi.RocStr) callconv(.c) abi.HostFallible_nestedResult {
    _ = arg0;
    return okResult(abi.HostFallible_nestedResult);
}

export fn wasm_main() [*]const u8 {
    failure_count = 0;
    report_len = 0;
    abi.roc_main();
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
