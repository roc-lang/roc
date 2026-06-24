//! Shared symbol-ABI bridge used by shim archives.
//!
//! The generated platform shim defines the hosted dispatch table. Each concrete
//! shim root exports `roc_shim_get_ops` and delegates here to build the RocOps
//! value over host-provided runtime symbols.

const std = @import("std");
const builtins = @import("builtins");

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;

const extern_host = struct {
    extern fn roc_alloc(length: usize, alignment: usize) ?*anyopaque;
    extern fn roc_dealloc(ptr: *anyopaque, alignment: usize) void;
    extern fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque;
    extern fn roc_dbg(bytes: [*]const u8, len: usize) void;
    extern fn roc_expect_failed(bytes: [*]const u8, len: usize) void;
    extern fn roc_crashed(bytes: [*]const u8, len: usize) void;
};

/// Hosted dispatch table defined by the generated platform shim module, in
/// hosted-section order.
extern const roc_shim_hosted_fns: [*]const builtins.host_abi.HostedFn;
extern const roc_shim_hosted_count: usize;

fn shimAlloc(_: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return extern_host.roc_alloc(length, alignment);
}

fn shimDealloc(_: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    extern_host.roc_dealloc(ptr, alignment);
}

fn shimRealloc(_: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return extern_host.roc_realloc(ptr, new_length, alignment);
}

fn shimDbg(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    extern_host.roc_dbg(bytes, len);
}

fn shimExpectFailed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    extern_host.roc_expect_failed(bytes, len);
}

fn shimCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    extern_host.roc_crashed(bytes, len);
}

var shim_ops: RocOps = undefined;
var shim_ops_initialized = false;

/// Return the process-global RocOps value backed by host runtime symbols.
pub fn getOps() *RocOps {
    if (!shim_ops_initialized) {
        shim_ops = .{
            .env = @ptrCast(&shim_ops),
            .roc_alloc = shimAlloc,
            .roc_dealloc = shimDealloc,
            .roc_realloc = shimRealloc,
            .roc_dbg = shimDbg,
            .roc_expect_failed = shimExpectFailed,
            .roc_crashed = shimCrashed,
            .hosted_fns = .{
                .count = @intCast(roc_shim_hosted_count),
                .fns = @constCast(roc_shim_hosted_fns),
            },
        };
        shim_ops_initialized = true;
    }
    return &shim_ops;
}

/// Return the RocOps value as an opaque pointer for the C ABI export.
pub fn getOpsOpaque() *anyopaque {
    return @ptrCast(getOps());
}

/// Build the default platform's `List(Str)` CLI argument value.
pub fn buildDefaultRunCliArgs(app_args: []const [*:0]const u8, gpa: Allocator) Allocator.Error!RocList {
    if (app_args.len == 0) return RocList.empty();

    const ops = getOps();
    const roc_strs = try gpa.alloc(RocStr, app_args.len);
    defer gpa.free(roc_strs);

    for (app_args, 0..) |arg_z, index| {
        const arg = std.mem.span(arg_z);
        const sanitized = try sanitizeUtf8(arg, gpa);
        defer if (sanitized.ptr != arg.ptr) gpa.free(sanitized);
        roc_strs[index] = RocStr.fromSlice(sanitized, ops);
    }

    return RocList.fromSlice(RocStr, roc_strs, true, ops);
}

fn sanitizeUtf8(input: []const u8, gpa: Allocator) Allocator.Error![]const u8 {
    if (std.unicode.utf8ValidateSlice(input)) return input;

    const buf = try gpa.alloc(u8, input.len * 3);
    var out_i: usize = 0;
    var in_i: usize = 0;
    while (in_i < input.len) {
        const seq_len = std.unicode.utf8ByteSequenceLength(input[in_i]) catch {
            buf[out_i] = 0xEF;
            buf[out_i + 1] = 0xBF;
            buf[out_i + 2] = 0xBD;
            out_i += 3;
            in_i += 1;
            continue;
        };
        if (in_i + seq_len > input.len) {
            buf[out_i] = 0xEF;
            buf[out_i + 1] = 0xBF;
            buf[out_i + 2] = 0xBD;
            out_i += 3;
            in_i += 1;
            continue;
        }
        if (std.unicode.utf8Decode(input[in_i..][0..seq_len])) |_| {
            @memcpy(buf[out_i..][0..seq_len], input[in_i..][0..seq_len]);
            out_i += seq_len;
            in_i += seq_len;
        } else |_| {
            buf[out_i] = 0xEF;
            buf[out_i + 1] = 0xBF;
            buf[out_i + 2] = 0xBD;
            out_i += 3;
            in_i += 1;
        }
    }
    return try gpa.realloc(buf, out_i);
}
