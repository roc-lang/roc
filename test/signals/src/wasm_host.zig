//! Browser-oriented Signals platform host symbols for wasm32 builds.
//!
//! This file is intentionally limited to the direct symbol ABI needed to link a
//! Signals Roc app as a wasm reactor. The JavaScript DOM/runtime protocol is a
//! later layer; this host provides allocation, diagnostics, and HostValue cells.

const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const render = @import("render_commands.zig");
const signal_graph = @import("signal_graph.zig");
const scope_tree = @import("scope_tree.zig");
const identity_table = @import("identity_table.zig");

const HostValue = u64;
const HostValueTypeTag = *u64;

comptime {
    const BuildRecord = struct { id: u64 };
    const BuildRow = struct { site_ordinal: u64 };
    _ = signal_graph.Node(BuildRecord);
    _ = scope_tree.Scope(BuildRow);
    _ = scope_tree.Branch.false_branch.opposite();
    _ = identity_table.NodeIdentity;
    _ = identity_table.DomIdentity;
}

const HostValueCell = struct {
    box: abi.RocBox,
    tag: ?HostValueTypeTag,
};

const HostValueSlot = union(enum) {
    vacant,
    occupied: HostValueCell,
};

var host_values: std.ArrayListUnmanaged(HostValueSlot) = .empty;
var command_buffer: render.Buffer = .{};
var roc_host_env: u8 = 0;
var roc_host = abi.RocHost{
    .env = @ptrCast(&roc_host_env),
    .roc_alloc = &rocAllocForAbi,
    .roc_dealloc = &rocDeallocForAbi,
    .roc_realloc = &rocReallocForAbi,
    .roc_dbg = &rocDbgForAbi,
    .roc_expect_failed = &rocExpectFailedForAbi,
    .roc_crashed = &rocCrashedForAbi,
};

fn failHost() noreturn {
    @trap();
}

fn checkedAdd(left: usize, right: usize) usize {
    return std.math.add(usize, left, right) catch failHost();
}

fn alignmentFromBytes(alignment: usize) std.mem.Alignment {
    if (alignment == 0 or !std.math.isPowerOfTwo(alignment)) failHost();
    return @enumFromInt(std.math.log2_int(usize, alignment));
}

fn allocWithHeader(length: usize, alignment_arg: usize) ?*anyopaque {
    const alignment = @max(alignment_arg, @sizeOf(usize));
    const align_log2 = alignmentFromBytes(alignment);
    const header_size = alignment;
    const total_size = checkedAdd(length, header_size);

    const base = std.heap.wasm_allocator.rawAlloc(total_size, align_log2, @returnAddress()) orelse return null;
    const size_ptr: *usize = @ptrCast(@alignCast(base));
    size_ptr.* = total_size;
    return @ptrCast(base + header_size);
}

fn deallocWithHeader(ptr: *anyopaque, alignment_arg: usize) void {
    const alignment = @max(alignment_arg, @sizeOf(usize));
    const align_log2 = alignmentFromBytes(alignment);
    const header_size = alignment;
    const user_ptr: [*]u8 = @ptrCast(ptr);
    const base = user_ptr - header_size;
    const size_ptr: *const usize = @ptrCast(@alignCast(base));
    const total_size = size_ptr.*;
    std.heap.wasm_allocator.rawFree(base[0..total_size], align_log2, @returnAddress());
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocWithHeader(length, alignment);
}

export fn roc_dealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    deallocWithHeader(ptr, alignment);
}

export fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment_arg: usize) callconv(.c) ?*anyopaque {
    const alignment = @max(alignment_arg, @sizeOf(usize));
    const header_size = alignment;
    const user_ptr: [*]u8 = @ptrCast(ptr);
    const old_base = user_ptr - header_size;
    const old_size_ptr: *const usize = @ptrCast(@alignCast(old_base));
    const old_total_size = old_size_ptr.*;
    const old_user_size = old_total_size - header_size;

    const new_ptr = allocWithHeader(new_length, alignment) orelse return null;
    const new_user_ptr: [*]u8 = @ptrCast(new_ptr);
    @memcpy(new_user_ptr[0..@min(old_user_size, new_length)], user_ptr[0..@min(old_user_size, new_length)]);
    deallocWithHeader(ptr, alignment);
    return new_ptr;
}

export fn roc_ui_command_buffer_ptr() callconv(.c) usize {
    return command_buffer.ptrAddress();
}

export fn roc_ui_command_buffer_len() callconv(.c) usize {
    return command_buffer.len();
}

export fn roc_ui_command_record_words() callconv(.c) usize {
    return render.Record.word_count;
}

export fn roc_ui_command_buffer_clear() callconv(.c) void {
    command_buffer.clearRetainingCapacity();
}

export fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    failHost();
}

fn rocAllocForAbi(_: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_alloc(length, alignment);
}

fn rocDeallocForAbi(_: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    roc_dealloc(ptr, alignment);
}

fn rocReallocForAbi(_: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_realloc(ptr, new_length, alignment);
}

fn rocDbgForAbi(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_dbg(bytes, len);
}

fn rocExpectFailedForAbi(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_expect_failed(bytes, len);
}

fn rocCrashedForAbi(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_crashed(bytes, len);
}

fn tagFromBox(box: abi.RocBox) HostValueTypeTag {
    return @ptrCast(@alignCast(box orelse failHost()));
}

fn retainBox(box: abi.RocBox) void {
    abi.increfBox(box, 1);
}

fn releaseBox(box: abi.RocBox) void {
    abi.decrefBox(box, &roc_host);
}

fn tagId(tag: HostValueTypeTag) u64 {
    return tag.*;
}

fn tagsMatch(actual: HostValueTypeTag, expected: HostValueTypeTag) bool {
    return actual == expected or (tagId(actual) != 0 and tagId(actual) == tagId(expected));
}

fn storeHostValue(box: abi.RocBox, tag: ?HostValueTypeTag) HostValue {
    for (host_values.items, 0..) |*slot, index| {
        switch (slot.*) {
            .vacant => {
                slot.* = .{ .occupied = .{ .box = box, .tag = tag } };
                return @intCast(index + 1);
            },
            .occupied => {},
        }
    }

    host_values.append(std.heap.wasm_allocator, .{ .occupied = .{ .box = box, .tag = tag } }) catch failHost();
    return @intCast(host_values.items.len);
}

fn hostValueSlot(value: HostValue) *HostValueSlot {
    if (value == 0) failHost();
    const index = value - 1;
    if (index >= host_values.items.len) failHost();
    return &host_values.items[@intCast(index)];
}

fn occupiedCell(value: HostValue) HostValueCell {
    return switch (hostValueSlot(value).*) {
        .vacant => failHost(),
        .occupied => |cell| cell,
    };
}

fn assertTag(value: HostValue, expected: HostValueTypeTag) void {
    const actual = occupiedCell(value).tag orelse failHost();
    if (!tagsMatch(actual, expected)) failHost();
}

export fn roc_host_value_clone(value: HostValue) callconv(.c) HostValue {
    const cell = occupiedCell(value);
    retainBox(cell.box);
    if (cell.tag) |tag| retainBox(@ptrCast(tag));
    return storeHostValue(cell.box, cell.tag);
}

export fn roc_host_value_get(value: HostValue) callconv(.c) abi.RocBox {
    const cell = occupiedCell(value);
    retainBox(cell.box);
    return cell.box;
}

export fn roc_host_value_get_tagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    defer releaseBox(@ptrCast(tag));
    assertTag(value, tag);
    return roc_host_value_get(value);
}

export fn roc_host_value_store(box: abi.RocBox) callconv(.c) HostValue {
    return storeHostValue(box, null);
}

export fn roc_host_value_store_tagged(box: abi.RocBox, tag_box: abi.RocBox) callconv(.c) HostValue {
    return storeHostValue(box, tagFromBox(tag_box));
}

export fn roc_host_value_take(value: HostValue) callconv(.c) abi.RocBox {
    const slot = hostValueSlot(value);
    return switch (slot.*) {
        .vacant => failHost(),
        .occupied => |cell| blk: {
            slot.* = .vacant;
            if (cell.tag) |tag| releaseBox(@ptrCast(tag));
            break :blk cell.box;
        },
    };
}

export fn roc_host_value_take_tagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    defer releaseBox(@ptrCast(tag));
    assertTag(value, tag);
    return roc_host_value_take(value);
}
