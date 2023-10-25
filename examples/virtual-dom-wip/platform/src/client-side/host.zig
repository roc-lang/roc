const std = @import("std");
const str = @import("glue").str;
const builtin = @import("builtin");
const RocStr = str.RocStr;

const Align = extern struct { a: usize, b: usize };
extern fn malloc(size: usize) callconv(.C) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dest: *anyopaque, src: *anyopaque, count: usize) *anyopaque;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = alignment;

    return malloc(size);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = old_size;
    _ = alignment;

    return realloc(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = alignment;

    free(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))));
}

export fn roc_panic(message: RocStr, tag_id: u32) callconv(.C) void {
    _ = tag_id;
    const msg = @as([*:0]const u8, @ptrCast(c_ptr));
    const stderr = std.io.getStdErr().writer();
    stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,
    capacity: usize,
};

const FromHost = extern struct {
    eventHandlerId: usize,
    eventJsonList: ?RocList,
    eventPlatformState: ?*anyopaque,
    initJson: RocList,
    isInitEvent: bool,
};

const ToHost = extern struct {
    platformState: *anyopaque,
    eventPreventDefault: bool,
    eventStopPropagation: bool,
};

extern fn roc__main_1_exposed(FromHost) callconv(.C) ToHost;

var platformState: ?*anyopaque = null;

// Called from JS
export fn roc_vdom_init(init_pointer: ?[*]u8, init_length: usize, init_capacity: usize) callconv(.C) void {
    const init_json = RocList{
        .bytes = init_pointer,
        .length = init_length,
        .capacity = init_capacity,
    };
    const from_host = FromHost{
        .eventHandlerId = std.math.maxInt(usize),
        .eventJsonList = null,
        .eventPlatformState = null,
        .initJson = init_json,
        .isInitEvent = true,
    };
    const to_host = roc__main_1_exposed(from_host);
    platformState = to_host.platformState;
}

// Called from JS
export fn roc_dispatch_event(list_ptr: ?[*]u8, list_length: usize, handler_id: usize) usize {
    const json_list = RocList{
        .bytes = list_ptr,
        .length = list_length,
        .capacity = list_length,
    };
    const from_host = FromHost{
        .eventHandlerId = handler_id,
        .eventJsonList = json_list,
        .eventPlatformState = platformState,
        .initJson = null,
        .isInitEvent = false,
    };
    const to_host = roc__main_1_exposed(from_host);
    platformState = to_host.platformState;
    return to_host.eventPreventDefault << 1 | to_host.eventStopPropagation;
}
