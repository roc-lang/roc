const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const maxInt = std.math.maxInt;

comptime {
    // This is a workaround for https://github.com/ziglang/zig/issues/8218
    // which is only necessary on macOS.
    //
    // Once that issue is fixed, we can undo the changes in
    // 177cf12e0555147faa4d436e52fc15175c2c4ff0 and go back to passing
    // -fcompiler-rt in link.rs instead of doing this. Note that this
    // workaround is present in many host.zig files, so make sure to undo
    // it everywhere!
    if (std.builtin.os.tag == .macos) {
        _ = @import("compiler_rt");
    }
}

const mem = std.mem;
const Allocator = mem.Allocator;

const WindowConfig = extern struct {
    // These need to be in alignment followed by alphabetical order.
    height: usize,
    title: RocStr,
    width: usize,
};
const Context = extern struct {
    // These need to be in alignment followed by alphabetical order.
    props: WindowConfig,
    handler: [*]u8,
};
extern fn roc__contextForHost_1_exposed() Context;
extern fn roc__contextForHost_1_exposed_generic([*]u8) void;
extern fn roc__contextForHost_size() i64;

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.C) ?*align(Align) c_void;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*c_void;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void {
    return malloc(size);
}

export fn roc_realloc(c_ptr: *c_void, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*c_void {
    return realloc(@alignCast(Align, @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    free(@alignCast(Align, @ptrCast([*]u8, c_ptr)));
}

export fn roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void {
    _ = tag_id;

    const stderr = std.io.getStdErr().writer();
    const msg = @ptrCast([*:0]const u8, c_ptr);
    stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

export fn roc_memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void {
    return memcpy(dst, src, size);
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void {
    return memset(dst, value, size);
}

const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

const SDL_WINDOWPOS_UNDEFINED = @bitCast(c_int, c.SDL_WINDOWPOS_UNDEFINED_MASK);
pub export fn main() callconv(.C) u8 {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    // Initialize the SDL and then get window properties from Roc.
    if (c.SDL_Init(c.SDL_INIT_VIDEO) != 0) {
        c.SDL_Log("Unable to initialize SDL: %s", c.SDL_GetError());
        return 1;
    }
    
    const size = @intCast(usize, roc__contextForHost_size());
    stdout.print("Roc size: {}\n", .{size}) catch unreachable;
    stdout.print("Host size: {}\n", .{@sizeOf(Context)}) catch unreachable;
    const context = roc__contextForHost_1_exposed();
    const window_props = context.props;

    stdout.print("{}\n", .{window_props.width}) catch unreachable;
    stdout.print("{}\n", .{window_props.height}) catch unreachable;
    stdout.print("{s}\n", .{window_props.title.asSlice()}) catch unreachable;

    const window = c.SDL_CreateWindow(
        window_props.title.asU8ptr(),
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        @intCast(c_int, window_props.width),
        @intCast(c_int, window_props.height),
        c.SDL_WINDOW_OPENGL,
    ) orelse unreachable;

    const renderer = c.SDL_CreateRenderer(window, -1, 0) orelse unreachable;

    // const size = @intCast(usize, roc__handleEventForHost_size());
    // const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    // var output = @ptrCast([*]u8, raw_output);
    // defer { allocator.free(raw_output); }

    var quit = false;
    while (!quit) {
        var event: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&event) != 0) {
            switch (event.@"type") {
                c.SDL_QUIT => {
                    quit = true;
                },
                else => {
                    // roc__handleEventForHost_1_exposed_generic(output);
                    // const closure_data_pointer = @ptrCast([*]u8, output);
                    // call_the_closure(closure_data_pointer);
                },
            }
        }

        _ = c.SDL_RenderClear(renderer);
        // _ = c.SDL_RenderCopy(renderer, zig_texture, 0, 0);
        c.SDL_RenderPresent(renderer);

        c.SDL_Delay(17);
    }
    return 0;
}

fn call_the_closure(closure_data_pointer: [*]u8) void {
    const allocator = std.heap.page_allocator;

    const size = roc__handleEventForHost_1_Fx_result_size();
    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        allocator.free(raw_output);
    }

    const flags: u8 = 0;

    roc__handleEventForHost_1_Fx_caller(&flags, closure_data_pointer, output);

    const elements = @ptrCast([*]u64, @alignCast(8, output));

    var flag = elements[0];

    if (flag == 0) {
        return;
    } else {
        unreachable;
    }
}

export fn roc_fx_putLine(rocPath: str.RocStr) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();

    for (rocPath.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }

    stdout.print("\n", .{}) catch unreachable;
}

fn readLine() []u8 {
    const stdin = std.io.getStdIn().reader();
    return (stdin.readUntilDelimiterOrEof(&line_buf, '\n') catch unreachable) orelse "";
}
