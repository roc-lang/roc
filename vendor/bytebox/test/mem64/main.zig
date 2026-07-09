const std = @import("std");
const bytebox = @import("bytebox");
const Val = bytebox.Val;

pub fn main(process_init: std.process.Init) !void {
    std.debug.print("\nRunning mem64 test...\n", .{});

    const allocator: std.mem.Allocator = process_init.gpa;

    const wasm_data: []u8 = try std.Io.Dir.cwd().readFileAlloc(process_init.io, "zig-out/bin/memtest.wasm", allocator, .limited(1024 * 512));
    defer allocator.free(wasm_data);

    const module_def = try bytebox.createModuleDefinition(allocator, .{});
    defer module_def.destroy();
    try module_def.decode(wasm_data);

    const module_instance = try bytebox.createModuleInstance(.Stack, module_def, allocator);
    defer module_instance.destroy();
    try module_instance.instantiate(.{});

    const handle = try module_instance.getFunctionHandle("memtest");
    const input = [4]Val{ .{ .I32 = 27368 }, .{ .I64 = 34255 }, .{ .F32 = 34234.8690 }, .{ .F64 = 989343.2849 } };
    var output = [1]Val{.{ .I32 = 0 }};
    try module_instance.invoke(handle, &input, &output, .{});

    if (output[0].I32 != 0) {
        return error.TestFailed;
    }

    std.debug.print("success\n", .{});
}
