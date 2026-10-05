const std = @import("std");
const bytebox = @import("bytebox");

test "decode, instantiate, and execute a WebAssembly export" {
    const wasm = &[_]u8{
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
        0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f, 0x03,
        0x02, 0x01, 0x00, 0x07, 0x07, 0x01, 0x03, 0x72,
        0x75, 0x6e, 0x00, 0x00, 0x0a, 0x06, 0x01, 0x04,
        0x00, 0x41, 0x2a, 0x0b,
    };
    const definition = try bytebox.createModuleDefinition(std.testing.allocator, .{ .debug_name = "vendor-upgrade" });
    defer definition.destroy();
    try definition.decode(wasm);
    const instance = try bytebox.createModuleInstance(.Stack, definition, std.testing.allocator);
    defer instance.destroy();
    try instance.instantiate(.{});
    const handle = try instance.getFunctionHandle("run");
    var returns = [_]bytebox.Val{.{ .I32 = 0 }};
    try instance.invoke(handle, &.{}, &returns, .{});
    try std.testing.expectEqual(@as(i32, 42), returns[0].I32);
}

test "extern vector storage preserves WebAssembly SIMD returns" {
    const wasm = &[_]u8{
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
        0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7b, 0x03,
        0x02, 0x01, 0x00, 0x07, 0x07, 0x01, 0x03, 0x72,
        0x75, 0x6e, 0x00, 0x00, 0x0a, 0x16, 0x01, 0x14,
        0x00, 0xfd, 0x0c, 0x00, 0x00, 0x80, 0x3f, 0x00,
        0x00, 0x00, 0x40, 0x00, 0x00, 0x40, 0x40, 0x00,
        0x00, 0x80, 0x40, 0x0b,
    };
    const definition = try bytebox.createModuleDefinition(std.testing.allocator, .{ .debug_name = "vendor-simd" });
    defer definition.destroy();
    try definition.decode(wasm);
    const instance = try bytebox.createModuleInstance(.Stack, definition, std.testing.allocator);
    defer instance.destroy();
    try instance.instantiate(.{});
    const handle = try instance.getFunctionHandle("run");
    var returns: [1]bytebox.Val = undefined;
    try instance.invoke(handle, &.{}, &returns, .{});
    try std.testing.expectEqual(@as(usize, 16), @sizeOf(bytebox.Val));
    try std.testing.expectEqual(@as(usize, 16), @alignOf(bytebox.Val));
    try std.testing.expectEqualSlices(f32, &.{ 1, 2, 3, 4 }, &returns[0].V128);
}

fn appendLeb128(bytes: *std.ArrayList(u8), value: usize) !void {
    var remaining = value;
    while (remaining >= 0x80) {
        try bytes.append(std.testing.allocator, @as(u8, @intCast(remaining & 0x7f)) | 0x80);
        remaining >>= 7;
    }
    try bytes.append(std.testing.allocator, @intCast(remaining));
}

test "if continuations retain full-width instruction indices" {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(std.testing.allocator);
    // local.get 0; if (result i32); 70,000 nops; i32.const 41; else;
    // i32.const 42; end; end. The continuation cannot fit into u16.
    try body.appendSlice(std.testing.allocator, &.{ 0x00, 0x20, 0x00, 0x04, 0x7f });
    try body.appendNTimes(std.testing.allocator, 0x01, 70_000);
    try body.appendSlice(std.testing.allocator, &.{ 0x41, 0x29, 0x05, 0x41, 0x2a, 0x0b, 0x0b });
    var code: std.ArrayList(u8) = .empty;
    defer code.deinit(std.testing.allocator);
    try code.append(std.testing.allocator, 1);
    try appendLeb128(&code, body.items.len);
    try code.appendSlice(std.testing.allocator, body.items);
    var wasm: std.ArrayList(u8) = .empty;
    defer wasm.deinit(std.testing.allocator);
    try wasm.appendSlice(std.testing.allocator, &.{
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
        0x01, 0x06, 0x01, 0x60, 0x01, 0x7f, 0x01, 0x7f,
        0x03, 0x02, 0x01, 0x00, 0x07, 0x07, 0x01, 0x03,
        0x72, 0x75, 0x6e, 0x00, 0x00, 0x0a,
    });
    try appendLeb128(&wasm, code.items.len);
    try wasm.appendSlice(std.testing.allocator, code.items);
    const definition = try bytebox.createModuleDefinition(std.testing.allocator, .{ .debug_name = "vendor-wide-if" });
    defer definition.destroy();
    try definition.decode(wasm.items);
    const instance = try bytebox.createModuleInstance(.Stack, definition, std.testing.allocator);
    defer instance.destroy();
    try instance.instantiate(.{});
    const handle = try instance.getFunctionHandle("run");
    var returns: [1]bytebox.Val = undefined;
    try instance.invoke(handle, &.{.{ .I32 = 0 }}, &returns, .{});
    try std.testing.expectEqual(@as(i32, 42), returns[0].I32);
    try instance.invoke(handle, &.{.{ .I32 = 1 }}, &returns, .{});
    try std.testing.expectEqual(@as(i32, 41), returns[0].I32);
}
