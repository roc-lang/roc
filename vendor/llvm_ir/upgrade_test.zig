//! Pin Roc's additions while refreshing the vendored LLVM builder.
const std = @import("std");
const Builder = @import("Builder.zig");

test "explicit target layout and deferred entry allocations survive serialization" {
    const layout = "e-m:e-p:32:32-i64:64-n8:16:32-S128";
    var builder = try Builder.init(.{
        .allocator = std.testing.allocator,
        .name = "upgrade.roc",
        .triple = "wasm32-unknown-unknown",
        .data_layout = layout,
        .strip = false,
    });
    defer builder.deinit();

    const function = try builder.addFunction(
        try builder.fnType(.void, &.{}, .normal),
        try builder.strtabString("entry_allocas"),
        .default,
    );
    var wip = try Builder.WipFunction.init(&builder, .{ .function = function, .strip = false });
    defer wip.deinit();
    const entry = try wip.block(0, "entry");
    wip.cursor = .{ .block = entry };
    _ = try wip.allocaInEntry(.i32, .none, .fromByteUnits(4), .default, "first");
    _ = try wip.allocaInEntry(.i64, .none, .fromByteUnits(8), .default, "second");
    _ = try wip.retVoid();
    try wip.finish();

    var text = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer text.deinit();
    try builder.print(&text.writer);
    const ir = text.written();
    try std.testing.expect(std.mem.find(u8, ir, layout) != null);
    try std.testing.expect(std.mem.find(u8, ir, "wasm32-unknown-unknown") != null);
    const second = std.mem.find(u8, ir, "%second = alloca i64").?;
    const first = std.mem.find(u8, ir, "%first = alloca i32").?;
    const ret = std.mem.find(u8, ir, "ret void").?;
    try std.testing.expect(second < first and first < ret);

    const bitcode = try builder.toBitcode(std.testing.allocator, .{
        .name = "Roc",
        .version = .{ .major = 0, .minor = 0, .patch = 0 },
    });
    defer std.testing.allocator.free(bitcode);
    try std.testing.expectEqual(@as(u32, 0xdec04342), bitcode[0]);
}

test "successive module assembly contributions are preserved" {
    var builder = try Builder.init(.{ .allocator = std.testing.allocator });
    defer builder.deinit();
    var first = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer first.deinit();
    try first.writer.writeAll("first");
    try builder.finishModuleAsm(&first);
    var second = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer second.deinit();
    try second.writer.writeAll("second");
    try builder.finishModuleAsm(&second);
    try std.testing.expectEqualStrings("first\nsecond\n", builder.module_asm.items);
}
