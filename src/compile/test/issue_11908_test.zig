//! Numeric source lists must remain compact through their first evaluation.
const std = @import("std");
const check = @import("check");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

const TestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

const Measurement = struct { statements: usize, locals: usize, unpacked_lists: usize };

fn checkList(source: []const u8, scalar: check.ConstStore.ConstPackedScalar, expected: []const u8) TestError!Measurement {
    return checkListWithDebug(source, scalar, expected, &.{});
}

fn checkListWithDebug(source: []const u8, scalar: check.ConstStore.ConstPackedScalar, expected: []const u8, messages: []const []const u8) TestError!Measurement {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const coord = build.coordinator.?;
    try std.testing.expect(!coord.hasUserErrors());
    const artifact = artifact: {
        var packages = coord.packages.iterator();
        while (packages.next()) |package| {
            for (package.value_ptr.*.modules.items) |*module| {
                if (std.mem.eql(u8, module.name, "Repro")) break :artifact module.checkedArtifact().?;
            }
        }
        return error.TestUnexpectedResult;
    };
    try std.testing.expectEqual(messages.len, artifact.compile_time_debug.entries.len);
    for (messages, artifact.compile_time_debug.entries) |message, entry| {
        try std.testing.expectEqualStrings(message, artifact.compile_time_debug.message(entry));
    }
    var constants: usize = 0;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .constant) continue;
        constants += 1;
        try std.testing.expect(root.payload == .const_node);
        const value = artifact.const_store.get(root.payload.const_node);
        try std.testing.expect(value == .list and value.list == .packed_bytes);
        const packed_list = value.list.packed_bytes;
        try std.testing.expectEqual(scalar, packed_list.element.?);
        try std.testing.expectEqual(expected.len / scalar.byteWidth(), packed_list.len);
        try std.testing.expectEqualSlices(u8, expected, artifact.const_store.blobBytes(packed_list.bytes));
    }
    try std.testing.expectEqual(@as(usize, 1), constants);
    // Inspect the program used for the initial evaluation, before the checked
    // constant can be restored from ConstStore. Include abandoned rewrite rows:
    // even transient IR must not grow with the number of literal elements.
    const store = &coord.program_session.?.host.?.lir_result.store;
    var unpacked_lists: usize = 0;
    for (store.getCFStmts()) |stmt| {
        if (stmt == .assign_list) unpacked_lists += 1;
    }
    return .{ .statements = store.cfStmtCount(), .locals = store.localCount(), .unpacked_lists = unpacked_lists };
}

test "issue 11908: numeric literal list evaluation has constant LIR size" {
    const gpa = std.testing.allocator;
    var small: Measurement = undefined;
    for ([_]usize{ 8, 512 }) |count| {
        var source: std.ArrayList(u8) = .empty;
        defer source.deinit(gpa);
        try source.appendSlice(gpa, "module [values]\nvalues : List(U8)\nvalues = [");
        const expected = try gpa.alloc(u8, count);
        defer gpa.free(expected);
        for (expected, 0..) |*byte, index| {
            byte.* = @truncate(index);
            try source.print(gpa, "{d},", .{byte.*});
        }
        try source.appendSlice(gpa, "]\n");
        const measured = try checkList(source.items, .u8, expected);
        if (count == 8) small = measured else try std.testing.expectEqualDeep(small, measured);
    }
}

test "issue 11908: packed source numeral lists preserve scalar bits" {
    const Case = struct { name: []const u8, scalar: check.ConstStore.ConstPackedScalar, values: []const u8, expected: []const u8 };
    const cases = [_]Case{
        .{ .name = "U8", .scalar = .u8, .values = "0,255", .expected = "\x00\xff" },
        .{ .name = "I8", .scalar = .i8, .values = "-128,127", .expected = "\x80\x7f" },
        .{ .name = "U16", .scalar = .u16, .values = "1,65535", .expected = "\x01\x00\xff\xff" },
        .{ .name = "I16", .scalar = .i16, .values = "-32768,32767", .expected = "\x00\x80\xff\x7f" },
        .{ .name = "U32", .scalar = .u32, .values = "4294967295", .expected = "\xff\xff\xff\xff" },
        .{ .name = "I32", .scalar = .i32, .values = "-2147483648", .expected = "\x00\x00\x00\x80" },
        .{ .name = "U64", .scalar = .u64, .values = "18446744073709551615", .expected = "\xff" ** 8 },
        .{ .name = "I64", .scalar = .i64, .values = "-9223372036854775808", .expected = "\x00" ** 7 ++ "\x80" },
        .{ .name = "U128", .scalar = .u128, .values = "340282366920938463463374607431768211455", .expected = "\xff" ** 16 },
        .{ .name = "I128", .scalar = .i128, .values = "-170141183460469231731687303715884105728", .expected = "\x00" ** 15 ++ "\x80" },
        .{ .name = "F32", .scalar = .f32, .values = "-0.0,1.5", .expected = "\x00\x00\x00\x80\x00\x00\xc0\x3f" },
        .{ .name = "F64", .scalar = .f64, .values = "-0.0,1.5", .expected = "\x00" ** 7 ++ "\x80\x00\x00\x00\x00\x00\x00\xf8\x3f" },
        .{ .name = "Dec", .scalar = .dec, .values = "0.000000000000000001,-0.000000000000000001", .expected = "\x01" ++ "\x00" ** 15 ++ "\xff" ** 16 },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "module [values]\nvalues : List({s})\nvalues = [{s}]\n", .{ case.name, case.values });
        defer std.testing.allocator.free(source);
        const measured = try checkList(source, case.scalar, case.expected);
        try std.testing.expectEqual(@as(usize, 0), measured.unpacked_lists);
    }
}

test "issue 11908: list packing preserves custom numeral conversions" {
    _ = try checkList(
        \\module [values]
        \\N := U8.{
        \\    from_numeral : Numeral -> Try(N, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(N.(7))
        \\}
        \\values : List(U8)
        \\values = List.map([300.N, 2.N], |N.(n)| n)
    ,
        .u8,
        "\x07\x07",
    );
}

test "issue 11908: list packing preserves element computations and debug observations" {
    _ = try checkListWithDebug(
        \\module [values]
        \\values : List(U8)
        \\values = [1, {
        \\    dbg 2.U8
        \\    3 + 4
        \\}]
    ,
        .u8,
        "\x01\x07",
        &.{"2"},
    );
}
