//! Lists of tag unions whose payloads are all zero-sized, and of records
//! holding them, are stored as packed bytes, and a list literal stores the
//! bytes evaluating the same list stores. A union with a payload that has
//! bytes stores one node per item.
const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

const TestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// What one module's one constant stored: its packed items, or null when
/// it stored them as nodes.
const Stored = struct {
    /// The packed item bytes, owned by the testing allocator.
    bytes: ?[]u8,
    len: u32,
    product_width: u32,
    /// Whether the checked-value writer stored the constant from its
    /// expression, rather than an evaluator from its result.
    value_form: bool,
};

fn storedList(source: []const u8) TestError!Stored {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, .lss);
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
    var found: ?Stored = null;
    errdefer if (found) |stored| if (stored.bytes) |bytes| gpa.free(bytes);
    for (artifact.compile_time_roots.roots.items()) |root| {
        if (root.kind != .constant) continue;
        try std.testing.expect(found == null and root.payload == .const_node);
        const value = artifact.const_store.get(root.payload.const_node);
        try std.testing.expect(value == .list);
        if (value.list.items == .nodes) {
            found = .{ .bytes = null, .len = @intCast(value.list.items.nodes.len), .product_width = 0, .value_form = root.completion == .write_value };
            continue;
        }
        const packed_list = value.list.items.packed_bytes;
        try std.testing.expect(packed_list.element == null);
        found = .{
            .bytes = try gpa.dupe(u8, artifact.const_store.blobData(packed_list.bytes)),
            .len = packed_list.len,
            .product_width = packed_list.product_width,
            .value_form = root.completion == .write_value,
        };
    }
    return found orelse error.TestUnexpectedResult;
}

/// The module `header` begins, declaring `values : annotation` as `literal`
/// and as evaluating the same list, stores the same packed bytes both ways:
/// `expected`, items `width` bytes wide.
fn expectPackedBothWays(header: []const u8, annotation: []const u8, literal: []const u8, width: u32, expected: []const u8) TestError!void {
    const gpa = std.testing.allocator;
    const literal_source = try std.fmt.allocPrint(gpa, "{s}\nvalues : {s}\nvalues = {s}\n", .{ header, annotation, literal });
    defer gpa.free(literal_source);
    // The concatenation is evaluated, so the list is the evaluator's result.
    const evaluated_source = try std.fmt.allocPrint(gpa, "{s}\nvalues : {s}\nvalues = List.concat([], {s})\n", .{ header, annotation, literal });
    defer gpa.free(evaluated_source);

    const from_literal = try storedList(literal_source);
    defer if (from_literal.bytes) |bytes| gpa.free(bytes);
    const from_evaluation = try storedList(evaluated_source);
    defer if (from_evaluation.bytes) |bytes| gpa.free(bytes);
    try std.testing.expect(from_literal.value_form);
    try std.testing.expect(!from_evaluation.value_form);
    for ([_]Stored{ from_literal, from_evaluation }) |stored| {
        try std.testing.expectEqual(width, stored.product_width);
        try std.testing.expectEqualSlices(u8, expected, stored.bytes orelse return error.TestUnexpectedResult);
    }
    try std.testing.expectEqual(from_literal.len, from_evaluation.len);
}

test "a list of Bools is stored as one byte per item both ways" {
    try expectPackedBothWays("module [values]", "List(Bool)", "[Bool.True, Bool.False, Bool.False, Bool.True]", 1, "\x01\x00\x00\x01");
}

test "a list of tags without payloads stores each tag's index in name order both ways" {
    try expectPackedBothWays("module [values]", "List([Red, Green, Blue])", "[Red, Blue, Green, Red]", 1, "\x02\x00\x01\x02");
}

test "a list of a nominal type's tags without payloads is stored packed both ways" {
    try expectPackedBothWays("module [values, Color]\nColor := [Red, Green, Blue]", "List(Color)", "[Color.Green, Color.Red]", 1, "\x01\x02");
}

test "a list of one tag without a payload stores no item bytes both ways" {
    try expectPackedBothWays("module [values]", "List([Only])", "[Only, Only, Only]", 0, "");
}

test "a list of records holding tags without payloads is stored packed both ways" {
    // Fields in name order: `color`, then `count`, then `on`.
    try expectPackedBothWays(
        "module [values]",
        "List({ on : Bool, count : U16, color : [Red, Green, Blue] })",
        "[{ on: Bool.True, count: 258, color: Green }, { on: Bool.False, count: 7, color: Blue }]",
        4,
        "\x01\x02\x01\x01" ++ "\x00\x07\x00\x00",
    );
}

test "a list of tags with a zero-sized payload is stored packed both ways" {
    try expectPackedBothWays("module [values]", "List([A({}), B])", "[A({}), B, B, A({})]", 1, "\x00\x01\x01\x00");
}

test "a list mixing bare tags and tags with zero-sized payloads is stored packed both ways" {
    try expectPackedBothWays("module [values]", "List([A, B({}), C({}, ({}, {}))])", "[C({}, ({}, {})), A, B({})]", 1, "\x02\x00\x01");
}

test "a list of tags whose payload is a union of one tag is stored packed both ways" {
    try expectPackedBothWays("module [values]", "List([A([Only]), B])", "[B, A(Only)]", 1, "\x01\x00");
}

test "a list of a nominal type's tags with zero-sized payloads is stored packed both ways" {
    try expectPackedBothWays("module [values, Wrap]\nWrap := [A({}), B]", "List(Wrap)", "[Wrap.B, Wrap.A({})]", 1, "\x01\x00");
}

test "a list of records holding tags with zero-sized payloads is stored packed both ways" {
    // Fields in name order: `n`, then `tag`.
    try expectPackedBothWays("module [values]", "List({ tag : [A({}), B], n : U8 })", "[{ tag: A({}), n: 9 }, { tag: B, n: 4 }]", 2, "\x09\x00\x04\x01");
}

test "a list of tags with a payload that has bytes is stored one node per item both ways" {
    const gpa = std.testing.allocator;
    for ([_][]const u8{ "[A(7), B, A(9)]", "List.concat([], [A(7), B, A(9)])" }) |value| {
        const source = try std.fmt.allocPrint(gpa, "module [values]\nvalues : List([A(U8), B])\nvalues = {s}\n", .{value});
        defer gpa.free(source);
        const stored = try storedList(source);
        try std.testing.expect(stored.bytes == null);
        try std.testing.expectEqual(@as(u32, 3), stored.len);
    }
}
