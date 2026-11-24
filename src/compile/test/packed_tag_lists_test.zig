//! A list constant is stored as its image (design.md "Stored Images"), and a
//! list literal stores the image evaluating the same list stores. A list of
//! tag unions whose payloads are all zero-sized holds each item's
//! discriminant, its tag's index in byte-wise name order. A constant whose
//! annotation names a tag union is polymorphic in the union's implicitly
//! open row (design.md "Polymorphic Constants"), and its canonical instance,
//! evaluated at the union closed, is what is read.
const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

const TestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// What one module's one list constant stored.
const Stored = struct {
    /// The list's item bytes, owned by the testing allocator.
    items: []u8,
    len: u64,
    /// Whether the checked-value writer stored the constant from its
    /// expression, rather than an evaluator from its result.
    value_form: bool,
    /// Whether the constant is polymorphic, so that its canonical
    /// instance, which an evaluator stores, holds the list.
    polymorphic: bool,
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
    errdefer if (found) |stored| gpa.free(stored.items);
    for (artifact.compile_time_roots.roots.items()) |constant| {
        if (constant.kind != .constant) continue;
        try std.testing.expect(found == null);
        const canonical = for (artifact.compile_time_roots.roots.items()) |candidate| {
            const argument = candidate.literal_argument orelse continue;
            if (argument.canonical and argument.site.node == @backingInt(constant.id)) break candidate;
        } else null;
        const root = canonical orelse constant;
        try std.testing.expect(root.payload == .const_node);
        const image = artifact.const_store.image(artifact.const_store.imageOf(root.payload.const_node) orelse return error.TestUnexpectedResult);
        const word = 8;
        const len = std.mem.readInt(u64, image.bytes[word..][0..word], .little);
        // The items run from the pointer word to the block's end: the list
        // holds no other allocation. A list of zero-sized items has none.
        const items: []const u8 = if (len == 0 or image.pointers.len == 0) &.{} else image.bytes[@intCast(std.mem.readInt(u64, image.bytes[0..word], .little))..];
        found = .{
            .items = try gpa.dupe(u8, items),
            .len = len,
            .value_form = root.completion == .write_value,
            .polymorphic = canonical != null,
        };
    }
    return found orelse error.TestUnexpectedResult;
}

/// The module `header` begins, declaring `values : annotation` as `literal`
/// and as evaluating the same list, stores the same items both ways, which
/// are `expected` when given.
fn expectSameImageBothWays(header: []const u8, annotation: []const u8, literal: []const u8, expected: ?[]const u8) TestError!void {
    const gpa = std.testing.allocator;
    const literal_source = try std.fmt.allocPrint(gpa, "{s}\nvalues : {s}\nvalues = {s}\n", .{ header, annotation, literal });
    defer gpa.free(literal_source);
    // The concatenation is evaluated, so the list is the evaluator's result.
    const evaluated_source = try std.fmt.allocPrint(gpa, "{s}\nvalues : {s}\nvalues = List.concat([], {s})\n", .{ header, annotation, literal });
    defer gpa.free(evaluated_source);

    const from_literal = try storedList(literal_source);
    defer gpa.free(from_literal.items);
    const from_evaluation = try storedList(evaluated_source);
    defer gpa.free(from_evaluation.items);
    try std.testing.expectEqual(from_literal.polymorphic, from_evaluation.polymorphic);
    try std.testing.expect(from_literal.value_form != from_literal.polymorphic);
    try std.testing.expect(!from_evaluation.value_form);
    // The evaluated concatenation may keep more capacity than the literal,
    // so the two agree on the items they hold.
    try std.testing.expectEqual(from_literal.len, from_evaluation.len);
    try std.testing.expectEqualSlices(u8, from_literal.items, from_evaluation.items);
    if (expected) |bytes| try std.testing.expectEqualSlices(u8, bytes, from_literal.items);
}

test "a list of Bools holds one byte per item both ways" {
    try expectSameImageBothWays("module [values]", "List(Bool)", "[Bool.True, Bool.False, Bool.False, Bool.True]", "\x01\x00\x00\x01");
}

test "a list of tags without payloads holds each tag's index in name order both ways" {
    try expectSameImageBothWays("module [values]", "List([Red, Green, Blue])", "[Red, Blue, Green, Red]", "\x02\x00\x01\x02");
}

test "a list of a nominal type's tags without payloads is the same image both ways" {
    try expectSameImageBothWays("module [values, Color]\nColor := [Red, Green, Blue]", "List(Color)", "[Color.Green, Color.Red]", "\x01\x02");
}

test "a list of one tag without a payload holds no item bytes both ways" {
    try expectSameImageBothWays("module [values]", "List([Only])", "[Only, Only, Only]", "");
}

test "a list of records holding tags without payloads is the same image both ways" {
    try expectSameImageBothWays(
        "module [values]",
        "List({ on : Bool, count : U16, color : [Red, Green, Blue] })",
        "[{ on: Bool.True, count: 258, color: Green }, { on: Bool.False, count: 7, color: Blue }]",
        null,
    );
}

test "a list of tags with a zero-sized payload holds each tag's index both ways" {
    try expectSameImageBothWays("module [values]", "List([A({}), B])", "[A({}), B, B, A({})]", "\x00\x01\x01\x00");
}

test "a list mixing bare tags and tags with zero-sized payloads holds each tag's index both ways" {
    try expectSameImageBothWays("module [values]", "List([A, B({}), C({}, ({}, {}))])", "[C({}, ({}, {})), A, B({})]", "\x02\x00\x01");
}

test "a list of tags whose payload is a union of one tag holds each tag's index both ways" {
    try expectSameImageBothWays("module [values]", "List([A([Only]), B])", "[B, A(Only)]", "\x01\x00");
}

test "a list of a nominal type's tags with zero-sized payloads holds each tag's index both ways" {
    try expectSameImageBothWays("module [values, Wrap]\nWrap := [A({}), B]", "List(Wrap)", "[Wrap.B, Wrap.A({})]", "\x01\x00");
}

test "a list of records holding tags with zero-sized payloads is the same image both ways" {
    try expectSameImageBothWays("module [values]", "List({ tag : [A({}), B], n : U8 })", "[{ tag: A({}), n: 9 }, { tag: B, n: 4 }]", null);
}

test "a list of tags with a payload that has bytes is the same image both ways" {
    try expectSameImageBothWays("module [values]", "List([A(U8), B])", "[A(7), B, A(9)]", null);
}
