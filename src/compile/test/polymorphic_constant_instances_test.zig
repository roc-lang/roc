//! Each closed instance at which a polymorphic constant is used is a
//! compile-time root of its own, evaluated and stored, and so is the
//! constant's canonical instance, every variable at its row default or the
//! empty tag union; the constant's own root is never evaluated (design.md
//! "Polymorphic Constants"). Both strategies store the same instances.
const std = @import("std");
const base = @import("base");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

const TestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// The instance roots of the module's constant `empty`: how many there are,
/// how many are canonical, and whether each stored a value.
const Instances = struct { count: usize, canonical: usize, stored: usize, constant_pending: bool };

fn instancesOf(source: []const u8, strategy: base.SpecializationStrategy) TestError!Instances {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, strategy);
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
    const roots = artifact.compile_time_roots.roots.items();
    const constant = for (roots) |root| {
        if (root.kind == .constant) break root;
    } else return error.TestUnexpectedResult;
    var instances: Instances = .{ .count = 0, .canonical = 0, .stored = 0, .constant_pending = constant.payload == .pending };
    for (roots) |root| {
        const argument = root.literal_argument orelse continue;
        if (argument.site.kind != .constant_instance or argument.site.node != @backingInt(constant.id)) continue;
        instances.count += 1;
        if (argument.canonical) instances.canonical += 1;
        if (root.payload == .const_node) instances.stored += 1;
    }
    return instances;
}

const two_uses =
    \\module [empty, sizes]
    \\empty : List(_a)
    \\empty = []
    \\sizes : (U64, U64)
    \\sizes = (List.len(List.append(empty, 1.U8)), List.len(List.append(empty, "s")))
;

test "a polymorphic constant used at two closed types has two instance roots and a canonical one" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        const instances = try instancesOf(two_uses, strategy);
        try std.testing.expectEqual(@as(usize, 3), instances.count);
        try std.testing.expectEqual(@as(usize, 1), instances.canonical);
        try std.testing.expectEqual(@as(usize, 3), instances.stored);
        try std.testing.expect(instances.constant_pending);
    }
}

const forwarded_use =
    \\module [empty, wrap, sizes]
    \\empty : List(_a)
    \\empty = []
    \\wrap : () -> List(_b)
    \\wrap = || empty
    \\sizes : (U64, U64)
    \\sizes = (List.len(List.append(wrap(), 1.U8)), List.len(List.append(wrap(), 2.U16)))
;

test "a use inside a generic function forwards the instance to each closed call site" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        // `wrap`'s own use is open, and each of its two calls closes it.
        const instances = try instancesOf(forwarded_use, strategy);
        try std.testing.expectEqual(@as(usize, 3), instances.count);
        try std.testing.expectEqual(@as(usize, 1), instances.canonical);
        try std.testing.expectEqual(@as(usize, 3), instances.stored);
    }
}

const canonical_use =
    \\module [empty, size]
    \\empty : List(_a)
    \\empty = []
    \\size : U64
    \\size = List.len(empty)
;

test "a use at the canonical instance shares the canonical root" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        const instances = try instancesOf(canonical_use, strategy);
        try std.testing.expectEqual(@as(usize, 1), instances.count);
        try std.testing.expectEqual(@as(usize, 1), instances.canonical);
        try std.testing.expectEqual(@as(usize, 1), instances.stored);
    }
}

const destructured_use =
    \\module [pair, sizes]
    \\pair = ([], 1.U64)
    \\sizes : (U64, U64)
    \\sizes = {
    \\    (items, _) = pair
    \\    (List.len(List.append(items, 1.U8)), List.len(List.append(items, "s")))
    \\}
;

test "each use of a name a destructuring binds reads the instance at the use's type" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        // `items` is read at `List(U8)` and at `List(Str)`: each use is an
        // instantiation of `pair` through the destructuring.
        const instances = try instancesOf(destructured_use, strategy);
        try std.testing.expectEqual(@as(usize, 3), instances.count);
        try std.testing.expectEqual(@as(usize, 1), instances.canonical);
        try std.testing.expectEqual(@as(usize, 3), instances.stored);
    }
}

const callable_binding_method =
    \\module [empty, Wrap, sizes]
    \\empty : List(_a)
    \\empty = []
    \\Wrap := [W].{
    \\    wrap = mk(empty)
    \\}
    \\mk : List(a) -> (Wrap, a -> List(a))
    \\mk = |items| |_w, x| List.append(items, x)
    \\w : Wrap
    \\w = W
    \\sizes : (U64, U64)
    \\sizes = (List.len(w.wrap(1.U8)), List.len(w.wrap("s")))
;

test "a dispatch to a method bound to a polymorphic callable binding instantiates what the binding reads" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        // `wrap` reads `empty` at its own variable, which each dispatch
        // fixes from the substitution checking recorded for it.
        const instances = try instancesOf(callable_binding_method, strategy);
        try std.testing.expectEqual(@as(usize, 3), instances.count);
        try std.testing.expectEqual(@as(usize, 1), instances.canonical);
        try std.testing.expectEqual(@as(usize, 3), instances.stored);
    }
}

const weak_record =
    \\module [Counter, shared, c, d]
    \\Counter := { count : U64 }.{
    \\    describe : Counter -> Str
    \\    describe = |r| r.count.to_str()
    \\}
    \\shared = { count: 1.U64 }
    \\c : Counter
    \\c = shared
    \\d = shared.describe()
;

test "a record literal's uncommitted tail keeps a constant weak: its uses share one lift and it has no instances" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        const instances = try instancesOf(weak_record, strategy);
        try std.testing.expectEqual(@as(usize, 0), instances.count);
    }
}
