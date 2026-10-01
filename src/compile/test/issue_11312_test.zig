//! Regression tests for issue #11312: a `crash` whose message is not a `Str`
//! reports the type mismatch once. The crash itself becomes the checked
//! runtime error, and a compile-time root whose evaluation reaches code
//! checking rejected discards its result without reporting the problem a
//! second time as a compile-time crash. Independent roots are still
//! evaluated.
//! repro for https://github.com/roc-lang/roc/issues/11312

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const RecoveryError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

const independent_defs =
    \\helper = |n| n + 1
    \\good = 123.U64
    \\fine = helper(1.U64)
    \\
;

/// Checks `source` as a module and expects exactly one type mismatch and no
/// other error. A compile-time constant whose source is `blocked_expr` reaches
/// that rejected code, so it stores the crash rather than a value; a top-level
/// expect is evaluated only by `roc test`. The independent `good` and `fine`
/// roots are evaluated.
fn expectRecovery(source: []const u8, imported_source: ?[]const u8, blocked_expr: []const u8) RecoveryError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const module_source = try std.mem.concat(gpa, u8, &.{ "module []\n", source, independent_defs });
    defer gpa.free(module_source);
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = module_source });
    if (imported_source) |imported| try tmp.dir.writeFile(io, .{ .sub_path = "Broken.roc", .data = imported });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var mismatches: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) {
                mismatches += 1;
            } else {
                if (report.severity != .warning) std.debug.print("unexpected report: {s}\n", .{report.title});
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 1), mismatches);

    const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    var found_blocked = false;
    var found_good = false;
    var found_fine = false;
    for (artifact.compile_time_roots.roots) |root| {
        const expr = artifact.checked_bodies.expr(root.expr);
        const expr_source = module_source[expr.source_region.start.offset..expr.source_region.end.offset];
        if (std.mem.eql(u8, expr_source, blocked_expr)) {
            if (root.kind != .expect) {
                try std.testing.expect(root.payload == .const_node);
                try std.testing.expect(artifact.const_store.get(root.payload.const_node) == .checked_error);
            }
            found_blocked = true;
        }
        const is_good = std.mem.eql(u8, expr_source, "123.U64");
        const is_fine = std.mem.eql(u8, expr_source, "helper(1.U64)");
        if (is_good or is_fine) {
            try std.testing.expectEqual(.eligible, root.request_eligibility);
            try std.testing.expect(root.payload == .const_node);
            try std.testing.expect(artifact.const_store.get(root.payload.const_node) != .checked_error);
            found_good = found_good or is_good;
            found_fine = found_fine or is_fine;
        }
    }
    try std.testing.expect(found_blocked);
    try std.testing.expect(found_good);
    try std.testing.expect(found_fine);
}

test "issue 11312: a root calling a function whose crash message is not a Str reports its problem once" {
    try expectRecovery(
        \\poly = || {
        \\    crash YYYYY
        \\    "x"
        \\}
        \\result = poly() == poly()
        \\
    , null, "poly() == poly()");
}

test "issue 11366: an expect calling a function with an erroneous inline expect reports its problem once" {
    try expectRecovery(
        \\xs : List(U64)
        \\xs = [1, 2, 3]
        \\f = |n| {
        \\    expect 3 >= xs.len
        \\    n
        \\}
        \\expect f(1) == 1
        \\
    , null, "f(1) == 1");
}

test "issue 11366: an expect calling an imported function with an erroneous inline expect reports its problem once" {
    try expectRecovery(
        \\import Broken
        \\expect Broken.f(1) == 1
        \\
    ,
        \\module [f]
        \\f : U64 -> U64
        \\f = |n| {
        \\    expect n == "bad"
        \\    n
        \\}
        \\
    , "Broken.f(1) == 1");
}

test "issue 11312: a root calling a function whose crash message is erroneous reports its problem once" {
    try expectRecovery(
        \\f : U64 -> Str
        \\f = |_| "m"
        \\poly = || {
        \\    crash f(Bad)
        \\    "x"
        \\}
        \\result = poly() == poly()
        \\
    , null, "poly() == poly()");
}

test "issue 11312: a root calling a function whose body contains a rejected call reports its problem once" {
    try expectRecovery(
        \\f : U64 -> Str
        \\f = |_| "m"
        \\poly = || {
        \\    _s = f(Bad)
        \\    "x"
        \\}
        \\result = poly() == poly()
        \\
    , null, "poly() == poly()");
}

test "issue 11312: a root reading a constant that calls into checked-error code reports its problem once" {
    try expectRecovery(
        \\poly = || {
        \\    crash YYYYY
        \\    "x"
        \\}
        \\first = poly()
        \\result = first == "x"
        \\
    , null, "first == \"x\"");
}

test "issue 11312: a root calling an imported function whose crash message is not a Str reports its problem once" {
    try expectRecovery(
        \\import Broken
        \\result = Broken.poly({}) == "x"
        \\
    ,
        \\module [poly]
        \\
        \\poly : {} -> Str
        \\poly = |_| {
        \\    crash YYYYY
        \\    "x"
        \\}
        \\
    , "Broken.poly({}) == \"x\"");
}

test "issue 11312: a root dispatching to a method whose crash message is not a Str reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    describe : Thing -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = thing.describe() == "x"
        \\
    , null, "thing.describe() == \"x\"");
}

test "issue 11312: a root reading an annotated constant whose initializer was rejected reports its problem once" {
    try expectRecovery(
        \\bad : U64
        \\bad = "bad"
        \\result = bad + 1
        \\
    , null, "bad + 1");
}

// repro for https://github.com/roc-lang/roc/issues/11923
// The `parser_for` codec call reaches `Format.parse_u8` through the format's
// method, so the expect is blocked by its type mismatch instead of being
// lowered and evaluated.
test "issue 11923: an expect reaching an erroneous format method through a codec reports its problem once" {
    try expectRecovery(
        \\Format := [Default].{
        \\    parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
        \\    parse_u8 = |_, _| Err(OtherErr)
        \\}
        \\expect (U8.parser_for(Format.Default))({}) == Err(Bad)
        \\
    , null, "(U8.parser_for(Format.Default))({}) == Err(Bad)");
}

test "issue 11923: a root whose call-site evidence selects an erroneous method reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    describe : Thing -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\f = |x| x.describe()
        \\thing : Thing
        \\thing = Thing
        \\result = f(thing) == "x"
        \\
    , null, "f(thing) == \"x\"");
}

test "issue 11923: a root whose structural equality reaches an erroneous is_eq reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    is_eq : Thing, Thing -> Bool
        \\    is_eq = |_, _| {
        \\        crash YYYYY
        \\        Bool.True
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = [thing] == [thing]
        \\
    , null, "[thing] == [thing]");
}

test "issue 11923: an expect reaching an erroneous format method through a generic codec call reports its problem once" {
    try expectRecovery(
        \\Format := [Default].{
        \\    parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
        \\    parse_u8 = |_, _| Err(OtherErr)
        \\}
        \\p = |fmt| U8.parser_for(fmt)
        \\expect (p(Format.Default))({}) == Err(Bad)
        \\
    , null, "(p(Format.Default))({}) == Err(Bad)");
}

test "issue 11923: a root dispatching to a method alias of a constrained procedure with an erroneous requirement reports its problem once" {
    try expectRecovery(
        \\Other := [Other].{
        \\    describe : Other -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\show_both : Thing, a -> Str where [a.describe : a -> Str]
        \\show_both = |_, x| x.describe()
        \\Thing := [Thing].{
        \\    show = show_both
        \\}
        \\thing : Thing
        \\thing = Thing
        \\other : Other
        \\other = Other
        \\result = thing.show(other) == "x"
        \\
    , null, "thing.show(other) == \"x\"");
}

test "issue 11923: a root reaching a method alias through call-site evidence reports its problem once" {
    try expectRecovery(
        \\Other := [Other].{
        \\    describe : Other -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\Thing := [Thing].{
        \\    show = Thing.show_impl
        \\    show_impl : Thing, a -> Str where [a.describe : a -> Str]
        \\    show_impl = |_, x| x.describe()
        \\}
        \\call_show = |t, o| t.show(o)
        \\thing : Thing
        \\thing = Thing
        \\other : Other
        \\other = Other
        \\result = call_show(thing, other) == "x"
        \\
    , null, "call_show(thing, other) == \"x\"");
}

test "issue 11923: a root dispatching to a monomorphic method alias of a constrained procedure reports its problem once" {
    try expectRecovery(
        \\Other := [Other].{
        \\    describe : Other -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\show_any : Thing, a -> Str where [a.describe : a -> Str]
        \\show_any = |_, x| x.describe()
        \\Thing := [Thing].{
        \\    show : Thing, Other -> Str
        \\    show = show_any
        \\}
        \\thing : Thing
        \\thing = Thing
        \\other : Other
        \\other = Other
        \\result = thing.show(other) == "x"
        \\
    , null, "thing.show(other) == \"x\"");
}

test "issue 11923: a root reaching a monomorphic method alias through call-site evidence reports its problem once" {
    try expectRecovery(
        \\Other := [Other].{
        \\    describe : Other -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\show_any : Thing, a -> Str where [a.describe : a -> Str]
        \\show_any = |_, x| x.describe()
        \\Thing := [Thing].{
        \\    show : Thing, Other -> Str
        \\    show = show_any
        \\}
        \\call_show = |t, o| t.show(o)
        \\thing : Thing
        \\thing = Thing
        \\other : Other
        \\other = Other
        \\result = call_show(thing, other) == "x"
        \\
    , null, "call_show(thing, other) == \"x\"");
}

test "issue 11923: a root reaching an imported monomorphic method alias of a constrained procedure reports its problem once" {
    try expectRecovery(
        \\import Broken exposing [Thing, Other]
        \\thing : Thing
        \\thing = Thing.Thing
        \\other : Other
        \\other = Other.Other
        \\result = thing.show(other) == "x"
        \\
    ,
        \\module [Thing, Other]
        \\
        \\Other := [Other].{
        \\    describe : Other -> Str
        \\    describe = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\show_any : Thing, a -> Str where [a.describe : a -> Str]
        \\show_any = |_, x| x.describe()
        \\Thing := [Thing].{
        \\    show : Thing, Other -> Str
        \\    show = show_any
        \\}
        \\
    , "thing.show(other) == \"x\"");
}

test "issue 11923: a root reaching an erroneous is_eq through a record element of List.contains reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    is_eq : Thing, Thing -> Bool
        \\    is_eq = |_, _| {
        \\        crash YYYYY
        \\        Bool.True
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = [{ a: 1, t: thing }].contains({ a: 1, t: thing })
        \\
    , null, "[{ a: 1, t: thing }].contains({ a: 1, t: thing })");
}

test "issue 11923: a root reaching an erroneous is_eq through a tuple element of List.contains reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    is_eq : Thing, Thing -> Bool
        \\    is_eq = |_, _| {
        \\        crash YYYYY
        \\        Bool.True
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = [(1, thing)].contains((1, thing))
        \\
    , null, "[(1, thing)].contains((1, thing))");
}

test "issue 11923: a root reaching an erroneous is_eq through record equality reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    is_eq : Thing, Thing -> Bool
        \\    is_eq = |_, _| {
        \\        crash YYYYY
        \\        Bool.True
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = { a: 1, t: thing } == { a: 1, t: thing }
        \\
    , null, "{ a: 1, t: thing } == { a: 1, t: thing }");
}

test "issue 11923: a root reaching an erroneous to_hash through a record key reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    is_eq : Thing, Thing -> Bool
        \\    is_eq = |_, _| Bool.True
        \\    to_hash : Thing, Hasher -> Hasher
        \\    to_hash = |_, h| {
        \\        crash YYYYY
        \\        h
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = Dict.empty().insert({ t: thing }, 1.U64).len()
        \\
    , null, "Dict.empty().insert({ t: thing }, 1.U64).len()");
}

test "issue 11923: a root reaching an erroneous to_inspect reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    to_inspect : Thing -> Str
        \\    to_inspect = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\thing : Thing
        \\thing = Thing
        \\result = Str.inspect(thing)
        \\
    , null, "Str.inspect(thing)");
}

test "issue 11923: a root reaching an erroneous to_inspect through a generic function reports its problem once" {
    try expectRecovery(
        \\Thing := [Thing].{
        \\    to_inspect : Thing -> Str
        \\    to_inspect = |_| {
        \\        crash YYYYY
        \\        "x"
        \\    }
        \\}
        \\show = |x| Str.inspect(x)
        \\thing : Thing
        \\thing = Thing
        \\result = show(thing)
        \\
    , null, "show(thing)");
}
