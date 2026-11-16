//! A declaration that takes a generalized value apart (design.md "Procedure
//! Aliases", a destructuring scheme alias): its pattern is the generalized
//! binding, so each name it binds is instantiated at every use, whether a
//! tuple pattern or a record pattern binds it.

const std = @import("std");
const can = @import("can");
const types = @import("types");
const TestEnv = @import("./TestEnv.zig");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

/// Constants whose types keep their variables: each holds a function that
/// requires a method of the type it is called at.
const hooks_source =
    \\Hooks :: [].{
    \\    pair = (|s| s.concat(s), 1)
    \\    rec = { twice: |s| s.concat(s), tag: "t" }
    \\    nested = { outer: { twice: |s| s.concat(s) } }
    \\    opt : { twice ?: _, tag : Str }
    \\    opt = { twice: |s| s.concat(s), tag: "t" }
    \\}
;

/// Functions that each take an optional field's destructured value at one
/// type.
const try_users =
    \\use_str : Try(Str -> Str, [MissingField]) -> Str
    \\use_str = |t| match t {
    \\    Ok(f) => f("ab")
    \\    Err(MissingField) => ""
    \\}
    \\
    \\use_list : Try(List(U8) -> List(U8), [MissingField]) -> List(U8)
    \\use_list = |t| match t {
    \\    Ok(f) => f([1, 2])
    \\    Err(MissingField) => []
    \\}
    \\
;

fn expectDefType(comptime source: []const u8, def_name: []const u8, expected: []const u8) TestEnv.TestEnvError!void {
    var hooks = try TestEnv.init("Hooks", hooks_source);
    defer hooks.deinit();
    try hooks.assertNoErrors();

    var env = try TestEnv.initWithImport("Test", source, "Hooks", &hooks);
    defer env.deinit();
    try env.assertDefType(def_name, expected);
}

/// The rank of the name the first statement of `v`'s block binds for the
/// record field `twice`.
fn destructuredNameRank(env: *const TestEnv) TestEnv.TestEnvError!types.Rank {
    const store = &env.module_env.store;
    const block = store.getExpr(try env.defExpr("v"));
    try std.testing.expect(block == .e_block);
    const stmt = store.getStatement(store.sliceStatements(block.e_block.stmts)[0]);
    try std.testing.expect(stmt == .s_decl);
    const pattern = store.getPattern(stmt.s_decl.pattern);
    try std.testing.expect(pattern == .record_destructure);
    const destruct_idx: CIR.Pattern.RecordDestruct.Idx = store.sliceRecordDestructs(pattern.record_destructure.destructs)[0];
    return env.module_env.types.resolveVar(ModuleEnv.varFrom(destruct_idx)).desc.rank;
}

test "destructured scheme alias: a record-destructured name is generalized" {
    var hooks = try TestEnv.init("Hooks", hooks_source);
    defer hooks.deinit();
    try hooks.assertNoErrors();

    var env = try TestEnv.initWithImport("Test",
        \\import Hooks
        \\
        \\v = {
        \\    { twice, tag: _ } = Hooks.rec
        \\    twice("xy")
        \\}
    , "Hooks", &hooks);
    defer env.deinit();
    try env.assertDefType("v", "Str");
    try std.testing.expectEqual(types.Rank.generalized, try destructuredNameRank(&env));
}

test "destructured scheme alias: a record-destructured name called with a bare literal settles the literal" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = {
        \\    { twice, tag } = Hooks.rec
        \\    (twice("xy"), tag)
        \\}
    , "v", "(Str, Str)");
}

test "destructured scheme alias: a record-destructured name is used at two types" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = {
        \\    { twice, tag: _ } = Hooks.rec
        \\    (twice("xy"), twice([1.U8, 2]))
        \\}
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a tuple-destructured name is used at two types" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = {
        \\    (twice, _) = Hooks.pair
        \\    (twice("xy"), twice([1.U8, 2]))
        \\}
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a name a nested record pattern binds is used at two types" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = {
        \\    { outer: { twice } } = Hooks.nested
        \\    (twice("xy"), twice([1.U8, 2]))
        \\}
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a record pattern over a local name for the constant is used at two types" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = {
        \\    rec = Hooks.rec
        \\    { twice, tag: _ } = rec
        \\    (twice("xy"), twice([1.U8, 2]))
        \\}
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a record-destructured name's use at a variable of the enclosing function" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = |x| {
        \\    { twice, tag: _ } = Hooks.rec
        \\    twice(x)
        \\}
    , "v", "a -> b where [a.concat : a, a -> b]");
}

test "destructured scheme alias: a name destructured out of an optional field is used at two types" {
    try expectDefType("import Hooks\n\n" ++ try_users ++
        \\v = {
        \\    { twice, tag: _ } = Hooks.opt
        \\    (use_str(twice), use_list(twice))
        \\}
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a name destructured out of an optional field and matched settles a literal" {
    try expectDefType(
        \\import Hooks
        \\
        \\v = {
        \\    { twice, tag } = Hooks.opt
        \\    match twice {
        \\        Ok(f) => (f("xy"), tag)
        \\        Err(MissingField) => ("", tag)
        \\    }
        \\}
    , "v", "(Str, Str)");
}

test "destructured scheme alias: a top-level record-destructured name is used at two types" {
    try expectDefType(
        \\import Hooks
        \\
        \\{ twice, tag: _ } = Hooks.rec
        \\
        \\v = (twice("xy"), twice([1.U8, 2]))
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a top-level tuple-destructured name is used at two types" {
    try expectDefType(
        \\import Hooks
        \\
        \\(twice, _) = Hooks.pair
        \\
        \\v = (twice("xy"), twice([1.U8, 2]))
    , "v", "(Str, List(U8))");
}

test "destructured scheme alias: a misuse of a record-destructured name is reported at the use alone" {
    var hooks = try TestEnv.init("Hooks", hooks_source);
    defer hooks.deinit();
    try hooks.assertNoErrors();

    var env = try TestEnv.initWithImport("Test",
        \\import Hooks
        \\
        \\v = {
        \\    { twice, tag } = Hooks.rec
        \\    (twice("xy"), tag + 1)
        \\}
    , "Hooks", &hooks);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

// A value that is not generalized has one type, shared by every use of a name
// destructured out of it.

test "destructured scheme alias: a record-destructured name of a weak value has one type" {
    var env = try TestEnv.init("Test",
        \\v = {
        \\    rec = { twice: |s| s.concat(s) }
        \\    { twice } = rec
        \\    (twice("xy"), twice([1.U8, 2]))
        \\}
    );
    defer env.deinit();
    try env.assertHasTypeError("Type Mismatch");
}

test "destructured scheme alias: a tuple-destructured name of a weak value has one type" {
    var env = try TestEnv.init("Test",
        \\v = {
        \\    pair = (|s| s.concat(s), 1)
        \\    (twice, _) = pair
        \\    (twice("xy"), twice([1.U8, 2]))
        \\}
    );
    defer env.deinit();
    try env.assertHasTypeError("Type Mismatch");
}

test "destructured scheme alias: a name destructured out of a weak value's optional field has one type" {
    var env = try TestEnv.init("Test", try_users ++
        \\v = {
        \\    rec : { twice ?: _ }
        \\    rec = { twice: |s| s.concat(s) }
        \\    { twice } = rec
        \\    (use_str(twice), use_list(twice))
        \\}
    );
    defer env.deinit();
    try env.assertHasTypeError("Type Mismatch");
}
