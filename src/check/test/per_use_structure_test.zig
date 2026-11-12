//! Which anonymous tag unions and records every use of a binding copies
//! (design.md "Anonymous Structure Is Copied Per Use"): only those something
//! the module can see could change, by lifting them into a nominal type or,
//! for a record, by widening it with an optional or defaulted field.

const std = @import("std");
const can = @import("can");
const TestEnv = @import("./TestEnv.zig");

const ModuleEnv = can.ModuleEnv;

/// Whether the tag union or record bound by the `stmt_index`th statement of
/// `f`'s body is per-use structure.
fn localIsPerUse(env: *TestEnv, stmt_index: usize) TestEnv.TestEnvError!bool {
    const stmt = try env.lambdaBodyStatement("f", stmt_index);
    try std.testing.expect(stmt == .s_decl);
    const resolved = env.module_env.types.resolveVar(ModuleEnv.varFrom(stmt.s_decl.pattern));
    try std.testing.expect(resolved.desc.content == .structure);
    const flat = resolved.desc.content.structure;
    try std.testing.expect(flat == .tag_union or flat == .record);
    return resolved.desc.flags.per_use_structure;
}

test "a tag union a visible nominal type could take in is copied per use" {
    const source =
        \\Size := [Big, Small]
        \\
        \\f = |n| {
        \\    sized : [Big, Small]
        \\    sized = if n > 0 Big else Small
        \\    fewer : [Small]
        \\    fewer = if n > 0 Small else Small
        \\    _ = sized
        \\    _ = fewer
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(try localIsPerUse(&env, 0));
    try std.testing.expect(try localIsPerUse(&env, 1));
}

test "a tag union no visible nominal type could take in is shared by every use" {
    const source =
        \\Size := [Big, Small]
        \\
        \\f = |n| {
        \\    temperature : [Hot, Cold]
        \\    temperature = if n > 0 Hot else Cold
        \\    more : [Big, Small, Medium]
        \\    more = if n > 0 Medium else Big
        \\    _ = temperature
        \\    _ = more
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(!try localIsPerUse(&env, 0));
    try std.testing.expect(!try localIsPerUse(&env, 1));
}

test "a nominal type declared in a function body counts before checking reaches it" {
    const source =
        \\f = |n| {
        \\    temperature : [Hot, Cold]
        \\    temperature = if n > 0 Hot else Cold
        \\    _ = temperature
        \\    n + 1
        \\}
        \\
        \\later = |{}| {
        \\    Weather := [Hot, Cold]
        \\    w : Weather
        \\    w = Hot
        \\    w
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(try localIsPerUse(&env, 0));
}

test "an open nominal backing row can take in every tag union" {
    const source =
        \\Loose(ext) := [Big, ..ext]
        \\
        \\f = |n| {
        \\    temperature : [Hot, Cold]
        \\    temperature = if n > 0 Hot else Cold
        \\    _ = temperature
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(try localIsPerUse(&env, 0));
}

test "a record a visible nominal type could take in is copied per use" {
    const source =
        \\Point := { x : U64, y : U64 }
        \\
        \\f = |n| {
        \\    both = { x: n, y: n }
        \\    one = { x: n }
        \\    _ = both
        \\    _ = one
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(try localIsPerUse(&env, 0));
    try std.testing.expect(try localIsPerUse(&env, 1));
}

test "a record nothing visible could lift or widen is shared by every use" {
    const source =
        \\Point := { x : U64, y : U64 }
        \\
        \\f = |n| {
        \\    other = { z: n }
        \\    more = { x: n, y: n, z: n }
        \\    _ = other
        \\    _ = more
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(!try localIsPerUse(&env, 0));
    try std.testing.expect(!try localIsPerUse(&env, 1));
}

test "a record lacking a visible optional field is copied per use" {
    const source =
        \\Options : { size : U64, color ?: Str }
        \\
        \\f = |n| {
        \\    lacking = { size: n }
        \\    holding = { size: n, color: "red" }
        \\    _ = lacking
        \\    _ = holding
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(try localIsPerUse(&env, 0));
    try std.testing.expect(!try localIsPerUse(&env, 1));
}

test "an optional field access counts as a widening field" {
    const source =
        \\read = |r| r.?color
        \\
        \\f = |n| {
        \\    lacking = { size: n }
        \\    _ = lacking
        \\    n + 1
        \\}
    ;
    var env = try TestEnv.initOpen("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(try localIsPerUse(&env, 0));
}
