//! Which anonymous tag unions every use of a binding copies (design.md
//! "Anonymous Structure Is Copied Per Use"): only those some nominal type
//! declaration the module can see could lift them into.

const std = @import("std");
const can = @import("can");
const TestEnv = @import("./TestEnv.zig");

const ModuleEnv = can.ModuleEnv;

/// Whether the tag union bound by the `stmt_index`th statement of `f`'s body
/// is per-use structure.
fn localIsPerUse(env: *TestEnv, stmt_index: usize) !bool {
    const stmt = try env.lambdaBodyStatement("f", stmt_index);
    const pattern = switch (stmt) {
        .s_decl => |decl| decl.pattern,
        else => return error.TestUnexpectedResult,
    };
    const resolved = env.module_env.types.resolveVar(ModuleEnv.varFrom(pattern));
    switch (resolved.desc.content) {
        .structure => |flat| switch (flat) {
            .tag_union => {},
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
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
