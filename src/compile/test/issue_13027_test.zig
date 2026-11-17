//! Boxy descriptors hold a nominal's `is_eq`, `to_hash`, and `to_inspect`
//! overrides only where a descriptor-guided equality, hashing, or inspection
//! the program performs can reach them. A descriptor needed only for layout
//! or ownership roots none of its overrides.
//! Repro for https://github.com/roc-lang/roc/issues/13027
const std = @import("std");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

/// Fails if any procedure the lowered program keeps is the `method` override
/// of `Token`.
fn expectNoTokenOverride(store: *const lir.LirStore, comptime method: []const u8) harness.LowerToLirHarnessError!void {
    for (0..store.getProcSpecs().len) |index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        if (std.mem.endsWith(u8, name, "Token." ++ method)) {
            std.debug.print("issue 13027: the program keeps the unused override procedure {s}\n", .{name});
            return error.TestUnexpectedResult;
        }
    }
}

fn expectTokenToInspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    for (0..store.getProcSpecs().len) |index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        if (std.mem.endsWith(u8, name, "Token.to_inspect")) return;
    }
    return error.TestUnexpectedResult;
}

fn expectNoTokenToInspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    try expectNoTokenOverride(store, "to_inspect");
}

fn expectNoTokenIsEq(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    try expectNoTokenOverride(store, "is_eq");
}

fn expectNoTokenToHash(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    try expectNoTokenOverride(store, "to_hash");
}

test "issue 13027: a boxy descriptor needed only for layout keeps no to_inspect override" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\Token := [Token(U64)].{
            \\    to_inspect : Token -> Str
            \\    to_inspect = |_token| "UNUSED_INSPECT_MARKER"
            \\}
            \\identity : a -> a
            \\identity = |x| x
            \\main! = |args| {
            \\    token : Token
            \\    token = Token(args.len())
            \\    n = match identity(token) { Token(k) => k + 1 }
            \\    echo!(n.to_str())
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .target_usize = width }, expectNoTokenToInspect);
    }
}

test "issue 13027: a boxy descriptor needed only for layout keeps no is_eq override" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\Token := [Token(U64)].{
            \\    is_eq : Token, Token -> Bool
            \\    is_eq = |left, _right| match left { Token(n) => n.to_str().starts_with("UNUSED_EQ_MARKER") }
            \\}
            \\identity : a -> a
            \\identity = |x| x
            \\main! = |args| {
            \\    token : Token
            \\    token = Token(args.len())
            \\    n = match identity(token) { Token(k) => k + 1 }
            \\    echo!(n.to_str())
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .target_usize = width }, expectNoTokenIsEq);
    }
}

test "issue 13027: hashing another type keeps no to_hash override of a descriptor needed only for layout" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\Token := [Token(U64)].{
            \\    to_hash : Token, Hasher -> Hasher
            \\    to_hash = |_token, hasher| "UNUSED_HASH_MARKER".to_hash(hasher)
            \\}
            \\identity : a -> a
            \\identity = |x| x
            \\main! = |args| {
            \\    input = args.len()
            \\    token : Token
            \\    token = Token(input)
            \\    other_key = (input, input)
            \\    other = Dict.single(other_key, input)
            \\    n = if other.contains(other_key) {
            \\        match identity(token) { Token(k) => k + 1 }
            \\    } else { 0 }
            \\    echo!(n.to_str())
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .target_usize = width }, expectNoTokenToHash);
    }
}

test "issue 13027: inspecting a value through its boxy descriptor keeps its to_inspect override" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\Token := [Token(U64)].{
            \\    to_inspect : Token -> Str
            \\    to_inspect = |_token| "USED_INSPECT_MARKER"
            \\}
            \\identity : a -> a
            \\identity = |x| x
            \\main! = |args| {
            \\    token : Token
            \\    token = Token(args.len())
            \\    echo!(Str.inspect([identity(token)]))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .target_usize = width }, expectTokenToInspect);
    }
}
