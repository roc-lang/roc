//! Regression tests for https://github.com/roc-lang/roc/issues/11471.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn expectTransparentAliasesErased(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    const types = &prepared.program.types;
    for (0..types.typeCount()) |index| {
        const content = types.get(@enumFromInt(index));
        if (content == .named) try std.testing.expect(content.named.kind != .alias);
    }
}

const count_app =
    \\Item : { uri : Str }
    \\count : Str -> U64
    \\count = |json| {
    \\    parsed : Try({ items : List(Item) }, _)
    \\    parsed = Json.parse(json)
    \\    match parsed {
    \\        Ok(record) => record.items.len()
    \\        Err(_) => 0
    \\    }
    \\}
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |_args| {
    \\    echo!(count("{\"items\":[{\"uri\":\"a\"}]}").to_str())
    \\    Ok({})
    \\}
;

test "issue 11471: a length method call on a parsed list field of an aliased element type lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(count_app, .{ .monotype_only = true });
}

test "issue 11471: a length method call on a parsed list field of an aliased element type lowers to LIR" {
    try harness.expectLowersToLir(count_app);
}

test "issue 11471: generic alias chains erase while recursive nominal backings remain" {
    try harness.expectLowersToLirWithOptions(
        \\Chain := [End, Next(Str, Chain)]
        \\Wrapped(a) : { value : a }
        \\Link : Chain
        \\Links : Wrapped(List(Link))
        \\count : Links -> U64
        \\count = |links| links.value.len()
        \\main! = |_args| {
        \\    echo!(count({ value: [Chain.Next("a", Chain.End)] }).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectTransparentAliasesErased });
}

test "issue 11471: a try-unwrapped parse keeps the aliased list element's parser call" {
    try harness.expectLowersToLirWithOptions(
        \\Item : { uri : Str }
        \\count : Str -> Try(U64, _)
        \\count = |json| {
        \\    parsed : Try({ items : List(Item) }, _)
        \\    parsed = Json.parse(json)
        \\    outer = parsed?
        \\    Ok(outer.items.len())
        \\}
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    _ = count("{\"items\":[{\"uri\":\"a\"}]}")
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}

test "issue 11471: a nested record around the aliased list element keeps its parser call" {
    try harness.expectLowersToLirWithOptions(
        \\Item : { uri : Str }
        \\count : Str -> U64
        \\count = |json| {
        \\    parsed : Try({ a : { items : List(Item) } }, _)
        \\    parsed = Json.parse(json)
        \\    match parsed {
        \\        Ok(record) => record.a.items.len()
        \\        Err(_) => 0
        \\    }
        \\}
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    echo!(count("{\"a\":{\"items\":[{\"uri\":\"a\"}]}}").to_str())
        \\    Ok({})
        \\}
    , .{ .monotype_only = true });
}
