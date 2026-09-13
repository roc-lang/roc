//! Regression test for issue #11217.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;
const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;
const std = @import("std");
const postcheck = @import("postcheck");

test "issue 11217: a local binding of a generalized function instantiates separately at each call" {
    // `alias` is a value binding whose right-hand side is the generalized
    // identity function, so it is a scheme too: the numeral call and the string
    // call each instantiate it, and the two instantiations never meet.
    try expectLowersToLir(
        \\id = |x| x
        \\
        \\main_alias = {
        \\    alias = id
        \\    (alias(1), alias("a"))
        \\}
        \\
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: alias chains preserve independent callable uses" {
    try expectLowersToLir(
        \\id = |x| x
        \\main_alias = {
        \\    first = id
        \\    second = first
        \\    third = second
        \\    (third(1), second("a"), first(Bool.True), third("b"))
        \\}
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: aliases retain local function captures across enclosing specializations" {
    try expectLowersToLir(
        \\make = |captured| {
        \\    pair = |x| (captured, x)
        \\    alias = pair
        \\    (alias(1), alias("a"))
        \\}
        \\main_alias = (make("capture"), make(42))
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: nested functions can reference an enclosing generalized alias" {
    try expectLowersToLir(
        \\id = |x| x
        \\main_alias = {
        \\    alias = id
        \\    use = |_| (alias(1), alias("a"))
        \\    use({})
        \\}
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: annotated aliases preserve fixed and quantified arguments" {
    try expectLowersToLir(
        \\first = |x, _y| x
        \\main_alias = {
        \\    alias : a, Str -> a
        \\    alias = first
        \\    (alias(1, "x"), alias("a", "y"))
        \\}
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: generalized aliases retain static dispatch evidence" {
    try expectLowersToLir(
        \\equal = |x, y| x == y
        \\main_alias = {
        \\    alias = equal
        \\    (alias(1, 1), alias("a", "a"))
        \\}
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: aliases of imported associated functions instantiate independently" {
    try expectLowersToLir(
        \\main_alias = {
        \\    length = List.len
        \\    (length([1]), length(["a"]))
        \\}
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: generalized aliases passed as function values retain their instantiations" {
    try expectLowersToLir(
        \\id = |x| x
        \\main_alias = {
        \\    alias = id
        \\    _ = Str.inspect(alias)
        \\    ([1].map(alias), ["a"].map(alias))
        \\}
        \\main! = |_args| {
        \\    _ = main_alias
        \\    Ok({})
        \\}
    );
}

test "issue 11217: Boxy materializes aliases at their typed uses" {
    try expectLowersToLirWithOptions(
        \\id = |x| x
        \\main! = |args| {
        \\    alias = id
        \\    _ = alias(args)
        \\    _ = alias(args.len())
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy });
}

test "issue 11217: Boxy aliases preserve runtime dictionaries" {
    try expectLowersToLirWithOptions(
        \\equal = |x, y| x == y
        \\main! = |args| {
        \\    eq = equal
        \\    _ = eq(args, [])
        \\    _ = eq(args.len(), 0)
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy });
}

test "issue 11217: Boxy capturing polymorphic declarations materialize at typed uses" {
    try expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    pair = |x| (args, x)
        \\    _ = pair(args)
        \\    _ = pair(args.len())
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy });
}

test "issue 11217: Boxy capturing aliases retain captures and dictionaries" {
    try expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    pair = |x, y| (args, x == y)
        \\    alias = pair
        \\    _ = alias(args, [])
        \\    _ = alias(args.len(), 0)
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy });
}

test "issue 11217: aliases reuse the original callable specialization families" {
    const sources = [_][]const u8{
        \\id = |x| x
        \\main! = |args| {
        \\    _ = id(args)
        \\    _ = id(args.len())
        \\    Ok({})
        \\}
        ,
        \\id = |x| x
        \\main! = |args| {
        \\    first = id
        \\    second = first
        \\    alias = second
        \\    _ = alias(args)
        \\    _ = alias(args.len())
        \\    Ok({})
        \\}
        ,
    };
    var diagnostics: [2]postcheck.Monotype.Lower.Diagnostics = @splat(.{});
    for (sources, &diagnostics) |source, *counts| {
        try expectLowersToLirWithOptions(source, .{
            .monotype_only = true,
            .monotype_diagnostics_out = counts,
        });
    }
    try std.testing.expect(diagnostics[0].specialization.template_misses > 0);
    try std.testing.expectEqual(diagnostics[0].specialization.template_misses, diagnostics[1].specialization.template_misses);
    try std.testing.expectEqual(diagnostics[0].specialization.nested_misses, diagnostics[1].specialization.nested_misses);
    try std.testing.expectEqual(diagnostics[0].body.deferred_template_bodies_lowered, diagnostics[1].body.deferred_template_bodies_lowered);
}
