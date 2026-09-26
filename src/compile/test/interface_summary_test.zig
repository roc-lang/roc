//! Interface constraint summaries preserve independent inputs and recursive closure.

const std = @import("std");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

test "interface summaries replay independent generic inputs with fresh-expansion verification" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(
        \\identity = |value| value
        \\through = |value| identity(value)
        \\render = |value| Json.to_str(through(value))
        \\main! = |args| {
        \\    echo!(render(args))
        \\    echo!(render(args))
        \\    echo!(render({ name: "first", value: 42 }))
        \\    echo!(render({ name: "second", value: 43 }))
        \\    Ok({})
        \\}
    , .{ .monotype_only = true, .monotype_diagnostics_out = &diagnostics });
    try std.testing.expect(diagnostics.specialization.interface_summary_hits > 0);
    try std.testing.expect(diagnostics.specialization.interface_summary_unchanged_hits > 0);
    try std.testing.expect(diagnostics.specialization.interface_summary_verifications > 0);
}

test "interface summaries share one expansion across parametric instantiations" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(
        \\count_items : List(a) -> U64
        \\count_items = |items| items.len()
        \\main! = |_args| {
        \\    echo!(count_items([1.U8, 2.U8]).to_str())
        \\    echo!(count_items(["a", "b", "c"]).to_str())
        \\    Ok({})
        \\}
    , .{ .monotype_only = true, .monotype_diagnostics_out = &diagnostics });
    try std.testing.expect(diagnostics.specialization.interface_parametric_requests >= 2);
    try std.testing.expect(diagnostics.specialization.interface_summary_hits > 0);
    try std.testing.expect(diagnostics.specialization.interface_summary_verifications > 0);
}

test "interface summaries finish mutually recursive components before reuse" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(
        \\first : a, U64 -> Try(a, [Oops])
        \\first = |value, count| if count == 0 Ok(value) else second(value, count - 1)
        \\second : a, U64 -> Try(a, [Oops])
        \\second = |value, count| if count == 0 Err(Oops) else first(value, count - 1)
        \\main! = |args| {
        \\    echo!(Str.inspect(first(args, 2)))
        \\    echo!(Str.inspect(first(args, 3)))
        \\    echo!(Str.inspect(second({ label: "recursive" }, 2)))
        \\    echo!(Str.inspect(second({ label: "recursive" }, 3)))
        \\    Ok({})
        \\}
    , .{ .monotype_only = true, .monotype_diagnostics_out = &diagnostics });
    try std.testing.expect(diagnostics.specialization.interface_summary_hits > 0);
}

test "interface summaries preserve open codec errors across worker store relocation" {
    try harness.expectSpecializationParallelismDeterministicLir(
        \\inner : {} -> Try({}, [Oops])
        \\inner = |{}| Ok({})
        \\outer : {} -> Try({}, [Oops])
        \\outer = |{}| inner({})
        \\decode = |body| {
        \\    result : Try({ foo : Str }, _)
        \\    result = Json.parse(body)
        \\    match result {
        \\        Err(err) => Err(Wrong(err))
        \\        Ok(_) => Ok({})
        \\    }
        \\}
        \\main! = |args| {
        \\    outer({})?
        \\    outer({})?
        \\    decode(Str.join_with(args, ""))
        \\}
    );
}

test "interface summaries include codec constraints discovered after a recursive edge" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(
        \\decode = |body| {
        \\    parsed : Try({ foo : Str }, _)
        \\    parsed = Json.parse(body)
        \\    match parsed {
        \\        Err(err) => Err(Wrong(err))
        \\        Ok(_) => Ok({})
        \\    }
        \\}
        \\first : Str, U64 -> Try({}, _)
        \\first = |body, count| if count > 0 second(body, count - 1) else decode(body)
        \\second : Str, U64 -> Try({}, _)
        \\second = |body, count| if count > 0 first(body, count - 1) else Err(Stopped)
        \\main! = |args| {
        \\    body = Str.join_with(args, "")
        \\    first(body, 2)?
        \\    first(body, 3)?
        \\    second(body, 2)?
        \\    second(body, 3)
        \\}
    , .{ .monotype_only = true, .monotype_diagnostics_out = &diagnostics });
    try std.testing.expect(diagnostics.specialization.interface_summary_verifications > 0);
}
