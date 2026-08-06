//! Regression tests for issue #10298.

const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLir = harness.expectLowersToLir;

test "issue 10298: named record rest destructure in main lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10298.
    try expectLowersToLir(
        \\main! = |_| {
        \\    { name: _, ..rest } = { name: "Roc", count: 41.I64 }
        \\    echo!("${rest.count.to_str()}\n")
        \\    Ok({})
        \\}
    );
}

test "issue 10298: named record rest is available to match guards and bodies" {
    try harness.expectLowersToLirWithOptions(
        \\choose = |person| match person {
        \\    { name: _, ..rest } if rest.count > 0 => rest.count
        \\    { name: _, ..rest } => rest.count + 1
        \\}
        \\
        \\main! = |args| {
        \\    person = { name: "Roc", count: args.len() }
        \\    echo!(choose(person).to_str())
        \\    Ok({})
        \\}
    , .{ .inline_mode = .wrappers });
}
