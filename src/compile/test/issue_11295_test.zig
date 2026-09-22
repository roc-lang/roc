//! Regression test for issue #11295.

const std = @import("std");
const base = @import("base");
const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11295
//
// `items.get(index)` is a static-dispatch call whose selected evidence names a
// different checked callable than the constraint callable recorded on the
// dispatch plan. Boxy direct-call planning must analyze the selected
// instantiation, because only that callable relates the return type's generic
// payload back to the receiver's element representation. Analyzing the
// constraint callable instead produces a return-descriptor requirement over a
// representation that has no runtime binding at the call site.
const generic_method_dispatch_source =
    \\get : List(a), U64 -> Try(a, [OutOfBounds])
    \\get = |items, index| items.get(index)
    \\
    \\main! = |_args| if get(["x"], 0) == Ok("x") Ok({}) else Err(Exit(1))
;

test "issue 11295: a generic method-dispatch return descriptor is sourced from the receiver" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(generic_method_dispatch_source, .{ .specialization_strategy = strategy });
    }
}

const generic_qualified_call_source =
    \\get : List(a), U64 -> Try(a, [OutOfBounds])
    \\get = |items, index| List.get(items, index)
    \\
    \\main! = |_args| if get(["x"], 0) == Ok("x") Ok({}) else Err(Exit(1))
;

test "issue 11295: a generic qualified-call return descriptor is sourced from the argument" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(generic_qualified_call_source, .{ .specialization_strategy = strategy });
    }
}

// The same edge, with the selected method declared in another module. The
// evidence node's callable instantiation belongs to the calling module while
// the worker template belongs to the declaring module, so the call boundary
// and the worker identity must be read in their own module contexts.
test "issue 11295: an imported nominal method keeps its edge callable in the calling module" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Bag.roc", .data =
        \\Bag(a) := { items : List(a) }.{
        \\    peek : Bag(a), U64 -> Try(a, [OutOfBounds])
        \\    peek = |bag, index| List.get(bag.items, index)
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\import Bag exposing [Bag]
        \\first : Bag(a) -> Try(a, [OutOfBounds])
        \\first = |bag| bag.peek(0)
        \\strings : Bag(Str)
        \\strings = { items: ["x"] }
        \\main! = |_args| if first(strings) == Ok("x") Ok({}) else Err(Exit(1))
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args| match main!(args) {
        \\    Ok({}) => 0
        \\    Err(Exit(code)) => code
        \\    Err(_) => 1
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectAppPathLowersToLirWithOptions(app_path, .{ .specialization_strategy = strategy });
    }
}

// Iterator protocol calls select their target the same way, so `for` over a
// generic parameter exercises the iterator call boundary's edge callable: the
// `iter` and `next` relations are instantiated at the element type of this
// receiver, not at the protocol constraint's own element variable.
const generic_iterator_source =
    \\count_and_keep : List(a) -> Try((U64, List(a)), [Empty])
    \\count_and_keep = |items| {
    \\    var $total = 0
    \\    for _item in items {
    \\        $total = $total + 1
    \\    }
    \\    if $total == 0 Err(Empty) else Ok(($total, items))
    \\}
    \\
    \\main! = |_args| match count_and_keep(["x", "y"]) {
    \\    Ok((2, _kept)) => Ok({})
    \\    _ => Err(Exit(1))
    \\}
;

test "issue 11295: a generic iterator for loop instantiates its protocol calls at the receiver" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(generic_iterator_source, .{ .specialization_strategy = strategy });
    }
}
