//! Regression tests for https://github.com/roc-lang/roc/issues/11245.
//!
//! A function whose body hands a callback to a pure higher-order builtin
//! (such as `List.join_map`) requires that callback to be pure. When the
//! callback's effect is still unresolved at the point of that call, the
//! requirement must still be enforced once a later use resolves it to
//! effectful: calling such a function with an effectful argument is a type
//! mismatch. Without that enforcement an `expect` that performs an effect is
//! accepted and crashes at runtime.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "issue 11245 - effectful function in a list handed to a pure join_map callback is rejected" {
    // Mirrors paCore/Util.roc from the issue: `apply` is checked on its own,
    // before any caller fixes the effectfulness of `functions`' elements.
    const util_src =
        \\Util :: [].{
        \\    apply = |lines, functions| {
        \\        lines_lists(lines)
        \\            |> List.join_map(|list_list| functions.join_map(|f| f(list_list)))
        \\    }
        \\    expect apply(["A"], [|_| ["A"], |_| ["B"], |_| ["C"]]) |> List.is_eq(["A", "B", "C"])
        \\    expect apply(["A"], [|a| a, |a| a]) |> List.is_eq(["A", "A"])
        \\}
        \\
        \\lines_lists = |input_lines| {
        \\    aux = |lines, acc|
        \\        match lines {
        \\            [] => acc
        \\            [_, .. as t] => aux(t, acc.append(lines))
        \\        }
        \\    aux(input_lines, [])
        \\}
        \\expect [1, 2, 3] |> lines_lists |> List.is_eq([[1, 2, 3], [2, 3], [3]])
    ;

    var util_module = try TestEnv.init("Util", util_src);
    defer util_module.deinit();

    try util_module.assertNoErrors();

    // Mirrors pa2html/lib/Pa2Html.roc from the issue: one of the functions
    // passed to `Util.apply` is effectful, so every element of the list is
    // `List(Str) => List(Str)`, which `apply` must reject because it feeds
    // each element to the pure callback of `List.join_map`.
    const pa2html_src =
        \\import Util
        \\
        \\Pa2Html :: [].{
        \\    transform = |lines| Util.apply(lines, [title, subheading(1), gnuplot!])
        \\    expect transform([".TL Go Sleep", ".GP", "plot sin(x)"]) |> ["<h1>Go Sleep</h1>", "<hr>"].is_eq()
        \\}
        \\
        \\title = |lines| match lines {
        \\    [first, ..] if first.starts_with(".TL ") => ["<h1>${first.drop_prefix(".TL ")}</h1>", "<hr>"]
        \\    _ => []
        \\}
        \\
        \\subheading : U64 -> _
        \\subheading = |level|
        \\    |lines| match lines {
        \\        [first, ..] if first.starts_with(".SH${level.to_str()} ") => ["<h${(level + 1).to_str()}>${first}</h${(level + 1).to_str()}>"]
        \\        _ => []
        \\    }
        \\
        \\gnuplot! : List(Str) => List(Str)
        \\gnuplot! = |lines| match lines {
        \\    [".GP", .. as script] => script
        \\    _ => []
        \\}
    ;

    var test_env = try TestEnv.initWithImport("Pa2Html", pa2html_src, "Util", &util_module);
    defer test_env.deinit();

    try test_env.assertHasTypeError("Type Mismatch");
}

test "issue 11245 - minimal: unresolved callback effect absorbed by a pure callback annotation" {
    // `apply` is effect-polymorphic in `f` when checked, and the lambda
    // `|_| f(x)` is handed to `call_with`, whose annotation demands a pure
    // callback. That makes `f` pure in `apply`'s type, so passing the
    // effectful `go!` must be a type mismatch.
    const src =
        \\go! : Str => Str
        \\go! = |s| s
        \\
        \\call_with : (Str -> Str) -> Str
        \\call_with = |k| k("z")
        \\
        \\apply = |x, f| call_with(|_| f(x))
        \\
        \\expect apply("a", go!) == "a"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertHasTypeError("Type Mismatch");
}
