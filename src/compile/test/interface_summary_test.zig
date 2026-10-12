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
    try std.testing.expectEqual(std.debug.runtime_safety, diagnostics.specialization.interface_summary_verifications > 0);
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
    try std.testing.expectEqual(std.debug.runtime_safety, diagnostics.specialization.interface_summary_verifications > 0);
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
    try std.testing.expectEqual(std.debug.runtime_safety, diagnostics.specialization.interface_summary_verifications > 0);
}

test "interface summaries share one expansion across user nominal container instantiations" {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(
        \\Chain(a) := [End, Next(a, Chain(a))].{
        \\    length : Chain(a) -> U64
        \\    length = |chain| length_help(chain, 0)
        \\}
        \\length_help : Chain(a), U64 -> U64
        \\length_help = |chain, count| match chain {
        \\    End => count
        \\    Next(_, rest) => length_help(rest, count + 1)
        \\}
        \\bytes : Chain(U8)
        \\bytes = Next(1, Next(2, End))
        \\words : Chain(Str)
        \\words = Next("a", Next("b", Next("c", End)))
        \\main! = |_args| {
        \\    echo!(Chain.length(bytes).to_str())
        \\    echo!(Chain.length(words).to_str())
        \\    Ok({})
        \\}
    , .{ .monotype_only = true, .monotype_diagnostics_out = &diagnostics });
    try std.testing.expect(diagnostics.specialization.interface_parametric_requests >= 2);
    try std.testing.expect(diagnostics.specialization.interface_summary_hits > 0);
    try std.testing.expectEqual(std.debug.runtime_safety, diagnostics.specialization.interface_summary_verifications > 0);
}

test "interface summaries stay verified for mutually recursive methods of an imported parametric container" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Index.roc", .data =
        \\Index(a) :: [Empty, Branch(List(Box(Index(a)))), Value(U64, a)].{
        \\    empty : {} -> Index(a)
        \\    empty = |{}| Empty
        \\    get : Index(a), U64 -> Try(a, [Missing])
        \\    get = |index, key| match index {
        \\        Empty => Err(Missing)
        \\        Value(stored_key, value) => if stored_key == key Ok(value) else Err(Missing)
        \\        Branch(children) => get(Box.unbox(children.get(key % 16) ?? crash "invalid branch"), key / 16)
        \\    }
        \\    set : Index(a), U64, a -> Index(a)
        \\    set = |index, key, value| match index {
        \\        Empty => Value(key, value)
        \\        Value(stored_key, previous) => if stored_key == key Value(key, value) else {
        \\            children = List.repeat(Box.box(Empty), 16)
        \\            with_previous = children.set(stored_key % 16, Box.box(Value(stored_key / 16, previous))) ?? crash "invalid split"
        \\            set_child(with_previous, key, value)
        \\        }
        \\        Branch(children) => set_child(children, key, value)
        \\    }
        \\    set_child : List(Box(Index(a))), U64, a -> Index(a)
        \\    set_child = |children, key, value|
        \\        Branch(children.update(key % 16, |child| Box.box(set(Box.unbox(child), key / 16, value))) ?? crash "invalid update")
        \\    remove : Index(a), U64 -> Index(a)
        \\    remove = |index, key| match index {
        \\        Empty => Empty
        \\        Value(stored_key, _) => if stored_key == key Empty else index
        \\        Branch(children) => Branch(children.update(key % 16, |child| Box.box(remove(Box.unbox(child), key / 16))) ?? crash "invalid removal")
        \\    }
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\import Index
        \\Action(a) := [Done, Update(a), Batch(List(Action(a)))]
        \\Route(a) : { id : U64, fire : a -> Action(a) }
        \\register! : List(Route(a)), Index(Route(a)) => Index(Route(a))
        \\register! = |routes, index| {
        \\    var $routes = index
        \\    for route in routes {
        \\        $routes = Index.set($routes, route.id, route)
        \\    }
        \\    $routes
        \\}
        \\main! = |args| {
        \\    key = List.len(args)
        \\    counts : Index(Route({ count : U64 }))
        \\    counts = register!([{ id: key, fire: |model| Update({ count: model.count + 1.U64 }) }, { id: key + 16, fire: |_| Done }], Index.empty({}))
        \\    if Index.get(counts, key).is_ok() Ok({}) else Err(Exit(1))
        \\}
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
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectAppPathLowersToLirWithOptions(app_path, .{ .monotype_only = true, .monotype_diagnostics_out = &diagnostics });
    try std.testing.expect(diagnostics.specialization.interface_parametric_requests >= 2);
    try std.testing.expectEqual(std.debug.runtime_safety, diagnostics.specialization.interface_summary_verifications > 0);
}

// `run`'s `Ok` payload is a checked variable nothing constrains, whose
// recorded default is the empty tag union. Every call after the first relates
// `outer`'s and `middle`'s interfaces from completed summaries, so a replay
// that closed the request's open leaf to that default early would change what
// dispatch evidence, reachability, and specialization identity read.
const open_leaf_forwarding =
    \\outer = |hooks| {
    \\    _ = hooks.get
    \\    middle(hooks)
    \\}
    \\middle = |hooks| {
    \\    get = hooks.get
    \\    _ = get("name")
    \\    run = hooks.run
    \\    output = run()?
    \\    parse(output.trim())
    \\}
    \\parse : Str -> Try(I32, _)
    \\parse = |_s| Ok(1)
    \\
;

test "interface summary replay of an open request leaf lowers like a fresh expansion within one body" {
    try harness.expectInterfaceSummaryReplayFaithfulLir(open_leaf_forwarding ++
        \\main! = |_args| {
        \\    hooks = { run: || Err(CommandNotFound), get: |_name| "x" }
        \\    _ = outer(hooks)
        \\    _ = outer(hooks)
        \\    Ok({})
        \\}
    );
}

test "interface summary replay of an open request leaf lowers like a fresh expansion across bodies" {
    try harness.expectInterfaceSummaryReplayFaithfulLir(open_leaf_forwarding ++
        \\first = |hooks| outer(hooks)
        \\second = |hooks| outer(hooks)
        \\main! = |_args| {
        \\    hooks = { run: || Err(CommandNotFound), get: |_name| "x" }
        \\    _ = first(hooks)
        \\    _ = second(hooks)
        \\    Ok({})
        \\}
    );
}
