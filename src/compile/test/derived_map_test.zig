//! Derived `map`/`map!` maps exactly the payload checking selected for the
//! relation, whether the relation is checked at its call or at an
//! instantiation of a generic function, under both specialization
//! strategies. `main!` exits nonzero when a value comes back wrong.

const std = @import("std");
const base = @import("base");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual };

fn runApp(lowered: *const lir.CheckedPipeline.LoweredProgram) RunError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    const program = &lowered.lir_result;
    var static_strings = try eval.LirInterpreter.buildStaticStrings(std.testing.allocator, &program.store);
    defer static_strings.deinit();
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(
        std.testing.allocator,
        &program.store,
        &program.layouts,
        eval.LirInterpreter.BoxyTables.fromResult(program),
        static_strings.view(),
        host.get_ops(),
    );
    defer interpreter.deinit();
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;
    // A zeroed RocList is the empty args list.
    var empty_args = [_]usize{ 0, 0, 0 };
    var exit_code: i8 = -1;
    _ = try interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&empty_args),
        .ret_ptr = @ptrCast(&exit_code),
    });
    try std.testing.expectEqual(@as(i8, 0), exit_code);
    try host.checkForLeaks();
}

fn expectAppRuns(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered) catch |err| {
        std.log.err("derived map app run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn expectRunsWith(strategy: base.SpecializationStrategy, body: []const u8) harness.LowerToLirHarnessError!void {
    try harness.runLoweredInspection(body, .{ .specialization_strategy = strategy }, expectAppRuns);
}

const only_payload_program =
    \\wrap : Str -> [Found(Str), Missing]
    \\wrap = |s| if Str.is_empty(s) Missing else Found(s)
    \\
    \\shout = |w| w.map(|s| Str.concat(s, "!"))
    \\
    \\main! = |_args| {
    \\    direct = wrap("hi").map(|s| Str.concat(s, "?"))
    \\    generic = shout(wrap("yo"))
    \\    missing = shout(wrap(""))
    \\    out = match (direct, generic, missing) {
    \\        (Found(a), Found(b), Missing) => Str.concat(a, b)
    \\        _ => "wrong"
    \\    }
    \\    if out == "hi?yo!" { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: only payload, direct and generic (lss)" {
    try expectRunsWith(.lss, only_payload_program);
}

test "derived map: only payload, direct and generic (boxy)" {
    try expectRunsWith(.boxy, only_payload_program);
}

const zero_sized_siblings_direct_program =
    \\status : U64 -> [Ready({}, U64), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready({}, n)
    \\
    \\main! = |_args| {
    \\    out = match (status(21).map(|n| n * 2), status(1).map(|n| n * 2), status(0).map(|n| n * 2)) {
    \\        (Ready(_, a), Waiting(_), Done) => a
    \\        _ => 0
    \\    }
    \\    if out == 42 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: zero-sized sibling payloads at a direct call (lss)" {
    try expectRunsWith(.lss, zero_sized_siblings_direct_program);
}

test "derived map: zero-sized sibling payloads at a direct call (boxy)" {
    try expectRunsWith(.boxy, zero_sized_siblings_direct_program);
}

const zero_sized_siblings_generic_program =
    \\status : U64 -> [Ready({}, U64), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready({}, n)
    \\
    \\double = |w| w.map(|n| n * 2)
    \\
    \\main! = |_args| {
    \\    out = match (double(status(21)), double(status(1))) {
    \\        (Ready(_, a), Waiting(_)) => a
    \\        _ => 0
    \\    }
    \\    if out == 42 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: zero-sized sibling payloads through a generic function (lss)" {
    try expectRunsWith(.lss, zero_sized_siblings_generic_program);
}

test "derived map: zero-sized sibling payloads through a generic function (boxy)" {
    try expectRunsWith(.boxy, zero_sized_siblings_generic_program);
}

const leading_payload_program =
    \\status : U64 -> [Ready(U64, {}), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready(n, {})
    \\
    \\double = |w| w.map(|n| n * 2)
    \\
    \\main! = |_args| {
    \\    direct = status(5).map(|n| n + 1)
    \\    out = match (direct, double(status(21)), double(status(1))) {
    \\        (Ready(d, _), Ready(a, _), Waiting(_)) => a + d
    \\        _ => 0
    \\    }
    \\    if out == 48 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: payload before a zero-sized sibling (lss)" {
    try expectRunsWith(.lss, leading_payload_program);
}

test "derived map: payload before a zero-sized sibling (boxy)" {
    try expectRunsWith(.boxy, leading_payload_program);
}

const several_payload_tags_program =
    \\pick : U64 -> [Empty({}), Full(U64), Pair({}, {})]
    \\pick = |n| if n == 0 Empty({}) else if n == 1 Pair({}, {}) else Full(n)
    \\
    \\stringify = |w| w.map(U64.to_str)
    \\
    \\main! = |_args| {
    \\    direct = pick(7).map(|n| n + 1)
    \\    out = match (direct, stringify(pick(42)), pick(1).map(U64.to_str)) {
    \\        (Full(8), Full(s), Pair(_, _)) => s
    \\        _ => "wrong"
    \\    }
    \\    if out == "42" { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: several tags with zero-sized payloads (lss)" {
    try expectRunsWith(.lss, several_payload_tags_program);
}

test "derived map: several tags with zero-sized payloads (boxy)" {
    try expectRunsWith(.boxy, several_payload_tags_program);
}

const zero_sized_unions_program =
    \\main! = |_args| {
    \\    from_zero_sized = match Just({}).map(|_| "mapped") {
    \\        Just(s) => s
    \\    }
    \\    to_zero_sized = match Wrapped("x").map(|_| {}) {
    \\        Wrapped(_) => "!"
    \\    }
    \\    if Str.concat(from_zero_sized, to_zero_sized) == "mapped!" { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: zero-sized input and output unions (lss)" {
    try expectRunsWith(.lss, zero_sized_unions_program);
}

test "derived map: zero-sized input and output unions (boxy)" {
    try expectRunsWith(.boxy, zero_sized_unions_program);
}

const nested_generic_program =
    \\status : U64 -> [Ready({}, U64), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready({}, n)
    \\
    \\double = |w| w.map(|n| n * 2)
    \\
    \\quadruple = |w| double(double(w))
    \\
    \\main! = |_args| {
    \\    out = match (quadruple(status(10)), quadruple(status(0))) {
    \\        (Ready(_, a), Done) => a
    \\        _ => 0
    \\    }
    \\    if out == 40 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: nested generic functions (lss)" {
    try expectRunsWith(.lss, nested_generic_program);
}

test "derived map: nested generic functions (boxy)" {
    try expectRunsWith(.boxy, nested_generic_program);
}

const enclosing_type_variable_program =
    \\lift = |x, keep| {
    \\    tagged = if keep Found(x) else Missing
    \\    match tagged.map(|v| [v, v]) {
    \\        Found(list) => List.len(list)
    \\        Missing => 0
    \\    }
    \\}
    \\
    \\triple = |w| w.map(|n| [n, n, n])
    \\
    \\count_up = |x| {
    \\    match triple(Ready({}, x)) {
    \\        Ready(_, list) => List.len(list)
    \\    }
    \\}
    \\
    \\main! = |_args| {
    \\    total = lift("a", True) + lift(5, False) + count_up("b") + count_up(7)
    \\    if total == 8 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: payloads of an enclosing function's type variable (lss)" {
    try expectRunsWith(.lss, enclosing_type_variable_program);
}

test "derived map: payloads of an enclosing function's type variable (boxy)" {
    try expectRunsWith(.boxy, enclosing_type_variable_program);
}

const nominal_program =
    \\Maybe(a) := [Just(a), Nothing].{
    \\    map : _
    \\}
    \\
    \\Outcome(ok, err) := [Good(ok), Bad(err)].{
    \\    map : _
    \\}
    \\
    \\Labeled := [Labeled({}, U64), Unlabeled].{
    \\    map : _
    \\}
    \\
    \\bump = |m| m.map(|n| n + 1)
    \\
    \\main! = |_args| {
    \\    just : Maybe(U64)
    \\    just = Just(41)
    \\    nothing : Maybe(U64)
    \\    nothing = Nothing
    \\    texts = match (just.map(U64.to_str), bump(just), bump(nothing)) {
    \\        (Just(s), Just(n), Nothing) => Str.concat(s, U64.to_str(n))
    \\        _ => "wrong"
    \\    }
    \\    outcome : Outcome(U64, {})
    \\    outcome = Good(7)
    \\    good = match outcome.map(U64.to_str) {
    \\        Good(s) => s
    \\        Bad(_) => "bad"
    \\    }
    \\    labeled : Labeled
    \\    labeled = Labeled({}, 1)
    \\    label = match bump(labeled) {
    \\        Labeled(_, 2) => "x"
    \\        _ => "wrong"
    \\    }
    \\    if Str.concat(Str.concat(texts, good), label) == "41427x" { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: nominal unions with declared map (lss)" {
    try expectRunsWith(.lss, nominal_program);
}

test "derived map: nominal unions with declared map (boxy)" {
    try expectRunsWith(.boxy, nominal_program);
}

const effectful_program =
    \\status : U64 -> [Ready({}, U64), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready({}, n)
    \\
    \\double! = |w| w.map!(|n| n * 2)
    \\
    \\main! = |_args| {
    \\    direct = status(5).map!(|n| n + 1)
    \\    out = match (direct, double!(status(21)), double!(status(1))) {
    \\        (Ready(_, d), Ready(_, a), Waiting(_)) => a + d
    \\        _ => 0
    \\    }
    \\    if out == 48 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map!: zero-sized sibling payloads, direct and generic (lss)" {
    try expectRunsWith(.lss, effectful_program);
}

test "derived map!: zero-sized sibling payloads, direct and generic (boxy)" {
    try expectRunsWith(.boxy, effectful_program);
}

const replayed_zero_sized_siblings_program =
    \\status : U64 -> [Ready({}, U64), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready({}, n)
    \\
    \\double = |w| w.map(|n| n * 2)
    \\
    \\main! = |_args| {
    \\    out = match (double(status(21)), double(status(1)), double(status(0)), double(status(4))) {
    \\        (Ready(_, a), Waiting(_), Done, Ready(_, b)) => a + b
    \\        _ => 0
    \\    }
    \\    if out == 50 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: replayed uses of a generic function over zero-sized sibling payloads (lss)" {
    try expectRunsWith(.lss, replayed_zero_sized_siblings_program);
}

test "derived map: replayed uses of a generic function over zero-sized sibling payloads (boxy)" {
    try expectRunsWith(.boxy, replayed_zero_sized_siblings_program);
}

const replayed_several_payload_tags_program =
    \\pick : U64 -> [Empty({}), Full(U64), Pair({}, {})]
    \\pick = |n| if n == 0 Empty({}) else if n == 1 Pair({}, {}) else Full(n)
    \\
    \\stringify = |w| w.map(U64.to_str)
    \\
    \\main! = |_args| {
    \\    out = match (stringify(pick(42)), stringify(pick(0)), stringify(pick(1)), stringify(pick(7))) {
    \\        (Full(s), Empty(_), Pair(_, _), Full(t)) => Str.concat(s, t)
    \\        _ => "wrong"
    \\    }
    \\    if out == "427" { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: replayed uses over several tags with zero-sized payloads (lss)" {
    try expectRunsWith(.lss, replayed_several_payload_tags_program);
}

test "derived map: replayed uses over several tags with zero-sized payloads (boxy)" {
    try expectRunsWith(.boxy, replayed_several_payload_tags_program);
}

const replayed_with_local_method_program =
    \\pick : U64 -> [Empty({}), Full(U64), Pair({}, {})]
    \\pick = |n| if n == 0 Empty({}) else if n == 1 Pair({}, {}) else Full(n)
    \\
    \\stringify = |w| w.map(U64.to_str)
    \\
    \\count = |n| {
    \\    Counter := { count : U64 }.{
    \\        value = |counter| counter.count + 1
    \\    }
    \\    Counter.{ count: n }.value()
    \\}
    \\
    \\main! = |_args| {
    \\    out = match (stringify(pick(42)), stringify(pick(0)), stringify(pick(1)), stringify(pick(count(6)))) {
    \\        (Full(s), Empty(_), Pair(_, _), Full(t)) => Str.concat(s, t)
    \\        _ => "wrong"
    \\    }
    \\    if out == "427" { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: replayed uses in a module declaring a local method (lss)" {
    try expectRunsWith(.lss, replayed_with_local_method_program);
}

test "derived map: replayed uses in a module declaring a local method (boxy)" {
    try expectRunsWith(.boxy, replayed_with_local_method_program);
}

const replayed_nominal_program =
    \\Maybe(a) := [Just(a), Nothing].{
    \\    map : _
    \\}
    \\
    \\bump = |m| m.map(|n| n + 1)
    \\
    \\main! = |_args| {
    \\    just : Maybe(U64)
    \\    just = Just(41)
    \\    nothing : Maybe(U64)
    \\    nothing = Nothing
    \\    out = match (bump(just), bump(nothing), bump(bump(just))) {
    \\        (Just(a), Nothing, Just(b)) => a + b
    \\        _ => 0
    \\    }
    \\    if out == 85 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map: replayed uses over a nominal union (lss)" {
    try expectRunsWith(.lss, replayed_nominal_program);
}

test "derived map: replayed uses over a nominal union (boxy)" {
    try expectRunsWith(.boxy, replayed_nominal_program);
}

const replayed_effectful_program =
    \\status : U64 -> [Ready({}, U64), Waiting({}), Done]
    \\status = |n| if n == 0 Done else if n == 1 Waiting({}) else Ready({}, n)
    \\
    \\double! = |w| w.map!(|n| n * 2)
    \\
    \\main! = |_args| {
    \\    out = match (double!(status(21)), double!(status(1)), double!(status(0)), double!(status(4))) {
    \\        (Ready(_, a), Waiting(_), Done, Ready(_, b)) => a + b
    \\        _ => 0
    \\    }
    \\    if out == 50 { Ok({}) } else { Err(Exit(1)) }
    \\}
    \\
;

test "derived map!: replayed uses of an effectful generic function (lss)" {
    try expectRunsWith(.lss, replayed_effectful_program);
}

test "derived map!: replayed uses of an effectful generic function (boxy)" {
    try expectRunsWith(.boxy, replayed_effectful_program);
}
