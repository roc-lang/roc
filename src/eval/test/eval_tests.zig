//! Data-driven eval test definitions for the inspect-only parallel runner.

const TestCase = @import("parallel_runner.zig").TestCase;
const regression_repros = @import("eval_regression_repros.zig");
const trmc_tests = @import("eval_trmc_tests.zig");
const closure_recursion_tests = @import("eval_closure_recursion_tests.zig");
const comptime_finalization_tests = @import("eval_comptime_finalization_tests.zig");
const crypto_tests = @import("eval_crypto_tests.zig");
const highest_lowest_tests = @import("eval_highest_lowest_tests.zig");
const issue_tests = @import("eval_issue_tests.zig");
const interpreter_style_tests = @import("eval_interpreter_style_tests.zig");
const low_level_tests = @import("eval_low_level_tests.zig");
const polymorphism_tests = @import("eval_polymorphism_tests.zig");
const recursive_data_tests = @import("eval_recursive_data_tests.zig");

/// All eval test cases, consumed by the parallel runner.
///
/// Every value-producing test is observed solely through `Str.inspect(...)`.
const core_tests = [_]TestCase{
    // Frontend problems
    .{ .name = "problem: name not in scope", .source = "undefinedVar", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec plus int type mismatch", .source = "1.0.Dec + 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec minus int type mismatch", .source = "1.0.Dec - 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec times int type mismatch", .source = "1.0.Dec * 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec div int type mismatch", .source = "1.0.Dec / 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: int plus dec type mismatch", .source = "1.I64 + 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: int minus dec type mismatch", .source = "1.I64 - 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: int times dec type mismatch", .source = "1.I64 * 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: int div dec type mismatch", .source = "1.I64 / 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: F32.is_eq is intentionally unavailable", .source = "F32.is_eq(1.0.F32, 1.0.F32)", .expected = .{ .problem = {} } },
    .{ .name = "problem: F64.is_eq is intentionally unavailable", .source = "F64.is_eq(1.0.F64, 1.0.F64)", .expected = .{ .problem = {} } },
    .{
        .name = "problem: annotation-only top-level value is not a runtime value",
        .source_kind = .module,
        .source =
        \\missing : Str
        \\
        \\main = missing
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "problem: annotation-only value in untaken branch is still not a runtime value",
        .source_kind = .module,
        .source =
        \\missing : Str
        \\
        \\main = if False { missing } else { "ok" }
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "problem: to_inspect must return Str",
        .source_kind = .module,
        .source =
        \\BadColor := [Red, Green, Blue].{
        \\    to_inspect : BadColor -> I64
        \\    to_inspect = |color| match color {
        \\        Red => 1
        \\        Green => 2
        \\        Blue => 3
        \\    }
        \\}
        \\
        \\main = {
        \\    red : BadColor
        \\    red = Red
        \\    Str.inspect(red)
        \\}
        ,
        .expected = .{ .problem = {} },
    },

    .{
        .name = "allocation - Str.drop_prefix returns seamless slice without allocating copy",
        .source =
        \\{
        \\    prefix = "WASM_SEAMLESS_SLICE_PREFIX:"
        \\    payload = Str.repeat("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 1)
        \\    source = Str.concat(prefix, payload)
        \\
        \\    Str.drop_prefix(source, prefix)
        \\}
        ,
        .expected = .{ .allocations_at_most = .{
            .output = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            .max_allocations = 2,
        } },
    },

    .{
        .name = "allocation - List.sublist returns seamless slice without allocating copy",
        .source =
        \\{
        \\    prefix = [76, 73, 83, 84, 95, 83, 69, 65, 77, 76, 69, 83, 83, 95, 83, 76, 73, 67, 69, 95, 80, 82, 69, 70, 73, 88, 58]
        \\    payload = [97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90]
        \\    source = List.concat(prefix, payload)
        \\    slice = List.sublist(source, { start: List.len(prefix), len: List.len(payload) })
        \\
        \\    match Str.from_utf8(slice) {
        \\        Ok(str) => str
        \\        Err(_) => ""
        \\    }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{
            .output = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            .max_allocations = 3,
        } },
    },

    // In-place List.map: same element type, unique list -> the output reuses
    // the input allocation, so map itself must not allocate.
    .{
        .name = "allocation - List.map reuses unique same-type list in place",
        .source =
        \\{
        \\    base = List.concat([1, 2], [3, 4])
        \\    doubled = List.map(base, |x| x * 2)
        \\    if doubled == List.concat([2, 4], [6, 8]) {
        \\        "ok"
        \\    } else {
        \\        "bad"
        \\    }
        \\}
        ,
        // Four list literals plus two concats; the map itself contributes
        // zero. The copy path would allocate one more and fail this ceiling.
        .expected = .{ .allocations_at_most = .{
            .output = "ok",
            .max_allocations = 6,
        } },
    },

    .{
        .name = "inspect: List.map in place over unique same-type list",
        .source = "List.map(List.concat([1, 2], [3, 4]), |x| x * 10)",
        .expected = .{ .inspect_str = "[10.0, 20.0, 30.0, 40.0]" },
    },

    // Cross-type reuse: a tuple and a record with the same field layouts are
    // different types but interchangeable in one allocation, so a unique
    // list still maps in place.
    .{
        .name = "allocation - List.map reuses unique list across tuple-to-record types",
        .source =
        \\{
        \\    base = List.concat([(1, 2)], [(3, 4)])
        \\    swapped = List.map(base, |(a, b)| { x: b, y: a })
        \\    if swapped == List.concat([{ x: 2, y: 1 }], [{ x: 4, y: 3 }]) {
        \\        "ok"
        \\    } else {
        \\        "bad"
        \\    }
        \\}
        ,
        // Four list literals plus two concats; the map itself contributes
        // zero. The copy path would allocate one more and fail this ceiling.
        .expected = .{ .allocations_at_most = .{
            .output = "ok",
            .max_allocations = 6,
        } },
    },

    // Cross-type reuse with refcounted elements: ownership of each string
    // moves out of the slot and back in inside the new record.
    .{
        .name = "inspect: List.map in place across tuple-to-record with strings",
        .source = "List.map(List.concat([(\"a\", 1)], [(\"b\", 2)]), |(s, n)| { label: s, n: n })",
        .expected = .{ .inspect_str = "[{ label: \"a\", n: 1.0 }, { label: \"b\", n: 2.0 }]" },
    },

    // The list is shared (used again after the map), so map must take the
    // copy path and leave the original untouched.
    .{
        .name = "inspect: List.map leaves shared list unchanged",
        .source =
        \\{
        \\    a = List.concat([1], [2])
        \\    b = List.map(a, |x| x + 1)
        \\    (a, b)
        \\}
        ,
        .expected = .{ .inspect_str = "([1.0, 2.0], [2.0, 3.0])" },
    },

    // The transform captures the list it is mapping, which holds the
    // refcount above 1; in-place mutation here would make later elements
    // observe already-written outputs through the capture.
    .{
        .name = "inspect: List.map transform capturing the mapped list",
        .source =
        \\{
        \\    a = List.concat([5], [7])
        \\    List.map(a, |x| x + (match a.get(0) {
        \\        Ok(v) => v
        \\        Err(_) => 0
        \\    }))
        \\}
        ,
        .expected = .{ .inspect_str = "[10.0, 12.0]" },
    },

    // A seamless slice can be uniquely owned yet must never be mutated in
    // place, because its buffer points into the middle of an allocation.
    .{
        .name = "inspect: List.map over unique seamless slice",
        .source =
        \\{
        \\    s = List.sublist(List.concat([1, 2], [3, 4]), { start: 1, len: 2 })
        \\    List.map(s, |x| x * 2)
        \\}
        ,
        .expected = .{ .inspect_str = "[4.0, 6.0]" },
    },

    // A unique list emptied at runtime still goes through the in-place
    // branch; the loop must tolerate len == 0.
    .{
        .name = "inspect: List.map in place over emptied unique list",
        .source = "List.map(List.drop_first(List.concat([1], [2]), 2), |x| x + 1)",
        .expected = .{ .inspect_str = "[]" },
    },

    // Zero-sized elements have no allocation to reuse; the runtime check
    // lowers to a constant 0 and the copy path handles them.
    .{
        .name = "inspect: List.map over zero-sized elements",
        .source = "List.map(List.concat([{}], [{}]), |x| x)",
        .expected = .{ .inspect_str = "[{}, {}]" },
    },

    // Str -> Str maps are in-place eligible and the elements are refcounted:
    // the identity transform exercises element ownership moving out of and
    // back into the buffer without double-frees or leaks.
    .{
        .name = "inspect: List.map identity over unique list of strings",
        .source = "List.map(List.concat([\"hello\"], [\"world\"]), |s| s)",
        .expected = .{ .inspect_str = "[\"hello\", \"world\"]" },
    },

    // Heap-allocated strings: each transform result replaces a heap-owning
    // element, so the old element must be released exactly once.
    .{
        .name = "inspect: List.map concat over unique list of heap strings",
        .source = "List.map(List.concat([Str.repeat(\"x\", 30)], [Str.repeat(\"y\", 30)]), |s| Str.concat(s, \"!\"))",
        .expected = .{ .inspect_str = "[\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx!\", \"yyyyyyyyyyyyyyyyyyyyyyyyyyyyyy!\"]" },
    },

    // Shared list of refcounted elements: copy path, original intact.
    .{
        .name = "inspect: List.map leaves shared string list unchanged",
        .source =
        \\{
        \\    a = List.concat(["x"], ["y"])
        \\    b = List.map(a, |s| Str.concat(s, "!"))
        \\    (a, b)
        \\}
        ,
        .expected = .{ .inspect_str = "([\"x\", \"y\"], [\"x!\", \"y!\"])" },
    },

    // The transform crashes partway through an in-place map; crash is fatal,
    // so the half-rewritten buffer must never be observed by cleanup.
    .{
        .name = "inspect: List.map transform crashes mid-list",
        .source =
        \\List.map(List.concat(["a"], ["b", "c"]), |s| {
        \\    if Str.starts_with(s, "b") {
        \\        crash "boom"
        \\    } else {
        \\        s
        \\    }
        \\})
        ,
        .expected = .{ .crash = {} },
    },

    // Basic expressions and control flow
    .{ .name = "inspect: integer literal", .source = "42", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: negative integer literal", .source = "-1234", .expected = .{ .inspect_str = "-1234.0" } },
    .{ .name = "inspect: decimal literal", .source = "1.5", .expected = .{ .inspect_str = "1.5" } },
    .{ .name = "inspect: boolean true", .source = "True", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: boolean false", .source = "False", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: string literal", .source = "\"hello\"", .expected = .{ .inspect_str = "\"hello\"" } },
    .{ .name = "inspect: empty string literal", .source = "\"\"", .expected = .{ .inspect_str = "\"\"" } },
    .{
        .name = "inspect: top-level callable result from compile-time evaluation",
        .source_kind = .module,
        .source =
        \\make_adder = |n| |x| x + n
        \\
        \\add_one = make_adder(1)
        \\
        \\main = add_one(41)
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: top-level boxed callable result from compile-time evaluation",
        .source_kind = .module,
        .source =
        \\make_boxed_adder : I64 -> (I64 -> I64)
        \\make_boxed_adder = |n| {
        \\    boxed_n = Box.box(n)
        \\
        \\    |x| x + Box.unbox(boxed_n)
        \\}
        \\
        \\add_one : I64 -> I64
        \\add_one = make_boxed_adder(1)
        \\
        \\main = add_one(41)
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "inspect: top-level record constant containing callable value",
        .source_kind = .module,
        .source =
        \\table = { f: |x| x + 1 }
        \\
        \\main = {
        \\    f = table.f
        \\    f(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: nested top-level record constant containing callable value",
        .source_kind = .module,
        .source =
        \\table = { nested: { f: |x| x + 1 } }
        \\
        \\main = {
        \\    f = table.nested.f
        \\    f(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: top-level tag payload constant containing callable value",
        .source_kind = .module,
        .source =
        \\handler = Ok(|x| x + 1)
        \\
        \\main = match handler {
        \\    Ok(f) => f(41)
        \\    Err(_) => 0
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: top-level list constant containing callable values",
        .source_kind = .module,
        .source =
        \\fns = [|x| x + 1, |x| x + 2]
        \\
        \\main = match List.first(fns) {
        \\    Ok(f) => f(41)
        \\    Err(_) => 0
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: compile-time callable result reused through top-level data",
        .source_kind = .module,
        .source =
        \\make_adder = |n| |x| x + n
        \\
        \\add_one = make_adder(1)
        \\table = { f: add_one, nested: { g: add_one } }
        \\
        \\main = add_one(10) + (table.f)(20) + (table.nested.g)(9)
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: numeric default specialization remains replaceable until constrained",
        .source_kind = .module,
        .source =
        \\add_one = |x| x + 1
        \\
        \\force_u8 : U8 -> U8
        \\force_u8 = |n| n
        \\
        \\main = {
        \\    defaulted = add_one(41)
        \\    constrained = force_u8(add_one(41))
        \\
        \\    (defaulted, constrained)
        \\}
        ,
        .expected = .{ .inspect_str = "(42.0, 42)" },
    },
    .{
        .name = "inspect: annotated Dec specialization is final evidence",
        .source_kind = .module,
        .source =
        \\add_one_dec : Dec -> Dec
        \\add_one_dec = |x| x + 1
        \\
        \\main = add_one_dec(41)
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: custom from_numeral receives contextual integer digits",
        .source_kind = .module,
        .source =
        \\Big := [
        \\    Value({ is_negative: Bool, before: List(U8), after: List(U8), count: U64 }),
        \\].{
        \\    from_numeral : Numeral -> Try(Big, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Value({
        \\        is_negative: numeral.is_negative(),
        \\        before: numeral.digits_before_pt(),
        \\        after: numeral.digits_after_pt(),
        \\        count: numeral.digits_after_pt_count(),
        \\    }))
        \\}
        \\
        \\force : Big -> Big
        \\force = |n| n
        \\
        \\main = {
        \\    value = force(42)
        \\
        \\    match value {
        \\        Value(parts) => (parts.is_negative, parts.before, parts.after, parts.count)
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "(False, [42], [], 0)" },
    },
    .{
        .name = "inspect: custom from_numeral receives fractional digit count",
        .source_kind = .module,
        .source =
        \\Big := [
        \\    Value({ is_negative: Bool, before: List(U8), after: List(U8), count: U64 }),
        \\].{
        \\    from_numeral : Numeral -> Try(Big, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Value({
        \\        is_negative: numeral.is_negative(),
        \\        before: numeral.digits_before_pt(),
        \\        after: numeral.digits_after_pt(),
        \\        count: numeral.digits_after_pt_count(),
        \\    }))
        \\}
        \\
        \\main = {
        \\    value : Big
        \\    value = 3.14
        \\
        \\    match value {
        \\        Value(parts) => (parts.is_negative, parts.before, parts.after, parts.count)
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "(False, [3], [14], 2)" },
    },
    .{
        .name = "inspect: custom from_numeral preserves exact huge fractional digits",
        .source_kind = .module,
        .source =
        \\Big := [
        \\    Value({ is_negative: Bool, before: List(U8), after: List(U8), count: U64 }),
        \\].{
        \\    from_numeral : Numeral -> Try(Big, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Value({
        \\        is_negative: numeral.is_negative(),
        \\        before: numeral.digits_before_pt(),
        \\        after: numeral.digits_after_pt(),
        \\        count: numeral.digits_after_pt_count(),
        \\    }))
        \\}
        \\
        \\main = {
        \\    value = 340282366920938463463374607431768211456.00000000000000000001.Big
        \\
        \\    match value {
        \\        Value(parts) => (parts.is_negative, parts.before, parts.after, parts.count)
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "(False, [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [1], 20)" },
    },
    .{
        .name = "inspect: custom from_numeral literal pattern dispatches through is_eq",
        .source_kind = .module,
        .source =
        \\Code := [Code(List(U8))].{
        \\    from_numeral : Numeral -> Try(Code, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Code(numeral.digits_before_pt()))
        \\    is_eq : Code, Code -> Bool
        \\    is_eq = |a, b| match (a, b) {
        \\        (Code(x), Code(y)) => x == y
        \\    }
        \\}
        \\
        \\force : Code -> Code
        \\force = |n| n
        \\
        \\describe : Code -> Str
        \\describe = |code| match code {
        \\    42 => "yes"
        \\    _ => "no"
        \\}
        \\
        \\main = (describe(force(42)), describe(force(7)))
        ,
        .expected = .{ .inspect_str = "(\"yes\", \"no\")" },
    },
    .{
        .name = "inspect: custom from_numeral literal pattern compares converted values not raw digits",
        .source_kind = .module,
        .source =
        \\Tally := [Tally(U64)].{
        \\    from_numeral : Numeral -> Try(Tally, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Tally(numeral.digits_before_pt().len()))
        \\    is_eq : Tally, Tally -> Bool
        \\    is_eq = |a, b| match (a, b) {
        \\        (Tally(x), Tally(y)) => x == y
        \\    }
        \\}
        \\
        \\force : Tally -> Tally
        \\force = |n| n
        \\
        \\describe : Tally -> Str
        \\describe = |tally| match tally {
        \\    100 => "one byte"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe(force(999)), describe(force(7)))
        ,
        // digits_before_pt holds base-256 digits: 999 is two bytes, while 100
        // and 7 are one byte each — so 7 converts equal to the pattern literal
        // and 999 does not, proving the pattern compares converted values.
        .expected = .{ .inspect_str = "(\"other\", \"one byte\")" },
    },
    .{
        .name = "inspect: custom from_numeral codepoint literals convert in exprs and patterns",
        .source_kind = .module,
        .source =
        \\Code := [Code(List(U8))].{
        \\    from_numeral : Numeral -> Try(Code, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Code(numeral.digits_before_pt()))
        \\    is_eq : Code, Code -> Bool
        \\    is_eq = |a, b| match (a, b) {
        \\        (Code(x), Code(y)) => x == y
        \\    }
        \\}
        \\
        \\force : Code -> Code
        \\force = |n| n
        \\
        \\describe : Code -> Str
        \\describe = |code| match code {
        \\    'a' => "letter a"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe(force('a')), describe(force('b')))
        ,
        .expected = .{ .inspect_str = "(\"letter a\", \"other\")" },
    },
    .{
        .name = "inspect: custom from_numeral fractional literal pattern dispatches through is_eq",
        .source_kind = .module,
        .source =
        \\Scale := [Scale(U64)].{
        \\    from_numeral : Numeral -> Try(Scale, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Scale(numeral.digits_after_pt_count()))
        \\    is_eq : Scale, Scale -> Bool
        \\    is_eq = |a, b| match (a, b) {
        \\        (Scale(x), Scale(y)) => x == y
        \\    }
        \\}
        \\
        \\force : Scale -> Scale
        \\force = |n| n
        \\
        \\describe : Scale -> Str
        \\describe = |scale| match scale {
        \\    2.5 => "one decimal"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe(force(7.5)), describe(force(0.25)))
        ,
        .expected = .{ .inspect_str = "(\"one decimal\", \"other\")" },
    },
    .{
        .name = "inspect: imported custom from_numeral converts literals in exprs and patterns",
        .source_kind = .module,
        .source =
        \\import TallyMod
        \\
        \\force : TallyMod.Tally -> TallyMod.Tally
        \\force = |n| n
        \\
        \\describe : TallyMod.Tally -> Str
        \\describe = |tally| match tally {
        \\    100 => "one byte"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe(force(999)), describe(force(7)))
        ,
        .imports = &.{.{
            .name = "TallyMod",
            .source =
            \\Tally := [Tally(U64)].{
            \\    from_numeral : Numeral -> Try(Tally, [InvalidNumeral(Str)])
            \\    from_numeral = |numeral| Ok(Tally(numeral.digits_before_pt().len()))
            \\    is_eq : Tally, Tally -> Bool
            \\    is_eq = |a, b| match (a, b) {
            \\        (Tally(x), Tally(y)) => x == y
            \\    }
            \\}
            ,
        }},
        .expected = .{ .inspect_str = "(\"other\", \"one byte\")" },
    },
    .{
        .name = "inspect: custom from_quote receives literal Str",
        .source_kind = .module,
        .source =
        \\Tag := [Tag(Str)].{
        \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Tag(str))
        \\}
        \\
        \\force : Tag -> Tag
        \\force = |t| t
        \\
        \\main = force("Roc")
        ,
        .expected = .{ .inspect_str = "Tag(\"Roc\")" },
    },
    .{
        .name = "inspect: from_quote literal defaults to Str",
        .source_kind = .module,
        .source =
        \\main = "hello".concat("!")
        ,
        .expected = .{ .inspect_str = "\"hello!\"" },
    },
    .{
        .name = "inspect: custom from_quote literal pattern dispatches through is_eq",
        .source_kind = .module,
        .source =
        \\Tag := [Tag(Str)].{
        \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Tag(str))
        \\    is_eq : Tag, Tag -> Bool
        \\    is_eq = |a, b| match (a, b) {
        \\        (Tag(x), Tag(y)) => x == y
        \\    }
        \\}
        \\
        \\force : Tag -> Tag
        \\force = |t| t
        \\
        \\describe : Tag -> Str
        \\describe = |tag| match tag {
        \\    "yes" => "matched"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe(force("yes")), describe(force("no")))
        ,
        .expected = .{ .inspect_str = "(\"matched\", \"other\")" },
    },
    .{
        .name = "inspect: string literal patterns on Str keep working",
        .source_kind = .module,
        .source =
        \\describe : Str -> Str
        \\describe = |s| match s {
        \\    "hello" => "greeting"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe("hello"), describe("bye"))
        ,
        .expected = .{ .inspect_str = "(\"greeting\", \"other\")" },
    },
    .{
        .name = "inspect: string interpolation pattern captures between delimiters",
        .source_kind = .module,
        .source =
        \\describe : Str -> (Str, Str)
        \\describe = |s| match s {
        \\    "foo${bar}baz${qux}etc" => (bar, qux)
        \\    _ => ("miss", "miss")
        \\}
        \\
        \\main = describe("fooleftbazrightetc")
        ,
        .expected = .{ .inspect_str = "(\"left\", \"right\")" },
    },
    .{
        .name = "inspect: string interpolation pattern falls through when delimiter is missing",
        .source_kind = .module,
        .source =
        \\describe : Str -> Str
        \\describe = |s| match s {
        \\    "foo${bar}baz${_}" => bar
        \\    _ => "miss"
        \\}
        \\
        \\main = describe("fooleftbuxright")
        ,
        .expected = .{ .inspect_str = "\"miss\"" },
    },
    .{
        .name = "inspect: string interpolation pattern stops at first delimiter byte",
        .source_kind = .module,
        .source =
        \\describe : Str -> Str
        \\describe = |s| match s {
        \\    "foo${bar}baz" => bar
        \\    _ => "miss"
        \\}
        \\
        \\main = (describe("fooleftbaz"), describe("fooleftbzzbaz"))
        ,
        .expected = .{ .inspect_str = "(\"left\", \"miss\")" },
    },
    .{
        .name = "inspect: string interpolation pattern discard matches suffix",
        .source_kind = .module,
        .source =
        \\describe : Str -> Str
        \\describe = |s| match s {
        \\    "${_}.txt" => "text"
        \\    _ => "other"
        \\}
        \\
        \\main = (describe("notes.txt"), describe("notes.md"))
        ,
        .expected = .{ .inspect_str = "(\"text\", \"other\")" },
    },
    .{
        .name = "inspect: string interpolation pattern tail capture and discard",
        .source_kind = .module,
        .source =
        \\describe : Str -> (Str, Str)
        \\describe = |s| match s {
        \\    "foo${name}blah${_}" => (name, "tail")
        \\    "prefix${rest}" => (rest, "rest")
        \\    _ => ("miss", "miss")
        \\}
        \\
        \\main = (describe("fooBARblahanything"), describe("prefixVALUE"))
        ,
        .expected = .{ .inspect_str = "((\"BAR\", \"tail\"), (\"VALUE\", \"rest\"))" },
    },
    .{
        .name = "inspect: string interpolation pattern allows empty captures with exact suffix",
        .source_kind = .module,
        .source =
        \\piece : Str -> Str
        \\piece = |s| match s {
        \\    "a${mid}b" => mid
        \\    _ => "miss"
        \\}
        \\
        \\main = (piece("ab"), piece("acb"), piece("acbd"), piece("a"))
        ,
        .expected = .{ .inspect_str = "(\"\", \"c\", \"miss\", \"miss\")" },
    },
    .{
        .name = "inspect: string interpolation pattern discards at start middle and end",
        .source_kind = .module,
        .source =
        \\classify : Str -> Str
        \\classify = |s| match s {
        \\    "${_}user/${id}/posts/${_}" => id
        \\    _ => "miss"
        \\}
        \\
        \\main = (classify("/api/user/42/posts/latest"), classify("prefixuser//posts/"), classify("/api/user/42/comments/latest"))
        ,
        .expected = .{ .inspect_str = "(\"42\", \"\", \"miss\")" },
    },
    .{
        .name = "inspect: string interpolation pattern overlapping suffix branches",
        .source_kind = .module,
        .source =
        \\route : Str -> (Str, Str)
        \\route = |s| match s {
        \\    "file:${name}.txt" => ("txt", name)
        \\    "file:${name}.json" => ("json", name)
        \\    _ => ("miss", "")
        \\}
        \\
        \\main = (route("file:notes.txt"), route("file:data.json"), route("file:data.md"))
        ,
        .expected = .{ .inspect_str = "((\"txt\", \"notes\"), (\"json\", \"data\"), (\"miss\", \"\"))" },
    },
    .{
        .name = "inspect: string interpolation pattern capture feeds read-only str builtins",
        .source_kind = .module,
        .source =
        \\describe : Str -> (Bool, Bool, Bool, Bool, Bool)
        \\describe = |s| match s {
        \\    "foo${name}bar" => (name == "token", Str.starts_with(name, "to"), Str.ends_with(name, "en"), Str.contains(name, "ke"), Str.caseless_ascii_equals(name, "TOKEN"))
        \\    _ => (False, False, False, False, False)
        \\}
        \\
        \\main = (describe("footokenbar"), describe("foomissbar"))
        ,
        .expected = .{ .inspect_str = "((True, True, True, True, True), (False, False, False, False, False))" },
    },
    .{
        .name = "allocation - string interpolation pattern capture returns seamless heap slice",
        .source =
        \\{
        \\    prefix = "MATCH_PREFIX:"
        \\    payload = Str.repeat("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 1)
        \\    source = Str.concat(prefix, payload)
        \\
        \\    match source {
        \\        "MATCH_PREFIX:${rest}" =>
        \\            if Str.contains(rest, "mnop") and Str.caseless_ascii_equals(rest, payload) {
        \\                rest
        \\            } else {
        \\                ""
        \\            }
        \\        _ => ""
        \\    }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{
            .output = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            .max_allocations = 2,
        } },
    },
    .{
        .name = "allocation - string interpolation pattern capture feeds drop prefix suffix without copy",
        .source =
        \\{
        \\    prefix = "MATCH_PREFIX:"
        \\    payload = Str.repeat("DROPabcdefghijklmnopqrstuvwxyz0123456789KEEP", 1)
        \\    source = Str.concat(prefix, payload)
        \\
        \\    match source {
        \\        "MATCH_PREFIX:${rest}" => Str.drop_suffix(Str.drop_prefix(rest, "DROP"), "KEEP")
        \\        _ => ""
        \\    }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{
            .output = "abcdefghijklmnopqrstuvwxyz0123456789",
            .max_allocations = 2,
        } },
    },
    .{
        .name = "allocation - string interpolation pattern middle capture remains heap slice",
        .source =
        \\{
        \\    payload = Str.repeat("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 1)
        \\    source = Str.concat(Str.concat("prefix/user/", payload), "/posts/tail")
        \\
        \\    match source {
        \\        "${_}/user/${id}/posts/${_}" => id
        \\        _ => ""
        \\    }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{
            .output = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            .max_allocations = 3,
        } },
    },
    .{
        .name = "inspect: string literal type suffix pins custom from_quote target",
        .source_kind = .module,
        .source =
        \\Tag := [Tag(Str)].{
        \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Tag(str))
        \\}
        \\
        \\main = "Roc".Tag
        ,
        .expected = .{ .inspect_str = "Tag(\"Roc\")" },
    },
    .{
        .name = "inspect: string literal Str type suffix",
        .source_kind = .module,
        .source =
        \\main = "hello".Str
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: multiline string literal type suffix on its own line",
        .source_kind = .module,
        .source =
        \\Tally := [Tally(U64)].{
        \\    from_quote : Str -> Try(Tally, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Tally(str.count_utf8_bytes()))
        \\}
        \\
        \\main = {
        \\    value =
        \\        \\line one
        \\        \\line two
        \\        .Tally
        \\    value
        \\}
        ,
        .expected = .{ .inspect_str = "Tally(17)" },
    },
    .{
        .name = "inspect: custom from_interpolation interpolates Str into a custom type",
        .source_kind = .module,
        .source =
        \\Url := [Url(Str)].{
        \\    from_quote : Str -> Try(Url, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Url(str))
        \\    from_interpolation : Str, Iter((Str, Str)) -> Url
        \\    from_interpolation = |first, rest| {
        \\        Url.Url(rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment)))
        \\    }
        \\    inner : Url -> Str
        \\    inner = |Url.Url(str)| str
        \\}
        \\
        \\main = {
        \\    domain = "example"
        \\    url : Url
        \\    url = "https://${domain}.com"
        \\    url
        \\}
        ,
        .expected = .{ .inspect_str = "Url(\"https://example.com\")" },
    },
    .{
        .name = "inspect: Try interpolation forwards to custom result type",
        .source_kind = .module,
        .source =
        \\Url := [Url(Str)].{
        \\    from_interpolation : Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])
        \\    from_interpolation = |first, rest| Ok(Url.Url(rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment))))
        \\}
        \\
        \\main = {
        \\    domain = "example"
        \\    url : Try(Url, [InvalidUrl])
        \\    url = "https://${domain}.com"
        \\    url
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(Url(\"https://example.com\"))" },
    },
    .{
        .name = "problem: nested Try interpolation does not recursively satisfy forwarding",
        .source_kind = .module,
        .source =
        \\Url := [Url(Str)].{
        \\    from_interpolation : Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])
        \\    from_interpolation = |first, rest| Ok(Url.Url(rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment))))
        \\}
        \\
        \\main = {
        \\    domain = "example"
        \\    url : Try(Try(Url, [InvalidUrl]), [Outer])
        \\    url = "https://${domain}.com"
        \\    url
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "inspect: suffixed interpolation accepts custom return type",
        .source_kind = .module,
        .source =
        \\Url := [Url(Str)].{
        \\    from_interpolation : Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])
        \\    from_interpolation = |first, rest| Ok(Url.Url(rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment))))
        \\}
        \\
        \\main = {
        \\    domain = "example"
        \\    "https://${domain}.com".Url
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(Url(\"https://example.com\"))" },
    },
    .{
        .name = "inspect: interpolation with adjacent and boundary interpolations",
        .source_kind = .module,
        .source =
        \\main = {
        \\    a = "one"
        \\    b = "two"
        \\    "${a}${b} and ${a}"
        \\}
        ,
        .expected = .{ .inspect_str = "\"onetwo and one\"" },
    },
    .{
        .name = "custom from_quote Err is a compile-time problem",
        .source_kind = .module,
        .source =
        \\Strict := [Strict].{
        \\    from_quote : Str -> Try(Strict, [BadQuotedBytes(Str)])
        \\    from_quote = |_str| Err(BadQuotedBytes("Strict rejects every string"))
        \\}
        \\
        \\force : Strict -> Strict
        \\force = |s| s
        \\
        \\main = force("nope")
        ,
        .expected = .problem,
    },
    .{
        .name = "custom from_numeral Err is a compile-time problem",
        .source_kind = .module,
        .source =
        \\Picky := [Picky].{
        \\    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
        \\    from_numeral = |_numeral| Err(InvalidNumeral("Picky rejects every numeral"))
        \\}
        \\
        \\force : Picky -> Picky
        \\force = |n| n
        \\
        \\main = force(42)
        ,
        .expected = .problem,
    },
    .{
        .name = "custom from_numeral Err in an uncalled function is a compile-time problem",
        .source_kind = .module,
        .source =
        \\Picky := [Picky].{
        \\    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
        \\    from_numeral = |_numeral| Err(InvalidNumeral("Picky rejects every numeral"))
        \\}
        \\
        \\unused : {} -> Picky
        \\unused = |_| 42
        \\
        \\main = "ok"
        ,
        .expected = .problem,
    },
    .{
        .name = "inspect: unconstrained empty list specialization remains replaceable until constrained",
        .source_kind = .module,
        .source =
        \\empty = || []
        \\
        \\force_strings : List(Str) -> List(Str)
        \\force_strings = |xs| xs
        \\
        \\main = {
        \\    unconstrained_len = empty().len()
        \\    strings = force_strings(empty())
        \\
        \\    (unconstrained_len, strings)
        \\}
        ,
        .expected = .{ .inspect_str = "(0, [])" },
    },
    .{
        .name = "inspect: explicit empty and zero-sized types are final evidence",
        .source_kind = .module,
        .source =
        \\main = {
        \\    impossible_items : List([])
        \\    impossible_items = []
        \\
        \\    markers = [{}, {}, {}]
        \\
        \\    (impossible_items.len(), markers.len())
        \\}
        ,
        .expected = .{ .inspect_str = "(0, 3)" },
    },
    .{
        .name = "inspect: Bool stored boxed tagged and passed as ordinary Roc value",
        .source_kind = .module,
        .source =
        \\choose : Bool, a, a -> a
        \\choose = |flag, yes, no| if flag yes else no
        \\
        \\main = {
        \\    boxed = Box.box(True)
        \\    record = { first: Box.unbox(boxed), second: False }
        \\    tagged = Ok(record.first)
        \\
        \\    match tagged {
        \\        Ok(True) => choose(record.second, 0, 42)
        \\        Ok(False) => 1
        \\        Err(_) => 2
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: top-level Bool constants inside heap containers remain values",
        .source_kind = .module,
        .source =
        \\flags = [False, True, False]
        \\table = { boxed: Box.box(True), tagged: Ok(False) }
        \\
        \\main = {
        \\    from_list = match List.get(flags, 1) {
        \\        Ok(flag) => flag
        \\        Err(_) => False
        \\    }
        \\
        \\    from_tag = match table.tagged {
        \\        Ok(flag) => flag
        \\        Err(_) => True
        \\    }
        \\
        \\    from_list == Box.unbox(table.boxed) and from_tag == False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: Str.inspect uses nominal to_inspect",
        .source_kind = .module,
        .source =
        \\Color := [Red, Green, Blue].{
        \\    to_inspect : Color -> Str
        \\    to_inspect = |color| match color {
        \\        Red => "Color::Red"
        \\        Green => "Color::Green"
        \\        Blue => "Color::Blue"
        \\    }
        \\}
        \\
        \\main = {
        \\    red : Color
        \\    red = Red
        \\    Str.inspect(red) == "Color::Red"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: Str.inspect default when no to_inspect exists",
        .source_kind = .module,
        .source =
        \\ColorDefault := [Red, Green, Blue]
        \\
        \\main = {
        \\    red : ColorDefault
        \\    red = Red
        \\    Str.inspect(red) == "Red"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: opaque nominal without to_inspect hides backing",
        .source_kind = .module,
        .source =
        \\Secret :: { key : Str }.{
        \\    new : Str -> Secret
        \\    new = |key| { key: key }
        \\}
        \\
        \\main = {
        \\    secret : Secret
        \\    secret = Secret.new("my_secret_key")
        \\    Str.inspect(secret)
        \\}
        ,
        .expected = .{ .inspect_str = "\"<opaque>\"" },
    },
    .{
        .name = "inspect: nested Str.inspect uses payload to_inspect",
        .source_kind = .module,
        .source =
        \\Color := [Red, Green, Blue].{
        \\    to_inspect : Color -> Str
        \\    to_inspect = |color| match color {
        \\        Red => "Color::Red"
        \\        Green => "Color::Green"
        \\        Blue => "Color::Blue"
        \\    }
        \\}
        \\
        \\main = {
        \\    red : Color
        \\    red = Red
        \\    record = { color: red, count: 42, name: "test" }
        \\    Str.inspect(record) == "{ color: Color::Red, count: 42.0, name: \"test\" }"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{ .name = "inspect: arithmetic", .source = "2 + 3 * 4", .expected = .{ .inspect_str = "14.0" } },
    .{ .name = "inspect: subtraction", .source = "5 - 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: multiplication", .source = "4 * 5", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: integer division", .source = "10 // 2", .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "inspect: modulo", .source = "7 % 3", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: if expression", .source = "if (1 == 1) 42 else 99", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: if false branch", .source = "if (1 == 2) 42 else 99", .expected = .{ .inspect_str = "99.0" } },
    .{ .name = "inspect: nested if inner true", .source = "if (1 == 1) (if (2 == 2) 100 else 200) else 300", .expected = .{ .inspect_str = "100.0" } },
    .{ .name = "inspect: nested if inner false", .source = "if (1 == 1) (if (2 == 3) 100 else 200) else 300", .expected = .{ .inspect_str = "200.0" } },
    .{ .name = "inspect: nested if outer false", .source = "if (1 == 2) (if (2 == 2) 100 else 200) else 300", .expected = .{ .inspect_str = "300.0" } },
    .{ .name = "inspect: tuple literal", .source = "(1, 2)", .expected = .{ .inspect_str = "(1.0, 2.0)" } },
    .{ .name = "inspect: tuple arithmetic", .source = "(5 + 1, 5 * 3)", .expected = .{ .inspect_str = "(6.0, 15.0)" } },
    .{ .name = "inspect: record field", .source = "{ x: 42, y: 99 }.x", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: record field second", .source = "{ x: 10, y: 20 }.y", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: nested record access", .source = "{ outer: { inner: 42 } }.outer.inner", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: deeper nested record access", .source = "{ a: { b: { c: 100 } } }.a.b.c", .expected = .{ .inspect_str = "100.0" } },
    .{ .name = "inspect: record field order independence", .source = "{ x: 1, y: 2 }.x + { y: 2, x: 1 }.x", .expected = .{ .inspect_str = "2.0" } },
    .{
        .name = "inspect: mixed-alignment record survives closure and list round trip",
        .source =
        \\{
        \\    make = || { a: 1.U8, b: 2.U64, c: 3.U16, d: True, e: 4.U8 }
        \\    wrap = |value| { items: [value], keep: value }
        \\    wrapped = wrap(make())
        \\    wrapped.items == [wrapped.keep]
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: function returning record with dec field",
        .source =
        \\{
        \\    make = |n| { x: n }
        \\    make(5).x
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: function returning nested record with dec field",
        .source =
        \\{
        \\    make = |n| { outer: { inner: n } }
        \\    make(5).outer.inner
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{ .name = "inspect: unary minus literal", .source = "-5", .expected = .{ .inspect_str = "-5.0" } },
    .{ .name = "inspect: unary minus nested", .source = "-(-10)", .expected = .{ .inspect_str = "10.0" } },
    .{ .name = "inspect: unary minus arithmetic", .source = "-(3 + 4)", .expected = .{ .inspect_str = "-7.0" } },
    .{ .name = "inspect: precedence with parentheses", .source = "(2 + 3) * 4", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: subtraction associativity", .source = "100 - 20 - 10", .expected = .{ .inspect_str = "70.0" } },
    .{ .name = "inspect: subtraction grouping", .source = "100 - (20 - 10)", .expected = .{ .inspect_str = "90.0" } },
    .{ .name = "inspect: mixed add subtract associativity", .source = "1 - 2 + 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: chained multiplication", .source = "2 * 3 * 4 * 5", .expected = .{ .inspect_str = "120.0" } },
    .{ .name = "inspect: integer division associativity", .source = "80 // 8 // 2", .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "inspect: integer division grouping", .source = "80 // (8 // 2)", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: modulo associativity", .source = "100 % 30 % 7", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "inspect: modulo grouping", .source = "100 % (30 % 7)", .expected = .{ .inspect_str = "0.0" } },
    .{ .name = "inspect: hexadecimal literal", .source = "0xFF", .expected = .{ .inspect_str = "255.0" } },
    .{ .name = "inspect: binary literal", .source = "0b1010", .expected = .{ .inspect_str = "10.0" } },
    .{
        .name = "inspect: interpolation",
        .source =
        \\{
        \\    hello = "Hello"
        \\    world = "World"
        \\    "${hello} ${world}"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World\"" },
    },
    .{ .name = "inspect: large string literal", .source = "\"This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting\"", .expected = .{ .inspect_str = "\"This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting\"" } },
    .{ .name = "inspect: small string literal", .source = "\"Small string test\"", .expected = .{ .inspect_str = "\"Small string test\"" } },
    .{ .name = "inspect: conditional string", .source = "if True \"This is a large string that exceeds small string optimization\" else \"Short\"", .expected = .{ .inspect_str = "\"This is a large string that exceeds small string optimization\"" } },
    .{ .name = "inspect: nested conditional string", .source = "if True (if False \"Inner small\" else \"Inner large string that exceeds small string optimization\") else \"Outer\"", .expected = .{ .inspect_str = "\"Inner large string that exceeds small string optimization\"" } },
    .{ .name = "inspect: record field small string", .source = "{ foo: \"Hello\" }.foo", .expected = .{ .inspect_str = "\"Hello\"" } },
    .{ .name = "inspect: record field large string", .source = "{ foo: \"This is a very long string that definitely exceeds the small string optimization limit\" }.foo", .expected = .{ .inspect_str = "\"This is a very long string that definitely exceeds the small string optimization limit\"" } },

    // Equality and mutable record cases
    .{ .name = "inspect: empty record equality", .source = "{} == {}", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "inspect: mutable record equality",
        .source =
        \\{
        \\    var $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: mutable record rebind equality",
        .source =
        \\{
        \\    var $x = { sum: 0 }
        \\    $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: mutable record loop accumulator equality",
        .source =
        \\{
        \\    var $acc = { sum: 0 }
        \\    for item in [1, 2, 3] {
        \\        $acc = { sum: $acc.sum + item }
        \\    }
        \\    $acc == { sum: 6 }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{ .name = "inspect: string field equality true", .source = "{ name: \"hello\" } == { name: \"hello\" }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: string field equality false", .source = "{ name: \"hello\" } == { name: \"world\" }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: nested record equality true", .source = "{ a: { x: 1 }, b: 2 } == { a: { x: 1 }, b: 2 }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: nested record equality false", .source = "{ a: { x: 1 }, b: 2 } == { a: { x: 2 }, b: 2 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: nested tuple equality true", .source = "((1, 2), 3) == ((1, 2), 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: nested tuple equality false", .source = "(1, (2, 3)) == (1, (2, 9))", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality same tag no payload ok ok", .source = "Ok == Ok", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality same tag no payload err err", .source = "Err == Err", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality same tag no payload ok err", .source = "Ok == Err", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality same tag no payload err ok", .source = "Err == Ok", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality same tag with payload equal", .source = "Ok(1) == Ok(1)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality same tag with payload not equal", .source = "Ok(1) == Ok(2)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality err payload equal", .source = "Err(1) == Err(1)", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "inspect: tag union equality different tags with payload",
        .source =
        \\{
        \\    x = Ok(1)
        \\    y = if Bool.False Ok(1) else Err(1)
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{ .name = "inspect: tag union match direct numeric payload", .source = "match Ok(10) { Ok(n) => n + 5, Err(_) => 0 }", .expected = .{ .inspect_str = "15.0" } },
    .{ .name = "inspect: tag union match direct record payload", .source = "match Ok({ value: 10 }) { Ok({ value }) => value + 5, Err(_) => 0 }", .expected = .{ .inspect_str = "15.0" } },
    .{ .name = "inspect: tag union equality string payloads equal", .source = "Ok(\"hello\") == Ok(\"hello\")", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality string payloads not equal", .source = "Ok(\"hello\") == Ok(\"world\")", .expected = .{ .inspect_str = "False" } },
    .{
        .name = "inspect: tag union equality three or more tags equal direct",
        .source =
        \\{
        \\    x = Red
        \\    y = Red
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: tag union equality three or more tags equal through if",
        .source =
        \\{
        \\    x = Red
        \\    y = if Bool.True Red else if Bool.True Green else Blue
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: tag union equality three or more tags not equal",
        .source =
        \\{
        \\    x = Red
        \\    y = if Bool.False Red else Green
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{ .name = "inspect: record inequality equal records", .source = "{ x: 1, y: 2 } != { x: 1, y: 2 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record inequality different records", .source = "{ x: 1, y: 2 } != { x: 1, y: 3 }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record inequality reordered equal records", .source = "{ x: 1, y: 2 } != { y: 2, x: 1 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tuple inequality equal tuples", .source = "(1, 2) != (1, 2)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tuple inequality different tuples", .source = "(1, 2) != (1, 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union inequality same tag no payload", .source = "Ok != Ok", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union inequality different tags no payload", .source = "Ok != Err", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union inequality same payload", .source = "Ok(1) != Ok(1)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union inequality different payload", .source = "Ok(1) != Ok(2)", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "inspect: match with tag containing pattern-bound variable regression",
        .source =
        \\match Some("x") {
        \\    Some(a) => Tagged(a)
        \\    None => Tagged("")
        \\}
        ,
        .expected = .{ .inspect_str = "Tagged(\"x\")" },
    },
    .{
        .name = "inspect: nested match with Result type regression",
        .source =
        \\match ["x"] {
        \\    [a] => {
        \\        match Ok(a) {
        \\            Ok(val) => Ok(val),
        \\            _ => Err(Oops)
        \\        }
        \\    }
        \\    _ => Err(Oops)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(\"x\")" },
    },
    .{ .name = "inspect: list equality single element regression", .source = "[1] == [1]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: list equality nested lists regression", .source = "[[1, 2]] == [[1, 2]]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: list equality single string element regression", .source = "[\"hello\"] == [\"hello\"]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record with list equality unequal length regression", .source = "{ a: [1] } == { a: [1, 2] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality unequal values regression", .source = "{ a: [1] } == { a: [2] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality empty vs singleton regression", .source = "{ a: [] } == { a: [1] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality singleton vs empty regression", .source = "{ a: [1] } == { a: [] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality mixed fields regression", .source = "{ a: [], b: 1 } == { a: [2], b: 1 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list inequality unequal length regression", .source = "{ a: [1] } != { a: [1, 2] }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record with list equality equal singleton regression", .source = "{ a: [1] } == { a: [1] }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record with list equality equal empty regression", .source = "{ a: [] } == { a: [] }", .expected = .{ .inspect_str = "True" } },

    // Typed lambdas and captures from the old eval suite
    .{ .name = "inspect: typed simple lambda increment", .source = "(|x| x + 1.I64)(5.I64)", .expected = .{ .inspect_str = "6" } },
    .{ .name = "inspect: typed simple lambda arithmetic", .source = "(|x| x * 2.I64 + 1.I64)(10.I64)", .expected = .{ .inspect_str = "21" } },
    .{ .name = "inspect: typed multi parameter lambda", .source = "(|x, y| x + y)(3.I64, 4.I64)", .expected = .{ .inspect_str = "7" } },
    .{ .name = "inspect: typed three parameter lambda", .source = "(|a, b, c| a + b + c)(1.I64, 2.I64, 3.I64)", .expected = .{ .inspect_str = "6" } },
    .{ .name = "inspect: typed lambda if body positive", .source = "(|x| if x > 0.I64 x else 0.I64)(5.I64)", .expected = .{ .inspect_str = "5" } },
    .{ .name = "inspect: typed lambda if body negative", .source = "(|x| if x > 0.I64 x else 0.I64)(-3.I64)", .expected = .{ .inspect_str = "0" } },
    .{
        .name = "inspect: crash not taken when lambda condition true",
        .source =
        \\(|x| if x > 0.I64 x else {
        \\    crash "this should not execute"
        \\    0.I64
        \\})(10.I64)
        ,
        .expected = .{ .inspect_str = "10" },
    },
    .{
        .name = "crash: lambda else branch crash",
        .source =
        \\(|x| if x > 0.I64 x else {
        \\    crash "crash in else!"
        \\    0.I64
        \\})(-5.I64)
        ,
        .expected = .{ .crash = {} },
    },
    .{ .name = "inspect: typed lambda unary minus", .source = "(|x| -x)(5.I64)", .expected = .{ .inspect_str = "-5" } },
    .{ .name = "inspect: typed lambda ignore arg", .source = "(|_x| 5.I64)(99.I64)", .expected = .{ .inspect_str = "5" } },
    .{ .name = "inspect: typed curried lambda", .source = "(|a| |b| a * b)(5.I64)(10.I64)", .expected = .{ .inspect_str = "50" } },
    .{ .name = "inspect: typed triple curried lambda", .source = "(((|a| |b| |c| a + b + c)(100.I64))(20.I64))(3.I64)", .expected = .{ .inspect_str = "123" } },
    .{ .name = "inspect: typed multi-arg lambda returning lambda", .source = "(|a, b, c| |d| a + b + c + d)(10.I64, 20.I64, 5.I64)(7.I64)", .expected = .{ .inspect_str = "42" } },
    .{ .name = "inspect: typed nested captures", .source = "(|y| (|x| (|z| x + y + z)(3.I64))(2.I64))(1.I64)", .expected = .{ .inspect_str = "6" } },
    .{
        .name = "inspect: typed captured lambda",
        .source =
        \\{
        \\    x = 10.I64
        \\    f = |y| x + y
        \\    f(5.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "15" },
    },
    .{
        .name = "inspect: typed captured lambda multiple vars",
        .source =
        \\{
        \\    x = 20.I64
        \\    y = 30.I64
        \\    f = |z| x + y + z
        \\    f(10.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: typed lambda many captures",
        .source =
        \\{
        \\    a = 100.I64
        \\    b = 200.I64
        \\    c = 300.I64
        \\    d = 400.I64
        \\    f = |n| a + b + c + d + n
        \\    f(5.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "1005" },
    },
    .{
        .name = "inspect: typed nested closure blocks",
        .source =
        \\(((|a| {
        \\    a_loc = a * 2.I64
        \\    |b| {
        \\        b_loc = a_loc + b
        \\        |c| b_loc + c
        \\    }
        \\})(100.I64))(20.I64))(3.I64)
        ,
        .expected = .{ .inspect_str = "223" },
    },
    .{ .name = "inspect: typed identity closure on string", .source = "(|s| s)(\"Test\")", .expected = .{ .inspect_str = "\"Test\"" } },

    // CaptureId regression suite: exercises the canonical-CaptureId join through
    // nested/curried closures, closures held before application, capture-set
    // reshaping, and captures that also appear inside call arguments.
    // Repro for issue 9897 ("function reference capture count differs from its
    // target"): nested callbacks capture an outer destructured binding while
    // forwarding through another function reference.
    .{
        .name = "capture-id: issue 9897 nested callback captures destructured binding",
        .source =
        \\{
        \\    c = |x, f| f(x)
        \\    (a, _) = ({}, 0)
        \\    c(0, |x| c(x, |_| a))
        \\}
        ,
        .expected = .{ .inspect_str = "{}" },
    },
    // The middle closure is bound and applied separately, so each fn_ref must
    // carry exactly its target's capture slots.
    .{
        .name = "capture-id: nested closure held before application",
        .source =
        \\{
        \\    make = |x| |y| |z| x + y + z
        \\    partial = make(1.I64)(2.I64)
        \\    partial(3.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    // Repro for PR 9874's order-sensitivity: a 5-deep curried chain, each level
    // adding a capture, so the capture spans must stay canonically ordered.
    .{
        .name = "capture-id: five-deep curried capture chain",
        .source = "(|a| |b| |c| |d| |e| a + b + c + d + e)(1.I64)(2.I64)(3.I64)(4.I64)(5.I64)",
        .expected = .{ .inspect_str = "15" },
    },
    // A captured variable that also appears inside a record argument to the
    // closure it is passed to, reshaped by specialization.
    .{
        .name = "capture-id: captured var also used in a record argument",
        .source =
        \\{
        \\    outer = |seed| {
        \\        inner = |next, arg| seed + next.n + arg
        \\        inner({ n: seed }, seed * 10.I64)
        \\    }
        \\    outer(2.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "24" },
    },
    // Two sibling closures capture the same outer binding via a shared helper,
    // so the capture-set fixpoint must give both the same canonical CaptureId.
    .{
        .name = "capture-id: sibling closures share one captured binding",
        .source =
        \\{
        \\    base = 10.I64
        \\    pick = |flag| {
        \\        add = |n| base + n
        \\        sub = |n| base - n
        \\        if flag add(3.I64) else sub(3.I64)
        \\    }
        \\    pick(True) + pick(False)
        \\}
        ,
        .expected = .{ .inspect_str = "20" },
    },

    // Untyped closures, HOFs, and recursion
    .{
        .name = "inspect: closure capturing one local variable",
        .source =
        \\{
        \\    y = 10
        \\    f = |x| x + y
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure capturing two local variables",
        .source =
        \\{
        \\    a = 3
        \\    b = 7
        \\    f = |x| x + a + b
        \\    f(10)
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "inspect: closure capturing a string",
        .source =
        \\{
        \\    greeting = "Hello"
        \\    f = |name| Str.concat(greeting, name)
        \\    f(" World")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World\"" },
    },
    .{
        .name = "inspect: closure capturing multiple strings",
        .source =
        \\{
        \\    prefix = "Hello"
        \\    suffix = "!"
        \\    f = |name| Str.concat(Str.concat(prefix, name), suffix)
        \\    f(" World")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World!\"" },
    },
    .{
        .name = "inspect: function returning closure",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    add5(10)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure factory reused twice",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    a = add5(10)
        \\    b = add5(20)
        \\    a + b
        \\}
        ,
        .expected = .{ .inspect_str = "40.0" },
    },
    .{
        .name = "inspect: two closures from same factory",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add3 = make_adder(3)
        \\    add7 = make_adder(7)
        \\    add3(10) + add7(10)
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: function returning string closure",
        .source =
        \\{
        \\    make_greeter = |greeting| |name| Str.concat(greeting, name)
        \\    greet = make_greeter("Hi ")
        \\    greet("Alice")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hi Alice\"" },
    },
    .{
        .name = "inspect: direct function with dec capture-shaped record arg",
        .source =
        \\{
        \\    apply = |x, captures| x + captures.n
        \\    apply(10, { n: 5 })
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: single-arg function with dec record field",
        .source =
        \\{
        \\    apply = |captures| captures.n + 10
        \\    apply({ n: 5 })
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: if expression joining dec result",
        .source =
        \\{
        \\    apply = |captures| captures.n + 10
        \\    if True apply({ n: 5 }) else 0
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: function returning tag union with dec payload",
        .source =
        \\{
        \\    make : Dec -> [Ok({ n : Dec })]
        \\    make = |n| Ok({ n: n })
        \\    make(5)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok({ n: 5.0 })" },
    },
    .{
        .name = "inspect: two-level closure factory",
        .source =
        \\{
        \\    make_op = |a| |b| |x| x + a + b
        \\    add_3_and_4 = make_op(3)(4)
        \\    add_3_and_4(10)
        \\}
        ,
        .expected = .{ .inspect_str = "17.0" },
    },
    .{
        .name = "inspect: passing closure to higher-order function",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    y = 10
        \\    apply(|x| x + y, 5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: higher-order function with two closures",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r1 + r2
        \\}
        ,
        .expected = .{ .inspect_str = "40.0" },
    },
    .{
        .name = "inspect: higher-order function returns first closure result",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    _r2 = apply(|x| x + b, 5)
        \\    r1
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: higher-order function returns second closure result",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    _r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r2
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: higher-order function applying twice",
        .source =
        \\{
        \\    apply_twice = |f, x| f(f(x))
        \\    y = 3
        \\    apply_twice(|x| x + y, 10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: higher-order function returning string",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    prefix = "Hello "
        \\    apply(|name| Str.concat(prefix, name), "World")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World\"" },
    },
    .{
        .name = "inspect: polymorphic identity over closure result",
        .source =
        \\{
        \\    id = |x| x
        \\    y = 10
        \\    f = |x| x + y
        \\    id(f(5))
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: polymorphic closure function with int and string",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    n = 10
        \\    prefix = "Hi "
        \\    num_result = apply(|x| x + n, 5)
        \\    str_result = apply(|s| Str.concat(prefix, s), "Bob")
        \\    if (num_result > 0) str_result else ""
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hi Bob\"" },
    },
    .{
        .name = "inspect: closure forwarding to captured closure",
        .source =
        \\{
        \\    y = 5
        \\    inner = |x| x + y
        \\    outer = |x| inner(x)
        \\    outer(10)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure capturing another closure",
        .source =
        \\{
        \\    y = 5
        \\    inner = |x| x + y
        \\    outer = |x| inner(x) * 2
        \\    outer(10)
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: closure capturing factory-produced closure",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    double_add5 = |x| add5(x) * 2
        \\    double_add5(10)
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: if chooses first closure",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    f = if (True) |x| x + a else |x| x + b
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: if chooses second closure",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    f = if (False) |x| x + a else |x| x + b
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: closures with different capture counts",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    c = 30
        \\    f = if (True) |x| x + a else |x| x + b + c
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure in record field",
        .source =
        \\{
        \\    y = 10
        \\    rec = { f: |x| x + y }
        \\    f = rec.f
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: two closures in record",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_a = rec.add_a
        \\    add_b = rec.add_b
        \\    add_a(5) + add_b(5)
        \\}
        ,
        .expected = .{ .inspect_str = "40.0" },
    },
    .{
        .name = "inspect: record field closure add_a preserves capture",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_a = rec.add_a
        \\    add_a(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: parenthesized record field closure",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    (rec.add_b)(5)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: record field closure add_b preserves capture",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_b = rec.add_b
        \\    add_b(5)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: opaque function field lookup issue 9262",
        .source_kind = .module,
        .source =
        \\W(a) := { f : {} -> [V(a)] }.{
        \\    run : W(a) -> [V(a)]
        \\    run = |w| (w.f)({})
        \\
        \\    mk : a -> W(a)
        \\    mk = |val| { f: |_| V(val) }
        \\}
        \\
        \\main = W.run(W.mk("x")) == V("x")
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: compose two functions",
        .source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add1 = |x| x + 1
        \\    double_then_add1 = compose(add1, double)
        \\    double_then_add1(5)
        \\}
        ,
        .expected = .{ .inspect_str = "11.0" },
    },
    .{
        .name = "inspect: compose with captures",
        .source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    a = 3
        \\    b = 7
        \\    add_a = |x| x + a
        \\    add_b = |x| x + b
        \\    add_both = compose(add_a, add_b)
        \\    add_both(10)
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "inspect: pipe with closure",
        .source =
        \\{
        \\    pipe = |x, f| f(x)
        \\    y = 10
        \\    pipe(5, |x| x + y)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: recursive lambda factorial",
        .source =
        \\{
        \\    factorial = |n| if (n <= 1.I64) 1.I64 else n * factorial(n - 1.I64)
        \\    factorial(5.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "120" },
    },
    .{
        .name = "inspect: recursive closure factorial untyped",
        .source =
        \\{
        \\    factorial = |n| if (n <= 1) 1 else n * factorial(n - 1)
        \\    factorial(5)
        \\}
        ,
        .expected = .{ .inspect_str = "120.0" },
    },
    .{
        // Mutual recursion between local definitions is not supported: local
        // definitions are sequential (self-reference and backward references
        // only). This must be reported as an error rather than evaluated.
        .name = "problem: mutual recursion in local lambdas",
        .source =
        \\{
        \\    is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\    is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\    is_even(6.I64)
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "problem: mutual recursion in untyped closures",
        .source =
        \\{
        \\    is_even = |n| if (n == 0) True else is_odd(n - 1)
        \\    is_odd = |n| if (n == 0) False else is_even(n - 1)
        \\    if (is_even(4)) 1 else 0
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        // Coverage migrated from the (now-disallowed) local mutual-recursion
        // cases above: mutual recursion is supported at the top level and must
        // still evaluate to the same results.
        .name = "inspect: mutual recursion at top level (typed)",
        .source_kind = .module,
        .source =
        \\is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\main = is_even(6.I64)
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: mutual recursion at top level (untyped)",
        .source_kind = .module,
        .source =
        \\is_even = |n| if (n == 0) True else is_odd(n - 1)
        \\is_odd = |n| if (n == 0) False else is_even(n - 1)
        \\main = if (is_even(4)) 1 else 0
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: triple nested closure factory",
        .source =
        \\{
        \\    level1 = |a| |b| |c| |x| x + a + b + c
        \\    level2 = level1(1)
        \\    level3 = level2(2)
        \\    level4 = level3(3)
        \\    level4(10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: closure capturing another closure two levels",
        .source =
        \\{
        \\    a = 1
        \\    f = |x| x + a
        \\    b = 2
        \\    g = |x| f(x) + b
        \\    g(10)
        \\}
        ,
        .expected = .{ .inspect_str = "13.0" },
    },
    .{
        .name = "inspect: closure capturing another closure three levels",
        .source =
        \\{
        \\    a = 1
        \\    f = |x| x + a
        \\    b = 2
        \\    g = |x| f(x) + b
        \\    c = 3
        \\    h = |x| g(x) + c
        \\    h(10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: hof returns closure capturing argument closure",
        .source =
        \\{
        \\    make_doubler = |f| |x| f(f(x))
        \\    add3 = |x| x + 3
        \\    double_add3 = make_doubler(add3)
        \\    double_add3(10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: hof returns closure capturing closure with captures",
        .source =
        \\{
        \\    n = 5
        \\    add_n = |x| x + n
        \\    make_doubler = |f| |x| f(f(x))
        \\    double_add_n = make_doubler(add_n)
        \\    double_add_n(10)
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "inspect: chained closure factories",
        .source =
        \\{
        \\    step1 = |a| |b| |c| a + b + c
        \\    step2 = step1(100)
        \\    step3 = step2(20)
        \\    step3(3)
        \\}
        ,
        .expected = .{ .inspect_str = "123.0" },
    },
    .{
        .name = "inspect: regression issue 8727 captured closure return",
        .source =
        \\{
        \\    make_adder = |n| |x| n + x
        \\    add_ten = make_adder(10)
        \\    add_ten(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: polymorphic hof with closures capturing different types",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    offset = 100
        \\    prefix = "Result: "
        \\    num = apply(|x| x + offset, 23)
        \\    if (num > 0) apply(|s| Str.concat(prefix, s), "yes") else "no"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Result: yes\"" },
    },
    .{
        .name = "inspect: closure capture duplication integer specialization",
        .source =
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_num = make_getter(42)
        \\    get_num(0)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: closure capture duplication string specialization",
        .source =
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_str = make_getter("hello")
        \\    get_str(0)
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: closure over bool used in conditional",
        .source =
        \\{
        \\    flag = True
        \\    choose = |a, b| if (flag) a else b
        \\    choose(42, 0)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: deeply nested blocks add captures",
        .source =
        \\{
        \\    a = 1
        \\    r1 = {
        \\        b = 2
        \\        r2 = {
        \\            c = 3
        \\            f = |x| x + a + b + c
        \\            f(10)
        \\        }
        \\        r2
        \\    }
        \\    r1
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: same capture used by independent closures",
        .source =
        \\{
        \\    shared = 10
        \\    f = |x| x + shared
        \\    g = |x| x * shared
        \\    f(5) + g(3)
        \\}
        ,
        .expected = .{ .inspect_str = "45.0" },
    },
    .{
        .name = "inspect: closure returning captured string composition",
        .source =
        \\{
        \\    make_greeter = |greeting|
        \\        |name|
        \\            Str.concat(Str.concat(greeting, ", "), name)
        \\    hello = make_greeter("Hello")
        \\    hi = make_greeter("Hi")
        \\    r1 = hello("Alice")
        \\    r2 = hi("Bob")
        \\    Str.concat(Str.concat(r1, " and "), r2)
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello, Alice and Hi, Bob\"" },
    },
    .{
        .name = "inspect: same closure applied to multiple arguments",
        .source =
        \\{
        \\    base = 100
        \\    f = |x| x + base
        \\    a = f(1)
        \\    b = f(2)
        \\    c = f(3)
        \\    a + b + c
        \\}
        ,
        .expected = .{ .inspect_str = "306.0" },
    },
    .{
        .name = "inspect: immediately invoked closure with capture",
        .source =
        \\{
        \\    y = 42
        \\    (|x| x + y)(8)
        \\}
        ,
        .expected = .{ .inspect_str = "50.0" },
    },
    .{
        .name = "inspect: closure ignores argument and uses capture",
        .source =
        \\{
        \\    val = 99
        \\    f = |_| val
        \\    f(0)
        \\}
        ,
        .expected = .{ .inspect_str = "99.0" },
    },
    .{
        .name = "inspect: closure ignores capture and uses argument",
        .source =
        \\{
        \\    _unused = 999
        \\    f = |x| x + 1
        \\    f(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: monomorphic str identity",
        .source =
        \\{
        \\    identity : Str -> Str
        \\    identity = |val| val
        \\    identity("Hello")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: monomorphic dec identity",
        .source =
        \\{
        \\    identity : Dec -> Dec
        \\    identity = |val| val
        \\    identity(5)
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: monomorphic str identity if join",
        .source =
        \\{
        \\    str_id : Str -> Str
        \\    str_id = |val| val
        \\    num = 5
        \\    str = str_id("Hello")
        \\    if (num > 0) str else ""
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: multi-use closure with short captured string",
        .source =
        \\{
        \\    s = "short"
        \\    f = |_x| s
        \\    _a = f(0)
        \\    f(0)
        \\}
        ,
        .expected = .{ .inspect_str = "\"short\"" },
    },
    .{
        .name = "inspect: multi-use closure with heap captured string",
        .source =
        \\{
        \\    s = "This string is definitely longer than twenty three bytes"
        \\    f = |_x| s
        \\    _a = f(0)
        \\    f(0)
        \\}
        ,
        .expected = .{ .inspect_str = "\"This string is definitely longer than twenty three bytes\"" },
    },
    .{
        .name = "inspect: large record chained higher order calls w",
        .source =
        \\{
        \\    apply2 = |a, b, f| f(a, b)
        \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
        \\    result = apply2("w_val", step1.y, |w, y| { w, y })
        \\    result.w
        \\}
        ,
        .expected = .{ .inspect_str = "\"w_val\"" },
    },
    .{
        .name = "inspect: large record chained higher order calls y",
        .source =
        \\{
        \\    apply2 = |a, b, f| f(a, b)
        \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
        \\    result = apply2("w_val", step1.y, |w, y| { w, y })
        \\    result.y
        \\}
        ,
        .expected = .{ .inspect_str = "\"y_val\"" },
    },

    // Loops
    .{
        .name = "inspect: for loop sums list",
        .source =
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: for loop inside lambda body",
        .source =
        \\{
        \\    sum = |xs| {
        \\        var $sum = 0.I64
        \\        for item in xs {
        \\            $sum = $sum + item
        \\        }
        \\        $sum
        \\    }
        \\    sum([1.I64, 2.I64, 3.I64, 4.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "10" },
    },
    .{
        .name = "inspect: two-arg proc list iterator loop returns full length",
        .source =
        \\{
        \\    count = |items, _ignored| {
        \\        var $total = 0.U64
        \\        for _item in items {
        \\            $total = $total + 1
        \\        }
        \\        $total
        \\    }
        \\    count([10.I64, 20.I64, 30.I64, 40.I64, 50.I64], 123.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "inspect: for loop early return",
        .source =
        \\{
        \\    f = |list| {
        \\        for _item in list {
        \\            if True { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: for loop closure early return",
        .source =
        \\{
        \\    f = |list, pred| {
        \\        for item in list {
        \\            if pred(item) { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64], |_x| True)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: lambda list param calling List.len",
        .source =
        \\{
        \\    get_len = |l| List.len(l)
        \\    get_len([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda list param calling List.append",
        .source =
        \\{
        \\    add_one = |l| List.len(List.append(l, 99.I64))
        \\    add_one([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "inspect: lambda list param and var declaration",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $acc = [0.I64]
        \\        List.len($acc)
        \\    }
        \\    test_fn([1.I64, 2.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: lambda list param and list literal creation",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $acc = [0.I64]
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64, 20.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: lambda list param var and for loop",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0.I64
        \\        for e in l {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([10.I64, 20.I64, 30.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: lambda list param var and List.append no for loop",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $acc = [0.I64]
        \\        $acc = List.append($acc, 42.I64)
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64, 20.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: minimal lambda list param and for loop",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0.I64
        \\        for e in l {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1.I64, 2.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda list param for loop with allocation inside loop",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0.I64
        \\        for e in l {
        \\            $total = match List.last([e]) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1.I64, 2.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda for loop over internal list",
        .source =
        \\{
        \\    test_fn = |_x| {
        \\        var $total = 0.I64
        \\        for e in [1.I64, 2.I64, 3.I64] {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn(42.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "inspect: lambda list param internal loop allocation",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $total = 0.I64
        \\        for e in [1.I64, 2.I64] {
        \\            $total = match List.last([e]) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    test_fn([10.I64, 20.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda list param for loop empty iteration",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([])
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: lambda list param for loop append single iteration",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: lambda list param var for loop and List.append",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64, 20.I64, 30.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "inspect: closure in for loop with List.last regression",
        .source =
        \\{
        \\    sum_with_last = |l| {
        \\        var $total = 0.I64
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\            $total = match List.last($acc) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    sum_with_last([10.I64, 20.I64, 30.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: closure capturing for loop element with equality",
        .source =
        \\{
        \\    my_any = |lst, pred| {
        \\        for e in lst {
        \\            if pred(e) { return True }
        \\        }
        \\        False
        \\    }
        \\    check = |list| {
        \\        var $built = []
        \\        for item in list {
        \\            _x = my_any($built, |x| x == item)
        \\            $built = $built.append(item)
        \\        }
        \\        $built.len()
        \\    }
        \\    check([1, 2])
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },

    // Polymorphic source-level cases
    .{
        .name = "inspect: polymorphic identity returns string",
        .source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: direct polymorphic function usage",
        .source =
        \\{
        \\    id = |x| x
        \\    num1 = id(10)
        \\    str1 = id("Test")
        \\    num2 = id(20)
        \\    if (num1 == 10)
        \\        if (num2 == 20)
        \\            str1
        \\        else
        \\            "Failed2"
        \\    else
        \\        "Failed1"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Test\"" },
    },
    .{
        .name = "inspect: polymorphic return function then call int",
        .source = "(|_| (|x| x))(0)(42)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic return function then call string",
        .source = "(|_| (|x| x))(0)(\"hi\")",
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "inspect: polymorphic captured id applied to int",
        .source = "((|id| (|x| id(x)))(|y| y))(41)",
        .expected = .{ .inspect_str = "41.0" },
    },
    .{
        .name = "inspect: polymorphic captured id applied to string",
        .source = "((|id| (|x| id(x)))(|y| y))(\"ok\")",
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "inspect: polymorphic higher-order apply then call",
        .source = "((|f| (|x| f(x)))(|n| n + 1))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic higher-order apply twice",
        .source = "((|f| (|x| f(f(x))))(|n| n + 1))(40)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic pass constructed closure and apply",
        .source = "(|g| g(41))((|f| (|x| f(x)))(|y| y))",
        .expected = .{ .inspect_str = "41.0" },
    },
    .{
        .name = "inspect: polymorphic construct then pass then call",
        .source = "((|make| (|z| (make(|n| n + 1))(z)))(|f| (|x| f(x))))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic compose identity with plus one",
        .source = "(((|f| (|g| (|x| f(g(x)))))(|n| n + 1))(|y| y))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic return function using captured increment",
        .source = "(((|n| (|id| (|x| id(x + n))))(1))(|y| y))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: recursive countdown",
        .source =
        \\{
        \\    rec = |n| if (n == 0) 0 else rec(n - 1) + 1
        \\    rec(2)
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
    .{
        .name = "inspect: else if chain selects middle branch",
        .source =
        \\{
        \\    n = 1
        \\    if (n == 0)
        \\        "zero"
        \\    else if (n == 1)
        \\        "one"
        \\    else
        \\        "other"
        \\}
        ,
        .expected = .{ .inspect_str = "\"one\"" },
    },
    .{
        .name = "inspect: mutable variable reassign",
        .source =
        \\{
        \\    var $x = 1
        \\    $x = $x + 1
        \\    $x
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
    .{
        .name = "inspect: logical or short circuits",
        .source =
        \\if ((1 == 1) or { crash "nope" })
        \\    "ok"
        \\else
        \\    "bad"
        ,
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "inspect: logical and short circuits",
        .source =
        \\if ((1 == 0) and { crash "nope" })
        \\    "bad"
        \\else
        \\    "ok"
        ,
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "inspect: recursive fibonacci",
        .source =
        \\{
        \\    fib = |n| if (n == 0) 0 else if (n == 1) 1 else fib(n - 1) + fib(n - 2)
        \\    fib(5)
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: tag union one arg ok",
        .source = "Ok(42.0)",
        .expected = .{ .inspect_str = "Ok(42.0)" },
    },
    .{
        .name = "inspect: tag union multi arg point",
        .source = "Point(1.0, 2.0)",
        .expected = .{ .inspect_str = "Point(1.0, 2.0)" },
    },
    .{
        .name = "inspect: tag union nested in tuple regression",
        .source = "Ok((Name(\"hello\"), 5))",
        .expected = .{ .inspect_str = "Ok((Name(\"hello\"), 5.0))" },
    },
    .{
        .name = "inspect: multiple polymorphic instantiations",
        .source =
        \\{
        \\    id = |x| x
        \\
        \\    num1 = id(42)
        \\    str1 = id("Hello")
        \\    num2 = id(100)
        \\
        \\    if (num1 == 42)
        \\        if (num2 == 100)
        \\            str1
        \\        else
        \\            "Failed2"
        \\    else
        \\        "Failed1"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: early return in closure passed to List.map",
        .source =
        \\{
        \\    result = [Ok(1), Err({})].map(|x| Ok(x?))
        \\    List.len(result)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: early return in closure passed to List.fold",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    result = List.fold([Ok(1), Err({})], [], |acc, x| List.append(acc, compute(x)))
        \\    List.len(result)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: lambda wrapping try suffix result in Ok",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "inspect: lambda with tag match called directly",
        .source =
        \\{
        \\    f = |child|
        \\        match child {
        \\            Aaa(_, _) => 10.I64
        \\            Bbb(_) => 1.I64
        \\        }
        \\    f(Bbb(42.I64))
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: fold with simple addition lambda",
        .source =
        \\{
        \\    items = [1.I64, 2.I64, 3.I64]
        \\    List.fold(items, 0.I64, |acc, x| acc + x)
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "inspect: polymorphic tag payload substitution extract payload",
        .source =
        \\{
        \\    second : [Left(a), Right(b)], b -> b
        \\    second = |either, fallback| match either {
        \\        Left(_) => fallback
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(I64), Right(I64)]
        \\    input = Right(42.I64)
        \\    second(input, 0.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "inspect: polymorphic tag payload substitution multiple type vars",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)], e -> e
        \\    get_err = |result, fallback| match result {
        \\        Ok(_) => fallback
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Err("hello")
        \\    get_err(val, "")
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: attached methods on transparent tag union alias issue 8637",
        .source_kind = .module,
        .source =
        \\Iter(s) :: [It(s)].{
        \\    identity : Iter(s) -> Iter(s)
        \\    identity = |It(s_)| It(s_)
        \\}
        \\
        \\count : Iter({})
        \\count = It({})
        \\
        \\main = count.identity() == It({})
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "problem: polymorphic erroneous match branch",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| match result {
        \\        Ok(_) => ""
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Ok(42)
        \\    get_err(val)
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "problem: polymorphic erroneous if else branch",
        .source =
        \\{
        \\    get_val : Bool, e -> e
        \\    get_val = |flag, val| if (flag) "" else val
        \\
        \\    get_val(Bool.true, 42)
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "problem: polymorphic erroneous match in block",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| {
        \\        unused = 0
        \\        match result {
        \\            Ok(_) => ""
        \\            Err(e) => e
        \\        }
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Ok(42)
        \\    get_err(val)
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "inspect: polymorphic tag payload substitution wrap and unwrap",
        .source =
        \\{
        \\    second : [Left(a), Right(b)], b -> b
        \\    second = |either, fallback| match either {
        \\        Left(_) => fallback
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(Dec), Right(Dec)]
        \\    input = Right(42.0)
        \\    second(input, 0.0)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic tag payload layout in match expression",
        .source =
        \\{
        \\    transform_err : [Ok({}), Err(a)], (a -> b) -> [Ok({}), Err(b)]
        \\    transform_err = |try_val, transform| match try_val {
        \\        Err(a) => Err(transform(a))
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    err : [Ok({}), Err(I32)]
        \\    err = Err(42.I32)
        \\
        \\    result = transform_err(err, |_e| "hello")
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: polymorphic tag transform with match",
        .source =
        \\{
        \\    transform_err = |try_val| match try_val {
        \\        Err(_) => Err("hello")
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    err : [Ok({}), Err(I32)]
        \\    err = Err(42.I32)
        \\
        \\    result = transform_err(err)
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: recursive function with record stack restoration",
        .source =
        \\{
        \\    f = |n|
        \\        if n <= 0
        \\            0
        \\        else
        \\            { a: n, b: n * 2, c: n * 3, d: n * 4 }.a + f(n - 1)
        \\    f(1000)
        \\}
        ,
        .expected = .{ .inspect_str = "500500.0" },
    },
    .{
        .name = "inspect: polymorphic sum in block called with U64",
        .source =
        \\{
        \\    sum = |a, b| {
        \\        tmp = a
        \\        tmp + b
        \\    }
        \\    sum(100.U64, 160.U64)
        \\}
        ,
        .expected = .{ .inspect_str = "260" },
    },
    .{
        .name = "inspect: polymorphic predicate with comparison in block",
        .source =
        \\{
        \\    at_least = |threshold, value| {
        \\        current = value
        \\        current >= threshold
        \\    }
        \\    at_least(3, 5)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: polymorphic comparison lambda called directly",
        .source =
        \\{
        \\    greater_than = |lhs, rhs| lhs > rhs
        \\    greater_than(5, 3)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: polymorphic comparison lambda passed to List.any",
        .source =
        \\{
        \\    greater_than = |lhs, rhs| lhs > rhs
        \\    List.any([1, 2, 3, 4], |x| greater_than(x, 3))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: polymorphic function called with two list types",
        .source =
        \\{
        \\    my_len = |list| list.len()
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    b : List(Str)
        \\    b = ["x", "y"]
        \\    my_len(a) + my_len(b)
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "inspect: polymorphic function single call I64",
        .source =
        \\{
        \\    contains = |list, item| list.contains(item)
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    r = contains(a, 2)
        \\    if r { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: polymorphic function single call Str",
        .source =
        \\{
        \\    contains = |list, item| list.contains(item)
        \\    b : List(Str)
        \\    b = ["x", "y"]
        \\    r = contains(b, "x")
        \\    if r { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: polymorphic function with List.contains called with two types",
        .source =
        \\{
        \\    contains = |list, item| list.contains(item)
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    b : List(Str)
        \\    b = ["x", "y"]
        \\    r1 = contains(a, 2)
        \\    r2 = contains(b, "x")
        \\    if r1 and r2 { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: polymorphic function with List.contains called with multiple types",
        .source =
        \\{
        \\    dedup = |list| {
        \\        var $out = []
        \\        for item in list {
        \\            if !$out.contains(item) {
        \\                $out = $out.append(item)
        \\            }
        \\        }
        \\        $out
        \\    }
        \\    nums : List(I64)
        \\    nums = [1, 2, 3, 2, 1]
        \\    u1 = dedup(nums)
        \\    strs : List(Str)
        \\    strs = ["a", "b", "a"]
        \\    u2 = dedup(strs)
        \\    u1.len() + u2.len()
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "inspect: nested List.any true path with captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    List.any(["a"], |item| out.contains(item))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: nested List.any false path with captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    List.any(["b"], |item| out.contains(item))
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "inspect: direct List.contains captured Str control",
        .source =
        \\{
        \\    out = ["a"]
        \\    out.contains("a")
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: forwarding tag union with Str payload through proc call",
        .source =
        \\{
        \\    consume = |value| value == Ok({ x: "x" })
        \\    forward = |value| consume(value)
        \\    value = Ok({ x: "x" })
        \\    forward(value)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: Str.inspect through polymorphic wrapper",
        .source =
        \\{
        \\    show = |x| Str.inspect(x)
        \\    show(42.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "\"42\"" },
    },
    .{
        .name = "inspect: polymorphic additional specialization via List.append",
        .source =
        \\{
        \\    append_one = |acc, x| List.append(acc, x)
        \\    clone_via_fold = |xs| xs.fold(List.with_capacity(1), append_one)
        \\    _first_len = clone_via_fold([1.I64, 2.I64]).len()
        \\    clone_via_fold([[1.I64, 2.I64], [3.I64, 4.I64]]).len()
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },

    // Typed arithmetic matrix from origin/main
    .{ .name = "inspect: U8 plus", .source = "{ a : U8\n    a = 200\n    b : U8\n    b = 50\n    a + b\n}", .expected = .{ .inspect_str = "250" } },
    .{ .name = "inspect: U8 minus", .source = "{ a : U8\n    a = 200\n    b : U8\n    b = 50\n    a - b\n}", .expected = .{ .inspect_str = "150" } },
    .{ .name = "inspect: U8 times", .source = "{ a : U8\n    a = 15\n    b : U8\n    b = 17\n    a * b\n}", .expected = .{ .inspect_str = "255" } },
    .{ .name = "inspect: U8 div", .source = "{ a : U8\n    a = 240\n    b : U8\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "120" } },
    .{ .name = "inspect: U8 rem", .source = "{ a : U8\n    a = 200\n    b : U8\n    b = 13\n    a % b\n}", .expected = .{ .inspect_str = "5" } },

    .{ .name = "inspect: U16 plus", .source = "{ a : U16\n    a = 40000\n    b : U16\n    b = 20000\n    a + b\n}", .expected = .{ .inspect_str = "60000" } },
    .{ .name = "inspect: U16 minus", .source = "{ a : U16\n    a = 50000\n    b : U16\n    b = 10000\n    a - b\n}", .expected = .{ .inspect_str = "40000" } },
    .{ .name = "inspect: U16 times", .source = "{ a : U16\n    a = 256\n    b : U16\n    b = 255\n    a * b\n}", .expected = .{ .inspect_str = "65280" } },
    .{ .name = "inspect: U16 div", .source = "{ a : U16\n    a = 60000\n    b : U16\n    b = 3\n    a // b\n}", .expected = .{ .inspect_str = "20000" } },
    .{ .name = "inspect: U16 rem", .source = "{ a : U16\n    a = 50000\n    b : U16\n    b = 128\n    a % b\n}", .expected = .{ .inspect_str = "80" } },

    .{ .name = "inspect: U32 plus", .source = "{ a : U32\n    a = 3000000000\n    b : U32\n    b = 1000000000\n    a + b\n}", .expected = .{ .inspect_str = "4000000000" } },
    .{ .name = "inspect: U32 minus", .source = "{ a : U32\n    a = 3000000000\n    b : U32\n    b = 1000000000\n    a - b\n}", .expected = .{ .inspect_str = "2000000000" } },
    .{ .name = "inspect: U32 times", .source = "{ a : U32\n    a = 65536\n    b : U32\n    b = 65535\n    a * b\n}", .expected = .{ .inspect_str = "4294901760" } },
    .{ .name = "inspect: U32 div", .source = "{ a : U32\n    a = 4000000000\n    b : U32\n    b = 1000\n    a // b\n}", .expected = .{ .inspect_str = "4000000" } },
    .{ .name = "inspect: U32 rem", .source = "{ a : U32\n    a = 3000000000\n    b : U32\n    b = 128\n    a % b\n}", .expected = .{ .inspect_str = "0" } },

    .{ .name = "inspect: U64 plus", .source = "{ a : U64\n    a = 10000000000000000000\n    b : U64\n    b = 5000000000000000000\n    a + b\n}", .expected = .{ .inspect_str = "15000000000000000000" } },
    .{ .name = "inspect: U64 minus", .source = "{ a : U64\n    a = 15000000000000000000\n    b : U64\n    b = 5000000000000000000\n    a - b\n}", .expected = .{ .inspect_str = "10000000000000000000" } },
    .{ .name = "inspect: U64 times", .source = "{ a : U64\n    a = 4294967296\n    b : U64\n    b = 4294967295\n    a * b\n}", .expected = .{ .inspect_str = "18446744069414584320" } },
    .{ .name = "inspect: U64 div", .source = "{ a : U64\n    a = 15000000000000000000\n    b : U64\n    b = 1000000\n    a // b\n}", .expected = .{ .inspect_str = "15000000000000" } },
    .{ .name = "inspect: U64 rem", .source = "{ a : U64\n    a = 10000000000000000000\n    b : U64\n    b = 256\n    a % b\n}", .expected = .{ .inspect_str = "0" } },

    .{ .name = "inspect: U128 plus", .source = "{ a : U128\n    a = 100000000000000000000000000000\n    b : U128\n    b = 50000000000000000000000000000\n    a + b\n}", .expected = .{ .inspect_str = "150000000000000000000000000000" } },
    .{ .name = "inspect: U128 minus", .source = "{ a : U128\n    a = 150000000000000000000000000000\n    b : U128\n    b = 50000000000000000000000000000\n    a - b\n}", .expected = .{ .inspect_str = "100000000000000000000000000000" } },
    .{ .name = "inspect: U128 times", .source = "{ a : U128\n    a = 13043817825332782212\n    b : U128\n    b = 13043817825332782212\n    a * b\n}", .expected = .{ .inspect_str = "170141183460469231722567801800623612944" } },
    .{ .name = "inspect: U128 div", .source = "{ a : U128\n    a = 100000000000000000000000000000\n    b : U128\n    b = 10000000000000000\n    a // b\n}", .expected = .{ .inspect_str = "10000000000000" } },
    .{ .name = "inspect: U128 rem", .source = "{ a : U128\n    a = 100000000000000000000000000000\n    b : U128\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "10" } },

    .{ .name = "inspect: I8 negate", .source = "{ a : I8\n    a = -127\n    -a\n}", .expected = .{ .inspect_str = "127" } },
    .{ .name = "inspect: I8 plus", .source = "{ a : I8\n    a = -100\n    b : I8\n    b = -20\n    a + b\n}", .expected = .{ .inspect_str = "-120" } },
    .{ .name = "inspect: I8 minus", .source = "{ a : I8\n    a = -50\n    b : I8\n    b = 70\n    a - b\n}", .expected = .{ .inspect_str = "-120" } },
    .{ .name = "inspect: I8 times", .source = "{ a : I8\n    a = -16\n    b : I8\n    b = 8\n    a * b\n}", .expected = .{ .inspect_str = "-128" } },
    .{ .name = "inspect: I8 div", .source = "{ a : I8\n    a = -128\n    b : I8\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-64" } },
    .{ .name = "inspect: I8 rem", .source = "{ a : I8\n    a = -128\n    b : I8\n    b = 7\n    a % b\n}", .expected = .{ .inspect_str = "-2" } },

    .{ .name = "inspect: I16 negate", .source = "{ a : I16\n    a = -32767\n    -a\n}", .expected = .{ .inspect_str = "32767" } },
    .{ .name = "inspect: I16 plus", .source = "{ a : I16\n    a = -20000\n    b : I16\n    b = -10000\n    a + b\n}", .expected = .{ .inspect_str = "-30000" } },
    .{ .name = "inspect: I16 minus", .source = "{ a : I16\n    a = -10000\n    b : I16\n    b = 20000\n    a - b\n}", .expected = .{ .inspect_str = "-30000" } },
    .{ .name = "inspect: I16 times", .source = "{ a : I16\n    a = -256\n    b : I16\n    b = 128\n    a * b\n}", .expected = .{ .inspect_str = "-32768" } },
    .{ .name = "inspect: I16 div", .source = "{ a : I16\n    a = -32768\n    b : I16\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-16384" } },
    .{ .name = "inspect: I16 rem", .source = "{ a : I16\n    a = -32768\n    b : I16\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-98" } },

    .{ .name = "inspect: I32 negate", .source = "{ a : I32\n    a = -2147483647\n    -a\n}", .expected = .{ .inspect_str = "2147483647" } },
    .{ .name = "inspect: I32 plus", .source = "{ a : I32\n    a = -1000000000\n    b : I32\n    b = -500000000\n    a + b\n}", .expected = .{ .inspect_str = "-1500000000" } },
    .{ .name = "inspect: I32 minus", .source = "{ a : I32\n    a = -1000000000\n    b : I32\n    b = 500000000\n    a - b\n}", .expected = .{ .inspect_str = "-1500000000" } },
    .{ .name = "inspect: I32 times", .source = "{ a : I32\n    a = -65536\n    b : I32\n    b = 32768\n    a * b\n}", .expected = .{ .inspect_str = "-2147483648" } },
    .{ .name = "inspect: I32 div", .source = "{ a : I32\n    a = -2147483648\n    b : I32\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-1073741824" } },
    .{ .name = "inspect: I32 rem", .source = "{ a : I32\n    a = -2147483648\n    b : I32\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-2" } },

    .{ .name = "inspect: I64 negate", .source = "{ a : I64\n    a = -9223372036854775807\n    -a\n}", .expected = .{ .inspect_str = "9223372036854775807" } },
    .{ .name = "inspect: I64 plus", .source = "{ a : I64\n    a = -5000000000000\n    b : I64\n    b = -3000000000000\n    a + b\n}", .expected = .{ .inspect_str = "-8000000000000" } },
    .{ .name = "inspect: I64 minus", .source = "{ a : I64\n    a = -5000000000000\n    b : I64\n    b = 3000000000000\n    a - b\n}", .expected = .{ .inspect_str = "-8000000000000" } },
    .{ .name = "inspect: I64 times", .source = "{ a : I64\n    a = -4294967296\n    b : I64\n    b = 2147483648\n    a * b\n}", .expected = .{ .inspect_str = "-9223372036854775808" } },
    .{ .name = "inspect: I64 div", .source = "{ a : I64\n    a = -9223372036854775808\n    b : I64\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-4611686018427387904" } },
    .{ .name = "inspect: I64 rem", .source = "{ a : I64\n    a = -9223372036854775808\n    b : I64\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-8" } },

    .{ .name = "inspect: I128 negate", .source = "{ a : I128\n    a = -85070591730234615865843651857942052864\n    -a\n}", .expected = .{ .inspect_str = "85070591730234615865843651857942052864" } },
    .{ .name = "inspect: I128 plus", .source = "{ a : I128\n    a = -100000000000000000000000\n    b : I128\n    b = -50000000000000000000000\n    a + b\n}", .expected = .{ .inspect_str = "-150000000000000000000000" } },
    .{ .name = "inspect: I128 minus", .source = "{ a : I128\n    a = -100000000000000000000000\n    b : I128\n    b = 50000000000000000000000\n    a - b\n}", .expected = .{ .inspect_str = "-150000000000000000000000" } },
    .{ .name = "inspect: I128 times", .source = "{ a : I128\n    a = -18446744073709551616\n    b : I128\n    b = 9223372036854775808\n    a * b\n}", .expected = .{ .inspect_str = "-170141183460469231731687303715884105728" } },
    .{ .name = "inspect: I128 div", .source = "{ a : I128\n    a = -170141183460469231731687303715884105728\n    b : I128\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-85070591730234615865843651857942052864" } },
    .{ .name = "inspect: I128 rem", .source = "{ a : I128\n    a = -170141183460469231731687303715884105728\n    b : I128\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-29" } },

    .{ .name = "inspect: F32 literal", .source = "3.14.F32", .expected = .{ .inspect_str = "3.14" } },
    .{ .name = "inspect: F32 variable assignment", .source = "{ a : F32\n    a = 3.14.F32\n    a\n}", .expected = .{ .inspect_str = "3.14" } },
    .{ .name = "inspect: F32 negate", .source = "{ a : F32\n    a = 3.14.F32\n    -a\n}", .expected = .{ .inspect_str = "-3.14" } },
    .{ .name = "inspect: F32 plus", .source = "{ a : F32\n    a = 1.5.F32\n    b : F32\n    b = 2.5.F32\n    a + b\n}", .expected = .{ .inspect_str = "4" } },
    .{ .name = "inspect: F32 minus", .source = "{ a : F32\n    a = 10.0.F32\n    b : F32\n    b = 3.5.F32\n    a - b\n}", .expected = .{ .inspect_str = "6.5" } },
    .{ .name = "inspect: F32 times", .source = "{ a : F32\n    a = 2.5.F32\n    b : F32\n    b = 4.0.F32\n    a * b\n}", .expected = .{ .inspect_str = "10" } },
    .{ .name = "inspect: F32 div", .source = "{ a : F32\n    a = 10.0.F32\n    b : F32\n    b = 2.0.F32\n    a / b\n}", .expected = .{ .inspect_str = "5" } },

    .{ .name = "inspect: F64 negate", .source = "{ a : F64\n    a = 3.141592653589793.F64\n    -a\n}", .expected = .{ .inspect_str = "-3.141592653589793" } },
    .{ .name = "inspect: F64 plus", .source = "{ a : F64\n    a = 1.5.F64\n    b : F64\n    b = 2.5.F64\n    a + b\n}", .expected = .{ .inspect_str = "4" } },
    .{ .name = "inspect: F64 minus", .source = "{ a : F64\n    a = 10.5.F64\n    b : F64\n    b = 3.25.F64\n    a - b\n}", .expected = .{ .inspect_str = "7.25" } },
    .{ .name = "inspect: F64 times", .source = "{ a : F64\n    a = 2.5.F64\n    b : F64\n    b = 4.0.F64\n    a * b\n}", .expected = .{ .inspect_str = "10" } },
    .{ .name = "inspect: F64 div", .source = "{ a : F64\n    a = 10.0.F64\n    b : F64\n    b = 2.0.F64\n    a / b\n}", .expected = .{ .inspect_str = "5" } },

    .{ .name = "inspect: Dec literal", .source = "3.14.Dec", .expected = .{ .inspect_str = "3.14" } },
    .{ .name = "inspect: Dec negate", .source = "{ a : Dec\n    a = 3.14.Dec\n    -a\n}", .expected = .{ .inspect_str = "-3.14" } },
    .{ .name = "inspect: Dec plus", .source = "{ a : Dec\n    a = 1.5.Dec\n    b : Dec\n    b = 2.5.Dec\n    a + b\n}", .expected = .{ .inspect_str = "4.0" } },
    .{ .name = "inspect: Dec minus", .source = "{ a : Dec\n    a = 10.0.Dec\n    b : Dec\n    b = 3.5.Dec\n    a - b\n}", .expected = .{ .inspect_str = "6.5" } },
    .{ .name = "inspect: Dec times", .source = "{ a : Dec\n    a = 2.5.Dec\n    b : Dec\n    b = 4.0.Dec\n    a * b\n}", .expected = .{ .inspect_str = "10.0" } },
    .{ .name = "inspect: Dec div", .source = "{ a : Dec\n    a = 10.0.Dec\n    b : Dec\n    b = 2.0.Dec\n    a / b\n}", .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "inspect: Dec to_str", .source = "{ a : Dec\n    a = 100.0.Dec\n    Dec.to_str(a)\n}", .expected = .{ .inspect_str = "\"100.0\"" } },

    // Remaining semantic ports from interpreter_style_test.zig
    .{ .name = "inspect: match list rest binds slice", .source = "match [1, 2, 3] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }", .expected = .{ .inspect_str = "3.0" } },
    .{
        .name = "inspect: simple for loop sum",
        .source =
        \\{
        \\    var total = 0
        \\    for n in [1, 2, 3, 4] {
        \\        total = total + n
        \\    }
        \\    total
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{ .name = "inspect: inline identity lambda on string", .source = "(|x| x)(\"Hello\")", .expected = .{ .inspect_str = "\"Hello\"" } },
    .{ .name = "inspect: inline increment lambda on dec literal", .source = "(|n| n + 1)(41)", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: inline binary add lambda on dec literals", .source = "(|a, b| a + b)(40, 2)", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: slash division defaults to Dec", .source = "6 / 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: decimal addition simple fraction", .source = "0.2 + 0.3", .expected = .{ .inspect_str = "0.5" } },
    .{ .name = "inspect: decimal division by integer literal", .source = "0.5 / 2", .expected = .{ .inspect_str = "0.25" } },
    .{ .name = "inspect: custom tag renders as tag name", .source = "MyTag", .expected = .{ .inspect_str = "MyTag" } },
    .{
        .name = "inspect: record update copies base fields",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: point.y }
        \\    (updated.x, updated.y)
        \\}
        ,
        .expected = .{ .inspect_str = "(1.0, 2.0)" },
    },
    .{
        .name = "inspect: record update overrides field",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: 3 }
        \\    (updated.x, updated.y)
        \\}
        ,
        .expected = .{ .inspect_str = "(1.0, 3.0)" },
    },
    .{
        .name = "inspect: record update expression references base",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: point.y + 5 }
        \\    updated.y
        \\}
        ,
        .expected = .{ .inspect_str = "7.0" },
    },
    .{ .name = "inspect: match tuple pattern destructures", .source = "match (1, 2) { (1, b) => b, _ => 0 }", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: match bool patterns", .source = "match True { True => 1, False => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: match result tag payload", .source = "match Ok(3) { Ok(n) => n + 1, Err(_) => 0 }", .expected = .{ .inspect_str = "4.0" } },
    .{
        .name = "inspect: match branch alternatives remap first binder",
        .source =
        \\{
        \\    value = if True Ok(3) else Err(4)
        \\    match value { Ok(v) | Err(v) => v }
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "inspect: match branch alternatives remap later binder",
        .source =
        \\{
        \\    value = if False Ok(3) else Err(4)
        \\    match value { Ok(v) | Err(v) => v }
        \\}
        ,
        .expected = .{ .inspect_str = "4.0" },
    },
    .{ .name = "inspect: match record destructures fields", .source = "match { x: 1, y: 2 } { { x, y } => x + y }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "inspect: render Try.Ok literal", .source = "match True { True => Ok(42), False => Err(\"boom\") }", .expected = .{ .inspect_str = "Ok(42.0)" } },
    .{ .name = "inspect: render Try.Err string", .source = "match True { True => Err(\"boom\"), False => Ok(42) }", .expected = .{ .inspect_str = "Err(\"boom\")" } },
    .{ .name = "inspect: render Try.Ok tuple payload", .source = "match True { True => Ok((1, 2)), False => Err(\"boom\") }", .expected = .{ .inspect_str = "Ok((1.0, 2.0))" } },
    .{ .name = "inspect: effectful Try.map_ok callback keeps effectful source type", .source = "Try.map_ok!(Try.Ok(100), |val| val - 50)", .expected = .{ .inspect_str = "Ok(50.0)" } },
    .{ .name = "inspect: match tuple payload tag", .source = "match Ok((1, 2)) { Ok((a, b)) => a + b, Err(_) => 0 }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "inspect: match record payload tag", .source = "match Err({ code: 1, msg: \"boom\" }) { Err({ code, msg: _msg }) => code, Ok(_) => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: direct list pattern destructure sum", .source = "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }", .expected = .{ .inspect_str = "6.0" } },
    .{ .name = "inspect: list pattern keeps more specific rest branch first", .source = "match [1, 2] { [x, y, ..] => y, [x, ..] => x, _ => 0 }", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: list pattern falls through to less specific rest branch", .source = "match [1] { [x, y, ..] => y, [x, ..] => x, _ => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: List.len on literal", .source = "List.len([1, 2, 3])", .expected = .{ .inspect_str = "3" } },
    .{
        .name = "inspect: generic local List.len specialization stays resolved",
        .source_kind = .module,
        .source =
        \\measure = |xs| xs.len()
        \\
        \\main = (measure([1, 2, 3]), measure(["a", "bb"]))
        ,
        .expected = .{ .inspect_str = "(3, 2)" },
    },
    .{
        .name = "inspect: top-level empty callable list has no reachable callable slots",
        .source_kind = .module,
        .source =
        \\empty_fns : List((I64 -> I64))
        \\empty_fns = []
        \\
        \\main = List.len(empty_fns)
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "inspect: promoted callable captures empty callable list schema only",
        .source_kind = .module,
        .source =
        \\make_len : List((I64 -> I64)) -> (I64 -> U64)
        \\make_len = |fns| |_x| List.len(fns)
        \\
        \\len_empty : I64 -> U64
        \\len_empty = make_len([])
        \\
        \\main = len_empty(41.I64)
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "inspect: promoted callable ignores inactive callable tag payload",
        .source_kind = .module,
        .source =
        \\make_tagged : [A(I64), B((I64 -> I64))] -> (I64 -> I64)
        \\make_tagged = |tagged| |x|
        \\    match tagged {
        \\        A(n) => x + n
        \\        B(f) => f(x)
        \\    }
        \\
        \\add1 : I64 -> I64
        \\add1 = make_tagged(A(1.I64))
        \\
        \\main = add1(41.I64)
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{ .name = "inspect: List.fold builtin sum", .source = "List.fold([1, 2, 3], 0, |acc, item| acc + item)", .expected = .{ .inspect_str = "6.0" } },
    .{
        .name = "inspect: List.iter folds builtin iterator",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3].iter()
        \\    Iter.fold(iter, 0.I64, |acc, item| acc + item)
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "inspect: List.iter maps builtin iterator",
        .source =
        \\{
        \\    iter = Iter.map([10.I64, 20].iter(), |item| item + 1)
        \\    Iter.fold(iter, 0.I64, |acc, item| acc + item)
        \\}
        ,
        .expected = .{ .inspect_str = "32" },
    },
    .{
        .name = "inspect: Iter.iter returns the same iterator",
        .source =
        \\{
        \\    iter = [1.I64, 2].iter().iter()
        \\    Iter.fold(iter, 0.I64, |acc, item| acc + item)
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: Iter.single yields one item",
        .source =
        \\{
        \\    iter = Iter.single(42.I64)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[42]" },
    },
    .{
        .name = "inspect: Iter.single reports known length one",
        .source = "Iter.size_hint(Iter.single(42.I64))",
        .expected = .{ .inspect_str = "Known(1)" },
    },
    .{
        .name = "inspect: Iter.concat yields first then second",
        .source =
        \\{
        \\    iter = [1.I64, 2].iter().concat([3, 4].iter())
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3, 4]" },
    },
    .{
        .name = "inspect: Iter.concat starts second after empty first",
        .source =
        \\{
        \\    iter = [].iter().concat([1.I64, 2].iter())
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2]" },
    },
    .{
        .name = "inspect: Iter.concat continues after skips",
        .source =
        \\{
        \\    first = [1.I64, 2, 3].iter().keep_if(|item| item > 1)
        \\    iter = first.concat([4, 5].iter())
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[2, 3, 4, 5]" },
    },
    .{
        .name = "inspect: Iter.concat reports summed known length",
        .source = "Iter.size_hint([1.I64, 2].iter().concat([3, 4, 5].iter()))",
        .expected = .{ .inspect_str = "Known(5)" },
    },
    .{
        .name = "inspect: Iter.concat reports unknown length when either side is unknown",
        .source = "Iter.size_hint([1.I64, 2, 3].iter().keep_if(|item| item > 1).concat([4].iter()))",
        .expected = .{ .inspect_str = "Unknown" },
    },
    .{
        .name = "inspect: Iter.append yields item after iterator",
        .source =
        \\{
        \\    iter = [1.I64, 2].iter().append(3)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3]" },
    },
    .{
        .name = "inspect: Iter.append reports incremented known length",
        .source = "Iter.size_hint([1.I64, 2, 3].iter().append(4))",
        .expected = .{ .inspect_str = "Known(4)" },
    },
    .{
        .name = "inspect: Iter.keep_if emits skip with rest iterator",
        .source =
        \\match Iter.next(Iter.keep_if([1.I64, 2].iter(), |item| item > 1)) {
        \\    Skip({ rest }) => Iter.fold(rest, 0.I64, |acc, item| acc + item) == 2
        \\    _ => Bool.False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: Iter.keep_if fold continues after skips",
        .source =
        \\{
        \\    iter = Iter.keep_if([1.I64, 2, 3, 4, 5].iter(), |item| I64.rem_by(item, 2) == 0)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[2, 4]" },
    },
    .{
        .name = "inspect: Iter.drop_if fold continues after skips",
        .source =
        \\{
        \\    iter = Iter.drop_if([1.I64, 2, 3, 4, 5].iter(), |item| I64.rem_by(item, 2) == 0)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 3, 5]" },
    },
    .{
        .name = "for loop over filtered iterator continues after skips",
        .source =
        \\{
        \\    iter = Iter.keep_if([1.I64, 2, 3, 4, 5].iter(), |item| I64.rem_by(item, 2) == 1)
        \\    var $sum = 0.I64
        \\    for item in iter {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "9" },
    },
    .{
        .name = "inspect: recursive custom iterator take_first works in for loop",
        .source_kind = .module,
        .source =
        \\fib_iter : Iter(U64)
        \\fib_iter = {
        \\    adv : ((U64, U64) -> Try((U64, (U64, U64)), [NoMore]))
        \\    adv = |(a, b)| Try.Ok((a, (b, a + b)))
        \\
        \\    Iter.custom(
        \\        (0.U64, 1.U64),
        \\        Unknown,
        \\        adv,
        \\    )
        \\}
        \\
        \\main = {
        \\    var $out = []
        \\    for f in fib_iter.take_first(5) {
        \\        $out = $out.append(f)
        \\    }
        \\    $out
        \\}
        ,
        .expected = .{ .inspect_str = "[0, 1, 1, 2, 3]" },
    },
    .{
        .name = "inspect: Iter.step_by yields first then every nth",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3, 4, 5].iter().step_by(2)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 3, 5]" },
    },
    .{
        .name = "inspect: Iter.step_by even length stops within bounds",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3, 4, 5, 6].iter().step_by(2)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 3, 5]" },
    },
    .{
        .name = "inspect: Iter.step_by of 1 is identity",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3].iter().step_by(1)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3]" },
    },
    .{
        .name = "inspect: Iter.step_by of 0 is empty",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3].iter().step_by(0)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[]" },
    },
    .{
        .name = "inspect: Iter.step_by on empty source is empty",
        .source =
        \\{
        \\    iter = [].iter().step_by(3)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[]" },
    },
    .{
        .name = "inspect: Iter.step_by larger than length yields first only",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3].iter().step_by(10)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1]" },
    },
    .{
        .name = "inspect: Iter.step_by on a range",
        .source =
        \\{
        \\    iter = (1.U64..=10).step_by(3)
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 4, 7, 10]" },
    },
    .{
        .name = "inspect: Iter.step_by counts values across skips from keep_if",
        .source =
        \\{
        \\    evens = [1.I64, 2, 3, 4, 5, 6].iter().keep_if(|x| I64.rem_by(x, 2) == 0)
        \\    Iter.fold(evens.step_by(2), [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[2, 6]" },
    },
    .{
        .name = "inspect: Iter.step_by preserves known length as ceil",
        .source = "Iter.size_hint([1.I64, 2, 3, 4, 5].iter().step_by(2))",
        .expected = .{ .inspect_str = "Known(3)" },
    },
    .{
        .name = "inspect: Iter.step_by known length on a range",
        .source = "Iter.size_hint((1.U64..=10).step_by(3))",
        .expected = .{ .inspect_str = "Known(4)" },
    },
    .{
        .name = "inspect: Iter.step_by of 0 has known length zero",
        .source = "Iter.size_hint([1.I64, 2, 3].iter().step_by(0))",
        .expected = .{ .inspect_str = "Known(0)" },
    },
    .{
        .name = "inspect: Iter.step_by reports unknown length for unknown source",
        .source = "Iter.size_hint([1.I64, 2, 3, 4].iter().keep_if(|x| x > 1).step_by(2))",
        .expected = .{ .inspect_str = "Unknown" },
    },
    .{
        .name = "inspect: Iter.rev reverses items",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3].iter().rev()
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[3, 2, 1]" },
    },
    .{
        .name = "inspect: Iter.rev on empty source is empty",
        .source =
        \\{
        \\    iter = [].iter().rev()
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[]" },
    },
    .{
        .name = "inspect: Iter.rev on a singleton",
        .source =
        \\{
        \\    iter = [1.I64].iter().rev()
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[1]" },
    },
    .{
        .name = "inspect: Iter.rev on a range",
        .source =
        \\{
        \\    iter = (1.U64..=5).rev()
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[5, 4, 3, 2, 1]" },
    },
    .{
        .name = "inspect: Iter.rev of unknown-length source upgrades to known",
        .source =
        \\{
        \\    odds = [1.I64, 2, 3, 4, 5].iter().keep_if(|x| I64.rem_by(x, 2) == 1)
        \\    Iter.fold(odds.rev(), [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[5, 3, 1]" },
    },
    .{
        .name = "inspect: Iter.rev composed with step_by",
        .source =
        \\{
        \\    iter = [1.I64, 2, 3, 4, 5].iter().step_by(2).rev()
        \\    Iter.fold(iter, [], |acc, item| acc.append(item))
        \\}
        ,
        .expected = .{ .inspect_str = "[5, 3, 1]" },
    },
    .{
        .name = "inspect: Iter.rev reports known length",
        .source = "Iter.size_hint([1.I64, 2, 3].iter().rev())",
        .expected = .{ .inspect_str = "Known(3)" },
    },
    .{
        .name = "inspect: Iter.rev upgrades unknown length to known",
        .source = "Iter.size_hint([1.I64, 2, 3, 4, 5].iter().keep_if(|x| I64.rem_by(x, 2) == 1).rev())",
        .expected = .{ .inspect_str = "Known(3)" },
    },
    .{ .name = "inspect: List.any true on integers", .source = "List.any([1, 0, 1, 0, -1], |x| x > 0)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.any false on positive integers with negative predicate", .source = "List.any([9, 8, 7, 6, 5], |x| x < 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.any false on empty list", .source = "List.any([], |x| x < 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.all false when some elements fail", .source = "List.all([9, 18, 7, 6, 15], |x| x < 10)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.all true on small integers", .source = "List.all([9, 8, 7, 6, 5], |x| x < 10)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.all on empty list is True", .source = "List.all([], |x| x < 10)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.contains false for missing element", .source = "List.contains([-1, -2, -3, 1, 2, 3], 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.contains true when element is found", .source = "List.contains([1, 2, 3, 4, 5], 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.contains false on empty list", .source = "List.contains([], 3333)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: U32 literal survives call boundary", .source = "(|x| x)(1.U32)", .expected = .{ .inspect_str = "1" } },
    .{ .name = "inspect: U32 parameter computes with static dispatch", .source = "(|current| current + 1)(1.U32)", .expected = .{ .inspect_str = "2" } },
    .{
        .name = "inspect: mutable parameter reads initial value",
        .source =
        \\{
        \\    read = |var $current| $current
        \\    read(1.U32)
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: local mutable U32 reassigns computed value",
        .source =
        \\{
        \\    var $current = 1.U32
        \\    $current = $current + 1
        \\    $current
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: mutable parameter reassigns literal",
        .source =
        \\{
        \\    set = |var $current| {
        \\        $current = 2.U32
        \\        $current
        \\    }
        \\    set(1.U32)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: mutable parameter computes from initial value",
        .source =
        \\{
        \\    bump = |var $current| $current + 1
        \\    bump(1.U32)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: mutable parameter reassign reads updated value",
        .source =
        \\{
        \\    bump = |var $current| {
        \\        $current = $current + 1
        \\        $current
        \\    }
        \\    bump(1.U32)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: mutable parameter while loop counts through bound",
        .source =
        \\{
        \\    count_to = |var $current, end| {
        \\        var $count = 0.U32
        \\        while $current <= end {
        \\            $count = $count + 1
        \\            $current = $current + 1
        \\        }
        \\        $count
        \\    }
        \\    count_to(1.U32, 5.U32)
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "inspect: local while loop appends full U32 range",
        .source =
        \\{
        \\    var $current = 1.U32
        \\    var $answer = []
        \\    while $current <= 5.U32 {
        \\        $answer = $answer.append($current)
        \\        $current = $current + 1
        \\    }
        \\    $answer
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3, 4, 5]" },
    },
    .{
        .name = "inspect: U32.to builds inclusive range iterator",
        .source = "Iter.fold(1.U32..=5.U32, [], |acc, item| acc.append(item))",
        .expected = .{ .inspect_str = "[1, 2, 3, 4, 5]" },
    },
    .{
        .name = "inspect: U32.until builds exclusive range iterator",
        .source = "Iter.fold(0.U32..<3.U32, [], |acc, item| acc.append(item))",
        .expected = .{ .inspect_str = "[0, 1, 2]" },
    },
    .{
        .name = "inspect: I64.until builds exclusive range iterator",
        .source = "Iter.fold((-2.I64)..<2.I64, [], |acc, item| acc.append(item))",
        .expected = .{ .inspect_str = "[-2, -1, 0, 1]" },
    },
    .{
        .name = "inspect: numeric to methods all return iterators",
        .source =
        \\{
        \\    u8 = Iter.fold(1.U8..=3.U8, 0.U8, |acc, item| acc + item)
        \\    i8 = Iter.fold((-1.I8)..=1.I8, 0.I8, |acc, item| acc + item)
        \\    u16 = Iter.fold(1.U16..=3.U16, 0.U16, |acc, item| acc + item)
        \\    i16 = Iter.fold((-1.I16)..=1.I16, 0.I16, |acc, item| acc + item)
        \\    u32 = Iter.fold(1.U32..=3.U32, 0.U32, |acc, item| acc + item)
        \\    i32 = Iter.fold((-1.I32)..=1.I32, 0.I32, |acc, item| acc + item)
        \\    u64 = Iter.fold(1.U64..=3.U64, 0.U64, |acc, item| acc + item)
        \\    i64 = Iter.fold((-1.I64)..=1.I64, 0.I64, |acc, item| acc + item)
        \\    u128 = Iter.fold(1.U128..=3.U128, 0.U128, |acc, item| acc + item)
        \\    i128 = Iter.fold((-1.I128)..=1.I128, 0.I128, |acc, item| acc + item)
        \\    dec = Iter.fold(1.0..=3.0, 0.0.Dec, |acc, item| acc + item)
        \\    (u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, dec)
        \\}
        ,
        .expected = .{ .inspect_str = "(6, 0, 6, 0, 6, 0, 6, 0, 6, 0, 6.0)" },
    },
    .{
        .name = "inspect: numeric until methods all return iterators",
        .source =
        \\{
        \\    u8 = Iter.fold(1.U8..<3.U8, 0.U64, |acc, _| acc + 1)
        \\    i8 = Iter.fold((-1.I8)..<1.I8, 0.U64, |acc, _| acc + 1)
        \\    u16 = Iter.fold(1.U16..<3.U16, 0.U64, |acc, _| acc + 1)
        \\    i16 = Iter.fold((-1.I16)..<1.I16, 0.U64, |acc, _| acc + 1)
        \\    u32 = Iter.fold(1.U32..<3.U32, 0.U64, |acc, _| acc + 1)
        \\    i32 = Iter.fold((-1.I32)..<1.I32, 0.U64, |acc, _| acc + 1)
        \\    u64 = Iter.fold(1.U64..<3.U64, 0.U64, |acc, _| acc + 1)
        \\    i64 = Iter.fold((-1.I64)..<1.I64, 0.U64, |acc, _| acc + 1)
        \\    u128 = Iter.fold(1.U128..<3.U128, 0.U64, |acc, _| acc + 1)
        \\    i128 = Iter.fold((-1.I128)..<1.I128, 0.U64, |acc, _| acc + 1)
        \\    dec = Iter.fold(1.0..<3.0, 0.U64, |acc, _| acc + 1)
        \\    (u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, dec)
        \\}
        ,
        .expected = .{ .inspect_str = "(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)" },
    },
    .{
        .name = "inspect: checked integer arithmetic reports boundary errors",
        .source =
        \\{
        \\    (
        \\        U8.add_try(250, 5),
        \\        U8.add_try(250, 6),
        \\        U8.sub_try(0, 1),
        \\        U8.mul_try(16, 16),
        \\        U8.div_try(1, 0),
        \\        I8.add_try(126, 1),
        \\        I8.add_try(127, 1),
        \\        I8.add_try(I8.lowest, -1),
        \\        I8.sub_try(I8.lowest, 1),
        \\        I8.mul_try(63, 2),
        \\        I8.mul_try(64, 2),
        \\        I8.mul_try(I8.lowest, -1),
        \\        I8.div_try(I8.lowest, -1),
        \\        I8.div_try(1, 0),
        \\        I8.div_try(-7, 2),
        \\        I64.add_try(I64.highest, -1),
        \\        I64.add_try(I64.highest, 1),
        \\        U128.mul_try(U128.highest, 2),
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(Ok(255), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(127), Err(Overflow), Err(Overflow), Err(Overflow), Ok(126), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(-3), Ok(9223372036854775806), Err(Overflow), Err(Overflow))" },
    },
    .{
        .name = "inspect: unsigned integer try arithmetic covers every width",
        .source =
        \\{
        \\    check_u8 =
        \\        U8.add_try(U8.highest, 0) == Ok(U8.highest)
        \\        and U8.add_try(U8.highest, 1) == Err(Overflow)
        \\        and U8.sub_try(0, 0) == Ok(0)
        \\        and U8.sub_try(0, 1) == Err(Overflow)
        \\        and U8.mul_try(U8.highest, 1) == Ok(U8.highest)
        \\        and U8.mul_try(U8.highest, 2) == Err(Overflow)
        \\        and U8.div_try(U8.highest, 1) == Ok(U8.highest)
        \\        and U8.div_try(1, 0) == Err(DivByZero)
        \\        and U8.pow_try(2, 3) == Ok(8)
        \\        and U8.pow_try(U8.highest, 2) == Err(Overflow)
        \\        and U8.div_ceil_try(7, 2) == Ok(4)
        \\        and U8.div_ceil_try(1, 0) == Err(DivByZero)
        \\    check_u16 =
        \\        U16.add_try(U16.highest, 0) == Ok(U16.highest)
        \\        and U16.add_try(U16.highest, 1) == Err(Overflow)
        \\        and U16.sub_try(0, 0) == Ok(0)
        \\        and U16.sub_try(0, 1) == Err(Overflow)
        \\        and U16.mul_try(U16.highest, 1) == Ok(U16.highest)
        \\        and U16.mul_try(U16.highest, 2) == Err(Overflow)
        \\        and U16.div_try(U16.highest, 1) == Ok(U16.highest)
        \\        and U16.div_try(1, 0) == Err(DivByZero)
        \\        and U16.pow_try(2, 3) == Ok(8)
        \\        and U16.pow_try(U16.highest, 2) == Err(Overflow)
        \\        and U16.div_ceil_try(7, 2) == Ok(4)
        \\        and U16.div_ceil_try(1, 0) == Err(DivByZero)
        \\    check_u32 =
        \\        U32.add_try(U32.highest, 0) == Ok(U32.highest)
        \\        and U32.add_try(U32.highest, 1) == Err(Overflow)
        \\        and U32.sub_try(0, 0) == Ok(0)
        \\        and U32.sub_try(0, 1) == Err(Overflow)
        \\        and U32.mul_try(U32.highest, 1) == Ok(U32.highest)
        \\        and U32.mul_try(U32.highest, 2) == Err(Overflow)
        \\        and U32.div_try(U32.highest, 1) == Ok(U32.highest)
        \\        and U32.div_try(1, 0) == Err(DivByZero)
        \\        and U32.pow_try(2, 3) == Ok(8)
        \\        and U32.pow_try(U32.highest, 2) == Err(Overflow)
        \\        and U32.div_ceil_try(7, 2) == Ok(4)
        \\        and U32.div_ceil_try(1, 0) == Err(DivByZero)
        \\    check_u64 =
        \\        U64.add_try(U64.highest, 0) == Ok(U64.highest)
        \\        and U64.add_try(U64.highest, 1) == Err(Overflow)
        \\        and U64.sub_try(0, 0) == Ok(0)
        \\        and U64.sub_try(0, 1) == Err(Overflow)
        \\        and U64.mul_try(U64.highest, 1) == Ok(U64.highest)
        \\        and U64.mul_try(U64.highest, 2) == Err(Overflow)
        \\        and U64.div_try(U64.highest, 1) == Ok(U64.highest)
        \\        and U64.div_try(1, 0) == Err(DivByZero)
        \\        and U64.pow_try(2, 3) == Ok(8)
        \\        and U64.pow_try(U64.highest, 2) == Err(Overflow)
        \\        and U64.div_ceil_try(7, 2) == Ok(4)
        \\        and U64.div_ceil_try(1, 0) == Err(DivByZero)
        \\    check_u128 =
        \\        U128.add_try(U128.highest, 0) == Ok(U128.highest)
        \\        and U128.add_try(U128.highest, 1) == Err(Overflow)
        \\        and U128.sub_try(0, 0) == Ok(0)
        \\        and U128.sub_try(0, 1) == Err(Overflow)
        \\        and U128.mul_try(U128.highest, 1) == Ok(U128.highest)
        \\        and U128.mul_try(U128.highest, 2) == Err(Overflow)
        \\        and U128.div_try(U128.highest, 1) == Ok(U128.highest)
        \\        and U128.div_try(1, 0) == Err(DivByZero)
        \\        and U128.pow_try(2, 3) == Ok(8)
        \\        and U128.pow_try(U128.highest, 2) == Err(Overflow)
        \\        and U128.div_ceil_try(7, 2) == Ok(4)
        \\        and U128.div_ceil_try(1, 0) == Err(DivByZero)
        \\    (check_u8, check_u16, check_u32, check_u64, check_u128)
        \\}
        ,
        .expected = .{ .inspect_str = "(True, True, True, True, True)" },
    },
    .{
        .name = "inspect: signed integer try arithmetic covers every width",
        .source =
        \\{
        \\    i8 = (
        \\        I8.add_try(I8.highest, -1),
        \\        I8.add_try(I8.highest, 1),
        \\        I8.sub_try(I8.lowest, -1),
        \\        I8.sub_try(I8.lowest, 1),
        \\        I8.mul_try(I8.highest, 1),
        \\        I8.mul_try(I8.highest, 2),
        \\        I8.mul_try(I8.lowest, -1),
        \\        I8.div_try(I8.lowest, -1),
        \\        I8.div_try(1, 0),
        \\        I8.pow_try(2, 3),
        \\        I8.pow_try(2, -1),
        \\        I8.pow_try(I8.highest, 2),
        \\        I8.pow_try(-1, -3),
        \\        I8.div_ceil_try(7, 2),
        \\        I8.div_ceil_try(-7, 2),
        \\        I8.div_ceil_try(I8.lowest, -1),
        \\    )
        \\    i16 = (
        \\        I16.add_try(I16.highest, -1),
        \\        I16.add_try(I16.highest, 1),
        \\        I16.sub_try(I16.lowest, -1),
        \\        I16.sub_try(I16.lowest, 1),
        \\        I16.mul_try(I16.highest, 1),
        \\        I16.mul_try(I16.highest, 2),
        \\        I16.mul_try(I16.lowest, -1),
        \\        I16.div_try(I16.lowest, -1),
        \\        I16.div_try(1, 0),
        \\        I16.pow_try(2, 3),
        \\        I16.pow_try(2, -1),
        \\        I16.pow_try(I16.highest, 2),
        \\        I16.pow_try(-1, -3),
        \\        I16.div_ceil_try(7, 2),
        \\        I16.div_ceil_try(-7, 2),
        \\        I16.div_ceil_try(I16.lowest, -1),
        \\    )
        \\    i32 = (
        \\        I32.add_try(I32.highest, -1),
        \\        I32.add_try(I32.highest, 1),
        \\        I32.sub_try(I32.lowest, -1),
        \\        I32.sub_try(I32.lowest, 1),
        \\        I32.mul_try(I32.highest, 1),
        \\        I32.mul_try(I32.highest, 2),
        \\        I32.mul_try(I32.lowest, -1),
        \\        I32.div_try(I32.lowest, -1),
        \\        I32.div_try(1, 0),
        \\        I32.pow_try(2, 3),
        \\        I32.pow_try(2, -1),
        \\        I32.pow_try(I32.highest, 2),
        \\        I32.pow_try(-1, -3),
        \\        I32.div_ceil_try(7, 2),
        \\        I32.div_ceil_try(-7, 2),
        \\        I32.div_ceil_try(I32.lowest, -1),
        \\    )
        \\    i64 = (
        \\        I64.add_try(I64.highest, -1),
        \\        I64.add_try(I64.highest, 1),
        \\        I64.sub_try(I64.lowest, -1),
        \\        I64.sub_try(I64.lowest, 1),
        \\        I64.mul_try(I64.highest, 1),
        \\        I64.mul_try(I64.highest, 2),
        \\        I64.mul_try(I64.lowest, -1),
        \\        I64.div_try(I64.lowest, -1),
        \\        I64.div_try(1, 0),
        \\        I64.pow_try(2, 3),
        \\        I64.pow_try(2, -1),
        \\        I64.pow_try(I64.highest, 2),
        \\        I64.pow_try(-1, -3),
        \\        I64.div_ceil_try(7, 2),
        \\        I64.div_ceil_try(-7, 2),
        \\        I64.div_ceil_try(I64.lowest, -1),
        \\    )
        \\    i128 = (
        \\        I128.add_try(I128.highest, -1),
        \\        I128.add_try(I128.highest, 1),
        \\        I128.sub_try(I128.lowest, -1),
        \\        I128.sub_try(I128.lowest, 1),
        \\        I128.mul_try(I128.highest, 1),
        \\        I128.mul_try(I128.highest, 2),
        \\        I128.mul_try(I128.lowest, -1),
        \\        I128.div_try(I128.lowest, -1),
        \\        I128.div_try(1, 0),
        \\        I128.pow_try(2, 3),
        \\        I128.pow_try(2, -1),
        \\        I128.pow_try(I128.highest, 2),
        \\        I128.pow_try(-1, -3),
        \\        I128.div_ceil_try(7, 2),
        \\        I128.div_ceil_try(-7, 2),
        \\        I128.div_ceil_try(I128.lowest, -1),
        \\    )
        \\    (i8, i16, i32, i64, i128)
        \\}
        ,
        .expected = .{ .inspect_str = "((Ok(126), Err(Overflow), Ok(-127), Err(Overflow), Ok(127), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(8), Err(Underflow), Err(Overflow), Ok(-1), Ok(4), Ok(-3), Err(Overflow)), (Ok(32766), Err(Overflow), Ok(-32767), Err(Overflow), Ok(32767), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(8), Err(Underflow), Err(Overflow), Ok(-1), Ok(4), Ok(-3), Err(Overflow)), (Ok(2147483646), Err(Overflow), Ok(-2147483647), Err(Overflow), Ok(2147483647), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(8), Err(Underflow), Err(Overflow), Ok(-1), Ok(4), Ok(-3), Err(Overflow)), (Ok(9223372036854775806), Err(Overflow), Ok(-9223372036854775807), Err(Overflow), Ok(9223372036854775807), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(8), Err(Underflow), Err(Overflow), Ok(-1), Ok(4), Ok(-3), Err(Overflow)), (Ok(170141183460469231731687303715884105726), Err(Overflow), Ok(-170141183460469231731687303715884105727), Err(Overflow), Ok(170141183460469231731687303715884105727), Err(Overflow), Err(Overflow), Err(Overflow), Err(DivByZero), Ok(8), Err(Underflow), Err(Overflow), Ok(-1), Ok(4), Ok(-3), Err(Overflow)))" },
    },
    .{
        .name = "inspect: try APIs unify with open error rows",
        .source =
        \\{
        \\    label : Try(a, [Overflow, DivByZero, Underflow, SqrtOfNegative, ..]) -> Str
        \\    label = |result| match result {
        \\        Ok(_) => "ok"
        \\        Err(Overflow) => "overflow"
        \\        Err(DivByZero) => "div-by-zero"
        \\        Err(Underflow) => "underflow"
        \\        Err(SqrtOfNegative) => "sqrt-of-negative"
        \\        Err(_) => "other"
        \\    }
        \\
        \\    (
        \\        label(U8.add_try(1, 2)),
        \\        label(U8.add_try(U8.highest, 1)),
        \\        label(I8.div_try(1, 0)),
        \\        label(I8.pow_try(2, -1)),
        \\        label(Dec.sqrt_try(-1.0)),
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(\"ok\", \"overflow\", \"div-by-zero\", \"underflow\", \"sqrt-of-negative\")" },
    },
    .{
        .name = "inspect: try row widening preserves ok payload",
        .source =
        \\{
        \\    result : Try(I8, [Overflow])
        \\    result = I8.mul_try(1, 2)
        \\
        \\    widened : Try(I8, [Overflow, Underflow])
        \\    widened =
        \\        match result {
        \\            Ok(value) => Ok(value)
        \\            Err(Overflow) => Err(Overflow)
        \\        }
        \\
        \\    widened
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(2)" },
    },
    .{
        .name = "inspect: high arity conditional try preserves ok payload",
        .source =
        \\{
        \\    step = |lowest, highest, zero, one, two, neg_one, acc, base, exponent|
        \\        if exponent == zero {
        \\            Ok(acc)
        \\        } else {
        \\            Err(Overflow)
        \\        }
        \\
        \\    step(I8.lowest, I8.highest, 0.I8, 1.I8, 2.I8, -1.I8, 1.I8, 2.I8, 0.I8)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(1)" },
    },
    .{
        .name = "inspect: numeric compare covers all integer widths and Dec",
        .source =
        \\{
        \\    (
        \\        U8.compare(0, U8.highest) == LT and U8.compare(U8.highest, U8.highest) == EQ and U8.compare(U8.highest, 0) == GT,
        \\        I8.compare(I8.lowest, 0) == LT and I8.compare(0, 0) == EQ and I8.compare(I8.highest, 0) == GT,
        \\        U16.compare(0, U16.highest) == LT and U16.compare(U16.highest, U16.highest) == EQ and U16.compare(U16.highest, 0) == GT,
        \\        I16.compare(I16.lowest, 0) == LT and I16.compare(0, 0) == EQ and I16.compare(I16.highest, 0) == GT,
        \\        U32.compare(0, U32.highest) == LT and U32.compare(U32.highest, U32.highest) == EQ and U32.compare(U32.highest, 0) == GT,
        \\        I32.compare(I32.lowest, 0) == LT and I32.compare(0, 0) == EQ and I32.compare(I32.highest, 0) == GT,
        \\        U64.compare(0, U64.highest) == LT and U64.compare(U64.highest, U64.highest) == EQ and U64.compare(U64.highest, 0) == GT,
        \\        I64.compare(I64.lowest, 0) == LT and I64.compare(0, 0) == EQ and I64.compare(I64.highest, 0) == GT,
        \\        U128.compare(0, U128.highest) == LT and U128.compare(U128.highest, U128.highest) == EQ and U128.compare(U128.highest, 0) == GT,
        \\        I128.compare(I128.lowest, 0) == LT and I128.compare(0, 0) == EQ and I128.compare(I128.highest, 0) == GT,
        \\        Dec.compare(Dec.lowest, 0.0) == LT and Dec.compare(0.0, 0.0) == EQ and Dec.compare(Dec.highest, 0.0) == GT,
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(True, True, True, True, True, True, True, True, True, True, True)" },
    },
    .{
        .name = "inspect: Dec constants and math APIs are deterministic",
        .source =
        \\{
        \\    within = |actual, expected| Dec.abs_diff(actual, expected) <= 0.000000000001
        \\    half_pi = Dec.div_by(Dec.pi, 2.0)
        \\    quarter_pi = Dec.div_by(Dec.pi, 4.0)
        \\
        \\    constants =
        \\        Dec.to_str(Dec.e) == "2.718281828459045235"
        \\        and Dec.to_str(Dec.pi) == "3.141592653589793238"
        \\        and Dec.to_str(Dec.tau) == "6.283185307179586476"
        \\    sqrt =
        \\        Dec.sqrt(1.44) == 1.2
        \\        and Dec.sqrt(0.000000000000000001) == 0.000000001
        \\        and match Dec.sqrt_try(-1.0) {
        \\            Ok(_) => False
        \\            Err(SqrtOfNegative) => True
        \\        }
        \\    pow =
        \\        Dec.pow(2.0, 3.0) == 8.0
        \\        and Dec.pow(2.0, -3.0) == 0.125
        \\        and within(Dec.pow(4.0, 0.5), 2.0)
        \\    trig =
        \\        within(Dec.sin(0.0), 0.0)
        \\        and within(Dec.cos(0.0), 1.0)
        \\        and within(Dec.sin(half_pi), 1.0)
        \\        and within(Dec.cos(Dec.pi), -1.0)
        \\        and within(Dec.tan(quarter_pi), 1.0)
        \\    inverse =
        \\        within(Dec.asin(1.0), half_pi)
        \\        and within(Dec.acos(1.0), 0.0)
        \\        and within(Dec.atan(1.0), quarter_pi)
        \\
        \\    (constants, sqrt, pow, trig, inverse)
        \\}
        ,
        .expected = .{ .inspect_str = "(True, True, True, True, True)" },
    },
    .{
        .name = "crash: Dec.sqrt rejects negative input",
        .source = "Dec.sqrt(-1.0)",
        .expected = .{ .crash = {} },
    },
    .{
        .name = "crash: Dec.asin rejects values outside domain",
        .source = "Dec.asin(2.0)",
        .expected = .{ .crash = {} },
    },
    .{
        .name = "crash: Dec.pow rejects negative base with fractional exponent",
        .source = "Dec.pow(-2.0, 0.5)",
        .expected = .{ .crash = {} },
    },
    .{
        .name = "inspect: numeric inclusive ranges stop at highest",
        .source =
        \\{
        \\    count = |iter| Iter.fold(iter, 0.U64, |acc, _| acc + 1)
        \\    (
        \\        count(U8.highest..=U8.highest),
        \\        count(I8.highest..=I8.highest),
        \\        count(U16.highest..=U16.highest),
        \\        count(I16.highest..=I16.highest),
        \\        count(U32.highest..=U32.highest),
        \\        count(I32.highest..=I32.highest),
        \\        count(U64.highest..=U64.highest),
        \\        count(I64.highest..=I64.highest),
        \\        count(U128.highest..=U128.highest),
        \\        count(I128.highest..=I128.highest),
        \\        count(Dec.highest..=Dec.highest),
        \\        count((Dec.highest - 0.5)..=Dec.highest),
        \\        count((Dec.highest - 1.0)..=Dec.highest),
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2)" },
    },
    .{
        .name = "inspect: numeric exclusive ranges stop at highest",
        .source =
        \\{
        \\    count = |iter| Iter.fold(iter, 0.U64, |acc, _| acc + 1)
        \\    (
        \\        count(U8.highest..<U8.highest),
        \\        count(I8.highest..<I8.highest),
        \\        count(U16.highest..<U16.highest),
        \\        count(I16.highest..<I16.highest),
        \\        count(U32.highest..<U32.highest),
        \\        count(I32.highest..<I32.highest),
        \\        count(U64.highest..<U64.highest),
        \\        count(I64.highest..<I64.highest),
        \\        count(U128.highest..<U128.highest),
        \\        count(I128.highest..<I128.highest),
        \\        count(Dec.highest..<Dec.highest),
        \\        count((Dec.highest - 0.5)..<Dec.highest),
        \\        count((Dec.highest - 1.0)..<Dec.highest),
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)" },
    },
    .{
        .name = "inspect: numeric exclusive ranges include predecessor of highest",
        .source =
        \\{
        \\    count = |iter| Iter.fold(iter, 0.U64, |acc, _| acc + 1)
        \\    (
        \\        count((U8.highest - 1)..<U8.highest),
        \\        count((I8.highest - 1)..<I8.highest),
        \\        count((U16.highest - 1)..<U16.highest),
        \\        count((I16.highest - 1)..<I16.highest),
        \\        count((U32.highest - 1)..<U32.highest),
        \\        count((I32.highest - 1)..<I32.highest),
        \\        count((U64.highest - 1)..<U64.highest),
        \\        count((I64.highest - 1)..<I64.highest),
        \\        count((U128.highest - 1)..<U128.highest),
        \\        count((I128.highest - 1)..<I128.highest),
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)" },
    },
    .{
        // Directly exercises the steps_between length primitive: ascending ->
        // Known(count); descending and equal -> Known(0) (the lower guard
        // branch the range boundary tests never hit); and the U128/I128
        // over-U64-width + Dec cases -> Unknown (the fallback that feeds the
        // from_iter grow path).
        .name = "inspect: steps_between reports Known counts, Known(0) descending, Unknown on overflow",
        .source =
        \\{
        \\    (
        \\        U8.steps_between(5, 10),
        \\        U8.steps_between(10, 5),
        \\        U8.steps_between(5, 5),
        \\        I8.steps_between(-3, 2),
        \\        I8.steps_between(2, -3),
        \\        U128.steps_between(0, 100),
        \\        U128.steps_between(0, U128.highest),
        \\        I128.steps_between(0, I128.highest),
        \\        Dec.steps_between(1.0, 5.0),
        \\    )
        \\}
        ,
        .expected = .{ .inspect_str = "(Known(5), Known(0), Known(0), Known(5), Known(0), Known(100), Unknown, Unknown, Unknown)" },
    },
    .{
        // Collecting a range whose length is Unknown (Dec ranges always report
        // Unknown) must route through from_iter's grow path rather than exact
        // preallocation. It also exercises the generic from_numeral fold: the
        // range constructor's literal `1` is monomorphized to Dec here, so this
        // must compile (not just interpret) on every backend.
        .name = "inspect: collect over Unknown-length (Dec) range grows correctly",
        .source_kind = .module,
        .source =
        \\main : List(Dec)
        \\main = Iter.collect(1.0..<4.0)
        ,
        .expected = .{ .inspect_str = "[1.0, 2.0, 3.0]" },
    },
    .{
        .name = "inspect: generic local attached method specialization on nominal",
        .source_kind = .module,
        .source =
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
        ,
        .expected = .{ .inspect_str = "(5, 8)" },
    },
    .{
        .name = "inspect: generic local attached method specialization picks different nominal targets",
        .source_kind = .module,
        .source =
        \\Crate := [Crate(U64)].{
        \\  get : Crate -> U64
        \\  get = |Crate.Crate(n)| n
        \\}
        \\
        \\Count := [Count(U64)].{
        \\  get : Count -> U64
        \\  get = |Count.Count(n)| n + 100
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Crate.Crate(5)), read(Count.Count(8)))
        ,
        .expected = .{ .inspect_str = "(5, 108)" },
    },
    .{
        .name = "inspect: same-named block-local attached methods keep distinct nominal owners",
        .source_kind = .module,
        .source =
        \\first = {
        \\    Local := [First(U64)].{
        \\        get : Local -> U64
        \\        get = |Local.First(n)| n
        \\    }
        \\
        \\    Local.First(5).get()
        \\}
        \\
        \\second = {
        \\    Local := [Second(U64)].{
        \\        get : Local -> U64
        \\        get = |Local.Second(n)| n + 100
        \\    }
        \\
        \\    Local.Second(8).get()
        \\}
        \\
        \\main = (first, second)
        ,
        .expected = .{ .inspect_str = "(5, 108)" },
    },
    .{
        .name = "inspect: block-local associated bare forward reference preserves later sibling capture",
        .source_kind = .module,
        .source =
        \\main = {
        \\    captured = 41.U64
        \\    Local := [Local].{
        \\        first = second
        \\        second = captured
        \\    }
        \\    Local.first
        \\}
        ,
        .expected = .{ .inspect_str = "41" },
    },
    .{
        .name = "inspect: explicit where method constraint keeps owner generic",
        .source_kind = .module,
        .source =
        \\Crate := [Crate(U64)].{
        \\  get : Crate -> U64
        \\  get = |Crate.Crate(n)| n
        \\}
        \\
        \\Count := [Count(U64)].{
        \\  get : Count -> U64
        \\  get = |Count.Count(n)| n + 100
        \\}
        \\
        \\read : item -> U64 where [item.get : item -> U64]
        \\read = |value| value.get()
        \\
        \\main = (read(Crate.Crate(5)), read(Count.Count(8)))
        ,
        .expected = .{ .inspect_str = "(5, 108)" },
    },
    .{
        .name = "inspect: cross-module attached method specialization on imported nominal",
        .source_kind = .module,
        .source =
        \\import CounterMod
        \\
        \\main = CounterMod.Counter(41).get()
        ,
        .imports = &.{.{
            .name = "CounterMod",
            .source =
            \\Counter := [Counter(U64)].{
            \\  get : Counter -> U64
            \\  get = |Counter.Counter(n)| n
            \\}
            ,
        }},
        .expected = .{ .inspect_str = "41" },
    },
    .{
        .name = "inspect: static dispatch receiver result feeds another method call",
        .source_kind = .module,
        .source =
        \\Utf8Fmt := [Fmt].{
        \\  encode_bool : Utf8Fmt, Bool -> Try(List(U8), [])
        \\  encode_bool = |_fmt, b| {
        \\    if b {
        \\      Ok([116, 114, 117, 101])
        \\    } else {
        \\      Ok([102, 97, 108, 115, 101])
        \\    }
        \\  }
        \\}
        \\
        \\main = {
        \\  fmt : Utf8Fmt
        \\  fmt = Fmt
        \\
        \\  my_bool : Bool
        \\  my_bool = True
        \\
        \\  bytes = my_bool.encode(fmt).ok_or([])
        \\  Str.from_utf8_lossy(bytes)
        \\}
        ,
        .expected = .{ .inspect_str = "\"true\"" },
    },
    .{
        .name = "inspect: structural tag equality through function call result issue 8897",
        .source_kind = .module,
        .source =
        \\nth : List(Str), U64 -> Try(Str, [Nope])
        \\nth = |l, i| {
        \\  match List.get(l, i) {
        \\    Ok(e) => Ok(e)
        \\    Err(OutOfBounds) => Err(Nope)
        \\  }
        \\}
        \\
        \\main = {
        \\  first = nth(["a", "b", "c", "d", "e"], 2) == Ok("c")
        \\  second = nth(["a"], 2) == Err(Nope)
        \\  (first, second)
        \\}
        ,
        .expected = .{ .inspect_str = "(True, True)" },
    },
    .{
        .name = "inspect: cross-module polymorphic attached method specialization from helper module",
        .source_kind = .module,
        .source =
        \\import CrateMod
        \\import CountMod
        \\import Helpers
        \\
        \\main = (Helpers.read(CrateMod.Crate(5)), Helpers.read(CountMod.Count(8)))
        ,
        .imports = &.{
            .{
                .name = "CrateMod",
                .source =
                \\Crate := [Crate(U64)].{
                \\  get : Crate -> U64
                \\  get = |Crate.Crate(n)| n
                \\}
                ,
            },
            .{
                .name = "CountMod",
                .source =
                \\Count := [Count(U64)].{
                \\  get : Count -> U64
                \\  get = |Count.Count(n)| n + 100
                \\}
                ,
            },
            .{
                .name = "Helpers",
                .source =
                \\Helpers := [].{
                \\  read = |value| value.get()
                \\}
                ,
            },
        },
        .expected = .{ .inspect_str = "(5, 108)" },
    },
    .{
        .name = "inspect: imported where helper remains generic when another owner is visible",
        .source_kind = .module,
        .source =
        \\import CountMod
        \\import Helpers
        \\
        \\main = Helpers.read(CountMod.Count(8))
        ,
        .imports = &.{
            .{
                .name = "CrateMod",
                .source =
                \\Crate := [Crate(U64)].{
                \\  get : Crate -> U64
                \\  get = |Crate.Crate(n)| n
                \\}
                ,
            },
            .{
                .name = "CountMod",
                .source =
                \\Count := [Count(U64)].{
                \\  get : Count -> U64
                \\  get = |Count.Count(n)| n + 100
                \\}
                ,
            },
            .{
                .name = "Helpers",
                .source =
                \\import CrateMod
                \\
                \\Helpers := [].{
                \\  read : item -> U64 where [item.get : item -> U64]
                \\  read = |value| value.get()
                \\}
                ,
            },
        },
        .expected = .{ .inspect_str = "108" },
    },
    .{
        .name = "inspect: record field access remains separate from method calls",
        .source =
        \\{
        \\    record = { get: |n| n + 1, value: 41 }
        \\    getter = record.get
        \\    getter(record.value)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{ .name = "inspect: empty record literal", .source = "{}", .expected = .{ .inspect_str = "{}" } },
    .{ .name = "inspect: decimal literal one eighth", .source = "0.125", .expected = .{ .inspect_str = "0.125" } },
    .{ .name = "inspect: decimal addition one tenth plus two tenths", .source = "0.1 + 0.2", .expected = .{ .inspect_str = "0.3" } },
    .{ .name = "inspect: f64 is_float_eq true", .source = "F64.is_float_eq(1.0.F64, 1.0.F64)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int and decimal equality", .source = "1 == 1.0", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int less than", .source = "3 < 4", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int greater than false", .source = "5 > 8", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: f64 greater than", .source = "3.5.F64 > 1.25.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: decimal less than or equal", .source = "0.5 <= 0.5", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int and f64 less than", .source = "1 < 2.0.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int and decimal greater than false", .source = "3 > 5.5", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: bool inequality", .source = "True != False", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: decimal inequality false", .source = "0.5 != 0.5", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: f64 is_float_eq false", .source = "F64.is_float_eq(3.25.F64, 4.0.F64)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: decimal equality false", .source = "0.125 == 0.25", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: direct record literal render", .source = "{ x: 1, y: 2 }", .expected = .{ .inspect_str = "{ x: 1.0, y: 2.0 }" } },
    .{
        .name = "inspect: crash at end of if branch does not poison taken path",
        .source =
        \\{
        \\    f = |x| {
        \\        if x == 0 {
        \\            crash "division by zero"
        \\        }
        \\        42 / x
        \\    }
        \\    f(2)
        \\}
        ,
        .expected = .{ .inspect_str = "21.0" },
    },
    .{
        .name = "inspect: break inside for loop",
        .source =
        \\{
        \\    var $sum = 0
        \\    for i in [1, 2, 3, 4, 5] {
        \\        if i == 4 {
        \\            break
        \\        }
        \\        $sum = $sum + i
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: break inside while loop",
        .source =
        \\{
        \\    var $i = 1
        \\    var $sum = 0
        \\    while $i <= 5 {
        \\        if $i == 4 {
        \\            break
        \\        }
        \\        $sum = $sum + $i
        \\        $i = $i + 1
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: recursive tuple list split does not leak or corrupt result",
        .source_kind = .module,
        .source =
        \\split_by_digit_count : (U64, U64) -> List((U64, U64))
        \\split_by_digit_count = |(start, end)| {
        \\    start_digits = count_digits(start)
        \\    end_digits = count_digits(end)
        \\
        \\    if start_digits == end_digits {
        \\        [(start, end)]
        \\    } else {
        \\        boundary = pow(10, start_digits) - 1
        \\        first_range = (start, boundary)
        \\        split_by_digit_count((boundary + 1, end)).append(first_range)
        \\    }
        \\}
        \\
        \\count_digits : U64 -> U64
        \\count_digits = |n| {
        \\    if n == 0 { return 1 }
        \\    var $count = 0
        \\    var $num = n
        \\    while $num > 0 {
        \\        $count = $count + 1
        \\        $num = $num // 10
        \\    }
        \\    $count
        \\}
        \\
        \\pow : U64, U64 -> U64
        \\pow = |base, exp| {
        \\    if exp == 0 {
        \\        1
        \\    } else {
        \\        var $result = 1
        \\        var $b = base
        \\        var $e = exp
        \\        while $e > 0 {
        \\            if $e % 2 == 1 {
        \\                $result = $result * $b
        \\            }
        \\            $b = $b * $b
        \\            $e = $e // 2
        \\        }
        \\        $result
        \\    }
        \\}
        \\
        \\main = split_by_digit_count((1, 1000)).len()
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "inspect: dbg preserves recursive tag union in list child",
        .source_kind = .module,
        .source =
        \\Node := [
        \\    Text(Str),
        \\    Element(Str, List(Node)),
        \\].{
        \\    text : Str -> Node
        \\    text = |content| {
        \\        result = Text(content)
        \\        dbg result
        \\        result
        \\    }
        \\
        \\    element : Str, List(Node) -> Node
        \\    element = |tag, children| {
        \\        result = Element(tag, children)
        \\        dbg result
        \\        result
        \\    }
        \\}
        \\
        \\main = {
        \\    text_node = Node.text("hello")
        \\    elem = Node.element("div", [text_node])
        \\
        \\    match elem {
        \\        Element(_tag, children) =>
        \\            match List.first(children) {
        \\                Ok(child) =>
        \\                    match child {
        \\                        Text(content) => content == "hello"
        \\                        Element(_, _) => False
        \\                    }
        \\                Err(_) => False
        \\            }
        \\        Text(_) => False
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: chained ? with Str.from_utf8 and I32.from_str (issue 9456)",
        .source_kind = .module,
        .source =
        \\parse = |s| {
        \\    chars = s.to_utf8()
        \\    match chars {
        \\        [] => Err(ParsingError)
        \\        [_first, .. as rest] => {
        \\            num_str = Str.from_utf8(rest)?
        \\            _num = I32.from_str(num_str)?
        \\            Ok({})
        \\        }
        \\    }
        \\}
        \\
        \\main = parse("L12")
        ,
        .expected = .{ .inspect_str = "Ok({})" },
    },
};

pub const tests = core_tests ++ comptime_finalization_tests.tests ++ crypto_tests.tests ++ closure_recursion_tests.tests ++ recursive_data_tests.tests ++ low_level_tests.tests ++ highest_lowest_tests.tests ++ polymorphism_tests.tests ++ issue_tests.tests ++ interpreter_style_tests.tests ++ regression_repros.tests ++ trmc_tests.tests;
