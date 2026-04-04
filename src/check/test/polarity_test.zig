//! Polarity tests for tag union open/closed inference in type annotations.

const TestEnv = @import("./TestEnv.zig");

const checkTypesModule = TestEnv.checkTypesModule;
const checkTypesModuleDefs = TestEnv.checkTypesModuleDefs;
const checkTypesModuleUserFacing = TestEnv.checkTypesModuleUserFacing;
const DefAndExpectation = TestEnv.DefAndExpectation;

// polarity //

test "check type - polarity - output is inferred as open" {
    const source =
        \\mk_my_tag = || MyTag
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "mk_my_tag", .expected = "({}) -> [MyTag, ..]" },
        },
    );
}

test "check type - polarity - input is inferred as closed" {
    const source =
        \\mk_my_tag = |MyTag as a| a
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "mk_my_tag", .expected = "[MyTag] -> [MyTag]" },
        },
    );
}

test "check type - wildcard match is inferred as open" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Red => Try.Ok(x)
        \\    _ => Try.Err("Not red")
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Red, ..a] -> Try([Red, ..a], Str)" },
        },
    );
}

test "check type - exhaustive match is inferred as closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Red => Try.Ok(x)
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Red] -> Try([Red], err)" },
        },
    );
}

test "check type - annotation with named open ext prevents closing" {
    // Using a named ext var `..a` links both occurrences to the same rigid,
    // so the annotation unification succeeds and the match cannot close it.
    // A wildcard is needed because the rigid ext means unknown tags could exist.
    const source =
        \\test : [Red, ..a] -> Try([Red, ..a], err)
        \\test = |x| {
        \\  match(x) {
        \\    Red => Try.Ok(x)
        \\    _ => Try.Ok(x)
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Red, ..a] -> Try([Red, ..a], err)" },
        },
    );
}

test "check type - annotation with open ext without wildcard is non-exhaustive" {
    // The annotation says [Red, ..a] so matching only Red without a wildcard
    // is non-exhaustive — unknown tags could exist.
    const source =
        \\test : [Red, ..a] -> Try([Red, ..a], err)
        \\test = |x| {
        \\  match(x) {
        \\    Red => Try.Ok(x)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .fail, "NON-EXHAUSTIVE MATCH");
}

test "check type - exhaustive match with nested payload is inferred as closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok(Red) => "red"
        \\    Ok(Blue) => "blue"
        \\    Err(msg) => msg
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err(Str), Ok([Blue, Red])] -> Str" },
        },
    );
}

test "check type - exhaustive match with nested payload with wildcard is inferred as open" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok(Red) => "red"
        \\    Ok(Blue) => "blue"
        \\    Ok(_) => "unknown"
        \\    Err(msg) => msg
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err(Str), Ok([Blue, Red, ..])] -> Str" },
        },
    );
}

test "check type - exhaustive match with multiple tags is inferred as closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Red => "red"
        \\    Blue => "blue"
        \\    Green => "green"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Blue, Green, Red] -> Str" },
        },
    );
}

test "check type - match with underscore binding is inferred as open" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Red => "red"
        \\    _ => "not red"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Red, ..] -> Str" },
        },
    );
}

test "check type - exhaustive match with deeply nested tags is inferred as closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Outer(Inner(Leaf)) => "leaf"
        \\    Outer(Inner(Node)) => "node"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Outer([Inner([Leaf, Node])])] -> Str" },
        },
    );
}

test "check type - exhaustive match mixed nested closure" {
    // Outer is closed (Ok + Err exhaustive), inner Ok payload is closed (A + B),
    // inner Err payload stays open (variable binding)
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok(A) => "a"
        \\    Ok(B) => "b"
        \\    Err(e) => e
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err(Str), Ok([A, B])] -> Str" },
        },
    );
}

test "check type - exhaustive match with multi-arg tag mixed open closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Pair(Red, _y) => "red"
        \\    Pair(Blue, _y) => "blue"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            // First arg: Red, Blue (no wildcard -> closed)
            // Second arg: _y, _y (variable bindings -> open)
            .{ .def = "test", .expected = "[Pair([Blue, Red], _a)] -> Str" },
        },
    );
}

test "check type - exhaustive match nested wildcard keeps inner open" {
    // Inner wildcard means inner tag union stays open, but outer is still closed
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Wrapper(Red) => "red"
        \\    Wrapper(_) => "other"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Wrapper([Red, ..])] -> Str" },
        },
    );
}

test "check type - exhaustive match single tag no payload is closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Done => "done"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Done] -> Str" },
        },
    );
}

test "check type - exhaustive match with underscore-as keeps tag union open" {
    // `_ as x` should unwrap to `_` via unwrapAsPatternIdx,
    // triggering the catch-all early return and keeping the union open.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Red => Red
        \\    _ as other => other
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Red, ..a] -> [Red, ..a]" },
        },
    );
}

test "check type - exhaustive match closes tag union inside tuple element" {
    // The tag union [Red, Blue] lives inside the first tuple element.
    // Closure must traverse through the tuple pattern to find and close it.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    (Red, _) => "r"
        \\    (Blue, _) => "b"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "([Blue, Red], _field) -> Str" },
        },
    );
}

test "check type - exhaustive match closes tag union inside record field" {
    // The tag union [Active, Inactive] lives inside the record's `status` field.
    // Closure must traverse through the record destructure to find and close it.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    { status: Active } => "active"
        \\    { status: Inactive } => "inactive"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "{ status: [Active, Inactive], .. } -> Str" },
        },
    );
}

test "check type - wildcard in record field keeps nested tag union open" {
    // Same structure as above but with a wildcard — the traversal through the
    // record finds the wildcard and correctly keeps the tag union open.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    { status: Active } => "active"
        \\    { status: _ } => "other"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "{ status: [Active, ..], .. } -> Str" },
        },
    );
}

test "check type - exhaustive match closes tag union through tag then tuple" {
    // Path to closure: tag payload (Ok/Err) -> tuple element -> tag union (Red/Blue).
    // Tests that closure traverses tag -> tuple -> tag union correctly.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok((Red, _a)) => "ok-red"
        \\    Ok((Blue, _a)) => "ok-blue"
        \\    Err(_e) => "err"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err(_a), Ok(([Blue, Red], _field))] -> Str" },
        },
    );
}

test "check type - exhaustive match closes tag union through tag then record" {
    // Path to closure: tag payload (Ok/Err) -> record field (status) -> tag union (On/Off).
    // Tests that closure traverses tag -> record -> tag union correctly.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok({ status: On }) => "ok-on"
        \\    Ok({ status: Off }) => "ok-off"
        \\    Err(_e) => "err"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err(_a), Ok({ status: [Off, On], .. })] -> Str" },
        },
    );
}

test "check type - exhaustive match opens and closes tag unions through tag then record then tuple" {
    // Two record fields inside Ok's payload: `status` is exhaustively matched (On/Off)
    // so it should be closed, while `mode` always has a wildcard so it stays open.
    // Tests that closure traverses tag -> record -> field independently per field.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok({ status: On, mode: (Big, Fast) }) => "ok-on"
        \\    Ok({ status: Off, mode: (Big, _) }) => "ok-off"
        \\    Err(_e) => "err"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err(_a), Ok({ mode: ([Big], [Fast, ..]), status: [Off, On], .. })] -> Str" },
        },
    );
}

test "check type - exhaustive match closes tag union through tuple then record" {
    // Path: tuple element -> record field -> tag union.
    // First tuple element is a record whose `color` field has an exhaustive tag union.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    ({ color: Red }, _) => "r"
        \\    ({ color: Blue }, _) => "b"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "({ color: [Blue, Red], .. }, _field) -> Str" },
        },
    );
}

test "check type - exhaustive match closes tag union through record then tuple" {
    // Path: record field -> tuple -> tag union.
    // The record's `pair` field is a tuple, and the tag union inside the first
    // tuple element should be closed.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    { pair: (On, _) } => "on"
        \\    { pair: (Off, _) } => "off"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "{ pair: ([Off, On], _field), .. } -> Str" },
        },
    );
}

test "check type - exhaustive match with different payload structures per tag" {
    // Ok has a record payload, Err has a plain tag payload.
    // The outer tag union ends up as a chained extension due to different payload types,
    // but the closure logic follows the ext var chain to close all levels.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok({ level: High }) => "ok-high"
        \\    Ok({ level: Low }) => "ok-low"
        \\    Err(Critical) => "err-crit"
        \\    Err(Warning) => "err-warn"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err([Critical, Warning]), Ok({ level: [High, Low], .. })] -> Str" },
        },
    );
}

test "check type - exhaustive match with different payload structures per tag, mixed" {
    // Ok has a record payload, Err has a plain tag payload.
    // The outer tag union ends up as a chained extension due to different payload types,
    // but the closure logic follows the ext var chain to close all levels.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok({ level: High }) => "ok-high"
        \\    Ok({ level: _ }) => "ok-low"
        \\    Err(Critical) => "err-crit"
        \\    Err(_) => "err-warn"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err([Critical, ..]), Ok({ level: [High, ..], .. })] -> Str" },
        },
    );
}

test "check type - exhaustive match with different payload structures per tag, mixed, with same branches" {
    // Ok has a record payload, Err has a plain tag payload.
    // The outer tag union ends up as a chained extension due to different payload types,
    // but the closure logic follows the ext var chain to close all levels.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok({ level: High }) | Ok({ level: _ }) => "ok"
        \\    Err(Critical) | Err(_) => "err"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Err([Critical, ..]), Ok({ level: [High, ..], .. })] -> Str" },
        },
    );
}

test "check type - exhaustive match deeply nested 3 levels mixed open closed" {
    // 3 levels of tags: outer closed, one middle branch closed, one middle open
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    L1a(L2a(L3a)) => "aaa"
        \\    L1a(L2a(L3b)) => "aab"
        \\    L1a(L2b(_y)) => "ab"
        \\    L1b => "b"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            // Outer [L1a, L1b]: closed
            // Middle [L2a, L2b]: closed
            // L2a's payload [L3a, L3b]: closed (both listed)
            // L2b's payload _y: open (variable binding)
            .{ .def = "test", .expected = "[L1a([L2a([L3a, L3b]), L2b(_a)]), L1b] -> Str" },
        },
    );
}

test "check type - exhaustive match 4 levels deep all closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    W(X(Y(Z))) => "wxyz"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[W([X([Y([Z])])])] -> Str" },
        },
    );
}

test "check type - exhaustive match 4 levels deep all but top closed" {
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    W(X(Y(Z))) | W(A) => "wxyz"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[W([A, X([Y([Z])])])] -> Str" },
        },
    );
}

test "check type - exhaustive match same tag name at multiple nesting levels" {
    // "Ok" appears at both level 1 and level 2.
    // The closure logic must not confuse the two — each level filters
    // only the patterns passed to it by the parent.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    Ok(Ok(A)) => "ok-ok-a"
        \\    Ok(Ok(B)) => "ok-ok-b"
        \\    Ok(Err(C)) => "ok-err-c"
        \\    Ok(Err(D)) => "ok-err-d"
        \\    Err(Ok(E)) => "err-ok-e"
        \\    Err(Err(_)) => "err-err"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            // Outer [Ok, Err]: closed (both listed, no catch-all)
            // Ok's payload [Ok, Err]: closed
            // Ok>Ok payload [A, B]: closed
            // Ok>Err payload [C, D]: closed
            // Err's payload [Ok, Err]: closed
            // Err>Ok payload [E]: closed (only one tag but exhaustive)
            // Err>Err payload _: open (catch-all)
            .{ .def = "test", .expected = "[Err([Err(_a), Ok([E])]), Ok([Err([C, D]), Ok([A, B])])] -> Str" },
        },
    );
}

test "check type - exhaustive match same record field at multiple nesting levels" {
    // Field "a" appears in both outer and inner record destructures.
    // Each level should independently close the tag union in its own "a" field.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    { a: { a: Red } } => "red"
        \\    { a: { a: Blue } } => "blue"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            // Inner "a" field's tag union [Red, Blue]: closed
            .{ .def = "test", .expected = "{ a: { a: [Blue, Red], .. }, .. } -> Str" },
        },
    );
}

test "check type - exhaustive match same tuple position at multiple nesting levels" {
    // Position 0 used at both outer and inner tuple levels.
    // Each level independently closes tag unions at its own position.
    // All branches destructure both levels so closure can recurse through.
    const source =
        \\test = |x| {
        \\  match(x) {
        \\    ((Red, On), (A, B)) => "1"
        \\    ((Red, Off), (A, B)) => "2"
        \\    ((Blue, On), (A, B)) => "3"
        \\    ((Blue, Off), (A, B)) => "4"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            // Inner tuple pos 0: [Red, Blue] closed, pos 1: [On, Off] closed
            // Outer tuple pos 1: single-element tuple with [A] closed
            .{ .def = "test", .expected = "(([Blue, Red], [Off, On]), ([A], [B])) -> Str" },
        },
    );
}

test "check type - exhaustive match does not over-close static dispatch return type" {
    // A static dispatch method returns a tag (inferred as open tag union).
    // The caller pattern matches it exhaustively without wildcards.
    // Each call site gets a fresh instantiation, so closing at one site
    // should not affect the type at another.
    const source =
        \\Maker := {}.{
        \\  get = |_maker| Red
        \\}
        \\
        \\narrow = {
        \\  val = Maker.get(Maker)
        \\  match val {
        \\    Red => "red"
        \\  }
        \\}
        \\
        \\broad = {
        \\  val = Maker.get(Maker)
        \\  match val {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Maker.get", .expected = "_arg -> [Red, ..]" },
            .{ .def = "narrow", .expected = "Str" },
            .{ .def = "broad", .expected = "Str" },
        },
    );
}

test "check type - exhaustive match close does not leak through shared variable" {
    // A value is returned from a match AND also used after the match.
    // The match closes the tag union, but the value is the same variable
    // used in both places. This tests that closing at the match site
    // doesn't prevent the value from being used at a broader type.
    const source =
        \\Maker := {}.{
        \\  get = |_maker| Red
        \\}
        \\
        \\test = {
        \\  val = Maker.get(Maker)
        \\  matched = match val {
        \\    Red => "red"
        \\  }
        \\  matched
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Maker.get", .expected = "_arg -> [Red, ..]" },
            .{ .def = "test", .expected = "Str" },
        },
    );
}

test "check type - exhaustive match close with value reuse after match" {
    // The match condition variable is used both in the match and passed
    // to a function that expects a broader union type.
    //
    // Exhaustively matching `val` without a wildcard closes its tag union
    // to [Red]. When `val` is then passed to `accept_broad` (which expects
    // [Blue, Red]), the closed [Red] can't unify with [Blue, Red].
    // This is the correct Roc semantics (confirmed by the Rust compiler).
    const source =
        \\Maker := {}.{
        \\  get = |_maker| Red
        \\}
        \\
        \\accept_broad = |color| {
        \\  match color {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
        \\
        \\test = {
        \\  val = Maker.get(Maker)
        \\  _narrow_result = match val {
        \\    Red => "red"
        \\  }
        \\  broad_result = accept_broad(val)
        \\  broad_result
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The first argument being passed to this function has the wrong type:
        \\**test:17:18:**
        \\```roc
        \\  broad_result = accept_broad(val)
        \\```
        \\                              ^^^
        \\
        \\This argument has the type:
        \\
        \\    [Red]
        \\
        \\But `accept_broad` needs the first argument to be:
        \\
        \\    [Blue, Red]
        \\
        \\
    );
}

test "check type - exhaustive match close with value reuse - no static dispatch" {
    // Same closing behavior as above, without static dispatch.
    // Exhaustively matching without a wildcard closes the tag union,
    // preventing it from unifying with a broader type afterward.
    const source =
        \\make = |{}| Red
        \\
        \\accept_broad = |color| {
        \\  match color {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
        \\
        \\test = {
        \\  val = make({})
        \\  _narrow_result = match val {
        \\    Red => "red"
        \\  }
        \\  broad_result = accept_broad(val)
        \\  broad_result
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The first argument being passed to this function has the wrong type:
        \\**test:15:18:**
        \\```roc
        \\  broad_result = accept_broad(val)
        \\```
        \\                              ^^^
        \\
        \\This argument has the type:
        \\
        \\    [Red]
        \\
        \\But `accept_broad` needs the first argument to be:
        \\
        \\    [Blue, Red]
        \\
        \\
    );
}

test "check type - annotation keeps tag union open despite exhaustive match" {
    // Return types are implicitly open in positive position (polarity).
    // Even though the caller matches exhaustively without a wildcard,
    // the return type remains open due to polarity.
    // Each call site gets an open type, so the value can be used at a broader type.
    const source =
        \\make : {} -> [Red]
        \\make = |{}| Red
        \\
        \\accept_broad = |color| {
        \\  match color {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
        \\
        \\test = {
        \\  val = make({})
        \\  _narrow_result = match val {
        \\    Red => "red"
        \\    _ => "other"
        \\  }
        \\  broad_result = accept_broad(val)
        \\  broad_result
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "make", .expected = "{} -> [Red, ..]" },
            .{ .def = "test", .expected = "Str" },
        },
    );
}

test "check type - annotated open arg not closed by exhaustive match in body" {
    // The function arg is annotated as open [Red, ..].
    // Matching all known tags in the body doesn't close it because
    // the ext var is rigid (from annotation), not flex.
    const source =
        \\test : [Red, ..] -> Str
        \\test = |x| {
        \\  match x {
        \\    Red => "red"
        \\    _ => "other"
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "[Red, ..] -> Str" },
        },
    );
}

test "check type - annotated open return type preserved after caller exhaustive match" {
    // Static dispatch method with return type implicitly open due to polarity.
    // Caller matches exhaustively then reuses the value — polarity keeps the type open,
    // so the second use at a broader type succeeds.
    const source =
        \\Maker := [Maker].{
        \\  get : Maker -> [Red]
        \\  get = |_maker| Red
        \\}
        \\
        \\accept_broad = |color| {
        \\  match color {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
        \\
        \\test = {
        \\  val = Maker.get(Maker)
        \\  _narrow_result = match val {
        \\    Red => "red"
        \\    _ => "other"
        \\  }
        \\  broad_result = accept_broad(val)
        \\  broad_result
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Maker.get", .expected = "Maker -> [Red, ..]" },
            .{ .def = "test", .expected = "Str" },
        },
    );
}

test "check type - annotated open return type still closed by exhaustive match without wildcard" {
    // `make` is annotated as returning [Red, ..] (open), but when instantiated
    // at the call site, the rigid ext var becomes flex. The exhaustive match
    // without a wildcard closes that flex var, so `val` becomes [Red] (closed)
    // and can't unify with [Blue, Red]. Confirmed by Rust compiler.
    const source =
        \\make : {} -> [Red, ..]
        \\make = |{}| Red
        \\
        \\accept_broad = |color| {
        \\  match color {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
        \\
        \\test = {
        \\  val = make({})
        \\  _narrow_result = match val {
        \\    Red => "red"
        \\  }
        \\  broad_result = accept_broad(val)
        \\  broad_result
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The first argument being passed to this function has the wrong type:
        \\**test:16:18:**
        \\```roc
        \\  broad_result = accept_broad(val)
        \\```
        \\                              ^^^
        \\
        \\This argument has the type:
        \\
        \\    [Red]
        \\
        \\But `accept_broad` needs the first argument to be:
        \\
        \\    [Blue, Red]
        \\
        \\
    );
}

test "check type - annotated open arg not closed even with exhaustive match" {
    // Function arg annotated as [Red, ..] (open). The `..` creates a rigid
    // ext var that the exhaustive match cannot close. The arg stays open as
    // [Red, ..], which still can't unify with [Blue, Red] (closed) because
    // the rigid ext prevents adding Blue.
    // A wildcard is needed in the match because the rigid ext makes it non-exhaustive.
    const source =
        \\accept_broad = |color| {
        \\  match color {
        \\    Red => "red"
        \\    Blue => "blue"
        \\  }
        \\}
        \\
        \\test : [Red, ..] -> Str
        \\test = |x| {
        \\  _narrow_result = match x {
        \\    Red => "red"
        \\    _ => "other"
        \\  }
        \\  accept_broad(x)
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The first argument being passed to this function has the wrong type:
        \\**test:14:3:**
        \\```roc
        \\  accept_broad(x)
        \\```
        \\               ^
        \\
        \\This argument has the type:
        \\
        \\    [Red, ..]
        \\
        \\But `accept_broad` needs the first argument to be:
        \\
        \\    [Blue, Red]
        \\
        \\**Hint:** This tag union open, but I expected it to be closed.
        \\
        \\
    );
}

test "check type - tag union - ext hints 1" {
    const source =
        \\bar : [A, B] -> [X, Y]
        \\bar = |_| X
        \\
        \\foo : [A, B] -> [X, Y]
        \\foo = |tag| bar(tag)
    ;
    // With polarity, [X, Y] in return (positive) position is implicitly open [X, Y, ..].
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "[A, B] -> [X, Y, ..]");
}

test "check type - tag union - ext hints 2" {
    const source =
        \\foo : [A, B, ..] -> [A, B]
        \\foo = |a| a
    ;
    // With polarity, [A, B] in return (positive) position is now open [A, B, ..].
    // The arg [A, B, ..] has explicit .. (flex ext). The return [A, B, ..] has a
    // polarity_open rigid ext. Unifying flex with rigid ext still produces a mismatch.
    // In user-facing mode, the polarity_open extension on the annotation is elided, showing [A, B].
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This expression is used in an unexpected way:
        \\**test:2:11:2:12:**
        \\```roc
        \\foo = |a| a
        \\```
        \\          ^
        \\
        \\It has the type:
        \\
        \\    [A, B, ..]
        \\
        \\But the annotation say it should be:
        \\
        \\    [A, B]
        \\
        \\
    );
}

test "check type - polarity - basic pos open return" {
    const source =
        \\foo : Str -> [Ok(U64), Err(Str)]
        \\foo = |_| Ok(1)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> [Err(Str), Ok(U64), ..]");
}

test "check type - polarity - alias in pos resolves to open" {
    const source =
        \\MyResult(a) : [Ok(a), Err(Str)]
        \\
        \\foo : Str -> MyResult(U64)
        \\foo = |_| Ok(1)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> MyResult(U64)");
}

test "check type - polarity - alias in neg resolves to closed" {
    const source =
        \\MyResult(a) : [Ok(a), Err(Str)]
        \\
        \\bar : MyResult(U64) -> Bool
        \\bar = |result| {
        \\    match(result) {
        \\        Ok(_) => True
        \\        Err(_) => False
        \\    }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "bar" } }, "MyResult(U64) -> Bool");
}

test "check type - polarity - basic neg closed arg" {
    const source =
        \\foo : [A, B] -> Str
        \\foo = |tag|
        \\    match tag {
        \\        A => "a"
        \\        B => "b"
        \\    }
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "[A, B] -> Str");
}

test "check type - polarity - higher order flip" {
    const source =
        \\foo : ([A, B] -> [X, Y]) -> Str
        \\foo = |callback|
        \\    match callback(A) {
        \\        X => "x"
        \\        Y => "y"
        \\    }
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "([A, B, ..] -> [X, Y]) -> Str");
}

test "check type - polarity - opaque always closed" {
    const source =
        \\MyColor := [Red, Green, Blue]
        \\
        \\foo : Str -> MyColor
        \\foo = |_| MyColor.Red
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> MyColor");
}

test "check type - polarity - undeclared tag fails" {
    const source =
        \\sneaky : Str -> [Ok(U64), Err(Str)]
        \\sneaky = |s| Bad(s)
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - polarity - match directly on function return" {
    const source =
        \\get_tag : Str -> [A, B, C]
        \\get_tag = |_| A
        \\
        \\use_tag : Str -> Str
        \\use_tag = |s|
        \\    match get_tag(s) {
        \\        A => "a"
        \\        B => "b"
        \\        C => "c"
        \\    }
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "use_tag" } }, "Str -> Str");
}

test "check type - polarity - aliased return type is open" {
    const source =
        \\MyTag : [A, B, C]
        \\
        \\get_tag : Str -> MyTag
        \\get_tag = |_| A
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "get_tag" } }, "Str -> MyTag");
}

test "check type - polarity - match directly on aliased function return" {
    // Matching directly on a function whose return is an alias should work,
    // because at the call site the polarity_open rigid gets instantiated to flex.
    const source =
        \\MyTag : [A, B, C]
        \\
        \\get_tag : Str -> MyTag
        \\get_tag = |_| A
        \\
        \\use_tag : Str -> Str
        \\use_tag = |s|
        \\    match get_tag(s) {
        \\        A => "a"
        \\        B => "b"
        \\        C => "c"
        \\    }
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "use_tag" } }, "Str -> Str");
}

test "check type - polarity - match on non-aliased function return (control)" {
    const source =
        \\get_tag : Str -> [A, B, C]
        \\get_tag = |_| A
        \\
        \\use_tag : Str -> Str
        \\use_tag = |s|
        \\    match get_tag(s) {
        \\        A => "a"
        \\        B => "b"
        \\        C => "c"
        \\    }
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "use_tag" } }, "Str -> Str");
}

test "check type - polarity - match directly on aliased function return (with binding)" {
    // Same as above but binding to an intermediate value with an alias annotation.
    const source =
        \\MyTag : [A, B, C]
        \\
        \\get_tag : Str -> MyTag
        \\get_tag = |_| A
        \\
        \\use_tag : Str -> Str
        \\use_tag = |s| {
        \\    result : MyTag
        \\    result = get_tag(s)
        \\    match result {
        \\        A => "a"
        \\        B => "b"
        \\        C => "c"
        \\    }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "use_tag" } }, "Str -> Str");
}

test "check type - polarity - user facing pos return elided" {
    const source =
        \\foo : Str -> [Ok(U64), Err(Str)]
        \\foo = |_| Ok(1)
    ;
    try checkTypesModuleUserFacing(source, .{ .pass = .{ .def = "foo" } }, "Str -> [Err(Str), Ok(U64)]");
}

test "check type - polarity - user facing neg arg with flex ext shown" {
    const source =
        \\foo : [A, B, ..] -> Str
        \\foo = |_| "hello"
    ;
    try checkTypesModuleUserFacing(source, .{ .pass = .{ .def = "foo" } }, "[A, B, ..] -> Str");
}

test "check type - polarity - value annotation closed, passed to closed param (issue 8872)" {
    // Reproduces the pattern from eval test issue 8872:
    // A polymorphic function with closed param annotation receives a value
    // whose annotation is also closed (value annotations don't get polarity_open —
    // only function return types are implicitly open).
    const source =
        \\transform_err : [Ok({}), Err(a)], (a -> b) -> [Ok({}), Err(b)]
        \\transform_err = |try_val, transform| match try_val {
        \\    Err(a) => Err(transform(a))
        \\    Ok(ok) => Ok(ok)
        \\}
        \\
        \\err : [Ok({}), Err(I32)]
        \\err = Err(42.I32)
        \\
        \\result = transform_err(err, |_e| "hello")
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "result" } }, "[Err(Str), Ok({}), ..]");
}

test "check type - polarity - explicit open ext value passed to closed param" {
    // Value annotation with explicit `..` creates a rigid ext.
    // Passing to a function with closed param is a TYPE MISMATCH because
    // the rigid ext can't unify with the closed (empty_tag_union) param.
    const source =
        \\transform_err : [Ok({}), Err(a)], (a -> b) -> [Ok({}), Err(b)]
        \\transform_err = |try_val, transform| match try_val {
        \\    Err(a) => Err(transform(a))
        \\    Ok(ok) => Ok(ok)
        \\}
        \\
        \\err : [Ok({}), Err(I32), ..]
        \\err = Err(42.I32)
        \\
        \\result = transform_err(err, |_e| "hello")
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - polarity - pressure test - aoc day1 gleam port" {
    // Port of an Advent of Code solution from Gleam to Roc.
    // Exercises: type aliases for tag unions and records, higher-order functions
    // (List.fold, List.keep_if, List.map), Try chaining with map_ok/map_err,
    // pattern matching, value annotations with tag unions, and passing values
    // between functions with open/closed tag union params and returns.
    const source =
        \\Dir : [Left, Right]
        \\
        \\Cmd : { dir : Dir, val : I64 }
        \\
        \\St : { cur_val : I64, num_times_at_0 : I64 }
        \\
        \\parse_cmd : Str -> Try(Cmd, Str)
        \\parse_cmd = |line| {
        \\    if Str.starts_with(line, "R") {
        \\        val_str = Str.drop_prefix(line, "R")
        \\        I64.from_str(val_str)
        \\            .map_ok(|val| { dir: Right, val: val })
        \\            .map_err(|_| "Invalid input")
        \\    } else if Str.starts_with(line, "L") {
        \\        val_str = Str.drop_prefix(line, "L")
        \\        I64.from_str(val_str)
        \\            .map_ok(|val| { dir: Left, val: val })
        \\            .map_err(|_| "Invalid input")
        \\    } else {
        \\        Err("Invalid input")
        \\    }
        \\}
        \\
        \\step : St, Cmd -> St
        \\step = |state, cmd| {
        \\    adjusted = match cmd.dir {
        \\        Left => cmd.val * -1.I64
        \\        Right => cmd.val
        \\    }
        \\    next_val = state.cur_val + adjusted
        \\    next_count = if next_val == 0.I64 {
        \\        state.num_times_at_0 + 1.I64
        \\    } else {
        \\        state.num_times_at_0
        \\    }
        \\    { cur_val: next_val, num_times_at_0: next_count }
        \\}
        \\
        \\run : List(Str) -> Try(St, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Str.count_utf8_bytes(l) > 0)
        \\    cmds : List(Try(Cmd, Str))
        \\    cmds = List.map(filtered, |l| parse_cmd(l))
        \\    initial : St
        \\    initial = { cur_val: 50.I64, num_times_at_0: 0.I64 }
        \\    Try.Ok(List.fold(cmds, initial, |state, cmd_result| {
        \\        match cmd_result {
        \\            Ok(cmd) => step(state, cmd)
        \\            Err(_) => state
        \\        }
        \\    }))
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(St, Str)");
}

test "check type - polarity - pressure test - aoc day2 gleam port" {
    // Port of AoC day 2 from Gleam to Roc.
    // Exercises: Bool-returning predicates, I64 arithmetic, List.fold/keep_if,
    // Try chaining with map_ok/map_err, Str parsing, value annotations.
    const source =
        \\ParsedRange : { lower : I64, upper : I64 }
        \\
        \\parse_range : Str -> Try(ParsedRange, Str)
        \\parse_range = |input| {
        \\    trimmed = Str.trim(input)
        \\    if Str.is_empty(trimmed) {
        \\        Err("Empty input")
        \\    } else {
        \\        I64.from_str(trimmed)
        \\            .map_ok(|val| { lower: val, upper: val + 10.I64 })
        \\            .map_err(|_| "Invalid number")
        \\    }
        \\}
        \\
        \\is_in_range : I64, ParsedRange -> Bool
        \\is_in_range = |num, range| {
        \\    num >= range.lower and num <= range.upper
        \\}
        \\
        \\count_valid : List(I64), ParsedRange -> I64
        \\count_valid = |nums, range| {
        \\    valid = List.keep_if(nums, |n| is_in_range(n, range))
        \\    List.fold(valid, 0.I64, |acc, _n| acc + 1.I64)
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    ranges : List(Try(ParsedRange, Str))
        \\    ranges = List.map(filtered, |line| parse_range(line))
        \\    total = List.fold(ranges, 0.I64, |acc, range_result| {
        \\        match range_result {
        \\            Ok(range) => acc + count_valid([], range)
        \\            Err(_) => acc
        \\        }
        \\    })
        \\    Try.Ok(total)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - pressure test - aoc day3 gleam port" {
    // Port of AoC day 3 from Gleam to Roc.
    // Exercises: List index operations (get), nested List.map producing pairs,
    // List.fold for accumulation, pattern matching on Try results,
    // Str comparison via is_empty, type aliases for records.
    const source =
        \\Combo : { first : Str, second : Str }
        \\
        \\ComboResult : Try(Str, Str)
        \\
        \\make_pairs : List(Str) -> List(Combo)
        \\make_pairs = |graphemes| {
        \\    List.fold(graphemes, { idx: 0.U64, pairs: [] }, |acc, g| {
        \\        rest = List.drop_first(graphemes, acc.idx + 1)
        \\        new_pairs = List.map(rest, |sub_g| {
        \\            combined : Combo
        \\            combined = { first: g, second: sub_g }
        \\            combined
        \\        })
        \\        { idx: acc.idx + 1, pairs: List.concat(acc.pairs, new_pairs) }
        \\    }).pairs
        \\}
        \\
        \\get_highest_combo : Str -> ComboResult
        \\get_highest_combo = |input| {
        \\    parts = Str.split_on(input, "")
        \\    pairs = make_pairs(parts)
        \\    match List.first(pairs) {
        \\        Err(_) => Err("No pairs found")
        \\        Ok(best) => Try.Ok(Str.concat(best.first, best.second))
        \\    }
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    results : List(ComboResult)
        \\    results = List.map(filtered, |line| get_highest_combo(line))
        \\    count = List.fold(results, 0.I64, |acc, res| {
        \\        match res {
        \\            Ok(_combo_str) => acc + 1.I64
        \\            Err(_) => acc
        \\        }
        \\    })
        \\    Try.Ok(count)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - pressure test - aoc day4 gleam port" {
    // Port of AoC day 4 from Gleam to Roc.
    // Exercises: Tag union types (Obj with Empty/Roll), Point record type,
    // nested pattern matching on tag unions, List.fold with tag union accumulator,
    // Bool returns from tag union inspection, functions taking/returning tag unions.
    const source =
        \\Obj : [Empty, Roll]
        \\
        \\Pt : { x : I64, y : I64 }
        \\
        \\Cell : { pos : Pt, obj : Obj }
        \\
        \\max_neighbors : I64
        \\max_neighbors = 4.I64
        \\
        \\get_nearby : Pt -> List(Pt)
        \\get_nearby = |pt| {
        \\    [
        \\        { x: pt.x - 1.I64, y: pt.y - 1.I64 },
        \\        { x: pt.x - 1.I64, y: pt.y },
        \\        { x: pt.x - 1.I64, y: pt.y + 1.I64 },
        \\        { x: pt.x, y: pt.y - 1.I64 },
        \\        { x: pt.x, y: pt.y + 1.I64 },
        \\        { x: pt.x + 1.I64, y: pt.y - 1.I64 },
        \\        { x: pt.x + 1.I64, y: pt.y },
        \\        { x: pt.x + 1.I64, y: pt.y + 1.I64 },
        \\    ]
        \\}
        \\
        \\count_neighbor_rolls : Pt, List(Cell) -> I64
        \\count_neighbor_rolls = |pt, grid| {
        \\    nearby = get_nearby(pt)
        \\    List.fold(nearby, 0.I64, |count, neighbor_pt| {
        \\        found = List.keep_if(grid, |cell|
        \\            cell.pos.x == neighbor_pt.x and cell.pos.y == neighbor_pt.y
        \\        )
        \\        match List.first(found) {
        \\            Ok(cell) => match cell.obj {
        \\                Roll => count + 1.I64
        \\                Empty => count
        \\            }
        \\            Err(_) => count
        \\        }
        \\    })
        \\}
        \\
        \\find_accessible_rolls : List(Cell) -> List(Cell)
        \\find_accessible_rolls = |grid| {
        \\    List.keep_if(grid, |cell| {
        \\        match cell.obj {
        \\            Empty => Bool.False
        \\            Roll => count_neighbor_rolls(cell.pos, grid) < max_neighbors
        \\        }
        \\    })
        \\}
        \\
        \\parse_cell : Str, I64, I64 -> Try(Cell, Str)
        \\parse_cell = |char, row, col| {
        \\    if Str.is_empty(char) {
        \\        Err("Empty cell")
        \\    } else if Str.starts_with(char, ".") {
        \\        Try.Ok({ pos: { x: row, y: col }, obj: Empty })
        \\    } else if Str.starts_with(char, "@") {
        \\        Try.Ok({ pos: { x: row, y: col }, obj: Roll })
        \\    } else {
        \\        Err("Invalid cell")
        \\    }
        \\}
        \\
        \\run : List(Str) -> I64
        \\run = |lines| {
        \\    grid : List(Cell)
        \\    grid = List.fold(lines, [], |acc, _line| acc)
        \\    accessible = find_accessible_rolls(grid)
        \\    List.fold(accessible, 0.I64, |count, _cell| count + 1.I64)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> I64");
}

test "check type - polarity - pressure test - aoc day5 gleam port" {
    // Port of AoC day 5 from Gleam to Roc.
    // Exercises: Tag union with 5+ constructors (RangeCompare), Range record type,
    // match expressions with many arms, nested function calls returning tag unions,
    // Try chaining, functions that pattern match on multi-constructor tag unions.
    const source =
        \\Range : { start : I64, end : I64 }
        \\
        \\RangeCompare : [
        \\    NoOverlap,
        \\    AContainedInB,
        \\    BContainedInA,
        \\    AEndBStartOverlap,
        \\    BEndAStartOverlap,
        \\]
        \\
        \\compare_ranges : Range, Range -> RangeCompare
        \\compare_ranges = |a, b| {
        \\    if a.start >= b.start and a.end <= b.end {
        \\        AContainedInB
        \\    } else if b.start >= a.start and b.end <= a.end {
        \\        BContainedInA
        \\    } else if a.start <= b.start and a.end >= b.start {
        \\        AEndBStartOverlap
        \\    } else if b.start <= a.start and b.end >= a.start {
        \\        BEndAStartOverlap
        \\    } else {
        \\        NoOverlap
        \\    }
        \\}
        \\
        \\merge_ranges : Range, Range -> Try(Range, [NoOverlap])
        \\merge_ranges = |a, b| {
        \\    cmp : RangeCompare
        \\    cmp = compare_ranges(a, b)
        \\    match cmp {
        \\        NoOverlap => Err(NoOverlap)
        \\        AContainedInB => Try.Ok(b)
        \\        BContainedInA => Try.Ok(a)
        \\        AEndBStartOverlap => Try.Ok({ start: a.start, end: b.end })
        \\        BEndAStartOverlap => Try.Ok({ start: b.start, end: a.end })
        \\    }
        \\}
        \\
        \\flatten_ranges : List(Range) -> List(Range)
        \\flatten_ranges = |ranges| {
        \\    List.fold(ranges, [], |acc, cur_range| {
        \\        merged = List.fold(acc, { found: Bool.False, result: [] }, |inner_acc, acc_range| {
        \\            match merge_ranges(acc_range, cur_range) {
        \\                Ok(merged_range) => { found: Bool.True, result: List.append(inner_acc.result, merged_range) }
        \\                Err(_) => { found: inner_acc.found, result: List.append(inner_acc.result, acc_range) }
        \\            }
        \\        })
        \\        if merged.found {
        \\            merged.result
        \\        } else {
        \\            List.append(acc, cur_range)
        \\        }
        \\    })
        \\}
        \\
        \\is_in_range : I64, Range -> Bool
        \\is_in_range = |id, range| {
        \\    id >= range.start and id <= range.end
        \\}
        \\
        \\parse_range : Str -> Try(Range, Str)
        \\parse_range = |str| {
        \\    parts = Str.split_on(str, "-")
        \\    match List.first(parts) {
        \\        Err(_) => Err("Expected 2 parts in range")
        \\        Ok(lower_str) => {
        \\            I64.from_str(lower_str)
        \\                .map_ok(|lower| { start: lower, end: lower })
        \\                .map_err(|_| "Expected a number")
        \\        }
        \\    }
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    ranges_result : List(Try(Range, Str))
        \\    ranges_result = List.map(filtered, |line| parse_range(Str.trim(line)))
        \\    ranges = List.fold(ranges_result, [], |acc, r| {
        \\        match r {
        \\            Ok(range) => List.append(acc, range)
        \\            Err(_) => acc
        \\        }
        \\    })
        \\    flat = flatten_ranges(ranges)
        \\    total = List.fold(flat, 0.I64, |acc, range| {
        \\        acc + (range.end - range.start) + 1.I64
        \\    })
        \\    Try.Ok(total)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - pressure test - aoc day6 gleam port" {
    // Port of AoC day 6 from Gleam to Roc.
    // Exercises: Tag union for operations (Add/Mul), Worksheet record type
    // with nested List fields, pattern matching on operation tags,
    // List.fold with different initial values per operation, nested map.
    const source =
        \\Operation : [Add, Mul]
        \\
        \\Worksheet : { cols : List(List(I64)), ops : List(Operation) }
        \\
        \\solve_col : List(I64), Operation -> I64
        \\solve_col = |numbers, op| {
        \\    match op {
        \\        Add => List.fold(numbers, 0.I64, |acc, cur| acc + cur)
        \\        Mul => List.fold(numbers, 1.I64, |acc, cur| acc * cur)
        \\    }
        \\}
        \\
        \\solve : Worksheet -> I64
        \\solve = |worksheet| {
        \\    col_solutions = List.fold(
        \\        worksheet.cols,
        \\        { op_idx: 0.U64, total: 0.I64 },
        \\        |acc, col| {
        \\            op_result = List.get(worksheet.ops, acc.op_idx)
        \\            col_val = match op_result {
        \\                Ok(op) => solve_col(col, op)
        \\                Err(_) => 0.I64
        \\            }
        \\            { op_idx: acc.op_idx + 1, total: acc.total + col_val }
        \\        }
        \\    )
        \\    col_solutions.total
        \\}
        \\
        \\parse_op : Str -> Try(Operation, Str)
        \\parse_op = |str| {
        \\    if Str.starts_with(str, "+") {
        \\        Try.Ok(Add)
        \\    } else if Str.starts_with(str, "*") {
        \\        Try.Ok(Mul)
        \\    } else {
        \\        Err("Invalid operation")
        \\    }
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    cols : List(List(I64))
        \\    cols = List.map(filtered, |_line| [])
        \\    ops : List(Operation)
        \\    ops = [Add, Mul]
        \\    worksheet : Worksheet
        \\    worksheet = { cols: cols, ops: ops }
        \\    Try.Ok(solve(worksheet))
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - pressure test - aoc day7 gleam port" {
    // Port of AoC day 7 from Gleam to Roc.
    // Exercises: Tag union with 4 constructors (Cell: Start/Beam/Space/Splitter),
    // Diagram record wrapping List(List(Cell)), nested pattern matching,
    // functions returning tag unions used in match arms, recursive-style fold.
    const source =
        \\Cell : [Start, Beam, Space, Splitter]
        \\
        \\Diagram : { rows : List(List(Cell)) }
        \\
        \\parse_cell : Str -> Try(Cell, Str)
        \\parse_cell = |grapheme| {
        \\    if Str.starts_with(grapheme, "S") {
        \\        Try.Ok(Start)
        \\    } else if Str.starts_with(grapheme, ".") {
        \\        Try.Ok(Space)
        \\    } else if Str.starts_with(grapheme, "^") {
        \\        Try.Ok(Splitter)
        \\    } else {
        \\        Err("Invalid cell")
        \\    }
        \\}
        \\
        \\is_beam_source : Cell -> Bool
        \\is_beam_source = |cell| {
        \\    match cell {
        \\        Beam => Bool.True
        \\        Start => Bool.True
        \\        Space => Bool.False
        \\        Splitter => Bool.False
        \\    }
        \\}
        \\
        \\count_splits : Diagram -> I64
        \\count_splits = |diagram| {
        \\    List.fold(diagram.rows, 0.I64, |row_acc, row| {
        \\        List.fold(row, row_acc, |cell_acc, cell| {
        \\            match cell {
        \\                Splitter => cell_acc + 1.I64
        \\                Start => cell_acc
        \\                Beam => cell_acc
        \\                Space => cell_acc
        \\            }
        \\        })
        \\    })
        \\}
        \\
        \\propagate_beam : Cell, Cell -> Cell
        \\propagate_beam = |above, current| {
        \\    if is_beam_source(above) {
        \\        match current {
        \\            Space => Beam
        \\            Splitter => Splitter
        \\            Start => Start
        \\            Beam => Beam
        \\        }
        \\    } else {
        \\        current
        \\    }
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    rows : List(List(Cell))
        \\    rows = List.map(filtered, |_line| [Space, Beam, Splitter, Start])
        \\    diagram : Diagram
        \\    diagram = { rows: rows }
        \\    Try.Ok(count_splits(diagram))
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - pressure test - aoc day8 gleam port" {
    // Port of AoC day 8 from Gleam to Roc.
    // Exercises: 3D point records, I64 arithmetic, List operations on records,
    // helper functions computing distances, nested record field access,
    // functions returning records, pattern matching on Try for parsing.
    const source =
        \\Point3D : { x : I64, y : I64, z : I64 }
        \\
        \\PointPair : { a : Point3D, b : Point3D, dist : I64 }
        \\
        \\abs_val : I64 -> I64
        \\abs_val = |n| {
        \\    if n < 0.I64 { n * -1.I64 } else { n }
        \\}
        \\
        \\manhattan_distance : Point3D, Point3D -> I64
        \\manhattan_distance = |a, b| {
        \\    abs_val(a.x - b.x) + abs_val(a.y - b.y) + abs_val(a.z - b.z)
        \\}
        \\
        \\point_sum : Point3D -> I64
        \\point_sum = |p| p.x + p.y + p.z
        \\
        \\point_max : Point3D, Point3D -> Point3D
        \\point_max = |a, b| {
        \\    if point_sum(a) > point_sum(b) { a } else { b }
        \\}
        \\
        \\find_closest_pair : List(Point3D) -> Try(PointPair, Str)
        \\find_closest_pair = |points| {
        \\    pairs = List.fold(points, [], |acc, point_a| {
        \\        new_pairs = List.map(points, |point_b| {
        \\            pp : PointPair
        \\            pp = { a: point_a, b: point_b, dist: manhattan_distance(point_a, point_b) }
        \\            pp
        \\        })
        \\        List.concat(acc, new_pairs)
        \\    })
        \\    non_zero = List.keep_if(pairs, |pair| pair.dist > 0.I64)
        \\    match List.first(non_zero) {
        \\        Err(_) => Err("No pairs found")
        \\        Ok(first_pair) => {
        \\            best = List.fold(non_zero, first_pair, |best_so_far, cur| {
        \\                if cur.dist < best_so_far.dist { cur } else { best_so_far }
        \\            })
        \\            Try.Ok(best)
        \\        }
        \\    }
        \\}
        \\
        \\parse_point : Str -> Try(Point3D, Str)
        \\parse_point = |line| {
        \\    parts = Str.split_on(line, ",")
        \\    match List.first(parts) {
        \\        Err(_) => Err("Expected 3 numbers")
        \\        Ok(x_str) => {
        \\            I64.from_str(Str.trim(x_str))
        \\                .map_ok(|x| { x: x, y: 0.I64, z: 0.I64 })
        \\                .map_err(|_| "Invalid number")
        \\        }
        \\    }
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    points_result : List(Try(Point3D, Str))
        \\    points_result = List.map(filtered, |line| parse_point(line))
        \\    points = List.fold(points_result, [], |acc, r| {
        \\        match r {
        \\            Ok(pt) => List.append(acc, pt)
        \\            Err(_) => acc
        \\        }
        \\    })
        \\    find_closest_pair(points)
        \\        .map_ok(|pair| pair.dist)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - pressure test - aoc day9 gleam port" {
    // Port of AoC day 9 from Gleam to Roc.
    // Exercises: 2D point records, Polygon bounding box record,
    // area calculation with I64 arithmetic, combinations via nested fold,
    // List.keep_if with complex predicates, pattern matching on list results,
    // multiple helper functions with record params and returns.
    const source =
        \\Point2D : { x : I64, y : I64 }
        \\
        \\Polygon : { min_x : I64, max_x : I64, min_y : I64, max_y : I64 }
        \\
        \\area : Point2D, Point2D -> I64
        \\area = |a, b| {
        \\    dx = a.x - b.x
        \\    dy = a.y - b.y
        \\    abs_dx = if dx < 0.I64 { dx * -1.I64 } else { dx }
        \\    abs_dy = if dy < 0.I64 { dy * -1.I64 } else { dy }
        \\    (abs_dx + 1.I64) * (abs_dy + 1.I64)
        \\}
        \\
        \\get_bounding_box : List(Point2D) -> Try(Polygon, Str)
        \\get_bounding_box = |points| {
        \\    match List.first(points) {
        \\        Err(_) => Err("Empty point list")
        \\        Ok(first) => {
        \\            initial : Polygon
        \\            initial = { min_x: first.x, max_x: first.x, min_y: first.y, max_y: first.y }
        \\            bounds = List.fold(points, initial, |acc, cur| {
        \\                next_min_x = if cur.x < acc.min_x { cur.x } else { acc.min_x }
        \\                next_max_x = if cur.x > acc.max_x { cur.x } else { acc.max_x }
        \\                next_min_y = if cur.y < acc.min_y { cur.y } else { acc.min_y }
        \\                next_max_y = if cur.y > acc.max_y { cur.y } else { acc.max_y }
        \\                { min_x: next_min_x, max_x: next_max_x, min_y: next_min_y, max_y: next_max_y }
        \\            })
        \\            Try.Ok(bounds)
        \\        }
        \\    }
        \\}
        \\
        \\make_combinations : List(Point2D) -> List({ a : Point2D, b : Point2D })
        \\make_combinations = |points| {
        \\    List.fold(points, { idx: 0.U64, combos: [] }, |acc, elem1| {
        \\        rest = List.drop_first(points, acc.idx + 1)
        \\        new_combos = List.map(rest, |elem2| { a: elem1, b: elem2 })
        \\        { idx: acc.idx + 1, combos: List.concat(acc.combos, new_combos) }
        \\    }).combos
        \\}
        \\
        \\find_max_area : List(Point2D) -> I64
        \\find_max_area = |points| {
        \\    combos = make_combinations(points)
        \\    areas = List.map(combos, |combo| area(combo.a, combo.b))
        \\    List.fold(areas, 0.I64, |max_so_far, cur| {
        \\        if cur > max_so_far { cur } else { max_so_far }
        \\    })
        \\}
        \\
        \\parse_point : Str -> Try(Point2D, Str)
        \\parse_point = |line| {
        \\    parts = Str.split_on(line, ",")
        \\    match List.first(parts) {
        \\        Err(_) => Err("Expected 2 numbers")
        \\        Ok(x_str) => {
        \\            I64.from_str(Str.trim(x_str))
        \\                .map_ok(|x| { x: x, y: 0.I64 })
        \\                .map_err(|_| "Invalid number")
        \\        }
        \\    }
        \\}
        \\
        \\run : List(Str) -> Try(I64, Str)
        \\run = |lines| {
        \\    filtered = List.keep_if(lines, |l| Bool.not(Str.is_empty(l)))
        \\    points_result : List(Try(Point2D, Str))
        \\    points_result = List.map(filtered, |line| parse_point(line))
        \\    points = List.fold(points_result, [], |acc, r| {
        \\        match r {
        \\            Ok(pt) => List.append(acc, pt)
        \\            Err(_) => acc
        \\        }
        \\    })
        \\    bbox = get_bounding_box(points)
        \\    max = find_max_area(points)
        \\    bbox.map_ok(|_b| max)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "List(Str) -> Try(I64, Str)");
}

test "check type - polarity - composed open returns" {
    try checkTypesModule(
        \\foo : Str -> [A, B]
        \\foo = |_s| A
        \\
        \\bar : Str -> [A, B]
        \\bar = |_s| B
        \\
        \\baz = |s| match Bool.True { True => foo(s), False => bar(s) }
    , .{ .pass = .{ .def = "baz" } },
        "Str -> [A, B, ..]"
    );
}
