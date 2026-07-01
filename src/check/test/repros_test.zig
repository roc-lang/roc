//! Regression tests for specific bug fixes.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "check - repro - issue 8764" {
    const src =
        \\main! = |_| {}
        \\         
        \\NumericRangeType : [
        \\    ExcludesEndValue, # ..<
        \\    IncludesEndValue, # ..=
        \\]
        \\
        \\TokenContents : [
        \\    NewlineToken,
        \\    OpenBracketToken, # (
        \\    CloseBracketToken, # )
        \\    OpenSquareBracketToken, # [
        \\    CloseSquareBracketToken, # ]
        \\    OpenBraceToken, # {
        \\    CloseBraceToken, # }
        \\    CommaToken, # ,
        \\    ColonToken, # :
        \\    AssignToken, # =
        \\    EqualToken, # ==
        \\    MinusToken, # -
        \\    ArrowToken, # ->
        \\    DotDotToken(NumericRangeType),
        \\    DotToken, # .
        \\    UIntDigitsToken(U64),
        \\    IntDigitsToken(I64),
        \\    FloatDigitsToken(F64),
        \\    StrDigitsToken(Str),
        \\    ForToken,
        \\    InToken,
        \\    IfToken,
        \\    ElseToken,
        \\    DoToken,
        \\    WhileToken,
        \\    MutToken,
        \\    IdentToken(Str),
        \\    CommentToken(Str),
        \\    StringToken(Str),
        \\    EndOfFileToken,
        \\]
        \\
        \\get_next_token : Try(TokenContents, Str)
        \\get_next_token = {
        \\    contents : TokenContents
        \\    contents = DotDotToken(ExcludesEndValue)
        \\    Ok(contents)
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 8785" {
    // Pattern matching on recursive opaque type element retrieved from List
    const src =
        \\main! = |_| {}
        \\
        \\Node := [
        \\    Element(Str, List(Node)),
        \\    Text(Str),
        \\]
        \\
        \\test_pattern_match = |node| {
        \\    match node {
        \\        Element(tag, _) => tag
        \\        Text(text) => text
        \\    }
        \\}
        \\
        \\result = {
        \\    element_val : Node
        \\    element_val = Element("span", [])
        \\
        \\    element_list : List(Node)
        \\    element_list = [element_val]
        \\
        \\    match List.first(element_list) {
        \\        Ok(v) => test_pattern_match(v)
        \\        Err(_) => "Error"
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 8919" {
    // Instance method syntax should not produce UNUSED VALUE error
    // when the method returns {}. Previously, the return type was not
    // resolved before the unused value check due to deferred constraints.
    // The bug only manifests when the method call is an s_expr statement
    // (not the final expression of a block).
    const src =
        \\main! = |_| {}
        \\
        \\Foo := { x: I64 }.{
        \\    show! = |_self| {}
        \\}
        \\
        \\test_method! = |_| {
        \\    f : Foo
        \\    f = { x: 42 }
        \\    f.show!()
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129" {
    // Type variable becomes 'rigid' and won't unify with concrete types when
    // captured in a closure created inside a function body
    const src =
        \\BugRepro(a) := { run : {} -> a }.{
        \\    wrap : a -> BugRepro(a)
        \\    wrap = |value| { run: |{}| value }
        \\
        \\    unwrap : BugRepro(a) -> a
        \\    unwrap = |b| (b.run)({})
        \\}
        \\
        \\expect BugRepro.unwrap(BugRepro.wrap("hi")) == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 simpler" {
    // Simplified version: just wrap a value without closure
    const src =
        \\BugRepro(a) := { value : a }.{
        \\    wrap : a -> BugRepro(a)
        \\    wrap = |v| { value: v }
        \\}
        \\
        \\expect (BugRepro.wrap("hi")).value == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 closure capture" {
    // The issue is specifically about closures capturing type variables
    // This test uses a record with a closure field without nominal types
    const src =
        \\main! = |_| {}
        \\
        \\# Simple version that tests just the closure capture
        \\wrap : a -> ({} -> a)
        \\wrap = |value| |{}| value
        \\
        \\# Call the wrapped closure
        \\result = wrap("hi")({})
        \\
        \\expect result == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 - minimal closure capture" {
    // Minimal test case: a polymorphic function that returns a closure capturing the type variable
    const src =
        \\main! = |_| {}
        \\
        \\wrap : a -> ({} -> a)
        \\wrap = |value| |{}| value
        \\
        \\expect wrap("hi")({}) == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 - control no closure" {
    // Control test: polymorphic function WITHOUT a closure - this should pass
    const src =
        \\main! = |_| {}
        \\
        \\identity : a -> a
        \\identity = |value| value
        \\
        \\expect identity("hi") == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9817 - delayed top-level parser recursion" {
    const src =
        \\Parser(a) : { run : {} -> a }
        \\
        \\lazy : ({} -> Parser(a)) -> Parser(a)
        \\lazy = |thunk| {
        \\    { run: |_| (thunk({}).run)({}) }
        \\}
        \\
        \\map : Parser(a) -> Parser(a)
        \\map = |parser| {
        \\    { run: |_| (parser.run)({}) }
        \\}
        \\
        \\p : Parser(I64)
        \\p = lazy(|_| q)
        \\
        \\q : Parser(I64)
        \\q = map(p)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9693 - polymorphic helper method constraints are not defaulted at def site" {
    const src =
        \\is_valid : Str -> U64
        \\is_valid = |str| {
        \\    str.to_utf8().map_with_index(char_value).len()
        \\}
        \\
        \\char_value = |char, index| {
        \\    if char >= '0' and char <= '9' {
        \\        Ok((10 - index) * (char - '0').to_u64())
        \\    } else {
        \\        Err(BadChar)
        \\    }
        \\}
        \\
        \\main! = |_args| {
        \\    dbg is_valid("1234")
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9129 - with explicit block" {
    // Test with explicit block - still fails
    const src =
        \\main! = |_| {}
        \\
        \\wrap : a -> ({} -> a)
        \\wrap = |value| {
        \\    |{}| value
        \\}
        \\
        \\expect wrap("hi")({}) == "hi"
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 8848" {
    // Pattern matching on recursive opaque type element retrieved from List
    const src =
        \\ValueCombinationMethod := [
        \\  Modulo,
        \\]
        \\
        \\Value := [
        \\    UInt(U64),
        \\    CombinedValue({
        \\        combination_method: ValueCombinationMethod,
        \\        value1: Value,
        \\        value2: Value,
        \\    }),
        \\]
        \\
        \\TokenContents : []
        \\
        \\TokenizerResult : (
        \\    Try(TokenContents, Str),
        \\    U64, # Index of start of token/error
        \\    U64, # New index in file
        \\)
        \\get_next_token : List(U8), U64 -> TokenizerResult
        \\get_next_token = |_file, _index| {
        \\    (Err("todo"), 0, 0)
        \\}
        \\
        \\parse_value : List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)
        \\parse_value = |file, result, possibilities| {
        \\    (first_value, var $index) = parse_first_value(file, result, possibilities)?
        \\    (token, token_pos, $index) = get_next_token(file, $index)
        \\    (value1, token2, token2_pos) = match (first_value, token) {
        \\      (VariableReference(name), Ok(OpenBracketToken)) => {
        \\        # TODO
        \\        (t2, t2_pos, $index) = get_next_token(file, $index)
        \\        (FunctionCall({name, args: []}), t2, t2_pos)
        \\      }
        \\      _ => (first_value, token, token_pos)
        \\    }
        \\    combination_method1 : ValueCombinationMethod
        \\    combination_method1 = Modulo
        \\    (value2, $index) = parse_value(file, get_next_token(file, $index), [])
        \\    value : Value
        \\    value = match value2 {
        \\        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
        \\            # Combination method 1 is ran first
        \\            CombinedValue({
        \\                combination_method: combination_method2,
        \\                value1: CombinedValue({
        \\                    combination_method: combination_method1,
        \\                    value1: value1,
        \\                    value2: value2A,
        \\                }),
        \\                value2: value2B,
        \\            })
        \\        }
        \\        _ => CombinedValue({combination_method: combination_method1, value1, value2})
        \\    }
        \\    Ok((value, $index))
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I32)])
        \\main! = |_args| {
        \\    (value, _index) = parse_value(Str.to_utf8("15 + 4 == 19"), 0, []).map_err!(|e| {
        \\        Stderr.line!(e)
        \\        Exit(1)
        \\    })?
        \\    Stdout.line!(Str.inspect(value))
        \\    Ok({})
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    // No assertion here, this repro previously panicked, so that's the
    // regression we're guarding against
}

test "check - repro - bad return branch mismatch after utf8 empty guard" {
    const src =
        \\main! = |_| {}
        \\
        \\to_uppercase : U8 -> U8
        \\to_uppercase = |ch| ch - 32
        \\
        \\capitalize_first : Str -> Str
        \\capitalize_first = |s| {
        \\    bytes = Str.to_utf8(s)
        \\    if List.is_empty(bytes) {
        \\        return ""
        \\    }
        \\
        \\    first = match List.first(bytes) {
        \\        Ok(b) => b
        \\        Err(_) => 0
        \\    }
        \\    first_is_lower = first >= 'a' and first <= 'z'
        \\    new_first = if first_is_lower to_uppercase(first) else first
        \\    match Str.from_utf8([new_first]) {
        \\        Ok(str) => str
        \\        Err(_) => s
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - bad inline if branch mismatch in utf8 byte loop" {
    const src =
        \\main! = |_| {}
        \\
        \\to_lowercase : U8 -> U8
        \\to_lowercase = |ch| ch + 32
        \\
        \\to_lower_snake_case : Str -> Str
        \\to_lower_snake_case = |s| {
        \\    bytes = Str.to_utf8(s)
        \\    var $output = []
        \\
        \\    for byte in bytes {
        \\        is_upper = byte >= 'A' and byte <= 'Z'
        \\        new_byte = if is_upper to_lowercase(byte) else byte
        \\        $output = $output.append(new_byte)
        \\    }
        \\
        \\    match Str.from_utf8($output) {
        \\        Ok(str) => str
        \\        Err(_) => s
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - using dbg in a function does not make it effectful" {
    const src =
        \\main! = |_args| {}
        \\
        \\User := [
        \\  LoggedIn(Str),
        \\  Guest
        \\].{
        \\  welcome : User -> {}
        \\  welcome = |self| match (self) {
        \\    Guest => dbg "Welcome Guest"
        \\    LoggedIn(name) => dbg Str.concat("Welcome Guest", name)
        \\  }
        \\}
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - equality on tag-union alias" {
    const src =
        \\Color : [Red, Green, Blue]
        \\
        \\pick : U8 -> Color
        \\pick = |n| match n {
        \\    0 => Red
        \\    1 => Green
        \\    _ => Blue
        \\}
        \\
        \\is_red : Bool
        \\is_red = pick(0) == Red
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - inequality on tag-union alias" {
    const src =
        \\Color : [Red, Green, Blue]
        \\
        \\pick : U8 -> Color
        \\pick = |n| match n {
        \\    0 => Red
        \\    1 => Green
        \\    _ => Blue
        \\}
        \\
        \\not_red : Bool
        \\not_red = pick(0) != Red
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - equality on record alias" {
    const src =
        \\Point : { x : U8, y : U8 }
        \\
        \\origin : Point
        \\origin = { x: 0, y: 0 }
        \\
        \\is_origin : Bool
        \\is_origin = origin == { x: 0, y: 0 }
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9500 - equality on record alias of tag-union aliases" {
    const src =
        \\Palette : [None, Color1, Color2, Color3, Color4]
        \\
        \\DrawColors : {
        \\    primary : Palette,
        \\    secondary : Palette,
        \\}
        \\
        \\from_flags : U8 -> DrawColors
        \\from_flags = |flags| match flags {
        \\    0 => { primary: None, secondary: None }
        \\    _ => { primary: Color2, secondary: Color4 }
        \\}
        \\
        \\is_match : Bool
        \\is_match = from_flags(1) == { primary: Color2, secondary: Color4 }
    ;
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "check - repro - issue 9491 - local recursive def generalizes rigid type param" {
    // A local, self-recursive annotated function on a parametric recursive
    // nominal type, called through a separate annotated local helper, used to
    // produce a spurious `RBTree(k)` != `RBTree(k)` mismatch because the
    // recursive function's rigid type parameter was never generalized.
    const src =
        \\main! = |_args| {}
        \\
        \\RBTree(k) := [
        \\    Empty,
        \\    Node(RBTree(k)),
        \\].{
        \\    delete = |tree| {
        \\        delRBTree : RBTree(k) -> RBTree(k)
        \\        delRBTree = |inner| {
        \\            match inner {
        \\                RBTree.Node(Empty) => Empty
        \\                RBTree.Node(RBTree.Node(x)) => RBTree.Node(x)->delRBTree()
        \\                Empty => Empty
        \\            }
        \\        }
        \\        delCurr : RBTree(k) -> RBTree(k)
        \\        delCurr = |t| {
        \\            match t {
        \\                RBTree.Node(inner) => inner->delRBTree()
        \\                _ => t
        \\            }
        \\        }
        \\        tree->delCurr()
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9491 - distinct type param names still unify" {
    // Same as above, but the helper uses a differently-named type parameter.
    // Before the fix this surfaced as `RBTree(j)` != `RBTree(k)`, proving the
    // two sides were distinct ungeneralized rigids.
    const src =
        \\main! = |_args| {}
        \\
        \\RBTree(k) := [
        \\    Empty,
        \\    Node(RBTree(k)),
        \\].{
        \\    delete = |tree| {
        \\        delRBTree : RBTree(k) -> RBTree(k)
        \\        delRBTree = |inner| {
        \\            match inner {
        \\                RBTree.Node(Empty) => Empty
        \\                RBTree.Node(RBTree.Node(x)) => RBTree.Node(x)->delRBTree()
        \\                Empty => Empty
        \\            }
        \\        }
        \\        delCurr : RBTree(j) -> RBTree(j)
        \\        delCurr = |t| {
        \\            match t {
        \\                RBTree.Node(inner) => inner->delRBTree()
        \\                _ => t
        \\            }
        \\        }
        \\        tree->delCurr()
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9491 - self-recursive local fn generalizes (no nominal)" {
    // A self-recursive local annotated function with a rigid type parameter,
    // called by a separate annotated local helper, must generalize so each call
    // site instantiates a fresh type parameter.
    const src =
        \\main! = |_args| {
        \\    loop : a -> a
        \\    loop = |z| loop(z)
        \\    caller : b -> b
        \\    caller = |w| loop(w)
        \\    _ = caller
        \\    {}
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9491 - nested self-recursive local fns generalize" {
    // The self-recursion generalization fix must work for local defs nested
    // arbitrarily deep inside other function bodies, not just one level down.
    const src =
        \\main! = |_args| {}
        \\
        \\RBTree(k) := [
        \\    Empty,
        \\    Node(RBTree(k)),
        \\].{
        \\    delete = |tree| {
        \\        outer = |o| {
        \\            delRBTree : RBTree(k) -> RBTree(k)
        \\            delRBTree = |inner| {
        \\                match inner {
        \\                    RBTree.Node(Empty) => Empty
        \\                    RBTree.Node(RBTree.Node(x)) => RBTree.Node(x)->delRBTree()
        \\                    Empty => Empty
        \\                }
        \\            }
        \\            delCurr : RBTree(k) -> RBTree(k)
        \\            delCurr = |t| {
        \\                match t {
        \\                    RBTree.Node(i2) => i2->delRBTree()
        \\                    _ => t
        \\                }
        \\            }
        \\            o->delCurr()
        \\        }
        \\        tree->outer()
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - self-recursive local fn after early return in enclosing body" {
    // An early `return` in the enclosing function body records an early-return
    // constraint that `processReturnConstraints` later compacts out of the
    // shared constraint list while checking the local def's lambda. The local
    // def's recursive reference is tracked in a dedicated stack (NOT that shared
    // list), so the compaction cannot affect its validation. Well-typed: no
    // spurious errors.
    const src =
        \\main! = |_args| {
        \\    f : U64 -> U64
        \\    f = |n| {
        \\        if n == 0 {
        \\            return 0
        \\        }
        \\        loop : a -> a
        \\        loop = |z| loop(z)
        \\        _ = loop
        \\        n
        \\    }
        \\    _ = f
        \\    {}
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - self-recursive local fn recursive use still type-checked after early return" {
    // Companion to the well-typed case above: the self-recursive local def's
    // recursive reference must STILL be validated after the early-return
    // compaction, so an ill-typed recursive use is caught. Here the recursive
    // call result is forced to `Str`, contradicting the rigid `a` in
    // `loop : a -> a`. If the reference were dropped, no error would be reported.
    const src =
        \\use_str : Str -> Str
        \\use_str = |s| s
        \\
        \\main! = |_args| {
        \\    f : U64 -> U64
        \\    f = |n| {
        \\        if n == 0 {
        \\            return 0
        \\        }
        \\        loop : a -> a
        \\        loop = |z| {
        \\            r = loop(z)
        \\            _ = use_str(r)
        \\            z
        \\        }
        \\        _ = loop
        \\        n
        \\    }
        \\    _ = f
        \\    {}
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
}

test "check - repro - issue 9670 - typed local binding of parametric fn result" {
    // Faithful reduction of the issue: a parser-combinator-style function whose
    // result is assigned to a local binding with an explicit type annotation that
    // reuses the enclosing function's type parameters produces a spurious
    // `Parser(input, a)` != `Parser(input, a)` mismatch (the two sides print
    // identically). NOTE: this currently FAILS — it documents the open bug.
    //
    // Root cause is NOT the local annotation (see the minimal case below): the
    // trigger is the multi-parameter alias `Parser(input, output)` used with one
    // parameter (`input`) passed through unchanged and another (`a`) wrapped in a
    // different constructor (`List(a)`) between argument and return.
    const src =
        \\Parser(input, output) : { parse : input -> output }
        \\
        \\sep_by1 : Parser(input, a), Parser(input, sep) -> Parser(input, List(a))
        \\sep_by1 = |_parser, _separator| { parse: |_in| [] }
        \\
        \\sep_by : Parser(input, a), Parser(input, sep) -> Parser(input, List(a))
        \\sep_by = |parser, separator| {
        \\    sb1 : Parser(input, List(a))
        \\    sb1 = sep_by1(parser, separator)
        \\    sb1
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9670 - minimal: multi-param alias, passthrough + wrapped param" {
    // Minimal trigger for issue 9670. A two-parameter type alias is applied with
    // its first parameter (`input`) passed through unchanged and its second
    // parameter (`a`) wrapped in `List` in the return. Calling such a function
    // yields a spurious `Parser(input, a)` != `Parser(input, a)` mismatch on the
    // argument. NOTE: this currently FAILS — it documents the open bug.
    //
    // Contrast (all pass today):
    //   * single-param alias wrapped in return:  `Wrap(a) -> Wrap(List(a))`
    //   * same param shape in arg and return:    `Parser(input, a) -> Parser(input, a)`
    //   * bare type var:                         `a -> List(a)`
    //   * pure builtin List chain:               `List(a) -> List(List(a))`
    const src =
        \\Parser(input, output) : { parse : input -> output }
        \\
        \\f : Parser(input, a) -> Parser(input, List(a))
        \\f = |_p| { parse: |_in| [] }
        \\
        \\g : Parser(input, a) -> Parser(input, List(a))
        \\g = |p| f(p)
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - issue 9491 follow-up - top-level mutually recursive parametric fns" {
    // Mutually recursive functions over a parametric recursive nominal type must
    // type-check: each cross-call instantiates the callee, so the two members'
    // rigid type parameters don't clash into a spurious `T(k)` != `T(k)`.
    const src =
        \\RBMut(k) := [
        \\    Empty,
        \\    Node(RBMut(k)),
        \\]
        \\
        \\delA : RBMut(k) -> RBMut(k)
        \\delA = |inner| match inner {
        \\    RBMut.Node(x) => x->delB()
        \\    Empty => Empty
        \\}
        \\
        \\delB : RBMut(k) -> RBMut(k)
        \\delB = |t| match t {
        \\    RBMut.Node(inner) => inner->delA()
        \\    _ => t
        \\}
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "check - repro - B092 - ambiguous List.sum on empty list is rejected" {
    const src =
        \\main = || (List.sum([]))
    ;
    var test_env = try TestEnv.initWithExecutableRootNames("Test", src, &.{"main"});
    defer test_env.deinit();

    try std.testing.expect(test_env.checker.problems.problems.items.len > 0);
}
