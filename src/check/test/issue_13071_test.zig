//! Regression tests for issue 13071.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/13071
//
// A method's `where` clause failing for a receiver whose type is an alias of a
// nominal type reports at the method call, as it does when the receiver's type
// names the nominal type directly, not where the alias was instantiated.

const use_source =
    \\import Lib
    \\
    \\Use :: [].{}
    \\
    \\run : {} -> Try({ a : Str }, [Bad])
    \\run = |_| {
    \\    txn = Lib.begin({})
    \\    txn.parse("x")
    \\}
;

// The call's receiver, `txn` in `txn.parse("x")`.
const call_receiver: TestEnv.SourceSpan = .{ .line = 8, .start_column = 5, .end_column = 8 };

test "issue 13071: a where-clause failure through an aliased receiver reports at the call" {
    var lib = try TestEnv.init("Lib",
        \\Lib :: [].{
        \\    Enc :: [Default].{}
        \\
        \\    Txn : LibTxn
        \\
        \\    begin : {} -> Txn
        \\    begin = |_| LibTxn.{ n: 0 }
        \\}
        \\
        \\LibTxn :: { n : U64 }.{
        \\    parse : Lib.Txn, Str -> Try(row, [Bad])
        \\        where [row.parser_for : Lib.Enc -> (Str -> Try({ value : row, rest : Str }, [Bad]))]
        \\    parse = |_, input| {
        \\        Row : row
        \\        Row.parser_for(Lib.Enc.Default)(input).map_ok(|parsed| parsed.value)
        \\    }
        \\}
    );
    defer lib.deinit();
    try lib.assertNoErrors();

    var use = try TestEnv.initWithImport("Use", use_source, "Lib", &lib);
    defer use.deinit();
    try use.assertOneTypeErrorHighlightsWithin("Missing Method", call_receiver);
}

test "issue 13071: the same failure through a directly named receiver reports at the call" {
    var lib = try TestEnv.init("Lib",
        \\Lib :: [].{
        \\    Enc :: [Default].{}
        \\
        \\    Txn :: { n : U64 }.{
        \\        parse : Txn, Str -> Try(row, [Bad])
        \\            where [row.parser_for : Enc -> (Str -> Try({ value : row, rest : Str }, [Bad]))]
        \\        parse = |_, input| {
        \\            Row : row
        \\            Row.parser_for(Enc.Default)(input).map_ok(|parsed| parsed.value)
        \\        }
        \\    }
        \\
        \\    begin : {} -> Txn
        \\    begin = |_| Txn.{ n: 0 }
        \\}
    );
    defer lib.deinit();
    try lib.assertNoErrors();

    var use = try TestEnv.initWithImport("Use", use_source, "Lib", &lib);
    defer use.deinit();
    try use.assertOneTypeErrorHighlightsWithin("Missing Method", call_receiver);
}
