//! Regression tests for https://github.com/roc-lang/roc/issues/13006.
//!
//! A lambda written directly in a field of an annotated record literal is
//! checked against the field's annotated function type, exactly like the same
//! lambda as an annotated named function or as a direct call argument. Its
//! parameter is therefore the annotated nominal type, and a generic method of
//! that type can be called at two different type arguments in its body.

const TestEnv = @import("./TestEnv.zig");

test "issue 13006 - lambda in an annotated record field calls a generic method at two types" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Cell(a) :: { name : Str }.{}
        \\
        \\Store :: { names : List(Str) }.{
        \\    read : Store, Cell(a) -> List(a)
        \\    read = |_, _| []
        \\}
        \\
        \\count_cell : Cell(U64)
        \\count_cell = { name: "count" }
        \\
        \\title_cell : Cell(Str)
        \\title_cell = { name: "title" }
        \\
        \\in_record : { seen : Store -> (List(U64), List(Str)) }
        \\in_record = { seen: |s| (s.read(count_cell), s.read(title_cell)) }
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("in_record", "{ seen: Store -> (List(U64), List(Str)) }");
}

test "issue 13006 - lambdas in an annotated list and record update call a generic method at two types" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Cell(a) :: { name : Str }.{}
        \\
        \\Store :: { names : List(Str) }.{
        \\    read : Store, Cell(a) -> List(a)
        \\    read = |_, _| []
        \\}
        \\
        \\count_cell : Cell(U64)
        \\count_cell = { name: "count" }
        \\
        \\title_cell : Cell(Str)
        \\title_cell = { name: "title" }
        \\
        \\R : { seen : Store -> (List(U64), List(Str)), n : U64 }
        \\
        \\base : R
        \\base = { seen: |_| ([], []), n: 0 }
        \\
        \\updated : R
        \\updated = { ..base, seen: |s| (s.read(count_cell), s.read(title_cell)) }
        \\
        \\listed : List(Store -> (List(U64), List(Str)))
        \\listed = [|s| (s.read(count_cell), s.read(title_cell))]
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "issue 13006 - lambda in an annotated record field matches the named function" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Cell(a) :: { name : Str }.{}
        \\
        \\Store :: { names : List(Str) }.{
        \\    read : Store, Cell(a) -> List(a)
        \\    read = |_, _| []
        \\}
        \\
        \\count_cell : Cell(U64)
        \\count_cell = { name: "count" }
        \\
        \\title_cell : Cell(Str)
        \\title_cell = { name: "title" }
        \\
        \\named : Store -> (List(U64), List(Str))
        \\named = |s| (s.read(count_cell), s.read(title_cell))
        \\
        \\in_record : { seen : Store -> (List(U64), List(Str)) }
        \\in_record = { seen: |s| (s.read(count_cell), s.read(title_cell)) }
        \\
        \\both : Store -> ((List(U64), List(Str)), (List(U64), List(Str)))
        \\both = |store| (named(store), (in_record.seen)(store))
    );
    defer env.deinit();

    try env.assertNoErrors();
}

const preamble =
    \\Repro :: [].{}
    \\
    \\Cell(a) :: { name : Str }.{}
    \\
    \\Store :: { names : List(Str) }.{
    \\    read : Store, Cell(a) -> List(a)
    \\    read = |_, _| []
    \\}
    \\
    \\count_cell : Cell(U64)
    \\count_cell = { name: "count" }
    \\
    \\title_cell : Cell(Str)
    \\title_cell = { name: "title" }
    \\
;

test "issue 13006 - lambdas in an annotated tuple item and tag payload call a generic method at two types" {
    var env = try TestEnv.init("Repro", preamble ++
        \\pair : ((Store -> (List(U64), List(Str))), U64)
        \\pair = (|s| (s.read(count_cell), s.read(title_cell)), 0)
        \\
        \\wrapped : [Seen((Store -> (List(U64), List(Str))))]
        \\wrapped = Seen(|s| (s.read(count_cell), s.read(title_cell)))
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "issue 13006 - a lambda with no expected type still needs one read method per parameter type" {
    var env = try TestEnv.init("Repro", preamble ++
        \\unannotated = { seen: |s| (s.read(count_cell), s.read(title_cell)) }
    );
    defer env.deinit();

    try env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 15, .start_column = 28, .end_column = 46 });
}

test "issue 13006 - a stored lambda whose arity differs from its slot is the ordinary mismatch" {
    var env = try TestEnv.init("Repro", preamble ++
        \\in_record : { seen : Store -> (List(U64), List(Str)) }
        \\in_record = { seen: |s, _extra| (s.read(count_cell), s.read(title_cell)) }
    );
    defer env.deinit();

    try env.assertOneTypeError("Type Mismatch");
}
