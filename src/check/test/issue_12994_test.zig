//! Regression test for https://github.com/roc-lang/roc/issues/12994.
//!
//! An associated item declared by an annotation alone (such as a derived
//! `parser_for : _` or `is_eq : _`) is a declaration of its name wherever it is
//! written, so references to it resolve the same way whether they appear before
//! or after the annotation.
const TestEnv = @import("TestEnv.zig");

fn expectChecks(source: []const u8) !void {
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12994: a custom parser_for calls a sibling's derived parser_for declared after it" {
    try expectChecks(
        \\Test :: [].{
        \\    Message := { fields : Fields }.{
        \\        parser_for = |encoding| {
        \\            parse_fields = Fields.parser_for(encoding)
        \\            |state| {
        \\                parsed = parse_fields(state)?
        \\                Ok({ value: { fields: parsed.value }, rest: parsed.rest })
        \\            }
        \\        }
        \\    }
        \\
        \\    Fields := { message : Str }.{ parser_for : _ }
        \\}
        \\
        \\parse_message : Str -> Try(Test.Message, _)
        \\parse_message = |json| Json.parse(json)
        \\
    );
}

test "issue 12994: a custom parser_for calls a top-level derived parser_for declared after it" {
    try expectChecks(
        \\Message := { fields : Fields }.{
        \\    parser_for = |encoding| {
        \\        parse_fields = Fields.parser_for(encoding)
        \\        |state| {
        \\            parsed = parse_fields(state)?
        \\            Ok({ value: { fields: parsed.value }, rest: parsed.rest })
        \\        }
        \\    }
        \\}
        \\
        \\Fields := { message : Str }.{ parser_for : _ }
        \\
        \\parse_message : Str -> Try(Message, _)
        \\parse_message = |json| Json.parse(json)
        \\
    );
}

test "issue 12994: a sibling's derived is_eq is called before its declaration" {
    try expectChecks(
        \\A := { b : B }.{
        \\    same : A, A -> Bool
        \\    same = |x, y| B.is_eq(x.b, y.b)
        \\}
        \\
        \\B := { n : U64 }.{ is_eq : _ }
        \\
    );
}

test "issue 12994: a type's own derived is_eq is called before its declaration" {
    try expectChecks(
        \\A := { n : U64 }.{
        \\    same : A, A -> Bool
        \\    same = |x, y| A.is_eq(x, y)
        \\
        \\    is_eq : _
        \\}
        \\
    );
}

test "issue 12994: a bare derived is_eq is called before its declaration in the same block" {
    try expectChecks(
        \\A := { n : U64 }.{
        \\    same : A, A -> Bool
        \\    same = |x, y| is_eq(x, y)
        \\
        \\    is_eq : _
        \\}
        \\
    );
}

test "issue 12994: a bare derived is_eq is called after its declaration in the same block" {
    try expectChecks(
        \\A := { n : U64 }.{
        \\    is_eq : _
        \\
        \\    same : A, A -> Bool
        \\    same = |x, y| is_eq(x, y)
        \\}
        \\
    );
}
