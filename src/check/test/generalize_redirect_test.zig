//! Regression test for issue #8656: rank panic when variable redirects to higher-rank variable.
//!
//! The original bug was: when a variable added to the var_pool at rank 1
//! was later redirected (via setVarRedirect) to a variable at rank 2,
//! generalization would try to add the resolved variable at rank 2 to the
//! tmp_var_pool which only goes up to rank 1, causing a panic.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "nested lambda with higher-rank variables does not panic during generalization" {
    // This code structure triggered the bug in the original report.
    // It involves nested lambdas that create rank-2 variables, pattern matching
    // on tuples, and recursive functions.
    //
    // The key pattern is:
    // 1. A function with a nested lambda (ret) that creates rank-2 type variables
    // 2. Pattern matching on tuples that exercises the type checker
    // 3. Recursive calls that can cause variable redirects across ranks
    const source =
        \\{
        \\    Maybe(t) : [
        \\        Some(t),
        \\        None,
        \\    ]
        \\
        \\    TokenContents : [
        \\        NewlineToken,
        \\        SymbolsToken(Str),
        \\        SnakeCaseIdentToken(Str),
        \\        EndOfFileToken,
        \\    ]
        \\
        \\    TokenizerResult : (
        \\        Try(TokenContents, Str),
        \\        U64,
        \\        U64,
        \\    )
        \\
        \\    get_next_token : List(U8), U64 -> TokenizerResult
        \\    get_next_token = |file, index| {
        \\        match List.get(file, index) {
        \\            Ok(_) => (Ok(NewlineToken), index, index + 1)
        \\            Err(_) => (Ok(EndOfFileToken), index, index)
        \\        }
        \\    }
        \\
        \\    tokenize_identifier = |file, index, acc, start_index| {
        \\        char = List.get(file, index)
        \\        ret = || {
        \\            match Str.from_utf8(acc) {
        \\                Ok(str) => (Ok(SnakeCaseIdentToken(str)), start_index, index)
        \\                Err(_) => (Err("Invalid UTF8"), start_index, index)
        \\            }
        \\        }
        \\        match char {
        \\            Ok(c) => {
        \\                if ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or (c == '_') {
        \\                    tokenize_identifier(file, index + 1, List.append(acc, c), start_index)
        \\                } else {
        \\                    ret()
        \\                }
        \\            }
        \\            _ => ret()
        \\        }
        \\    }
        \\
        \\    parse_pattern = |file, tokenizer_result| {
        \\        (token, _, index) = tokenizer_result
        \\        match token {
        \\            Ok(SnakeCaseIdentToken(ident)) => {
        \\                match get_next_token(file, index) {
        \\                    (Ok(SymbolsToken(":")), _, index2) => Ok((ident, Some("type"), index2))
        \\                    _ => Ok((ident, None, index))
        \\                }
        \\            }
        \\            _ => Err("expected pattern")
        \\        }
        \\    }
        \\
        \\    parse_pattern
        \\}
    ;
    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // If we get here without panicking, the test passes.
    // The bug would cause a panic during type checking with:
    // "trying to add var at rank 2, but current rank is 1"
    try test_env.assertNoErrors();
}
