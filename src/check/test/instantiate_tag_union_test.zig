//! Test for instantiating tag unions with tag payloads
//! This test is a regression test for a bug where tag union args were uninitialized.

const TestEnv = @import("./TestEnv.zig");

test "instantiate polymorphic function with nested recursive tag unions" {
    // This tests instantiation of polymorphic functions with nested recursive calls
    // that return tag unions with payloads. The code pattern is:
    // 1. Multiple mutually-recursive functions return tuples containing Try types
    // 2. Pattern matching destructures tuples to extract Try values
    // 3. Functions are called multiple times triggering instantiation
    // 4. Deep nesting of match expressions
    //
    // This is a regression test that ensures complex nested tag union patterns
    // type-check without panicking.
    const source =
        \\tokenize_identifier = |file, index, acc, start_index| {
        \\    ret = || {
        \\        match Str.from_utf8(acc) {
        \\            Ok(str) => (Ok(str), start_index, index)
        \\            Err(_) => (Err("bad utf8"), start_index, index)
        \\        }
        \\    }
        \\    match List.get(file, index) {
        \\        Ok(c) =>
        \\            if (c >= 97) and (c <= 122) {
        \\                tokenize_identifier(file, index + 1, List.append(acc, c), start_index)
        \\            } else {
        \\                ret()
        \\            }
        \\        _ => ret()
        \\    }
        \\}
        \\
        \\get_next_token = |file, index| {
        \\    match List.get(file, index) {
        \\        Ok(c) =>
        \\            if (c >= 97) and (c <= 122) {
        \\                tokenize_identifier(file, index + 1, [c], index)
        \\            } else {
        \\                (Err("unexpected"), index, index + 1)
        \\            }
        \\        Err(_) =>
        \\            (Ok("eof"), index, index)
        \\    }
        \\}
        \\
        \\parse_pattern_match_starting = |file, ident, tokenizer_result| {
        \\    (token, _, index) = tokenizer_result
        \\    match token {
        \\        Ok(s) => Ok((ident, Some(s), token, index))
        \\        _ => Ok((ident, None, token, index))
        \\    }
        \\}
        \\
        \\parse_pattern_match = |file, tokenizer_result| {
        \\    (token, _, index) = tokenizer_result
        \\    match token {
        \\        Ok(ident) =>
        \\            parse_pattern_match_starting(file, ident, get_next_token(file, index))
        \\        _ => Err("error")
        \\    }
        \\}
        \\
        \\parse_value = |file, tokenizer_result, possibilities| {
        \\    (token, token_pos, index) = tokenizer_result
        \\    match token {
        \\        Ok(n) => {
        \\            match get_next_token(file, index) {
        \\                (Ok(_), _, new_index) => Ok((n, new_index))
        \\                _ => Ok((n, index))
        \\            }
        \\        }
        \\        _ => Err("failed")
        \\    }
        \\}
        \\
        \\parse_block = |file, index, acc| {
        \\    match get_next_token(file, index) {
        \\        (Ok(n), _, index2) => {
        \\            (token, token_pos, index3) = get_next_token(file, index2)
        \\            match token {
        \\                Ok(_) => {
        \\                    (args, index4) = parse_function_call_args(file, index3, [])?
        \\                    parse_block(file, index4, acc)
        \\                }
        \\                _ => {
        \\                    (pattern, token4, index4, possibilities) = parse_pattern_match_starting(file, n, (token, token_pos, index3))?
        \\                    parse_block(file, index4, acc)
        \\                }
        \\            }
        \\        }
        \\        got => Err("error")
        \\    }
        \\}
        \\
        \\parse_function_call_args = |file, index, acc| {
        \\    (token, token_pos, index2) = get_next_token(file, index)
        \\    match token {
        \\        Ok(_) => Ok((acc, index2))
        \\        _ => {
        \\            (value, index3) = parse_value(file, (token, token_pos, index2), [])?
        \\            (token2, token2_pos, index4) = get_next_token(file, index3)
        \\            match token2 {
        \\                Ok(_) => parse_function_call_args(file, index4, List.append(acc, value))
        \\                _ => Err("error")
        \\            }
        \\        }
        \\    }
        \\}
        \\
        \\main = {
        \\    file = [104, 101, 108, 108, 111]
        \\    parse_block(file, 0, [])
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // We just care that it doesn't panic during type checking
}
