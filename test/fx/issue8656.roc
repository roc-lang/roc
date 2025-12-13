app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8656
# Tests that generalization handles variables that redirect to higher-rank
# variables without panicking.
#
# The original bug was: when a variable added to the var_pool at rank 1
# was later redirected (via setVarRedirect) to a variable at rank 2,
# generalization would try to add the resolved variable at rank 2 to the
# tmp_var_pool which only goes up to rank 1, causing a panic.

main! = || {
    Stdout.line!("ok")
}

# The following code structure triggered the bug in the original report.
# It involves complex type definitions with nested tuples and recursive functions.

Maybe(t) : [
    Some(t),
    None,
]

TokenContents : [
    NewlineToken,
    SymbolsToken(Str),
    SnakeCaseIdentToken(Str),
    EndOfFileToken,
]

TokenizerResult : (
    Try(TokenContents, Str),
    U64,
    U64,
)

get_next_token : List(U8), U64 -> TokenizerResult
get_next_token = |file, index| {
    match List.get(file, index) {
        Ok('\n') => (Ok(NewlineToken), index, index + 1)
        Err(_) => (Ok(EndOfFileToken), index, index)
    }
}

# This function has a nested lambda (ret) that creates rank-2 variables
tokenize_identifier = |file, index, acc, start_index| {
    char = List.get(file, index)
    ret = || {
        match Str.from_utf8(acc) {
            Ok(str) => (Ok(SnakeCaseIdentToken(str)), start_index, index)
            Err(_) => (Err("Invalid UTF8"), start_index, index)
        }
    }
    match char {
        Ok(c) => {
            if ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or (c == '_') {
                tokenize_identifier(file, index + 1, List.append(acc, c), start_index)
            } else {
                ret()
            }
        }
        _ => ret()
    }
}

# This function with pattern matching on tuples exercises the type checker
parse_pattern = |file, tokenizer_result| {
    (token, _, index) = tokenizer_result
    match token {
        Ok(SnakeCaseIdentToken(ident)) => {
            match get_next_token(file, index) {
                (Ok(SymbolsToken(":")), _, index2) => Ok((ident, Some("type"), index2))
                _ => Ok((ident, None, index))
            }
        }
        _ => Err("expected pattern")
    }
}
