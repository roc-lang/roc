app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.6/2BfGn4M9uWJNhDVeMghGeXNVDFijMfPsmmVeo6M4QjKX.tar.zst"
}

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
    utf8 = [120, 121, 122]
    _parsed = parse!(utf8, 0, [])?
    Ok({})
}


TokenContents : [
    NewlineToken,
    OpenBracketToken,
    CloseBracketToken,
    OpenBraceToken,
    CloseBraceToken,
    SymbolsToken(Str),
    CommaToken,
    CamalCaseIdentToken(Str),
    SnakeCaseIdentToken(Str),
    EndOfFileToken,
]
Token : (TokenContents, U64)

TokenizerResult : (
    Try(TokenContents, Str),
    U64,
    U64,
)
tokenize_identifier = |file, index, acc, start_index| {
    char = List.get(file, index)
    ret = || {
        match Str.from_utf8(acc) {
            Ok(str) => {
                (Ok(SnakeCaseIdentToken(str)), start_index, index)
            }
            Err(BadUtf8({problem, index: i})) => {
                (Err("Invalid UTF8 at index ${i.to_str()}: ${Str.inspect(problem)}"), start_index, index)
            }
        }
    }
    match char {
        Ok(c) => {
            if ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or (c == '_') {
                tokenize_identifier(file, index+1, List.append(acc, c), start_index)
            } else {
                ret()
            }
        }
        _ => ret()
    }
}

get_next_token : List(U8), U64 -> TokenizerResult
get_next_token = |file, index| {
    match List.get(file, index) {
        Ok('\n') => {
            (Ok(NewlineToken), index, index + 1)
       }
       Ok(c) => {
           if ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or (c == '_') {
               tokenize_identifier(file, index+1, [c], index)
           } else {
               (Ok(EndOfFileToken), index, index)
           }
       }
        Err(_) => {
            (Ok(EndOfFileToken), index, index)
        }
    }
}

format_error = |_, _| {
    "error"
}

FuncValue : { arguments : List(Str), body : List(Statement) }
Value : [
    VariableReference(Str),
]
Statement := [
    CaseStatement({ value: Value, branches : List(List(Statement)) }),
]
Type := [
    Name(Str),
]
TopLevel : [
    ValueDefinition({name: Str, position: U64, value: FuncValue}),
    TypeDefinition({name: Str, type: Type, position: U64}),
]

parse_type = |file, index| {
    Ok((Name("T"), index))
}

parse_block : List(U8), U64, List(Statement) -> Try((List(Statement), U64), Str)
parse_block = |file, index, acc| {
    Ok((acc, index))
}

parse_function_def_args = |file, index, acc| {
    match get_next_token(file, index) {
        (Ok(SnakeCaseIdentToken(name)), _, newIndex) => {
            match get_next_token(file, newIndex) {
                (Ok(CommaToken), _, newNewIndex) => parse_function_def_args(file, newNewIndex, List.append(acc, name))
                (Ok(SymbolsToken("|")), _, newNewIndex) => Ok((acc, newNewIndex))
                got => Err(format_error(got, ["`,`", "`|` to finish the function argument section"]))
            }
        }
        (Ok(SymbolsToken("|")), _, newIndex) => Ok((acc, newIndex))
        got => Err(format_error(got, ["an identifier for one of the functions arguments", "`|` to finish the function argument section"]))
    }
}

parse_function : List(U8), U64 -> Try((FuncValue, U64), Str)
parse_function = |file, index| {
    index2 = (match get_next_token(file, index) {
        (Ok(SymbolsToken("|")), _, i) => Ok(i)
        got => Err(format_error(got, ["`|` to start a function definition"]))
    })?
    (args, index3) = parse_function_def_args(file, index2, [])?
    index4 = (match get_next_token(file, index3) {
        (Ok(OpenBraceToken), _, i) => Ok(i)
        got => Err(format_error(got, ["`{` to start a function block"]))
    })?
    (body, index5) = parse_block(file, index4, [])?
    val: FuncValue
    val = { arguments: args, body: body }
    Ok((val, index5))
}

parse! : List(U8), U64, List(TopLevel) => Try(List(TopLevel), Str)
parse! = |utf8, index, acc| {
    match get_next_token(utf8, index) {
        (Ok(SnakeCaseIdentToken(name)), pos, index2) => {
            match get_next_token(utf8, index2) {
                (Ok(SymbolsToken(":")), _, index3) => {
                    (type, index4) = parse_type(utf8, index3)?
                    parse!(utf8, index4, List.append(acc, TypeDefinition({ name: name, type: type, position: pos })))
                }
                (Ok(SymbolsToken("=")), _, index3) => {
                    (def, index4) = parse_function(utf8, index3)?
                    parse!(utf8, index4, List.append(acc, ValueDefinition({ name: name, value: def, position: pos })))
                }
                _ => Ok(acc)
            }
        }
        (Ok(EndOfFileToken), _, _) => Ok(acc)
        _ => Ok(acc)
    }
}
