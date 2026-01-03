app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin
import pf.Stderr

read_file_loop! = |file_name, acc| {
    match Stdin.line!() {
        "end" => Ok(Str.join_with(acc, "\n"))
        other => read_file_loop!(file_name, List.append(acc, other))
    }
}

# TODO: Switch to `File.read_utf8!(file_name)` when that is implemented
read_file! : Str => Try(Str, Str)
read_file! = |file_name| {
    Stdout.line!("Enter the contents of ${file_name} followed by `end` for the end of the file")
    read_file_loop!(file_name, [])
}

# TODO: Switch to `File.write_utf8!(file_name, text)` when that is implemented
write_file! : Str, Str => Try({}, Str)
write_file! = |file_name, text| {
    Stdout.line!("`${file_name}` \"written\" with the following contents:\n${text}")
    Ok({})
}

str_err! : Str, Try(ok, Str) => Try(ok, [Exit(I32)])
str_err! = |stage, result| {
    match result {
        Ok(value) => Ok(value)
        Err(msg) => {
            Stderr.line!("Failed to ${stage}:\n${msg}")
            Err(Exit(1))
        }
    }
}

main! : List(Str) => Try({}, [Exit(I32)])
main! = |args| {
    match args {
        [_, file_name] => {
            text = str_err!("read file `${file_name}`", read_file!(file_name))?
            Stderr.line!(text)
            utf8 = Str.to_utf8(text)
            Stderr.line!("converted to utf8")
            parsed = str_err!("parse", parse!(utf8, 0, []))?
            Stderr.line!("parsed: ${Str.inspect(parsed)}")
            typed = str_err!("type check", type_check(parsed))?
            Stderr.line!("typed")
            js = emit_js(typed)
            Stderr.line!("js emitted")
            js_file = "${file_name}.js"
            _var = str_err!("write js to `${js_file}`", write_file!(js_file, js))?
            Stderr.line!("js written")
            Ok({})
        }

        _ => {
            Stdout.line!("Invalid arguments: expected just the name of a file to compile and execute")
            Err(Exit(1))
        }
    }
}

type_check : List(TopLevel) -> Try(List(TopLevel), Str)
type_check = |ast| {
    Ok(ast)
}

emit_js = |_typed_ast| {"TODO"}

NumericRangeType : [
    ExcludesEndValue, # ..<
    IncludesEndValue, # ..=
]

TokenContents : [
    NewlineToken,
    OpenBracketToken, # (
    CloseBracketToken, # )
    OpenSquareBracketToken, # [
    CloseSquareBracketToken, # ]
    OpenBraceToken, # {
    CloseBraceToken, # }
    CommaToken, # ,
    ColonToken, # :
    AssignToken, # =
    EqualToken, # ==
    MinusToken, # -
    ArrowToken, # ->
    DotDotToken(NumericRangeType),
    DotToken, # .
    ModuloToken, # %
    UIntDigitsToken(U64),
    IntDigitsToken(I64),
    FloatDigitsToken(F64),
    StrDigitsToken(Str),
    ForToken,
    InToken,
    IfToken,
    ElseToken,
    DoToken,
    WhileToken,
    MutToken,
    IdentToken(Str),
    CommentToken(Str),
    StringToken(Str),
    EndOfFileToken,
]
Token : (TokenContents, U64)

tokenize_identifier : List(U8), U64, List(U8), U64 -> TokenizerResult
tokenize_identifier = |file, index, acc, start_index| {
    char = List.get(file, index)
    match char {
        Ok(c) => {
            if ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or ('0' <= c and c <= '9') or (c == '_') or (c == '!') or (c == '$') {
                return tokenize_identifier(file, index+1, List.append(acc, c), start_index)
            }
        }
        _ => {}
    }
    match Str.from_utf8(acc) {
        Ok(str) => {
            token : TokenContents
            token = match str {
                "for" => ForToken
                "in" => InToken
                "if" => IfToken
                "else" => ElseToken
                "while" => WhileToken
                "do" => DoToken
                "mut" => MutToken
                _ => IdentToken(str)
            }
            (Ok(token), start_index, index)
        }
        Err(BadUtf8({problem, index: i})) => {
            (Err(BadUtf8(problem)), i, index)
        }
    }
}

TokenizerError : [
    UnexpectedCharacter(U8),
    UnexpectedEndOfFile,
    ExpectedDigitButGot(U8),
    ExpectedDigitButGotEndOfFile,
    BadUtf8([
        InvalidStartByte,
	    UnexpectedEndOfSequence,
	    ExpectedContinuation,
	    OverlongEncoding,
	    CodepointTooLarge,
	    EncodesSurrogateHalf,
    ]),
]

tokenize_digits : List(U8), U64, List(U8) -> (List(U8), U64)
tokenize_digits = |file, index, digits| {
    match List.get(file, index) {
        Ok(c) => {
            if '0' <= c and c <= '9' {
                return tokenize_digits(file, index + 1, List.append(digits, c))
            } else if c == '_' {
                return tokenize_digits(file, index + 1, digits)
            }
        }
        _ => {}
    }
    (digits, index)
}

TokenizerResult : (
    Try(TokenContents, TokenizerError),
    U64, # Index of start of token/error
    U64, # New index in file
)
get_next_token : List(U8), U64 -> TokenizerResult
get_next_token = |file, index| {
    out = match List.get(file, index) {
        Ok(' ') => get_next_token(file, index + 1)
        Ok('\n') => (Ok(NewlineToken), index, index + 1)
        Ok('(') => (Ok(OpenBracketToken), index, index + 1)
        Ok(')') => (Ok(CloseBracketToken), index, index + 1)
        Ok('{') => (Ok(OpenBraceToken), index, index + 1)
        Ok('}') => (Ok(CloseBraceToken), index, index + 1)
        Ok('[') => (Ok(OpenSquareBracketToken), index, index + 1)
        Ok(']') => (Ok(CloseSquareBracketToken), index, index + 1)
        Ok(',') => (Ok(CommaToken), index, index + 1)
        Ok(':') => (Ok(ColonToken), index, index + 1)
        Ok('%') => (Ok(ModuloToken), index, index + 1)
        Ok('=') => {
            match List.get(file, index+1) {
                Ok('=') => (Ok(EqualToken), index, index + 2)
                _ => (Ok(AssignToken), index, index + 1)
            }
        }
        Ok('-') => {
            match List.get(file, index+1) {
                Ok('>') => (Ok(ArrowToken), index, index + 2)
                _ => (Ok(MinusToken), index, index + 1)
            }
        }
        Ok('.') => {
            match List.get(file, index+1) {
                Ok('.') => match List.get(file, index+2) {
                    Ok('<') => {
                        (Ok(DotDotToken(ExcludesEndValue)), index, index + 3)
                    }
                    Ok('=') => {
                        (Ok(DotDotToken(IncludesEndValue)), index, index + 3)
                    }
                    # TODO: Improve these errors
                    Ok(c) => (Err(UnexpectedCharacter(c)), index+2, index+2)
                    Err(_) => (Err(UnexpectedEndOfFile), index+2, index+2)
                }
                _ => (Ok(DotToken), index, index + 1)
            }
        }
        Ok(c) => {
            if ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or (c == '_') {
                tokenize_identifier(file, index+1, [c], index)
            } else if ('0' <= c and c <= '9') {
                # TODO: Add support for negatives and decimals
                (digits, index2) = tokenize_digits(file, index + 1, [c])
                str = Str.from_utf8_lossy(digits)
                token : TokenContents
                token = match U64.from_str(str) {
                    Ok(num) => UIntDigitsToken(num)
                    Err(_) => StrDigitsToken(str)
                }
                (Ok(token), index, index2)
            } else {
                (Err(UnexpectedCharacter(c)), index, index + 1)
            }
        }
        Err(_) => {
            (Ok(EndOfFileToken), index, index)
        }
    }
    # match out {
        # (Ok(token), _, _) => {Stdout.line!(Str.inspect(token))}
    # }
    # Stdout.line!(Str.inspect(out))
    out
}

format_error : List(U8), Try(TokenContents, TokenizerError), U64, List(Str) -> Str
format_error = |code, result, index, possibilities| {
    (line, col) = code.sublist({start: 0, len: index}).fold((1.U64, 1.U64), |(l, c), char| {
        if char == '\n' {
            (l+1, 1)
        } else {
            (l, c+1)
        }
    })
    got = match result {
        Ok(token) => Str.inspect(token)
        Err(UnexpectedCharacter(c)) => "the unexpected character `${Str.from_utf8_lossy([c])}`"
        Err(error) => "the tokenization error ${Str.inspect(error)}"
    }
    expected = match possibilities {
        [one] => ": ${one}"
        _ => {
            expected_str = possibilities.map(|e| "- ${e}")->Str.join_with("\n")
            " either:\n${expected_str}"
        }
    }
    "At line ${line.to_str()} and column ${col.to_str()}:\nExpected${expected}\nGot: ${got}"
}

# Comment : { text : Str, position : U64 }
# Match : {
#     value : Value,
#     branches : List {}, # TODO
# }
# Loop : {
#     captures : List Str,
#     in : Value,
#     block : List Statement,
# }
#
Maybe(t) : [
    Some(t),
    None,
]
#
ValueDestination : [
    # TODO: Add pattern matching
    Mutation((Str, Maybe(Type))),
    Definition((Str, Maybe(Type))), # infer if it is a constant or mutable definition from whether there is an underscore at the end of the variable name
]

Operation : [
    SetTo,
    IncrementBy,
    DecrementBy,
    MultiplyBy,
    DivideBy,
]
FuncValue : {
    arguments: List((Str, Type)),
    body: List(Statement),
    return_values: List((
        Str, # May be a blank string for unnamed return values
        Type,
    )),
}
FuncCall : {
    name: Str,
    args: List(Value), # TODO: Add named arguments
}

# Operations with higher prioraty (prioraty 3 is the highest prioraty) are executed first
# See https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
get_prioraty : ValueCombinationMethod -> U64
get_prioraty = |method| {
  match method {
    BooleanAnd | BooleanOr | BooleanNot => 0
    IsEqual | IsNotEqual | IsGreaterThan | IsLessThan | IsGreaterThanOrEqual | IsLessThanOrEqual => 1
    Multiply | Divide | Modulo => 2
    Add | Subtract => 3
  }
}

ValueCombinationMethod := [
    BooleanAnd, BooleanOr, BooleanNot,
    IsEqual, IsNotEqual, IsGreaterThan, IsLessThan, IsGreaterThanOrEqual, IsLessThanOrEqual,
    Multiply, Divide, Modulo,
    Add, Subtract,
]

Value := [
    FunctionCall(FuncCall),
    FunctionValue(FuncValue),
    VariableReference(Str),
    UInt(U64),
    Int(I64),
    Float(F64),
    CombinedValue({
        combination_method: ValueCombinationMethod,
        value1: Value,
        value2: Value,
    }),
]
Iterator : [
    NumericRange({
        start: Value,
        end: Value,
        type: NumericRangeType,
    }),
    IterableValue(Value),
]
Statement := [
    FuncCall(FuncCall),
    ForLoop({
        identifiers: List(Str),
        iterator: Iterator,
        reversed: Bool, # TODO
        block: List(Statement),
    }),
    IfStatement({
      # TODO: Add else
      condition: Value,
      block: List(Statement),
    }),
#    VariableStatement({ dest : ValueDestination, op : Operation, value : Value }),
]

Type := [
    Name(Str),
    Array((
        U64, # Length, 0 means that the length is dynamic
        Type, # The type of the elements in the array
    )),
    SumType(List((Str, Type))),
    StructType(List((Str, Type, Value))),
]
# ValueDefinition(possibleValues) : { name : Str, value : possibleValues, type : Type, position : U64 }
TopLevel : [
    Import({ name : Str, position : U64 }),
    ValueDefinition({name: Str, position: U64, value: FuncValue}),
    TypeDefinition({name: Str, type: Type, position: U64}),
]

parse_type : List(U8), U64 -> Try((Type, U64), Str)
parse_type = |file, var $index| {
    (token, token_pos, $index) = get_next_token(file, $index)
    Stdout.line!("matching on first token")
    match token {
        Ok(IdentToken(type)) => {
            Stdout.line!("ident type")
            Ok((Name(type), $index))
        }
        Ok(OpenSquareBracketToken) => {
            Stdout.line!("list type")
            (token2, token2_pos, $index) = get_next_token(file, $index)
            Stdout.line!("matching on list type second token")
            error = Err(format_error(file, token2, token2_pos, ["]", "a number that is greater than 0"]))
            length = match token2 {
                Ok(CloseSquareBracketToken) => 0
                Ok(UIntDigitsToken(size)) => {
                    if size <= 0 { return error }
                    (token3, token3_pos, $index) = get_next_token(file, $index)
                    match token3 {
                        Ok(CloseSquareBracketToken) => size
                        _ => {
                            return Err(format_error(file, token3, token3_pos, ["]"]))
                        }
                    }
                }
                _ => { return error }
            }
            Stdout.line!("parsing list element type")
            (elem_type, $index) = parse_type(file, $index)?
            Stdout.line!("returning")
            Ok((Array((length, elem_type)), $index))
        }
        _ => {
            Err(format_error(file, token, token_pos, ["a type"]))
        }
    }
}

parse_first_value : List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)
parse_first_value = |file, (token, token_pos, var $index), possibilities| {
    Stdout.line!("parsing first value")
    out = match token {
        Ok(IdentToken(name)) => Ok((VariableReference(name), $index))
        Ok(UIntDigitsToken(x)) => Ok((UInt(x), $index))
        Ok(IntDigitsToken(x)) => Ok((Int(x), $index))
        Ok(FloatDigitsToken(x)) => Ok((Float(x), $index))
        # TODO
        _ => {
            Err(format_error(file, token, token_pos, List.concat(possibilities, [
                "An identifier for a variable to be referenced",
                "A UIntDigitsToken",
                "An IntDigitsToken",
                "A FloatDigitsToken",
            ])))
        }
    }
    Stdout.line!("finished parsing first value")
    out
}

parse_value : List(U8), TokenizerResult, List(Str) -> Try((Try(TokenContents, TokenizerError), U64, List(Str), Value, U64), Str)
parse_value = |file, result, possibilities| {
    (first_value, var $index) = parse_first_value(file, result, possibilities)?
    (token, token_pos, $index) = get_next_token(file, $index)
    (value1, token2, token2_pos, expected) = match (first_value, token) {
      (VariableReference(name), Ok(OpenBracketToken)) => {
        # TODO
        (t2, t2_pos, $index) = get_next_token(file, $index)
        (FunctionCall({name, args: []}), t2, t2_pos, [])
      }
      (VariableReference(name), _) => (first_value, token, token_pos, ["`(` to call a function called `${name}`"])
      _ => (first_value, token, token_pos, [])
    }
    combination_method1 : ValueCombinationMethod
    combination_method1 = match token2 {
        Ok(ModuloToken) => Modulo
        # TODO
        _ => {
          return Ok((token2, token2_pos, expected.concat(["`%`"]), value1, $index))
        }
    }
    (token3, token3_pos, expected2, value2, $index) = parse_value(file, get_next_token(file, $index), [])?
    value = match value2 {
        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
            # TODO: Uncomment this if statement when https://github.com/roc-lang/roc/issues/8775 is fixed
          #if get_prioraty(combination_method1) < get_prioraty(combination_method2) {
                # Combination method 2 is ran first
                CombinedValue({combination_method: combination_method1, value1, value2})
           #        } else {
                 # Combination method 1 is ran first
            #     CombinedValue({
            #         combination_method: combination_method2,
            #         value1: CombinedValue({
            #             combination_method: combination_method1,
            #             value1: value1,
            #             value2: value2A,
            #         }),
            #         value2: value2B,
            #     })
            # }
        }
        _ => CombinedValue({combination_method: combination_method1, value1, value2})
    }
    Ok((token3, token3_pos, expected2, value, $index))
}


parse_function_call_args = |file, var $index, acc| {
    (token, token_pos, $index) = get_next_token(file, $index)
    match token {
        Ok(CloseBracketToken) => Ok((acc, $index))
        _ => {
            (token2, token2_pos, expected, value, $index) = parse_value(file, (token, token_pos, $index), ["A close bracket token"])?
            match token2 {
                Ok(CommaToken) => parse_function_call_args(file, $index, List.append(acc, value))
                Ok(CloseBracketToken) => Ok((acc, $index))
                _ => Err(format_error(file, token2, token2_pos, expected.concat(["`,`", "`)`"])))
            }
        }
    }
}

parse_ident_list = |file, var $index, acc| {
    (token, token_pos, $index) = get_next_token(file, $index)
    match token {
        Ok(IdentToken(name)) => {
            (token2, token2_pos, $index) = get_next_token(file, $index)
            match token2 {
                Ok(CommaToken) => parse_ident_list(file, $index, List.append(acc, name))
                _ => Ok((token2, token2_pos, List.append(acc, name), $index))
            }
        }
        _ => Err(format_error(file, token, token_pos, ["an identifier (hint: trailing commas are not allowed here)"]))
    }
}

parse_block : List(U8), U64, List(Statement) -> Try((List(Statement), U64), Str)
parse_block = |file, var $index, acc| {
    (token, token_pos, $index) = get_next_token(file, $index)
    match token {
        Ok(NewlineToken) => {
            parse_block(file, $index, acc)
        }
        Ok(IfToken) => {
            (token2, token2_pos, expected, condition, $index) = parse_value(file, get_next_token(file, $index), [])?
            match token2 {
              Ok(OpenBraceToken) => {}
              _ => {
                return Err(format_error(file, token2, token2_pos, expected.append("`{`")))
              }
            }
            (block, $index) = parse_block(file, $index, [])?
            # TODO: Add else
            parse_block(file, $index, List.append(acc, IfStatement({condition, block})))
        }
        Ok(ForToken) => {
            # TODO: Add support for reversed for loops
            (token2, token2_pos, identifiers, $index) = parse_ident_list(file, $index, [])?
            match token2 {
                Ok(InToken) => {}
                _ => {
                    return Err(format_error(file, token2, token2_pos, ["`in`", "`,` to add a new identifier"]))
                }
            }
            (token3, token3_pos, expected, value, $index) = parse_value(file, get_next_token(file, $index), [])?
            iterator : Iterator
            iterator = match token3 {
                Ok(DotDotToken(type)) => {
                    (token4, token4_pos, expected2, value2, $index) = parse_value(file, get_next_token(file, $index), [])?
                    match token4 {
                        Ok(OpenBraceToken) => {}
                        _ => {
                            return Err(format_error(file, token4, token4_pos, expected2.append("`{` to start the body of the for loop")))
                        }
                    }
                    NumericRange({start: value, end: value2, type})
                }
                Ok(OpenBraceToken) => {
                    IterableValue(value)
                }
                _ => {
                    return Err(format_error(file, token3, token3_pos, expected.concat(["`..<` or `..=` to create a numeric range iterator", "`{` to use the previous value as the iterator"])))
                }
            }
            (block, $index) = parse_block(file, $index, [])?
            parse_block(file, $index, List.append(acc, ForLoop({identifiers, iterator, block, reversed: False})))
        }
        Ok(IdentToken(name)) => {
            (token2, token2_pos, $index) = get_next_token(file, $index)
            match token2 {
                Ok(OpenBracketToken) => {
                    (args, $index) = parse_function_call_args(file, $index, [])?
                    parse_block(file, $index, List.append(acc, FuncCall({name, args})))
                }
                Ok(CommaToken) => {Err("TODO (in parse_block)")}
                Ok(AssignToken) => {
                    # TODO: Parse variable assignment
                    Err("TODO (in parse_block)")
                    # parse_block(file, index3, acc)
                }
                _ => Err(format_error(file, token2, token2_pos, ["`(` to call a function", "`,`", "`=`"]))
            }
        }
        Ok(CloseBraceToken) => Ok((acc, $index))
        _ => Err(format_error(file, token, token_pos, ["`if`", "`for`", "a newline", "An identifier", "}"]))
    }
}

parse_function_def_args : List(U8), U64, List((Str, Type)) -> Try((List((Str, Type)), U64), Str)
parse_function_def_args = |file, var $index, acc| {
    (token, token_pos, $index) = get_next_token(file, $index)
    match token {
        Ok(IdentToken(name)) => {
            (token2, token2_pos, $index) = get_next_token(file, $index)
            match token2 {
                Ok(ColonToken) => {
                    Stdout.line!("parsing type")
                    (type, $index) = parse_type(file, $index)?
                    Stdout.line!("finished parsing type")
                    parse_function_def_args(file, $index, List.append(acc, (name, type)))
                }
                _ => Err(format_error(file, token2, token2_pos, ["`:` to set the type of the `${name}` argument"]))
            }
        }
        Ok(CloseBracketToken) => Ok((acc, $index))
        _ => Err(format_error(file, token, token_pos, ["an identifier for one of the functions arguments", "`)` to finish the function argument section"]))
    }
}

parse_function_definition : List(U8), U64 -> Try((FuncValue, U64), Str)
parse_function_definition = |file, var $index| {
    Stdout.line!("parsing function def")
    (token, token_pos, $index) = get_next_token(file, $index)
    match token {
        Ok(OpenBracketToken) => {}
        _ => Err(format_error(file, token, token_pos, ["`(` to start a function definition"]))?
    }
    Stdout.line!("parsing function def args")
    (args, $index) = parse_function_def_args(file, $index, [])?
    Stdout.line!("getting next token")
    (token2, token2_pos, $index) = get_next_token(file, $index)
    Stdout.line!("parsing function return values")
    return_values : List((Str, Type))
    return_values = match token2 {
        Ok(OpenBraceToken) => []
        Ok(OpenBracketToken) => {
            # TODO: Parse multiple return values
            []
        }
        Ok(ArrowToken) => {
            (type, $index) = parse_type(file, $index)?
            (token3, token3_pos, $index) = get_next_token(file, $index)
            match token3 {
                Ok(OpenBraceToken) => [("", type)]
                _ => {
                    return Err(format_error(file, token3, token3_pos, ["`{` to start a function block"]))
                }
            }
        }
        _ => {
            return Err(format_error(file, token2, token2_pos, ["`{` to start a function block", "`->` to set the return type of the function"]))
        }
    }
    (body, $index) = parse_block(file, $index, [])?
    val: FuncValue
    val = { arguments: args, body: body, return_values }
    Ok((val, $index))
}

parse! : List(U8), U64, List(TopLevel) => Try(List(TopLevel), Str)
parse! = |file, var $index, acc| {
    (token, pos, $index) = get_next_token(file, $index)
    match token {
        Ok(IdentToken(name)) => {
            Stderr.line!("ident")
            (token2, token2_pos, $index) = get_next_token(file, $index)
            match token2 {
                Ok(ColonToken) => {
                    Stderr.line!(":")
                    (type, $index) = parse_type(file, $index)?
                    parse!(file, $index, List.append(acc, TypeDefinition({ name: name, type: type, position: pos })))
                }
                Ok(AssignToken) => {
                    Stderr.line!("= at index ${$index.to_str()}")
                    (def, $index) = parse_function_definition(file, $index)?
                    Stderr.line!("finished =")
                    parse!(file, $index, List.append(acc, ValueDefinition({ name: name, value: def, position: pos })))
                }
                _ => {
                    Err(format_error(file, token2, token2_pos, ["`:`", "`=`"]))
                }
            }
        }
        Ok(EndOfFileToken) => {
            Stderr.line!("End of file")
            Ok(acc)
        }
        _ => {
            Stdout.line!("expected")
            Err(format_error(file, token, pos, ["a top level definition"]))
        }
    }
}
