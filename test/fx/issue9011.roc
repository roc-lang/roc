app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Issue 9011: Panics with "cast causes pointer to be null" or "integer overflow"
# when using recursive functions with mutable variables and tagged unions.

TokenContents : [
    EqualToken,
    ModuloToken,
    UIntDigitsToken(U64),
    IdentToken(Str),
    EndOfFileToken,
]

TokenizerResult : (
    TokenContents,
    U64,
)

get_next_token! : U64 => TokenizerResult
get_next_token! = |index| {
    tokens : List(TokenContents)
    tokens = [IdentToken("i"), ModuloToken, UIntDigitsToken(15), EqualToken, UIntDigitsToken(0)]
    match List.get(tokens, index) {
        Ok(value) => (value, index+1)
        Err(_) => (EndOfFileToken, index)
    }
}

ValueCombinationMethod := [
    BooleanAnd, BooleanOr, BooleanNot,
    IsEqual, IsNotEqual, IsGreaterThan, IsLessThan, IsGreaterThanOrEqual, IsLessThanOrEqual,
    Multiply, Divide, Modulo,
    Add, Subtract,
]

Value := [
    UInt(U64),
    CombinedValue({
        combination_method: ValueCombinationMethod,
    }),
]

parse_value! : TokenizerResult => Try((TokenContents, Value, U64), Str)
parse_value! = |(_, var $index)| {
    Stdout.line!("parse_value! called")
    (token2, $index) = get_next_token!($index)
    combination_method1 : ValueCombinationMethod
    combination_method1 = match token2 {
        ModuloToken => Modulo
        _ => {
            Stdout.line!("Returning early from parse_value!")
            return Ok((token2, UInt(3), $index))
        }
    }
    Stdout.line!("finished creating combination method 1 in parse_value!")
    (token3, _value2, $index) = parse_value!(get_next_token!($index))?
    Stdout.line!("finished calling parse_value!")
    value : Value
    value = CombinedValue({combination_method: combination_method1})
    Ok((token3, value, $index))
}

main! = || {
    parsed = parse_value!(get_next_token!(0))
    Stdout.line!("parsed: ${Str.inspect(parsed)}")
}
