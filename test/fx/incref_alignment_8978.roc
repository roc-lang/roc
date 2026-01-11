app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result = parse_value!(get_next_token!(0))
    Stdout.line!("parsed: ${Str.inspect(result)}")
}

TokenContents : [
    EqualToken, # ==
    UIntDigitsToken(U64),
    ModuloToken,
    IdentToken(Str),
    EndOfFileToken,
]

TokenizerResult : (
    TokenContents,
    U64, # New index in file
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
        value1: Value,
        value2: Value,
    }),
]

parse_first_value! : () => Value
parse_first_value! = || {
    UInt(3)
}

parse_value! : TokenizerResult => Try((TokenContents, List(Str), Value, U64), Str)
parse_value! = |(_, var $index)| {
    value1 = parse_first_value!()
    (token2, $index) = get_next_token!($index)
    combination_method1 : ValueCombinationMethod
    combination_method1 = match token2 {
         ModuloToken => Modulo
         _ => {
            return Ok((token2, ["`%`"], value1, $index))
        }
    }
    (token3, expected2, value2, $index) = parse_value!(get_next_token!($index))?
    value : Value
    value = CombinedValue({combination_method: combination_method1, value1, value2})
    Ok((token3, expected2, value, $index))
}
