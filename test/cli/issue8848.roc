# Regression test for https://github.com/roc-lang/roc/issues/8848
# The bug causes a panic "trying to add var at rank 2, but current rank is 1"
# during generalization when using mutable variables ($var) that are
# reassigned in nested scopes (like inside match branches).

ValueCombinationMethod := [
  Modulo,
]

Value := [
    UInt(U64),
    CombinedValue({
        combination_method: ValueCombinationMethod,
        value1: Value,
        value2: Value,
    }),
]

TokenContents : []

TokenizerResult : (
    Try(TokenContents, Str),
    U64, # Index of start of token/error
    U64, # New index in file
)
get_next_token : List(U8), U64 -> TokenizerResult
get_next_token = |file, index| {
    (Err("todo"), 0, 0)
}

parse_value : List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)
parse_value = |file, result, possibilities| {
    (first_value, var $index) = parse_first_value(file, result, possibilities)?
    (token, token_pos, $index) = get_next_token(file, $index)
    (value1, token2, token2_pos) = match (first_value, token) {
      (VariableReference(name), Ok(OpenBracketToken)) => {
        # This reassignment of $index inside the match branch triggers the bug
        (t2, t2_pos, $index) = get_next_token(file, $index)
        (FunctionCall({name, args: []}), t2, t2_pos)
      }
      _ => (first_value, token, token_pos)
    }
    combination_method1 : ValueCombinationMethod
    combination_method1 = Modulo
    (value2, $index) = parse_value(file, get_next_token(file, $index), [])
    value : Value
    value = match value2 {
        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
            CombinedValue({
                combination_method: combination_method2,
                value1: CombinedValue({
                    combination_method: combination_method1,
                    value1: value1,
                    value2: value2A,
                }),
                value2: value2B,
            })
        }
        _ => CombinedValue({combination_method: combination_method1, value1, value2})
    }
    Ok((value, $index))
}
