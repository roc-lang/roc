interface Variable
    exposes [Variable, fromUtf8, toIndex, totalCount, toStr]
    imports []

# Variables in False can only be single letters. Thus, the valid variables are "a" to "z".
# This opaque type deals with ensure we always have valid variables.
Variable := U8

totalCount : Nat
totalCount =
    0x7A # "z"
    - 0x61 # "a"
    + 1

toStr : Variable -> Str
toStr = \@Variable char ->
    when Str.fromUtf8 [char] is
        Ok str -> str
        _ -> "_"

fromUtf8 : U8 -> Result Variable [InvalidVariableUtf8]
fromUtf8 = \char ->
    if
        char
        >= 0x61 # "a"
        && char
        <= 0x7A # "z"
    then
        Ok (@Variable char)
    else
        Err InvalidVariableUtf8

toIndex : Variable -> Nat
toIndex = \@Variable char ->
    Num.intCast (char - 0x61) # "a"
# List.first (Str.toUtf8 "a")
