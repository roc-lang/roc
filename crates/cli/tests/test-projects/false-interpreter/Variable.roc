module [Variable, from_utf8, to_index, total_count, to_str]

# Variables in False can only be single letters. Thus, the valid variables are "a" to "z".
# This opaque type deals with ensure we always have valid variables.
Variable := U8

total_count : U64
total_count =
    0x7A # "z"
    - 0x61 # "a"
    + 1

to_str : Variable -> Str
to_str = \@Variable(char) ->
    when Str.from_utf8([char]) is
        Ok(str) -> str
        _ -> "_"

from_utf8 : U8 -> Result Variable [InvalidVariableUtf8]
from_utf8 = \char ->
    if
        char >= 0x61 # "a"
        and char <= 0x7A # "z"
    then
        Ok(@Variable(char))
    else
        Err(InvalidVariableUtf8)

to_index : Variable -> U64
to_index = \@Variable(char) ->
    Num.int_cast((char - 0x61)) # "a"
