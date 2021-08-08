interface MiniParsec exposes [ result, test1, test2, showPair] imports []

# » \(Pair a b) -> Str.concat (Str.concat a "::") b
# <function> : [ Pair Str Str ]* -> Str
#
# » (\(Pair a b) -> Str.concat (Str.concat a "::") b) (Pair "x" "y")
# "x::y" : Str
# 

showPair : [Pair Str Str] -> Str
showPair = \(Pair a b) -> Str.concat (Str.concat a "::") b


# result: succeed without consuming input
#
# » ((\v -> (\inp -> [Pair v inp])) "a") "xyz"
# [ Pair "a" "xyz" ] : List [ Pair Str Str ]*
#
result : a -> (b -> List [ Pair a b ]*)
result = \v -> (\inp -> [Pair v inp])

test1 : I64 -> Str
test1 = \n -> 
    (result "a") n |> List.len |> Str.fromInt

test2 : I64 -> Str
test2 = \n -> 
    when ((result "a") n |> List.len) == 1 is
        True -> "Ok"
        False -> "Error!"


