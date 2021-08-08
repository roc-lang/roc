interface MiniParsec exposes [ result, testResult, showPair] imports []

showPair : [Pair Str Str] -> Str
showPair = \(Pair a b) -> Str.concat (Str.concat a "::") b


# result: succeed without consuming input
#
# » ((\v -> (\inp -> [Pair v inp])) "a") "xyz"
# [ Pair "a" "xyz" ] : List [ Pair Str Str ]*
#
result : a -> (b -> List [ Pair a b ]*)
result = \v -> (\inp -> [Pair v inp])


testResult =  \a, input -> 
    when ((result a) input |> List.first) is
                        Ok pair -> showPair pair
                        _ -> "Error"



