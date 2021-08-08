interface MiniParsec exposes [ result, test1, test2] imports []

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
