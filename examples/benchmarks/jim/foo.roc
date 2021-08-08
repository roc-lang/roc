interface MiniParsec exposes [ result, test1, idI64] imports []

result : a -> (b -> List [ Pair a b ]*)
result = \v -> (\inp -> [Pair v inp])

test1 : Nat -> Nat
test1 = \n -> (result "a") n  |> List.len

idI64 : I64 -> I64
idI64 = \n -> n



