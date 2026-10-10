Functions := [].{
    even : U64 -> Bool
    even = |n| if n == 0.U64 Bool.True else Functions.odd(n - 1.U64)

    odd : U64 -> Bool
    odd = |n| if n == 0.U64 Bool.False else Functions.even(n - 1.U64)

    result : Bool
    result = Functions.even(4.U64)
}

expect Functions.result == Bool.True
