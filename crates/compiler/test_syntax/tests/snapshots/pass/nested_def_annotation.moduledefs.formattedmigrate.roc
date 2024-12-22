main =
    wrapped_not_eq : a, a -> Bool
    wrapped_not_eq = \num1, num2 ->
        num1 != num2

    wrapped_not_eq 2 3
