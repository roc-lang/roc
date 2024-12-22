maybe_early_return = \x ->
    y =
        when x is
            5 ->
                return
                    "abc"

            _ -> x + 2

    Num.to_str y

maybe_early_retun 3