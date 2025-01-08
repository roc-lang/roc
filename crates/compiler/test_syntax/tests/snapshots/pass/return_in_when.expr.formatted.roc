maybeEarlyReturn = \x ->
    y =
        when x is
            5 ->
                return
                    "abc"

            _ -> x + 2

    Num.to_str y

maybeEarlyRetun 3