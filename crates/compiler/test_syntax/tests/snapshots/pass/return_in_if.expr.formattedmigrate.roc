maybe_early_return = \x ->
    y =
        if x > 5 then
            return "abc"
        else
            x + 2

    Num.to_str y

maybe_early_return 10