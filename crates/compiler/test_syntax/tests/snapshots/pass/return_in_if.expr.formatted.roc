maybeEarlyReturn = \x ->
    y =
        if x > 5 then
            return "abc"
        else
            x + 2

    Num.toStr y

maybeEarlyReturn 10