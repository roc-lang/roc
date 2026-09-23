CallableHelper := [].{
    make_renderer : Str -> (Str -> Str)
    make_renderer = |prefix| {
        |suffix| "${prefix}${suffix}"
    }

    render : Str -> Str
    render = make_renderer("an imported compile-time capture:")
}

expect CallableHelper.render("helper") == "an imported compile-time capture:helper"
