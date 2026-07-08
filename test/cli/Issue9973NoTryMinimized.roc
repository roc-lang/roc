# minimized shape from https://github.com/roc-lang/roc/issues/9973 with no `?`
# at all: two plain `return Err(..)` with distinct tags plus a trailing
# forward-reference call, inside a nested closure. Panicked identically to the
# `?` form ("trying to add var at rank 3, but current rank is 2") because
# early_return constraints share the same unowned drain as try_operator ones.
main! = |args| {
    run = || {
        if List.len(args) > 99 {
            return Err(FirstError)
        }
        if List.len(args) > 999 {
            return Err(SecondError)
        }
        _ = helper({})
        Ok({})
    }
    run()
}

helper = |x| Ok(x)
