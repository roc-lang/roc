# repro for https://github.com/roc-lang/roc/issues/9943
# The second random_value use is poisoned by the ambiguity machinery
# (replaceExprWithRuntimeError). Publication must still publish a type root
# for the runtime-error node so the MISSING METHOD diagnostics are reported
# instead of panicking with "checked expr type root was not published".
RandomState := U64

RandomFormat := [Default]

random_value : RandomState -> (a, RandomState) where [a.decode : RandomState, RandomFormat -> (Try(a, Str), RandomState)]
random_value = |rs| {
    Shape : a
    match Shape.decode(rs, RandomFormat.Default) {
        (Ok(value), new_state) => (value, new_state)
    }
}

main! = |_args| {
    Ok({})
}

expect
    (|state| {
        (a, state1) = random_value(state)
        (b, _) = random_value(state1)
        True
    })(RandomState.(42))
