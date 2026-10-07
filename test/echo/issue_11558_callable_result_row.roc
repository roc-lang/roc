# repro for https://github.com/roc-lang/roc/issues/11558
#
# `mk(f)` returns a closure whose result is only known through `.map_err`
# dispatch, and `?` widens that result into `main!`'s error row. Every
# backend, including `--specialize=no`, runs to completion.
mk = |f| {
    show = || f({}).map_err(|_| ShowFailed)
    show
}

main! = |_args| {
    f : {} -> Try({}, [Empty])
    f = |_| Ok({})
    mk(f)()?
    echo!("done\n")
    Ok({})
}
