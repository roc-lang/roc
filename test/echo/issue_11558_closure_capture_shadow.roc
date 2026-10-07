# repro for https://github.com/roc-lang/roc/issues/11558
#
# The lambda passed to `check` needs a descriptor for `always`'s own type
# variable `a`, which it captures from `always`'s frame. Every backend,
# including `--specialize=no`, prints the same result.
check : a, (a -> Bool) -> Bool
check = |x, pred| pred(x)

always : a -> Bool
always = |x| check(x, |_| True)

main! = |_args| {
    echo!("${Str.inspect(always("hi"))}\n")
    Ok({})
}
