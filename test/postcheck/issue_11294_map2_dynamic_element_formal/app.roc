app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11294
combine : List(I64), List(I64) -> List(I64)
combine = |left, right| left.map2(right, |a, b| a + b)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    left : List(I64)
    left = [1, 2, 3]
    right : List(I64)
    right = [10, 20, 30]

    if combine(left, right) == [11, 22, 33] Ok({}) else Err(Exit(1))
}
