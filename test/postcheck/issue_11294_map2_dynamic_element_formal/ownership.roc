app [main!] { pf: platform "./platform/main.roc" }

combine_nested : List(List(Str)), List(Str) -> List(List(Str))
combine_nested = |left, right| left.map2(right, |a, b| a.append(b))

combine_strings : List(Str), List(Str) -> List(Str)
combine_strings = |left, right| left.map2(right, |a, b| a.concat(b))

combine_numbers : List(I64), List(I64) -> List(I64)
combine_numbers = |left, right| left.map2(right, |a, b| a + b)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    first = "first string deliberately longer than inline storage"
    second = "second string deliberately longer than inline storage"
    nested_ok = combine_nested([[first], [second]], [second, first, second]) == [[first, second], [second, first]]
    strings_ok = combine_strings([first], [second]) == [first.concat(second)]
    numbers_ok = combine_numbers([1, 2], [10, 20]) == [11, 22]
    empty_ok = combine_strings([], [first]) == []
    if nested_ok and strings_ok and numbers_ok and empty_ok Ok({}) else Err(Exit(1))
}
