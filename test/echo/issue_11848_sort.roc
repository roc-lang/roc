# repro for https://github.com/roc-lang/roc/issues/11848
#
# Sorting compares through each numeric type's own `compare` low-level and
# calls a comparator through the sort low-level's fixed ABI, whose ordering is
# the closed `[Before, Same, After]`.
sorted_desc = |items| items.sort_reversed()

main! = |_| {
    scores = [90, 72, 85]
    echo!("${Str.inspect(scores.sort())} ${Str.inspect(scores.sort_reversed())} ${Str.inspect(scores.sort_by(|x| x))}\n")
    people = [{ name: "b", age: 30.U8 }, { name: "a", age: 20 }, { name: "c", age: 25 }]
    echo!("${Str.inspect(people.sort_by(|p| p.age).map(|p| p.name))}\n")
    echo!("${Str.inspect([5.I64, -3, 9, 0].sort_with(|a, b| b.order_relative_to(a)))} ${Str.inspect(sorted_desc([1.U16, 3, 2]))}\n")
    echo!("${Str.inspect((1.U8.order_relative_to(2), 2.U8.order_relative_to(2), 3.U8.order_relative_to(2)))}\n")
    Ok({})
}
