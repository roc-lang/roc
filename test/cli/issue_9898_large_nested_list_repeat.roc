# repro for https://github.com/roc-lang/roc/issues/9898
# A large nested repeated list should lower and run without a compiler panic;
# its 676,000 elements are ten times what a u16 count can hold.
m = List.repeat(List.repeat(1, 2600), 260)

main! = |args| {
    index = List.len(args)
    _ = List.get(m, index)?

    Ok({})
}
