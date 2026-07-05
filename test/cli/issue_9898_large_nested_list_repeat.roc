# repro for https://github.com/roc-lang/roc/issues/9898
# A large nested repeated list should lower and run without a compiler panic.
m = List.repeat(List.repeat(1, 260), 260)

main! = |args| {
    index = List.len(args)
    _ = List.get(m, index)?

    Ok({})
}
