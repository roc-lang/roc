main! = |args| {
    # repro for https://github.com/roc-lang/roc/issues/9857
    _ = args.get(0)? * 10

    Ok({})
}
