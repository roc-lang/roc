# repro for https://github.com/roc-lang/roc/issues/10492: built default-platform binaries pass process args to main!
main! = |raw_args| {
    for arg in raw_args {
        echo!("[${arg}]")
    }
    Ok({})
}
