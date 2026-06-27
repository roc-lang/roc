# repro for https://github.com/roc-lang/roc/issues/7348
# Optimized builds that omit inline expects must not run the expect condition.
main! = |_args| {
    expect {
        crash "inline expect condition was evaluated"
    }
    echo!("Hello, World!")
    Ok({})
}
