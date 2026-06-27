# repro for https://github.com/roc-lang/roc/issues/7348
# Optimized builds that omit inline expects must not run the expect condition.
main! = |_args| {
    expect {
        dbg "omitted dbg output"
        crash "inline expect condition was evaluated"
    }
    _omitted = {
        expect {
            dbg "omitted final-expression dbg output"
            crash "inline expect final expression was evaluated"
        }
    }
    echo!("Hello, World!")
    Ok({})
}
