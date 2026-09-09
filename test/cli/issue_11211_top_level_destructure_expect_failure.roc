# The declaration must be checked even though its binder is unused.
Ok(unused) = {
    expect 1 == 2
    Ok(5.U64)
}

main! = |_args| Ok({})
