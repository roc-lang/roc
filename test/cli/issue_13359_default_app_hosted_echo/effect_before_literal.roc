f! = |_a| {
    echo!("x")
    4
}

main! = |_args| {
    echo!(Str.inspect(f!(1)))
    Ok({})
}
