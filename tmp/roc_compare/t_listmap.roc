main! = |_args| {
    xs = [1, 2, 3, 4, 5]
    doubled = xs.map(|n| n * 2)
    Echo.line!(Str.inspect(doubled))
    Ok({})
}
