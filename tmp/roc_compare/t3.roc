main! = |_args| {
    mapped = 0.I32.to(4).stream().map(|i| i + 1).collect!()
    Echo.line!(Str.inspect(mapped))
    Ok({})
}
