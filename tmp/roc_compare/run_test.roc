main! = |_args| {
    lifted = 0.I32.to(4).stream().collect!()
    mapped = 0.I32.to(4).stream().map(|i| i + 1).collect!()
    pure = 0.I32.to(4).collect()
    Echo.line!("lift:   ${Str.inspect(lifted)}")
    Echo.line!("mapped: ${Str.inspect(mapped)}")
    Echo.line!("pure:   ${Str.inspect(pure)}")
    Ok({})
}
