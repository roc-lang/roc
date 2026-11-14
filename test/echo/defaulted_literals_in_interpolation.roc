# Two number literals default inside `main!`: the copy of `timeout`'s and the
# ones `average` is called with. Each one's `to_str` requirement names the
# interpolation's part type, which is `Str` because `echo!` takes a `Str`. A
# default cannot narrow a type that is already settled, so neither default
# changes `main!`'s type and neither is worth a warning.
timeout = 30

average = |a, b| (a + b) / 2

main! = |_args| {
    echo!("${timeout.to_str()} ${average(1.5, 2.5).to_str()}")
    Ok({})
}
