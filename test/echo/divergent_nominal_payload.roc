# A nominal construction whose payload diverges evaluates that payload, which
# crashes; the construction never produces a value.
wrap : Str -> Try(Str, [Bad])
wrap = |message| Ok({ crash message })

main! = |args| {
	echo!("started")
	echo!(Str.inspect(wrap("nominal payload diverged after ${args.len().to_str()} args")))
	Ok({})
}
