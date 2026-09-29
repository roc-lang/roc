Runner :: {}.{
	run = |_runner, body| body({}).repeat(3)
}

demo = |runner| (runner.run(|{}| "a"), runner.run(|{}| [1.U64]))

main! = |_args| {
	echo!("${Str.inspect(demo(Runner.{}))}\n")
	Ok({})
}
