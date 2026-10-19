Runner :: {}.{
	run = |_runner, body| body({}).repeat(3)
}

demo : Runner -> (Str, List(List(U64)))
demo = |runner| (runner.run(|{}| "a"), runner.run(|{}| [1.U64]))

main! = |_args| {
	echo!("${Str.inspect(demo(Runner.{}))}\n")
	Ok({})
}
