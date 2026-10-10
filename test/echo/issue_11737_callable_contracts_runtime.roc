import Issue11737Db

demo = |db| (db.run(|{}| "a"), db.run(|{}| 42.U64))

forward = |db| demo(db)

saved = { invoke: forward }

main! = |_args| {
	invoke = saved.invoke
	result = invoke(Issue11737Db.new({ fetch: |{}| "x" }))
	echo!("${Str.inspect(result)}\n")
	Ok({})
}
