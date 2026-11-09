# A call whose second argument diverges evaluates its first argument, then
# the divergent one, which crashes; the callee is never called.
join : Str, Str -> Str
join = |a, b| Str.concat(a, b)

run : Str -> Str
run = |first| join({
	dbg first
	first
}, { crash "call argument diverged" })

main! = |args| {
	echo!("started")
	echo!(run("first argument of ${args.len().to_str()}"))
	Ok({})
}
