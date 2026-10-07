# repro for https://github.com/roc-lang/roc/issues/11993
#
# A `to_inspect` of a type declared in a function body is an inspect override
# wherever the value is inspected.

show = |x| Str.inspect(x)

main! = |args| {
	Plain := { count : U64 }.{
		to_inspect : Plain -> Str
		to_inspect = |p| "Plain(${p.count.to_str()})"
	}
	p = Plain.{ count: args.len() }
	echo!("${Str.inspect(p)} ${Str.inspect({ inner: p })} ${show(p)}\n")
	Ok({})
}
