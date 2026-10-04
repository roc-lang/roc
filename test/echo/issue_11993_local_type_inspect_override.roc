# repro for https://github.com/roc-lang/roc/issues/11993
#
# A `to_inspect` of a type declared in a function body is an inspect override
# when it is promoted to a procedure, wherever the value is inspected. One that
# captures a local stays a local procedure: inspection silently renders the
# default form, and it remains an ordinary method that explicit calls reach.

show = |x| Str.inspect(x)

main! = |args| {
	Plain := { count : U64 }.{
		to_inspect : Plain -> Str
		to_inspect = |p| "Plain(${p.count.to_str()})"
	}
	p = Plain.{ count: args.len() }
	echo!("${Str.inspect(p)} ${Str.inspect({ inner: p })} ${show(p)}\n")

	offset = 1
	Shifted := { count : U64 }.{
		to_inspect : Shifted -> Str
		to_inspect = |s| "Shifted(${(s.count + offset).to_str()})"
	}
	s = Shifted.{ count: args.len() }
	echo!("${Str.inspect(s)} ${show(s)} ${s.to_inspect()}\n")
	Ok({})
}
