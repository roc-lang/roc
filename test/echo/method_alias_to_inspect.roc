# `to_inspect` bound to an unannotated generic function (`to_inspect =
# render`) on a top-level type and on a function-body type, and to a helper
# annotated `Ann -> Str`. Each can be used at its type `-> Str`, so each is an
# inspect override wherever the value is inspected: directly, nested in
# records, lists, and tuples, and through generic helpers. Called explicitly,
# and through a generic caller, each is an ordinary method whose body converts
# literals and interpolates other requirements' results.

render_top = |c| "Top(${(c.count + 1).to_str()})"

quote_top = |_c| "top"

Top := { count : U64 }.{
	to_inspect = render_top
	label = quote_top
}

render_ann : Ann -> Str
render_ann = |a| "Ann(${(a.count + 1).to_str()})"

Ann := { count : U64 }.{
	to_inspect = render_ann
}

show = |x| Str.inspect(x)

show_nested = |x| "${show(x)}/${show({ v: x })}/${show([x])}"

call_inspect = |x| x.to_inspect()

call_label = |x| x.label()

main! = |_args| {
	render_local = |c| "Local(${(c.count * 2).to_str()})"
	Local := { count : U64 }.{
		to_inspect = render_local
	}

	t = Top.{ count: 3 }
	l = Local.{ count: 4 }
	echo!("${Str.inspect(t)} ${Str.inspect(l)}\n")
	echo!("${Str.inspect({ a: t, b: l })} ${Str.inspect([t, t])} ${Str.inspect((l, [l]))}\n")
	echo!("${show(t)} ${show(l)}\n")
	echo!("${show_nested(t)} ${show_nested(l)}\n")
	echo!("${t.to_inspect()} ${l.to_inspect()} ${t.label()}\n")
	echo!("${call_inspect(t)} ${call_inspect(l)} ${call_label(t)}\n")
	a = Ann.{ count: 6 }
	echo!("${Str.inspect(a)} ${Str.inspect({ v: [a] })} ${show(a)} ${show_nested(a)} ${a.to_inspect()} ${call_inspect(a)}\n")
	Ok({})
}
