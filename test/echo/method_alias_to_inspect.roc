# `to_inspect` bound to an unannotated generic function (`to_inspect =
# render`), on a top-level type and on function-body types with a
# non-capturing and a capturing helper. The helper's type is not `T -> Str`,
# so it is no inspect override: `Str.inspect` renders the default form
# directly, nested in records, lists, and tuples, and through generic helpers.
# Called explicitly, each is an ordinary method whose body converts literals
# and interpolates other requirements' results. A `to_inspect` bound to a
# helper annotated `Ann -> Str` is an override wherever the value is inspected.

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

main! = |args| {
	render_local = |c| "Local(${(c.count * 2).to_str()})"
	Local := { count : U64 }.{
		to_inspect = render_local
	}

	extra = List.len(args) + 5
	render_cap = |c| "Cap(${(c.count + extra).to_str()})"
	Cap := { count : U64 }.{
		to_inspect = render_cap
	}

	t = Top.{ count: 3 }
	l = Local.{ count: 4 }
	c = Cap.{ count: 1 }
	echo!("${Str.inspect(t)} ${Str.inspect(l)} ${Str.inspect(c)}\n")
	echo!("${Str.inspect({ a: t, b: l, c: c })} ${Str.inspect([t, t])} ${Str.inspect((l, [c]))}\n")
	echo!("${show(t)} ${show(l)} ${show(c)}\n")
	echo!("${show_nested(t)} ${show_nested(l)} ${show_nested(c)}\n")
	echo!("${t.to_inspect()} ${l.to_inspect()} ${c.to_inspect()} ${t.label()}\n")
	echo!("${call_inspect(t)} ${call_inspect(l)} ${call_inspect(c)} ${call_label(t)}\n")
	a = Ann.{ count: 6 }
	echo!("${Str.inspect(a)} ${Str.inspect({ v: [a] })} ${show(a)} ${show_nested(a)} ${a.to_inspect()} ${call_inspect(a)}\n")
	Ok({})
}
