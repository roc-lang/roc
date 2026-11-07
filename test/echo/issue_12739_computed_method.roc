# repro for https://github.com/roc-lang/roc/issues/12739
# A top-level method bound to a computed callable (`to_inspect = make()`) is a
# method: compile-time evaluation produces its function value, and explicit
# calls, Str.inspect, ==, where-clause dictionaries, and compile-time expects
# all call that value. A type declared in a function body may bind a method
# to such a top-level value too.
app [main!] {}

make : () -> (Rows -> Str)
make = || |r| "computed ${r.n.to_str()}"

same : () -> (Rows, Rows -> Bool)
same = || |_a, _b| Bool.False

render = make()

make_plain = || |_x| "plain"

plain = make_plain()

plain_other = make_plain()

# A computed callable over a type variable holds the owner's descriptor, which
# holds the method: Boxy's descriptor names the method's entry, not its value.
Other := { k : U8 }.{
	to_inspect = plain_other
}

Rows := { n : U8 }.{
	to_inspect = make()
	is_eq = same()
	shown = render
}

describe : a -> Str where [a.shown : a -> Str]
describe = |x| "<${x.shown()}>"

expect Rows.({ n: 2 }).to_inspect() == "computed 2"
expect describe(Rows.({ n: 3 })) == "<computed 3>"

main! = |_args| {
	x = Rows.({ n: 1 })
	Local := { m : U8 }.{
		to_inspect = plain
	}
	l = Local.({ m: 1 })
	echo!("${Str.inspect(x)} ${x.to_inspect()} ${Str.inspect(x == x)} ${x.shown()} ${describe(x)} ${Str.inspect([x])} ${l.to_inspect()} ${Str.inspect(Other.({ k: 2 }))} ${Str.inspect([Other.({ k: 3 })])}\n")
	Ok({})
}
