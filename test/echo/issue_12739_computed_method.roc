# repro for https://github.com/roc-lang/roc/issues/12739
# A top-level method bound to a computed callable (`to_inspect = make()`) is a
# method: compile-time evaluation produces its function value, and explicit
# calls, Str.inspect, ==, where-clause dictionaries, and compile-time expects
# all call that value.
app [main!] {}

make : () -> (Rows -> Str)
make = || |r| "computed ${r.n.to_str()}"

same : () -> (Rows, Rows -> Bool)
same = || |_a, _b| Bool.False

render = make()

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
	echo!("${Str.inspect(x)} ${x.to_inspect()} ${Str.inspect(x == x)} ${x.shown()} ${describe(x)} ${Str.inspect([x])}\n")
	Ok({})
}
