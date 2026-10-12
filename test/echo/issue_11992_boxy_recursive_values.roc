# repro for https://github.com/roc-lang/roc/issues/11992 under `--specialize=no`
#
# A value may refer to itself from inside a function in its own definition.
# Boxy builds such a value with a slot the function captures before the value
# exists, then fills the slot. The program also covers a callable-holding
# nominal value placed in a tag payload, which Boxy converts through lowering
# rather than a runtime adapter.

Thing := { call : {} -> [Again(Thing), Done], name : Str }

Generic(a) := { call : {} -> [Again(Generic(a)), Done], v : a }

Holder := { call : {} -> Str }

walk : Thing, U64, Str -> Str
walk = |t, n, acc| if n == 0 acc else match (t.call)({}) {
	Again(next) => walk(next, n - 1, acc.concat(next.name))
	Done => acc
}

top : Thing
top = { call: |{}| Again(top), name: "t" }

make : a -> Generic(a)
make = |x| {
	g : Generic(a)
	g = { call: |{}| Again(g), v: x }
	g
}

second : Generic(a) -> a
second = |g| match (g.call)({}) {
	Again(next) => next.v
	Done => g.v
}

pair : a, b -> (a, b)
pair = |x, y| {
	(f, n) = (|{}| (n, y), x)
	f({})
}

countdown : U64 -> Str
countdown = |start| {
	step : U64 -> Str
	step = if start > 100 {
		|_| "big"
	} else {
		|n| if n == 0 "done" else step(n - 1)
	}
	step(start)
}

main! = |args| {
	k = List.len(args)
	name = Str.repeat("ab", 2)
	thing : Thing
	thing = { call: |{}| Again(thing), name }
	var $count = 0
	other : Thing
	other = if k == 0 {
		$count = 5
		{ call: |{}| Again(other), name: "o" }
	} else {
		{ call: |{}| Done, name: "x" }
	}
	holder : Holder
	holder = { call: |{}| "h" }
	wrapped = if k == 0 Wrap(holder) else Missing
	(p, q) = pair(Str.repeat("x", 3), k + 2)
	echo!(
		Str.join_with(
			[
				walk(thing, 3, ""),
				walk(other, 2, ""),
				$count.to_str(),
				walk(top, 2, ""),
				second(make(Str.repeat("g", 2))),
				second(make(k + 7)).to_str(),
				"${p}${q.to_str()}",
				countdown(k + 5),
				countdown(k + 500),
				match wrapped {
					Wrap(h) => (h.call)({})
					Missing => "m"
				},
			],
			",",
		),
	)
	Ok({})
}
