app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/12957 with a `Dict`. A
# closure captures a record holding a dict. Each call writes one existing key
# and returns a new closure built around the result. The closure lives in a
# list: each step takes it out with `List.replace`, calls it, and puts the
# returned closure back. Nothing else refers to the dict, so every insert must
# update it in place.
Step := [Step((U64 -> Step))]

# The captured record holds the dict and one other field.
beside : Dict(U64, U64), U64 -> Step
beside = |d0, n0| {
	held = { d: d0, n: n0 }
	Step.Step(
		|i| {
			{ d, n } = held
			beside(d.insert(i, i), n)
		},
	)
}

# The captured record holds only the dict.
only : Dict(U64, U64) -> Step
only = |d0| {
	held = { d: d0 }
	Step.Step(
		|i| {
			{ d } = held
			only(d.insert(i, i))
		},
	)
}

# The closure captures the dict and the number as two variables.
separate : Dict(U64, U64), U64 -> Step
separate = |d, n| Step.Step(|i| separate(d.insert(i, i), n))

# The dict is read twice through the captured record: once to measure it and
# once to write it.
twice : Dict(U64, U64), U64 -> Step
twice = |d0, n0| {
	held = { d: d0, n: n0 }
	Step.Step(
		|i| {
			k = held.d.len()
			twice(held.d.insert(i, i + k), held.n)
		},
	)
}

# The dict sits in a record in a record in the capture.
deep : Dict(U64, U64), U64 -> Step
deep = |d0, n0| {
	held = { inner: { d: d0, m: n0 }, n: n0 }
	Step.Step(
		|i| {
			{ d, m } = held.inner
			deep(d.insert(i, i + m), held.n)
		},
	)
}

placeholder : {} -> Step
placeholder = |_| Step.Step(|_| placeholder({}))

allocations! : Step => U64
allocations! = |start| {
	var $kids = [start]
	before = Host.alloc_count!()
	var $i = 0
	while $i < 100 {
		{ list, prev } = $kids.replace(0, placeholder({})) ?? crash "kid"
		Step.Step(call) = prev
		$kids = list.set(0, call($i)) ?? crash "kid"
		$i = $i + 1
	}
	Host.alloc_count!() - before
}

big : U64 -> Dict(U64, U64)
big = |n| {
	var $d = Dict.with_capacity(n)
	var $j = 0
	while $j < n {
		$d = $d.insert($j, $j)
		$j = $j + 1
	}
	$d
}

run! : Str => Str
run! = |input| {
	n = 1_000 + input.count_utf8_bytes()
	a = allocations!(beside(big(n), 1))
	b = allocations!(only(big(n)))
	c = allocations!(separate(big(n), 1))
	t = allocations!(twice(big(n), 1))
	e = allocations!(deep(big(n), 1))
	"100 calls: beside ${a.to_str()}, only ${b.to_str()}, separate ${c.to_str()}, twice ${t.to_str()}, deep ${e.to_str()} allocations"
}
