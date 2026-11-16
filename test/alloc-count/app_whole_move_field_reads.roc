app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A record is passed whole to a function that owns it, and the caller reads
# one of the record's fields afterwards. The caller has to keep that field
# alive across the call, but nothing else: the fields it never reads again
# must reach the callee uniquely owned, so the callee updates them in place.
# Each case reports the allocations the call made.
State : { a : List(U64), b : List(U64) }

Deep : { a : List(U64), inner : { b : List(U64), c : List(U64) }, n : U64 }

Step := [Step((U64 -> Step))]

# Recursive, so no optimization level inlines these. Each owns its record,
# appends to the record's `a`, and drops the rest.
push_a : State, U64 -> List(U64)
push_a = |s, k| if k == 0 s.a.append(k) else push_a(s, k - 1)

push_deep : Deep, U64 -> List(U64)
push_deep = |s, k| if k == 0 s.a.append(s.n) else push_deep(s, k - 1)

push_wrapped : { s : State, n : U64 }, U64 -> List(U64)
push_wrapped = |w, k| if k == 0 w.s.a.append(w.n) else push_wrapped(w, k - 1)

set_a : State, U64, U64 -> List(U64)
set_a = |s, i, k| if k == 0 (s.a.set(i, i) ?? crash "set") else if k > 50 [] else set_a(s, i, k - 1)

state : U64 -> State
state = |n| { a: List.with_capacity(16).append(n), b: List.with_capacity(16).append(n + 1) }

deep : U64 -> Deep
deep = |n| { a: List.with_capacity(16).append(n), inner: { b: List.with_capacity(16).append(n + 1), c: List.with_capacity(16).append(n + 2) }, n }

first : List(U64) -> U64
first = |list| List.get(list, 0) ?? 0

# Only the other field is read afterwards.
other! : State => Str
other! = |s| {
	before = Host.alloc_count!()
	a2 = push_a(s, 3)
	allocs = Host.alloc_count!() - before
	"other ${allocs.to_str()}/${List.len(a2).to_str()}/${List.len(s.b).to_str()}"
}

# The appended field itself is read afterwards, so the append must copy.
same! : State, U64 => Str
same! = |s, n| {
	before = Host.alloc_count!()
	a2 = push_a(s, 3)
	allocs = Host.alloc_count!() - before
	"same ${allocs.to_str()}/${List.len(a2).to_str()}/${(first(s.a) - n).to_str()}"
}

# The call sits in one branch and the read follows the join.
branch! : State, Bool => Str
branch! = |s, cond| {
	before = Host.alloc_count!()
	a2 = if cond push_a(s, 3) else []
	allocs = Host.alloc_count!() - before
	"branch ${allocs.to_str()}/${List.len(a2).to_str()}/${List.len(s.b).to_str()}"
}

# The record is passed whole twice. The first call must leave it intact; the
# second, measured here, is the last to read `a`.
twice! : State => Str
twice! = |s| {
	a1 = push_a(s, 3)
	before = Host.alloc_count!()
	a2 = push_a(s, 3)
	allocs = Host.alloc_count!() - before
	"twice ${allocs.to_str()}/${List.len(a1).to_str()}/${List.len(a2).to_str()}/${List.len(s.b).to_str()}"
}

# The record is passed whole on every iteration of a loop, so every call
# shares it and copies.
looped! : State => Str
looped! = |s| {
	var $total = 0
	var $i = 0
	before = Host.alloc_count!()
	while $i < 3 {
		$total = $total + List.len(push_a(s, 3))
		$i = $i + 1
	}
	allocs = Host.alloc_count!() - before
	"looped ${allocs.to_str()}/${$total.to_str()}/${List.len(s.b).to_str()}"
}

# Only one list of a nested record is read afterwards.
nested! : Deep => Str
nested! = |s| {
	before = Host.alloc_count!()
	a2 = push_deep(s, 3)
	allocs = Host.alloc_count!() - before
	"nested ${allocs.to_str()}/${List.len(a2).to_str()}/${List.len(s.inner.c).to_str()}"
}

# Only a number is read afterwards.
scalar! : Deep, U64 => Str
scalar! = |s, n| {
	before = Host.alloc_count!()
	a2 = push_deep(s, 3)
	allocs = Host.alloc_count!() - before
	"scalar ${allocs.to_str()}/${List.len(a2).to_str()}/${(s.n - n).to_str()}"
}

# The record is stored in another record, which is passed on.
wrapped! : State => Str
wrapped! = |s| {
	before = Host.alloc_count!()
	a2 = push_wrapped({ s, n: 7 }, 3)
	allocs = Host.alloc_count!() - before
	"wrapped ${allocs.to_str()}/${List.len(a2).to_str()}/${List.len(s.b).to_str()}"
}

# The record is read out of a record a closure captures, passed whole, and one
# of its lists is read afterwards to rebuild the closure. The closure lives in
# a list, as in https://github.com/roc-lang/roc/issues/12957.
captured : State -> Step
captured = |s0| {
	held = { s: s0 }
	Step.Step(
		|i| {
			{ s } = held
			a = set_a(s, 0, 3 + i - i)
			captured({ a, b: s.b })
		},
	)
}

placeholder : {} -> Step
placeholder = |_| Step.Step(|_| placeholder({}))

captured! : State => Str
captured! = |s| {
	var $kids = [captured(s)]
	before = Host.alloc_count!()
	var $i = 0
	while $i < 100 {
		{ list, prev } = $kids.replace(0, placeholder({})) ?? crash "kid"
		Step.Step(call) = prev
		$kids = list.set(0, call($i)) ?? crash "kid"
		$i = $i + 1
	}
	allocs = Host.alloc_count!() - before
	"captured ${allocs.to_str()}"
}

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input)
	"${other!(state(n))}, ${same!(state(n), n)}, ${branch!(state(n), first(state(n).a) == n)}, ${twice!(state(n))}, ${looped!(state(n))}, ${nested!(deep(n))}, ${scalar!(deep(n), n)}, ${wrapped!(state(n))}, ${captured!(state(n))}"
}
