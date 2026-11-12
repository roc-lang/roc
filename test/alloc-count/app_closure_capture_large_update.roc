app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/12957 through calls the
# inliner declines. A closure captures a record holding a dict or a record of
# lists, possibly nested one level deeper. Each call passes the dict or the
# record to a function too large to inline, whose borrowed base variant would
# copy the dict or list it updates and whose owned variant updates it in place.
# The closure lives in a list: each step takes it out with `List.replace`,
# calls it, and puts the returned closure back. Nothing else refers to the
# dict or the lists, so every update must happen in place.
Step := [Step((U64 -> Step))]

Cols : { xs : List(U64), ys : List(U64) }

# Large enough not to be inlined. The first branch ignores `d`, so its owned
# variant is not the base signature.
bump : Dict(U64, U64), U64 -> Dict(U64, U64)
bump = |d, i| {
	if i > 1_000_000_000 {
		Dict.empty()
	} else {
		var $a = d
		$a = $a.insert(i + 0, i * 3 + $a.len())
		$a = $a.insert(i + 1, i * 4 + $a.len())
		$a = $a.insert(i + 2, i * 5 + $a.len())
		$a = $a.insert(i + 3, i * 6 + $a.len())
		$a = $a.insert(i + 4, i * 7 + $a.len())
		$a = $a.insert(i + 5, i * 8 + $a.len())
		$a = $a.insert(i + 6, i * 9 + $a.len())
		$a = $a.insert(i + 7, i * 10 + $a.len())
		$a = $a.insert(i + 8, i * 11 + $a.len())
		$a = $a.insert(i + 9, i * 12 + $a.len())
		$a = $a.insert(i + 10, i * 13 + $a.len())
		$a = $a.insert(i + 11, i * 14 + $a.len())
		$a = $a.insert(i + 12, i * 15 + $a.len())
		$a = $a.insert(i + 13, i * 16 + $a.len())
		$a = $a.insert(i + 14, i * 17 + $a.len())
		$a = $a.insert(i + 15, i * 18 + $a.len())
		$a = $a.insert(i + 16, i * 19 + $a.len())
		$a = $a.insert(i + 17, i * 20 + $a.len())
		$a = $a.insert(i + 18, i * 21 + $a.len())
		$a = $a.insert(i + 19, i * 22 + $a.len())
		$a = $a.insert(i + 20, i * 23 + $a.len())
		$a = $a.insert(i + 21, i * 24 + $a.len())
		$a = $a.insert(i + 22, i * 25 + $a.len())
		$a = $a.insert(i + 23, i * 26 + $a.len())
		$a
	}
}

# The same with a record of lists: one list is updated, the other kept.
poke : Cols, U64 -> Cols
poke = |c, i| {
	if i > 1_000_000_000 {
		{ xs: [], ys: [] }
	} else {
		var $xs = c.xs
		$xs = $xs.set(i + 0, i * 3 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 1, i * 4 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 2, i * 5 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 3, i * 6 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 4, i * 7 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 5, i * 8 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 6, i * 9 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 7, i * 10 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 8, i * 11 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 9, i * 12 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 10, i * 13 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 11, i * 14 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 12, i * 15 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 13, i * 16 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 14, i * 17 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 15, i * 18 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 16, i * 19 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 17, i * 20 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 18, i * 21 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 19, i * 22 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 20, i * 23 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 21, i * 24 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 22, i * 25 + $xs.len()) ?? crash "set"
		$xs = $xs.set(i + 23, i * 26 + $xs.len()) ?? crash "set"
		{ xs: $xs, ys: c.ys }
	}
}

# The captured record holds only the dict.
dict_only : Dict(U64, U64) -> Step
dict_only = |d0| {
	held = { d: d0 }
	Step.Step(
		|i| {
			{ d } = held
			dict_only(bump(d, i))
		},
	)
}

# The dict sits in a record in a record in the capture.
dict_deep : Dict(U64, U64), U64 -> Step
dict_deep = |d0, n0| {
	held = { inner: { d: d0, m: n0 }, n: n0 }
	Step.Step(
		|i| {
			{ d, m } = held.inner
			dict_deep(bump(d, i + m), held.n)
		},
	)
}

# The captured record holds only the record of lists.
cols_only : Cols -> Step
cols_only = |c0| {
	held = { c: c0 }
	Step.Step(
		|i| {
			{ c } = held
			cols_only(poke(c, i))
		},
	)
}

# The captured record holds the record of lists and one other field.
cols_beside : Cols, U64 -> Step
cols_beside = |c0, n0| {
	held = { c: c0, n: n0 }
	Step.Step(
		|i| {
			{ c, n } = held
			cols_beside(poke(c, i + n), n)
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

cols : U64 -> Cols
cols = |n| { xs: List.repeat(0, n), ys: List.repeat(0, n) }

run! : Str => Str
run! = |input| {
	n = 1_000 + input.count_utf8_bytes()
	a = allocations!(dict_only(big(n)))
	b = allocations!(dict_deep(big(n), 1))
	c = allocations!(cols_only(cols(n)))
	e = allocations!(cols_beside(cols(n), 1))
	"100 calls: dict_only ${a.to_str()}, dict_deep ${b.to_str()}, cols_only ${c.to_str()}, cols_beside ${e.to_str()} allocations"
}
