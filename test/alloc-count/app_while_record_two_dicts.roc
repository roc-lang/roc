app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A `while` loop over a `var` holding a record of two dicts and a number, in a
# function with two callers, reached through a function value. `$state` is the
# only reference to the record, so every insert of an existing key must happen
# in place and the loop must allocate nothing at every optimization level.
State : { first : Dict(U64, U64), second : Dict(U64, U64), writes : U64 }

update : State, U64 -> State
update = |state, key| {
	{ first, second, writes } = state
	{ first: first.insert(key, key + 1), second: second.insert(key, key), writes: writes + 1 }
}

looped : State, U64 -> State
looped = |state, count| {
	var $state = state
	var $key = 0
	while $key < count {
		$state = update($state, $key)
		$key = $key + 1
	}
	$state
}

either : State, Bool -> State
either = |state, all| if (all) looped(state, 1000) else looped(state, 20)

apply : (State, Bool -> State), State, Bool -> State
apply = |f, state, all| f(state, all)

dict_of : U64 -> Dict(U64, U64)
dict_of = |n| {
	var $d = Dict.with_capacity(n)
	var $i = 0
	while $i < n {
		$d = $d.insert($i, $i)
		$i = $i + 1
	}
	$d
}

state_of : U64 -> State
state_of = |n| { first: dict_of(n), second: dict_of(n), writes: 0 }

run! : Str => Str
run! = |input| {
	n = 1000 + input.count_utf8_bytes()
	state = state_of(n)
	before = Host.alloc_count!()
	after = apply(either, state, n > 0)
	allocations = Host.alloc_count!() - before
	expect Dict.get(after.first, 999) == Ok(1000)
	expect Dict.get(after.second, 999) == Ok(999)
	"${after.writes.to_str()} updates of two dicts of ${n.to_str()}: ${allocations.to_str()} allocations"
}
