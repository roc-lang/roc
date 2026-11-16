app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Fallible callees whose whole body is an `if` or a `match` that builds its
# result tag in every branch. Each fails before touching the list, so a loop
# that keeps the old list on failure never needs it alongside the appended
# one and should allocate only as the list grows, not once per iteration.
append_if : List(U64), U64 -> Try(List(U64), U64)
append_if = |list, item|
	if (list.len() > 1000000000) Err(item) else Ok(list.append(item))

append_match : List(U64), U64 -> Try(List(U64), U64)
append_match = |list, item|
	match list.len() > 1000000000 {
		Bool.True => Err(item)
		Bool.False => {
			Ok(list.append(item))
		}
	}

loop_if : U64 -> U64
loop_if = |n| {
	var $list = []
	var $index = 0
	while $index < n {
		$list = match append_if($list, $index) {
			Ok(next) => next
			Err(_) => $list
		}
		$index = $index + 1
	}
	$list.len()
}

loop_match : U64 -> U64
loop_match = |n| {
	var $list = []
	var $index = 0
	while $index < n {
		$list = match append_match($list, $index) {
			Ok(next) => next
			Err(_) => $list
		}
		$index = $index + 1
	}
	$list.len()
}

count! : (U64 -> U64), U64 => U64
count! = |f, n| {
	before = Host.alloc_count!()
	_ = f(n)
	Host.alloc_count!() - before
}

run! : Str => Str
run! = |_input| {
	n = 1024
	counts = [count!(loop_if, n), count!(loop_match, n)]
	# Growing a list to 1024 elements takes a handful of allocations; copying
	# it on every iteration takes one per element.
	linear = counts.all(|allocs| allocs < 64)
	shown = Str.join_with(counts.map(|allocs| allocs.to_str()), " ")
	"allocations: ${shown}, linear: ${if (linear) "yes" else "no"}"
}
