app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A uniquely owned `Dict` that a loop overwrites present keys of is updated
# in place: the entry list leaves the lookup's dying `Found` result by a take,
# so the write finds it unique. The loop allocates fewer times than it
# overwrites, only where a `.boxy` probe's frame-replacing call carries its
# dynamic arguments (about every other overwrite, as the probe lengths vary
# with the hash seed); a copying overwrite would allocate at least once per
# overwrite for the entry list it changes, on top of those carries.
# Compile-time evaluation runs the same `.boxy` code as a `--specialize=no`
# build.
run! : Str => Str
run! = |input| {
	count = 2000 + Str.count_utf8_bytes(input)

	var $dict = Dict.with_capacity(count)
	var $j = 0
	while $j < count {
		$dict = Dict.insert($dict, $j, $j)
		$j = $j + 1
	}

	before = Host.alloc_count!()
	var $k = 0
	while $k < count {
		$dict = Dict.insert($dict, $k, $k * 2)
		$k = $k + 1
	}
	overwrite_allocs = Host.alloc_count!() - before

	expect Dict.len($dict) == count
	expect Dict.get($dict, 1) == Ok(2)
	expect Dict.get($dict, count - 1) == Ok((count - 1) * 2)

	verdict = if overwrite_allocs < count "fewer than" else "at least as many as"
	"${count.to_str()} overwrites: allocations ${verdict} the overwrites"
}
