app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/11934
# A dying buffer passed through a non-inlined clear/refill function must keep
# its capacity. Count the initial reservation and every refill before output.
refill : List(U32), U64 -> Try(List(U32), [Bug])
refill = |list, k| {
	var $list = List.clear(list)
	if k > 1000 {
		return Err(Bug)
	} else {
	}
	var $i = 0
	while $i < 5000 {
		$list = List.append($list, 7)
		$i = $i + 1
	}
	Ok($list)
}

run! : Str => Str
run! = |input| {
	rounds = Str.count_utf8_bytes(input)
	before = Host.alloc_count!()
	var $list = List.with_capacity(5000)
	var $round = 0
	while $round < rounds {
		$list = match refill($list, $round) {
			Ok(list) => list
			Err(Bug) => crash "unexpected refill failure"
		}
		$round = $round + 1
	}
	allocs = Host.alloc_count!() - before
	expect allocs == 1
	expect List.len($list) == 5000
	expect List.first($list) == Ok(7)
	"refill length: ${List.len($list).to_str()}, refill allocations: ${allocs.to_str()}"
}
