app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Generic arithmetic allocates nothing under `--specialize=no`. An unannotated
# helper is generic over its number literals' type: its self tail call
# changes a dynamic parameter, which the loop carries in its own frame rather
# than handing to a frame-replacing call. A `for` over a range reads the
# literals of `range_iter_standard` from the roots of Builtin's alias edge
# rather than converting them on every step; each step still builds its
# successor iterator, one allocation, and the range's iterator and its step
# function are two more.
count_down = |remaining, acc, step| if remaining == 0 acc else count_down(remaining - 1, acc + step, step)

count_to_zero = |remaining| if remaining == 0 0 else count_to_zero(remaining - 1)

sum_range : U64 -> U64
sum_range = |n| {
	var $total = 0
	for j in 0..<n {
		$total = $total + j
	}
	$total
}

run! : Str => Str
run! = |input| {
	size = Str.count_utf8_bytes(input)
	before_generic = Host.alloc_count!()
	var $total = 0
	var $i = 0
	while $i < 100 {
		$total = $total + count_down($i, 0, size) + count_to_zero($i)
		$i = $i + 1
	}
	generic_allocs = Host.alloc_count!() - before_generic
	before_range = Host.alloc_count!()
	range_total = sum_range(100 + size)
	range_allocs = Host.alloc_count!() - before_range
	"generic recursion: ${$total.to_str()} ${generic_allocs.to_str()} allocations; range of ${(100 + size).to_str()}: ${range_total.to_str()} ${range_allocs.to_str()} allocations"
}
