app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Regression test for issue 11047. Threading an iterator through Iter.custom's
# state must not allocate once per item.
run! : Str => Str
run! = |input| {
	seed = Str.count_utf8_bytes(input)
	small = seed * 8
	large = seed * 64

	native_small_before = Host.alloc_count!()
	native_small_sum = Iter.fold(Iter.map(U64.until(0, small), |x| x + 1), 0, |a, x| a + x)
	native_small_allocs = Host.alloc_count!() - native_small_before

	native_large_before = Host.alloc_count!()
	native_large_sum = Iter.fold(Iter.map(U64.until(0, large), |x| x + 1), 0, |a, x| a + x)
	native_large_allocs = Host.alloc_count!() - native_large_before

	scalar_small_before = Host.alloc_count!()
	scalar_small_sum = Iter.fold(counting(0, small), 0, |a, x| a + x)
	scalar_small_allocs = Host.alloc_count!() - scalar_small_before

	scalar_large_before = Host.alloc_count!()
	scalar_large_sum = Iter.fold(counting(0, large), 0, |a, x| a + x)
	scalar_large_allocs = Host.alloc_count!() - scalar_large_before

	threaded_small_before = Host.alloc_count!()
	threaded_small_sum = Iter.fold(passthrough(U64.until(0, small)), 0, |a, x| a + x)
	threaded_small_allocs = Host.alloc_count!() - threaded_small_before

	threaded_large_before = Host.alloc_count!()
	threaded_large_sum = Iter.fold(passthrough(U64.until(0, large)), 0, |a, x| a + x)
	threaded_large_allocs = Host.alloc_count!() - threaded_large_before

	index_small_before = Host.alloc_count!()
	index_small_sum = Iter.fold(with_index(U64.until(0, small)), 0, |a, (i, x)| a + i + x)
	index_small_allocs = Host.alloc_count!() - index_small_before

	index_large_before = Host.alloc_count!()
	index_large_sum = Iter.fold(with_index(U64.until(0, large)), 0, |a, (i, x)| a + i + x)
	index_large_allocs = Host.alloc_count!() - index_large_before

	record_small_before = Host.alloc_count!()
	record_small_sum = Iter.fold(record_passthrough(U64.until(0, small)), 0, |a, x| a + x)
	record_small_allocs = Host.alloc_count!() - record_small_before

	record_large_before = Host.alloc_count!()
	record_large_sum = Iter.fold(record_passthrough(U64.until(0, large)), 0, |a, x| a + x)
	record_large_allocs = Host.alloc_count!() - record_large_before

	expect native_small_sum == small * (small + 1) / 2
	expect native_large_sum == large * (large + 1) / 2
	expect scalar_small_sum == small * (small - 1) / 2
	expect scalar_large_sum == large * (large - 1) / 2
	expect threaded_small_sum == small * (small - 1) / 2
	expect threaded_large_sum == large * (large - 1) / 2
	expect index_small_sum == small * (small - 1)
	expect index_large_sum == large * (large - 1)
	expect record_small_sum == small * (small - 1) / 2
	expect record_large_sum == large * (large - 1) / 2

	expect native_large_allocs <= native_small_allocs + 32
	expect scalar_large_allocs <= scalar_small_allocs + 32
	expect threaded_large_allocs <= threaded_small_allocs + 32
	expect index_large_allocs <= index_small_allocs + 32
	expect record_large_allocs <= record_small_allocs + 32

	"iter custom items: ${small.to_str()} ${large.to_str()}, threaded sums: ${threaded_small_sum.to_str()} ${threaded_large_sum.to_str()}, allocations: native ${native_small_allocs.to_str()} ${native_large_allocs.to_str()}, scalar ${scalar_small_allocs.to_str()} ${scalar_large_allocs.to_str()}, threaded ${threaded_small_allocs.to_str()} ${threaded_large_allocs.to_str()}, with_index ${index_small_allocs.to_str()} ${index_large_allocs.to_str()}, record ${record_small_allocs.to_str()} ${record_large_allocs.to_str()}"
}

passthrough : Iter(U64) -> Iter(U64)
passthrough = |source|
	Iter.custom(
		source,
		Unknown,
		|src|
			match Iter.next(src) {
				Done => Err(NoMore)
				Skip({ rest }) => Ok((0, rest))
				One({ item, rest }) => Ok((item, rest))
			},
	)

counting : U64, U64 -> Iter(U64)
counting = |from, to|
	Iter.custom(
		from,
		Unknown,
		|i| if i < to { Ok((i, i + 1)) } else { Err(NoMore) },
	)

record_passthrough : Iter(U64) -> Iter(U64)
record_passthrough = |source| {
	base = { source: source, marker: 0 }
	Iter.custom(
		{ ..base, marker: 1 },
		Unknown,
		|state|
			match Iter.next(state.source) {
				Done => Err(NoMore)
				Skip({ rest }) => Ok((0, { source: rest, marker: state.marker }))
				One({ item, rest }) => Ok((item, { source: rest, marker: state.marker }))
			},
	)
}

with_index : Iter(a) -> Iter((U64, a))
with_index = |source|
	Iter.custom(
		(0, source),
		Iter.size_hint(source),
		|(i, src)| advance_indexed(i, src),
	)

advance_indexed : U64, Iter(a) -> Try(((U64, a), (U64, Iter(a))), [NoMore])
advance_indexed = |i, src|
	match Iter.next(src) {
		Done => Err(NoMore)
		Skip({ rest }) => advance_indexed(i, rest)
		One({ item, rest }) => Ok(((i, item), (i + 1, rest)))
	}
