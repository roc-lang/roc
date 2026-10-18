app [run!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12294: reading a top-level
# list of boxed capturing closures allocates the same number of times whether
# or not the app's modules were served from the checked-module cache, including
# a closure that reads the constant holding it.

import pf.Host

handlers : List(Box(({} -> U64)))
handlers = {
	n = 42
	[Box.box(|{}| n)]
}

greeters : List(Box(({} -> Str)))
greeters = {
	name = "a greeting long enough to live on the heap"
	[Box.box(|{}| name)]
}

counters : List(Box((U64 -> U64)))
counters = {
	base = 42
	[Box.box(|k| if k == 0 base else List.len(counters) + k)]
}

run! : Str => Str
run! = |_input| {
	before = Host.alloc_count!()
	kept = List.drop_first(handlers, 0)
	handler_allocs = Host.alloc_count!() - before
	before_greeters = Host.alloc_count!()
	kept_greeters = List.drop_first(greeters, 0)
	greeter_allocs = Host.alloc_count!() - before_greeters
	before_counters = Host.alloc_count!()
	kept_counters = List.drop_first(counters, 0)
	counter_allocs = Host.alloc_count!() - before_counters
	counted = List.fold(kept_counters, 0, |sum, counter| sum + Box.unbox(counter)(5))

	"handlers: ${List.len(kept).to_str()}, allocations: ${handler_allocs.to_str()}, greeters: ${List.len(kept_greeters).to_str()}, allocations: ${greeter_allocs.to_str()}, counters: ${List.len(kept_counters).to_str()}, allocations: ${counter_allocs.to_str()}, counted: ${counted.to_str()}"
}
