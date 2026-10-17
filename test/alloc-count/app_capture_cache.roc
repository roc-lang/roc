app [run!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12294: reading a top-level
# list of boxed capturing closures allocates the same number of times whether
# or not the app's modules were served from the checked-module cache.

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

run! : Str => Str
run! = |_input| {
	before = Host.alloc_count!()
	kept = List.drop_first(handlers, 0)
	handler_allocs = Host.alloc_count!() - before
	before_greeters = Host.alloc_count!()
	kept_greeters = List.drop_first(greeters, 0)
	greeter_allocs = Host.alloc_count!() - before_greeters

	"handlers: ${List.len(kept).to_str()}, allocations: ${handler_allocs.to_str()}, greeters: ${List.len(kept_greeters).to_str()}, allocations: ${greeter_allocs.to_str()}"
}
