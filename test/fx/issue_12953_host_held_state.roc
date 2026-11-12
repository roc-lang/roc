app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# Issue 12953: the host holds an app state whose fields are unboxed functions,
# passed through a hosted `Box(a)` slot, and the app calls them afterwards.
State := { count : U64, on_event : U64, U64 -> U64, label : Str, render : U64 -> Str, twice : (U64 -> U64) -> (U64 -> U64) }

main! = || {
	offset = 7
	label = "a label long enough to live on the heap rather than inline"
	state : State
	state = { count: 1, on_event: |count, event| count + event + offset, label, render: |n| "${label}: ${U64.to_str(n)}", twice: |f| |n| f(f(n)) }
	Host.store_seed!(Box.box(state))
	returned : Box(State)
	returned = Host.take_seed!()
	back = Box.unbox(returned)
	next = ((back.twice)(|n| (back.on_event)(n, 10)))(back.count)
	Stdout.line!((back.render)(next))
}
