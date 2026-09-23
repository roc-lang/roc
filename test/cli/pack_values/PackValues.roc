# Reaches closed module functions directly and as values (mapped over a
# list, held in a list of functions, stored in a record field), so a rebuild
# served from the object cache exercises the forwarding entries.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Ops
import pf.Stdout

apply_all : List(U64 -> U64), U64 -> List(U64)
apply_all = |fns, value| List.map(fns, |f| f(value))

main! = || {
	direct = Ops.double(21)
	mapped = List.map([1, 2, 3], Ops.double)
	applied = apply_all([Ops.double, Ops.triple, |n| Ops.clamp(n, 4)], 5)
	holder = { op: Ops.triple }
	total = List.sum(mapped) + List.sum(applied) + direct + (holder.op)(7)
	Stdout.line!(U64.to_str(total))
}
