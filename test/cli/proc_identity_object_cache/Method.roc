# Lib's generic code dispatches to a method this module declares. An edit
# elsewhere in the module leaves the method's identity, and so the identity
# of Lib's specialization that dispatches to it, unchanged; editing the
# method's body changes only the method's own code. The method is more than
# a wrapper, so Lib's code calls it rather than holding a copy of it.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

Crate := { mass : U64 }.{
	weight : Crate -> U64
	weight = |crate| {
		doubled = crate.mass * 2
		doubled + crate.mass % 3
	}
}

main! = |args| {
	base = List.len(args)
	crates = [Crate.{ mass: base }, Crate.{ mass: base + 1 }, Crate.{ mass: base + 2 }]
	Stdout.line!("total ${Lib.total_weight(crates).to_str()}")
	Ok({})
}
