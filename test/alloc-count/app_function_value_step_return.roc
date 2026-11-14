app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A loop whose stepping function is chosen at runtime and called through a
# function value. Each step takes the list out of the state it is given and
# appends to it, and the loop never reads that state again, so the call must
# hand the state over and the list must grow in place.
Step : [Done, Next({ items : List(U64), left : U64 })]

step_append : { items : List(U64), left : U64 } -> Step
step_append = |state| {
	if state.left == 0 {
		Done
	} else {
		Next({ items: state.items.append(state.left), left: state.left - 1 })
	}
}

step_prepend : { items : List(U64), left : U64 } -> Step
step_prepend = |state| {
	if state.left == 0 {
		Done
	} else {
		Next({ items: state.items.prepend(state.left), left: state.left - 1 })
	}
}

run_steps : ({ items : List(U64), left : U64 } -> Step), U64 -> U64
run_steps = |step, n| {
	var $state = { items: [], left: n }
	var $steps = 0
	while Bool.True {
		match step($state) {
			Done => {
				return $steps
			}
			Next(next) => {
				$state = next
				$steps = $steps + 1
			}
		}
	}
}

run! : Str => Str
run! = |input| {
	n = 1024
	step = if input == "prepend" {
		step_prepend
	} else {
		step_append
	}
	before = Host.alloc_count!()
	steps = run_steps(step, n)
	allocs = Host.alloc_count!() - before
	# Growing the list to 1024 elements takes a handful of allocations;
	# copying it on every step takes one per element.
	"${steps.to_str()} steps, allocations: ${allocs.to_str()}, linear: ${if (allocs < 64) "yes" else "no"}"
}
