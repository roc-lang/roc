platform "glue-generic-callable-arg"
	requires {
		unused : {} -> {}
	}
	exposes [Shapes]
	packages {}
	provides { "handler": handler, "stepper": stepper, "adder": adder }
	targets: {}

import Shapes

handler : {} -> Shapes.Handler(U64)
handler = |_| H(|_| {})

stepper : {} -> Box(U64 -> Shapes.Step)
stepper = |_| Box.box(|n| if n == 0 Done else Emit(n))

adder : {} -> Box(Shapes.Adder)
adder = |_| Box.box(Shapes.Adder.(|x| x + 1))
