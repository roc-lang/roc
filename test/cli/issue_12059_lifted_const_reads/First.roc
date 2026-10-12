First := [].{
	Options := { size : U64 ?? 1, name : Str ?? "first" }

	size : Options -> U64
	size = |options| options.size

	name : Options -> Str
	name = |options| options.name

	Step := { run : U64 -> U64, label : Str ?? "none" }

	call : Step, U64 -> U64
	call = |step, n| (step.run)(n)

	Handler := { run : Options -> U64, make : U64 -> Options }

	go : Handler, U64 -> U64
	go = |handler, n| (handler.run)((handler.make)(n))
}
