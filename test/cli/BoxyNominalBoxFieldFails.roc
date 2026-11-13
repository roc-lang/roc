## `BoxyNominalBoxField`'s stored nominal box under a failing expectation:
## `roc test` reports the failure, releases the value it compared, and runs the
## expectation after it.
BoxyNominalBoxFieldFails := [].{}

Boxed :: Box({ prop : U64 }).{
	make : U64 -> Boxed
	make = |value| Boxed.(Box.box({ prop: value }))

	prop : Boxed -> U64
	prop = |Boxed.(boxed)| Box.unbox(boxed).prop
}

Run : { actual : [From, Sliced(Boxed)], name : Str }

props : List(Run) -> List(U64)
props = |runs|
	runs.map(
		|run| match run.actual {
			Sliced(boxed) => Boxed.prop(boxed)
			From => 0
		},
	)

expect props([{ actual: Sliced(Boxed.make(1)), name: "a" }]) == [2]
expect props([{ actual: Sliced(Boxed.make(1)), name: "a" }]) == [1]
