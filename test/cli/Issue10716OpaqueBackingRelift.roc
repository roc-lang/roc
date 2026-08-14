Issue10716OpaqueBackingRelift :: [].{
	State :: { value : U64 }.{
		new : U64 -> State
		new = |value| State.({ value: value })

		value : State -> U64
		value = |State.(state)| state.value
	}

	make_runner : {} -> { run : State -> U64 }
	make_runner = |_| {
		run = |State.(state)| Issue10716OpaqueBackingRelift.State.value(state)
		{ run }
	}
}

expect {
	runner = Issue10716OpaqueBackingRelift.make_runner({})
	run = runner.run
	run(Issue10716OpaqueBackingRelift.State.new(42)) == 42
}
