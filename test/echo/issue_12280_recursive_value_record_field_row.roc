# `state` and `run_steps` refer to each other, so `run_steps` uses `state`
# while `state`'s own definition is still being checked. A tag row inside a
# record field is not widened at each use: every use shares it, and the
# annotation bounds it to `[Idle]`, so neither list can add a `Busy` tag.
state : { tag : [Idle], step : {} -> {} }
state = { tag: Idle, step: |_| run_steps({}) }

run_steps = |_| {
	_ = [state.tag, Busy(1.U64)]
	_ = [state.tag, Busy({})]
	{}
}

main! = |_args| {
	echo!("before")
	(state.step)({})
	Ok({})
}
