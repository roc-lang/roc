app [State, program] { pf: platform "./platform/main.roc" }

State : {
	offset : U64,
	sequence : U64,
}

program = { make_outcome, }

make_outcome : State -> [Response(U16), Stream({ sequence : U64, step : Box(U64 -> U64) })]
make_outcome = |context|
	if context.sequence == 0 {
		Response(204)
	} else {
		Stream({
			sequence: context.sequence,
			step: Box.box(|wake| context.offset + context.sequence + wake),
		})
	}
