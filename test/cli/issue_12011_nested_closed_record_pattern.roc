# A closed nested record pattern that leaves out one of its field's fields is
# a type mismatch. Checking reports it and retires the match, so `roc check`
# and `roc` report the mismatch instead of crashing while lowering it, and the
# independent match before it still runs.

Event := { payload : { id : U64, note : Str } }

summarize : Event -> Str
summarize = |event| match event {
	Event.{ payload: { id } } => id.to_str()
}

describe : Event -> Str
describe = |event| match event {
	Event.{ payload: { id, .. } } => "event ${id.to_str()}"
}

main! = |_args| {
	echo!(describe(Event.{ payload: { id: 7, note: "hi" } }))
	echo!(summarize(Event.{ payload: { id: 1, note: "hi" } }))
	Ok({})
}
