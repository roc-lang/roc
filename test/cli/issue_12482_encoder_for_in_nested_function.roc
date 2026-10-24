# repro for https://github.com/roc-lang/roc/issues/12482
# A derived encoder_for of a generic nominal type, used inside a function
# nested in a polymorphic function, must be validated at each specialization
# of the enclosing function, rather than crashing the compiler in post-check
# monotype.

W(a) := a.{
	encoder_for : _
}

returned_lambda = |r| |{}| Json.to_str(W.(r))

returned_local = |r| {
	g = |{}| Json.to_str(W.(r))
	g
}

called_local = |r| {
	g = |_| Json.to_str(W.(r))
	g({})
}

annotated_local = |r| {
	g : {} -> Str
	g = |{}| Json.to_str(W.(r))
	g
}

nested_local = |r| {
	h = |{}| {
		g = |{}| Json.to_str(W.(r))
		g
	}
	h({})
}

expect returned_lambda(7.U64)({}) == "7"
expect returned_local(7.U64)({}) == "7"
expect returned_local("a")({}) == "\"a\""
expect called_local(7.U64) == "7"
expect annotated_local([1.I8, 2])({}) == "[1,2]"
expect nested_local(7.U64)({}) == "7"
