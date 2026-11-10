# An annotated value's row is the annotation's at every use, so a pattern that
# names another tag is a mismatch. The nested parser's incompatible error
# payload is rejected independently; matching cannot widen either contract.
Issue11561ClosedRowPattern :: [].{}

Token := {}.{
	parser_for : encoding -> (state -> Try({ value : Token, rest : state }, [InvalidJson(U8)]))
	parser_for = |_| |_| Err(InvalidJson(7))
}

expect {
	v : Try({ x : Token }, [InvalidJson(Str)])
	v = Json.parse("{\"x\":null}")
	match v {
		Err(Unrelated) => True
		_ => False
	}
}
