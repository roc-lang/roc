# An annotated value's row is the annotation's at every use, so a pattern that
# names MissingRequiredField is a mismatch, and the definition itself still
# produces that tag through its derived parser, so its annotation must list it.
Issue11561ClosedRowPattern :: [].{}

expect {
	v : Try({ x : Str }, [InvalidJson(Str)])
	v = Json.parse("{}")
	match v {
		Err(MissingRequiredField(_)) => True
		_ => False
	}
}
