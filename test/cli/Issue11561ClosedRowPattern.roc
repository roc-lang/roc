# A use that names MissingRequiredField widens the value's row, but the
# definition itself still produces that tag through its derived parser, so
# its annotation must list it.
Issue11561ClosedRowPattern :: [].{}

expect {
	v : Try({ x : Str }, [InvalidJson(Str)])
	v = Json.parse("{}")
	match v {
		Err(MissingRequiredField(_)) => True
		_ => False
	}
}
