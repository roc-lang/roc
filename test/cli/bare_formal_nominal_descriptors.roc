# A nominal whose backing is its own type parameter keeps its own descriptor
# methods inside lists, tuples and tags, including inside polymorphic
# functions, and describes a payload whose other type parameter stays unpinned.

C(a) := a.{
	to_inspect : C(a) -> Str
	to_inspect = |_c| "C!"
	is_eq : C(a), C(a) -> Bool
	is_eq = |_x, _y| Bool.False
	encoder_for : _
}

W(a) := a.{
	encoder_for : _
	is_eq : _
}

Two(a) := [Two(a)].{
	is_eq : _
}

Reply(ret, err) := [Ok(ret), Err(err)].{
	encoder_for : _
	is_eq : _
}

inspect_list = |r| Str.inspect([C.(r), C.(r)])

eq_list = |r| [C.(r)] == [C.(r)]

inspect_tag = |r| Str.inspect(Two.Two(C.(r)))

inspect_tuple = |r| Str.inspect((C.(r), W.(r), r))

unwrap = |C.(x)| x

inspect_unwrapped = |r| Str.inspect([unwrap(C.(r))])

derived_in_tag = |r| Two.Two(W.(r)) == Two.Two(W.(r))

derived_in_list = |r| [W.(r)] == [W.(r)]

encode_unpinned = |r| Json.to_str(Reply.Ok(W.(Reply.Err(r))))

encode_custom = |r| Json.to_str(C.(r))

encode_custom_list = |r| Json.to_str([C.(r)])

inspect_wrapped_custom = |r| Str.inspect([W.(C.(r))])

encode_custom_nested = |r| Json.to_str(C.(C.(r)))

encode_custom_over_nominal = |r| Json.to_str(C.(Reply.Err(r)))

expect inspect_list("a") == "[C!, C!]"
expect !eq_list(1.U8)
expect inspect_tag(1.U8) == "Two(C!)"
expect inspect_tuple([1.I8]) == "(C!, [1], [1])"
expect inspect_unwrapped("z") == "[\"z\"]"
expect derived_in_tag("a")
expect derived_in_list(1.U8)
expect encode_unpinned(1.U8) == "{\"Ok\":{\"Err\":1}}"
expect encode_custom([1.I8]) == "[1]"
expect encode_custom_list(2.I8) == "[2]"
expect encode_custom_nested(1.U8) == "1"
expect encode_custom_over_nominal(1.U8) == "{\"Err\":1}"
expect Json.to_str(W.(Reply.Err(1.U8))) == "{\"Err\":1}"
expect Str.inspect([C.(1.U8)]) == "[C!]"
expect inspect_wrapped_custom(1.U8) == "[C!]"
expect Str.inspect(W.(C.(1.U8))) == "C!"
expect Str.inspect(W.(W.(C.(1.U8)))) == "C!"
