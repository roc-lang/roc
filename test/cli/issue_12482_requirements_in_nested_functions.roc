# Functions nested in a polymorphic function receive the method requirements
# their enclosing function's type variables carry, whether they use them
# directly, through a structural value, or through a generic nominal.

Two(a) := [Two(a)].{
	encoder_for : _
	is_eq : _
}

Reply(ret, err) := [Ok(ret), Err(err)].{
	encoder_for : _
}

tuple_in_closure = |r| |x| |{}| Json.to_str((r, x))

eq_in_closure = |r| |{}| (r, r) == (r, r)

nested_nominal = |r| |{}| Json.to_str(Two.Two(Two.Two(r)))

nominal_over_list = |r| |{}| Json.to_str(Two.Two([r]))

tuple_of_nominals = |r, s| |{}| Json.to_str((Reply.Ok(r), Reply.Err(s)))

closure_over_closure = |r| {
	inner = |n| |{}| Json.to_str(Two.Two((r, n)))
	inner(3.U8)
}

eq_of_nominals = |r| |{}| [Two.Two(r), Two.Two(r)] == [Two.Two(r)]

expect tuple_in_closure(1.U8)("x")({}) == "[1,\"x\"]"
expect eq_in_closure(1.U8)({})
expect nested_nominal(1.U16)({}) == "{\"Two\":{\"Two\":1}}"
expect nominal_over_list(1.U8)({}) == "{\"Two\":[1]}"
expect tuple_of_nominals(1.U8, "e")({}) == "[{\"Ok\":1},{\"Err\":\"e\"}]"
expect closure_over_closure(1.U8)({}) == "{\"Two\":[1,3]}"
expect !eq_of_nominals(1.U8)({})
