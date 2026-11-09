# A derived encoder_for of a nominal whose backing is its own type parameter
# encodes the argument it stores, including inside a polymorphic function.

W(a) := a.{
	encoder_for : _
}

direct = |r| Json.to_str(W.(r))

in_list = |r| Json.to_str([W.(r), W.(r)])

through_helper = |r| |{}| direct(r)

expect direct(7.U64) == "7"
expect direct("a") == "\"a\""
expect in_list(7.U64) == "[7,7]"
expect through_helper([1.I8, 2])({}) == "[1,2]"
expect Json.to_str(W.(7.U64)) == "7"
