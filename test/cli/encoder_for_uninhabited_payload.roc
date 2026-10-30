# A derived encoder_for of a nominal whose type argument is never constrained
# encodes the constructed tag. The never-constructed tag's payload closes to
# the empty tag union, so no value has that tag and its payload's encoder is
# statically unreachable.

Reply(ret, err) := [Ok(ret), Err(err)].{
	encoder_for : _
}

ok_reply = |r| Json.to_str(Reply.Ok(r))

err_reply = |e| Json.to_str(Reply.Err(e))

expect Json.to_str(Reply.Ok(1.U16)) == "{\"Ok\":1}"
expect ok_reply("a") == "{\"Ok\":\"a\"}"
expect err_reply([1.U8, 2]) == "{\"Err\":[1,2]}"
expect {
	v : Reply(U16, _)
	v = Reply.Ok(1)
	Json.to_str(v) == "{\"Ok\":1}"
}
