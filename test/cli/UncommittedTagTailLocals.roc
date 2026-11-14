UncommittedTagTailLocals :: [].{}

# A local function that matches on a tag parameter quantifies the uncommitted
# tag tail its match closed, so it is a generalized local. It reads the
# enclosing function's encoder evidence and is returned and called at an
# anonymous union and at a nominal `Try`; the same shape over an ordinary type
# variable is the control.

W(a) := a.{
	encoder_for : _
}

returned_tag_local = |r| {
	g = |tag| match tag {
		Ok(n) => {
			m : U8
			m = n
			Json.to_str(W.(r)).concat(Str.inspect(m))
		}
		Err(e) => Str.concat("err", e)
	}
	g
}

nested_tag_local = |r| {
	h = |{}| {
		g = |tag| match tag {
			Ok(n) => {
			m : U8
			m = n
			Json.to_str(W.(r)).concat(Str.inspect(m))
		}
			Err(e) => Str.concat("err", e)
		}
		g
	}
	h({})
}

returned_var_local = |r| {
	g = |x| {
		_ = x
		Json.to_str(W.(r))
	}
	g
}

anno : Try(U8, Str)
anno = Ok(3)

expect returned_tag_local(7.U64)(Ok(1)) == "71"
expect returned_tag_local(7.U64)(Err("!")) == "err!"
expect returned_tag_local(7.U64)(anno) == "73"
expect nested_tag_local("a")(Ok(1)) == "\"a\"1"
expect nested_tag_local("a")(anno) == "\"a\"3"
expect returned_var_local(7.U64)(1) == "7"
