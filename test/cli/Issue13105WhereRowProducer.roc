Item := {}.{
	read : Str -> Try(Item, [Bad, Gone])
	read = |s| if (s == "") Err(Gone) else Err(Bad)
}

read_any : Str -> Try(a, [Bad, ..errs]) where [a.read : Str -> Try(a, [Bad, ..errs])]
read_any = |s| {
	Shape : a
	Shape.read(s)
}

describe : Str -> Try(a, Str) where [a.read : Str -> Try(a, [Bad, Gone])]
describe = |s|
	match read_any(s) {
		Ok(v) => Ok(v)
		Err(Gone) => Err("gone")
		Err(Bad) => Err("bad")
	}

expect {
	r : Try(Item, Str)
	r = describe("")
	match r {
		Err(m) => m == "gone"
		Ok(_) => Bool.False
	}
}
