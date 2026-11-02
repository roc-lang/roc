RequestDeterminedResultRows :: [].{}

# https://github.com/roc-lang/roc/issues/12617
#
# Each `handle_*` composes a `?` error into its result row, so its other
# branch's call flows into a row wider than the one the checker recorded for
# that call.

hooks = { run: |x| x }

ok_hole : Str -> Try(Str, _)
ok_hole = |s| Ok(s)

ok_rigid : Str -> Try(Str, e)
ok_rigid = |s| Ok(s)

ok_eq : Str -> Try(Str, e) where [e.is_eq : e, e -> Bool]
ok_eq = |s| Ok(s)

handle_hole = |n|
	if (hooks.run)(n) == 1 {
		Err(Failed)?
		Ok("a")
	} else {
		ok_hole("b")
	}

handle_rigid = |n|
	if n == 1 {
		Err(Failed)?
		Ok("a")
	} else {
		ok_rigid("b")
	}

handle_eq = |n|
	if n == 1 {
		Err(Failed)?
		Ok("a")
	} else {
		ok_eq("b")
	}

# A producer constraint keeps the call at its own row: `e` is parsed as
# `[Exit(I32)]`, so `"Failed"` is not an `e`.
read_err : Str -> Try(Str, e) where [e.Json.Parseable([InvalidJson(Str)])]
read_err = |s| match Json.parse(s) {
	Ok(v) => Err(v)
	Err(_) => Ok(s)
}

handle_parsed = |input|
	if input == "" {
		Err(Failed)?
		Ok("a")
	} else {
		read_err(input)
	}

parse : Str -> Try(Str, [Empty])
parse = |s| if s == "" Err(Empty) else Ok(s)

wrap : (() -> Try(Str, [Missing, ..others])) -> Try(Str, [Missing, ..others])
wrap = |body| body()

run = |s| wrap(|| ok_rigid(parse(s)?))

expect {
	r : Try(Str, [Failed, Exit(I32)])
	r = handle_hole(2)
	r == Ok("b")
}

expect {
	r : Try(Str, [Failed, Exit(I32)])
	r = handle_hole(1)
	r == Err(Failed)
}

expect {
	r : Try(Str, [Failed, Exit(I32)])
	r = handle_rigid(2)
	r == Ok("b")
}

expect {
	r : Try(Str, [Failed, Exit(I32)])
	r = handle_eq(2)
	r == Ok("b")
}

expect {
	r : Try(Str, [Failed, Exit(I32)])
	r = handle_parsed("\"Failed\"")
	r == Ok("\"Failed\"")
}

expect {
	r : Try(Str, [Failed, Exit(I32)])
	r = handle_parsed("{\"Exit\":4}")
	r == Err(Exit(4))
}

expect run("a") == Ok("a")

expect run("") == Err(Empty)
