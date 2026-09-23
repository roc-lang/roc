Issue11562BoxyDescriptorSources :: [].{}

# repro for https://github.com/roc-lang/roc/issues/11562
# Boxy (--specialize=no) descriptor-source failures: errors flowing back out
# through Json.parse's open error row, Dict's generic implementation, and open
# error rows through higher-order callables. Each expect asserts the intended
# correct result; on the buggy pipeline they crash with boxy lower/interpreter
# invariant violations instead.

expect {
	v : Try({ a : Str, b : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"a\":\"x\"}")
	v == Err(MissingRequiredField("b"))
}

expect {
	v : Try({ a : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("")
	match v {
		Err(InvalidJson(_)) => True
		_ => False
	}
}

expect {
	d = Dict.from_list([("a", 1), ("b", 2)])
	Dict.len(d) == 2
}

run : (Str -> Try(U64, [Bad, ..errs])), Str -> Try(U64, [Bad, ..errs])
run = |f, s| {
	n = f(s)?
	Ok(n + 1)
}

expect {
	g : Str -> Try(U64, [Bad, Other(Str)])
	g = |s| Err(Other(s))
	r : Try(U64, [Bad, Other(Str)])
	r = run(g, "x")
	r == Err(Other("x"))
}
