BoxyDerivedJsonParsers :: [].{}

Counted := { name : Str, count : U8 ?? 10 }.{
	parser_for : _
	is_eq : _
}

# Issue 11356: a derived record parser whose error row is inferred.
expect {
	v : Try({ a : Str }, _)
	v = Json.parse("{\"a\":\"x\"}")
	match v {
		Ok({ a }) => a == "x"
		_ => False
	}
}

expect {
	v : Try({ a : Str, b : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"b\":\"y\",\"a\":\"x\",\"skipped\":[1,{\"c\":null}]}")
	v == Ok({ a: "x", b: "y" })
}

expect {
	v : Try({ a : Str, b ?: Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"a\":\"x\"}")
	v == Ok({ a: "x" })
}

expect {
	v : Try({ a : Str, b ?: Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"a\":\"x\",\"b\":\"y\"}")
	v == Ok({ a: "x", b: "y" })
}

expect {
	v : Try(Counted, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"name\":\"n\"}")
	v == Ok({ name: "n", count: 10 })
}

expect {
	v : Try(Counted, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"count\":3,\"name\":\"n\"}")
	v == Ok({ name: "n", count: 3 })
}

expect {
	v : Try(List(Str), [InvalidJson(Str)])
	v = Json.parse("[\"x\", \"y\"]")
	v == Ok(["x", "y"])
}

expect {
	v : Try(List(Str), [InvalidJson(Str)])
	v = Json.parse("[]")
	v == Ok([])
}

expect {
	v : Try((Str, Str), [InvalidJson(Str)])
	v = Json.parse("[\"x\", \"y\"]")
	v == Ok(("x", "y"))
}

expect {
	v : Try([Foo, Bar(Str), Baz(Str, Str)], [InvalidJson(Str)])
	v = Json.parse("\"Foo\"")
	v == Ok(Foo)
}

expect {
	v : Try([Foo, Bar(Str), Baz(Str, Str)], [InvalidJson(Str)])
	v = Json.parse("{\"Bar\":\"a\"}")
	v == Ok(Bar("a"))
}

expect {
	v : Try([Foo, Bar(Str), Baz(Str, Str)], [InvalidJson(Str)])
	v = Json.parse("{\"Baz\":[\"a\",\"b\"]}")
	v == Ok(Baz("a", "b"))
}
