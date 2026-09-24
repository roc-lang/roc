Issue11562BoxyDescriptorSources :: [].{}

# repro for https://github.com/roc-lang/roc/issues/11562
# Boxy (--specialize=no) takes every descriptor from an exact planned source:
# the checked call-site substitution, the enclosing frame, or the operand
# itself. These cover errors flowing back out through open error rows, closures
# written inside generic functions, Box, Dict and Set's generic
# implementations, inspection, and derived encoders.

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

# A closure written inside a generic function describes its parameters with the
# enclosing function's descriptors, whichever higher-order function receives it.

map_identity : List(k) -> List(k)
map_identity = |xs| List.map(xs, |p| p)

expect List.len(map_identity(["a"])) == 1

my_fold : List(a), s, (s, a -> s) -> s
my_fold = |xs, s, g| match xs {
	[x, ..] => g(s, x)
	_ => s
}

count_first : List(k) -> U64
count_first = |xs| my_fold(xs, 0, |acc, _p| acc + 1)

expect count_first(["a"]) == 1

apply_to : (a -> b), a -> b
apply_to = |g, x| g(x)

apply_lambda : k -> k
apply_lambda = |x| apply_to(|p| p, x)

expect apply_lambda("a") == "a"

identity : k -> k
identity = |p| p

apply_named : k -> k
apply_named = |x| apply_to(identity, x)

expect apply_named("a") == "a"

apply_unannotated = |x| apply_to(|p| p, x)

expect apply_unannotated("a") == "a"

returns_closure : k -> (k -> k)
returns_closure = |_x| |p| p

expect returns_closure("a")("b") == "b"

keys_of : List((k, v)) -> List(k)
keys_of = |xs| List.fold(xs, [], |acc, (k, _v)| List.append(acc, k))

expect List.len(keys_of([("a", 1)])) == 1

total_until : U64 -> U64
total_until = |bound| (0.U64).until(bound).fold(0, |sum, n| sum + n)

expect total_until(3) == 3

unbox_it : Box(a) -> a
unbox_it = |boxed| Box.unbox(boxed)

expect Str.inspect(unbox_it(Box.box("hi"))) == "\"hi\""

rebox : Box(a) -> Box(a)
rebox = |b| Box.box(Box.unbox(b))

through_box : a -> a
through_box = |x| Box.unbox(rebox(Box.box(x)))

expect Str.inspect(through_box("hi")) == "\"hi\""

# Callable arguments whose own parameters carry descriptors.

write_item : U64, (U64 -> Try(U64, err)) -> Try(U64, err)
write_item = |state, write_value| {
	encoded = write_value(state)?
	Ok(encoded + 1)
}

apply_writer : (U64, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
apply_writer = |w| w(1, |x| Ok(x))

expect {
	r : Try(U64, [Oops])
	r = apply_writer(write_item)
	r == Ok(2)
}

run_open : (Str -> Try(U64, err)), Str -> Try(U64, err)
run_open = |f, s| {
	n = f(s)?
	Ok(n + 1)
}

expect {
	g : Str -> Try(U64, Str)
	g = |s| Err(s)
	r = run_open(g, "x")
	r == Err("x")
}

# Set and Dict through their generic implementations.

expect {
	s = Set.from_list(["a", "b", "a"])
	Set.len(s) == 2
}

expect {
	d = Dict.single(Dict.single("k", "inner"), "nested")
	d.get(Dict.single("k", "inner")) == Ok("nested")
}

expect {
	v : Try(Set(Str), [InvalidJson(Str)])
	v = Json.parse("[\"x\", \"y\", \"x\"]")
	match v {
		Ok(s) => Set.len(s) == 2
		_ => False
	}
}

expect {
	v : Try(Dict(Str, U64), [InvalidJson(Str)])
	v = Json.parse("{\"a\":1}")
	match v {
		Ok(d) => Dict.len(d) == 1
		_ => False
	}
}

# Inspection of generic values and nominals.

expect Str.inspect(Dict.from_list([("a", 1.U64)])) == "Dict.from_list([(\"a\", 1)])"

expect Str.inspect(Set.from_list(["a"])) == "Set.from_list([\"a\"])"

show : a -> Str
show = |x| Str.inspect(x)

expect show({ v: "x" }) == "{ v: \"x\" }"

Wrapped(a) := [W(a)].{
	to_inspect : Wrapped(a) -> Str where [a.to_inspect : a -> Str]
	to_inspect = |w| match w {
		W(v) => "W(${Str.inspect(v)})"
	}
}

expect Str.inspect(Wrapped.W("x")) == "W(\"x\")"

# Derived encoders.

expect Json.to_str("x") == "\"x\""

expect Json.to_str({ a: "x" }) == "{\"a\":\"x\"}"

expect Json.to_str({ a: 1.U64 }) == "{\"a\":1}"

expect Json.to_str(("x", "y")) == "[\"x\",\"y\"]"

expect Json.to_str(["x"]) == "[\"x\"]"

expect Json.to_str([1.0, 2.0]) == "[1.0,2.0]"

expect Json.to_str(Foo) == "\"Foo\""

expect Json.to_str([Foo, Bar]) == "[\"Foo\",\"Bar\"]"

expect Json.to_str(A("x", "y")) == "{\"A\":[\"x\",\"y\"]}"

expect Json.to_str(Dict.from_list([("a", 1.U64)])) == "{\"a\":1}"

expect Json.to_str(Dict.from_list([(Red, 1.U64)])) == "{\"Red\":1}"

expect Json.to_str(Set.from_list(["a"])) == "[\"a\"]"

# A generic function encoding a record or tuple built from its own type
# variables forwards its dictionary to the generic encoder, which describes the
# record's presence-slot payloads through the record's own descriptor.

record_json = |a, b| Json.to_str({ a, b })

expect record_json("x", "y") == "{\"a\":\"x\",\"b\":\"y\"}"

tuple_json = |a, b| Json.to_str((a, b))

expect tuple_json("x", "y") == "[\"x\",\"y\"]"
