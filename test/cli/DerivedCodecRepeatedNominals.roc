DerivedCodecRepeatedNominals :: [].{}

Plain := { name : Str }.{
	parser_for : _
	encoder_for : _
}

Choice := [A, B(Str)].{ parser_for : _ }

Node := { name : Str, kids : List(Node) }.{ parser_for : _ }

Tree := [Leaf(Str), Node(List(Tree))].{
	encoder_for : _
	parser_for : _
}

Inner := [Leaf(Str), Stop].{ parser_for : _ }

Outer := [Wrap(List(Inner))].{ parser_for : _ }

# A nominal with a hand-written parser that needs its payload's parser.
Opt(a) := [
	None,
	Has(a),
].{
	map : Opt(a), (a -> b) -> Opt(b)
	map = |o, f|
		match o {
			Has(a) => Has(f(a))
			None => None
		}

	with_default : Opt(a), a -> a
	with_default = |o, default|
		match o {
			Has(a) => a
			None => default
		}

	parser_for : encoding -> (state -> Try({ value : Opt(a), rest : state }, [InvalidJson(Str), MissingRequiredField(Str)]))
		where [
			a.parser_for : encoding -> (state -> Try({ value : a, rest : state }, [InvalidJson(Str), MissingRequiredField(Str)])),
			encoding.parse_null : encoding, state -> Try(state, [InvalidJson(Str)]),
		]
	parser_for = |encoding| {
		Elem : a
		parse_elem = Elem.parser_for(encoding)

		|state|
			match encoding.parse_null(state) {
				Ok(rest) => Ok({ value: None, rest })
				Err(InvalidJson(_)) =>
					match parse_elem(state) {
						Ok(parsed) => Ok({ value: Has(parsed.value), rest: parsed.rest })
						Err(InvalidJson(e)) => Err(InvalidJson(e))
						Err(MissingRequiredField(f)) => Err(MissingRequiredField(f))
					}
			}
	}
}

PlainAlias : Plain

Holder(a) := { x : a }.{
	parser_for : _
	encoder_for : _
}

Left := { right : List(Right) }.{ parser_for : _ }

Right := { left : List(Left), name : Str }.{ parser_for : _ }

right_names : Left -> Str
right_names = |Left.({ right })|
	List.fold(right, "", |acc, Right.({ left, name })| Str.concat(Str.concat(acc, name), Str.join_with(List.map(left, right_names), "")))

name_of : Plain -> Str
name_of = |Plain.({ name })| name

names : Node -> Str
names = |Node.({ name, kids })|
	List.fold(kids, name, |acc, kid| Str.concat(acc, names(kid)))

describe_choice : Choice -> Str
describe_choice = |choice|
	match choice {
		A => "A"
		B(s) => s
	}

describe_inner : Inner -> Str
describe_inner = |inner|
	match inner {
		Leaf(s) => s
		Stop => "."
	}

# One derived nominal in two fields of a record.
expect {
	v : Try({ c : Plain, d : Plain }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"c\":{\"name\":\"a\"},\"d\":{\"name\":\"b\"}}")
	match v {
		Ok({ c, d }) => Str.concat(name_of(c), name_of(d)) == "ab"
		Err(_) => False
	}
}

expect Json.to_str({ c: Plain.({ name: "a" }), d: Plain.({ name: "b" }) }) == "{\"c\":{\"name\":\"a\"},\"d\":{\"name\":\"b\"}}"

expect {
	v : Try({ c : Choice, d : Choice }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"c\":\"A\",\"d\":{\"B\":\"x\"}}")
	match v {
		Ok({ c, d }) => Str.concat(describe_choice(c), describe_choice(d)) == "Ax"
		Err(_) => False
	}
}

# The nominal first as a list element, then as a field.
expect {
	v : Try({ c : List(Plain), d : Plain }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"c\":[{\"name\":\"a\"},{\"name\":\"b\"}],\"d\":{\"name\":\"c\"}}")
	match v {
		Ok({ c, d }) => Str.concat(Str.join_with(List.map(c, name_of), ""), name_of(d)) == "abc"
		Err(_) => False
	}
}

# A recursive derived record.
expect {
	v : Try(Node, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"name\":\"a\",\"kids\":[{\"name\":\"b\",\"kids\":[{\"name\":\"c\",\"kids\":[]}]},{\"name\":\"d\",\"kids\":[]}]}")
	match v {
		Ok(node) => names(node) == "abcd"
		Err(_) => False
	}
}

# A recursive derived tag union, encoded and parsed back.
expect {
	encoded = Json.to_str(Tree.Node([Tree.Leaf("a"), Tree.Node([Tree.Leaf("b")]), Tree.Node([])]))
	v : Try(Tree, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse(encoded)
	match v {
		Ok(tree) => encoded == "{\"Node\":[{\"Leaf\":\"a\"},{\"Node\":[{\"Leaf\":\"b\"}]},{\"Node\":[]}]}" and Json.to_str(tree) == encoded
		Err(_) => False
	}
}

# A derived tag union in a list payload of another derived tag union.
expect {
	v : Try(Outer, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"Wrap\":[{\"Leaf\":\"a\"},\"Stop\",{\"Leaf\":\"b\"}]}")
	match v {
		Ok(Wrap(items)) => Str.join_with(List.map(items, describe_inner), "") == "a.b"
		Err(_) => False
	}
}

# A list of derived tag unions at the root.
expect {
	v : Try(List(Inner), [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("[{\"Leaf\":\"a\"},\"Stop\"]")
	match v {
		Ok(items) => Str.join_with(List.map(items, describe_inner), "") == "a."
		Err(_) => False
	}
}

# Two fields of one derived nominal, one spelled through an alias.
expect {
	v : Try({ c : Plain, d : PlainAlias }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"c\":{\"name\":\"a\"},\"d\":{\"name\":\"b\"}}")
	match v {
		Ok({ c, d }) => Str.concat(name_of(c), name_of(d)) == "ab"
		Err(_) => False
	}
}

# A generic derived nominal at one argument twice and at another once.
expect {
	v : Try({ c : Holder(Str), d : Holder(Str), e : Holder(U8) }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"c\":{\"x\":\"a\"},\"d\":{\"x\":\"b\"},\"e\":{\"x\":7}}")
	match v {
		Ok({ c: Holder.({ x: c }), d: Holder.({ x: d }), e: Holder.({ x: e }) }) => c == "a" and d == "b" and e == 7
		Err(_) => False
	}
}

expect Json.to_str({ c: Holder.({ x: "a" }), d: Holder.({ x: "b" }), e: Holder.({ x: 7.U8 }) }) == "{\"c\":{\"x\":\"a\"},\"d\":{\"x\":\"b\"},\"e\":{\"x\":7}}"

# Mutually recursive derived parsers.
expect {
	v : Try(Left, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"right\":[{\"left\":[{\"right\":[{\"left\":[],\"name\":\"y\"}]}],\"name\":\"x\"}]}")
	match v {
		Ok(left) => right_names(left) == "xy"
		Err(_) => False
	}
}

# Two fields of one nominal with a hand-written parser.
expect {
	v : Try({ a : Opt(Str), b : Opt(Str) }, [InvalidJson(Str), MissingRequiredField(Str)])
	v = Json.parse("{\"a\":\"x\",\"b\":null}")
	match v {
		Ok({ a, b }) => Opt.with_default(a, "none") == "x" and Opt.with_default(b, "none") == "none"
		Err(_) => False
	}
}
