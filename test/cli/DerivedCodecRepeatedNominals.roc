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
