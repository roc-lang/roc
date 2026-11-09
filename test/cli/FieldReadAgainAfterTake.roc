FieldReadAgainAfterTake := {}

# Fields of a record that is about to be freed are read, passed to functions
# that take ownership of them, and read again, in different orders. Each
# expect passes only when every read still sees the field it names.

Tree := [Node(List(Tree)), Leaf(List(Str))]

consume : Tree -> List(Str)
consume = |tree| match tree {
	Leaf(items) => items
	Node(kids) => {
		var $out = []
		for kid in kids {
			$out = $out.concat(consume(kid))
		}
		$out
	}
}

find : Tree, Str -> Try(Str, [Missing])
find = |tree, name| match tree {
	Leaf(items) => items.find_first(|s| s == name).map_err(|_| Missing)
	Node(kids) => match kids.first() {
		Ok(kid) => find(kid, name)
		Err(_) => Err(Missing)
	}
}

View : { indexed : Bool, first : Tree }

lookup_first : View, Str -> Try(Str, [Missing])
lookup_first = |view, name| {
	if !view.indexed {
		return Err(Missing)
	}
	find(view.first, name)
}

indexed : Str -> { view : View, cache : Str }
indexed = |s| {
	items = ["a${s}", "b${s}"]
	{ view: { indexed: True, first: Node([Leaf(items)]) }, cache: "c${s}" }
}

pair : Str -> { a : Tree, b : Str }
pair = |s| {
	items = ["a${s}", "b${s}"]
	{ a: Node([Leaf(items)]), b: "c${s}" }
}

# The record is lent whole before its field is moved out of it.
expect {
	view = indexed("").view
	b = lookup_first(view, "a") == Ok("a")
	a = consume(view.first) == ["a", "b"]
	a and b
}

# The record is passed whole without any field read through it.
expect {
	view = indexed("").view
	lookup_first(view, "b") == Ok("b")
}

# The same field is read twice, and the first read is used after the second.
expect {
	r = pair("")
	t = r.a
	x = consume(r.a)
	y = consume(t)
	x == y
}
