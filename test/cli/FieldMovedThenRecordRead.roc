FieldMovedThenRecordRead := {}

# repro for https://github.com/roc-lang/roc/issues/12436
# A record that is a field of a returned record has one of its fields passed
# to a function that consumes it, and afterwards the whole record is passed to
# a function that reads that same field again. The field must still be valid
# at the second read, so the expect passes.

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

expect {
	view = indexed("").view
	a = consume(view.first) == ["a", "b"]
	b = lookup_first(view, "a") == Ok("a")
	a and b
}
