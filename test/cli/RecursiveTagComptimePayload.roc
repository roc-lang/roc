# A compile-time constant that lands in a recursive tag's payload.
#
# `build`'s argument keeps the outer tag out of the constant folder, so only the
# inner `Wrap("A", Leaf)` folds. A recursive payload slot holds a pointer, while
# the folded root's own procedure returns the union unboxed, so freezing the
# root has to write the payload as its own static node and point the slot at it.
RecursiveTagComptimePayload := [
	Leaf,
	Wrap(Str, RecursiveTagComptimePayload),
	Branch(Str, List(RecursiveTagComptimePayload)),
].{
	label : RecursiveTagComptimePayload -> Str
	label = |node| match node {
		Wrap(name, _) => name
		Branch(name, _) => name
		Leaf => "leaf"
	}

	depth : RecursiveTagComptimePayload -> U64
	depth = |node| match node {
		Wrap(_, inner) => 1 + RecursiveTagComptimePayload.depth(inner)
		Branch(_, children) => 1 + children.fold(
			0,
			|so_far, child| {
				child_depth = RecursiveTagComptimePayload.depth(child)
				if child_depth > so_far {
					child_depth
				} else {
					so_far
				}
			},
		)
		Leaf => 1
	}

	build : Str -> RecursiveTagComptimePayload
	build = |name| RecursiveTagComptimePayload.Wrap(
		name,
		RecursiveTagComptimePayload.Wrap("A", RecursiveTagComptimePayload.Leaf),
	)

	# The constant reaches a list payload rather than a direct one.
	branch : Str -> RecursiveTagComptimePayload
	branch = |name| RecursiveTagComptimePayload.Branch(
		name,
		[
			RecursiveTagComptimePayload.Wrap("B", RecursiveTagComptimePayload.Leaf),
			RecursiveTagComptimePayload.Leaf,
		],
	)

	# Two constants under one runtime-dependent tag.
	pair : Str -> RecursiveTagComptimePayload
	pair = |name| RecursiveTagComptimePayload.Branch(
		name,
		[
			RecursiveTagComptimePayload.Wrap("C", RecursiveTagComptimePayload.Leaf),
			RecursiveTagComptimePayload.Wrap("D", RecursiveTagComptimePayload.Leaf),
		],
	)
}

expect RecursiveTagComptimePayload.build("X").label() == "X"

expect RecursiveTagComptimePayload.build("X").depth() == 3

expect RecursiveTagComptimePayload.branch("Y").label() == "Y"

expect RecursiveTagComptimePayload.branch("Y").depth() == 3

expect RecursiveTagComptimePayload.pair("Z").depth() == 3

# The whole value folds, so the slot holds the union itself rather than a
# pointer to it. Both shapes have to keep working.
expect {
	whole = RecursiveTagComptimePayload.Wrap("W", RecursiveTagComptimePayload.Leaf)
	whole.depth() == 2
}
