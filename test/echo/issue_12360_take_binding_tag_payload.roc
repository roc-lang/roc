# repro for https://github.com/roc-lang/roc/issues/12360
# `and_then` destructures `f(val)` into `inner_val` and `inner_thunks`,
# taking the list out of the dying tree, and hands `inner_thunks` to
# `List.concat`, which consumes it. The taken list must be released exactly
# once, including when `Tree.map` and `Tree.and_then` rebuild trees whose
# thunk lists recursively capture further trees.

Tree(a) := [Node(a, List(() -> Tree(a)))].{
	singleton : a -> Tree(a)
	singleton = |val| Tree.Node(val, [])

	map : Tree(a), (a -> b) -> Tree(b)
	map = |Tree.Node(val, thunks), f|
		Tree.Node(f(val), thunks.map(|thunk| || map(thunk(), f)))

	and_then : Tree(a), (a -> Tree(b)) -> Tree(b)
	and_then = |Tree.Node(val, thunks), f| {
		Tree.Node(inner_val, inner_thunks) = f(val)
		outer_shrunks = thunks.map(|thunk| || and_then(thunk(), f))
		Tree.Node(inner_val, List.concat(inner_thunks, outer_shrunks))
	}

	tuple2 : Tree(a), Tree(b) -> Tree((a, b))
	tuple2 = |ta, tb|
		and_then(ta, |a| map(tb, |b| (a, b)))

	shrink : U64, U64 -> Tree(U64)
	shrink = |current, target|
		if current == target {
			singleton(current)
		} else {
			var $acc = [0]
			var $step = current / 2
			while $step > 0 {
				$acc = $acc.append(current - $step)
				$step = $step / 2
			}
			Tree.Node(current, $acc.map(|h| || shrink(h, target)))
		}
}

Gen(a) := {
	generator : U64 -> (Tree(a), U64),
}.{
	run : Gen(a), U64 -> (Tree(a), U64)
	run = |gen, seed| (gen.generator)(seed)

	tuple2 : Gen(a), Gen(b) -> Gen((a, b))
	tuple2 = |ga, gb| {
		generator: |seed| {
			(tree_a, s1) = run(ga, seed)
			(tree_b, s2) = run(gb, s1)
			pair_tree = Tree.tuple2(tree_a, tree_b)
			mapped_tree = Tree.map(pair_tree, |(a, b)| (a, b))
			(mapped_tree, s2)
		},
	}

	u64 : Gen(U64)
	u64 = {
		generator: |seed| {
			val = (seed * 123 + 456) % 1000
			(Tree.shrink(val, 0), seed + 1)
		},
	}
}

expect {
	gen = Gen.tuple2(Gen.u64, Gen.u64)
	var $i = 0
	while $i < 20 {
		_ = Gen.run(gen, $i)
		$i = $i + 1
	}
	Bool.True
}
