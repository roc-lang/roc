app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Each function below returns literals that are already their own values,
# which their bodies keep rather than evaluating them at compile time on their
# own. A literal with a heap part (a list, a long string, a recursive payload)
# is read from the program image, so calling these functions allocates
# nothing, and the values keep the image's reference count: appending to one
# copies it and leaves the literal as it was for the next call.
Tree := [Leaf, Node(Tree, U64, Tree)]

names : U64 -> List(Str)
names = |n| if n == 0 [] else ["first", "a string literal that is far too long to be held inline", "third"]

points : U64 -> List({ x : I64, y : I64 })
points = |n| if n == 0 [] else [{ x: 1, y: 2 }, { x: 3, y: 4 }]

# A block with no statements is its final expression.
blocks : U64 -> List(Str)
blocks = |n| if n == 0 { [] } else { ["one", "a block's string literal that is far too long to be held inline"] }

nested : U64 -> List(List(U8))
nested = |n| if n == 0 [] else [[1, 2], [], [3]]

tree : U64 -> Tree
tree = |n| if n == 0 Tree.Leaf else Tree.Node(Tree.Leaf, 5, Tree.Node(Tree.Leaf, 6, Tree.Leaf))

total : Tree -> U64
total = |t| match t {
	Leaf => 0
	Node(left, value, right) => total(left) + value + total(right)
}

# The literal is part of a call that also reads runtime data, so the call
# runs at runtime and the literal alone is kept as data.
labels : Str -> List(Str)
labels = |extra| List.concat(["one label", "another label that is far too long to be held inline"], [extra])

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input) + 1
	start = Host.alloc_count!()

	var $sum = 0
	var $i = 0
	while $i < 1000 {
		$sum = $sum + List.len(names(n)) + List.len(points(n)) + List.len(nested(n)) + List.len(blocks(n)) + total(tree(n))
		$i = $i + 1
	}
	read_allocs = Host.alloc_count!() - start

	first = List.append(names(n), input)
	second = List.append(names(n), input)
	append_allocs = Host.alloc_count!() - start - read_allocs

	var $count = 0
	var $j = 0
	while $j < 1000 {
		$count = $count + List.len(labels(input))
		$j = $j + 1
	}
	concat_allocs = Host.alloc_count!() - start - read_allocs - append_allocs

	expect read_allocs == 0
	expect append_allocs == 2
	expect concat_allocs == 2000
	expect List.len(first) == 4
	expect List.len(names(n)) == 3
	expect List.first(second) == Ok("first")
	"sum: ${$sum.to_str()}, labels: ${$count.to_str()}, allocations: ${read_allocs.to_str()} ${append_allocs.to_str()} ${concat_allocs.to_str()}"
}
