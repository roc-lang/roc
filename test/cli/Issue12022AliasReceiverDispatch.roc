# A receiver whose type is a transparent alias of a where-constrained type
# variable dispatches exactly as the variable itself does, in both
# specialization strategies.
Item := [Item(U64)].{
	score : Item -> U64
	score = |Item.Item(n)| n

	is_eq : Item, Item -> Bool
	is_eq = |Item.Item(a), Item.Item(b)| a % 10 == b % 10
}

Other := { v : U64 }.{
	score : Other -> U64
	score = |o| o.v * 2
}

Wrapper(a) : a

Twice(a) : Wrapper(a)

score_wrapped : Wrapper(a) -> U64 where [a.score : a -> U64]
score_wrapped = |value| value.score()

score_twice : Twice(a) -> U64 where [a.score : a -> U64]
score_twice = |value| value.score() + score_wrapped(value)

forward : Wrapper(b) -> U64 where [b.score : b -> U64]
forward = |value| score_twice(value) + 1

same : Wrapper(a), Wrapper(a) -> Bool where [a.is_eq : a, a -> Bool]
same = |x, y| x == y

expect score_wrapped(Item.Item(42)) == 42
expect score_twice(Item.Item(5)) == 10
expect forward(Other.{ v: 10 }) == 41
expect same(Item.Item(1), Item.Item(11))
expect !same(Item.Item(1), Item.Item(12))
