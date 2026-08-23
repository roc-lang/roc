Issue10730AssociatedBlockExpects :: U64.{
	bar = |n| Issue10730AssociatedBlockExpects.(n)

	baz = |Issue10730AssociatedBlockExpects.(n)| n

	expect bar(5).baz() == 5

	# An associated expect can reference an item declared later in the block.
	expect quadruple(3) == 12

	quadruple : U64 -> U64
	quadruple = |n| n * 4

	Nested := [Yes, No].{
		flip = |Nested.(tag)| match tag {
			Yes => Nested.(No)
			No => Nested.(Yes)
		}

		to_num = |Nested.(tag)| match tag {
			Yes => 1
			No => 2
		}

		expect Nested.(No).flip().to_num() == 1
		expect Nested.(Yes).flip().to_num() == 2
	}
}

expect Issue10730AssociatedBlockExpects.bar(7).baz() == 7
