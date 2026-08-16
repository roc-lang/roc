Issue10730AssociatedBlockExpectFails :: U64.{
	double = |Issue10730AssociatedBlockExpectFails.(n)| n * 2

	expect Issue10730AssociatedBlockExpectFails.(5).double() == 10
	expect Issue10730AssociatedBlockExpectFails.(5).double() == 11
}
