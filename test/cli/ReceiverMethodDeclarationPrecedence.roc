# A receiver can have both a method declared on its own associated block and a
# receiver extension declared in an empty nominal namespace. The receiver's own
# declaration wins for method-call syntax, independently of source order, while
# both qualified functions remain available.

ReceiverMethodDeclarationPrecedence :: [].{}

Api := [].{
	value : Thing -> U64
	value = |_| 2
}

Thing := [Default].{
	value : Thing -> U64
	value = |_| 1
}

OtherThing := [Default].{
	value : OtherThing -> U64
	value = |_| 3
}

OtherApi := [].{
	value : OtherThing -> U64
	value = |_| 4
}

expect Api.value(Thing.Default) == 2

expect {
	thing = Thing.Default
	thing.value() == 1
}

expect OtherApi.value(OtherThing.Default) == 4

expect {
	thing = OtherThing.Default
	thing.value() == 3
}
