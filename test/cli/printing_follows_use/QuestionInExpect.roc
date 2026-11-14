# The value `?` reports when it meets an `Err` inside an `expect` prints as
# the expect's type has it: here the nominal type a use made the record.

QuestionInExpect :: [].{}

Shown := { id : U64 }.{
	to_inspect : Shown -> Str
	to_inspect = |s| Str.concat("shown ", s.id.to_str())
}

failing = |id| {
	r = { id: id }
	Err(r)
}

expect {
	x : Try(Shown, Shown)
	x = failing(3)
	v = x?
	v.id == 3
}
