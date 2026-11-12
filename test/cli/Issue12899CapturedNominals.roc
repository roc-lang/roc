# Explicit parameters and transitive/recursive captures must use the same
# instantiation rule under both specialization strategies.
Issue12899CapturedNominals :: [].{}

nested : a, b -> Str
nested = |value, _unused| {
	Inner(t) := [Inner(a, t)]
	Outer := [Outer(Inner(Str))]
	Str.inspect([Outer.Outer(Inner.Inner(value, "hi"))])
}

recursive : a -> Str
recursive = |value| {
	Chain := [End, Next(a, Chain)]
	Str.inspect(Chain.Next(value, Chain.End))
}

same_name : a -> Str
same_name = |x| {
	Captured := [Captured(a)]
	Pair(a) := [Pair(Captured, a)]
	value : Pair(Str)
	value = Pair.Pair(Captured.Captured(x), x)
	Str.inspect(value)
}

expect nested(5.U64, "unused") == "[Outer(Inner(5, \"hi\"))]"
expect nested("hello", 0.U8) == "[Outer(Inner(\"hello\", \"hi\"))]"
expect nested(5.U64, 0.U16) == "[Outer(Inner(5, \"hi\"))]"
expect recursive(5.U64) == "Next(5, End)"
expect recursive("hi") == "Next(\"hi\", End)"
expect same_name(5.U64) == "Pair(Captured(5), 5)"
expect same_name("hi") == "Pair(Captured(\"hi\"), \"hi\")"
