BoxyStaticOverrideAdapter :: [].{}

# A closed nominal whose `is_eq` override requires `Str.split_first` at a
# wider error row than the method's own, which seals to `[NotFound]`: the
# override's dictionary for that requirement is an adapter converting the
# method's result, wherever the descriptor holding the override is built.
Pair(a) := { v : a }.{
	is_eq : Pair(a), Pair(a) -> Bool where [a.split_first : a, Str -> Try({ before : a, after : a }, [NotFound, Broken])]
	is_eq = |x, y|
		match x.v.split_first(",") {
			Ok(_) => match y.v.split_first(",") {
				Ok(_) => Bool.True
				Err(_) => Bool.False
			}
			Err(_) => Bool.False
		}
}

same : t, t -> Bool where [t.is_eq : t, t -> Bool]
same = |x, y| x == y

split : Pair(Str)
split = { v: "a,b" }

whole : Pair(Str)
whole = { v: "ab" }

expect same([split], [split])
expect !same([split], [whole])
expect split == split
