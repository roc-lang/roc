DerivedTagMap :: [].{}

Maybe(a) : [Just(a), Nothing]

NamedMaybe(a) := [NamedJust(a), NamedNothing].{
	is_eq : _
	to_hash : _
	map : _
	map! : _
}

OpaqueMaybe(a) :: [OpaqueJust(a), OpaqueNothing].{
	map : _
}

ConcreteMaybe := [ConcreteJust(U64), ConcreteNothing].{
	map : _
}

MyResult(ok, err) := [MyOk(ok), MyErr(err)].{
	map : _
}

ZstWrapper := [Wrapped({})].{}

NestedParameterMaybe(a) := [NestedParameter({ value : a }), NestedParameterNothing].{
	map : _
}

CustomMaybe(a) := [CustomJust(a), CustomNothing].{
	map : CustomMaybe(a), (a -> b) -> CustomMaybe(b)
	map = |_, _| CustomNothing
}

FunctionBox := [Holds({} -> {})].{
	is_eq : FunctionBox, FunctionBox -> Bool
	is_eq = |_, _| True

	to_hash : FunctionBox, Hasher -> Hasher
	to_hash = |_, hasher| hasher
}

expect {
	value : Maybe(U64)
	value = Just(41)

	value.map(|number| number + 1) == Just(42)
}

map_with_empty_payload : [Chosen(U64), Impossible([])] -> [Chosen(Str), Impossible([])]
map_with_empty_payload = |value| value.map(U64.to_str)

expect map_with_empty_payload(Chosen(42)) == Chosen("42")

expect {
	value : [Selected(U64), Ignored(ZstWrapper)]
	value = Selected(42)

	value.map(U64.to_str) == Selected("42")
}

expect {
	value : MyResult(U64, {})
	value = MyOk(42)

	match value.map(U64.to_str) {
		MyOk("42") => True
		_ => False
	}
}

expect {
	value : NestedParameterMaybe(U64)
	value = NestedParameter({ value: 41 })

	match value.map(|record| { value: record.value + 1 }) {
		NestedParameter({ value: 42 }) => True
		_ => False
	}
}

expect {
	value : CustomMaybe(U64)
	value = CustomJust(42)

	match value.map(U64.to_str) {
		CustomNothing => True
		_ => False
	}
}

expect {
	value : Maybe(U64)
	value = Nothing

	value.map(|number| number + 1) == Nothing
}

# When every payload is zero-sized, the sole payload of the sole unary tag
# is selected without needing an annotation on the transform.
expect Just({}).map(|_| "mapped") == Just("mapped")

expect {
	value : [Stuff({}, U64, [Blah], [Etc], { blah : {} }), Etc({}, [Whatever]), Other([Blah])]
	value = Stuff({}, 42, Blah, Etc, { blah: {} })
	mapped = value.map(U64.to_str)

	mapped == Stuff({}, "42", Blah, Etc, { blah: {} })
}

expect {
	value : NamedMaybe(U64)
	value = NamedJust(42)

	match value.map(U64.to_str) {
		NamedJust("42") => True
		_ => False
	}
}

expect {
	left : NamedMaybe(U64)
	left = NamedJust(42)
	right : NamedMaybe(U64)
	right = NamedJust(42)

	left == right
}

expect {
	left : [Outer(NamedMaybe(U64))]
	left = Outer(NamedJust(42))
	right : [Outer(NamedMaybe(U64))]
	right = Outer(NamedJust(42))

	left == right
}

expect {
	value : NamedMaybe(U64)
	value = NamedJust(42)

	Set.contains(Set.single(value), value)
}

expect {
	value : [Outer(NamedMaybe(U64))]
	value = Outer(NamedJust(42))

	Set.contains(Set.single(value), value)
}

expect {
	left : [Outer(FunctionBox)]
	left = Outer(Holds(|{}| {}))
	right : [Outer(FunctionBox)]
	right = Outer(Holds(|{}| {}))

	(left == right) and Set.contains(Set.single(left), right)
}

expect {
	value : OpaqueMaybe(U64)
	value = OpaqueJust(42)

	match value.map(U64.to_str) {
		OpaqueJust("42") => True
		_ => False
	}
}

expect {
	value : ConcreteMaybe
	value = ConcreteJust(41)

	match value.map(|number| number + 1) {
		ConcreteJust(42) => True
		_ => False
	}
}

effectful_map! : Maybe(U64) => Maybe(U64)
effectful_map! = |value| value.map!(|number| number + 1)
