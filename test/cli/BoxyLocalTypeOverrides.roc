BoxyLocalTypeOverrides :: [].{}

# A type declared in a generic function body whose backing names the body's
# type variable is instantiated at that variable's type.
wrapped : a -> Str
wrapped = |x| {
	Wrap := [Wrap(a)]
	Str.inspect([Wrap.Wrap(x)])
}

expect wrapped(5.U64) == "[Wrap(5)]"
expect wrapped("hi") == "[Wrap(\"hi\")]"

# A method generic in its receiver captures its own type's descriptor.
custom : a -> Str
custom = |x| {
	Local := [Local(a)].{
		to_inspect = |_value| "custom"
	}
	Str.inspect([Local.Local(x)])
}

expect custom(5.U64) == "[custom]"

# A method whose signature names the body's type variable stays a local
# procedure of the body: comparing values compares through it.
never_equal : a -> Bool
never_equal = |x| {
	Local := [Local(a)].{
		is_eq : Local, Local -> Bool
		is_eq = |_, _| Bool.False
	}
	value = Local.Local(x)
	[value] == [value]
}

expect never_equal(5.U64) == Bool.False
expect never_equal("hi") == Bool.False

# A closed type's method that names the body's type variable is a local
# procedure too, so the closed type's descriptor is composed in the body.
closed_never_equal : a -> Bool
closed_never_equal = |_x| {
	Tag := [Tag(U64)].{
		is_eq = |_, _| {
			empty : List(a)
			empty = []
			List.len(empty) > 0
		}
	}
	value = Tag.Tag(1)
	[value] == [value]
}

expect closed_never_equal(5.U64) == Bool.False
