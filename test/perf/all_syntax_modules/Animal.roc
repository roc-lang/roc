import Shared

# A nominal type with a custom is_eq method
Animal := [Dog(Str), Cat(Str)].{
	is_eq = |a, b| match (a, b) {
		(Dog(name1), Dog(name2)) => name1 == name2
		(Cat(name1), Cat(name2)) => name1 == name2
		_ => Bool.False
	}

	describe : Animal -> Str
	describe = |animal| match animal {
		Dog(name) => Shared.label("Dog", name)
		Cat(name) => Shared.label("Cat", name)
	}

	# `...` is translated to `crash "not implemented"`.
	make_noise : Animal -> Str
	make_noise = |_animal| ...
}
