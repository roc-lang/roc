Lib := [].{
	Thing : { value : Str }

	make : Str -> Lib.Thing
	make = |value| { value: value }
}
