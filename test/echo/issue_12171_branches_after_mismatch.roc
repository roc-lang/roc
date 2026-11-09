# Each conditional below has a branch whose value does not match the earlier
# branches; that branch is a type error. The branches after it are not
# compared with the others, and the `fallback` they read keeps its own `Bool`
# type, so `choose` still returns `fallback` from its first branch. Reading
# `Mode.current` evaluates its mismatching branch and crashes.
Mode := [Off, Invalid, On].{
	current : Mode
	current = {
		fallback = Bool.False
		default_mode = Off
		if (
			match default_mode {
				Off => fallback
				Invalid => Off
				On => fallback
			}
		) Off else Off
	}
}

choose : Bool -> Bool
choose = |c| {
	fallback = Bool.False
	if c fallback else if c Off else if c fallback else Bool.True
}

main! = |_| {
	echo!("before")
	echo!(Str.inspect(choose(Bool.True)))
	echo!(Str.inspect(Mode.current))
	Ok({})
}
