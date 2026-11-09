# `Widget.is_enabled` takes a `Bool`, so calling it with `Widget.Enabled` is a
# type error and the `if` condition is a runtime error. The `if` is a match
# branch's value, lowered at the `match`'s result, and crashes before choosing
# a branch.
Widget := [Enabled, Disabled].{
	is_enabled : Bool -> Bool
	is_enabled = |flag_input| flag_input

	current : Widget
	current = {
		settings = { enabled: Bool.True }
		match settings {
			{ enabled: _ } =>
				if Widget.is_enabled(Widget.Enabled) Widget.Enabled else Widget.Disabled
		}
	}
}

main! = |_| {
	echo!("before")
	echo!(Str.inspect(Widget.current))
	Ok({})
}
