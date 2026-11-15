LiftingMaker := [].{
	Counter := { count : U64 }.{
		describe : Counter -> Str
		describe = |_| "counter"
	}

	make = |n| { count: n }

	unit = |_n| {}

	lifted : Counter
	lifted = make(1)

	Token := {}.{
		describe : Token -> Str
		describe = |_| "token"
	}

	lifted_token : Token
	lifted_token = unit(1)
}
