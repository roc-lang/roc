Bool := [True, False].{
	not : Bool -> Bool
	not = |bool| match bool {
		Bool.True => Bool.False
		Bool.False => Bool.True
	}

	equals : Bool, Bool -> Bool
	equals = |a, b| match a {
		Bool.True => b
		Bool.False => Bool.not(b)
	}
}
