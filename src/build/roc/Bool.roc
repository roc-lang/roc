Bool := [True, False].{
	not : Bool -> Bool
	not = |bool| match bool {
		Bool.True => Bool.False
		Bool.False => Bool.True
	}

	and : Bool, Bool -> Bool
	and = |a, b| match a {
		Bool.True => b
		Bool.False => Bool.False
	}

	or : Bool, Bool -> Bool
	or = |a, b| match a {
		Bool.True => Bool.True
		Bool.False => b
	}
}
