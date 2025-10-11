Bool := [True, False].{
	not : Bool -> Bool
	not = |bool| match bool {
		Bool.True => Bool.False
		Bool.False => Bool.True
	}
}
