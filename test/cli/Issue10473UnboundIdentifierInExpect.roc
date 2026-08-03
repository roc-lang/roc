Issue10473UnboundIdentifierInExpect :: {}.{}

expect {
	match Bool.True {
		Bool.True => Bool.True,
		_ => false,
	}
}
