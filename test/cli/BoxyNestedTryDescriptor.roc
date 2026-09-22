BoxyNestedTryDescriptor :: [].{}

## A nominal nested inside another use of itself binds its declaration's
## formals again for the inner use.
f : Try(Try(U64, [Unset]), [NoMatch]) -> Try(Try(U64, [Unset]), [NoMatch])
f = |val| {
	match val {
		Err(NoMatch) => f(Ok(Err(Unset)))
		Ok(prev) => Ok(prev)
	}
}

expect {
	match f(Err(NoMatch)) {
		Ok(Err(Unset)) => Bool.True
		_ => Bool.False
	}
}
