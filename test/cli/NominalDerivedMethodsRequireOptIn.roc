NominalDerivedMethodsRequireOptIn :: [].{}

NoOptIn := [NoOptInValue(U64), NoOptInNothing].{}

bad_map : NoOptIn -> NoOptIn
bad_map = |value| value.map(|number| number + 1)

expect {
	left : NoOptIn
	left = NoOptInValue(1)
	right : NoOptIn
	right = NoOptInValue(1)

	left == right
}

bad_hash : NoOptIn, Hasher -> Hasher
bad_hash = |value, hasher| value.to_hash(hasher)

bad_structural_equality : [Wrapped(NoOptIn)], [Wrapped(NoOptIn)] -> Bool
bad_structural_equality = |left, right| left == right
