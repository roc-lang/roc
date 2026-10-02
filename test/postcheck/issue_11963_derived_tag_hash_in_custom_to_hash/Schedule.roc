import Rule

Schedule :: { rule : Rule }.{
	new : Rule -> Schedule
	new = |rule| { rule: rule }

	is_eq : Schedule, Schedule -> Bool
	is_eq = |a, b| Rule.definition(a.rule) == Rule.definition(b.rule)

	to_hash : Schedule, Hasher -> Hasher
	to_hash = |value, hasher| Rule.definition(value.rule).to_hash(hasher)
}
