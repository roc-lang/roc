## Dependency module for BoxyDemandedOverrides.roc: generic workers that
## inspect and compare values of a type their caller's module declares, so
## the caller fills their tables from the needs this module exports.
BoxyDemandedOverridesDep :: [].{
	render : a -> Str
	render = |value| Str.inspect(value)

	same : a, a -> Bool where [a.is_eq : a, a -> Bool]
	same = |left, right| left == right
}
