import Shared

# A record builder combines wrapped field values using the suffix type's `map2`.
Builder(a) := { value : a }.{
	pure : a -> Builder(a)
	pure = |x| { value: x }

	map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
	map2 = |a, b, combine| { value: combine(a.value, b.value) }

	run : Builder(a) -> a
	run = |builder| builder.value

	size_hint : U64
	size_hint = Shared.perf_edit + 2
}
