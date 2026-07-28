app [main!] { pf: platform "platform/main.roc" }

T(a, b) := [Two(a, b), One(b)].{
	map : T(a, b), (a -> c) -> T(c, b)
	map = |value, transform|
		match value {
			Two(first, second) => Two(transform(first), second)
			One(second) => One(second)
		}
}

main! = || {
	v : T(U8, U8)
	v = One(0)
	_ = v.map(|x| x)
}
