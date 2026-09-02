app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

inspect_any : a -> Str
inspect_any = |value| Str.inspect(value)

main! = || {
	set = Set.from_list([1.I64, 2, 1, 3])
	Stdout.line!(inspect_any(set))

	dict = Dict.from_list([("a", 1.I64), ("b", 2), ("a", 3)])
	Stdout.line!(inspect_any(dict))

	empty_set : Set(I64)
	empty_set = Set.empty()
	Stdout.line!(inspect_any(empty_set))

	empty_dict : Dict(Str, I64)
	empty_dict = Dict.empty()
	Stdout.line!(inspect_any(empty_dict))

	nested = {
		labels: Set.from_list(["red", "blue"]),
		scores: Dict.from_list([("alice", 10.I64), ("bob", 20)]),
	}
	Stdout.line!(Str.inspect(nested))
}
