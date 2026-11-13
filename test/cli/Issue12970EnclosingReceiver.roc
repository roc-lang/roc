Issue12970EnclosingReceiver := [].{
	distinct : List(k) -> U64
		where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
	distinct = |keys| Set.from_list(keys).len()

	# `labelled` generalizes while `keys` is unknown, `distinct(keys)` then
	# makes `keys` a list, and the later use of `labelled` must still mean the
	# enclosing function's own `keys`.
	returned : Str -> (List(U64) -> List(Str))
	returned = |prefix| |keys| {
		labelled = |suffix| keys.map(|k| "${prefix}${k.to_str()}${suffix}")
		n = distinct(keys)
		labelled("/${n.to_str()}")
	}

	inferred = |prefix, keys| {
		labelled = |suffix| keys.map(|k| "${prefix}${k.to_str()}${suffix}")
		n = distinct(keys)
		labelled("/${n.to_str()}")
	}
}

expect Issue12970EnclosingReceiver.returned("#")([7, 7]) == ["#7/1", "#7/1"]
expect Issue12970EnclosingReceiver.inferred("#", [7.U64, 7]) == ["#7/1", "#7/1"]
expect Issue12970EnclosingReceiver.inferred("#", ["a", "b"]) == ["#a/2", "#b/2"]
