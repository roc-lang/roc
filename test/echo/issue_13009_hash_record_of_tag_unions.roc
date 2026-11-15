# repro for https://github.com/roc-lang/roc/issues/13009
# Set and Dict keys that are records of ten three-variant tag-union fields.
# Their derived `to_hash` builds and runs at every optimization level, equal
# records land on the same key, and records differing in any one field, in
# variant or in payload, are distinct keys. The values depend on the command
# line so hashing runs at runtime.
Status : [Loading, Ready(Str), Failed(Str)]

Big : {
	f0 : Status,
	f1 : Status,
	f2 : Status,
	f3 : Status,
	f4 : Status,
	f5 : Status,
	f6 : Status,
	f7 : Status,
	f8 : Status,
	f9 : Status,
}

uniform : Status -> Big
uniform = |s| { f0: s, f1: s, f2: s, f3: s, f4: s, f5: s, f6: s, f7: s, f8: s, f9: s }

main! = |args| {
	word = Str.join_with(args.drop_first(1), "")
	base = uniform(Loading)
	ready = uniform(Ready(word))
	failed = uniform(Failed(word))
	last_differs = { ..base, f9: Ready(word) }
	first_differs = { ..base, f0: Failed(word) }
	payload_differs = { ..ready, f5: Ready(Str.concat(word, "!")) }

	set =
		Set.empty()
			.insert(base)
			.insert(ready)
			.insert(failed)
			.insert(last_differs)
			.insert(first_differs)
			.insert(payload_differs)
			.insert(uniform(Loading))
			.insert(uniform(Ready(word)))
	echo!("set ${set.len().to_str()} ${Str.inspect(set.contains({ ..ready, f5: Ready(word) }))} ${Str.inspect(set.contains({ ..failed, f3: Loading }))}\n")

	dict =
		Dict.empty()
			.insert(base, 0)
			.insert(ready, 1)
			.insert(failed, 2)
			.insert(last_differs, 3)
			.insert(payload_differs, 4)
			.insert(uniform(Ready(word)), 5)
	echo!("dict ${dict.len().to_str()} ${Str.inspect(dict.get(ready))} ${Str.inspect(dict.get(payload_differs))} ${Str.inspect(dict.get(first_differs))}\n")
	Ok({})
}
