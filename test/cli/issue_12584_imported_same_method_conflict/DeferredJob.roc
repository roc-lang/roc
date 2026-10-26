DeferredJob := [].{
	count : _ -> Try(U64, _)
	count = |db| db.get(|n| n)

	name : _ -> Try(Str, _)
	name = |db| db.get(|n| n.to_str())

	both = |args| {
		_ = count(args.db)
		_ = name(args.db)
		Ok({})
	}

	run : _ -> Try({}, _)
	run = |args| both(args)
}
