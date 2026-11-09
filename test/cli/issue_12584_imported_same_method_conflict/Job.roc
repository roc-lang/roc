Job := [].{
	run : _ -> {}
	run = |db| Job.create(db)

	create = |db| {
		_a = Job.first(db)
		_b = Job.second(db)
		{}
	}

	first : _ -> List(Bool)
	first = |db| db.query(|b| b)

	second : _ -> List(Str)
	second = |db| db.query(|b| Str.inspect(b))
}
