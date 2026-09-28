Issue11737Db(deps) :: { deps : deps }.{
	new = |deps| Issue11737Db.{ deps }

	run : Issue11737Db(_), _ -> _
	run = |db, body| {
		fetch = db.deps.fetch
		(fetch({}).repeat(3), body({}))
	}
}
