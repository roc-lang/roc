# repro for https://github.com/roc-lang/roc/issues/12718
# A derived JSON parser for a record whose field is a Dict parses an empty
# object field into an empty Dict at every optimization level. The input
# depends on the command line so the parse runs at runtime.
main! = |args| {
	suffix = Str.join_with(args.drop_first(1), "")

	parsed : Try({ files : Dict(Str, Str) }, _)
	parsed = Json.parse(Str.concat("{\"files\": {}}", suffix))
	match parsed {
		Ok(r) => echo!("ok ${r.files.len().to_str()}\n")
		Err(_) => echo!("err\n")
	}

	topology : Try({ topology : Dict(Str, List(Str)) }, _)
	topology = Json.parse(Str.concat("{\"topology\":{}}", suffix))
	match topology {
		Ok(r) => echo!("ok ${r.topology.len().to_str()}\n")
		Err(_) => echo!("err\n")
	}

	Ok({})
}
