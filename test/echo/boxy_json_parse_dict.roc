# A derived JSON dict parser reads each entry through the format's dict
# protocol. The input depends on the command line so the parse runs at runtime.
main! = |args| {
	suffix = Str.join_with(args.drop_first(1), "")

	counts : Try(Dict(Str, U64), _)
	counts = Json.parse(Str.concat("{\"a\": 1, \"b\": 2}", suffix))
	match counts {
		Ok(dict) => echo!("${Str.inspect(dict.get("b"))} ${Str.inspect(dict.len())}\n")
		Err(err) => echo!("${Str.inspect(err)}\n")
	}

	Ok({})
}
