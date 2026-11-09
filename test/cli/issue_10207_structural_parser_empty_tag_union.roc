main! : List(Str) => Try({}, [Exit(I8)])
main! = |_| {
	match read_and_handle_message("") {
		Ok({}) => Ok({}),
		Err(_) => Err(Exit(1))
	}
}

Message : [
	INFO,
]

decode_message : Str -> Try(Message, [InvalidJson(Str)])
decode_message = |input| {
	s = Json.parse(input)?
	Ok(INFO)
}

read_and_handle_message : Str -> Try({}, [StdoutErr(Str), InvalidJson(Str)])
read_and_handle_message = |input| {
	message = decode_message(input)?
	match message {
		INFO => Ok({}),
	}
}
