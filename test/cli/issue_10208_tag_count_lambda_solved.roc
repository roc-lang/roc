main! : List(Str) => Try({}, [Exit(I8)])
main! = |_| {
	match read_and_handle_message({}) {
		Ok({}) => Ok({}),
		Err(_) => Err(Exit(1))
	}
}

Message : [
	INFO({ version: Str }),
	PING,
]

decode_message : {} -> Try(Message, Encoding.Json.ParseErr)
decode_message = |_| {
	s = Json.parse("")?
	Ok(INFO(s))
}

read_and_handle_message : {} -> Try({}, [StdoutErr(Str), InvalidJson(Str), MissingRequiredField(Str)])
read_and_handle_message = |_| {
	message = decode_message({})?
	match message {
		PING => Ok({}),
		INFO(_) => Ok({}),
	}
}
