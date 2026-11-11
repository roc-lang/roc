# A nested parser with an incompatible error payload is rejected once at the
# annotated constant's parse call. Its later use must not report it again.
Host := { name : Str }.{
	parser_for : encoding -> (state -> Try({ value : Host, rest : state }, [InvalidJson(U8)]))
	parser_for = |_| |_| Err(InvalidJson(7))
}

config : Try({ host : Host }, [InvalidJson(Str)])
config = Json.parse("{\"host\":null}")

main! = |_args| {
	echo!("before")
	host = match config {
		Ok(parsed) => parsed.host.name
		Err(InvalidJson(message)) => message
	}
	echo!(host)
	Ok({})
}
