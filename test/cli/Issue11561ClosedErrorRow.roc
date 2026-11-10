# A nested custom parser must use the format's whole error type, including
# payloads. An incompatible parser is rejected whether the enclosing
# annotation belongs to a value or a function.
Issue11561ClosedErrorRow :: [].{}

Token := {}.{
	parser_for : encoding -> (state -> Try({ value : Token, rest : state }, [InvalidJson(U8)]))
	parser_for = |_| |_| Err(InvalidJson(7))
}

parse_x : Str -> Try({ x : Token }, [InvalidJson(Str)])
parse_x = |s| Json.parse(s)

expect {
	v : Try({ x : Token }, [InvalidJson(Str)])
	v = Json.parse("{\"x\":null}")
	v.is_err()
}

expect parse_x("{\"x\":null}").is_err()
