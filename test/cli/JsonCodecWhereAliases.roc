JsonCodecWhereAliases :: [].{}

# The Json codec constraints are nameable, so a generic helper can say
# "JSON-encodable" / "JSON-parseable" without naming the encoder's internal
# format, cursor, or output-state types.

FooDto := { bar : Str }.{
	encoder_for : _
	parser_for : _
}

to_json : a -> Str where [a.Json.Encodable([])]
to_json = |value| Json.to_str(value)

to_json_try : a -> Try(Str, err) where [a.Json.Encodable(err)]
to_json_try = |value| Json.to_str_try(value)

from_json : Str -> Try(a, [InvalidJson(Str)]) where [a.Json.Parseable([InvalidJson(Str)])]
from_json = |src| Json.parse(src)

read_headers : Str -> Try(a, [BadHeader, MissingHeader(Str)]) where [a.Encoding.HttpHeader.Parseable([BadHeader, MissingHeader(Str)])]
read_headers = |raw| Encoding.HttpHeader.parse(raw)

encoded : Str
encoded = to_json(FooDto.{ bar: "hi" })

encoded_try : Try(Str, [])
encoded_try = to_json_try(FooDto.{ bar: "hi" })

decoded : Try(FooDto, [InvalidJson(Str)])
decoded = from_json("{\"bar\":\"hi\"}")

expect {
	result : Try({ content_length : U64 }, [BadHeader, MissingHeader(Str)])
	result = read_headers("")
	result == Err(MissingHeader("content-length"))
}
