app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

# A built executable registers each dictionary worker's return layout in its
# entry wrapper. Every input below depends on the command line, so these codecs
# run in the executable rather than at compile time. The stored values take
# their codecs from the evidence they were produced with, whether the shape has
# no method owner, declares a structural method, or is a generic container. A
# generic method that returns its encoder names its element only inside that
# function, so the stored value's type is what binds the element.

Wrap(a) := { inner : a }.{
	encoder_for : encoding -> (Wrap(a), state -> Try(state, err))
		where [a.encoder_for : encoding -> (a, state -> Try(state, err))]
	encoder_for = |encoding| {
		Inner : a
		encode_inner = Inner.encoder_for(encoding)
		|self, state| encode_inner(self.inner, state)
	}
}

parser : {} -> (Str -> Try(a, [InvalidJson(Str)])) where [a.Json.Parseable([InvalidJson(Str)])]
parser = |{}| |text| Json.parse(text)

encoder : {} -> (a -> Str) where [a.Json.Encodable([])]
encoder = |{}| |value| Json.to_str(value)

stored_str_parser : Str -> Try(Str, [InvalidJson(Str)])
stored_str_parser = parser({})

stored_int_parser : Str -> Try(I64, [InvalidJson(Str)])
stored_int_parser = parser({})

stored_tag_parser : Str -> Try([Invalid], [InvalidJson(Str)])
stored_tag_parser = parser({})

stored_list_parser : Str -> Try(List(Str), [InvalidJson(Str)])
stored_list_parser = parser({})

stored_str_encoder : Str -> Str
stored_str_encoder = encoder({})

stored_record_encoder : { a : Str } -> Str
stored_record_encoder = encoder({})

stored_list_encoder : List(I64) -> Str
stored_list_encoder = encoder({})

stored_nested_list_encoder : List(List(Str)) -> Str
stored_nested_list_encoder = encoder({})

stored_wrap_encoder : Wrap(List(Str)) -> Str
stored_wrap_encoder = encoder({})

main! = |args| {
	count = Str.inspect(args.len())
	quoted = Str.inspect(count)

	direct_int : Try(I64, [InvalidJson(Str)])
	direct_int = Json.parse(count)
	Stdout.line!(Str.inspect(direct_int))

	direct_str : Try(Str, [InvalidJson(Str)])
	direct_str = Json.parse(quoted)
	Stdout.line!(Str.inspect(direct_str))

	direct_invalid : Try(Str, [InvalidJson(Str)])
	direct_invalid = Json.parse(Str.concat("nope", count))
	Stdout.line!(Str.inspect(direct_invalid))

	Stdout.line!(Str.inspect(stored_str_parser(quoted)))
	Stdout.line!(Str.inspect(stored_int_parser(count)))
	Stdout.line!(Str.inspect(stored_tag_parser(count)))
	Stdout.line!(Str.inspect(stored_list_parser("[${quoted}]")))
	Stdout.line!(stored_str_encoder(count))
	Stdout.line!(stored_record_encoder({ a: count }))
	Stdout.line!(stored_list_encoder([args.len().to_i64_wrap()]))
	Stdout.line!(stored_nested_list_encoder([[count]]))
	Stdout.line!(stored_wrap_encoder(Wrap.{ inner: [count] }))
	Ok({})
}
