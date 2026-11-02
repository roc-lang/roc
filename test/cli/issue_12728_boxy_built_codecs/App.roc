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

# A custom format's method names neither the format nor the state its callback
# threads, so a stored encoder's own type reaches neither, and a format's type
# argument can be a value's layout. The stored encoder keeps the concrete type of
# the method that built it.
Format := [Format].{
	encode_list : Str, U64, ({ text : Str }, ({ text : Str }, (Str -> Try(Str, err)) -> Try({ text : Str }, err)) -> Try({ text : Str }, err)) -> Try(Str, err)
	encode_list = |state, count, write_items| {
		write = |current, write_item| {
			match write_item(current.text) {
				Ok(text) => Ok({ text: Str.concat(text, ";") })
				Err(err) => Err(err)
			}
		}
		match write_items({ text: Str.concat(Str.concat(state, Str.inspect(count)), ":") }, write) {
			Ok(done) => Ok(done.text)
			Err(err) => Err(err)
		}
	}
}

Tagged(tag) := [Tagged(tag)].{
	encode_list : Str, U64, ({ text : Str }, ({ text : Str }, (Str -> Try(Str, err)) -> Try({ text : Str }, err)) -> Try({ text : Str }, err)) -> Try(Str, err)
	encode_list = |state, _count, write_items| {
		write = |current, write_item| {
			match write_item(current.text) {
				Ok(text) => Ok({ text: Str.concat(text, "|") })
				Err(err) => Err(err)
			}
		}
		match write_items({ text: Str.concat(state, "<") }, write) {
			Ok(done) => Ok(Str.concat(done.text, ">"))
			Err(err) => Err(err)
		}
	}
}

Item := { value : I64 }.{
	encoder_for : encoding -> (Item, Str -> Try(Str, err))
	encoder_for = |_| |self, state| Ok(Str.concat(state, Str.inspect(self.value)))
}

format_encoder : {} -> (a, Str -> Try(Str, []))
	where [a.encoder_for : Format -> (a, Str -> Try(Str, []))]
format_encoder = |{}| {
	A : a
	A.encoder_for(Format.Format)
}

tagged_encoder : {} -> (a, Str -> Try(Str, []))
	where [a.encoder_for : Tagged(U8) -> (a, Str -> Try(Str, []))]
tagged_encoder = |{}| {
	A : a
	A.encoder_for(Tagged.Tagged(1))
}

# A format's method can dispatch on the format's type argument, and the same
# format at another argument is a different dictionary.
Label := [Label(Str)].{
	to_label : Label -> Str
	to_label = |self| {
		Label(text) = self
		text
	}
}

Prefixed(p) := { prefix : p }.{
	encode_item : Prefixed(p), Str, I64 -> Str where [p.to_label : p -> Str]
	encode_item = |self, state, value| Str.concat(Str.concat(state, self.prefix.to_label()), Str.inspect(value))
}

Value := { value : I64 }.{
	encoder_for : encoding -> (Value, Str -> Str) where [encoding.encode_item : encoding, Str, I64 -> Str]
	encoder_for = |encoding| |self, state| encoding.encode_item(state, self.value)
}

Values := { values : List(Value) }.{
	encoder_for : encoding -> (Values, Str -> Str) where [encoding.encode_item : encoding, Str, I64 -> Str]
	encoder_for = |encoding| {
		encode_value = Value.encoder_for(encoding)
		|self, state| self.values.fold(state, |text, value| encode_value(value, text))
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

stored_format_encoder : List(Item), Str -> Try(Str, [])
stored_format_encoder = format_encoder({})

stored_nested_format_encoder : List(List(Item)), Str -> Try(Str, [])
stored_nested_format_encoder = format_encoder({})

stored_tagged_encoder : List(Item), Str -> Try(Str, [])
stored_tagged_encoder = tagged_encoder({})

stored_prefixed_encoder : Values, Str -> Str
stored_prefixed_encoder = Values.encoder_for(Prefixed.{ prefix: Label.Label("#") })

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
	items = [Item.{ value: 7 }, Item.{ value: args.len().to_i64_wrap() }]
	Stdout.line!(Str.inspect(stored_format_encoder(items, "items=")))
	Stdout.line!(Str.inspect(stored_nested_format_encoder([items, []], "nested=")))
	Stdout.line!(Str.inspect(stored_tagged_encoder(items, "tagged=")))
	Stdout.line!(stored_prefixed_encoder(Values.{ values: [Value.{ value: 5 }, Value.{ value: args.len().to_i64_wrap() }] }, "prefixed="))
	Ok({})
}
