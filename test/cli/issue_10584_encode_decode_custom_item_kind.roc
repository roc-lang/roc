# Repro for https://github.com/roc-lang/roc/issues/10584:
# a custom Json codec for a nominal tag union must round-trip through List.
ItemKind := [Text, Method].{
	is_eq : _

	encoder_for : encoding -> (ItemKind, state -> Try(state, []))
		where [
			encoding.encode_u32 : U32, state -> Try(state, []),
		]
	encoder_for = |_encoding| {
		Encoding : encoding

		|ItemKind.(item_kind), state| {
			u32 = match item_kind {
				Text => 1
				Method => 2
			}
			Encoding.encode_u32(u32, state)
		}
	}

	parser_for : encoding -> (state -> Try({ value : ItemKind, rest : state }, [TooShort, InvalidJson(Str), ..]))
		where [
			encoding.parse_u32 : encoding, state -> Try({ value : U32, rest : state }, [InvalidJson(Str)]),
		]
	parser_for = |encoding| {
		|state| {
			parsed = encoding.parse_u32(state) ? |InvalidJson(err)| InvalidJson(err)
			item_kind : Try(ItemKind, [TooShort, ..])
			item_kind = match parsed.value {
				1 => Ok(ItemKind.(Text))
				2 => Ok(ItemKind.(Method))
				_ => Err(TooShort)
			}
			Ok({ value: item_kind?, rest: parsed.rest })
		}
	}
}

original_list : List(ItemKind)
original_list = [ItemKind.(Text), ItemKind.(Method)]

encoded_str : Str
encoded_str = Json.to_str(original_list)

expect encoded_str == "[1,2]"

decoded_list : Try(List(ItemKind), _)
decoded_list = Json.parse(encoded_str)

expect decoded_list == Ok(original_list)

main! = |_args| {
	decoded_list?
	|> List.map(Str.inspect)
	|> Str.join_with("\n")
	|> echo!
	Ok({})
}
