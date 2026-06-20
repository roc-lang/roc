## Universal value type for crossing the Roc/Host boundary.
## All signal and event values are encoded to this format.
NodeValue := [
	NvI64(I64),
	NvStr(Str),
	NvBool(Bool),
	NvUnit,
	NvList(List(NodeValue)),
].{

	## Format instance - used as the format argument for encode/decode
	format : NodeValue
	format = NvUnit

	## Encode methods for where clause constraints
	## Pattern: encode_TYPE : NodeValue, TYPE -> Try(NodeValue, [])
	encode_i64 : NodeValue, I64 -> Try(NodeValue, [])
	encode_i64 = |_fmt, n| Ok(NvI64(n))

	encode_str : NodeValue, Str -> Try(NodeValue, [])
	encode_str = |_fmt, s| Ok(NvStr(s))

	encode_bool : NodeValue, Bool -> Try(NodeValue, [])
	encode_bool = |_fmt, b| Ok(NvBool(b))

	encode_unit : NodeValue, {} -> Try(NodeValue, [])
	encode_unit = |_fmt, {}| Ok(NvUnit)

	encode_list : NodeValue, List(item), (item, NodeValue -> Try(NodeValue, err)) -> Try(NodeValue, err)
	encode_list = |fmt, items, encode_item| {
		encoded_result =
			List.fold(
				items,
				Ok([]),
				|acc_result, item| {
					match acc_result {
						Ok(acc) =>
							match encode_item(item, fmt) {
								Ok(nv) => Ok(List.append(acc, nv))
								Err(err) => Err(err)
							}

						Err(err) => Err(err)
					}
				},
			)

		match encoded_result {
			Ok(encoded_items) => Ok(NvList(encoded_items))
			Err(err) => Err(err)
		}
	}

	## Decode methods for where clause constraints
	## Pattern: decode_TYPE : NodeValue, NodeValue -> (Try(TYPE, [TypeMismatch]), NodeValue)
	decode_i64 : NodeValue, NodeValue -> (Try(I64, [TypeMismatch]), NodeValue)
	decode_i64 = |_fmt, nv| {
		match nv {
			NvI64(n) => (Ok(n), nv)
			_ => (Err(TypeMismatch), nv)
		}
	}

	decode_str : NodeValue, NodeValue -> (Try(Str, [TypeMismatch]), NodeValue)
	decode_str = |_fmt, nv| {
		match nv {
			NvStr(s) => (Ok(s), nv)
			_ => (Err(TypeMismatch), nv)
		}
	}

	decode_bool : NodeValue, NodeValue -> (Try(Bool, [TypeMismatch]), NodeValue)
	decode_bool = |_fmt, nv| {
		match nv {
			NvBool(b) => (Ok(b), nv)
			_ => (Err(TypeMismatch), nv)
		}
	}

	decode_unit : NodeValue, NodeValue -> (Try({}, [TypeMismatch]), NodeValue)
	decode_unit = |_fmt, nv| {
		match nv {
			NvUnit => (Ok({}), nv)
			_ => (Err(TypeMismatch), nv)
		}
	}

	decode_list :
		NodeValue,
		NodeValue,
		(NodeValue, NodeValue -> (Try(item, [TypeMismatch]), NodeValue)) -> (Try(List(item), [TypeMismatch]), NodeValue)
	decode_list = |fmt, nv, decode_item| {
		match nv {
			NvList(items) => {
				decoded_result =
					List.fold(
						items,
						Ok([]),
						|acc_result, item_nv| {
							match acc_result {
								Ok(acc) => {
									(item_result, _) = decode_item(item_nv, fmt)
									match item_result {
										Ok(item) => Ok(List.append(acc, item))
										Err(err) => Err(err)
									}
								}

								Err(err) => Err(err)
							}
						},
					)

				(decoded_result, nv)
			}

			_ => (Err(TypeMismatch), nv)
		}
	}

}
