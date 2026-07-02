Builtin :: [].{
	Encoding :: {}.{
		# Compiler-generated structural record field-name metadata used by derived
		# parsers. The phantom _shape ties a FieldName handle to the exact
		# FieldNames value that produced it, so generated parsers can dispatch
		# without user-visible slots. Users can only observe a FieldName through
		# FieldName.name. Compiler-generated code supplies a hidden backing value
		# with the record-slot index and metadata.
		FieldName(_shape) :: { index : U64, name : Str, name_len : U64 }.{
			FieldNames(_shape) :: {}.{
				rename_fields : FieldNames(_shape), (Str -> Str) -> FieldNames(_shape)
				shortest_name : FieldNames(_shape) -> U64
				longest_name : FieldNames(_shape) -> U64
				iter : FieldNames(_shape) -> Iter(FieldName(_shape))
				for_size : FieldNames(_shape), U64 -> Iter(FieldName(_shape))
			}

			name : FieldName(_shape) -> Str
		}

		ParseTagUnionSpec(_shape) :: {}.{
			parse : ParseTagUnionSpec(_shape), { tag : Str, encoding : _encoding, state : _state, missing : _err } -> Try({ value : _shape, rest : _state }, _err)
		}

		JsonState :: [Input(Str)]
		JsonEncodeState :: { output : List(U8), container_commas : List(Bool) }

		JsonEncoding :: [Default, CamelCase, TrailingCommas].{
			rename_field : JsonEncoding, Str -> Str
			rename_field = |encoding, name|
				match encoding {
					Default => name
					TrailingCommas => name
					CamelCase => Json.snake_to_camel(name)
				}

			allows_trailing_commas : JsonEncoding -> Bool
			allows_trailing_commas = |encoding|
				match encoding {
					Default => False
					CamelCase => False
					TrailingCommas => True
				}

			parse_str : JsonEncoding, JsonState -> Try({ value : Str, rest : JsonState }, Json)
			parse_str = |_, state|
				match state {
					Input(raw) => {
						trimmed = Str.trim_start(raw)
						if Str.starts_with(trimmed, "\"") {
							string_parts = Json.split_json_string_tail(Str.drop_prefix(trimmed, "\""))?
							rest = Str.trim_start(string_parts.after)
							Ok({ value: string_parts.value, rest: JsonState.Input(rest) })
						} else {
							Err(Json.invalid_json)
						}
					}
				}

			parse_bool : JsonEncoding, JsonState -> Try({ value : Bool, rest : JsonState }, Json)
			parse_bool = |_, state|
				match state {
					Input(raw) => Json.parse_json_bool(raw)
				}

			parse_u8 : JsonEncoding, JsonState -> Try({ value : U8, rest : JsonState }, Json)
			parse_u8 = |_, state|
				match state {
					Input(raw) => Json.parse_json_unsigned_int(raw, u8_from_str)
				}

			parse_i8 : JsonEncoding, JsonState -> Try({ value : I8, rest : JsonState }, Json)
			parse_i8 = |_, state|
				match state {
					Input(raw) => Json.parse_json_signed_int(raw, i8_from_str)
				}

			parse_u16 : JsonEncoding, JsonState -> Try({ value : U16, rest : JsonState }, Json)
			parse_u16 = |_, state|
				match state {
					Input(raw) => Json.parse_json_unsigned_int(raw, u16_from_str)
				}

			parse_i16 : JsonEncoding, JsonState -> Try({ value : I16, rest : JsonState }, Json)
			parse_i16 = |_, state|
				match state {
					Input(raw) => Json.parse_json_signed_int(raw, i16_from_str)
				}

			parse_u32 : JsonEncoding, JsonState -> Try({ value : U32, rest : JsonState }, Json)
			parse_u32 = |_, state|
				match state {
					Input(raw) => Json.parse_json_unsigned_int(raw, u32_from_str)
				}

			parse_i32 : JsonEncoding, JsonState -> Try({ value : I32, rest : JsonState }, Json)
			parse_i32 = |_, state|
				match state {
					Input(raw) => Json.parse_json_signed_int(raw, i32_from_str)
				}

			parse_u64 : JsonEncoding, JsonState -> Try({ value : U64, rest : JsonState }, Json)
			parse_u64 = |_, state|
				match state {
					Input(raw) => Json.parse_json_unsigned_int(raw, u64_from_str)
				}

			parse_i64 : JsonEncoding, JsonState -> Try({ value : I64, rest : JsonState }, Json)
			parse_i64 = |_, state|
				match state {
					Input(raw) => Json.parse_json_signed_int(raw, i64_from_str)
				}

			parse_u128 : JsonEncoding, JsonState -> Try({ value : U128, rest : JsonState }, Json)
			parse_u128 = |_, state|
				match state {
					Input(raw) => Json.parse_json_unsigned_int(raw, u128_from_str)
				}

			parse_i128 : JsonEncoding, JsonState -> Try({ value : I128, rest : JsonState }, Json)
			parse_i128 = |_, state|
				match state {
					Input(raw) => Json.parse_json_signed_int(raw, i128_from_str)
				}

			parse_dec : JsonEncoding, JsonState -> Try({ value : Dec, rest : JsonState }, Json)
			parse_dec = |_, state|
				match state {
					Input(raw) => Json.parse_json_number(raw, Json.dec_from_json_number)
				}

			parse_f32 : JsonEncoding, JsonState -> Try({ value : F32, rest : JsonState }, Json)
			parse_f32 = |_, state|
				match state {
					Input(raw) => Json.parse_json_number(raw, f32_from_str)
				}

			parse_f64 : JsonEncoding, JsonState -> Try({ value : F64, rest : JsonState }, Json)
			parse_f64 = |_, state|
				match state {
					Input(raw) => Json.parse_json_number(raw, f64_from_str)
				}

			parse_null : JsonEncoding, JsonState -> Try(JsonState, Json)
			parse_null = |_, state|
				match state {
					Input(raw) => Json.parse_json_null(raw)
				}

			parse_array_start : JsonEncoding, JsonState -> Try(JsonState, Json)
			parse_array_start = |_, state|
				match state {
					Input(raw) => Json.parse_array_start_from_json(raw)
				}

			parse_array_next : JsonEncoding, JsonState -> Try([Element(JsonState), Done(JsonState)], Json)
			parse_array_next = |_, state|
				match state {
					Input(raw) => Json.parse_array_next_from_json(raw)
				}

			parse_array_after_element : JsonEncoding, JsonState -> Try([Continue(JsonState), Done(JsonState)], Json)
			parse_array_after_element = |encoding, state|
				match state {
					Input(raw) => Json.parse_array_after_element_from_json(encoding, raw)
				}

			parse_record_field : JsonEncoding,
			FieldName.FieldNames(_shape),
			JsonState -> Try(
				[
					Field({ field : FieldName(_shape), rest : JsonState }),
					TryField({ name : Str, rest : JsonState }),
					TryFieldCaseless({ name : Str, rest : JsonState }),
					Continue({ rest : JsonState }),
					Done({ rest : JsonState }),
				],
				Json,
			)
			parse_record_field = |encoding, _, state|
				match state {
					Input(raw) => Json.parse_record_field_from_object(encoding, raw)
				}

			parse_object_next : JsonEncoding, JsonState -> Try([Entry({ key : Str, rest : JsonState }), Done({ rest : JsonState })], Json)
			parse_object_next = |encoding, state|
				match state {
					Input(raw) =>
						match Json.parse_record_field_from_object(encoding, raw) {
							Ok(Field({ field, rest })) => Ok(Entry({ key: FieldName.name(field), rest }))
							Ok(TryField({ name, rest })) => Ok(Entry({ key: name, rest }))
							Ok(TryFieldCaseless({ name, rest })) => Ok(Entry({ key: name, rest }))
							Ok(Continue(_)) => Err(Json.invalid_json)
							Ok(Done({ rest })) => Ok(Done({ rest: rest }))
							Err(err) => Err(err)
						}
					}

			skip_record_field : JsonEncoding, JsonState -> Try(JsonState, Json)
			skip_record_field = |encoding, state| Json.skip_json_value(encoding, state)

			missing_record_field : JsonEncoding, Str, JsonState -> Json
			missing_record_field = |_, _, _| Json.MissingRequired

			missing_optional_field : JsonEncoding, Str, JsonState -> [Missing, ..]
			missing_optional_field = |_, _, _| Missing

			invalid_value : JsonEncoding, JsonState -> Json
			invalid_value = |_, _| Json.invalid_json

			parse_tag_union : JsonEncoding, ParseTagUnionSpec(a), JsonState -> Try({ value : a, rest : JsonState }, Json)
			parse_tag_union = |encoding, spec, state|
				match state {
					Input(value) => Json.parse_tag_union_from_json(value, encoding, spec)
				}

			begin_record : JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			begin_record = |state|
				Ok(
					JsonEncodeState.{
						output: u8_append(state.output, 123),
						container_commas: state.container_commas.append(False),
					},
				)

			encode_record_field : Str, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_record_field = |field, state| {
				with_comma = if Json.container_needs_comma(state) {
					u8_append(state.output, 44)
				} else {
					state.output
				}
				output = u8_append(Json.append_json_quoted_string(with_comma, field), 58)

				Ok(
					JsonEncodeState.{
						output,
						container_commas: Json.mark_container_has_item(state.container_commas),
					},
				)
			}

			end_record : JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			end_record = |state|
				Ok(
					JsonEncodeState.{
						output: u8_append(state.output, 125),
						container_commas: List.drop_last(state.container_commas, 1),
					},
				)

			begin_array : JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			begin_array = |state|
				Ok(
					JsonEncodeState.{
						output: u8_append(state.output, 91),
						container_commas: state.container_commas.append(False),
					},
				)

			encode_array_element : JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_array_element = |state| {
				output = if Json.container_needs_comma(state) {
					u8_append(state.output, 44)
				} else {
					state.output
				}

				Ok(
					JsonEncodeState.{
						output,
						container_commas: Json.mark_container_has_item(state.container_commas),
					},
				)
			}

			end_array : JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			end_array = |state|
				Ok(
					JsonEncodeState.{
						output: u8_append(state.output, 93),
						container_commas: List.drop_last(state.container_commas, 1),
					},
				)

			encode_str : Str, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_str = |value, state|
				Ok(
					JsonEncodeState.{
						output: Json.append_json_quoted_string(state.output, value),
						container_commas: state.container_commas,
					},
				)

			encode_bool : Bool, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_bool = |value, state|
				Ok(
					JsonEncodeState.{
						output: Json.append_json_string_bytes(state.output, if value "true" else "false"),
						container_commas: state.container_commas,
					},
				)

			encode_u8 : U8, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u8 = |value, state| Json.encode_json_number(json_u8_to_str(value), state)

			encode_i8 : I8, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i8 = |value, state| Json.encode_json_number(json_i8_to_str(value), state)

			encode_u16 : U16, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u16 = |value, state| Json.encode_json_number(json_u16_to_str(value), state)

			encode_i16 : I16, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i16 = |value, state| Json.encode_json_number(json_i16_to_str(value), state)

			encode_u32 : U32, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u32 = |value, state| Json.encode_json_number(json_u32_to_str(value), state)

			encode_i32 : I32, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i32 = |value, state| Json.encode_json_number(json_i32_to_str(value), state)

			encode_u64 : U64, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u64 = |value, state|
				Ok(
					JsonEncodeState.{
						output: Json.append_json_string_bytes(state.output, json_u64_to_str(value)),
						container_commas: state.container_commas,
					},
				)

			encode_i64 : I64, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i64 = |value, state| Json.encode_json_number(json_i64_to_str(value), state)

			encode_u128 : U128, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u128 = |value, state| Json.encode_json_number(json_u128_to_str(value), state)

			encode_i128 : I128, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i128 = |value, state| Json.encode_json_number(json_i128_to_str(value), state)

			encode_dec : Dec, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_dec = |value, state| Json.encode_json_number(json_dec_to_str(value), state)

			encode_f32 : F32, JsonEncodeState -> Try(JsonEncodeState, [Infinity, NaN, NegativeInfinity])
			encode_f32 = |value, state| {
				if json_f32_is_nan(value) {
					Err(NaN)
				} else if json_f32_is_infinite(value) {
					if json_f32_is_negative(value) {
						Err(NegativeInfinity)
					} else {
						Err(Infinity)
					}
				} else {
					Json.encode_json_number(json_f32_to_str(value), state)
				}
			}

			encode_f64 : F64, JsonEncodeState -> Try(JsonEncodeState, [Infinity, NaN, NegativeInfinity])
			encode_f64 = |value, state| {
				if json_f64_is_nan(value) {
					Err(NaN)
				} else if json_f64_is_infinite(value) {
					if json_f64_is_negative(value) {
						Err(NegativeInfinity)
					} else {
						Err(Infinity)
					}
				} else {
					Json.encode_json_number(json_f64_to_str(value), state)
				}
			}

			encode_null : JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_null = |state|
				Ok(
					JsonEncodeState.{
						output: Json.append_json_string_bytes(state.output, "null"),
						container_commas: state.container_commas,
					},
				)

			parse_key_str : JsonEncoding, Str -> Try(Str, Json)
			parse_key_str = |_, key| Ok(key)

			parse_key_bool : JsonEncoding, Str -> Try(Bool, Json)
			parse_key_bool = |_, key|
				if Str.is_eq(key, "true") {
					Ok(True)
				} else if Str.is_eq(key, "false") {
					Ok(False)
				} else {
					Err(Json.invalid_json)
				}

			parse_key_u8 : JsonEncoding, Str -> Try(U8, Json)
			parse_key_u8 = |_, key| Json.parse_json_key_unsigned_int(key, u8_from_str)

			parse_key_i8 : JsonEncoding, Str -> Try(I8, Json)
			parse_key_i8 = |_, key| Json.parse_json_key_signed_int(key, i8_from_str)

			parse_key_u16 : JsonEncoding, Str -> Try(U16, Json)
			parse_key_u16 = |_, key| Json.parse_json_key_unsigned_int(key, u16_from_str)

			parse_key_i16 : JsonEncoding, Str -> Try(I16, Json)
			parse_key_i16 = |_, key| Json.parse_json_key_signed_int(key, i16_from_str)

			parse_key_u32 : JsonEncoding, Str -> Try(U32, Json)
			parse_key_u32 = |_, key| Json.parse_json_key_unsigned_int(key, u32_from_str)

			parse_key_i32 : JsonEncoding, Str -> Try(I32, Json)
			parse_key_i32 = |_, key| Json.parse_json_key_signed_int(key, i32_from_str)

			parse_key_u64 : JsonEncoding, Str -> Try(U64, Json)
			parse_key_u64 = |_, key| Json.parse_json_key_unsigned_int(key, u64_from_str)

			parse_key_i64 : JsonEncoding, Str -> Try(I64, Json)
			parse_key_i64 = |_, key| Json.parse_json_key_signed_int(key, i64_from_str)

			parse_key_u128 : JsonEncoding, Str -> Try(U128, Json)
			parse_key_u128 = |_, key| Json.parse_json_key_unsigned_int(key, u128_from_str)

			parse_key_i128 : JsonEncoding, Str -> Try(I128, Json)
			parse_key_i128 = |_, key| Json.parse_json_key_signed_int(key, i128_from_str)

			parse_key_dec : JsonEncoding, Str -> Try(Dec, Json)
			parse_key_dec = |_, key| Json.parse_json_key_number(key, Json.dec_from_json_number)

			parse_key_f32 : JsonEncoding, Str -> Try(F32, Json)
			parse_key_f32 = |_, key| Json.parse_json_key_number(key, f32_from_str)

			parse_key_f64 : JsonEncoding, Str -> Try(F64, Json)
			parse_key_f64 = |_, key| Json.parse_json_key_number(key, f64_from_str)

			encode_key_str : JsonEncoding, Str -> Try(Str, _never_fails)
			encode_key_str = |_, key| Ok(key)

			encode_key_bool : JsonEncoding, Bool -> Try(Str, _never_fails)
			encode_key_bool = |_, key| Ok(if key "true" else "false")

			encode_key_u8 : JsonEncoding, U8 -> Try(Str, _never_fails)
			encode_key_u8 = |_, key| Ok(json_u8_to_str(key))

			encode_key_i8 : JsonEncoding, I8 -> Try(Str, _never_fails)
			encode_key_i8 = |_, key| Ok(json_i8_to_str(key))

			encode_key_u16 : JsonEncoding, U16 -> Try(Str, _never_fails)
			encode_key_u16 = |_, key| Ok(json_u16_to_str(key))

			encode_key_i16 : JsonEncoding, I16 -> Try(Str, _never_fails)
			encode_key_i16 = |_, key| Ok(json_i16_to_str(key))

			encode_key_u32 : JsonEncoding, U32 -> Try(Str, _never_fails)
			encode_key_u32 = |_, key| Ok(json_u32_to_str(key))

			encode_key_i32 : JsonEncoding, I32 -> Try(Str, _never_fails)
			encode_key_i32 = |_, key| Ok(json_i32_to_str(key))

			encode_key_u64 : JsonEncoding, U64 -> Try(Str, _never_fails)
			encode_key_u64 = |_, key| Ok(json_u64_to_str(key))

			encode_key_i64 : JsonEncoding, I64 -> Try(Str, _never_fails)
			encode_key_i64 = |_, key| Ok(json_i64_to_str(key))

			encode_key_u128 : JsonEncoding, U128 -> Try(Str, _never_fails)
			encode_key_u128 = |_, key| Ok(json_u128_to_str(key))

			encode_key_i128 : JsonEncoding, I128 -> Try(Str, _never_fails)
			encode_key_i128 = |_, key| Ok(json_i128_to_str(key))

			encode_key_dec : JsonEncoding, Dec -> Try(Str, _never_fails)
			encode_key_dec = |_, key| Ok(json_dec_to_str(key))

			encode_key_f32 : JsonEncoding, F32 -> Try(Str, [Infinity, NaN, NegativeInfinity])
			encode_key_f32 = |_, key| {
				if json_f32_is_nan(key) {
					Err(NaN)
				} else if json_f32_is_infinite(key) {
					if json_f32_is_negative(key) {
						Err(NegativeInfinity)
					} else {
						Err(Infinity)
					}
				} else {
					Ok(json_f32_to_str(key))
				}
			}

			encode_key_f64 : JsonEncoding, F64 -> Try(Str, [Infinity, NaN, NegativeInfinity])
			encode_key_f64 = |_, key| {
				if json_f64_is_nan(key) {
					Err(NaN)
				} else if json_f64_is_infinite(key) {
					if json_f64_is_negative(key) {
						Err(NegativeInfinity)
					} else {
						Err(Infinity)
					}
				} else {
					Ok(json_f64_to_str(key))
				}
			}
		}

		HttpHeaderState :: { raw : Str }

		HttpHeaderEncoding :: [Caseless].{
			rename_field : HttpHeaderEncoding, Str -> Str
			rename_field = |_, name| HttpHeader.underscores_to_dashes(name)

			parse_str : HttpHeaderEncoding, HttpHeaderState -> Try({ value : Str, rest : HttpHeaderState }, HttpHeader)
			parse_str = |_, state| {
				value_parts = HttpHeader.take_header_value(state.raw)?
				Ok({ value: value_parts.value, rest: HttpHeaderState.{ raw: value_parts.after } })
			}

			parse_u64 : HttpHeaderEncoding, HttpHeaderState -> Try({ value : U64, rest : HttpHeaderState }, HttpHeader)
			parse_u64 = |_, state| {
				value_parts = HttpHeader.take_header_value(state.raw)?

				match u64_from_str(value_parts.value) {
					Ok(value) => Ok({ value, rest: HttpHeaderState.{ raw: value_parts.after } })
					Err(_) => Err(HttpHeader.BadHeader)
				}
			}

			parse_record_field : HttpHeaderEncoding,
			FieldName.FieldNames(_shape),
			HttpHeaderState -> Try(
				[
					Field({ field : FieldName(_shape), rest : HttpHeaderState }),
					TryField({ name : Str, rest : HttpHeaderState }),
					TryFieldCaseless({ name : Str, rest : HttpHeaderState }),
					Continue({ rest : HttpHeaderState }),
					Done({ rest : HttpHeaderState }),
				],
				HttpHeader,
			)
			parse_record_field = |_, fields, state|
				HttpHeader.parse_record_field_from_headers(fields, state.raw)

			skip_record_field : HttpHeaderEncoding, HttpHeaderState -> Try(HttpHeaderState, HttpHeader)
			skip_record_field = |_, state| {
				parts = HttpHeader.take_header_value(state.raw)?
				Ok(HttpHeaderState.{ raw: parts.after })
			}

			missing_record_field : HttpHeaderEncoding, Str, HttpHeaderState -> HttpHeader
			missing_record_field = |_, _, _| HttpHeader.MissingRequired

			missing_optional_field : HttpHeaderEncoding, Str, HttpHeaderState -> [Missing]
			missing_optional_field = |_, _, _| Missing
		}

		Json := [MissingRequired, InvalidJson].{
			parse_str : JsonEncoding, JsonState -> Try({ value : Str, rest : JsonState }, Json)
			parse_str = |encoding, state| JsonEncoding.parse_str(encoding, state)

			parse_bool : JsonEncoding, JsonState -> Try({ value : Bool, rest : JsonState }, Json)
			parse_bool = |encoding, state| JsonEncoding.parse_bool(encoding, state)

			parse_u8 : JsonEncoding, JsonState -> Try({ value : U8, rest : JsonState }, Json)
			parse_u8 = |encoding, state| JsonEncoding.parse_u8(encoding, state)

			parse_i8 : JsonEncoding, JsonState -> Try({ value : I8, rest : JsonState }, Json)
			parse_i8 = |encoding, state| JsonEncoding.parse_i8(encoding, state)

			parse_u16 : JsonEncoding, JsonState -> Try({ value : U16, rest : JsonState }, Json)
			parse_u16 = |encoding, state| JsonEncoding.parse_u16(encoding, state)

			parse_i16 : JsonEncoding, JsonState -> Try({ value : I16, rest : JsonState }, Json)
			parse_i16 = |encoding, state| JsonEncoding.parse_i16(encoding, state)

			parse_u32 : JsonEncoding, JsonState -> Try({ value : U32, rest : JsonState }, Json)
			parse_u32 = |encoding, state| JsonEncoding.parse_u32(encoding, state)

			parse_i32 : JsonEncoding, JsonState -> Try({ value : I32, rest : JsonState }, Json)
			parse_i32 = |encoding, state| JsonEncoding.parse_i32(encoding, state)

			parse_u64 : JsonEncoding, JsonState -> Try({ value : U64, rest : JsonState }, Json)
			parse_u64 = |encoding, state| JsonEncoding.parse_u64(encoding, state)

			parse_i64 : JsonEncoding, JsonState -> Try({ value : I64, rest : JsonState }, Json)
			parse_i64 = |encoding, state| JsonEncoding.parse_i64(encoding, state)

			parse_u128 : JsonEncoding, JsonState -> Try({ value : U128, rest : JsonState }, Json)
			parse_u128 = |encoding, state| JsonEncoding.parse_u128(encoding, state)

			parse_i128 : JsonEncoding, JsonState -> Try({ value : I128, rest : JsonState }, Json)
			parse_i128 = |encoding, state| JsonEncoding.parse_i128(encoding, state)

			parse_dec : JsonEncoding, JsonState -> Try({ value : Dec, rest : JsonState }, Json)
			parse_dec = |encoding, state| JsonEncoding.parse_dec(encoding, state)

			parse_f32 : JsonEncoding, JsonState -> Try({ value : F32, rest : JsonState }, Json)
			parse_f32 = |encoding, state| JsonEncoding.parse_f32(encoding, state)

			parse_f64 : JsonEncoding, JsonState -> Try({ value : F64, rest : JsonState }, Json)
			parse_f64 = |encoding, state| JsonEncoding.parse_f64(encoding, state)

			parse_null : JsonEncoding, JsonState -> Try(JsonState, Json)
			parse_null = |encoding, state| JsonEncoding.parse_null(encoding, state)

			parse_array_start : JsonEncoding, JsonState -> Try(JsonState, Json)
			parse_array_start = |encoding, state| JsonEncoding.parse_array_start(encoding, state)

			parse_array_next : JsonEncoding, JsonState -> Try([Element(JsonState), Done(JsonState)], Json)
			parse_array_next = |encoding, state| JsonEncoding.parse_array_next(encoding, state)

			parse_array_after_element : JsonEncoding, JsonState -> Try([Continue(JsonState), Done(JsonState)], Json)
			parse_array_after_element = |encoding, state| JsonEncoding.parse_array_after_element(encoding, state)

			encode_str : JsonEncoding, Str, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_str = |_, value, state| JsonEncoding.encode_str(value, state)

			encode_bool : JsonEncoding, Bool, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_bool = |_, value, state| JsonEncoding.encode_bool(value, state)

			encode_u8 : JsonEncoding, U8, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u8 = |_, value, state| JsonEncoding.encode_u8(value, state)

			encode_i8 : JsonEncoding, I8, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i8 = |_, value, state| JsonEncoding.encode_i8(value, state)

			encode_u16 : JsonEncoding, U16, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u16 = |_, value, state| JsonEncoding.encode_u16(value, state)

			encode_i16 : JsonEncoding, I16, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i16 = |_, value, state| JsonEncoding.encode_i16(value, state)

			encode_u32 : JsonEncoding, U32, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u32 = |_, value, state| JsonEncoding.encode_u32(value, state)

			encode_i32 : JsonEncoding, I32, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i32 = |_, value, state| JsonEncoding.encode_i32(value, state)

			encode_u64 : JsonEncoding, U64, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u64 = |_, value, state| JsonEncoding.encode_u64(value, state)

			encode_i64 : JsonEncoding, I64, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i64 = |_, value, state| JsonEncoding.encode_i64(value, state)

			encode_u128 : JsonEncoding, U128, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_u128 = |_, value, state| JsonEncoding.encode_u128(value, state)

			encode_i128 : JsonEncoding, I128, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_i128 = |_, value, state| JsonEncoding.encode_i128(value, state)

			encode_dec : JsonEncoding, Dec, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_dec = |_, value, state| JsonEncoding.encode_dec(value, state)

			encode_f32 : JsonEncoding, F32, JsonEncodeState -> Try(JsonEncodeState, [Infinity, NaN, NegativeInfinity])
			encode_f32 = |_, value, state| JsonEncoding.encode_f32(value, state)

			encode_f64 : JsonEncoding, F64, JsonEncodeState -> Try(JsonEncodeState, [Infinity, NaN, NegativeInfinity])
			encode_f64 = |_, value, state| JsonEncoding.encode_f64(value, state)

			encode_null : JsonEncoding, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_null = |_, state| JsonEncoding.encode_null(state)

			begin_array : JsonEncoding, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			begin_array = |_, state| JsonEncoding.begin_array(state)

			encode_array_element : JsonEncoding, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_array_element = |_, state| JsonEncoding.encode_array_element(state)

			end_array : JsonEncoding, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			end_array = |_, state| JsonEncoding.end_array(state)

			encode : a -> Try(Str, err)
				where [
					a.encode_to : a, JsonEncoding -> (JsonEncodeState -> Try(JsonEncodeState, err)),
				]
			encode = |value| {
				encode_shape = value.encode_to(JsonEncoding.Default)
				encoded = encode_shape(JsonEncodeState.{ output: u8_list_with_capacity(64), container_commas: [] })?

				Ok(Str.from_utf8_lossy(encoded.output))
			}

			parse : Str -> Try(a, Json)
				where [
					a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json)),
				]
			parse = |json| {
				Shape : a
				parse_shape = Shape.parser_for(JsonEncoding.Default)
				parsed = parse_shape(JsonState.Input(json))?

				match parsed.rest {
					Input(rest) =>
						if Str.is_empty(Str.trim_start(rest)) {
							Ok(parsed.value)
						} else {
							Err(Json.invalid_json)
						}
					}
			}

			parse_trailing_commas : Str -> Try(a, Json)
				where [
					a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json)),
				]
			parse_trailing_commas = |json| {
				Shape : a
				parse_shape = Shape.parser_for(JsonEncoding.TrailingCommas)
				parsed = parse_shape(JsonState.Input(json))?

				match parsed.rest {
					Input(rest) =>
						if Str.is_empty(Str.trim_start(rest)) {
							Ok(parsed.value)
						} else {
							Err(Json.invalid_json)
						}
					}
			}

			parser_camel : () -> (Str -> Try(a, Json))
				where [
					a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json)),
				]
			parser_camel = || {
				Shape : a
				parse_shape = Shape.parser_for(JsonEncoding.CamelCase)

				|json| {
					parsed = parse_shape(JsonState.Input(json))?

					match parsed.rest {
						Input(rest) =>
							if Str.is_empty(Str.trim_start(rest)) {
								Ok(parsed.value)
							} else {
								Err(Json.invalid_json)
							}
						}
				}
			}

			invalid_json : Json
			invalid_json = Json.InvalidJson

			parse_json_bool : Str -> Try({ value : Bool, rest : JsonState }, Json)
			parse_json_bool = |raw| {
				trimmed = Str.trim_start(raw)
				parts = Json.split_json_scalar_tail(trimmed)?

				if Str.is_eq(parts.value, "true") {
					Ok({ value: True, rest: JsonState.Input(Str.trim_start(parts.after)) })
				} else if Str.is_eq(parts.value, "false") {
					Ok({ value: False, rest: JsonState.Input(Str.trim_start(parts.after)) })
				} else {
					Err(Json.invalid_json)
				}
			}

			parse_json_null : Str -> Try(JsonState, Json)
			parse_json_null = |raw| {
				trimmed = Str.trim_start(raw)
				parts = Json.split_json_scalar_tail(trimmed)?

				if Str.is_eq(parts.value, "null") {
					Ok(JsonState.Input(Str.trim_start(parts.after)))
				} else {
					Err(Json.invalid_json)
				}
			}

			parse_array_start_from_json : Str -> Try(JsonState, Json)
			parse_array_start_from_json = |raw| {
				trimmed = Str.trim_start(raw)

				if Str.starts_with(trimmed, "[") {
					Ok(JsonState.Input(Str.trim_start(Str.drop_prefix(trimmed, "["))))
				} else {
					Err(Json.invalid_json)
				}
			}

			parse_array_next_from_json : Str -> Try([Element(JsonState), Done(JsonState)], Json)
			parse_array_next_from_json = |raw| {
				trimmed = Str.trim_start(raw)

				if Str.starts_with(trimmed, "]") {
					Ok(Done(JsonState.Input(Str.trim_start(Str.drop_prefix(trimmed, "]")))))
				} else {
					Ok(Element(JsonState.Input(trimmed)))
				}
			}

			parse_array_after_element_from_json : JsonEncoding, Str -> Try([Continue(JsonState), Done(JsonState)], Json)
			parse_array_after_element_from_json = |encoding, raw| {
				trimmed = Str.trim_start(raw)

				if Str.starts_with(trimmed, "]") {
					Ok(Done(JsonState.Input(Str.trim_start(Str.drop_prefix(trimmed, "]")))))
				} else if Str.starts_with(trimmed, ",") {
					after_comma = Str.trim_start(Str.drop_prefix(trimmed, ","))

					if Str.starts_with(after_comma, "]") {
						if JsonEncoding.allows_trailing_commas(encoding) {
							Ok(Done(JsonState.Input(Str.trim_start(Str.drop_prefix(after_comma, "]")))))
						} else {
							Err(Json.invalid_json)
						}
					} else {
						Ok(Continue(JsonState.Input(after_comma)))
					}
				} else {
					Err(Json.invalid_json)
				}
			}

			parse_json_unsigned_int : Str, (Str -> Try(a, [BadNumStr])) -> Try({ value : a, rest : JsonState }, Json)
			parse_json_unsigned_int = |raw, parse_num| {
				trimmed = Str.trim_start(raw)
				parts = Json.split_json_scalar_tail(trimmed)?

				if Json.is_json_unsigned_int_literal(parts.value) {
					match parse_num(parts.value) {
						Ok(value) => Ok({ value, rest: JsonState.Input(Str.trim_start(parts.after)) })
						Err(_) => Err(Json.invalid_json)
					}
				} else {
					Err(Json.invalid_json)
				}
			}

			parse_json_signed_int : Str, (Str -> Try(a, [BadNumStr])) -> Try({ value : a, rest : JsonState }, Json)
			parse_json_signed_int = |raw, parse_num| {
				trimmed = Str.trim_start(raw)
				parts = Json.split_json_scalar_tail(trimmed)?

				if Json.is_json_signed_int_literal(parts.value) {
					match parse_num(parts.value) {
						Ok(value) => Ok({ value, rest: JsonState.Input(Str.trim_start(parts.after)) })
						Err(_) => Err(Json.invalid_json)
					}
				} else {
					Err(Json.invalid_json)
				}
			}

			parse_json_number : Str, (Str -> Try(a, [BadNumStr])) -> Try({ value : a, rest : JsonState }, Json)
			parse_json_number = |raw, parse_num| {
				trimmed = Str.trim_start(raw)
				parts = Json.split_json_scalar_tail(trimmed)?

				if Json.is_json_number(parts.value) {
					match parse_num(parts.value) {
						Ok(value) => Ok({ value, rest: JsonState.Input(Str.trim_start(parts.after)) })
						Err(_) => Err(Json.invalid_json)
					}
				} else {
					Err(Json.invalid_json)
				}
			}

			dec_from_json_number : Str -> Try(Dec, [BadNumStr])
			dec_from_json_number = |value|
				match Json.split_json_number_exponent(value) {
					Ok(split) => Json.dec_from_json_exponent_parts(split.mantissa, split.exponent)
					Err(NotFound) => dec_from_str(value)
				}

			split_json_number_exponent : Str -> Try({ mantissa : Str, exponent : Str }, [NotFound])
			split_json_number_exponent = |value|
				match Str.find_first(value, "e") {
					Ok(split) => Ok({ mantissa: split.before, exponent: split.after })
					Err(NotFound) =>
						match Str.find_first(value, "E") {
							Ok(split) => Ok({ mantissa: split.before, exponent: split.after })
							Err(NotFound) => Err(NotFound)
						}
					}

			dec_from_json_exponent_parts : Str, Str -> Try(Dec, [BadNumStr])
			dec_from_json_exponent_parts = |mantissa, exponent_text| {
				exponent_digits = if Str.starts_with(exponent_text, "+") {
					Str.drop_prefix(exponent_text, "+")
				} else {
					exponent_text
				}

				exponent = match i64_from_str(exponent_digits) {
					Ok(parsed) => parsed
					Err(_) => return Err(BadNumStr)
				}

				negative = Str.starts_with(mantissa, "-")
				unsigned_mantissa = if negative {
					Str.drop_prefix(mantissa, "-")
				} else {
					mantissa
				}

				parts = match Str.find_first(unsigned_mantissa, ".") {
					Ok(split) => { int_part: split.before, frac_part: split.after }
					Err(NotFound) => { int_part: unsigned_mantissa, frac_part: "" }
				}

				digits = Str.concat(parts.int_part, parts.frac_part)

				if Json.json_dec_digits_are_zero(digits) {
					dec_from_str("0")
				} else {
					int_len = Str.count_utf8_bytes(parts.int_part).to_i64_wrap()
					raw_point = int_len + exponent
					trimmed = Json.trim_json_dec_leading_zeros(digits, raw_point)

					if trimmed.point > 21 or trimmed.point < -18 {
						Err(BadNumStr)
					} else {
						normalized = Json.normalize_json_dec_digits(negative, trimmed.digits, trimmed.digits_len, trimmed.point)?
						dec_from_str(normalized)
					}
				}
			}

			json_dec_digits_are_zero : Str -> Bool
			json_dec_digits_are_zero = |digits| {
				bytes = Str.to_utf8(digits)

				for byte in bytes {
					if byte != 48 {
						return False
					}
				}

				True
			}

			trim_json_dec_leading_zeros : Str, I64 -> { digits : Str, digits_len : U64, point : I64 }
			trim_json_dec_leading_zeros = |digits, point| {
				bytes = Str.to_utf8(digits)
				len = List.len(bytes)
				var $index = 0

				while $index < len and list_get_unsafe(bytes, $index) == 48 {
					$index = $index + 1
				}

				{
					digits: Str.from_utf8_lossy(List.drop_first(bytes, $index)),
					digits_len: len - $index,
					point: point - $index.to_i64_wrap(),
				}
			}

			normalize_json_dec_digits : Bool, Str, U64, I64 -> Try(Str, [BadNumStr])
			normalize_json_dec_digits = |negative, digits, digits_len, point| {
				sign = if negative "-" else ""

				if point <= 0 {
					zero_count = point.negate().to_u64_wrap()

					Ok(
						Str.concat(
							sign,
							Str.concat("0.", Str.concat(Str.repeat("0", zero_count), digits)),
						),
					)
				} else {
					point_u64 = point.to_u64_wrap()

					if point_u64 >= digits_len {
						zero_count = point_u64 - digits_len
						Ok(Str.concat(sign, Str.concat(digits, Str.repeat("0", zero_count))))
					} else {
						bytes = Str.to_utf8(digits)
						before = Str.from_utf8_lossy(List.take_first(bytes, point_u64))
						after = Str.from_utf8_lossy(List.drop_first(bytes, point_u64))

						Ok(Str.concat(sign, Str.concat(before, Str.concat(".", after))))
					}
				}
			}

			parse_json_key_unsigned_int : Str, (Str -> Try(a, [BadNumStr])) -> Try(a, Json)
			parse_json_key_unsigned_int = |key, parse_num|
				if Json.is_json_unsigned_int_literal(key) {
					match parse_num(key) {
						Ok(value) => Ok(value)
						Err(_) => Err(Json.invalid_json)
					}
				} else {
					Err(Json.invalid_json)
				}

			parse_json_key_signed_int : Str, (Str -> Try(a, [BadNumStr])) -> Try(a, Json)
			parse_json_key_signed_int = |key, parse_num|
				if Json.is_json_signed_int_literal(key) {
					match parse_num(key) {
						Ok(value) => Ok(value)
						Err(_) => Err(Json.invalid_json)
					}
				} else {
					Err(Json.invalid_json)
				}

			parse_json_key_number : Str, (Str -> Try(a, [BadNumStr])) -> Try(a, Json)
			parse_json_key_number = |key, parse_num|
				if Json.is_json_number(key) {
					match parse_num(key) {
						Ok(value) => Ok(value)
						Err(_) => Err(Json.invalid_json)
					}
				} else {
					Err(Json.invalid_json)
				}

			encode_json_number : Str, JsonEncodeState -> Try(JsonEncodeState, _never_fails)
			encode_json_number = |value, state|
				Ok(
					JsonEncodeState.{
						output: Json.append_json_string_bytes(state.output, value),
						container_commas: state.container_commas,
					},
				)

			container_needs_comma : JsonEncodeState -> Bool
			container_needs_comma = |state|
				match List.last(state.container_commas) {
					Ok(needs_comma) => needs_comma
					Err(ListWasEmpty) => {
						crash "json encoder container stack underflow"
					}
				}

			mark_container_has_item : List(Bool) -> List(Bool)
			mark_container_has_item = |container_commas|
				List.drop_last(container_commas, 1).append(True)

			append_json_string_bytes : List(U8), Str -> List(U8)
			append_json_string_bytes = |out, value| {
				bytes = Str.to_utf8(value)
				len = List.len(bytes)
				var $out = u8_list_reserve(out, len)
				var $index = 0

				while $index < len {
					$out = u8_append($out, list_get_unsafe(bytes, $index))
					$index = $index + 1
				}

				$out
			}

			append_json_quoted_string : List(U8), Str -> List(U8)
			append_json_quoted_string = |out, value| {
				bytes = Str.to_utf8(value)
				len = List.len(bytes)
				var $out = u8_list_reserve(out, len + 2)
				var $index = 0

				$out = u8_append($out, 34)

				while $index < len {
					byte = list_get_unsafe(bytes, $index)
					$out = Json.append_json_string_byte($out, byte)
					$index = $index + 1
				}

				$out = u8_append($out, 34)
				$out
			}

			append_json_string_byte : List(U8), U8 -> List(U8)
			append_json_string_byte = |bytes, byte|
				if byte == 34 {
					u8_append(u8_append(bytes, 92), 34)
				} else if byte == 92 {
					u8_append(u8_append(bytes, 92), 92)
				} else if byte == 8 {
					u8_append(u8_append(bytes, 92), 98)
				} else if byte == 9 {
					u8_append(u8_append(bytes, 92), 116)
				} else if byte == 10 {
					u8_append(u8_append(bytes, 92), 110)
				} else if byte == 12 {
					u8_append(u8_append(bytes, 92), 102)
				} else if byte == 13 {
					u8_append(u8_append(bytes, 92), 114)
				} else if byte < 32 {
					Json.append_json_unicode_escape(bytes, byte)
				} else {
					u8_append(bytes, byte)
				}

			append_json_unicode_escape : List(U8), U8 -> List(U8)
			append_json_unicode_escape = |bytes, byte| {
				high = byte / 16
				low = byte % 16

				u8_append(
					u8_append(
						u8_append(
							u8_append(
								u8_append(
									u8_append(bytes, 92),
									117,
								),
								48,
							),
							48,
						),
						Json.hex_digit_byte(high),
					),
					Json.hex_digit_byte(low),
				)
			}

			hex_digit_byte : U8 -> U8
			hex_digit_byte = |value|
				if value < 10 {
					value + 48
				} else {
					value + 87
				}

			parse_record_field_from_object : JsonEncoding,
			Str -> Try(
				[
					Field({ field : FieldName(_shape), rest : JsonState }),
					TryField({ name : Str, rest : JsonState }),
					TryFieldCaseless({ name : Str, rest : JsonState }),
					Continue({ rest : JsonState }),
					Done({ rest : JsonState }),
				],
				Json,
			)
			parse_record_field_from_object = |encoding, raw| {
				remaining = Str.trim_start(raw)

				if Str.starts_with(remaining, "{") {
					return Json.parse_record_field_after_object_start(encoding, Str.trim_start(Str.drop_prefix(remaining, "{")))
				}

				Json.parse_record_field_after_value(encoding, remaining)
			}

			parse_record_field_after_object_start = |encoding, remaining| {
				if Str.starts_with(remaining, "}") {
					after_record = Str.trim_start(Str.drop_prefix(remaining, "}"))
					return Ok(Done({ rest: JsonState.Input(after_record) }))
				}

				if Str.starts_with(remaining, ",") {
					return Err(Json.invalid_json)
				}

				Json.parse_record_field_start(encoding, remaining)
			}

			parse_record_field_after_value = |encoding, remaining| {
				if Str.starts_with(remaining, "}") {
					after_record = Str.trim_start(Str.drop_prefix(remaining, "}"))
					return Ok(Done({ rest: JsonState.Input(after_record) }))
				}

				if !Str.starts_with(remaining, ",") {
					return Err(Json.invalid_json)
				}

				after_comma = Str.trim_start(Str.drop_prefix(remaining, ","))

				if Str.starts_with(after_comma, "}") {
					if JsonEncoding.allows_trailing_commas(encoding) {
						after_record = Str.trim_start(Str.drop_prefix(after_comma, "}"))
						return Ok(Done({ rest: JsonState.Input(after_record) }))
					} else {
						return Err(Json.invalid_json)
					}
				}

				Json.parse_record_field_start(encoding, after_comma)
			}

			parse_record_field_start = |_, remaining| {
				if !Str.starts_with(remaining, "\"") {
					return Err(Json.invalid_json)
				}

				key_parts = Json.split_json_string_tail(Str.drop_prefix(remaining, "\""))?
				key = key_parts.value
				after_key = Str.trim_start(key_parts.after)

				if !Str.starts_with(after_key, ":") {
					return Err(Json.invalid_json)
				}

				after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))
				rest = JsonState.Input(after_colon)

				Ok(TryField({ name: key, rest }))
			}

			snake_to_camel : Str -> Str
			snake_to_camel = |text|
				match Str.find_first(text, "_") {
					Ok({ before, after }) =>
						before.concat(Json.upper_first_ascii(Json.snake_to_camel(after)))

					Err(NotFound) => text
				}

			upper_first_ascii : Str -> Str
			upper_first_ascii = |text| {
				bytes = Str.to_utf8(text)

				match List.first(bytes) {
					Ok(first) => {
						upper = if first >= 97 {
							if first <= 122 {
								first - 32
							} else {
								first
							}
						} else {
							first
						}

						match Str.from_utf8([upper].concat(List.drop_first(bytes, 1))) {
							Ok(value) => value
							Err(_) => text
						}
					}

					Err(_) => text
				}
			}

			skip_json_value : JsonEncoding, JsonState -> Try(JsonState, Json)
			skip_json_value = |encoding, state|
				match state {
					Input(raw) => {
						trimmed = Str.trim_start(raw)

						if Str.starts_with(trimmed, "\"") {
							value_parts = Json.split_json_string_tail(Str.drop_prefix(trimmed, "\""))?
							Ok(JsonState.Input(Str.trim_start(value_parts.after)))
						} else if Str.starts_with(trimmed, "{") {
							Json.skip_json_object(encoding, trimmed)
						} else if Str.starts_with(trimmed, "[") {
							Json.skip_json_array(encoding, trimmed)
						} else {
							scalar_parts = Json.split_json_scalar_tail(trimmed)?
							if Json.is_json_scalar(scalar_parts.value) {
								Ok(JsonState.Input(Str.trim_start(scalar_parts.after)))
							} else {
								Err(Json.invalid_json)
							}
						}
					}
				}

			skip_json_object : JsonEncoding, Str -> Try(JsonState, Json)
			skip_json_object = |encoding, raw| {
				remaining = Str.trim_start(raw)

				if !Str.starts_with(remaining, "{") {
					return Err(Json.invalid_json)
				}

				var $after_field = Str.trim_start(Str.drop_prefix(remaining, "{"))

				if Str.starts_with($after_field, "}") {
					return Ok(JsonState.Input(Str.trim_start(Str.drop_prefix($after_field, "}"))))
				}

				while True {
					if !Str.starts_with($after_field, "\"") {
						return Err(Json.invalid_json)
					}

					key_parts = Json.split_json_string_tail(Str.drop_prefix($after_field, "\""))?
					after_key = Str.trim_start(key_parts.after)

					if !Str.starts_with(after_key, ":") {
						return Err(Json.invalid_json)
					}

					after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))
					skipped = Json.skip_json_value(encoding, JsonState.Input(after_colon))?

					match skipped {
						Input(after_value) => {
							after_value_trimmed = Str.trim_start(after_value)

							if Str.starts_with(after_value_trimmed, "}") {
								return Ok(JsonState.Input(Str.trim_start(Str.drop_prefix(after_value_trimmed, "}"))))
							}

							if !Str.starts_with(after_value_trimmed, ",") {
								return Err(Json.invalid_json)
							}

							after_comma = Str.trim_start(Str.drop_prefix(after_value_trimmed, ","))

							if Str.starts_with(after_comma, "}") {
								if JsonEncoding.allows_trailing_commas(encoding) {
									return Ok(JsonState.Input(Str.trim_start(Str.drop_prefix(after_comma, "}"))))
								} else {
									return Err(Json.invalid_json)
								}
							}

							$after_field = after_comma
						}
					}
				}
			}

			skip_json_array : JsonEncoding, Str -> Try(JsonState, Json)
			skip_json_array = |encoding, raw| {
				remaining = Str.trim_start(raw)

				if !Str.starts_with(remaining, "[") {
					return Err(Json.invalid_json)
				}

				var $after_value = Str.trim_start(Str.drop_prefix(remaining, "["))

				if Str.starts_with($after_value, "]") {
					return Ok(JsonState.Input(Str.trim_start(Str.drop_prefix($after_value, "]"))))
				}

				while True {
					skipped = Json.skip_json_value(encoding, JsonState.Input($after_value))?

					match skipped {
						Input(after_nested_value) => {
							after_nested_value_trimmed = Str.trim_start(after_nested_value)

							if Str.starts_with(after_nested_value_trimmed, "]") {
								return Ok(JsonState.Input(Str.trim_start(Str.drop_prefix(after_nested_value_trimmed, "]"))))
							}

							if !Str.starts_with(after_nested_value_trimmed, ",") {
								return Err(Json.invalid_json)
							}

							after_comma = Str.trim_start(Str.drop_prefix(after_nested_value_trimmed, ","))

							if Str.starts_with(after_comma, "]") {
								if JsonEncoding.allows_trailing_commas(encoding) {
									return Ok(JsonState.Input(Str.trim_start(Str.drop_prefix(after_comma, "]"))))
								} else {
									return Err(Json.invalid_json)
								}
							}

							$after_value = after_comma
						}
					}
				}
			}

			parse_tag_union_from_json : Str, JsonEncoding, ParseTagUnionSpec(a) -> Try({ value : a, rest : JsonState }, Json)
			parse_tag_union_from_json = |raw, encoding, spec| {
				remaining = Str.trim_start(raw)

				if Str.starts_with(remaining, "\"") {
					key_split = Json.split_json_string_tail(Str.drop_prefix(remaining, "\""))?

					return ParseTagUnionSpec.parse(
						spec,
						{
							tag: key_split.value,
							encoding,
							state: JsonState.Input(key_split.after),
							missing: Json.invalid_json,
						},
					)
				}

				if !Str.starts_with(remaining, "{") {
					return Err(Json.invalid_json)
				}

				after_open = Str.trim_start(Str.drop_prefix(remaining, "{"))

				if !Str.starts_with(after_open, "\"") {
					return Err(Json.invalid_json)
				}

				key_split = Json.split_json_string_tail(Str.drop_prefix(after_open, "\""))

				match key_split {
					Ok(key_parts) => {
						tag_name = key_parts.value
						after_key = Str.trim_start(key_parts.after)

						if !Str.starts_with(after_key, ":") {
							return Err(Json.invalid_json)
						}

						payload = Str.trim_start(Str.drop_prefix(after_key, ":"))

						if Str.starts_with(payload, "}") {
							return Err(Json.invalid_json)
						}

						if Str.starts_with(payload, ",") {
							return Err(Json.invalid_json)
						}

						parsed = ParseTagUnionSpec.parse(
							spec,
							{
								tag: tag_name,
								encoding,
								state: JsonState.Input(payload),
								missing: Json.invalid_json,
							},
						)?

						match parsed.rest {
							Input(after_payload) => Json.finish_tag_payload(encoding, parsed.value, after_payload)
						}
					}
					Err(_) => Err(Json.invalid_json)
				}
			}

			finish_tag_payload : JsonEncoding, a, Str -> Try({ value : a, rest : JsonState }, Json)
			finish_tag_payload = |encoding, value, raw| {
				remaining = Str.trim_start(raw)

				if Str.starts_with(remaining, "}") {
					after_close = Str.trim_start(Str.drop_prefix(remaining, "}"))
					return Ok({ value, rest: JsonState.Input(after_close) })
				}

				if Str.starts_with(remaining, ",") {
					after_comma = Str.trim_start(Str.drop_prefix(remaining, ","))

					if JsonEncoding.allows_trailing_commas(encoding) {
						if Str.starts_with(after_comma, "}") {
							after_close = Str.trim_start(Str.drop_prefix(after_comma, "}"))
							return Ok({ value, rest: JsonState.Input(after_close) })
						}
					}

					return Err(Json.invalid_json)
				}

				empty_payload = Json.consume_empty_json_object(remaining)?
				after_payload = Str.trim_start(empty_payload.after)

				if Str.starts_with(after_payload, "}") {
					after_close = Str.trim_start(Str.drop_prefix(after_payload, "}"))
					Ok({ value, rest: JsonState.Input(after_close) })
				} else if Str.starts_with(after_payload, ",") {
					after_comma = Str.trim_start(Str.drop_prefix(after_payload, ","))

					if JsonEncoding.allows_trailing_commas(encoding) {
						if Str.starts_with(after_comma, "}") {
							after_close = Str.trim_start(Str.drop_prefix(after_comma, "}"))
							return Ok({ value, rest: JsonState.Input(after_close) })
						}
					}

					Err(Json.invalid_json)
				} else {
					Err(Json.invalid_json)
				}
			}

			consume_empty_json_object : Str -> Try({ after : Str }, Json)
			consume_empty_json_object = |raw| {
				remaining = Str.trim_start(raw)

				if !Str.starts_with(remaining, "{") {
					return Err(Json.invalid_json)
				}

				after_open = Str.trim_start(Str.drop_prefix(remaining, "{"))

				if Str.starts_with(after_open, "}") {
					Ok({ after: Str.drop_prefix(after_open, "}") })
				} else {
					Err(Json.invalid_json)
				}
			}

			is_json_scalar : Str -> Bool
			is_json_scalar = |value|
				if Str.is_eq(value, "null") {
					True
				} else if Str.is_eq(value, "true") {
					True
				} else if Str.is_eq(value, "false") {
					True
				} else {
					Json.is_json_number(value)
				}

			is_json_number : Str -> Bool
			is_json_number = |value| {
				bytes = Str.to_utf8(value)
				len = List.len(bytes)

				if len == 0 {
					return False
				}

				var $index = 0

				first = list_get_unsafe(bytes, $index)

				if first == 45 {
					$index = $index + 1

					if $index == len {
						return False
					}
				}

				int_first = list_get_unsafe(bytes, $index)

				if int_first == 48 {
					$index = $index + 1
				} else if Json.is_json_digit_one_to_nine(int_first) {
					$index = $index + 1

					while $index < len {
						byte = list_get_unsafe(bytes, $index)

						if Json.is_json_digit(byte) {
							$index = $index + 1
						} else {
							break
						}
					}
				} else {
					return False
				}

				if $index < len {
					byte = list_get_unsafe(bytes, $index)

					if byte == 46 {
						$index = $index + 1

						if $index == len {
							return False
						}

						first_fraction_byte = list_get_unsafe(bytes, $index)

						if !Json.is_json_digit(first_fraction_byte) {
							return False
						}

						while $index < len {
							fraction_byte = list_get_unsafe(bytes, $index)

							if Json.is_json_digit(fraction_byte) {
								$index = $index + 1
							} else {
								break
							}
						}
					}
				}

				if $index < len {
					byte = list_get_unsafe(bytes, $index)

					if Json.is_json_exponent_marker(byte) {
						$index = $index + 1

						if $index == len {
							return False
						}

						exponent_first_byte = list_get_unsafe(bytes, $index)

						if Json.is_json_sign(exponent_first_byte) {
							$index = $index + 1

							if $index == len {
								return False
							}
						}

						first_exponent_digit = list_get_unsafe(bytes, $index)

						if !Json.is_json_digit(first_exponent_digit) {
							return False
						}

						while $index < len {
							exponent_byte = list_get_unsafe(bytes, $index)

							if Json.is_json_digit(exponent_byte) {
								$index = $index + 1
							} else {
								break
							}
						}
					}
				}

				$index == len
			}

			is_json_u64_literal : Str -> Bool
			is_json_u64_literal = |value| Json.is_json_unsigned_int_literal(value)

			is_json_unsigned_int_literal : Str -> Bool
			is_json_unsigned_int_literal = |value| {
				bytes = Str.to_utf8(value)
				len = List.len(bytes)

				if len == 0 {
					return False
				}

				first = list_get_unsafe(bytes, 0)

				if first == 48 {
					return len == 1
				}

				if !Json.is_json_digit_one_to_nine(first) {
					return False
				}

				var $index = 1

				while $index < len {
					byte = list_get_unsafe(bytes, $index)

					if Json.is_json_digit(byte) {
						$index = $index + 1
					} else {
						return False
					}
				}

				True
			}

			is_json_signed_int_literal : Str -> Bool
			is_json_signed_int_literal = |value| {
				if Str.starts_with(value, "-") {
					Json.is_json_unsigned_int_literal(Str.drop_prefix(value, "-"))
				} else {
					Json.is_json_unsigned_int_literal(value)
				}
			}

			is_json_digit : U8 -> Bool
			is_json_digit = |byte|
				if byte >= 48 {
					byte <= 57
				} else {
					False
				}

			is_json_digit_one_to_nine : U8 -> Bool
			is_json_digit_one_to_nine = |byte|
				if byte >= 49 {
					byte <= 57
				} else {
					False
				}

			is_json_exponent_marker : U8 -> Bool
			is_json_exponent_marker = |byte|
				if byte == 69 {
					True
				} else {
					byte == 101
				}

			is_json_sign : U8 -> Bool
			is_json_sign = |byte|
				if byte == 43 {
					True
				} else {
					byte == 45
				}

			split_json_string_tail : Str -> Try({ value : Str, after : Str }, Json)
			split_json_string_tail = |tail| {
				quote_split = Str.find_first(tail, "\"")

				match quote_split {
					Ok(quote_parts) => {
						slash_split = Str.find_first(tail, "\\")

						match slash_split {
							Ok(slash_parts) =>
								if Str.count_utf8_bytes(slash_parts.before) < Str.count_utf8_bytes(quote_parts.before) {
									Err(Json.invalid_json)
								} else {
									Ok({ value: quote_parts.before, after: quote_parts.after })
								}
							Err(NotFound) => Ok({ value: quote_parts.before, after: quote_parts.after })
						}
					}
					Err(NotFound) => Err(Json.invalid_json)
				}
			}

			split_json_scalar_tail : Str -> Try({ value : Str, after : Str }, Json)
			split_json_scalar_tail = |raw| {
				var $value = raw
				var $after = ""
				var $offset = Str.count_utf8_bytes(raw)

				match Str.find_first(raw, ",") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = ",".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				match Str.find_first(raw, "}") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = "}".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				match Str.find_first(raw, "]") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = "]".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				match Str.find_first(raw, " ") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = " ".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				match Str.find_first(raw, "\n") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = "\n".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				match Str.find_first(raw, "\t") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = "\t".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				match Str.find_first(raw, "\r") {
					Ok(parts) => {
						parts_offset = Str.count_utf8_bytes(parts.before)
						if parts_offset < $offset {
							$value = parts.before
							$after = "\r".concat(parts.after)
							$offset = parts_offset
						}
					}
					Err(NotFound) => {}
				}

				if Str.is_empty($value) {
					Err(Json.invalid_json)
				} else {
					Ok({ value: $value, after: $after })
				}
			}

		}

		HttpHeader := [MissingRequired, BadHeader].{
			parser_for : () -> (Str -> Try(output, HttpHeader))
				where [
					output.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)),
				]
			parser_for = || {
				Output : output

				parse_output : HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)
				parse_output = Output.parser_for(HttpHeaderEncoding.Caseless)

				|raw| {
					parsed = parse_output(HttpHeaderState.{ raw })?
					Ok(parsed.value)
				}
			}

			parse : Str -> Try(output, HttpHeader)
				where [
					output.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)),
				]
			parse = |raw| {
				Output : output

				parse_output = Output.parser_for(HttpHeaderEncoding.Caseless)
				parsed = parse_output(HttpHeaderState.{ raw })?

				Ok(parsed.value)
			}

			parse_str : HttpHeaderEncoding, HttpHeaderState -> Try({ value : Str, rest : HttpHeaderState }, HttpHeader)
			parse_str = |encoding, state| HttpHeaderEncoding.parse_str(encoding, state)

			parse_u64 : HttpHeaderEncoding, HttpHeaderState -> Try({ value : U64, rest : HttpHeaderState }, HttpHeader)
			parse_u64 = |encoding, state| HttpHeaderEncoding.parse_u64(encoding, state)

			parse_record_field_from_headers : FieldName.FieldNames(_shape),
			Str -> Try(
				[
					Field({ field : FieldName(_shape), rest : HttpHeaderState }),
					TryField({ name : Str, rest : HttpHeaderState }),
					TryFieldCaseless({ name : Str, rest : HttpHeaderState }),
					Continue({ rest : HttpHeaderState }),
					Done({ rest : HttpHeaderState }),
				],
				HttpHeader,
			)
			parse_record_field_from_headers = |fields, headers|
				if Str.is_empty(headers) {
					Ok(Done({ rest: HttpHeaderState.{ raw: "" } }))
				} else {
					line_parts = match Str.find_first(headers, "\r\n") {
						Ok(parts) => parts
						Err(NotFound) => { before: headers, after: "" }
					}

					if Str.is_empty(line_parts.before) {
						Ok(Done({ rest: HttpHeaderState.{ raw: line_parts.after } }))
					} else {
						match Str.find_first(headers, ":") {
							Ok({ before: name, after: value_start }) => {
								name_len = Str.count_utf8_bytes(name)
								line_len = Str.count_utf8_bytes(line_parts.before)

								if name_len >= line_len {
									Err(HttpHeader.BadHeader)
								} else {
									if name_len < FieldName.FieldNames.shortest_name(fields) {
										Ok(Continue({ rest: HttpHeaderState.{ raw: line_parts.after } }))
									} else {
										if name_len > FieldName.FieldNames.longest_name(fields) {
											Ok(Continue({ rest: HttpHeaderState.{ raw: line_parts.after } }))
										} else {
											Ok(
												TryFieldCaseless(
													{
														name,
														rest: HttpHeaderState.{ raw: value_start },
													},
												),
											)
										}
									}
								}
							}

							Err(NotFound) => Err(HttpHeader.BadHeader)
						}
					}
				}

			take_header_value : Str -> Try({ value : Str, after : Str }, HttpHeader)
			take_header_value = |raw|
				match Str.find_first(raw, "\r\n") {
					Ok({ before, after }) => Ok({ value: Str.trim(before), after })
					Err(NotFound) => Ok({ value: Str.trim(raw), after: "" })
				}

			underscores_to_dashes : Str -> Str
			underscores_to_dashes = |text|
				match Str.find_first(text, "_") {
					Ok({ before, after }) =>
						Str.concat(Str.concat(before, "-"), HttpHeader.underscores_to_dashes(after))

					Err(NotFound) => text
				}
		}
	}

	Str :: [ProvidedByCompiler].{
		Utf8Problem := [
			InvalidStartByte,
			UnexpectedEndOfSequence,
			ExpectedContinuation,
			OverlongEncoding,
			CodepointTooLarge,
			EncodesSurrogateHalf,
		].{
			is_eq : Utf8Problem, Utf8Problem -> Bool
		}

		is_empty : Str -> Bool
		is_empty = |str| Str.count_utf8_bytes(str) == 0

		## Concatenates two strings together.
		## ```roc
		## expect "ab".concat("cd") == "abcd"
		## expect "hello".concat("") == "hello"
		## expect "".concat("") == ""
		## ```
		concat : Str, Str -> Str

		## Determines whether or not the first Str contains the second.
		## ```roc
		## expect "foobarbaz".contains("bar")
		## expect !"apple".contains("orange")
		## expect "anything".contains("")
		## ```
		contains : Str, Str -> Bool

		## Return the [Str] with all whitespace removed from both the beginning
		## as well as the end.
		## ```roc
		## expect "   Hello      \n\n".trim() == "Hello"
		## ```
		trim : Str -> Str

		## Return the [Str] with all whitespace removed from the beginning.
		## ```roc
		## expect "   Hello      \n\n".trim_start() == "Hello      \n\n"
		## ```
		trim_start : Str -> Str

		## Return the [Str] with all whitespace removed from the end.
		## ```roc
		## expect "   Hello      \n\n".trim_end() == "   Hello"
		## ```
		trim_end : Str -> Str

		## Returns `True` if all the [ASCII characters](https://en.wikipedia.org/wiki/ASCII) in both strings are the same,
		## ignoring differences in capitalization.
		## Non-ASCII characters must all be exactly the same,
		## including capitalization. For example:
		##
		## ```roc
		##  expect "café".caseless_ascii_equals("CAFé")
		##
		##  expect !"café".caseless_ascii_equals("CAFÉ")
		## ```
		##
		## The first call returns `True` because all the ASCII characters are the same
		## when ignoring differences in capitalization, and the only non-ASCII character
		## (`é`) is the same in both strings. The second call returns `False` because
		## `é` and `É` are not ASCII characters, and they are different.
		##
		## This function is useful for things like [command-line flags](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_option)
		## and [environment variable names](https://en.wikipedia.org/wiki/Environment_variable)
		## where you know in advance that you're dealing with a string containing only ASCII characters.
		## It has better performance than lowercasing operations which take Unicode into account.
		##
		## That said, strings received from user input can always contain
		## non-ASCII Unicode characters, and lowercasing [Unicode](https://unicode.org) works
		## differently in different languages. For example, the string `"I"` lowercases to `"i"`
		## in English and to `"ı"` (a [dotless i](https://en.wikipedia.org/wiki/Dotless_I))
		## in Turkish. These rules can also change in each [Unicode release](https://www.unicode.org/releases/),
		## so we have separate [`unicode` package](https://github.com/roc-lang/unicode)
		## for Unicode capitalization that can be upgraded independently from the language's builtins.
		##
		## To convert a string's ASCII characters to uppercase or lowercase, you can use [Str.with_ascii_uppercased]
		## or [Str.with_ascii_lowercased].
		caseless_ascii_equals : Str, Str -> Bool

		## Returns a version of the string with all [ASCII characters](https://en.wikipedia.org/wiki/ASCII) lowercased.
		## Non-ASCII characters are left unmodified. For example:
		##
		## ```roc
		## expect "CALFÉ".with_ascii_lowercased() == "calfÉ"
		## ```
		##
		## This function is useful for things like [command-line flags](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_option)
		## and [environment variable names](https://en.wikipedia.org/wiki/Environment_variable)
		## where you know in advance that you're dealing with a string containing only ASCII characters.
		## It has better performance than lowercasing operations which take Unicode into account.
		##
		## That said, strings received from user input can always contain
		## non-ASCII Unicode characters, and lowercasing [Unicode](https://unicode.org) works
		## differently in different languages. For example, the string `"I"` lowercases to `"i"`
		## in English and to `"ı"` (a [dotless i](https://en.wikipedia.org/wiki/Dotless_I))
		## in Turkish. These rules can also change in each [Unicode release](https://www.unicode.org/releases/),
		## so we have separate [`unicode` package](https://github.com/roc-lang/unicode)
		## for Unicode capitalization that can be upgraded independently from the language's builtins.
		##
		## To do a case-insensitive comparison of the ASCII characters in a string,
		## you can use [Str.caseless_ascii_equals].
		with_ascii_lowercased : Str -> Str

		## Returns a version of the string with all [ASCII characters](https://en.wikipedia.org/wiki/ASCII) uppercased.
		## Non-ASCII characters are left unmodified. For example:
		##
		## ```roc
		##  expect "café".with_ascii_uppercased() == "CAFé"
		## ```
		##
		## This function is useful for things like
		## [command-line flags](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_option)
		## and [environment variable names](https://en.wikipedia.org/wiki/Environment_variable)
		## where you know in advance that you're dealing with a string containing only ASCII characters.
		## It has better performance than uppercasing operations which take Unicode into account.
		##
		## That said, strings received from user input can always contain
		## non-ASCII Unicode characters, and uppercasing [Unicode](https://unicode.org)
		## works differently in different languages.
		## For example, the string `"i"` uppercases to `"I"` in English and to `"İ"`
		## (a [dotted I](https://en.wikipedia.org/wiki/%C4%B0)) in Turkish.
		## These rules can also change in each Unicode release,
		## so we have a separate [`unicode` package](https://github.com/roc-lang/unicode) for Unicode capitalization
		## that can be upgraded independently from the language's builtins.
		##
		## To do a case-insensitive comparison of the ASCII characters in a string,
		## you can use [Str.caseless_ascii_equals].
		with_ascii_uppercased : Str -> Str

		## Check if the given [Str] starts with a value.
		## ```roc
		## expect "ABC".starts_with("A") == Bool.True
		## expect "ABC".starts_with("X") == Bool.False
		## ```
		starts_with : Str, Str -> Bool

		## Check if the given [Str] ends with a value.
		## ```roc
		## expect "ABC".ends_with("C") == Bool.True
		## expect "ABC".ends_with("X") == Bool.False
		## ```
		ends_with : Str, Str -> Bool

		## Repeats a string the given number of times.
		## ```roc
		## expect "z".repeat(3) == "zzz"
		## expect "na".repeat(8) == "nananananananana"
		## ```
		## Returns `""` when given `""` for the string or `0` for the count.
		## ```roc
		## expect "".repeat(10) == ""
		## expect "anything".repeat(0) == ""
		## ```
		repeat : Str, U64 -> Str

		## Adds a prefix to the given [Str].
		## ```roc
		## expect "Awesome".with_prefix("Roc") == "RocAwesome"
		## ```
		with_prefix : Str, Str -> Str
		with_prefix = |string, prefix| Str.concat(prefix, string)

		## Drops the given prefix [Str] from the start of a [Str]
		## If the prefix is not found, returns the original string.
		##
		## ```roc
		## expect "bar".drop_prefix("foo") == "bar"
		## expect "foobar".drop_prefix("foo") == "bar"
		## ```
		drop_prefix : Str, Str -> Str

		## Drops the given prefix from the start of a [Str], comparing ASCII
		## letters without regard to capitalization. Non-ASCII bytes must match
		## exactly.
		##
		## The returned [Str] is a slice of the original string. If the prefix is
		## not found, returns `Err(NotFound)`.
		##
		## ```roc
		## expect "Cache-Control: max-age=0".drop_prefix_caseless_ascii("cache-control") == Ok(": max-age=0")
		## expect "X-Api-Key".drop_prefix_caseless_ascii("x_api_key") == Err(NotFound)
		## ```
		drop_prefix_caseless_ascii : Str, Str -> Try(Str, [NotFound])
		drop_prefix_caseless_ascii = |source, prefix| {
			split = str_drop_prefix_caseless_ascii_raw(source, prefix)

			if split.found {
				Ok(split.after)
			} else {
				Err(NotFound)
			}
		}

		## Drops the given suffix [Str] from the end of a [Str]
		## If the suffix is not found, returns the original string.
		##
		## ```roc
		## expect "bar".drop_suffix("foo") == "bar"
		## expect "barfoo".drop_suffix("foo") == "bar"
		## ```
		drop_suffix : Str, Str -> Str

		## Finds the first occurrence of a delimiter in a string.
		##
		## The returned strings are slices of the original string.
		##
		## ```roc
		## expect "foo: bar".find_first(":") == Ok({ before: "foo", after: " bar" })
		## expect "foo".find_first(":") == Err(NotFound)
		## ```
		find_first : Str, Str -> Try({ before : Str, after : Str }, [NotFound])
		find_first = |source, delimiter| {
			split = str_find_first_raw(source, delimiter)

			if split.found {
				Ok({ before: split.before, after: split.after })
			} else {
				Err(NotFound)
			}
		}

		## Gives the number of bytes in a [Str] value.
		## ```roc
		## expect "Hello World".count_utf8_bytes() == 11
		## ```
		count_utf8_bytes : Str -> U64

		## Returns a string of the specified capacity without any content.
		##
		## This is like calling [Str.reserve] on an empty string. It's intended for
		## building up a string incrementally, for example by calling [Str.concat] on it:
		##
		## ```roc
		## greeting = "Hello and welcome to Roc"
		## subject = "Awesome Programmer"
		##
		## # Evaluates to "Hello and welcome to Roc, Awesome Programmer!"
		## # Note that string interpolation with "${greeting}, ${subject}!" would be preferred here,
		## # this is just a simple example to demonstrate `Str.with_capacity`.
		## hello_world =
		##     Str.with_capacity(45)
		##         .concat(greeting)
		##         .concat(", ")
		##         .concat(subject)
		##         .concat("!")
		## ```
		##
		## When the final size is known up front, [Str.with_capacity] guarantees that
		## subsequent calls to [Str.concat] will not need to reallocate. Whether this
		## is faster than starting from `""` depends on the system allocator: many
		## allocators can extend an existing allocation in place, making the difference
		## small or negligible. The benefit is most pronounced when reallocation would
		## otherwise force a full copy of the string.
		##
		## If you don't know the exact capacity, passing a value larger than necessary
		## still avoids reallocation, at the cost of using more memory than is needed.
		##
		## For more details, see [Str.reserve].
		with_capacity : U64 -> Str

		## Increase a string's capacity by at least the given number of additional bytes.
		##
		## When you plan to append more bytes onto a string, [Str.reserve] can help
		## avoid intermediate reallocations during a chain of [Str.concat] calls.
		## Consider the following example, which does not use [Str.reserve]:
		##
		## ```roc
		## greeting = "Hello and welcome to Roc"
		## subject = "Awesome Programmer"
		##
		## # Evaluates to "Hello and welcome to Roc, Awesome Programmer!"
		## hello_world =
		##     greeting
		##         .concat(", ")
		##         .concat(subject)
		##         .concat("!")
		## ```
		##
		## In this example:
		## 1. We start with `greeting`, which has both a length and capacity of 24 bytes.
		## 2. `.concat(", ")` sees there isn't enough capacity for 2 more bytes, so
		##    it allocates a new 26-byte buffer, copies the existing 24 bytes into
		##    it, and writes `", "` at the end. The old allocation is deallocated.
		## 3. `.concat(subject)` repeats the process: allocate a 44-byte buffer,
		##    copy the 26 existing bytes, write `"Awesome Programmer"`.
		## 4. `.concat("!")` repeats once more: allocate 45 bytes, copy 44, write `"!"`.
		##
		## With [Str.reserve], the chain only needs one allocation up front:
		##
		## ```roc
		## greeting = "Hello and welcome to Roc"
		## subject = "Awesome Programmer"
		##
		## hello_world =
		##     greeting
		##         .reserve(21)
		##         .concat(", ")
		##         .concat(subject)
		##         .concat("!")
		## ```
		##
		## Here, `.reserve(21)` ensures there is room for an additional 21 bytes
		## (`", "` + `"Awesome Programmer"` + `"!"`), allocating a 45-byte buffer
		## and copying `greeting` into it. The subsequent [Str.concat] calls all fit
		## in that capacity and do not need to reallocate.
		##
		## Whether this is actually faster depends on the system allocator: many
		## allocators can extend an existing allocation in place, in which case the
		## per-concat reallocation is cheap and [Str.reserve] makes little observable
		## difference. The benefit is most pronounced when reallocation would
		## otherwise force a full copy of the string.
		##
		## [Str.reserve] is not free — when more capacity is needed, it always
		## performs a heap allocation. Only use it when you actually expect to make
		## use of the extra capacity.
		##
		## When you don't know exactly how many bytes you'll need, choosing a value
		## somewhat higher than necessary is usually safe; a value that's too low
		## may force later reallocation, while a value much higher than necessary
		## just wastes memory.
		##
		## If you plan to use [Str.reserve] on an empty string, use [Str.with_capacity]
		## instead.
		reserve : Str, U64 -> Str

		## Shrink the memory footprint of a str such that its capacity and length are equal.
		## Note: This will also convert seamless slices to regular lists.
		release_excess_capacity : Str -> Str

		## Returns a [List] of the string's [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit).
		## (To split the string into a [List] of smaller [Str] values instead of [U8] values,
		## see [Str.split_on].)
		## ```roc
		## expect "Roc".to_utf8() == [82, 111, 99]
		## expect "鹏".to_utf8() == [233, 185, 143]
		## expect "சி".to_utf8() == [224, 174, 154, 224, 174, 191]
		## expect "🐦".to_utf8() == [240, 159, 144, 166]
		## ```
		to_utf8 : Str -> List(U8)

		## Converts a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit) to a string.
		## Any grouping of invalid byte sequences are replaced with a single unicode replacement character '�'.
		##
		## An invalid byte sequence is defined as
		## - a 2-byte-sequence starting byte, followed by less than 1 continuation byte
		## - a 3-byte-sequence starting byte, followed by less than 2 continuation bytes
		## - a 4-byte-sequence starting byte, followed by less than 3 continuation bytes
		## - an invalid codepoint from the surrogate pair block
		## - an invalid codepoint greater than 0x110000 encoded as a 4-byte sequence
		## - any valid codepoint encoded as an incorrect sequence, for instance a codepoint that should be a 2-byte sequence encoded as a 3- or 4-byte sequence
		##
		## ```roc
		## expect Str.from_utf8_lossy([82, 111, 99, 240, 159, 144, 166]) == "Roc🐦"
		## expect Str.from_utf8_lossy([82, 255, 99]) == "R�c"
		## expect Str.from_utf8_lossy([82, 0xED, 0xA0, 0xBD, 99]) == "R�c"
		## ```
		from_utf8_lossy : List(U8) -> Str

		## Converts a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit) to a string.
		##
		## Returns `Err` if the given bytes are invalid UTF-8, and returns `Ok("")` when given `[]`.
		## ```roc
		## expect Str.from_utf8([82, 111, 99]) == Ok("Roc")
		## expect Str.from_utf8([233, 185, 143]) == Ok("鹏")
		## expect Str.from_utf8([224, 174, 154, 224, 174, 191]) == Ok("சி")
		## expect Str.from_utf8([240, 159, 144, 166]) == Ok("🐦")
		## expect Str.from_utf8([]) == Ok("")
		## expect Str.from_utf8([255]).is_err()
		## ```
		from_utf8 : List(U8) -> Try(Str, [BadUtf8({ problem : Str.Utf8Problem, index : U64 }), ..])

		## Converts a string literal to a [Str].
		##
		## The compiler calls this when a string literal's type is [Str], passing
		## the literal's contents after escape processing.
		## ```roc
		## expect Str.from_quote("Roc") == Ok("Roc")
		## ```
		from_quote : Str -> Try(Str, [BadQuotedBytes(Str)])
		from_quote = |str| Ok(str)

		## Assembles an interpolated string literal.
		##
		## The compiler calls this when a string literal contains interpolations:
		## the first argument is the literal segment before the first
		## interpolation, and the iterator yields each interpolated value paired
		## with the literal segment that follows it.
		from_interpolation : Str, Iter((Str, Str)) -> Str
		from_interpolation = |first, rest|
			rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment))

		## Split a string around a separator.
		##
		## Passing `""` for the separator is not useful;
		## it returns the original string wrapped in a [List].
		## ```roc
		## expect "1,2,3".split_on(",") == ["1","2","3"]
		## expect "1,2,3".split_on("") == ["1,2,3"]
		## ```
		split_on : Str, Str -> List(Str)

		## Combines a [List] of strings into a single string, with a separator
		## string in between each.
		## ```roc
		## expect Str.join_with(["one", "two", "three"], ", ") == "one, two, three"
		## expect Str.join_with(["1", "2", "3", "4"], ".") == "1.2.3.4"
		## ```
		join_with : List(Str), Str -> Str

		## Returns `True` if the two strings are exactly the same.
		is_eq : Str, Str -> Bool

		## Feed a string into a [Hasher].
		to_hash : Str, Hasher -> Hasher
		to_hash = |str, hasher| Hasher.write_str(hasher, str)

		## Returns a human-readable representation of a value, useful for debugging.
		inspect : _val -> Str

		## Encode a string using a format that provides encode_str
		encode : Str, fmt -> Try(encoded, err)
			where [fmt.encode_str : fmt, Str -> Try(encoded, err)]
		encode = |self, format| {
			format.encode_str(self)
		}

		## Decode a string using a format that provides decode_str
		decode : src, fmt -> (Try(Str, err), src)
			where [fmt.decode_str : fmt, src -> (Try(Str, err), src)]
		decode = |source, format| {
			Fmt : fmt
			Fmt.decode_str(format, source)
		}
	}

	Hasher :: { state : U64 }.{

		## Feed a [Bool] into the hasher.
		write_bool : Hasher, Bool -> Hasher

		## Feed a [U8] into the hasher.
		write_u8 : Hasher, U8 -> Hasher

		## Feed a [U16] into the hasher.
		write_u16 : Hasher, U16 -> Hasher

		## Feed a [U32] into the hasher.
		write_u32 : Hasher, U32 -> Hasher

		## Feed a [U64] into the hasher.
		write_u64 : Hasher, U64 -> Hasher

		## Feed a [U128] into the hasher.
		write_u128 : Hasher, U128 -> Hasher

		## Feed an [I8] into the hasher.
		write_i8 : Hasher, I8 -> Hasher

		## Feed an [I16] into the hasher.
		write_i16 : Hasher, I16 -> Hasher

		## Feed an [I32] into the hasher.
		write_i32 : Hasher, I32 -> Hasher

		## Feed an [I64] into the hasher.
		write_i64 : Hasher, I64 -> Hasher

		## Feed an [I128] into the hasher.
		write_i128 : Hasher, I128 -> Hasher

		## Feed an [F32] into the hasher.
		write_f32 : Hasher, F32 -> Hasher

		## Feed an [F64] into the hasher.
		write_f64 : Hasher, F64 -> Hasher

		## Feed a [Dec] into the hasher.
		write_dec : Hasher, Dec -> Hasher

		## Feed bytes into the hasher.
		write_bytes : Hasher, List(U8) -> Hasher

		## Feed a string into the hasher.
		write_str : Hasher, Str -> Hasher
	}

	## Cryptographic digest functions for byte data.
	##
	## Use `Crypto.SHA256.hash(bytes)` or `Crypto.BLAKE3.hash(bytes)` when the
	## whole input is already in memory. The result is an algorithm-specific
	## typed digest, so SHA-256 digests compare only with SHA-256 digests and
	## BLAKE3 digests compare only with BLAKE3 digests.
	##
	## Digest bytes and hex are both available: `to_bytes` returns the raw
	## 32-byte digest, while `to_hex` returns 64 lowercase hexadecimal
	## characters. `from_bytes` and `from_hex` validate that external digest data
	## has the exact expected length before producing a typed digest.
	##
	## For input that arrives in pieces, use `hash_chunks(chunks)` with an
	## `Iter(List(U8))`, or use `Hasher.empty`, `Hasher.write`, and
	## `Hasher.finish` manually. `finish` observes the current hasher state; it
	## does not consume the prior value.
	##
	## These are digest APIs. They are not password hashing, KDF, HMAC, keyed
	## BLAKE3, or digital signature APIs.
	Crypto :: {}.{
		DigestBytesErr := [WrongLength({ expected : U64, actual : U64 })].{}
		DigestHexErr := [
			WrongLength({ expected : U64, actual : U64 }),
			InvalidHex({ index : U64, byte : U8 }),
		].{}

		## SHA-256 hashing.
		##
		## Use `hash` for one input list, `hash_chunks` for an iterator of byte
		## chunks, and `Hasher` when you want to thread the state yourself.
		SHA256 :: {}.{

			## A typed SHA-256 digest.
			Digest :: { bytes : List(U8) }.{

				## Return this SHA-256 digest as its 32 raw bytes.
				to_bytes : Digest -> List(U8)
				to_bytes = |digest| digest.bytes

				## Return this SHA-256 digest as 64 lowercase hexadecimal characters.
				to_hex : Digest -> Str
				to_hex = |digest| crypto_digest_to_hex(digest.bytes)

				## Build a SHA-256 digest from exactly 32 raw bytes.
				from_bytes : List(U8) -> Try(Digest, Crypto.DigestBytesErr)
				from_bytes = |bytes|
					match crypto_digest_from_bytes(bytes) {
						Ok(valid_bytes) => Ok(Digest.{ bytes: valid_bytes })
						Err(err) => Err(err)
					}

				## Build a SHA-256 digest from exactly 64 ASCII hex characters.
				## Uppercase and lowercase hex are accepted; `0x` prefixes are not.
				from_hex : Str -> Try(Digest, Crypto.DigestHexErr)
				from_hex = |hex|
					match crypto_digest_from_hex(hex) {
						Ok(bytes) => Ok(Digest.{ bytes })
						Err(err) => Err(err)
					}

				## Returns `True` if both SHA-256 digests contain the same bytes.
				is_eq : Digest, Digest -> Bool
				is_eq = |left, right| List.is_eq(left.bytes, right.bytes)

				## Feed the digest bytes into a regular non-cryptographic [Hasher].
				to_hash : Digest, Builtin.Hasher -> Builtin.Hasher
				to_hash = |digest, hasher| Builtin.Hasher.write_bytes(hasher, digest.bytes)

				## Return a typed, human-readable SHA-256 digest.
				to_inspect : Digest -> Str
				to_inspect = |digest| Str.concat("SHA256.Digest(\"", Str.concat(Digest.to_hex(digest), "\")"))
			}

			## Incremental SHA-256 state.
			Hasher :: { state : List(U8) }.{

				## Start a fresh SHA-256 hasher.
				empty : () -> Hasher
				empty = || Hasher.{ state: crypto_sha256_hasher_empty() }

				## Add bytes to this SHA-256 hasher and return the updated hasher.
				write : Hasher, List(U8) -> Hasher
				write = |hasher, bytes| Hasher.{ state: crypto_sha256_hasher_write(hasher.state, bytes) }

				## Finish a SHA-256 hasher snapshot and return its digest.
				## The hasher value is immutable, so callers can also continue writing to the prior state.
				finish : Hasher -> Digest
				finish = |hasher| Digest.{ bytes: crypto_sha256_hasher_finish(hasher.state) }
			}

			## Hash all bytes in one SHA-256 operation.
			##
			## ```roc
			## digest = Crypto.SHA256.hash("abc".to_utf8())
			## hex = digest.to_hex()
			## ```
			hash : List(U8) -> Digest
			hash = |bytes| Digest.{ bytes: crypto_sha256_hash_bytes(bytes) }

			## Hash byte chunks in order with SHA-256.
			##
			## ```roc
			## digest = Crypto.SHA256.hash_chunks(["a".to_utf8(), "bc".to_utf8()].iter())
			## ```
			hash_chunks : Iter(List(U8)) -> Digest
			hash_chunks = |chunks| Hasher.finish(Iter.fold(chunks, Hasher.empty(), Hasher.write))
		}

		## BLAKE3 hashing in the default unkeyed digest mode.
		##
		## Use `hash` for one input list, `hash_chunks` for an iterator of byte
		## chunks, and `Hasher` when you want to thread the state yourself.
		BLAKE3 :: {}.{

			## A typed BLAKE3 digest.
			Digest :: { bytes : List(U8) }.{

				## Return this BLAKE3 digest as its 32 raw bytes.
				to_bytes : Digest -> List(U8)
				to_bytes = |digest| digest.bytes

				## Return this BLAKE3 digest as 64 lowercase hexadecimal characters.
				to_hex : Digest -> Str
				to_hex = |digest| crypto_digest_to_hex(digest.bytes)

				## Build a BLAKE3 digest from exactly 32 raw bytes.
				from_bytes : List(U8) -> Try(Digest, Crypto.DigestBytesErr)
				from_bytes = |bytes|
					match crypto_digest_from_bytes(bytes) {
						Ok(valid_bytes) => Ok(Digest.{ bytes: valid_bytes })
						Err(err) => Err(err)
					}

				## Build a BLAKE3 digest from exactly 64 ASCII hex characters.
				## Uppercase and lowercase hex are accepted; `0x` prefixes are not.
				from_hex : Str -> Try(Digest, Crypto.DigestHexErr)
				from_hex = |hex|
					match crypto_digest_from_hex(hex) {
						Ok(bytes) => Ok(Digest.{ bytes })
						Err(err) => Err(err)
					}

				## Returns `True` if both BLAKE3 digests contain the same bytes.
				is_eq : Digest, Digest -> Bool
				is_eq = |left, right| List.is_eq(left.bytes, right.bytes)

				## Feed the digest bytes into a regular non-cryptographic [Hasher].
				to_hash : Digest, Builtin.Hasher -> Builtin.Hasher
				to_hash = |digest, hasher| Builtin.Hasher.write_bytes(hasher, digest.bytes)

				## Return a typed, human-readable BLAKE3 digest.
				to_inspect : Digest -> Str
				to_inspect = |digest| Str.concat("BLAKE3.Digest(\"", Str.concat(Digest.to_hex(digest), "\")"))
			}

			## Incremental BLAKE3 state.
			Hasher :: { state : List(U8) }.{

				## Start a fresh BLAKE3 hasher.
				empty : () -> Hasher
				empty = || Hasher.{ state: crypto_blake3_hasher_empty() }

				## Add bytes to this BLAKE3 hasher and return the updated hasher.
				write : Hasher, List(U8) -> Hasher
				write = |hasher, bytes| Hasher.{ state: crypto_blake3_hasher_write(hasher.state, bytes) }

				## Finish a BLAKE3 hasher snapshot and return its digest.
				## The hasher value is immutable, so callers can also continue writing to the prior state.
				finish : Hasher -> Digest
				finish = |hasher| Digest.{ bytes: crypto_blake3_hasher_finish(hasher.state) }
			}

			## Hash all bytes in one BLAKE3 operation.
			##
			## ```roc
			## digest = Crypto.BLAKE3.hash("abc".to_utf8())
			## hex = digest.to_hex()
			## ```
			hash : List(U8) -> Digest
			hash = |bytes| Digest.{ bytes: crypto_blake3_hash_bytes(bytes) }

			## Hash byte chunks in order with BLAKE3.
			##
			## ```roc
			## digest = Crypto.BLAKE3.hash_chunks(["a".to_utf8(), "bc".to_utf8()].iter())
			## ```
			hash_chunks : Iter(List(U8)) -> Digest
			hash_chunks = |chunks| Hasher.finish(Iter.fold(chunks, Hasher.empty(), Hasher.write))
		}
	}

	Iter(item) :: {
		# The sequence being iterated, or e.g. a range, is captured in the step thunk.
		len_if_known : [Known(U64), Unknown],
		step : () -> [One({ item : item, rest : Iter(item) }), Skip({ rest : Iter(item) }), Done],
	}.{
		# The general unfold. `advance` maps a seed to either the next item paired with the
		# next seed, or `NoMore`. `custom` owns rebuilding the rest from the new seed, so the
		# seed type stays hidden inside the step closure and never appears in `Iter(item)`.
		custom : state, [Known(U64), Unknown], (state -> Try((item, state), [NoMore])) -> Iter(item)
		custom = |seed, len_if_known, advance| {
			len_if_known,
			step: ||
				match advance(seed) {
					Ok((item, next_seed)) =>
						One(
							{
								item,
								rest: Iter.custom(
									next_seed,
									match len_if_known {
										Known(l) => Known(l - 1)
										Unknown => Unknown
									},
									advance,
								),
							},
						)
					Err(NoMore) => Done
				},
		}

		## Iterator over `num` values from `start` up to but not including `end`.
		## Returns an empty iterator if `start >= end`. This is what `start..<end` desugars to.
		exclusive_range : num, num -> Iter(num)
			where [
				num.is_lt : num, num -> Bool,
				num.add_try : num, num -> Try(num, [Overflow]),
				num.from_numeral : Builtin.Num.Numeral -> Try(num, [InvalidNumeral(Str)]),
				num.steps_between : num, num -> [Known(U64), Unknown],
			]
		exclusive_range = |start, end| {
			len_if_known: start.steps_between(end),
			step: ||
				if start < end {
					One(
						{
							item: start,
							rest: match start.add_try(1) {
								Ok(next) => if next < end {
									Iter.exclusive_range(next, end)
								} else {
									range_done()
								}
								Err(Overflow) => range_done()
							},
						},
					)
				} else {
					Done
				},
		}

		## Iterator over `num` values from `start` up to and including `end`.
		## Returns an empty iterator if `start > end`. This is what `start..=end` desugars to.
		inclusive_range : num, num -> Iter(num)
			where [
				num.is_lte : num, num -> Bool,
				num.add_try : num, num -> Try(num, [Overflow]),
				num.from_numeral : Builtin.Num.Numeral -> Try(num, [InvalidNumeral(Str)]),
				num.steps_between : num, num -> [Known(U64), Unknown],
			]
		inclusive_range = |start, end| {
			len_if_known: if start <= end {
				match start.steps_between(end) {
					Known(n) => match n.add_try(1) {
						Ok(len) => Known(len)
						Err(Overflow) => Unknown
					}
					Unknown => Unknown
				}
			} else {
				Known(0)
			},
			step: ||
				if start <= end {
					One(
						{
							item: start,
							rest: match start.add_try(1) {
								Ok(next) => if next <= end {
									Iter.inclusive_range(next, end)
								} else {
									range_done()
								}
								Err(Overflow) => range_done()
							},
						},
					)
				} else {
					Done
				},
		}

		iter : Iter(item) -> Iter(item)
		iter = |self| self

		## Returns an iterator that yields exactly one item.
		## ```roc
		## expect Iter.fold(Iter.single(42.I64), [], |acc, item| acc.append(item)) == [42]
		## ```
		single : item -> Iter(item)
		single = |item| {
			len_if_known: Known(1),
			step: || One({ item, rest: range_done() }),
		}

		## Returns an iterator that yields the given item first, followed by
		## everything the given iterator yields.
		##
		## The compiler uses this to assemble the iterator it passes to
		## `from_interpolation` when checking an interpolated string literal.
		## ```roc
		## expect Iter.fold([2, 3].iter().prepended(1), [], |acc, item| acc.append(item)) == [1, 2, 3]
		## ```
		prepended : Iter(item), item -> Iter(item)
		prepended = |rest, item| {
			len_if_known: match rest.len_if_known {
				Known(n) => Known(n + 1)
				Unknown => Unknown
			},
			step: || One({ item, rest }),
		}

		## Returns an iterator that yields all items from the first iterator,
		## then all items from the second iterator.
		## ```roc
		## expect Iter.fold([1.I64, 2].iter().concat([3, 4].iter()), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
		## ```
		concat : Iter(item), Iter(item) -> Iter(item)
		concat = |first, second| {
			len_if_known: match first.len_if_known {
				Known(first_len) => match second.len_if_known {
					Known(second_len) => if second_len > 18446744073709551615 - first_len {
						Unknown
					} else {
						Known(first_len + second_len)
					}
					Unknown => Unknown
				}
				Unknown => Unknown
			},
			step: ||
				match Iter.next(first) {
					Done => Iter.next(second)
					Skip({ rest }) => Skip({ rest: Iter.concat(rest, second) })
					One({ item, rest }) => One({ item, rest: Iter.concat(rest, second) })
				},
		}

		## Returns an iterator that yields everything from the given iterator,
		## followed by the given item.
		## ```roc
		## expect Iter.fold([1.I64, 2].iter().append(3), [], |acc, item| acc.append(item)) == [1, 2, 3]
		## ```
		append : Iter(item), item -> Iter(item)
		append = |iterator, last| {
			len_if_known: match iterator.len_if_known {
				Known(len) => if len == 18446744073709551615 {
					Unknown
				} else {
					Known(len + 1)
				}
				Unknown => Unknown
			},
			step: ||
				match Iter.next(iterator) {
					Done => One({ item: last, rest: range_done() })
					Skip({ rest }) => Skip({ rest: Iter.append(rest, last) })
					One({ item, rest }) => One({ item, rest: Iter.append(rest, last) })
				},
		}

		next : Iter(item) -> [One({ item : item, rest : Iter(item) }), Skip({ rest : Iter(item) }), Done]
		next = |iterator| (iterator.step)()

		map : Iter(a), (a -> b) -> Iter(b)
		map = |iterator, transform|
			match iterator {
				{ len_if_known, step } => {
					len_if_known,
					step: ||
						match step() {
							Done => Done
							Skip({ rest }) => Skip({ rest: Iter.map(rest, transform) })
							One({ item, rest }) => One({ item: transform(item), rest: Iter.map(rest, transform) })
						},
				}
			}

		keep_if : Iter(a), (a -> Bool) -> Iter(a)
		keep_if = |iterator, predicate|
			match iterator {
				{ step, .. } => {
					len_if_known: Unknown,
					step: || {
						match step() {
							Done => Done
							Skip({ rest }) => Skip({ rest: Iter.keep_if(rest, predicate) })
							One({ item, rest }) =>
								if predicate(item) {
									One({ item, rest: Iter.keep_if(rest, predicate) })
								} else {
									Skip({ rest: Iter.keep_if(rest, predicate) })
								}
							}
					},
				}
			}

		drop_if : Iter(a), (a -> Bool) -> Iter(a)
		drop_if = |iterator, predicate|
			match iterator {
				{ step, .. } => {
					len_if_known: Unknown,
					step: || {
						match step() {
							Done => Done
							Skip({ rest }) => Skip({ rest: Iter.drop_if(rest, predicate) })
							One({ item, rest }) =>
								if predicate(item) {
									Skip({ rest: Iter.drop_if(rest, predicate) })
								} else {
									One({ item, rest: Iter.drop_if(rest, predicate) })
								}
							}
					},
				}
			}

		fold : Iter(a), acc, (acc, a -> acc) -> acc
		fold = |iterator, acc, step|
			match Iter.next(iterator) {
				Done => acc
				Skip({ rest }) => Iter.fold(rest, acc, step)
				One({ item, rest }) => Iter.fold(rest, step(acc, item), step)
			}

		## Returns the iterator's length if it is known up front, so collections
		## can pre-size their allocation.
		size_hint : Iter(item) -> [Known(U64), Unknown]
		size_hint = |iterator| match iterator {
			{ len_if_known, .. } => len_if_known
		}

		## Collect this iterator into any output type that provides `from_iter`.
		collect : Iter(item) -> output
			where [output.from_iter : Iter(item) -> output]
		collect = |iterator| {
			Output : output
			Output.from_iter(iterator)
		}

		## Lift this pure iterator into an effectful [Stream], so it can be combined
		## with effectful operations like [Stream.map].
		stream : Iter(item) -> Stream(item)
		stream = |iterator| Stream.from_iter(iterator)

		## Returns an iterator that yields at most the first `n` items of this iterator.
		## If the source has fewer than `n` items, all of them are yielded.
		## ```roc
		## expect Iter.fold(Iter.take_first(List.iter([1, 2, 3, 4, 5]), 3), [], |acc, item| acc.append(item)) == [1, 2, 3]
		##
		## expect Iter.fold(Iter.take_first(List.iter([1, 2]), 5), [], |acc, item| acc.append(item)) == [1, 2]
		## ```
		take_first : Iter(item), U64 -> Iter(item)
		take_first = |iterator, n|
			if n == 0 {
				range_done()
			} else {
				match iterator {
					{ len_if_known, step } => {
						len_if_known: match len_if_known {
							Known(len) => Known(
								if len < n {
									len
								} else {
									n
								},
							)
							Unknown => Unknown
						},
						step: ||
							match step() {
								Done => Done
								Skip({ rest }) => Skip({ rest: Iter.take_first(rest, n) })
								One({ item, rest }) => One({ item, rest: Iter.take_first(rest, n - 1) })
							},
					}
				}
			}

		## Returns an iterator that skips the first `n` items of this iterator.
		## If the source has `n` or fewer items, the result is empty.
		## ```roc
		## expect Iter.fold(Iter.drop_first(List.iter([1, 2, 3, 4, 5]), 2), [], |acc, item| acc.append(item)) == [3, 4, 5]
		##
		## expect Iter.fold(Iter.drop_first(List.iter([1, 2, 3]), 10), [], |acc, item| acc.append(item)) == []
		## ```
		drop_first : Iter(item), U64 -> Iter(item)
		drop_first = |iterator, n|
			if n == 0 {
				iterator
			} else {
				match iterator {
					{ len_if_known, step } => {
						len_if_known: match len_if_known {
							Known(len) => Known(
								if len < n {
									0
								} else {
									len - n
								},
							)
							Unknown => Unknown
						},
						step: ||
							match step() {
								Done => Done
								Skip({ rest }) => Skip({ rest: Iter.drop_first(rest, n) })
								One({ item: _, rest }) => Skip({ rest: Iter.drop_first(rest, n - 1) })
							},
					}
				}
			}

		## Returns an iterator that yields the last `n` items of this iterator.
		## If the source has fewer than `n` items, all of them are yielded.
		##
		## When the source iterator's length is unknown, this materializes the
		## source into a list to find where the last `n` items begin. Avoid
		## calling this on iterators whose length is unknown and might be huge.
		## ```roc
		## expect Iter.fold(Iter.take_last(List.iter([1, 2, 3, 4, 5]), 3), [], |acc, item| acc.append(item)) == [3, 4, 5]
		##
		## expect Iter.fold(Iter.take_last(List.iter([1, 2]), 5), [], |acc, item| acc.append(item)) == [1, 2]
		## ```
		take_last : Iter(item), U64 -> Iter(item)
		take_last = |iterator, n|
			match iterator.len_if_known {
				Known(len) =>
					if len <= n {
						iterator
					} else {
						Iter.drop_first(iterator, len - n)
					}
				Unknown =>
					List.iter(List.take_last(Iter.fold(iterator, [], |acc, item| acc.append(item)), n))
				}

		## Returns an iterator that yields all items except the last `n`.
		## If the source has `n` or fewer items, the result is empty.
		##
		## When the source iterator's length is unknown, this materializes the
		## source into a list to find where the last `n` items begin. Avoid
		## calling this on iterators whose length is unknown and might be huge.
		## ```roc
		## expect Iter.fold(Iter.drop_last(List.iter([1, 2, 3, 4, 5]), 2), [], |acc, item| acc.append(item)) == [1, 2, 3]
		##
		## expect Iter.fold(Iter.drop_last(List.iter([1, 2, 3]), 10), [], |acc, item| acc.append(item)) == []
		## ```
		drop_last : Iter(item), U64 -> Iter(item)
		drop_last = |iterator, n|
			match iterator.len_if_known {
				Known(len) =>
					if len <= n {
						range_done()
					} else {
						Iter.take_first(iterator, len - n)
					}
				Unknown =>
					List.iter(List.drop_last(Iter.fold(iterator, [], |acc, item| acc.append(item)), n))
				}

		## Returns an iterator that yields the first item and then every `n`th item
		## after it, skipping the `n - 1` items in between. A step of `0` yields an
		## empty iterator.
		## ```roc
		## expect Iter.fold(Iter.step_by(List.iter([1, 2, 3, 4, 5]), 2), [], |acc, item| acc.append(item)) == [1, 3, 5]
		##
		## expect Iter.fold(Iter.step_by(List.iter([1, 2, 3]), 0), [], |acc, item| acc.append(item)) == []
		## ```
		step_by : Iter(item), U64 -> Iter(item)
		step_by = |iterator, n|
			if n == 0 {
				range_done()
			} else {
				match iterator {
					{ len_if_known, step } => {
						len_if_known: match len_if_known {
							Known(len) => Known(
								if len == 0 {
									0
								} else {
									(len - 1) / n + 1
								},
							)
							Unknown => Unknown
						},
						step: ||
							match step() {
								Done => Done
								Skip({ rest }) => Skip({ rest: Iter.step_by(rest, n) })
								One({ item, rest }) => One({ item, rest: Iter.step_by(Iter.drop_first(rest, n - 1), n) })
							},
					}
				}
			}

		## Returns an iterator that yields this iterator's items in reverse order.
		##
		## Because an [Iter] only moves forward, this materializes the source into a
		## list to reverse it. The result always has a known length, so collecting it
		## is efficient. Avoid calling this on iterators whose length is unknown and
		## might be huge.
		## ```roc
		## expect Iter.fold(Iter.rev(List.iter([1, 2, 3])), [], |acc, item| acc.append(item)) == [3, 2, 1]
		##
		## expect Iter.fold(Iter.rev(List.iter([])), [], |acc, item| acc.append(item)) == []
		## ```
		rev : Iter(item) -> Iter(item)
		rev = |iterator|
			List.iter(List.rev(Iter.fold(iterator, [], |acc, item| acc.append(item))))
	}

	## An effectful iterator: identical to [Iter] except that its `step!` thunk is
	## effectful, so combinators like [Stream.map!] can run effects per element while
	## staying lazy. Produced from an [Iter] via [Iter.map!] and driven by [Stream.collect!].
	Stream(item) :: {
		len_if_known : [Known(U64), Unknown],
		step! : () => [One({ item : item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done],
	}.{

		## Lift a pure [Iter] into a [Stream]. The lifted steps do no effect themselves,
		## but the stream can then be combined with effectful operations like [Stream.map].
		## Carries the source's length forward so [Stream.collect!] can pre-size.
		from_iter : Iter(item) -> Stream(item)
		from_iter = |iterator|
			{
				len_if_known: Iter.size_hint(iterator),
				step!: ||
					match Iter.next(iterator) {
						Done => Done
						Skip({ rest }) => Skip({ rest: Stream.from_iter(rest) })
						One({ item, rest }) => One({ item, rest: Stream.from_iter(rest) })
					},
			}

		## Transform each item of this stream. The transform may run effects; because
		## the stream's steps are already effectful, building the mapped stream stays
		## lazy (the transform runs only as the stream is driven).
		map : Stream(a), (a => b) -> Stream(b)
		map = |stream, transform!|
			match stream {
				{ len_if_known, step! } => {
					len_if_known,
					step!: ||
						match step!() {
							Done => Done
							Skip({ rest }) => Skip({ rest: Stream.map(rest, transform!) })
							One({ item, rest }) => One({ item: transform!(item), rest: Stream.map(rest, transform!) })
						},
				}
			}

		## Transform each item of this stream with an effectful function.
		map! : Stream(a), (a => b) => Stream(b)
		map! = |stream, transform!|
			match stream {
				{ len_if_known, step! } => {
					len_if_known,
					step!: ||
						match step!() {
							Done => Done
							Skip({ rest }) => Skip({ rest: Stream.map!(rest, transform!) })
							One({ item, rest }) => One({ item: transform!(item), rest: Stream.map!(rest, transform!) })
						},
				}
			}

		## Advance the stream by one step.
		next! : Stream(item) => [One({ item : item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done]
		next! = |stream| match stream {
			{ step!, .. } => step!()
		}

		## Returns the stream's length if it is known up front.
		size_hint : Stream(item) -> [Known(U64), Unknown]
		size_hint = |stream| match stream {
			{ len_if_known, .. } => len_if_known
		}

		## Drive the stream to completion with an explicit loop, collecting its items
		## into a [List] (pre-sized from `len_if_known` when known).
		collect! : Stream(item) => List(item)
		collect! = |stream| {
			# `Known(n)` guarantees exactly n items (count-changing combinators
			# report `Unknown`), so reserve up front and use the unchecked append.
			# When the length is unknown, start empty and grow with the reserving
			# append — the unchecked append would corrupt a zero-capacity list.
			length = Stream.size_hint(stream)
			cap = match length {
				Known(n) => n
				Unknown => 0
			}
			var $list = List.with_capacity(cap)
			var $rest = stream
			while Bool.True {
				match Stream.next!($rest) {
					Done => {
						break
					}
					Skip({ rest }) => {
						$rest = rest
					}
					One({ item, rest }) => {
						$list = match length {
							Known(_) => list_append_unsafe($list, item)
							Unknown => List.append($list, item)
						}
						$rest = rest
					}
				}
			}
			$list
		}
	}

	List(_item) :: [ProvidedByCompiler].{

		## Returns the length of the list, which is equal to the number of elements it contains.
		##
		## One [List] can store up to [I64.highest] elements on 64-bit targets and [I32.highest] on 32-bit targets like wasm.
		## This means the #U64 this function returns can always be safely converted to #I64 or #I32, depending on the target.
		len : List(_item) -> U64

		##  Check if the list is empty.
		## ```roc
		## [1, 2, 3].is_empty()
		##
		## [].is_empty()
		## ```
		is_empty : List(_item) -> Bool
		is_empty = |list| List.len(list) == 0

		## Iterate over the list from first to last.
		iter : List(item) -> Iter(item)
		iter = |list| {
			make = |index| {
				len = List.len(list)

				{
					len_if_known: Known(len - index),
					step: ||
						if index == len {
							Done
						} else {
							One({ item: list_get_unsafe(list, index), rest: make(index + 1) })
						},
				}
			}

			make(0)
		}

		## Build a list from a pure [Iter], pre-sizing the allocation from the
		## iterator's `len_if_known` when it is known up front. This is the `from_iter`
		## that [Iter.collect] dispatches to.
		from_iter : Iter(item) -> List(item)
		from_iter = |iterator| {
			# `Known(n)` guarantees the iterator yields exactly n items: every
			# combinator that can change the count (e.g. keep_if) reports `Unknown`.
			# So when the length is known we reserve the whole allocation up front
			# and write each item with the unchecked append; when it is unknown we
			# start empty and grow with the reserving append instead.
			length = iterator.len_if_known
			cap = match length {
				Known(n) => n
				Unknown => 0
			}

			var $list = List.with_capacity(cap)
			var $rest = iterator

			while Bool.True {
				match Iter.next($rest) {
					Done => {
						break
					}
					Skip({ rest }) => {
						$rest = rest
					}
					One({ item, rest }) => {
						$list = match length {
							Known(_) => list_append_unsafe($list, item)
							Unknown => List.append($list, item)
						}
						$rest = rest
					}
				}
			}

			$list
		}

		## Put two lists together.
		## ```roc
		## [1.I64, 2, 3].concat([4, 5])
		##
		## [0.I64, 1, 2].concat([3, 4])
		## ```
		concat : List(item), List(item) -> List(item)

		## Create a list with space for at least capacity elements
		with_capacity : U64 -> List(item)

		## Ensure this list has room for at least spare additional elements.
		reserve : List(item), U64 -> List(item)
		reserve = |list, spare| list_reserve(list, spare)

		## Reduce memory usage by trimming unused capacity.
		release_excess_capacity : List(item) -> List(item)
		release_excess_capacity = |list| list_release_excess_capacity(list)

		## Sort a list using a custom comparison function. The comparator receives two
		## elements and returns `LT`, `EQ`, or `GT` to indicate their relative order.
		## ```roc
		## expect [3, 1, 2].sort_with(|a, b| if a < b LT else if a > b GT else EQ) == [1, 2, 3]
		##
		## # Sort in descending order by swapping the LT and GT
		## expect [3, 1, 2].sort_with(|a, b| if a > b LT else if a < b GT else EQ) == [3, 2, 1]
		## ```
		sort_with : List(item), (item, item -> [LT, EQ, GT]) -> List(item)
		sort_with = |list, order| {
			list_len = List.len(list)

			if list_len < 2 {
				list
			} else {
				match List.first(list) {
					Ok(pivot) => {
						rest = List.drop_first(list, 1)
						less_or_equal = 
							List.keep_if(
								rest,
								|item|
									match order(item, pivot) {
										LT => True
										EQ => True
										GT => False
									},
							)
						greater = 
							List.keep_if(
								rest,
								|item|
									match order(item, pivot) {
										LT => False
										EQ => False
										GT => True
									},
							)

						List.concat(
							List.sort_with(less_or_equal, order),
							List.concat(List.single(pivot), List.sort_with(greater, order)),
						)
					}

					Err(_) => list
				}
			}
		}

		## Returns `True` if the two lists have the same length and their elements are pairwise equal.
		is_eq : List(item), List(item) -> Bool
			where [item.is_eq : item, item -> Bool]
		is_eq = |self, other| {
			if self.len() != other.len() {
				return False
			}

			var $index = 0

			while $index < self.len() {
				if list_get_unsafe(self, $index) != list_get_unsafe(other, $index) {
					return False
				}

				$index = $index + 1
			}

			True
		}

		## Feed a list into a [Hasher].
		to_hash : List(item), Hasher -> Hasher
			where [item.to_hash : item, Hasher -> Hasher]
		to_hash = |list, hasher| {
			Item : item
			var $next = Hasher.write_u64(hasher, List.len(list))
			for item in list {
				$next = Item.to_hash(item, $next)
			}
			$next
		}

		## Add a single element to the end of a list.
		## ```roc
		## [1.I64, 2, 3].append(4)
		##
		## [0.I64, 1, 2].append(3)
		## ```
		append : List(a), a -> List(a)
		append = |list, item| {
			reserved = List.reserve(list, 1)
			list_append_unsafe(reserved, item)
		}

		## Add a single element to the beginning of a list.
		## ```roc
		## expect [2, 3, 4].prepend(1) == [1, 2, 3, 4]
		##
		## expect [].prepend(0) == [0]
		## ```
		prepend : List(a), a -> List(a)

		## Returns the first element in the list, or `ListWasEmpty` if it was empty.
		## ```roc
		## expect [1, 2, 3].first() == Ok(1)
		## expect [].first() == Err(ListWasEmpty)
		## ```
		first : List(item) -> Try(item, [ListWasEmpty, ..])
		first = |list| if List.is_empty(list) {
			Try.Err(ListWasEmpty)
		} else {
			Try.Ok(list_get_unsafe(list, 0))
		}

		## Returns an element from a list at the given index.
		##
		## Returns `Err OutOfBounds` if the given index exceeds the List's length
		## ```roc
		## expect [100, 200, 300].get(1) == Ok(200)
		## expect [100, 200, 300].get(5) == Err(OutOfBounds)
		## ```
		get : List(item), U64 -> Try(item, [OutOfBounds, ..])
		get = |list, index| if index < List.len(list) {
			Try.Ok(list_get_unsafe(list, index))
		} else {
			Try.Err(OutOfBounds)
		}

		## Alias for [List.get], enabling the future `list[index]` subscript operator.
		## Returns an element from a list at the given index.
		##
		## Returns `Err OutOfBounds` if the given index exceeds the List's length
		## ```roc
		## expect ["bird", "lizard"].subscript(0) == Ok("bird")
		## expect ["bird", "lizard"].subscript(5) == Err(OutOfBounds)
		## ```
		subscript : List(item), U64 -> Try(item, [OutOfBounds, ..])
		subscript = |list, index| List.get(list, index)

		## Replaces the element at the given index with a new value.
		## ```roc
		## expect [10, 20, 30].set(1, 99) == Ok([10, 99, 30])
		##
		## expect [10, 20, 30].set(5, 99) == Err(OutOfBounds)
		## ```
		set : List(a), U64, a -> Try(List(a), [OutOfBounds, ..])
		set = |list, index, value|
			if index < List.len(list) {
				Ok(list_set_unsafe(list, index, value))
			} else {
				Err(OutOfBounds)
			}

		## Replaces the element at the given index, returning both the updated list
		## and the value that was replaced.
		## ```roc
		## expect [10, 20, 30].replace(1, 99) == Ok({ list: [10, 99, 30], prev: 20 })
		## expect [10, 20, 30].replace(5, 99) == Err(OutOfBounds)
		## ```
		replace : List(a), U64, a -> Try({ list : List(a), prev : a }, [OutOfBounds, ..])
		replace = |list, index, new_value|
			if index < List.len(list) {
				Ok(list_replace_unsafe(list, index, new_value))
			} else {
				Err(OutOfBounds)
			}

		## Updates the element at the given index by applying a function to it.
		## ```roc
		## expect [10, 20, 30].update(1, |x| x + 5) == Ok([10, 25, 30])
		##
		## expect [10, 20, 30].update(5, |x| x + 5) == Err(OutOfBounds)
		## ```
		update : List(a), U64, (a -> a) -> Try(List(a), [OutOfBounds, ..])
		update = |list, index, func| if index < List.len(list) {
			Ok(list_replace_unsafe(list, index, func(list_get_unsafe(list, index))).list)
		} else {
			Err(OutOfBounds)
		}

		## Exchanges the elements at the two given indices.
		## ```roc
		## expect [10, 20, 30, 40].swap(0, 3) == Ok([40, 20, 30, 10])
		##
		## expect [10, 20, 30].swap(0, 5) == Err(OutOfBounds)
		## ```
		swap : List(a), U64, U64 -> Try(List(a), [OutOfBounds, ..])
		swap = |list, index_1, index_2| {
			len = List.len(list)
			if index_1 < len and index_2 < len {
				Ok(list_swap_unsafe(list, index_1, index_2))
			} else {
				Err(OutOfBounds)
			}
		}

		## Returns the reversed list.
		## ```roc
		## expect [1, 2, 3].rev() == [3, 2, 1]
		## expect [].rev() == []
		## ```
		rev : List(item) -> List(item)
		rev = |list| {
			# TODO: Optimize with in-place update when list is unique
			empty_list = List.with_capacity(list.len())
			list.fold_rev(empty_list, |item, newlist| list_append_unsafe(newlist, item))
		}

		for_each! : List(item), (item => {}) => {}
		for_each! = |items, fun!| for item in items {
			fun!(item)
		}

		## Convert each element in the list to something new, by calling a conversion
		## function on each of them. Then return a new list of the converted values.
		## ```roc
		## expect [1, 2, 3].map(|num| num + 1) == [2, 3, 4]
		##
		## expect ["", "a", "bc"].map(Str.is_empty) == [Bool.True, Bool.False, Bool.False]
		## ```
		map : List(a), (a -> b) -> List(b)
		map = |list, transform| {
			match list_map_can_reuse(list, transform) {
				1 => {
					len = list.len()
					var $out = list_map_cast_unsafe(list)
					var $index = 0
					while $index < len {
						item = list_map_extract_unsafe($out, $index)
						$out = list_map_write_unsafe($out, $index, transform(item))
						$index = $index + 1
					}
					$out
				}
				_ => {
					var $new_list = List.with_capacity(list.len())
					for item in list {
						$new_list = list_append_unsafe($new_list, transform(item))
					}
					$new_list
				}
			}
		}

		## This works like [List.map], except it also passes the index
		## of the element to the conversion function.
		## ```roc
		## expect List.map_with_index([10, 20, 30], (|num, index| num + index)) == [10, 21, 32]
		## ```
		map_with_index : List(a), (a, U64 -> b) -> List(b)
		map_with_index = |list, transform| {
			var $new_list = List.with_capacity(list.len())
			var $index = 0
			for item in list {
				$new_list = list_append_unsafe($new_list, transform(item, $index))
				$index = $index + 1
			}
			$new_list
		}

		## Apply a binary function to pairs of elements from two lists, returning a list of results.
		## The result's length is the length of the shorter input list.
		## ```
		## expect [1, 2, 3].map2([10, 20, 30], |a, b| a + b) == [11, 22, 33]
		## expect [1, 2, 3, 4, 5].map2([10, 20], |a, b| a + b) == [11, 22]
		## ```
		map2 : List(a), List(b), (a, b -> c) -> List(c)
		map2 = |a_list, b_list, transform| {
			var $result = []
			var $index = 0
			while ($index < a_list.len() and $index < b_list.len()) {
				match (a_list.get($index), b_list.get($index)) {
					(Ok(a), Ok(b)) => {
						$result = $result.append(transform(a, b))
					}
					_ => {
						break
					}
				}
				$index = $index + 1
			}
			$result
		}

		## Run the given function on each element of a list, and return all the
		## elements for which the function returned `Bool.True`.
		## ```roc
		## [1.I64, 2, 3, 4].keep_if(|num| num > 2)
		## ```
		## ## Performance Details
		##
		## [List.keep_if] always returns a list that takes up exactly the same amount
		## of memory as the original, even if its length decreases. This is because it
		## can't know in advance exactly how much space it will need, and if it guesses a
		## length that's too low, it would have to re-allocate.
		##
		## (If you want to do an operation like this which reduces the memory footprint
		## of the resulting list, you can do two passes over the list with [List.fold] - one
		## to calculate the precise new size, and another to populate the new list.)
		##
		## If given a unique list, [List.keep_if] will mutate it in place to assemble the appropriate list.
		## If that happens, this function will not allocate any new memory on the heap.
		## If all elements in the list end up being kept, Roc will return the original
		## list unaltered.
		##
		keep_if : List(a), (a -> Bool) -> List(a)
		keep_if = |list, predicate|
			List.fold(
				list,
				[],
				|acc, elem|
					if predicate(elem) {
						List.concat(acc, [elem])
					} else {
						acc
					},
			)

		## Run the given function on each element of a list, and return all the
		## elements for which the function returned `Bool.False`.
		## ```roc
		## [1.I64, 2, 3, 4].drop_if(|num| num > 2)
		## ```
		## ## Performance Details
		##
		## `List.drop_if` has the same performance characteristics as [List.keep_if].
		## See its documentation for details on those characteristics!
		drop_if : List(a), (a -> Bool) -> List(a)
		drop_if = |list, predicate|
			List.fold(
				list,
				[],
				|acc, elem|
					if predicate(elem) {
						acc
					} else {
						List.concat(acc, [elem])
					},
			)

		## Run the given function on each element of a list, and return the
		## number of elements for which the function returned `Bool.True`.
		## ```roc
		## expect [1, -2, -3].count_if(I64.is_negative) == 2
		## expect [1, 2, 3].count_if(|num| num > 1) == 2
		## ```
		count_if : List(a), (a -> Bool) -> U64
		count_if = |list, predicate|
			List.fold(
				list,
				0,
				|acc, elem|
					if predicate(elem) {
						acc + 1
					} else {
						acc
					},
			)

		## Build a value using each element in the list.
		##
		## Starting with a given `state` value, this folds through each element in the
		## list from first to last, running a given `step` function on that element
		## which updates the `state`. It returns the final `state` at the end.
		## ```roc
		## [2, 4, 8].fold(0, U64.plus)
		## ```
		## This returns 14 because:
		## * `state` starts at 0
		## * Each `step` runs `state.plus(elem)`, and the return value becomes the new `state`.
		##
		## Here is a table of how `state` changes as [List.fold] folds over the elements
		## `[2, 4, 8]` using [U64.plus] as its `step` function to determine the next `state`.
		##
		## state | elem  | U64.plus(state, elem)
		## :---: | :---: | :----------------:
		## 0     |       |
		## 0     | 2     | 2
		## 2     | 4     | 6
		## 6     | 8     | 14
		##
		## The following returns -6:
		## ```roc
		## [1, 2, 3].fold(0, I64.minus)
		## ```
		## Note that in other languages, `fold` is sometimes called `reduce`,
		## `fold_left`, or `foldl`.
		fold : List(item), state, (state, item -> state) -> state
		fold = |list, init, step| {
			var $state = init

			for item in list {
				$state = step($state, item)
			}

			$state
		}

		## Like [List.fold], but at each step the function also receives the index of the current element.
		fold_with_index : List(item), state, (state, item, U64 -> state) -> state
		fold_with_index = |list, init, step| {
			var $state = init
			var $index = 0

			for item in list {
				$state = step($state, item, $index)
				$index = $index + 1
			}

			$state
		}

		## Same as [List.fold], except you can stop folding early.
		##
		## ## Performance Details
		##
		## Compared to [List.fold], this can potentially visit fewer elements (which can
		## improve performance) at the cost of making each step take longer.
		## However, the added cost to each step is extremely small, and can easily
		## be outweighed if it results in skipping even a small number of elements.
		##
		## As such, it is typically better for performance to use this over [List.fold]
		## if returning `Break` earlier than the last element is expected to be common.
		fold_until : List(item), state, (state, item -> [Continue(state), Break(state)]) -> state
		fold_until = |list, init, step| {
			var $state = init

			for item in list {
				match step($state, item) {
					Continue(new_state) => {
						$state = new_state
					}
					Break(final_state) => {
						$state = final_state
						break
					}
				}
			}

			$state
		}

		## Same as [List.fold_with_index], except you can stop folding early.
		fold_with_index_until : List(item), state, (state, item, U64 -> [Continue(state), Break(state)]) -> state
		fold_with_index_until = |list, init, step| {
			var $state = init
			var $index = 0

			for item in list {
				match step($state, item, $index) {
					Continue(new_state) => {
						$state = new_state
					}
					Break(final_state) => {
						$state = final_state
						break
					}
				}
				$index = $index + 1
			}

			$state
		}

		## Like [List.fold], but walks the list from last to first. The `step` function
		## receives the element first and the current `state` second.
		##
		## ```roc
		## expect [1, 2, 3].fold_rev(0, I64.minus) == 2
		## ```
		##
		## Here is a table of how `state` changes as [List.fold_rev] folds over the
		## elements `[1, 2, 3]` using [I64.minus] as its `step` function.
		##
		## state | elem  | I64.minus(elem, state)
		## :---: | :---: | :-------------------:
		## 0     |       |
		## 0     | 3     | 3
		## 3     | 2     | -1
		## -1    | 1     | 2
		##
		## Note that in other languages, `fold_rev` is sometimes called `reduce_right`,
		## `fold_right`, or `foldr`.
		fold_rev : List(item), state, (item, state -> state) -> state
		fold_rev = |list, init, step| {
			var $state = init
			var $index = list.len()

			while $index > 0 {
				$index = $index - 1
				item = list_get_unsafe(list, $index)
				$state = step(item, $state)
			}

			$state
		}

		## Returns `Bool.True` if the first list starts with the second list.
		##
		## If the second list is empty, this always returns `Bool.True`; every list
		## is considered to "start with" an empty list.
		##
		## If the first list is empty, this only returns `Bool.True` if the second list is empty.
		starts_with : List(a), List(a) -> Bool where [a.is_eq : a, a -> Bool]
		starts_with = |list, prefix|
			prefix == List.take_first(list, List.len(prefix))

		## Returns `Bool.True` if the first list ends with the second list.
		##
		## If the second list is empty, this always returns `Bool.True`; every list
		## is considered to "end with" an empty list.
		##
		## If the first list is empty, this only returns `Bool.True` if the second list is empty.
		ends_with : List(a), List(a) -> Bool where [a.is_eq : a, a -> Bool]
		ends_with = |list, suffix|
			suffix == List.take_last(list, List.len(suffix))

		## Run the given predicate on each element of the list, returning `Bool.True` if
		## any of the elements satisfy it.
		## ```roc
		## expect [1, 2, 3].any(|n| n % 2 == 0)
		##
		## expect ![1, 2, 3].any(|n| n < 0)
		## ```
		any : List(a), (a -> Bool) -> Bool
		any = |list, predicate| {
			for item in list {
				if predicate(item) {
					return True
				}
			}
			False
		}

		## Returns `Bool.True` if the list contains an element equal to the given value.
		## ```roc
		## expect [1, 2, 3].contains(2)
		##
		## expect ![1, 2, 3].contains(4)
		## ```
		contains : List(a), a -> Bool where [a.is_eq : a, a -> Bool]
		contains = |list, elt| {
			List.any(list, |x| x == elt)
		}

		## Run the given predicate on each element of the list, returning `Bool.True` if
		## all of the elements satisfy it.
		## ```roc
		## expect [2, 4, 6].all(|n| n % 2 == 0)
		##
		## expect ![1, 2, 3].all(|n| n % 2 == 0)
		## ```
		all : List(a), (a -> Bool) -> Bool
		all = |list, predicate| {
			for item in list {
				if Bool.not(predicate(item)) {
					return False
				}
			}
			True
		}

		## Returns the last element in the list, or `ListWasEmpty` if it was empty.
		## ```roc
		## expect [1, 2, 3].last() == Ok(3.0)
		## expect [].last() == Err(ListWasEmpty)
		## ```
		last : List(item) -> Try(item, [ListWasEmpty, ..])
		last = |list| if List.is_empty(list) {
			Try.Err(ListWasEmpty)
		} else {
			Try.Ok(list_get_unsafe(list, List.len(list) - 1))
		}

		## Create a list with a single element in it.
		##
		## ```roc
		## expect List.single(42) == [42.0]
		## ```
		single : item -> List(item)
		single = |x| [x]

		## Remove the element at the given index. If the index is out of bounds, the
		## list is returned unchanged.
		## ```roc
		## expect [10, 20, 30, 40].drop_at(1) == [10, 30, 40]
		##
		## expect [10, 20, 30].drop_at(5) == [10, 20, 30]
		## ```
		drop_at : List(a), U64 -> List(a)

		## Return a sublist of the list starting at `start` and containing up to `len`
		## elements. Out-of-bounds ranges are clamped, producing a shorter or empty list.
		## ```roc
		## expect [1, 2, 3, 4, 5].sublist({ start: 1, len: 3 }) == [2, 3, 4]
		##
		## expect [1, 2, 3].sublist({ start: 1, len: 10 }) == [2, 3]
		##
		## expect [1, 2, 3].sublist({ start: 10, len: 2 }) == []
		## ```
		sublist : List(a), { start : U64, len : U64 } -> List(a)

		## Return the first `n` elements of the list. If the list has fewer than `n`
		## elements, the entire list is returned.
		## ```roc
		## expect [1, 2, 3, 4, 5].take_first(3) == [1, 2, 3]
		##
		## expect [1, 2].take_first(10) == [1, 2]
		## ```
		take_first : List(a), U64 -> List(a)
		take_first = |list, n| {
			List.sublist(list, { len: n, start: 0 })
		}

		## Returns the given number of elements from the end of the list.
		## ```roc
		## expect [1, 2, 3, 4, 5, 6, 7, 8].take_last(4) == [5, 6, 7, 8]
		## ```
		## If there are fewer elements in the list than the requested number,
		## the entire list is returned.
		## ```roc
		## expect [1, 2].take_last(5) == [1, 2]
		## ```
		## To *remove* elements from the end of the list, use `List.take_first`.
		##
		## To remove elements from both the beginning and end of the list,
		## use `List.sublist`.
		##
		take_last : List(a), U64 -> List(a)
		take_last = |list, n| {
			len = List.len(list)
			start = if (len <= n) 0 else len - n
			List.sublist(list, { start: start, len: len })
		}

		## Drops n elements from the beginning of the list. If `n` is larger than the
		## list length, an empty list is returned.
		## ```roc
		## expect [1, 2, 3, 4, 5].drop_first(2) == [3, 4, 5]
		##
		## expect [1, 2, 3].drop_first(10) == []
		## ```
		drop_first : List(a), U64 -> List(a)
		drop_first = |list, n| {
			len = List.len(list)
			List.sublist(list, { start: n, len: len })
		}

		## Drops n elements from the end of the list. If `n` is larger than the
		## list length, an empty list is returned.
		## ```roc
		## expect [1, 2, 3, 4, 5].drop_last(2) == [1, 2, 3]
		##
		## expect [1, 2, 3].drop_last(10) == []
		## ```
		drop_last : List(a), U64 -> List(a)
		drop_last = |list, n| {
			len = List.len(list)
			take_len = if (len <= n) 0 else len - n
			List.sublist(list, { start: 0, len: take_len })
		}

		## Find the first element in a list that satisfies a given predicate, returning it wrapped in `Ok` if found, or `Err(NotFound)` if no such element exists.
		## ```
		## expect [1, 2, 3, 4].find_first(|x| x % 2 == 0) == Ok(2)
		## ```
		find_first : List(a), (a -> Bool) -> Try(a, [NotFound])
		find_first = |list, predicate| {
			for item in list if predicate(item) {
				return Ok(item)
			}
			return Err(NotFound)
		}

		## Find the last element in a list that satisfies a given predicate, returning it wrapped in `Ok` if found, or `Err(NotFound)` if no such element exists.
		## ```
		## expect [1, 2, 3, 4].find_last(|x| x % 2 == 0) == Ok(4)
		## ```
		find_last : List(a), (a -> Bool) -> Try(a, [NotFound])
		find_last = |list, predicate| {
			for item in list.rev() if predicate(item) {
				return Ok(item)
			}
			return Err(NotFound)
		}

		## Find the index of the first element in a list that satisfies a given predicate, returning it wrapped in `Ok` if found, or `Err(NotFound)` if no such element exists.
		## ```
		## expect [1, 2, 3, 4].find_first_index(|x| x > 1) == Ok(1)
		## ```
		find_first_index : List(a), (a -> Bool) -> Try(U64, [NotFound])
		find_first_index = |list, predicate| {
			var $idx = 0
			for item in list {
				if predicate(item) {
					return Ok($idx)
				}
				$idx = $idx + 1
			}
			return Err(NotFound)
		}

		## Find the index of the last element in a list that satisfies a given predicate, returning it wrapped in `Ok` if found, or `Err(NotFound)` if no such element exists.
		## ```
		## expect [1, 2, 3, 4].find_last_index(|x| x < 4) == Ok(2)
		## ```
		find_last_index : List(a), (a -> Bool) -> Try(U64, [NotFound])
		find_last_index = |list, predicate| {
			var $idx = list.len()

			while $idx > 0 {
				$idx = $idx - 1
				item = list_get_unsafe(list, $idx)
				if predicate(item) {
					return Ok($idx)
				}
			}
			return Err(NotFound)
		}

		# Split a list into two parts at a specified index, returning the part before the index and the part from the index onward.
		## ```
		## expect [0, 1, 2, 3, 4].split_at(2) == { before: [0, 1], others: [2, 3, 4] }
		## ```
		split_at : List(a), U64 -> { before : List(a), others : List(a) }
		split_at = |list, idx| {
			before = list.sublist({ start: 0, len: idx })
			len = list.len()
			others = if idx > len
				[]
			else
				list.sublist({ start: idx, len: len - idx })
			{ before, others }
		}

		## Split a list into sublists using a specified delimiter element.
		##
		## Consecutive delimiters and delimiters at the start or end of the list
		## produce empty sublists at the corresponding positions.
		## ```
		## expect [1, 2, 1, 2, 3].split_on(1) == [[], [2], [2, 3]]
		## expect [1, 1, 2].split_on(1) == [[], [], [2]]
		## ```
		split_on : List(a), a -> List(List(a)) where [a.is_eq : a, a -> Bool]
		split_on = |list, delim| list->split_if(|x| x == delim)

		## Split a list into sublists using a predicate function to identify delimiters.
		##
		## Consecutive delimiters and delimiters at the start or end of the list
		## produce empty sublists at the corresponding positions.
		## ```
		## expect [0, 1, 2, 3, 4].split_if(|x| x % 2 == 0) == [[], [1], [3], []]
		## ```
		split_if : List(a), (a -> Bool) -> List(List(a))
		split_if = |list, predicate| {
			var $acc = []
			var $current = []

			for item in list {
				if predicate(item) {
					$acc = $acc.append($current)
					$current = []
				} else {
					$current = $current.append(item)
				}
			}
			$acc.append($current)
		}

		## Split a list into sublists using a specified delimiter list.
		## ```
		## expect [1, 2, 3, 4, 5].split_on_list([2, 3]) == [[1], [4, 5]]
		## ```
		split_on_list : List(a), List(a) -> List(List(a))
			where [a.is_eq : a, a -> Bool]
		split_on_list = |list, delim_l| {
			if delim_l.is_empty() {
				return [list]
			}

			delim_len = delim_l.len()
			list_len = list.len()
			var $lists = []
			var $current = []
			var $skip = 0
			var $i = 0

			for elem in list {
				if $skip > 0 {
					$skip = $skip - 1
				} else if $i + delim_len <= list_len
					and list.sublist({ start: $i, len: delim_len }) == delim_l {
					$lists = $lists.append($current)
					$current = []
					$skip = delim_len - 1
				} else {
					$current = $current.append(elem)
				}
				$i = $i + 1
			}

			$lists.append($current)
		}

		## Split a list into two parts at the first occurrence of a specified delimiter element, returning the part before the delimiter and the part after it. If the delimiter is not found, return `Err(NotFound)`.
		## ```
		## expect [0, 1, 2, 1, 2].split_first(2) == Ok({ before: [0, 1], after: [1, 2] })
		## ```
		split_first : List(a), a -> Try({ before : List(a), after : List(a) }, [NotFound])
			where [a.is_eq : a, a -> Bool]
		split_first = |list, delim|
			match list->find_first_index(|x| x == delim) {
				Ok(index) => Ok(
					{
						before: list.sublist({ start: 0, len: index }),
						after: list.drop_first(index + 1),
					},
				)
				Err(NotFound) => Err(NotFound)
			}

		## Split a list into two parts at the last occurrence of a specified delimiter element, returning the part before the delimiter and the part after it. If the delimiter is not found, return `Err(NotFound)`.
		## ```
		## expect [0, 1, 2, 1, 2].split_last(1) == Ok({ before: [0, 1, 2], after: [2] })
		## ```
		split_last : List(a), a -> Try({ before : List(a), after : List(a) }, [NotFound])
			where [a.is_eq : a, a -> Bool]
		split_last = |list, delim|
			match list->find_last_index(|x| x == delim) {
				Ok(index) => Ok(
					{
						before: list.sublist({ start: 0, len: index }),
						after: list.drop_first(index + 1),
					},
				)
				Err(NotFound) => Err(NotFound)
			}

		## Build a list by repeating the given value `n` times.
		## ```roc
		## expect List.repeat(0, 3) == [0, 0, 0]
		##
		## expect List.repeat("hi", 0) == []
		## ```
		repeat : a, U64 -> List(a)
		repeat = |item, n| {
			var $list = List.with_capacity(n)
			var $count = 0
			while $count < n {
				$list = list_append_unsafe($list, item)
				$count = $count + 1
			}
			$list
		}

		## Sum the elements of a list. Works for any type that implements `plus` and
		## `default` methods, such as the numeric types.
		## ```roc
		## expect List.sum([1.I64, 2, 3, 4]) == 10
		##
		## expect List.sum([]) == 0.I64
		## ```
		sum : List(item) -> item
			where [item.plus : item, item -> item, item.default : item]
		sum = |list| {
			Item : item
			List.fold(list, Item.default(), |acc, elem| acc + elem)
		}

		## Find the minimum element in a list, or `Err(ListWasEmpty)` if the list is empty.
		## Works for any type that implements `min`.
		min : List(a) -> Try(a, [ListWasEmpty])
			where [a.min : a, a -> a]
		min = |list|
			match List.first(list) {
				Ok(initial) =>
					Ok(
						List.fold(
							list,
							initial,
							|best_so_far, elem| best_so_far.min(elem),
						),
					)

				Err(ListWasEmpty) =>
					Err(ListWasEmpty)
				}

		## Find the maximum element in a list, or `Err(ListWasEmpty)` if the list is empty.
		## Works for any type that implements `max`.
		max : List(a) -> Try(a, [ListWasEmpty])
			where [a.max : a, a -> a]
		max = |list|
			match List.first(list) {
				Ok(initial) =>
					Ok(
						List.fold(
							list,
							initial,
							|best_so_far, elem| best_so_far.max(elem),
						),
					)

				Err(ListWasEmpty) =>
					Err(ListWasEmpty)
				}

		## Encode a list using a format that provides encode_list
		encode : List(item), fmt -> Try(encoded, err)
			where [
				fmt.encode_list : fmt, List(item), (item, fmt -> Try(encoded, err)) -> Try(encoded, err),
				item.encode : item, fmt -> Try(encoded, err),
			]
		encode = |self, format| {
			format.encode_list(self, |elem, f| elem.encode(f))
		}

		## Decode a list using a format that provides decode_list
		decode : src, fmt -> (Try(List(item), err), src)
			where [
				fmt.decode_list : fmt, src, (src, fmt -> (Try(item, err), src)) -> (Try(List(item), err), src),
				item.decode : src, fmt -> (Try(item, err), src),
			]
		decode = |source, format| {
			Fmt : fmt
			Item : item
			Fmt.decode_list(format, source, |s, f| Item.decode(s, f))
		}

	}

	Bool := [False, True].{

		## Returns `Bool.False` when given `Bool.True`, and vice versa. This is
		## equivalent to the logic [NOT](https://en.wikipedia.org/wiki/Negation)
		## gate. The operator `!` can also be used as shorthand for `Bool.not`.
		## ```roc
		## expect Bool.not(Bool.False) == Bool.True
		## expect !Bool.False == Bool.True
		## ```
		not : Bool -> Bool
		not = |bool| match bool {
			Bool.True => Bool.False
			Bool.False => Bool.True
		}

		## Returns `Bool.True` if the two booleans are the same, and `Bool.False` if they are different.
		is_eq : Bool, Bool -> Bool
		is_eq = |a, b| match a {
			Bool.True => match b {
				Bool.True => Bool.True
				Bool.False => Bool.False
			}
			Bool.False => match b {
				Bool.True => Bool.False
				Bool.False => Bool.True
			}
		}

		## Feed a boolean into a [Hasher].
		to_hash : Bool, Hasher -> Hasher
		to_hash = |bool, hasher| Hasher.write_bool(hasher, bool)

		## Encode a bool using a format that provides encode_bool
		encode : Bool, fmt -> Try(encoded, err)
			where [fmt.encode_bool : fmt, Bool -> Try(encoded, err)]
		encode = |self, format| {
			format.encode_bool(self)
		}

		## Decode a bool using a format that provides decode_bool
		decode : src, fmt -> (Try(Bool, err), src)
			where [fmt.decode_bool : fmt, src -> (Try(Bool, err), src)]
		decode = |source, format| {
			Fmt : fmt
			Fmt.decode_bool(format, source)
		}
	}

	Box(item) :: [ProvidedByCompiler].{

		## Wraps a value in a generic, opaque representation (box) that can easily be passed to the platform.
		## Boxing is an expensive process because it copies the value from the stack to the heap.
		## This may provide a performance optimization for advanced use cases with large values.
		box : item -> Box(item)

		## Unwraps a value from a box. This is the inverse of `Box.box`, and is also an expensive operation.
		unbox : Box(item) -> item
	}

	Try(ok, err) := [Ok(ok), Err(err)].{

		## Returns `Bool.True` if the result indicates a success, else returns `Bool.False`.
		## ```roc
		## expect Try.Ok(5).is_ok()
		## ```
		is_ok : Try(_ok, _err) -> Bool
		is_ok = |try| match try {
			Ok(_) => True
			Err(_) => False
		}

		## Returns `Bool.True` if the result indicates a failure, else returns `Bool.False`.
		## ```roc
		## expect Try.Err("uh oh").is_err()
		## ```
		is_err : Try(_ok, _err) -> Bool
		is_err = |try| match try {
			Ok(_) => False
			Err(_) => True
		}

		## Forwards interpolated string literal assembly through an inner type
		## whose `from_interpolation` method returns the same [Try].
		from_interpolation : Str, Iter((interpolated, Str)) -> Try(ok, err)
			where [
				ok.from_interpolation : Str, Iter((interpolated, Str)) -> Try(ok, err),
			]
		from_interpolation = |first, rest| {
			OkType : ok

			OkType.from_interpolation(first, rest)
		}

		## If the result is `Ok`, returns the value it holds. Otherwise, returns
		## the given default value.
		##
		## Note: This function should be used sparingly, because it hides that an error
		## happened, which will make debugging harder. Prefer using `?` to forward errors or
		## handle them explicitly with `match`.
		## ```roc
		## expect Try.Err("uh oh").ok_or(42) == 42
		##
		## expect Try.Ok(7).ok_or(42) == 7
		## ```
		ok_or : Try(ok, _err), ok -> ok
		ok_or = |try, fallback| match try {
			Ok(val) => val
			Err(_) => fallback
		}

		## If the result is `Err`, returns the value it holds. Otherwise, returns
		## the given default value.
		## ```roc
		## expect Try.Err("uh oh").err_or("fallback") == "uh oh"
		##
		## expect Try.Ok(7).err_or("fallback") == "fallback"
		## ```
		err_or : Try(_ok, err), err -> err
		err_or = |try, fallback| match try {
			Err(val) => val
			Ok(_) => fallback
		}

		## If the result is `Ok`, transforms the value it holds by running a conversion
		## function on it. Then returns a new `Ok` holding the transformed value. If the
		## result is `Err`, this has no effect. Use [Try.map_err] to transform an `Err`.
		## ```roc
		## expect Try.Ok(12.I64).map_ok(|n| -n) == Ok(-12)
		##
		## expect {
		## 	err : Try(I64, Str)
		## 	err = Err("yipes!")
		## 	err.map_ok(|n| -n) == Err("yipes!")
		## }
		## ```
		## Functions like `map` are common in Roc; see for example [List.map] and [Set.map].
		map_ok : Try(a, err), (a -> b) -> Try(b, err)
		map_ok = |try, transform| match try {
			Err(err) => Err(err)
			Ok(a) => Ok(transform(a))
		}

		## Like [Try.map_ok], but the transform function is effectful. If the argument is
		## an `Ok`, the effect is run and its return value is wrapped in a new `Ok`. If
		## the result is `Err`, the effect is not run and the `Err` is returned unchanged.
		## ```roc
		## artist_try.map_ok!(|a| SQL.query!("SELECT * FROM albums WHERE artist_id = ?", [a.id]))
		## ```
		map_ok! : Try(a, err), (a => b) => Try(b, err)
		map_ok! = |try, transform!| match try {
			Err(err) => Err(err)
			Ok(a) => Ok(transform!(a))
		}

		## If the result is `Err`, transforms the value it holds by running a conversion
		## function on it. Then returns a new `Err` holding the transformed value. If
		## the result is `Ok`, this has no effect. Use [Try.map_ok] to transform an `Ok`.
		## ```roc
		## expect [].last().map_err(|_| ProvidedListIsEmpty) == Err(ProvidedListIsEmpty)
		##
		## expect [4].last().map_err(|_| ProvidedListIsEmpty) == Ok(4.0)
		## ```
		map_err : Try(ok, a), (a -> b) -> Try(ok, b)
		map_err = |try, transform| match try {
			Err(a) => Err(transform(a))
			Ok(ok) => Ok(ok)
		}

		## Like [Try.map_err], but the transform function is effectful. If the argument is
		## an `Err`, the effect is run and its return value is wrapped in a new `Err`. If
		## the result is `Ok`, the effect is not run and the `Ok` is returned unchanged.
		## ```roc
		## # Log the failure to the database only when the request errored.
		## request.map_err!(|e| SQL.execute!("INSERT INTO errors (message) VALUES (?)", [e.message]))
		## ```
		map_err! : Try(ok, a), (a => b) => Try(ok, b)
		map_err! = |try, transform!| match try {
			Err(a) => Err(transform!(a))
			Ok(ok) => Ok(ok)
		}

		## Returns `Bool.True` if the two `Try` values are the same variant (`Ok` or `Err`) and their contents are pairwise equal. Otherwise, returns `Bool.False`.
		is_eq : Try(ok, err), Try(ok, err) -> Bool
			where [
				ok.is_eq : ok, ok -> Bool,
				err.is_eq : err, err -> Bool,
			]
		is_eq = |a, b| match a {
			Ok(a_val) => {
				match b {
					Ok(b_val) => a_val.is_eq(b_val)
					Err(_) => False
				}
			}
			Err(a_val) => {
				match b {
					Ok(_) => False
					Err(b_val) => a_val.is_eq(b_val)
				}
			}
		}

		## Feed a [Try] into a [Hasher].
		to_hash : Try(ok, err), Hasher -> Hasher
			where [
				ok.to_hash : ok, Hasher -> Hasher,
				err.to_hash : err, Hasher -> Hasher,
			]
		to_hash = |try, hasher| match try {
			Ok(ok) => ok.to_hash(Hasher.write_u64(hasher, 0))
			Err(err) => err.to_hash(Hasher.write_u64(hasher, 1))
		}
	}

	Dict(k, v) :: [
		HashMap({ entries : List((k, v)), buckets : List({ dist_and_fingerprint : U32, entry_index : U32 }), max_entries_before_grow : U64, shifts : U8 }),
	].{
		DictBucket : { dist_and_fingerprint : U32, entry_index : U32 }

		DictData(k, v) : {
			entries : List((k, v)),
			buckets : List(DictBucket),
			max_entries_before_grow : U64,
			shifts : U8,
		}

		DictMetadata : {
			buckets : List(DictBucket),
			max_entries_before_grow : U64,
			shifts : U8,
		}

		## Returns `Bool.True` if the two dictionaries contain the same key-value
		## pairs, and `Bool.False` otherwise.
		is_eq : Dict(k, v), Dict(k, v) -> Bool
			where [
				k.is_eq : k, k -> Bool,
				k.to_hash : k, Hasher -> Hasher,
				v.is_eq : v, v -> Bool,
			]
		is_eq = |dict_a, dict_b| {
			if Dict.len(dict_a) != Dict.len(dict_b) {
				return False
			}

			list_a = Dict.to_list(dict_a)
			var $index = 0

			while $index < List.len(list_a) {
				(key, value) = list_get_unsafe(list_a, $index)
				match Dict.get(dict_b, key) {
					Try.Ok(value_b) => {
						if value != value_b {
							return False
						}
					}
					Try.Err(_) => {
						return False
					}
				}
				$index = $index + 1
			}

			True
		}

		## Feed a [Dict] into a [Hasher]. The hash is independent of insertion order.
		to_hash : Dict(k, v), Hasher -> Hasher
			where [
				k.to_hash : k, Hasher -> Hasher,
				v.to_hash : v, Hasher -> Hasher,
			]
		to_hash = |dict, hasher| match dict {
			HashMap(data) => {
				Key : k
				Value : v
				var $entry_hashes = 0

				for (key, value) in data.entries {
					entry_hash = hasher_finish(Value.to_hash(value, Key.to_hash(key, hasher_start(dict_seed()))))
					$entry_hashes = dict_combine_entry_hashes($entry_hashes, entry_hash)
				}

				Hasher.write_u64(Hasher.write_u64(hasher, List.len(data.entries)), $entry_hashes)
			}
		}

		## Returns an empty `Dict`.
		## ```roc
		## empty_dict = Dict.empty()
		## ```
		empty : () -> Dict(_k, _v)
		empty = || HashMap({ entries: [], buckets: [], max_entries_before_grow: 0, shifts: dict_initial_shifts })

		## Returns an empty `Dict` with room for at least the requested number of entries.
		with_capacity : U64 -> Dict(_k, _v)
		with_capacity = |requested| {
			metadata = dict_allocate_buckets_for_capacity(requested)
			HashMap(
				{
					entries: List.with_capacity(requested),
					buckets: metadata.buckets,
					max_entries_before_grow: metadata.max_entries_before_grow,
					shifts: metadata.shifts,
				},
			)
		}

		## Returns the number of entries the dictionary can hold before growing.
		capacity : Dict(_k, _v) -> U64
		capacity = |dict| match dict {
			HashMap(data) => data.max_entries_before_grow
		}

		## Ensure this dictionary has room for at least this many additional entries.
		reserve : Dict(k, v), U64 -> Dict(k, v)
			where [k.to_hash : k, Hasher -> Hasher]
		reserve = |dict, additional| match dict {
			HashMap(data) => {
				len = List.len(data.entries)
				desired = dict_add_capacity(len, additional)
				prepared = dict_ensure_capacity(data, desired)
				entries = List.reserve(prepared.entries, additional)
				HashMap({ entries, buckets: prepared.buckets, max_entries_before_grow: prepared.max_entries_before_grow, shifts: prepared.shifts })
			}
		}

		## Reduce unused dictionary capacity.
		release_excess_capacity : Dict(k, v) -> Dict(k, v)
			where [k.to_hash : k, Hasher -> Hasher]
		release_excess_capacity = |dict| match dict {
			HashMap(data) => {
				entries = List.release_excess_capacity(data.entries)
				metadata = dict_allocate_buckets_for_capacity(List.len(entries))
				buckets = dict_fill_buckets_from_entries(metadata.buckets, entries, metadata.shifts)
				HashMap({ entries, buckets, max_entries_before_grow: metadata.max_entries_before_grow, shifts: metadata.shifts })
			}
		}

		## Remove every entry while preserving the current capacity.
		clear : Dict(k, v) -> Dict(k, v)
		clear = |dict| match dict {
			HashMap(data) => {
				HashMap(
					{
						entries: List.take_first(data.entries, 0),
						buckets: List.map(data.buckets, |_| dict_empty_bucket),
						max_entries_before_grow: data.max_entries_before_grow,
						shifts: data.shifts,
					},
				)
			}
		}

		## Returns a `Dict` containing the key and value provided as input.
		## ```roc
		## expect Dict.single("A", "B") == Dict.empty().insert("A", "B")
		## ```
		single : k, v -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		single = |key, value| Dict.insert(Dict.empty(), key, value)

		## Returns the number of key-value pairs in the dictionary.
		## ```roc
		## expect Dict.empty()
		##            .insert("One", "A Song")
		##            .insert("Two", "Candy Canes")
		##            .insert("Three", "Boughs of Holly")
		##            .len() == 3
		## ```
		len : Dict(_k, _v) -> U64
		len = |dict| match dict {
			HashMap(data) => List.len(data.entries)
		}

		## Check if the dictionary is empty.
		## ```roc
		## expect !Dict.empty().insert("key", 42).is_empty()
		##
		## expect Dict.empty().is_empty()
		## ```
		is_empty : Dict(_k, _v) -> Bool
		is_empty = |dict| Dict.len(dict) == 0

		## Get the value for a given key. If there is a value for the specified
		## key it will return `Ok(value)`, otherwise return `Err(KeyNotFound)`.
		## ```roc
		## dictionary = Dict.empty()
		##                  .insert(1, "Apple")
		##                  .insert(2, "Orange")
		##
		## expect dictionary.get(1) == Ok("Apple")
		## expect dictionary.get(2000) == Err(KeyNotFound)
		## ```
		get : Dict(k, v), k -> Try(v, [KeyNotFound, ..])
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		get = |dict, key| match dict {
			HashMap(data) => match dict_find(data, key) {
				Found(found) => Try.Ok(found.value)
				Missing(_) => Try.Err(KeyNotFound)
			}
		}

		## Check if the dictionary has a value for a specified key.
		## ```roc
		## expect Dict.empty().insert(1234, "5678").contains(1234)
		## ```
		contains : Dict(k, _v), k -> Bool
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		contains = |dict, key| match dict {
			HashMap(data) => match dict_find(data, key) {
				Found(_) => True
				Missing(_) => False
			}
		}

		## Insert a value into the dictionary at a specified key. If the key
		## already exists, the existing value is replaced.
		## ```roc
		## expect Dict.empty()
		##            .insert("Apples", 12)
		##            .get("Apples") == Ok(12)
		## ```
		insert : Dict(k, v), k, v -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		insert = |dict, key, value| match dict {
			HashMap(data) => {
				match dict_find(data, key) {
					Found(found) => {
						entries = list_set_unsafe(data.entries, found.entry_index, (key, value))
						HashMap({ entries, buckets: data.buckets, max_entries_before_grow: data.max_entries_before_grow, shifts: data.shifts })
					}
					Missing(missing) => {
						if List.len(data.entries) < data.max_entries_before_grow {
							HashMap(dict_insert_new_data(data, missing.bucket_index, missing.dist_and_fingerprint, key, value))
						} else {
							prepared = dict_ensure_capacity(data, dict_add_capacity(List.len(data.entries), 1))
							match dict_find(prepared, key) {
								Found(_) => {
									crash "Dict invariant violated: found duplicate after growth"
								}
								Missing(grown_missing) => HashMap(dict_insert_new_data(prepared, grown_missing.bucket_index, grown_missing.dist_and_fingerprint, key, value))
							}
						}
					}
				}
			}
		}

		## Remove a value from the dictionary for a specified key.
		## ```roc
		## expect Dict.empty()
		##            .insert("Some", "Value")
		##            .remove("Some")
		##            .len() == 0
		## ```
		remove : Dict(k, v), k -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		remove = |dict, key| match dict {
			HashMap(data) => match dict_find(data, key) {
				Found(found) => HashMap(dict_remove_bucket_data(data, found.bucket_index))
				Missing(_) => dict
			}
		}

		## Returns the key-value pairs of a dictionary as a `List`.
		## ```roc
		## expect Dict.single(1, "One")
		##            .insert(2, "Two")
		##            .to_list() == [(1, "One"), (2, "Two")]
		## ```
		to_list : Dict(k, v) -> List((k, v))
		to_list = |dict| match dict {
			HashMap(data) => data.entries
		}

		## Create a `Dict` from a `List` of key-value pairs. If the list
		## contains duplicate keys, later values overwrite earlier ones.
		## ```roc
		## expect Dict.from_list([(1, "One"), (2, "Two")]) ==
		## 	Dict.single(1, "One").insert(2, "Two")
		## ```
		from_list : List((k, v)) -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		from_list = |list|
			List.fold(list, Dict.with_capacity(List.len(list)), |dict, (k, v)| Dict.insert(dict, k, v))

		## Returns the keys of a dictionary as a `List`.
		## ```roc
		## expect Dict.single(1, "One")
		##            .insert(2, "Two")
		##            .keys() == [1, 2]
		## ```
		keys : Dict(k, _v) -> List(k)
		keys = |dict| match dict {
			HashMap(data) => {
				var $keys = List.with_capacity(List.len(data.entries))
				for (key, _) in data.entries {
					$keys = List.append($keys, key)
				}
				$keys
			}
		}

		## Returns the values of a dictionary as a `List`.
		## ```roc
		## expect Dict.single(1, "One")
		##            .insert(2, "Two")
		##            .values() == ["One", "Two"]
		## ```
		values : Dict(_k, v) -> List(v)
		values = |dict| match dict {
			HashMap(data) => {
				var $values = List.with_capacity(List.len(data.entries))
				for (_, value) in data.entries {
					$values = List.append($values, value)
				}
				$values
			}
		}

		## Build a value by folding through each key-value pair in the
		## dictionary. Starting with a given `state` value, this runs the
		## given `step` function on each pair, using its return value as
		## the new `state`. It returns the final `state` at the end.
		## ```roc
		## expect Dict.empty()
		##            .insert("Apples", 12.U64)
		##            .insert("Oranges", 24)
		##            .fold(0, |count, _key, qty| count + qty) == 36
		## ```
		fold : Dict(k, v), state, (state, k, v -> state) -> state
		fold = |dict, init, step| match dict {
			HashMap(data) => {
				var $state = init
				for (key, value) in data.entries {
					$state = step($state, key, value)
				}
				$state
			}
		}

		## Run the given function on each key-value pair of a dictionary, and
		## return a dictionary with just the pairs for which the function
		## returned `Bool.True`.
		## ```roc
		## expect Dict.empty()
		##            .insert("Alice", 17.U64)
		##            .insert("Bob", 18)
		##            .insert("Charlie", 19)
		##            .keep_if(|(_k, v)| v >= 18)
		##            .len() == 2
		## ```
		keep_if : Dict(k, v), ((k, v) -> Bool) -> Dict(k, v)
			where [k.to_hash : k, Hasher -> Hasher]
		keep_if = |dict, predicate| match dict {
			HashMap(data) => {
				var $entries = List.with_capacity(List.len(data.entries))
				for (key, value) in data.entries {
					if predicate((key, value)) {
						$entries = List.append($entries, (key, value))
					}
				}
				empty_buckets = List.map(data.buckets, |_| dict_empty_bucket)
				buckets = dict_fill_buckets_from_entries(empty_buckets, $entries, data.shifts)
				HashMap({ entries: $entries, buckets, max_entries_before_grow: data.max_entries_before_grow, shifts: data.shifts })
			}
		}

		## Run the given function on each key-value pair of a dictionary, and
		## return a dictionary with just the pairs for which the function
		## returned `Bool.False`.
		## ```roc
		## expect Dict.empty()
		##            .insert("Alice", 17.U64)
		##            .insert("Bob", 18)
		##            .insert("Charlie", 19)
		##            .drop_if(|(_k, v)| v >= 18)
		##            .len() == 1
		## ```
		drop_if : Dict(k, v), ((k, v) -> Bool) -> Dict(k, v)
			where [k.to_hash : k, Hasher -> Hasher]
		drop_if = |dict, predicate| Dict.keep_if(dict, |pair| !predicate(pair))

		## Convert each value in the dictionary to something new, by calling a
		## conversion function on each of them which receives both the key
		## and the old value. Then return a new dictionary containing the same
		## keys and the converted values.
		## ```roc
		## expect Dict.empty()
		##            .insert("a", 1.I64)
		##            .insert("b", 2)
		##            .map(|_k, v| v * 10)
		##            == Dict.empty()
		##                   .insert("a", 10)
		##                   .insert("b", 20)
		## ```
		map : Dict(k, a), (k, a -> b) -> Dict(k, b)
		map = |dict, transform| match dict {
			HashMap(data) => {
				var $entries = List.with_capacity(List.len(data.entries))
				for (key, value) in data.entries {
					$entries = List.append($entries, (key, transform(key, value)))
				}
				HashMap({ entries: $entries, buckets: data.buckets, max_entries_before_grow: data.max_entries_before_grow, shifts: data.shifts })
			}
		}

		## Like [Dict.map], except the transformation function returns a
		## dictionary. At the end, all the dictionaries are joined together
		## (using [Dict.insert_all]) into one dictionary.
		##
		## You may know a similar function named `concat_map` in other languages.
		join_map : Dict(a, b), (a, b -> Dict(x, y)) -> Dict(x, y)
			where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
		join_map = |dict, transform| {
			var $acc = Dict.empty()
			for (k, v) in Dict.to_list(dict) {
				$acc = Dict.insert_all($acc, transform(k, v))
			}
			$acc
		}

		## Combine two dictionaries by keeping the
		## [union](https://en.wikipedia.org/wiki/Union_(set_theory)) of all the
		## key-value pairs. Where the same key is present in both dictionaries,
		## the value from the second input is kept.
		## ```roc
		## expect {
		## 	first = Dict.single(1, "Not Me").insert(2, "And Me")
		## 	second = Dict.single(1, "Keep Me").insert(3, "Me Too")
		## 	expected = Dict.single(1, "Keep Me").insert(2, "And Me").insert(3, "Me Too")
		## 	Dict.is_eq(Dict.insert_all(first, second), expected)
		## }
		## ```
		insert_all : Dict(k, v), Dict(k, v) -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		insert_all = |xs, ys| {
			var $acc = Dict.reserve(xs, Dict.len(ys))
			for (k, v) in Dict.to_list(ys) {
				$acc = Dict.insert($acc, k, v)
			}
			$acc
		}

		## Combine two dictionaries by keeping the
		## [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
		## of all the key-value pairs. Only pairs where the key exists in both
		## dictionaries and the values are equal are kept.
		## ```roc
		## expect {
		## 	first = Dict.single(1, "Keep Me").insert(2, "And Me").insert(3, "Not this one")
		## 	second = Dict.single(1, "Keep Me").insert(2, "And Me").insert(3, "Different")
		## 	expected = Dict.single(1, "Keep Me").insert(2, "And Me")
		##
		## 	Dict.is_eq(Dict.keep_shared(first, second), expected)
		## }
		## ```
		keep_shared : Dict(k, v), Dict(k, v) -> Dict(k, v)
			where [
				k.is_eq : k, k -> Bool,
				k.to_hash : k, Hasher -> Hasher,
				v.is_eq : v, v -> Bool,
			]
		keep_shared = |xs, ys| {
			var $acc = Dict.with_capacity(Dict.len(xs))
			for (k, v) in Dict.to_list(xs) {
				match Dict.get(ys, k) {
					Try.Ok(yv) => if yv == v {
						$acc = Dict.insert($acc, k, v)
					}
					Try.Err(_) => {}
				}
			}
			$acc
		}

		## Remove the key-value pairs in the first input whose keys are also in
		## the second using the
		## [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement).
		## ```roc
		## expect {
		## 	first = Dict.single(1, "Keep Me").insert(2, "And Me").insert(3, "Remove Me")
		## 	second = Dict.single(3, "Remove Me").insert(4, "I do nothing...")
		## 	expected = Dict.single(1, "Keep Me").insert(2, "And Me")
		##
		## 	Dict.is_eq(Dict.remove_all(first, second), expected)
		## }
		## ```
		remove_all : Dict(k, v), Dict(k, _w) -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		remove_all = |xs, ys| {
			var $acc = xs
			for (k, _) in Dict.to_list(ys) {
				$acc = Dict.remove($acc, k)
			}
			$acc
		}

		## Insert, update or remove a value for a specified key. This is more efficient
		## than doing a `Dict.get` and then a `Dict.insert` call.
		##
		## The provided function receives:
		## - `Ok(value)` if the key is currently in the dictionary.
		## - `Err(Missing)` if the key is not currently in the dictionary.
		##
		## It should return:
		## - `Ok(new_value)` to insert or update the value at the key.
		## - `Err(Missing)` to remove the key (or leave it absent).
		## ```roc
		## alter = |possible_value| match possible_value {
		## 	Err(Missing) => Ok(Bool.False)
		## 	Ok(value) => if value Err(Missing) else Ok(Bool.True)
		## }
		##
		## expect Dict.is_eq(
		## 	Dict.update(Dict.empty(), "a", alter),
		## 	Dict.single("a", Bool.False),
		## )
		## ```
		update : Dict(k, v), k, (Try(v, [Missing]) -> Try(v, [Missing])) -> Dict(k, v)
			where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
		update = |dict, key, alter| match dict {
			HashMap(data) => match dict_find(data, key) {
				Found(found) =>
					match alter(Try.Ok(found.value)) {
						Try.Ok(new_value) => {
							entries = list_set_unsafe(data.entries, found.entry_index, (key, new_value))
							HashMap({ entries, buckets: data.buckets, max_entries_before_grow: data.max_entries_before_grow, shifts: data.shifts })
						}
						Try.Err(Missing) => HashMap(dict_remove_bucket_data(data, found.bucket_index))
					}
				Missing(missing) =>
					match alter(Try.Err(Missing)) {
						Try.Ok(new_value) =>
							if List.len(data.entries) < data.max_entries_before_grow {
								HashMap(dict_insert_new_data(data, missing.bucket_index, missing.dist_and_fingerprint, key, new_value))
							} else {
								Dict.insert(dict, key, new_value)
							}
						Try.Err(Missing) => dict
					}
				}
		}
	}

	Set(item) :: [Items(List(item))].{

		## Returns `Bool.True` if the two sets contain the same values, and `Bool.False` otherwise.
		is_eq : Set(a), Set(a) -> Bool
			where [a.is_eq : a, a -> Bool]
		is_eq = |set_a, set_b| {
			list_a = Set.to_list(set_a)
			list_b = Set.to_list(set_b)

			if List.len(list_a) != List.len(list_b) {
				False
			} else {
				state = List.fold(
					list_a,
					{ all_found: Bool.True, check: list_b },
					|st, elem|
						if List.contains(st.check, elem) {
							st
						} else {
							{ all_found: Bool.False, check: st.check }
						},
				)
				state.all_found
			}
		}

		## Creates a new empty `Set`.
		empty : () -> Set(_item)
		empty = || Items([])

		## Creates a new `Set` with a single value.
		## ```roc
		## Set.single(42.I64)
		## ```
		single : item -> Set(item)
		single = |elem| Items([elem])

		## Counts the number of values in a given `Set`.
		## ```roc
		## expect Set.single(42).len() == 1
		## ```
		len : Set(_item) -> U64
		len = |set| match set {
			Items(list) => List.len(list)
		}

		## Check if the set is empty.
		is_empty : Set(_item) -> Bool
		is_empty = |set| match set {
			Items(list) => List.is_empty(list)
		}

		## Test if a value is in the `Set`.
		contains : Set(a), a -> Bool
			where [a.is_eq : a, a -> Bool]
		contains = |set, elem| match set {
			Items(list) => List.contains(list, elem)
		}

		## Insert a value into a `Set`.
		insert : Set(a), a -> Set(a)
			where [a.is_eq : a, a -> Bool]
		insert = |set, elem| match set {
			Items(list) => Items(
				List.append(
					List.keep_if(list, |x| x != elem),
					elem,
				),
			)
		}

		## Removes the value from the given `Set`.
		remove : Set(a), a -> Set(a)
			where [a.is_eq : a, a -> Bool]
		remove = |set, elem| match set {
			Items(list) => Items(List.keep_if(list, |x| x != elem))
		}

		## Retrieve the values in a `Set` as a `List`.
		to_list : Set(a) -> List(a)
		to_list = |set| match set {
			Items(list) => list
		}

		## Create a `Set` from a `List` of values.
		from_list : List(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		from_list = |list| {
			Items(
				List.fold(
					list,
					[],
					|acc, elem|
						if List.contains(acc, elem) {
							acc
						} else {
							List.append(acc, elem)
						},
				),
			)
		}

		## Run the given function on each element in the `Set`, and return
		## a `Set` with just the elements for which the function returned `Bool.True`.
		## ```roc
		## expect Set.from_list([1, 2, 3, 4]).keep_if(|num| num > 2) == Set.from_list([3, 4])
		## ```
		keep_if : Set(a), (a -> Bool) -> Set(a)
		keep_if = |set, predicate| match set {
			Items(list) => Items(List.keep_if(list, predicate))
		}

		## Run the given function on each element in the `Set`, and return
		## a `Set` with just the elements for which the function returned `Bool.False`.
		## ```roc
		## expect Set.from_list([1, 2, 3, 4]).drop_if(|num| num > 2) == Set.from_list([1, 2])
		## ```
		drop_if : Set(a), (a -> Bool) -> Set(a)
		drop_if = |set, predicate| match set {
			Items(list) => Items(List.drop_if(list, predicate))
		}

		## Combine two `Set`s by keeping the
		## [union](https://en.wikipedia.org/wiki/Union_(set_theory))
		## of all the values.
		## ```roc
		## expect {
		## 	  a = Set.from_list([1, 2, 3])
		## 	  b = Set.from_list([3, 4, 5])
		## 	  Set.union(a, b) == Set.from_list([1, 2, 3, 4, 5])
		## }
		## ```
		union : Set(a), Set(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		union = |set_a, set_b|
			List.fold(Set.to_list(set_b), set_a, |acc, elem| Set.insert(acc, elem))

		## Combine two `Set`s by keeping the
		## [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
		## of all the values.
		## ```roc
		## expect {
		## 	  a = Set.from_list([1, 2, 3])
		## 	  b = Set.from_list([2, 3, 4])
		## 	  Set.intersection(a, b) == Set.from_list([2, 3])
		## }
		## ```
		intersection : Set(a), Set(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		intersection = |set_a, set_b| {
			list_a = Set.to_list(set_a)
			list_b = Set.to_list(set_b)

			state = List.fold(
				list_a,
				{ result: [], check: list_b },
				|st, elem|
					if List.contains(st.check, elem) {
						{ result: List.append(st.result, elem), check: st.check }
					} else {
						st
					},
			)
			Items(state.result)
		}

		## Remove the values in the first `Set` that are also in the second `Set`
		## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement).
		## ```roc
		## expect {
		## 	a = Set.from_list([1, 2, 3])
		## 	b = Set.from_list([2, 3, 4])
		## 	Set.difference(a, b) == Set.from_list([1])
		## }
		## ```
		difference : Set(a), Set(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		difference = |set_a, set_b| {
			list_a = Set.to_list(set_a)
			list_b = Set.to_list(set_b)

			state = List.fold(
				list_a,
				{ result: [], check: list_b },
				|st, elem|
					if List.contains(st.check, elem) {
						st
					} else {
						{ result: List.append(st.result, elem), check: st.check }
					},
			)
			Items(state.result)
		}

		## Convert each value in the set to something new, by calling a conversion
		## function on each of them. Then return a new set containing the unique
		## converted values.
		## ```roc
		## expect Set.from_list([1, 2, 3]).map(|n| n * 2) == Set.from_list([2, 4, 6])
		##
		## # Duplicates in the mapped output are collapsed — the result is a Set.
		## expect Set.from_list([1, -1, 2, -2]).map(|n| n * n) == Set.from_list([1, 4])
		## ```
		map : Set(a), (a -> b) -> Set(b)
			where [b.is_eq : b, b -> Bool]
		map = |set, transform| match set {
			Items(list) =>
				Items(
					List.fold(
						list,
						[],
						|acc, elem| {
							new_elem = transform(elem)
							if List.contains(acc, new_elem) {
								acc
							} else {
								List.append(acc, new_elem)
							}
						},
					),
				)
			}
	}

	Num :: {}.{
		Numeral :: [
			Literal(
				{ # TODO get rid of this wrapper once we have nominal records"
					# True iff there was a minus sign in front of the literal
					is_negative : Bool,
					# Base-256 digits before and after the decimal point, with any underscores
					# removed from the source code.
					#
					# Example: If I write "0356.5170" in the source file, that will be:
					# - [1, 100] before the pt, because in base-256, 356 = (1 * 256^1) + (100 * 256^0)
					# - [20, 50] after the pt, because in base-256, 5170 = (20 * 256^1) + (50 * 256^0)
					#
					# This design compactly represents the digits without wasting any memory
					# (because base-256 stores each digit using every single bit of the U8), and also
					# allows arbitrary digit length so that userspace custom number types can work with
					# arbitrarily long number literals as long as the number types can support them.
					digits_before_pt : List(U8),
					digits_after_pt : List(U8),
					digits_after_pt_count : U64,
				},
			),
		].{
			is_negative : Numeral -> Bool
			is_negative = |self| match self {
				# TODO make this a nominal record once we have those
				Literal({ is_negative: neg, digits_before_pt: _, digits_after_pt: _, digits_after_pt_count: _ }) => neg
			}

			digits_before_pt : Numeral -> List(U8)
			digits_before_pt = |self| match self {
				Literal({ is_negative: _, digits_before_pt: digits, digits_after_pt: _, digits_after_pt_count: _ }) => digits
			}

			digits_after_pt : Numeral -> List(U8)
			digits_after_pt = |self| match self {
				Literal({ is_negative: _, digits_before_pt: _, digits_after_pt: digits, digits_after_pt_count: _ }) => digits
			}

			digits_after_pt_count : Numeral -> U64
			digits_after_pt_count = |self| match self {
				Literal({ is_negative: _, digits_before_pt: _, digits_after_pt: _, digits_after_pt_count: count }) => count
			}
		}

		U8 :: [].{

			## Returns the default [U8] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U8.default() == 0
			## ```
			default : () -> U8
			default = || 0

			## The highest value representable by a [U8], which is `255`.
			## ```roc
			## expect U8.highest == 255
			## ```
			highest : U8
			highest = 255

			## The lowest value representable by a [U8], which is `0`.
			## ```roc
			## expect U8.lowest == 0
			## ```
			lowest : U8
			lowest = 0

			## Convert a [U8] to its decimal string representation.
			## ```roc
			## expect U8.to_str(42) == "42"
			## ```
			to_str : U8 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U8.is_zero(0)
			##
			## expect !U8.is_zero(7)
			## ```
			is_zero : U8 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U8.is_eq(3, 3)
			##
			## expect !U8.is_eq(3, 4)
			## ```
			is_eq : U8, U8 -> Bool

			## Feed a [U8] into a [Hasher].
			to_hash : U8, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_u8(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U8.is_gt(5, 3)
			##
			## expect !U8.is_gt(3, 3)
			## ```

			is_gt : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U8.is_gte(3, 3)
			##
			## expect !U8.is_gte(2, 3)
			## ```
			is_gte : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U8.is_lt(3, 5)
			##
			## expect !U8.is_lt(3, 3)
			## ```
			is_lt : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U8.is_lte(3, 3)
			##
			## expect !U8.is_lte(5, 3)
			## ```
			is_lte : U8, U8 -> Bool

			## Compare two [U8] values and return their ordering.
			## ```roc
			## expect U8.compare(1, 2) == LT
			##
			## expect U8.compare(2, 2) == EQ
			##
			## expect U8.compare(3, 2) == GT
			## ```
			compare : U8, U8 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect U8.is_even(4)
			##
			## expect !U8.is_even(5)
			## ```
			is_even : U8 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect U8.is_odd(5)
			##
			## expect !U8.is_odd(4)
			## ```
			is_odd : U8 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect U8.is_multiple_of(12, 3)
			##
			## expect U8.is_multiple_of(0, 0)
			##
			## expect !U8.is_multiple_of(5, 0)
			## ```
			is_multiple_of : U8, U8 -> Bool
			is_multiple_of = |value, divisor| unsigned_is_multiple_of(0, value, divisor)

			## Returns the greater of two [U8] values.
			## ```roc
			## expect U8.max(5, 3) == 5
			## ```
			max : U8, U8 -> U8
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U8] values.
			## ```roc
			## expect U8.min(5, 3) == 3
			## ```
			min : U8, U8 -> U8
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U8] values.
			## ```roc
			## expect U8.plus(2, 3) == 5
			## ```
			plus : U8, U8 -> U8

			## Add two [U8] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in a [U8].
			add_try : U8, U8 -> Try(U8, [Overflow, ..])
			add_try = |a, b| unsigned_add_try(U8.highest, a, b)

			steps_between : U8, U8 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end).to_u64())
				else
					Known(0)

			## Add two [U8] values, saturating at [U8.highest] on overflow rather than wrapping around.
			## ```roc
			## expect U8.plus_saturated(U8.highest, 1) == U8.highest
			##
			## expect U8.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : U8, U8 -> U8
			plus_saturated = |a, b|
				if b > highest - a
					highest
				else
					a + b

			## Subtract the second [U8] from the first.
			## ```roc
			## expect U8.minus(5, 3) == 2
			## ```
			minus : U8, U8 -> U8

			## Subtract the second [U8] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in a [U8].
			sub_try : U8, U8 -> Try(U8, [Overflow, ..])
			sub_try = |a, b| unsigned_sub_try(a, b)

			## Subtract the second [U8] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect U8.minus_saturated(0, 1) == 0
			##
			## expect U8.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : U8, U8 -> U8
			minus_saturated = |a, b| unsigned_minus_saturated(0, a, b)

			## Multiply two [U8] values.
			## ```roc
			## expect U8.times(4, 3) == 12
			## ```
			times : U8, U8 -> U8

			## Multiply two [U8] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in a [U8].
			mul_try : U8, U8 -> Try(U8, [Overflow, ..])
			mul_try = |a, b| unsigned_mul_try(U8.highest, 0, a, b)

			## Multiply two [U8] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect U8.times_saturated(U8.highest, 2) == U8.highest
			##
			## expect U8.times_saturated(4, 3) == 12
			## ```
			times_saturated : U8, U8 -> U8
			times_saturated = |a, b| unsigned_times_saturated(U8.highest, 0, a, b)

			## Raise the first [U8] value to the power of the second.
			## Crashes if the exact result does not fit in [U8].
			## ```roc
			## expect U8.pow(2, 3) == 8
			##
			## expect U8.pow(5, 0) == 1
			## ```
			pow : U8, U8 -> U8
			pow = |base, exponent|
				match U8.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
				}

			## Raise the first [U8] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect U8.pow_try(2, 3) == Ok(8)
			##
			## expect U8.pow_try(U8.highest, 2) == Err(Overflow)
			## ```
			pow_try : U8, U8 -> Try(U8, [Overflow, ..])
			pow_try = |base, exponent| unsigned_pow_try(U8.highest, 0, 1, 2, base, exponent)

			## Divide the first [U8] by the second, discarding any remainder. Crashes if the second [U8] is zero.
			## ```roc
			## expect U8.div_by(10, 2) == 5
			##
			## expect U8.div_by(11, 2) == 5
			## ```
			div_by : U8, U8 -> U8

			## Divide the first [U8] by the second, returning `Err(DivByZero)`
			## instead of crashing if the divisor is zero.
			div_try : U8, U8 -> Try(U8, [DivByZero, ..])
			div_try = |a, b| unsigned_div_try(0, a, b)

			## Divide the first [U8] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [U8].
			## ```roc
			## expect U8.div_ceil_by(7, 2) == 4
			##
			## expect U8.div_ceil_by(8, 2) == 4
			## ```
			div_ceil_by : U8, U8 -> U8
			div_ceil_by = |a, b|
				match U8.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
				}

			## Divide the first [U8] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect U8.div_ceil_try(7, 2) == Ok(4)
			##
			## expect U8.div_ceil_try(1, 0) == Err(DivByZero)
			## ```
			div_ceil_try : U8, U8 -> Try(U8, [DivByZero, ..])
			div_ceil_try = |a, b| unsigned_div_ceil_try(0, 1, a, b)

			## Divide the first [U8] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [U8.div_by].
			## ```roc
			## expect U8.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U8, U8 -> U8

			## Return the remainder of dividing the first [U8] by the second.
			## ```roc
			## expect U8.rem_by(7, 3) == 1
			## ```
			rem_by : U8, U8 -> U8

			## Return the modulus of the first [U8] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [U8.rem_by].
			## ```roc
			## expect U8.mod_by(7, 3) == 1
			## ```
			mod_by : U8, U8 -> U8

			## Return the absolute difference between two [U8] values.
			## ```roc
			## expect U8.abs_diff(2, 5) == 3
			##
			## expect U8.abs_diff(5, 2) == 3
			## ```
			abs_diff : U8, U8 -> U8

			## Shift the bits of a [U8] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 256).
			## ```roc
			## expect U8.shift_left_by(1, 3) == 8
			##
			## expect U8.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U8, U8 -> U8

			## Shift the bits of a [U8] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [U8.shift_right_zf_by].
			## ```roc
			## expect U8.shift_right_by(32, 2) == 8
			##
			## expect U8.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U8, U8 -> U8

			## Shift the bits of a [U8] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [U8.shift_right_by].
			## ```roc
			## expect U8.shift_right_zf_by(32, 2) == 8
			##
			## expect U8.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U8, U8 -> U8

			## Returns the bitwise AND of two [U8] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect U8.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : U8, U8 -> U8

			## Returns the bitwise OR of two [U8] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect U8.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : U8, U8 -> U8

			## Returns the bitwise XOR of two [U8] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect U8.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : U8, U8 -> U8

			## Returns the bitwise NOT of a [U8] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`.
			## ```roc
			## expect U8.bitwise_not(0) == 255
			## ```
			bitwise_not : U8 -> U8

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect U8.count_leading_zero_bits(1) == 7
			##
			## expect U8.count_leading_zero_bits(0) == 8
			## ```
			count_leading_zero_bits : U8 -> U8
			count_leading_zero_bits = |value| unsigned_count_leading_zero_bits(8, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect U8.count_trailing_zero_bits(8) == 3
			##
			## expect U8.count_trailing_zero_bits(0) == 8
			## ```
			count_trailing_zero_bits : U8 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(8, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect U8.count_one_bits(0b1011) == 3
			##
			## expect U8.count_one_bits(0) == 0
			## ```
			count_one_bits : U8 -> U8
			count_one_bits = |value| unsigned_count_one_bits(0, 2, value)

			## Build a [U8] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U8] (`0` to `255`), or if any element is not a valid digit.
			## ```roc
			## expect U8.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U8, [OutOfRange])
			from_int_digits = |digits| u8_from_int_digits(digits)

			## Convert a numeric literal into a [U8]. This is the hook the
			## compiler uses when a literal is given type [U8]; most code should
			## parse user text with [U8.from_str] instead.
			from_numeral : Numeral -> Try(U8, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| u8_from_str(str))

			## Parse a [U8] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U8] (`0` to `255`).
			## ```roc
			## expect U8.from_str("42") == Ok(42)
			##
			## expect U8.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U8, [BadNumStr, ..])

			## Iterator of integers beginning with this `U8` and ending with the other `U8`.
			## (Use [U8.until] instead to end with the other `U8` minus one.)
			## Returns an empty iterator if this `U8` is greater than the other.
			## ```roc
			## expect Iter.fold(U8.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(U8.to(3, 3), [], |acc, item| acc.append(item)) == [3]
			##
			## expect Iter.fold(U8.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : U8, U8 -> Iter(U8)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					Known(U8.to_u64(end) - U8.to_u64(start) + 1)
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match U8.add_try(start, 1) {
									Ok(next) => if next <= end {
										U8.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `U8` and ending with the other `U8` minus one.
			## (Use [U8.to] instead to end with the other `U8` exactly, instead of minus one.)
			## Returns an empty iterator if this `U8` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(U8.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(U8.until(3, 3), [], |acc, item| acc.append(item)) == []
			##
			## expect Iter.fold(U8.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : U8, U8 -> Iter(U8)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(U8.to_u64(end) - U8.to_u64(start))
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match U8.add_try(start, 1) {
									Ok(next) => if next < end {
										U8.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			# Conversions to signed integers (I8 is lossy, others are safe)

			## Convert a [U8] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; values from `128` to `255` wrap into the negative
			## range `-128` to `-1` (two's complement reinterpretation of the bits).
			## ```roc
			## expect U8.to_i8_wrap(42) == 42
			##
			## expect U8.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U8 -> I8

			## Convert a [U8] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; values from `128` to
			## `255` return `Err(OutOfRange)`.
			## ```roc
			## expect U8.to_i8_try(42) == Ok(42)
			##
			## expect U8.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U8 -> Try(I8, [OutOfRange, ..])

			## Convert a [U8] to an [I16]. This widening conversion preserves
			## every [U8] value exactly.
			to_i16 : U8 -> I16

			## Convert a [U8] to an [I32]. This widening conversion preserves
			## every [U8] value exactly.
			to_i32 : U8 -> I32

			## Convert a [U8] to an [I64]. This widening conversion preserves
			## every [U8] value exactly.
			to_i64 : U8 -> I64

			## Convert a [U8] to an [I128]. This widening conversion preserves
			## every [U8] value exactly.
			to_i128 : U8 -> I128

			# Conversions to unsigned integers (all safe widening)
			## Convert a [U8] to a [U16]. This widening conversion preserves
			## every [U8] value exactly.
			to_u16 : U8 -> U16

			## Convert a [U8] to a [U32]. This widening conversion preserves
			## every [U8] value exactly.
			to_u32 : U8 -> U32

			## Convert a [U8] to a [U64]. This widening conversion preserves
			## every [U8] value exactly.
			to_u64 : U8 -> U64

			## Convert a [U8] to a [U128]. This widening conversion preserves
			## every [U8] value exactly.
			to_u128 : U8 -> U128

			# Conversions to floating point (all safe)
			## Convert a [U8] to an [F32]. Every [U8] value is exactly
			## representable as an [F32].
			to_f32 : U8 -> F32

			## Convert a [U8] to an [F64]. Every [U8] value is exactly
			## representable as an [F64].
			to_f64 : U8 -> F64

			## Convert a [U8] to a [Dec]. Every [U8] value is exactly
			## representable as a [Dec].
			to_dec : U8 -> Dec

			## Encode a U8 using a format that provides encode_u8
			encode : U8, fmt -> Try(encoded, err)
				where [fmt.encode_u8 : fmt, U8 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u8(self)
			}

			## Decode a U8 using a format that provides decode_u8
			decode : src, fmt -> (Try(U8, err), src)
				where [fmt.decode_u8 : fmt, src -> (Try(U8, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u8(format, source)
			}
		}

		I8 :: [].{

			## Returns the default [I8] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I8.default() == 0
			## ```
			default : () -> I8
			default = || 0

			## The highest value representable by an [I8], which is `127`.
			## ```roc
			## expect I8.highest == 127
			## ```
			highest : I8
			highest = 127

			## The lowest value representable by an [I8], which is `-128`.
			## ```roc
			## expect I8.lowest == -128
			## ```
			lowest : I8
			lowest = -128

			## Convert an [I8] to its decimal string representation.
			## ```roc
			## expect I8.to_str(42) == "42"
			##
			## expect I8.to_str(-42) == "-42"
			## ```
			to_str : I8 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I8.is_zero(0)
			##
			## expect !I8.is_zero(7)
			## ```
			is_zero : I8 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I8.is_negative(-3)
			##
			## expect !I8.is_negative(0)
			## ```
			is_negative : I8 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I8.is_positive(3)
			##
			## expect !I8.is_positive(0)
			## ```
			is_positive : I8 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I8.is_eq(3, 3)
			##
			## expect !I8.is_eq(3, 4)
			## ```
			is_eq : I8, I8 -> Bool

			## Feed an [I8] into a [Hasher].
			to_hash : I8, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_i8(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I8.is_gt(5, 3)
			##
			## expect !I8.is_gt(3, 3)
			## ```
			is_gt : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I8.is_gte(3, 3)
			##
			## expect !I8.is_gte(2, 3)
			## ```
			is_gte : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I8.is_lt(3, 5)
			##
			## expect !I8.is_lt(3, 3)
			## ```
			is_lt : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I8.is_lte(3, 3)
			##
			## expect !I8.is_lte(5, 3)
			## ```
			is_lte : I8, I8 -> Bool

			## Compare two [I8] values and return their ordering.
			## ```roc
			## expect I8.compare(1, 2) == LT
			##
			## expect I8.compare(2, 2) == EQ
			##
			## expect I8.compare(3, 2) == GT
			## ```
			compare : I8, I8 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect I8.is_even(4)
			##
			## expect !I8.is_even(5)
			## ```
			is_even : I8 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect I8.is_odd(5)
			##
			## expect !I8.is_odd(4)
			## ```
			is_odd : I8 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect I8.is_multiple_of(12, 3)
			##
			## expect I8.is_multiple_of(0, 0)
			##
			## expect !I8.is_multiple_of(5, 0)
			##
			## expect I8.is_multiple_of(I8.lowest, -1)
			## ```
			is_multiple_of : I8, I8 -> Bool
			is_multiple_of = |value, divisor| signed_is_multiple_of(0, -1, value, divisor)

			## Returns the greater of two [I8] values.
			## ```roc
			## expect I8.max(5, 3) == 5
			##
			## expect I8.max(-3, -1) == -1
			## ```
			max : I8, I8 -> I8
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I8] values.
			## ```roc
			## expect I8.min(5, 3) == 3
			##
			## expect I8.min(-3, -1) == -3
			## ```
			min : I8, I8 -> I8
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I8]. Crashes on `-128`, since `128` does not fit in an [I8].
			## ```roc
			## expect I8.negate(3) == -3
			##
			## expect I8.negate(-3) == 3
			## ```
			negate : I8 -> I8

			## Return the absolute value of an [I8]. Crashes on `-128`, since `128`
			## does not fit in an [I8].
			## ```roc
			## expect I8.abs(3) == 3
			##
			## expect I8.abs(-3) == 3
			## ```
			abs : I8 -> I8

			## Add two [I8] values.
			## ```roc
			## expect I8.plus(2, 3) == 5
			## ```
			plus : I8, I8 -> I8

			## Add two [I8] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in an [I8].
			add_try : I8, I8 -> Try(I8, [Overflow, ..])
			add_try = |a, b| signed_add_try(I8.lowest, I8.highest, 0, a, b)

			steps_between : I8, I8 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end).to_u64())
				else
					Known(0)

			## Add two [I8] values, saturating at [I8.highest] or [I8.lowest] on overflow rather than wrapping around.
			## ```roc
			## expect I8.plus_saturated(I8.highest, 1) == I8.highest
			##
			## expect I8.plus_saturated(I8.lowest, -1) == I8.lowest
			##
			## expect I8.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : I8, I8 -> I8
			plus_saturated = |a, b|
				if b > 0 and a > highest - b
					highest
				else if b < 0 and a < lowest - b
					lowest
				else
					a + b

			## Subtract the second [I8] from the first.
			## ```roc
			## expect I8.minus(5, 3) == 2
			## ```
			minus : I8, I8 -> I8

			## Subtract the second [I8] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in an [I8].
			sub_try : I8, I8 -> Try(I8, [Overflow, ..])
			sub_try = |a, b| signed_sub_try(I8.lowest, I8.highest, 0, a, b)

			## Subtract the second [I8] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect I8.minus_saturated(I8.lowest, 1) == I8.lowest
			##
			## expect I8.minus_saturated(I8.highest, -1) == I8.highest
			##
			## expect I8.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : I8, I8 -> I8
			minus_saturated = |a, b| signed_minus_saturated(I8.lowest, I8.highest, 0, a, b)

			## Multiply two [I8] values.
			## ```roc
			## expect I8.times(4, 3) == 12
			## ```
			times : I8, I8 -> I8

			## Multiply two [I8] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in an [I8].
			mul_try : I8, I8 -> Try(I8, [Overflow, ..])
			mul_try = |a, b| signed_mul_try(I8.lowest, I8.highest, 0, -1, a, b)

			## Multiply two [I8] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect I8.times_saturated(I8.highest, 2) == I8.highest
			##
			## expect I8.times_saturated(I8.lowest, 2) == I8.lowest
			##
			## expect I8.times_saturated(4, 3) == 12
			## ```
			times_saturated : I8, I8 -> I8
			times_saturated = |a, b| signed_times_saturated(I8.lowest, I8.highest, 0, -1, a, b)

			## Raise the first [I8] value to the power of the second.
			## Crashes if the exact result does not fit in [I8].
			## ```roc
			## expect I8.pow(2, 3) == 8
			##
			## expect I8.pow(5, 0) == 1
			## ```
			pow : I8, I8 -> I8
			pow = |base, exponent|
				match I8.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
					Err(Underflow) => {
						crash "integer exponentiation underflowed"
					}
				}

			## Raise the first [I8] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect I8.pow_try(2, 3) == Ok(8)
			##
			## expect I8.pow_try(I8.highest, 2) == Err(Overflow)
			##
			## expect I8.pow_try(2, -1) == Err(Underflow)
			##
			## expect I8.pow_try(-1, -3) == Ok(-1)
			## ```
			pow_try : I8, I8 -> Try(I8, [Overflow, Underflow, ..])
			pow_try = |base, exponent| signed_pow_try(I8.lowest, I8.highest, 0, 1, 2, -1, base, exponent)

			## Divide the first [I8] by the second, discarding any remainder. Crashes if the second [I8] is zero.
			## ```roc
			## expect I8.div_by(10, 2) == 5
			##
			## expect I8.div_by(11, 2) == 5
			## ```
			div_by : I8, I8 -> I8

			## Divide the first [I8] by the second. Returns `Err(DivByZero)` if
			## the divisor is zero, or `Err(Overflow)` for `I8.lowest / -1`.
			div_try : I8, I8 -> Try(I8, [DivByZero, Overflow, ..])
			div_try = |a, b| signed_div_try(I8.lowest, 0, -1, a, b)

			## Divide the first [I8] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [I8].
			## ```roc
			## expect I8.div_ceil_by(7, 2) == 4
			##
			## expect I8.div_ceil_by(8, 2) == 4
			##
			## expect I8.div_ceil_by(-7, 2) == -3
			##
			## expect I8.div_ceil_by(-7, -2) == 4
			## ```
			div_ceil_by : I8, I8 -> I8
			div_ceil_by = |a, b|
				match I8.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
					Err(Overflow) => {
						crash "integer ceiling division overflowed"
					}
				}

			## Divide the first [I8] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect I8.div_ceil_try(7, 2) == Ok(4)
			##
			## expect I8.div_ceil_try(1, 0) == Err(DivByZero)
			##
			## expect I8.div_ceil_try(I8.lowest, -1) == Err(Overflow)
			## ```
			div_ceil_try : I8, I8 -> Try(I8, [DivByZero, Overflow, ..])
			div_ceil_try = |a, b| signed_div_ceil_try(I8.lowest, I8.highest, 0, 1, -1, a, b)

			## Divide the first [I8] by the second, truncating toward zero.
			## ```roc
			## expect I8.div_trunc_by(7, 2) == 3
			##
			## expect I8.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I8, I8 -> I8

			## Return the remainder of dividing the first [I8] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I8.rem_by(7, 3) == 1
			##
			## expect I8.rem_by(-7, 3) == -1
			## ```
			rem_by : I8, I8 -> I8

			## Return the modulus of the first [I8] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [I8.rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I8.mod_by(7, 3) == 1
			##
			## expect I8.mod_by(-7, 3) == 2
			## ```
			mod_by : I8, I8 -> I8

			## Return the absolute difference between two [I8] values as a [U8].
			## The result is a [U8] because the difference between two [I8] values
			## can be as large as `255`, which does not fit in an [I8].
			## ```roc
			## expect I8.abs_diff(2, 5) == 3
			##
			## expect I8.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I8, I8 -> U8

			## Shift the bits of an [I8] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I8.shift_left_by(1, 3) == 8
			##
			## expect I8.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I8, U8 -> I8

			## Shift the bits of an [I8] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I8.shift_right_by(32, 2) == 8
			##
			## expect I8.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I8, U8 -> I8

			## Shift the bits of an [I8] to the right by the given number of
			## positions.
			## ```roc
			## expect I8.shift_right_zf_by(32, 2) == 8
			##
			## expect I8.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I8, U8 -> I8

			## Returns the bitwise AND of two [I8] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect I8.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : I8, I8 -> I8

			## Returns the bitwise OR of two [I8] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect I8.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : I8, I8 -> I8

			## Returns the bitwise XOR of two [I8] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect I8.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : I8, I8 -> I8

			## Returns the bitwise NOT of an [I8] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`. For signed integers
			## this is equivalent to `-value - 1`.
			## ```roc
			## expect I8.bitwise_not(5) == -6
			## ```
			bitwise_not : I8 -> I8

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect I8.count_leading_zero_bits(-1) == 0
			##
			## expect I8.count_leading_zero_bits(0) == 8
			## ```
			count_leading_zero_bits : I8 -> U8
			count_leading_zero_bits = |value| signed_count_leading_zero_bits(8, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect I8.count_trailing_zero_bits(-8) == 3
			##
			## expect I8.count_trailing_zero_bits(0) == 8
			## ```
			count_trailing_zero_bits : I8 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(8, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect I8.count_one_bits(-1) == 8
			##
			## expect I8.count_one_bits(0) == 0
			## ```
			count_one_bits : I8 -> U8
			count_one_bits = |value| signed_count_one_bits(8, 0, 2, value)

			## Iterator of integers beginning with this `I8` and ending with the other `I8`.
			## (Use [I8.until] instead to end with the other `I8` minus one.)
			## Returns an empty iterator if this `I8` is greater than the other.
			## ```roc
			## expect Iter.fold(I8.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(I8.to(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0, 1]
			##
			## expect Iter.fold(I8.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : I8, I8 -> Iter(I8)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					Known(I64.to_u64_wrap(I8.to_i64(end) - I8.to_i64(start) + 1))
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match I8.add_try(start, 1) {
									Ok(next) => if next <= end {
										I8.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `I8` and ending with the other `I8` minus one.
			## (Use [I8.to] instead to end with the other `I8` exactly, instead of minus one.)
			## Returns an empty iterator if this `I8` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(I8.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(I8.until(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0]
			##
			## expect Iter.fold(I8.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : I8, I8 -> Iter(I8)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(I64.to_u64_wrap(I8.to_i64(end) - I8.to_i64(start)))
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match I8.add_try(start, 1) {
									Ok(next) => if next < end {
										I8.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build an [I8] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I8] (`-128` to `127`), or if any element is not a valid digit. The
			## result is always non-negative; to build a negative value, [I8.negate]
			## the result.
			## ```roc
			## expect I8.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I8, [OutOfRange])
			from_int_digits = |digits| i8_from_int_digits(digits)

			## Convert a numeric literal into an [I8]. This is the hook the
			## compiler uses when a literal is given type [I8]; most code should
			## parse user text with [I8.from_str] instead.
			from_numeral : Numeral -> Try(I8, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| i8_from_str(str))

			## Parse an [I8] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I8] (`-128` to `127`).
			## ```roc
			## expect I8.from_str("42") == Ok(42)
			##
			## expect I8.from_str("-1") == Ok(-1)
			##
			## expect I8.from_str("200") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(I8, [BadNumStr, ..])

			# Conversions to signed integers (all safe widening)
			## Convert an [I8] to an [I16]. This widening conversion preserves
			## every [I8] value exactly.
			to_i16 : I8 -> I16

			## Convert an [I8] to an [I32]. This widening conversion preserves
			## every [I8] value exactly.
			to_i32 : I8 -> I32

			## Convert an [I8] to an [I64]. This widening conversion preserves
			## every [I8] value exactly.
			to_i64 : I8 -> I64

			## Convert an [I8] to an [I128]. This widening conversion preserves
			## every [I8] value exactly.
			to_i128 : I8 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I8] to a [U8], wrapping on overflow. Values from `0` to
			## `127` are preserved; values from `-128` to `-1` wrap into the range
			## `128` to `255` (two's complement reinterpretation of the bits).
			## ```roc
			## expect I8.to_u8_wrap(42) == 42
			##
			## expect I8.to_u8_wrap(-56) == 200
			## ```
			to_u8_wrap : I8 -> U8

			## Convert an [I8] to a [U8], returning `Err(OutOfRange)` if the value
			## is negative. Values from `0` to `127` succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u8_try(42) == Ok(42)
			##
			## expect I8.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I8 -> Try(U8, [OutOfRange, ..])

			## Convert an [I8] to a [U16], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U16] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I8.to_u16_wrap(42) == 42
			##
			## expect I8.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I8 -> U16

			## Convert an [I8] to a [U16], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u16_try(42) == Ok(42)
			##
			## expect I8.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I8 -> Try(U16, [OutOfRange, ..])

			## Convert an [I8] to a [U32], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U32] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I8.to_u32_wrap(42) == 42
			##
			## expect I8.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I8 -> U32

			## Convert an [I8] to a [U32], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u32_try(42) == Ok(42)
			##
			## expect I8.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I8 -> Try(U32, [OutOfRange, ..])

			## Convert an [I8] to a [U64], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U64] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I8.to_u64_wrap(42) == 42
			##
			## expect I8.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I8 -> U64

			## Convert an [I8] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u64_try(42) == Ok(42)
			##
			## expect I8.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I8 -> Try(U64, [OutOfRange, ..])

			## Convert an [I8] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I8.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I8 -> U128

			## Convert an [I8] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u128_try(42) == Ok(42)
			##
			## expect I8.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I8 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			## Convert an [I8] to an [F32]. Every [I8] value is exactly
			## representable as an [F32].
			to_f32 : I8 -> F32

			## Convert an [I8] to an [F64]. Every [I8] value is exactly
			## representable as an [F64].
			to_f64 : I8 -> F64

			## Convert an [I8] to a [Dec]. Every [I8] value is exactly
			## representable as a [Dec].
			to_dec : I8 -> Dec

			## Encode an I8 using a format that provides encode_i8
			encode : I8, fmt -> Try(encoded, err)
				where [fmt.encode_i8 : fmt, I8 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i8(self)
			}

			## Decode an I8 using a format that provides decode_i8
			decode : src, fmt -> (Try(I8, err), src)
				where [fmt.decode_i8 : fmt, src -> (Try(I8, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i8(format, source)
			}
		}

		U16 :: [].{

			## Returns the default [U16] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U16.default() == 0
			## ```
			default : () -> U16
			default = || 0

			## The highest value representable by a [U16], which is `65535`.
			## ```roc
			## expect U16.highest == 65535
			## ```
			highest : U16
			highest = 65535

			## The lowest value representable by a [U16], which is `0`.
			## ```roc
			## expect U16.lowest == 0
			## ```
			lowest : U16
			lowest = 0

			## Convert a [U16] to its decimal string representation.
			## ```roc
			## expect U16.to_str(42) == "42"
			## ```
			to_str : U16 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U16.is_zero(0)
			##
			## expect !U16.is_zero(7)
			## ```
			is_zero : U16 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U16.is_eq(3, 3)
			##
			## expect !U16.is_eq(3, 4)
			## ```
			is_eq : U16, U16 -> Bool

			## Feed a [U16] into a [Hasher].
			to_hash : U16, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_u16(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U16.is_gt(5, 3)
			##
			## expect !U16.is_gt(3, 3)
			## ```
			is_gt : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U16.is_gte(3, 3)
			##
			## expect !U16.is_gte(2, 3)
			## ```
			is_gte : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U16.is_lt(3, 5)
			##
			## expect !U16.is_lt(3, 3)
			## ```
			is_lt : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U16.is_lte(3, 3)
			##
			## expect !U16.is_lte(5, 3)
			## ```
			is_lte : U16, U16 -> Bool

			## Compare two [U16] values and return their ordering.
			## ```roc
			## expect U16.compare(1, 2) == LT
			##
			## expect U16.compare(2, 2) == EQ
			##
			## expect U16.compare(3, 2) == GT
			## ```
			compare : U16, U16 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect U16.is_even(4)
			##
			## expect !U16.is_even(5)
			## ```
			is_even : U16 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect U16.is_odd(5)
			##
			## expect !U16.is_odd(4)
			## ```
			is_odd : U16 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect U16.is_multiple_of(12, 3)
			##
			## expect U16.is_multiple_of(0, 0)
			##
			## expect !U16.is_multiple_of(5, 0)
			## ```
			is_multiple_of : U16, U16 -> Bool
			is_multiple_of = |value, divisor| unsigned_is_multiple_of(0, value, divisor)

			## Returns the greater of two [U16] values.
			## ```roc
			## expect U16.max(5, 3) == 5
			## ```
			max : U16, U16 -> U16
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U16] values.
			## ```roc
			## expect U16.min(5, 3) == 3
			## ```
			min : U16, U16 -> U16
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U16] values.
			## ```roc
			## expect U16.plus(2, 3) == 5
			## ```
			plus : U16, U16 -> U16

			## Add two [U16] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in a [U16].
			add_try : U16, U16 -> Try(U16, [Overflow, ..])
			add_try = |a, b| unsigned_add_try(U16.highest, a, b)

			steps_between : U16, U16 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end).to_u64())
				else
					Known(0)

			## Add two [U16] values, saturating at [U16.highest] on overflow rather than wrapping around.
			## ```roc
			## expect U16.plus_saturated(U16.highest, 1) == U16.highest
			##
			## expect U16.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : U16, U16 -> U16
			plus_saturated = |a, b|
				if b > highest - a
					highest
				else
					a + b

			## Subtract the second [U16] from the first.
			## ```roc
			## expect U16.minus(5, 3) == 2
			## ```
			minus : U16, U16 -> U16

			## Subtract the second [U16] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in a [U16].
			sub_try : U16, U16 -> Try(U16, [Overflow, ..])
			sub_try = |a, b| unsigned_sub_try(a, b)

			## Subtract the second [U16] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect U16.minus_saturated(0, 1) == 0
			##
			## expect U16.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : U16, U16 -> U16
			minus_saturated = |a, b| unsigned_minus_saturated(0, a, b)

			## Multiply two [U16] values.
			## ```roc
			## expect U16.times(4, 3) == 12
			## ```
			times : U16, U16 -> U16

			## Multiply two [U16] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in a [U16].
			mul_try : U16, U16 -> Try(U16, [Overflow, ..])
			mul_try = |a, b| unsigned_mul_try(U16.highest, 0, a, b)

			## Multiply two [U16] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect U16.times_saturated(U16.highest, 2) == U16.highest
			##
			## expect U16.times_saturated(4, 3) == 12
			## ```
			times_saturated : U16, U16 -> U16
			times_saturated = |a, b| unsigned_times_saturated(U16.highest, 0, a, b)

			## Raise the first [U16] value to the power of the second.
			## Crashes if the exact result does not fit in [U16].
			## ```roc
			## expect U16.pow(2, 3) == 8
			##
			## expect U16.pow(5, 0) == 1
			## ```
			pow : U16, U16 -> U16
			pow = |base, exponent|
				match U16.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
				}

			## Raise the first [U16] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect U16.pow_try(2, 3) == Ok(8)
			##
			## expect U16.pow_try(U16.highest, 2) == Err(Overflow)
			## ```
			pow_try : U16, U16 -> Try(U16, [Overflow, ..])
			pow_try = |base, exponent| unsigned_pow_try(U16.highest, 0, 1, 2, base, exponent)

			## Divide the first [U16] by the second, discarding any remainder. Crashes if the second [U16] is zero.
			## ```roc
			## expect U16.div_by(10, 2) == 5
			##
			## expect U16.div_by(11, 2) == 5
			## ```
			div_by : U16, U16 -> U16

			## Divide the first [U16] by the second, returning `Err(DivByZero)`
			## instead of crashing if the divisor is zero.
			div_try : U16, U16 -> Try(U16, [DivByZero, ..])
			div_try = |a, b| unsigned_div_try(0, a, b)

			## Divide the first [U16] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [U16].
			## ```roc
			## expect U16.div_ceil_by(7, 2) == 4
			##
			## expect U16.div_ceil_by(8, 2) == 4
			## ```
			div_ceil_by : U16, U16 -> U16
			div_ceil_by = |a, b|
				match U16.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
				}

			## Divide the first [U16] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect U16.div_ceil_try(7, 2) == Ok(4)
			##
			## expect U16.div_ceil_try(1, 0) == Err(DivByZero)
			## ```
			div_ceil_try : U16, U16 -> Try(U16, [DivByZero, ..])
			div_ceil_try = |a, b| unsigned_div_ceil_try(0, 1, a, b)

			## Divide the first [U16] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [U16.div_by].
			## ```roc
			## expect U16.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U16, U16 -> U16

			## Return the remainder of dividing the first [U16] by the second.
			## ```roc
			## expect U16.rem_by(7, 3) == 1
			## ```
			rem_by : U16, U16 -> U16

			## Return the modulus of the first [U16] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [U16.rem_by].
			## ```roc
			## expect U16.mod_by(7, 3) == 1
			## ```
			mod_by : U16, U16 -> U16

			## Return the absolute difference between two [U16] values.
			## ```roc
			## expect U16.abs_diff(2, 5) == 3
			##
			## expect U16.abs_diff(5, 2) == 3
			## ```
			abs_diff : U16, U16 -> U16

			## Shift the bits of a [U16] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 65536).
			## ```roc
			## expect U16.shift_left_by(1, 3) == 8
			##
			## expect U16.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U16, U8 -> U16

			## Shift the bits of a [U16] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [U16.shift_right_zf_by].
			## ```roc
			## expect U16.shift_right_by(32, 2) == 8
			##
			## expect U16.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U16, U8 -> U16

			## Shift the bits of a [U16] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [U16.shift_right_by].
			## ```roc
			## expect U16.shift_right_zf_by(32, 2) == 8
			##
			## expect U16.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U16, U8 -> U16

			## Returns the bitwise AND of two [U16] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect U16.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : U16, U16 -> U16

			## Returns the bitwise OR of two [U16] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect U16.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : U16, U16 -> U16

			## Returns the bitwise XOR of two [U16] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect U16.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : U16, U16 -> U16

			## Returns the bitwise NOT of a [U16] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`.
			## ```roc
			## expect U16.bitwise_not(0) == 65535
			## ```
			bitwise_not : U16 -> U16

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect U16.count_leading_zero_bits(1) == 15
			##
			## expect U16.count_leading_zero_bits(0) == 16
			## ```
			count_leading_zero_bits : U16 -> U8
			count_leading_zero_bits = |value| unsigned_count_leading_zero_bits(16, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect U16.count_trailing_zero_bits(8) == 3
			##
			## expect U16.count_trailing_zero_bits(0) == 16
			## ```
			count_trailing_zero_bits : U16 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(16, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect U16.count_one_bits(0b1011) == 3
			##
			## expect U16.count_one_bits(0) == 0
			## ```
			count_one_bits : U16 -> U8
			count_one_bits = |value| unsigned_count_one_bits(0, 2, value)

			## Iterator of integers beginning with this `U16` and ending with the other `U16`.
			## (Use [U16.until] instead to end with the other `U16` minus one.)
			## Returns an empty iterator if this `U16` is greater than the other.
			## ```roc
			## expect Iter.fold(U16.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(U16.to(3, 3), [], |acc, item| acc.append(item)) == [3]
			##
			## expect Iter.fold(U16.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : U16, U16 -> Iter(U16)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					Known(U16.to_u64(end) - U16.to_u64(start) + 1)
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match U16.add_try(start, 1) {
									Ok(next) => if next <= end {
										U16.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `U16` and ending with the other `U16` minus one.
			## (Use [U16.to] instead to end with the other `U16` exactly, instead of minus one.)
			## Returns an empty iterator if this `U16` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(U16.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(U16.until(3, 3), [], |acc, item| acc.append(item)) == []
			##
			## expect Iter.fold(U16.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : U16, U16 -> Iter(U16)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(U16.to_u64(end) - U16.to_u64(start))
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match U16.add_try(start, 1) {
									Ok(next) => if next < end {
										U16.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build a [U16] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U16] (`0` to `65535`), or if any element is not a valid digit.
			## ```roc
			## expect U16.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U16, [OutOfRange])
			from_int_digits = |digits| u16_from_int_digits(digits)

			## Convert a numeric literal into a [U16]. This is the hook the
			## compiler uses when a literal is given type [U16]; most code should
			## parse user text with [U16.from_str] instead.
			from_numeral : Numeral -> Try(U16, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| u16_from_str(str))

			## Parse a [U16] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U16] (`0` to `65535`).
			## ```roc
			## expect U16.from_str("42") == Ok(42)
			##
			## expect U16.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U16, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U16] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U16.to_i8_wrap(42) == 42
			##
			## expect U16.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U16 -> I8

			## Convert a [U16] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; values from `128` to
			## `65535` return `Err(OutOfRange)`.
			## ```roc
			## expect U16.to_i8_try(42) == Ok(42)
			##
			## expect U16.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U16 -> Try(I8, [OutOfRange, ..])

			## Convert a [U16] to an [I16], wrapping on overflow. Values from `0` to
			## `32767` are preserved; values from `32768` to `65535` wrap into the
			## negative range `-32768` to `-1` (two's complement reinterpretation of
			## the bits).
			## ```roc
			## expect U16.to_i16_wrap(42) == 42
			##
			## expect U16.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U16 -> I16

			## Convert a [U16] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; values from `32768`
			## to `65535` return `Err(OutOfRange)`.
			## ```roc
			## expect U16.to_i16_try(42) == Ok(42)
			##
			## expect U16.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U16 -> Try(I16, [OutOfRange, ..])

			## Convert a [U16] to an [I32]. This widening conversion preserves
			## every [U16] value exactly.
			to_i32 : U16 -> I32

			## Convert a [U16] to an [I64]. This widening conversion preserves
			## every [U16] value exactly.
			to_i64 : U16 -> I64

			## Convert a [U16] to an [I128]. This widening conversion preserves
			## every [U16] value exactly.
			to_i128 : U16 -> I128

			# Conversions to unsigned integers

			## Convert a [U16] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U16.to_u8_wrap(42) == 42
			##
			## expect U16.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U16 -> U8

			## Convert a [U16] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; values from `256` to
			## `65535` return `Err(OutOfRange)`.
			## ```roc
			## expect U16.to_u8_try(42) == Ok(42)
			##
			## expect U16.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U16 -> Try(U8, [OutOfRange, ..])

			## Convert a [U16] to a [U32]. This widening conversion preserves
			## every [U16] value exactly.
			to_u32 : U16 -> U32

			## Convert a [U16] to a [U64]. This widening conversion preserves
			## every [U16] value exactly.
			to_u64 : U16 -> U64

			## Convert a [U16] to a [U128]. This widening conversion preserves
			## every [U16] value exactly.
			to_u128 : U16 -> U128

			# Conversions to floating point (all safe)
			## Convert a [U16] to an [F32]. Every [U16] value is exactly
			## representable as an [F32].
			to_f32 : U16 -> F32

			## Convert a [U16] to an [F64]. Every [U16] value is exactly
			## representable as an [F64].
			to_f64 : U16 -> F64

			## Convert a [U16] to a [Dec]. Every [U16] value is exactly
			## representable as a [Dec].
			to_dec : U16 -> Dec

			# Encode a U16 using a format that provides encode_u16
			encode : U16, fmt -> Try(encoded, err)
				where [fmt.encode_u16 : fmt, U16 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u16(self)
			}

			decode : src, fmt -> (Try(U16, err), src)
				where [fmt.decode_u16 : fmt, src -> (Try(U16, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u16(format, source)
			}
		}

		I16 :: [].{

			## Returns the default [I16] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I16.default() == 0
			## ```
			default : () -> I16
			default = || 0

			## The highest value representable by an [I16], which is `32767`.
			## ```roc
			## expect I16.highest == 32767
			## ```
			highest : I16
			highest = 32767

			## The lowest value representable by an [I16], which is `-32768`.
			## ```roc
			## expect I16.lowest == -32768
			## ```
			lowest : I16
			lowest = -32768

			## Convert an [I16] to its decimal string representation.
			## ```roc
			## expect I16.to_str(42) == "42"
			##
			## expect I16.to_str(-42) == "-42"
			## ```
			to_str : I16 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I16.is_zero(0)
			##
			## expect !I16.is_zero(7)
			## ```
			is_zero : I16 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I16.is_negative(-3)
			##
			## expect !I16.is_negative(0)
			## ```
			is_negative : I16 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I16.is_positive(3)
			##
			## expect !I16.is_positive(0)
			## ```
			is_positive : I16 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I16.is_eq(3, 3)
			##
			## expect !I16.is_eq(3, 4)
			## ```
			is_eq : I16, I16 -> Bool

			## Feed an [I16] into a [Hasher].
			to_hash : I16, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_i16(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I16.is_gt(5, 3)
			##
			## expect !I16.is_gt(3, 3)
			## ```
			is_gt : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I16.is_gte(3, 3)
			##
			## expect !I16.is_gte(2, 3)
			## ```
			is_gte : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I16.is_lt(3, 5)
			##
			## expect !I16.is_lt(3, 3)
			## ```
			is_lt : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I16.is_lte(3, 3)
			##
			## expect !I16.is_lte(5, 3)
			## ```
			is_lte : I16, I16 -> Bool

			## Compare two [I16] values and return their ordering.
			## ```roc
			## expect I16.compare(1, 2) == LT
			##
			## expect I16.compare(2, 2) == EQ
			##
			## expect I16.compare(3, 2) == GT
			## ```
			compare : I16, I16 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect I16.is_even(4)
			##
			## expect !I16.is_even(5)
			## ```
			is_even : I16 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect I16.is_odd(5)
			##
			## expect !I16.is_odd(4)
			## ```
			is_odd : I16 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect I16.is_multiple_of(12, 3)
			##
			## expect I16.is_multiple_of(0, 0)
			##
			## expect !I16.is_multiple_of(5, 0)
			##
			## expect I16.is_multiple_of(I16.lowest, -1)
			## ```
			is_multiple_of : I16, I16 -> Bool
			is_multiple_of = |value, divisor| signed_is_multiple_of(0, -1, value, divisor)

			## Returns the greater of two [I16] values.
			## ```roc
			## expect I16.max(5, 3) == 5
			##
			## expect I16.max(-3, -1) == -1
			## ```
			max : I16, I16 -> I16
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I16] values.
			## ```roc
			## expect I16.min(5, 3) == 3
			##
			## expect I16.min(-3, -1) == -3
			## ```
			min : I16, I16 -> I16
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I16]. Crashes on `-32768`, since `32768` does not fit in an [I16].
			## ```roc
			## expect I16.negate(3) == -3
			##
			## expect I16.negate(-3) == 3
			## ```
			negate : I16 -> I16

			## Return the absolute value of an [I16]. Crashes on `-32768`, since `32768`
			## does not fit in an [I16].
			## ```roc
			## expect I16.abs(3) == 3
			##
			## expect I16.abs(-3) == 3
			## ```
			abs : I16 -> I16

			## Add two [I16] values.
			## ```roc
			## expect I16.plus(2, 3) == 5
			## ```
			plus : I16, I16 -> I16

			## Add two [I16] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in an [I16].
			add_try : I16, I16 -> Try(I16, [Overflow, ..])
			add_try = |a, b| signed_add_try(I16.lowest, I16.highest, 0, a, b)

			steps_between : I16, I16 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end).to_u64())
				else
					Known(0)

			## Add two [I16] values, saturating at [I16.highest] or [I16.lowest] on overflow rather than wrapping around.
			## ```roc
			## expect I16.plus_saturated(I16.highest, 1) == I16.highest
			##
			## expect I16.plus_saturated(I16.lowest, -1) == I16.lowest
			##
			## expect I16.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : I16, I16 -> I16
			plus_saturated = |a, b|
				if b > 0 and a > highest - b
					highest
				else if b < 0 and a < lowest - b
					lowest
				else
					a + b

			## Subtract the second [I16] from the first.
			## ```roc
			## expect I16.minus(5, 3) == 2
			## ```
			minus : I16, I16 -> I16

			## Subtract the second [I16] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in an [I16].
			sub_try : I16, I16 -> Try(I16, [Overflow, ..])
			sub_try = |a, b| signed_sub_try(I16.lowest, I16.highest, 0, a, b)

			## Subtract the second [I16] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect I16.minus_saturated(I16.lowest, 1) == I16.lowest
			##
			## expect I16.minus_saturated(I16.highest, -1) == I16.highest
			##
			## expect I16.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : I16, I16 -> I16
			minus_saturated = |a, b| signed_minus_saturated(I16.lowest, I16.highest, 0, a, b)

			## Multiply two [I16] values.
			## ```roc
			## expect I16.times(4, 3) == 12
			## ```
			times : I16, I16 -> I16

			## Multiply two [I16] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in an [I16].
			mul_try : I16, I16 -> Try(I16, [Overflow, ..])
			mul_try = |a, b| signed_mul_try(I16.lowest, I16.highest, 0, -1, a, b)

			## Multiply two [I16] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect I16.times_saturated(I16.highest, 2) == I16.highest
			##
			## expect I16.times_saturated(I16.lowest, 2) == I16.lowest
			##
			## expect I16.times_saturated(4, 3) == 12
			## ```
			times_saturated : I16, I16 -> I16
			times_saturated = |a, b| signed_times_saturated(I16.lowest, I16.highest, 0, -1, a, b)

			## Raise the first [I16] value to the power of the second.
			## Crashes if the exact result does not fit in [I16].
			## ```roc
			## expect I16.pow(2, 3) == 8
			##
			## expect I16.pow(5, 0) == 1
			## ```
			pow : I16, I16 -> I16
			pow = |base, exponent|
				match I16.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
					Err(Underflow) => {
						crash "integer exponentiation underflowed"
					}
				}

			## Raise the first [I16] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect I16.pow_try(2, 3) == Ok(8)
			##
			## expect I16.pow_try(I16.highest, 2) == Err(Overflow)
			##
			## expect I16.pow_try(2, -1) == Err(Underflow)
			##
			## expect I16.pow_try(-1, -3) == Ok(-1)
			## ```
			pow_try : I16, I16 -> Try(I16, [Overflow, Underflow, ..])
			pow_try = |base, exponent| signed_pow_try(I16.lowest, I16.highest, 0, 1, 2, -1, base, exponent)

			## Divide the first [I16] by the second, discarding any remainder. Crashes if the second [I16] is zero.
			## ```roc
			## expect I16.div_by(10, 2) == 5
			##
			## expect I16.div_by(11, 2) == 5
			## ```
			div_by : I16, I16 -> I16

			## Divide the first [I16] by the second. Returns `Err(DivByZero)` if
			## the divisor is zero, or `Err(Overflow)` for `I16.lowest / -1`.
			div_try : I16, I16 -> Try(I16, [DivByZero, Overflow, ..])
			div_try = |a, b| signed_div_try(I16.lowest, 0, -1, a, b)

			## Divide the first [I16] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [I16].
			## ```roc
			## expect I16.div_ceil_by(7, 2) == 4
			##
			## expect I16.div_ceil_by(8, 2) == 4
			##
			## expect I16.div_ceil_by(-7, 2) == -3
			##
			## expect I16.div_ceil_by(-7, -2) == 4
			## ```
			div_ceil_by : I16, I16 -> I16
			div_ceil_by = |a, b|
				match I16.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
					Err(Overflow) => {
						crash "integer ceiling division overflowed"
					}
				}

			## Divide the first [I16] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect I16.div_ceil_try(7, 2) == Ok(4)
			##
			## expect I16.div_ceil_try(1, 0) == Err(DivByZero)
			##
			## expect I16.div_ceil_try(I16.lowest, -1) == Err(Overflow)
			## ```
			div_ceil_try : I16, I16 -> Try(I16, [DivByZero, Overflow, ..])
			div_ceil_try = |a, b| signed_div_ceil_try(I16.lowest, I16.highest, 0, 1, -1, a, b)

			## Divide the first [I16] by the second, truncating toward zero.
			## ```roc
			## expect I16.div_trunc_by(7, 2) == 3
			##
			## expect I16.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I16, I16 -> I16

			## Return the remainder of dividing the first [I16] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I16.rem_by(7, 3) == 1
			##
			## expect I16.rem_by(-7, 3) == -1
			## ```
			rem_by : I16, I16 -> I16

			## Return the modulus of the first [I16] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [I16.rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I16.mod_by(7, 3) == 1
			##
			## expect I16.mod_by(-7, 3) == 2
			## ```
			mod_by : I16, I16 -> I16

			## Return the absolute difference between two [I16] values as a [U16].
			## The result is a [U16] because the difference between two [I16] values
			## can be as large as `65535`, which does not fit in an [I16].
			## ```roc
			## expect I16.abs_diff(2, 5) == 3
			##
			## expect I16.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I16, I16 -> U16

			## Shift the bits of an [I16] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I16.shift_left_by(1, 3) == 8
			##
			## expect I16.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I16, U8 -> I16

			## Shift the bits of an [I16] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I16.shift_right_by(32, 2) == 8
			##
			## expect I16.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I16, U8 -> I16

			## Shift the bits of an [I16] to the right by the given number of
			## positions.
			## ```roc
			## expect I16.shift_right_zf_by(32, 2) == 8
			##
			## expect I16.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I16, U8 -> I16

			## Returns the bitwise AND of two [I16] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect I16.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : I16, I16 -> I16

			## Returns the bitwise OR of two [I16] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect I16.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : I16, I16 -> I16

			## Returns the bitwise XOR of two [I16] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect I16.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : I16, I16 -> I16

			## Returns the bitwise NOT of an [I16] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`. For signed integers
			## this is equivalent to `-value - 1`.
			## ```roc
			## expect I16.bitwise_not(5) == -6
			## ```
			bitwise_not : I16 -> I16

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect I16.count_leading_zero_bits(-1) == 0
			##
			## expect I16.count_leading_zero_bits(0) == 16
			## ```
			count_leading_zero_bits : I16 -> U8
			count_leading_zero_bits = |value| signed_count_leading_zero_bits(16, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect I16.count_trailing_zero_bits(-8) == 3
			##
			## expect I16.count_trailing_zero_bits(0) == 16
			## ```
			count_trailing_zero_bits : I16 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(16, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect I16.count_one_bits(-1) == 16
			##
			## expect I16.count_one_bits(0) == 0
			## ```
			count_one_bits : I16 -> U8
			count_one_bits = |value| signed_count_one_bits(16, 0, 2, value)

			## Iterator of integers beginning with this `I16` and ending with the other `I16`.
			## (Use [I16.until] instead to end with the other `I16` minus one.)
			## Returns an empty iterator if this `I16` is greater than the other.
			## ```roc
			## expect Iter.fold(I16.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(I16.to(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0, 1]
			##
			## expect Iter.fold(I16.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : I16, I16 -> Iter(I16)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					Known(I64.to_u64_wrap(I16.to_i64(end) - I16.to_i64(start) + 1))
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match I16.add_try(start, 1) {
									Ok(next) => if next <= end {
										I16.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `I16` and ending with the other `I16` minus one.
			## (Use [I16.to] instead to end with the other `I16` exactly, instead of minus one.)
			## Returns an empty iterator if this `I16` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(I16.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(I16.until(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0]
			##
			## expect Iter.fold(I16.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : I16, I16 -> Iter(I16)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(I64.to_u64_wrap(I16.to_i64(end) - I16.to_i64(start)))
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match I16.add_try(start, 1) {
									Ok(next) => if next < end {
										I16.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build an [I16] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I16] (`-32768` to `32767`), or if any element is not a valid digit.
			## The result is always non-negative; to build a negative value, [I16.negate]
			## the result.
			## ```roc
			## expect I16.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I16, [OutOfRange])
			from_int_digits = |digits| i16_from_int_digits(digits)

			## Convert a numeric literal into an [I16]. This is the hook the
			## compiler uses when a literal is given type [I16]; most code should
			## parse user text with [I16.from_str] instead.
			from_numeral : Numeral -> Try(I16, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| i16_from_str(str))

			## Parse an [I16] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I16] (`-32768` to `32767`).
			## ```roc
			## expect I16.from_str("42") == Ok(42)
			##
			## expect I16.from_str("-1") == Ok(-1)
			##
			## expect I16.from_str("40000") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(I16, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I16] to an [I8], wrapping on overflow. Values from `-128`
			## to `127` are preserved; other values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect I16.to_i8_wrap(42) == 42
			##
			## expect I16.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I16 -> I8

			## Convert an [I16] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_i8_try(42) == Ok(42)
			##
			## expect I16.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I16 -> Try(I8, [OutOfRange, ..])

			## Convert an [I16] to an [I32]. This widening conversion preserves
			## every [I16] value exactly.
			to_i32 : I16 -> I32

			## Convert an [I16] to an [I64]. This widening conversion preserves
			## every [I16] value exactly.
			to_i64 : I16 -> I64

			## Convert an [I16] to an [I128]. This widening conversion preserves
			## every [I16] value exactly.
			to_i128 : I16 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I16] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I16.to_u8_wrap(42) == 42
			##
			## expect I16.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I16 -> U8

			## Convert an [I16] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u8_try(42) == Ok(42)
			##
			## expect I16.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I16 -> Try(U8, [OutOfRange, ..])

			## Convert an [I16] to a [U16], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of the
			## [U16] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I16.to_u16_wrap(42) == 42
			##
			## expect I16.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I16 -> U16

			## Convert an [I16] to a [U16], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u16_try(42) == Ok(42)
			##
			## expect I16.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I16 -> Try(U16, [OutOfRange, ..])

			## Convert an [I16] to a [U32], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U32] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I16.to_u32_wrap(42) == 42
			##
			## expect I16.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I16 -> U32

			## Convert an [I16] to a [U32], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u32_try(42) == Ok(42)
			##
			## expect I16.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I16 -> Try(U32, [OutOfRange, ..])

			## Convert an [I16] to a [U64], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U64] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I16.to_u64_wrap(42) == 42
			##
			## expect I16.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I16 -> U64

			## Convert an [I16] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u64_try(42) == Ok(42)
			##
			## expect I16.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I16 -> Try(U64, [OutOfRange, ..])

			## Convert an [I16] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I16.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I16 -> U128

			## Convert an [I16] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u128_try(42) == Ok(42)
			##
			## expect I16.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I16 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			## Convert an [I16] to an [F32]. Every [I16] value is exactly
			## representable as an [F32].
			to_f32 : I16 -> F32

			## Convert an [I16] to an [F64]. Every [I16] value is exactly
			## representable as an [F64].
			to_f64 : I16 -> F64

			## Convert an [I16] to a [Dec]. Every [I16] value is exactly
			## representable as a [Dec].
			to_dec : I16 -> Dec

			# Encode an I16 using a format that provides encode_i16
			encode : I16, fmt -> Try(encoded, err)
				where [fmt.encode_i16 : fmt, I16 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i16(self)
			}

			decode : src, fmt -> (Try(I16, err), src)
				where [fmt.decode_i16 : fmt, src -> (Try(I16, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i16(format, source)
			}
		}

		U32 :: [].{

			## Returns the default [U32] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U32.default() == 0
			## ```
			default : () -> U32
			default = || 0

			## The highest value representable by a [U32], which is `4294967295`.
			## ```roc
			## expect U32.highest == 4294967295
			## ```
			highest : U32
			highest = 4294967295

			## The lowest value representable by a [U32], which is `0`.
			## ```roc
			## expect U32.lowest == 0
			## ```
			lowest : U32
			lowest = 0

			## Convert a [U32] to its decimal string representation.
			## ```roc
			## expect U32.to_str(42) == "42"
			## ```
			to_str : U32 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U32.is_zero(0)
			##
			## expect !U32.is_zero(7)
			## ```
			is_zero : U32 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U32.is_eq(3, 3)
			##
			## expect !U32.is_eq(3, 4)
			## ```
			is_eq : U32, U32 -> Bool

			## Feed a [U32] into a [Hasher].
			to_hash : U32, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_u32(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U32.is_gt(5, 3)
			##
			## expect !U32.is_gt(3, 3)
			## ```
			is_gt : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U32.is_gte(3, 3)
			##
			## expect !U32.is_gte(2, 3)
			## ```
			is_gte : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U32.is_lt(3, 5)
			##
			## expect !U32.is_lt(3, 3)
			## ```
			is_lt : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U32.is_lte(3, 3)
			##
			## expect !U32.is_lte(5, 3)
			## ```
			is_lte : U32, U32 -> Bool

			## Compare two [U32] values and return their ordering.
			## ```roc
			## expect U32.compare(1, 2) == LT
			##
			## expect U32.compare(2, 2) == EQ
			##
			## expect U32.compare(3, 2) == GT
			## ```
			compare : U32, U32 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect U32.is_even(4)
			##
			## expect !U32.is_even(5)
			## ```
			is_even : U32 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect U32.is_odd(5)
			##
			## expect !U32.is_odd(4)
			## ```
			is_odd : U32 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect U32.is_multiple_of(12, 3)
			##
			## expect U32.is_multiple_of(0, 0)
			##
			## expect !U32.is_multiple_of(5, 0)
			## ```
			is_multiple_of : U32, U32 -> Bool
			is_multiple_of = |value, divisor| unsigned_is_multiple_of(0, value, divisor)

			## Returns the greater of two [U32] values.
			## ```roc
			## expect U32.max(5, 3) == 5
			## ```
			max : U32, U32 -> U32
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U32] values.
			## ```roc
			## expect U32.min(5, 3) == 3
			## ```
			min : U32, U32 -> U32
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U32] values.
			## ```roc
			## expect U32.plus(2, 3) == 5
			## ```
			plus : U32, U32 -> U32

			## Add two [U32] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in a [U32].
			add_try : U32, U32 -> Try(U32, [Overflow, ..])
			add_try = |a, b| unsigned_add_try(U32.highest, a, b)

			steps_between : U32, U32 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end).to_u64())
				else
					Known(0)

			## Add two [U32] values, saturating at [U32.highest] on overflow rather than wrapping around.
			## ```roc
			## expect U32.plus_saturated(U32.highest, 1) == U32.highest
			##
			## expect U32.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : U32, U32 -> U32
			plus_saturated = |a, b|
				if b > highest - a
					highest
				else
					a + b

			## Subtract the second [U32] from the first.
			## ```roc
			## expect U32.minus(5, 3) == 2
			## ```
			minus : U32, U32 -> U32

			## Subtract the second [U32] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in a [U32].
			sub_try : U32, U32 -> Try(U32, [Overflow, ..])
			sub_try = |a, b| unsigned_sub_try(a, b)

			## Subtract the second [U32] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect U32.minus_saturated(0, 1) == 0
			##
			## expect U32.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : U32, U32 -> U32
			minus_saturated = |a, b| unsigned_minus_saturated(0, a, b)

			## Multiply two [U32] values.
			## ```roc
			## expect U32.times(4, 3) == 12
			## ```
			times : U32, U32 -> U32

			## Multiply two [U32] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in a [U32].
			mul_try : U32, U32 -> Try(U32, [Overflow, ..])
			mul_try = |a, b| unsigned_mul_try(U32.highest, 0, a, b)

			## Multiply two [U32] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect U32.times_saturated(U32.highest, 2) == U32.highest
			##
			## expect U32.times_saturated(4, 3) == 12
			## ```
			times_saturated : U32, U32 -> U32
			times_saturated = |a, b| unsigned_times_saturated(U32.highest, 0, a, b)

			## Raise the first [U32] value to the power of the second.
			## Crashes if the exact result does not fit in [U32].
			## ```roc
			## expect U32.pow(2, 3) == 8
			##
			## expect U32.pow(5, 0) == 1
			## ```
			pow : U32, U32 -> U32
			pow = |base, exponent|
				match U32.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
				}

			## Raise the first [U32] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect U32.pow_try(2, 3) == Ok(8)
			##
			## expect U32.pow_try(U32.highest, 2) == Err(Overflow)
			## ```
			pow_try : U32, U32 -> Try(U32, [Overflow, ..])
			pow_try = |base, exponent| unsigned_pow_try(U32.highest, 0, 1, 2, base, exponent)

			## Divide the first [U32] by the second, discarding any remainder. Crashes if the second [U32] is zero.
			## ```roc
			## expect U32.div_by(10, 2) == 5
			##
			## expect U32.div_by(11, 2) == 5
			## ```
			div_by : U32, U32 -> U32

			## Divide the first [U32] by the second, returning `Err(DivByZero)`
			## instead of crashing if the divisor is zero.
			div_try : U32, U32 -> Try(U32, [DivByZero, ..])
			div_try = |a, b| unsigned_div_try(0, a, b)

			## Divide the first [U32] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [U32].
			## ```roc
			## expect U32.div_ceil_by(7, 2) == 4
			##
			## expect U32.div_ceil_by(8, 2) == 4
			## ```
			div_ceil_by : U32, U32 -> U32
			div_ceil_by = |a, b|
				match U32.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
				}

			## Divide the first [U32] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect U32.div_ceil_try(7, 2) == Ok(4)
			##
			## expect U32.div_ceil_try(1, 0) == Err(DivByZero)
			## ```
			div_ceil_try : U32, U32 -> Try(U32, [DivByZero, ..])
			div_ceil_try = |a, b| unsigned_div_ceil_try(0, 1, a, b)

			## Divide the first [U32] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [U32.div_by].
			## ```roc
			## expect U32.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U32, U32 -> U32

			## Return the remainder of dividing the first [U32] by the second.
			## ```roc
			## expect U32.rem_by(7, 3) == 1
			## ```
			rem_by : U32, U32 -> U32

			## Return the modulus of the first [U32] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [U32.rem_by].
			## ```roc
			## expect U32.mod_by(7, 3) == 1
			## ```
			mod_by : U32, U32 -> U32

			## Return the absolute difference between two [U32] values.
			## ```roc
			## expect U32.abs_diff(2, 5) == 3
			##
			## expect U32.abs_diff(5, 2) == 3
			## ```
			abs_diff : U32, U32 -> U32

			## Shift the bits of a [U32] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 4294967296).
			## ```roc
			## expect U32.shift_left_by(1, 3) == 8
			##
			## expect U32.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U32, U8 -> U32

			## Shift the bits of a [U32] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [U32.shift_right_zf_by].
			## ```roc
			## expect U32.shift_right_by(32, 2) == 8
			##
			## expect U32.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U32, U8 -> U32

			## Shift the bits of a [U32] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [U32.shift_right_by].
			## ```roc
			## expect U32.shift_right_zf_by(32, 2) == 8
			##
			## expect U32.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U32, U8 -> U32

			## Returns the bitwise AND of two [U32] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect U32.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : U32, U32 -> U32

			## Returns the bitwise OR of two [U32] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect U32.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : U32, U32 -> U32

			## Returns the bitwise XOR of two [U32] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect U32.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : U32, U32 -> U32

			## Returns the bitwise NOT of a [U32] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`.
			## ```roc
			## expect U32.bitwise_not(0) == 4294967295
			## ```
			bitwise_not : U32 -> U32

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect U32.count_leading_zero_bits(1) == 31
			##
			## expect U32.count_leading_zero_bits(0) == 32
			## ```
			count_leading_zero_bits : U32 -> U8
			count_leading_zero_bits = |value| unsigned_count_leading_zero_bits(32, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect U32.count_trailing_zero_bits(8) == 3
			##
			## expect U32.count_trailing_zero_bits(0) == 32
			## ```
			count_trailing_zero_bits : U32 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(32, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect U32.count_one_bits(0b1011) == 3
			##
			## expect U32.count_one_bits(0) == 0
			## ```
			count_one_bits : U32 -> U8
			count_one_bits = |value| unsigned_count_one_bits(0, 2, value)

			## Iterator of integers beginning with this `U32` and ending with the other `U32`.
			## (Use [U32.until] instead to end with the other `U32` minus one.)
			## Returns an empty iterator if this `U32` is greater than the other.
			## ```roc
			## expect Iter.fold(U32.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(U32.to(3, 3), [], |acc, item| acc.append(item)) == [3]
			##
			## expect Iter.fold(U32.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : U32, U32 -> Iter(U32)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					Known(U32.to_u64(end) - U32.to_u64(start) + 1)
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match U32.add_try(start, 1) {
									Ok(next) => if next <= end {
										U32.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `U32` and ending with the other `U32` minus one.
			## (Use [U32.to] instead to end with the other `U32` exactly, instead of minus one.)
			## Returns an empty iterator if this `U32` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(U32.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(U32.until(3, 3), [], |acc, item| acc.append(item)) == []
			##
			## expect Iter.fold(U32.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : U32, U32 -> Iter(U32)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(U32.to_u64(end) - U32.to_u64(start))
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match U32.add_try(start, 1) {
									Ok(next) => if next < end {
										U32.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build a [U32] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U32] (`0` to `4294967295`), or if any element is not a valid digit.
			## ```roc
			## expect U32.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U32, [OutOfRange])
			from_int_digits = |digits| u32_from_int_digits(digits)

			## Convert a numeric literal into a [U32]. This is the hook the
			## compiler uses when a literal is given type [U32]; most code should
			## parse user text with [U32.from_str] instead.
			from_numeral : Numeral -> Try(U32, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| u32_from_str(str))

			## Parse a [U32] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U32] (`0` to `4294967295`).
			## ```roc
			## expect U32.from_str("42") == Ok(42)
			##
			## expect U32.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U32, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U32] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U32.to_i8_wrap(42) == 42
			##
			## expect U32.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U32 -> I8

			## Convert a [U32] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_i8_try(42) == Ok(42)
			##
			## expect U32.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U32 -> Try(I8, [OutOfRange, ..])

			## Convert a [U32] to an [I16], wrapping on overflow. Values from `0` to
			## `32767` are preserved; larger values wrap by truncating to the low 16
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U32.to_i16_wrap(42) == 42
			##
			## expect U32.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U32 -> I16

			## Convert a [U32] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_i16_try(42) == Ok(42)
			##
			## expect U32.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U32 -> Try(I16, [OutOfRange, ..])

			## Convert a [U32] to an [I32], wrapping on overflow. Values from `0` to
			## `2147483647` are preserved; values from `2147483648` to `4294967295`
			## wrap into the negative range `-2147483648` to `-1` (two's complement
			## reinterpretation of the bits).
			## ```roc
			## expect U32.to_i32_wrap(42) == 42
			##
			## expect U32.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : U32 -> I32

			## Convert a [U32] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `2147483647` succeed; values from
			## `2147483648` to `4294967295` return `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_i32_try(42) == Ok(42)
			##
			## expect U32.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : U32 -> Try(I32, [OutOfRange, ..])

			## Convert a [U32] to an [I64]. This widening conversion preserves
			## every [U32] value exactly.
			to_i64 : U32 -> I64

			## Convert a [U32] to an [I128]. This widening conversion preserves
			## every [U32] value exactly.
			to_i128 : U32 -> I128

			# Conversions to unsigned integers

			## Convert a [U32] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U32.to_u8_wrap(42) == 42
			##
			## expect U32.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U32 -> U8

			## Convert a [U32] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_u8_try(42) == Ok(42)
			##
			## expect U32.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U32 -> Try(U8, [OutOfRange, ..])

			## Convert a [U32] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; larger values wrap by truncating to the low 16
			## bits.
			## ```roc
			## expect U32.to_u16_wrap(42) == 42
			##
			## expect U32.to_u16_wrap(70000) == 4464
			## ```
			to_u16_wrap : U32 -> U16

			## Convert a [U32] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_u16_try(42) == Ok(42)
			##
			## expect U32.to_u16_try(70000) == Err(OutOfRange)
			## ```
			to_u16_try : U32 -> Try(U16, [OutOfRange, ..])

			## Convert a [U32] to a [U64]. This widening conversion preserves
			## every [U32] value exactly.
			to_u64 : U32 -> U64

			## Convert a [U32] to a [U128]. This widening conversion preserves
			## every [U32] value exactly.
			to_u128 : U32 -> U128

			# Conversions to floating point (all safe)
			## Convert a [U32] to an [F32]. This conversion may round because
			## [F32] has fewer integer precision bits than [U32].
			to_f32 : U32 -> F32

			## Convert a [U32] to an [F64]. Every [U32] value is exactly
			## representable as an [F64].
			to_f64 : U32 -> F64

			## Convert a [U32] to a [Dec]. Every [U32] value is exactly
			## representable as a [Dec].
			to_dec : U32 -> Dec

			# Encode a U32 using a format that provides encode_u32
			encode : U32, fmt -> Try(encoded, err)
				where [fmt.encode_u32 : fmt, U32 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u32(self)
			}

			decode : src, fmt -> (Try(U32, err), src)
				where [fmt.decode_u32 : fmt, src -> (Try(U32, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u32(format, source)
			}
		}

		I32 :: [].{

			## Returns the default [I32] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I32.default() == 0
			## ```
			default : () -> I32
			default = || 0

			## The highest value representable by an [I32], which is `2147483647`.
			## ```roc
			## expect I32.highest == 2147483647
			## ```
			highest : I32
			highest = 2147483647

			## The lowest value representable by an [I32], which is `-2147483648`.
			## ```roc
			## expect I32.lowest == -2147483648
			## ```
			lowest : I32
			lowest = -2147483648

			## Convert an [I32] to its decimal string representation.
			## ```roc
			## expect I32.to_str(42) == "42"
			##
			## expect I32.to_str(-42) == "-42"
			## ```
			to_str : I32 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I32.is_zero(0)
			##
			## expect !I32.is_zero(7)
			## ```
			is_zero : I32 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I32.is_negative(-3)
			##
			## expect !I32.is_negative(0)
			## ```
			is_negative : I32 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I32.is_positive(3)
			##
			## expect !I32.is_positive(0)
			## ```
			is_positive : I32 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I32.is_eq(3, 3)
			##
			## expect !I32.is_eq(3, 4)
			## ```
			is_eq : I32, I32 -> Bool

			## Feed an [I32] into a [Hasher].
			to_hash : I32, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_i32(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I32.is_gt(5, 3)
			##
			## expect !I32.is_gt(3, 3)
			## ```
			is_gt : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I32.is_gte(3, 3)
			##
			## expect !I32.is_gte(2, 3)
			## ```
			is_gte : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I32.is_lt(3, 5)
			##
			## expect !I32.is_lt(3, 3)
			## ```
			is_lt : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I32.is_lte(3, 3)
			##
			## expect !I32.is_lte(5, 3)
			## ```
			is_lte : I32, I32 -> Bool

			## Compare two [I32] values and return their ordering.
			## ```roc
			## expect I32.compare(1, 2) == LT
			##
			## expect I32.compare(2, 2) == EQ
			##
			## expect I32.compare(3, 2) == GT
			## ```
			compare : I32, I32 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect I32.is_even(4)
			##
			## expect !I32.is_even(5)
			## ```
			is_even : I32 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect I32.is_odd(5)
			##
			## expect !I32.is_odd(4)
			## ```
			is_odd : I32 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect I32.is_multiple_of(12, 3)
			##
			## expect I32.is_multiple_of(0, 0)
			##
			## expect !I32.is_multiple_of(5, 0)
			##
			## expect I32.is_multiple_of(I32.lowest, -1)
			## ```
			is_multiple_of : I32, I32 -> Bool
			is_multiple_of = |value, divisor| signed_is_multiple_of(0, -1, value, divisor)

			## Returns the greater of two [I32] values.
			## ```roc
			## expect I32.max(5, 3) == 5
			##
			## expect I32.max(-3, -1) == -1
			## ```
			max : I32, I32 -> I32
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I32] values.
			## ```roc
			## expect I32.min(5, 3) == 3
			##
			## expect I32.min(-3, -1) == -3
			## ```
			min : I32, I32 -> I32
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I32]. Crashes on `-2147483648`, since `2147483648` does not fit in an [I32].
			## ```roc
			## expect I32.negate(3) == -3
			##
			## expect I32.negate(-3) == 3
			## ```
			negate : I32 -> I32

			## Return the absolute value of an [I32]. Crashes on `-2147483648`, since
			## `2147483648` does not fit in an [I32].
			## ```roc
			## expect I32.abs(3) == 3
			##
			## expect I32.abs(-3) == 3
			## ```
			abs : I32 -> I32

			## Add two [I32] values.
			## ```roc
			## expect I32.plus(2, 3) == 5
			## ```
			plus : I32, I32 -> I32

			## Add two [I32] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in an [I32].
			add_try : I32, I32 -> Try(I32, [Overflow, ..])
			add_try = |a, b| signed_add_try(I32.lowest, I32.highest, 0, a, b)

			steps_between : I32, I32 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end).to_u64())
				else
					Known(0)

			## Add two [I32] values, saturating at [I32.highest] or [I32.lowest] on overflow rather than wrapping around.
			## ```roc
			## expect I32.plus_saturated(I32.highest, 1) == I32.highest
			##
			## expect I32.plus_saturated(I32.lowest, -1) == I32.lowest
			##
			## expect I32.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : I32, I32 -> I32
			plus_saturated = |a, b|
				if b > 0 and a > highest - b
					highest
				else if b < 0 and a < lowest - b
					lowest
				else
					a + b

			## Subtract the second [I32] from the first.
			## ```roc
			## expect I32.minus(5, 3) == 2
			## ```
			minus : I32, I32 -> I32

			## Subtract the second [I32] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in an [I32].
			sub_try : I32, I32 -> Try(I32, [Overflow, ..])
			sub_try = |a, b| signed_sub_try(I32.lowest, I32.highest, 0, a, b)

			## Subtract the second [I32] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect I32.minus_saturated(I32.lowest, 1) == I32.lowest
			##
			## expect I32.minus_saturated(I32.highest, -1) == I32.highest
			##
			## expect I32.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : I32, I32 -> I32
			minus_saturated = |a, b| signed_minus_saturated(I32.lowest, I32.highest, 0, a, b)

			## Multiply two [I32] values.
			## ```roc
			## expect I32.times(4, 3) == 12
			## ```
			times : I32, I32 -> I32

			## Multiply two [I32] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in an [I32].
			mul_try : I32, I32 -> Try(I32, [Overflow, ..])
			mul_try = |a, b| signed_mul_try(I32.lowest, I32.highest, 0, -1, a, b)

			## Multiply two [I32] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect I32.times_saturated(I32.highest, 2) == I32.highest
			##
			## expect I32.times_saturated(I32.lowest, 2) == I32.lowest
			##
			## expect I32.times_saturated(4, 3) == 12
			## ```
			times_saturated : I32, I32 -> I32
			times_saturated = |a, b| signed_times_saturated(I32.lowest, I32.highest, 0, -1, a, b)

			## Raise the first [I32] value to the power of the second.
			## Crashes if the exact result does not fit in [I32].
			## ```roc
			## expect I32.pow(2, 3) == 8
			##
			## expect I32.pow(5, 0) == 1
			## ```
			pow : I32, I32 -> I32
			pow = |base, exponent|
				match I32.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
					Err(Underflow) => {
						crash "integer exponentiation underflowed"
					}
				}

			## Raise the first [I32] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect I32.pow_try(2, 3) == Ok(8)
			##
			## expect I32.pow_try(I32.highest, 2) == Err(Overflow)
			##
			## expect I32.pow_try(2, -1) == Err(Underflow)
			##
			## expect I32.pow_try(-1, -3) == Ok(-1)
			## ```
			pow_try : I32, I32 -> Try(I32, [Overflow, Underflow, ..])
			pow_try = |base, exponent| signed_pow_try(I32.lowest, I32.highest, 0, 1, 2, -1, base, exponent)

			## Divide the first [I32] by the second, discarding any remainder. Crashes if the second [I32] is zero.
			## ```roc
			## expect I32.div_by(10, 2) == 5
			##
			## expect I32.div_by(11, 2) == 5
			## ```
			div_by : I32, I32 -> I32

			## Divide the first [I32] by the second. Returns `Err(DivByZero)` if
			## the divisor is zero, or `Err(Overflow)` for `I32.lowest / -1`.
			div_try : I32, I32 -> Try(I32, [DivByZero, Overflow, ..])
			div_try = |a, b| signed_div_try(I32.lowest, 0, -1, a, b)

			## Divide the first [I32] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [I32].
			## ```roc
			## expect I32.div_ceil_by(7, 2) == 4
			##
			## expect I32.div_ceil_by(8, 2) == 4
			##
			## expect I32.div_ceil_by(-7, 2) == -3
			##
			## expect I32.div_ceil_by(-7, -2) == 4
			## ```
			div_ceil_by : I32, I32 -> I32
			div_ceil_by = |a, b|
				match I32.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
					Err(Overflow) => {
						crash "integer ceiling division overflowed"
					}
				}

			## Divide the first [I32] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect I32.div_ceil_try(7, 2) == Ok(4)
			##
			## expect I32.div_ceil_try(1, 0) == Err(DivByZero)
			##
			## expect I32.div_ceil_try(I32.lowest, -1) == Err(Overflow)
			## ```
			div_ceil_try : I32, I32 -> Try(I32, [DivByZero, Overflow, ..])
			div_ceil_try = |a, b| signed_div_ceil_try(I32.lowest, I32.highest, 0, 1, -1, a, b)

			## Divide the first [I32] by the second, truncating toward zero.
			## ```roc
			## expect I32.div_trunc_by(7, 2) == 3
			##
			## expect I32.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I32, I32 -> I32

			## Return the remainder of dividing the first [I32] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I32.rem_by(7, 3) == 1
			##
			## expect I32.rem_by(-7, 3) == -1
			## ```
			rem_by : I32, I32 -> I32

			## Return the modulus of the first [I32] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [I32.rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I32.mod_by(7, 3) == 1
			##
			## expect I32.mod_by(-7, 3) == 2
			## ```
			mod_by : I32, I32 -> I32

			## Return the absolute difference between two [I32] values as a [U32].
			## The result is a [U32] because the difference between two [I32] values
			## can be as large as `4294967295`, which does not fit in an [I32].
			## ```roc
			## expect I32.abs_diff(2, 5) == 3
			##
			## expect I32.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I32, I32 -> U32

			## Shift the bits of an [I32] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I32.shift_left_by(1, 3) == 8
			##
			## expect I32.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I32, U8 -> I32

			## Shift the bits of an [I32] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I32.shift_right_by(32, 2) == 8
			##
			## expect I32.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I32, U8 -> I32

			## Shift the bits of an [I32] to the right by the given number of
			## positions.
			## ```roc
			## expect I32.shift_right_zf_by(32, 2) == 8
			##
			## expect I32.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I32, U8 -> I32

			## Returns the bitwise AND of two [I32] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect I32.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : I32, I32 -> I32

			## Returns the bitwise OR of two [I32] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect I32.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : I32, I32 -> I32

			## Returns the bitwise XOR of two [I32] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect I32.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : I32, I32 -> I32

			## Returns the bitwise NOT of an [I32] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`. For signed integers
			## this is equivalent to `-value - 1`.
			## ```roc
			## expect I32.bitwise_not(5) == -6
			## ```
			bitwise_not : I32 -> I32

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect I32.count_leading_zero_bits(-1) == 0
			##
			## expect I32.count_leading_zero_bits(0) == 32
			## ```
			count_leading_zero_bits : I32 -> U8
			count_leading_zero_bits = |value| signed_count_leading_zero_bits(32, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect I32.count_trailing_zero_bits(-8) == 3
			##
			## expect I32.count_trailing_zero_bits(0) == 32
			## ```
			count_trailing_zero_bits : I32 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(32, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect I32.count_one_bits(-1) == 32
			##
			## expect I32.count_one_bits(0) == 0
			## ```
			count_one_bits : I32 -> U8
			count_one_bits = |value| signed_count_one_bits(32, 0, 2, value)

			## Iterator of integers beginning with this `I32` and ending with the other `I32`.
			## (Use [I32.until] instead to end with the other `I32` minus one.)
			## Returns an empty iterator if this `I32` is greater than the other.
			## ```roc
			## expect Iter.fold(I32.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(I32.to(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0, 1]
			##
			## expect Iter.fold(I32.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : I32, I32 -> Iter(I32)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					Known(I64.to_u64_wrap(I32.to_i64(end) - I32.to_i64(start) + 1))
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match I32.add_try(start, 1) {
									Ok(next) => if next <= end {
										I32.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `I32` and ending with the other `I32` minus one.
			## (Use [I32.to] instead to end with the other `I32` exactly, instead of minus one.)
			## Returns an empty iterator if this `I32` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(I32.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(I32.until(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0]
			##
			## expect Iter.fold(I32.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : I32, I32 -> Iter(I32)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(I64.to_u64_wrap(I32.to_i64(end) - I32.to_i64(start)))
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match I32.add_try(start, 1) {
									Ok(next) => if next < end {
										I32.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build an [I32] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I32] (`-2147483648` to `2147483647`), or if any element is not a valid digit.
			## The result is always non-negative; to build a negative value, [I32.negate]
			## the result.
			## ```roc
			## expect I32.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I32, [OutOfRange])
			from_int_digits = |digits| i32_from_int_digits(digits)

			## Convert a numeric literal into an [I32]. This is the hook the
			## compiler uses when a literal is given type [I32]; most code should
			## parse user text with [I32.from_str] instead.
			from_numeral : Numeral -> Try(I32, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| i32_from_str(str))

			## Parse an [I32] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I32] (`-2147483648` to `2147483647`).
			## ```roc
			## expect I32.from_str("42") == Ok(42)
			##
			## expect I32.from_str("-1") == Ok(-1)
			##
			## expect I32.from_str("3000000000") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(I32, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I32] to an [I8], wrapping on overflow. Values from `-128`
			## to `127` are preserved; other values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect I32.to_i8_wrap(42) == 42
			##
			## expect I32.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I32 -> I8

			## Convert an [I32] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_i8_try(42) == Ok(42)
			##
			## expect I32.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I32 -> Try(I8, [OutOfRange, ..])

			## Convert an [I32] to an [I16], wrapping on overflow. Values from
			## `-32768` to `32767` are preserved; other values wrap by truncating
			## to the low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I32.to_i16_wrap(42) == 42
			##
			## expect I32.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : I32 -> I16

			## Convert an [I32] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_i16_try(42) == Ok(42)
			##
			## expect I32.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : I32 -> Try(I16, [OutOfRange, ..])

			## Convert an [I32] to an [I64]. This widening conversion preserves
			## every [I32] value exactly.
			to_i64 : I32 -> I64

			## Convert an [I32] to an [I128]. This widening conversion preserves
			## every [I32] value exactly.
			to_i128 : I32 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I32] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I32.to_u8_wrap(42) == 42
			##
			## expect I32.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I32 -> U8

			## Convert an [I32] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u8_try(42) == Ok(42)
			##
			## expect I32.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I32 -> Try(U8, [OutOfRange, ..])

			## Convert an [I32] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; other values wrap by truncating to the low 16
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I32.to_u16_wrap(42) == 42
			##
			## expect I32.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I32 -> U16

			## Convert an [I32] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u16_try(42) == Ok(42)
			##
			## expect I32.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I32 -> Try(U16, [OutOfRange, ..])

			## Convert an [I32] to a [U32], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of the
			## [U32] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I32.to_u32_wrap(42) == 42
			##
			## expect I32.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I32 -> U32

			## Convert an [I32] to a [U32], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u32_try(42) == Ok(42)
			##
			## expect I32.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I32 -> Try(U32, [OutOfRange, ..])

			## Convert an [I32] to a [U64], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U64] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I32.to_u64_wrap(42) == 42
			##
			## expect I32.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I32 -> U64

			## Convert an [I32] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u64_try(42) == Ok(42)
			##
			## expect I32.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I32 -> Try(U64, [OutOfRange, ..])

			## Convert an [I32] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I32.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I32 -> U128

			## Convert an [I32] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u128_try(42) == Ok(42)
			##
			## expect I32.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I32 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			## Convert an [I32] to an [F32]. This conversion may round because
			## [F32] has fewer integer precision bits than [I32].
			to_f32 : I32 -> F32

			## Convert an [I32] to an [F64]. Every [I32] value is exactly
			## representable as an [F64].
			to_f64 : I32 -> F64

			## Convert an [I32] to a [Dec]. Every [I32] value is exactly
			## representable as a [Dec].
			to_dec : I32 -> Dec

			# Encode an I32 using a format that provides encode_i32
			encode : I32, fmt -> Try(encoded, err)
				where [fmt.encode_i32 : fmt, I32 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i32(self)
			}

			# Decode an I32 using a format that provides decode_i32
			decode : src, fmt -> (Try(I32, err), src)
				where [fmt.decode_i32 : fmt, src -> (Try(I32, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i32(format, source)
			}
		}

		U64 :: [].{

			## Returns the default [U64] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U64.default() == 0
			## ```
			default : () -> U64
			default = || 0

			## The highest value representable by a [U64], which is
			## `18446744073709551615`.
			## ```roc
			## expect U64.highest == 18446744073709551615
			## ```
			highest : U64
			highest = 18446744073709551615

			## The lowest value representable by a [U64], which is `0`.
			## ```roc
			## expect U64.lowest == 0
			## ```
			lowest : U64
			lowest = 0

			## Convert a [U64] to its decimal string representation.
			## ```roc
			## expect U64.to_str(42) == "42"
			## ```
			to_str : U64 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U64.is_zero(0)
			##
			## expect !U64.is_zero(7)
			## ```
			is_zero : U64 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U64.is_eq(3, 3)
			##
			## expect !U64.is_eq(3, 4)
			## ```
			is_eq : U64, U64 -> Bool

			## Feed a [U64] into a [Hasher].
			to_hash : U64, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_u64(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U64.is_gt(5, 3)
			##
			## expect !U64.is_gt(3, 3)
			## ```
			is_gt : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U64.is_gte(3, 3)
			##
			## expect !U64.is_gte(2, 3)
			## ```
			is_gte : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U64.is_lt(3, 5)
			##
			## expect !U64.is_lt(3, 3)
			## ```
			is_lt : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U64.is_lte(3, 3)
			##
			## expect !U64.is_lte(5, 3)
			## ```
			is_lte : U64, U64 -> Bool

			## Compare two [U64] values and return their ordering.
			## ```roc
			## expect U64.compare(1, 2) == LT
			##
			## expect U64.compare(2, 2) == EQ
			##
			## expect U64.compare(3, 2) == GT
			## ```
			compare : U64, U64 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect U64.is_even(4)
			##
			## expect !U64.is_even(5)
			## ```
			is_even : U64 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect U64.is_odd(5)
			##
			## expect !U64.is_odd(4)
			## ```
			is_odd : U64 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect U64.is_multiple_of(12, 3)
			##
			## expect U64.is_multiple_of(0, 0)
			##
			## expect !U64.is_multiple_of(5, 0)
			## ```
			is_multiple_of : U64, U64 -> Bool
			is_multiple_of = |value, divisor| unsigned_is_multiple_of(0, value, divisor)

			## Returns the greater of two [U64] values.
			## ```roc
			## expect U64.max(5, 3) == 5
			## ```
			max : U64, U64 -> U64
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U64] values.
			## ```roc
			## expect U64.min(5, 3) == 3
			## ```
			min : U64, U64 -> U64
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U64] values.
			## ```roc
			## expect U64.plus(2, 3) == 5
			## ```
			plus : U64, U64 -> U64

			## Add two [U64] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in a [U64].
			add_try : U64, U64 -> Try(U64, [Overflow, ..])
			add_try = |a, b| unsigned_add_try(U64.highest, a, b)

			steps_between : U64, U64 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end))
				else
					Known(0)

			## Add two [U64] values, saturating at [U64.highest] on overflow rather than wrapping around.
			## ```roc
			## expect U64.plus_saturated(U64.highest, 1) == U64.highest
			##
			## expect U64.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : U64, U64 -> U64
			plus_saturated = |a, b|
				if b > highest - a
					highest
				else
					a + b

			## Subtract the second [U64] from the first.
			## ```roc
			## expect U64.minus(5, 3) == 2
			## ```
			minus : U64, U64 -> U64

			## Subtract the second [U64] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in a [U64].
			sub_try : U64, U64 -> Try(U64, [Overflow, ..])
			sub_try = |a, b| unsigned_sub_try(a, b)

			## Subtract the second [U64] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect U64.minus_saturated(0, 1) == 0
			##
			## expect U64.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : U64, U64 -> U64
			minus_saturated = |a, b| unsigned_minus_saturated(0, a, b)

			## Multiply two [U64] values.
			## ```roc
			## expect U64.times(4, 3) == 12
			## ```
			times : U64, U64 -> U64

			## Multiply two [U64] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in a [U64].
			mul_try : U64, U64 -> Try(U64, [Overflow, ..])
			mul_try = |a, b| unsigned_mul_try(U64.highest, 0, a, b)

			## Multiply two [U64] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect U64.times_saturated(U64.highest, 2) == U64.highest
			##
			## expect U64.times_saturated(4, 3) == 12
			## ```
			times_saturated : U64, U64 -> U64
			times_saturated = |a, b| unsigned_times_saturated(U64.highest, 0, a, b)

			## Raise the first [U64] value to the power of the second.
			## Crashes if the exact result does not fit in [U64].
			## ```roc
			## expect U64.pow(2, 3) == 8
			##
			## expect U64.pow(5, 0) == 1
			## ```
			pow : U64, U64 -> U64
			pow = |base, exponent|
				match U64.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
				}

			## Raise the first [U64] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect U64.pow_try(2, 3) == Ok(8)
			##
			## expect U64.pow_try(U64.highest, 2) == Err(Overflow)
			## ```
			pow_try : U64, U64 -> Try(U64, [Overflow, ..])
			pow_try = |base, exponent| unsigned_pow_try(U64.highest, 0, 1, 2, base, exponent)

			## Divide the first [U64] by the second, discarding any remainder. Crashes if the second [U64] is zero.
			## ```roc
			## expect U64.div_by(10, 2) == 5
			##
			## expect U64.div_by(11, 2) == 5
			## ```
			div_by : U64, U64 -> U64

			## Divide the first [U64] by the second, returning `Err(DivByZero)`
			## instead of crashing if the divisor is zero.
			div_try : U64, U64 -> Try(U64, [DivByZero, ..])
			div_try = |a, b| unsigned_div_try(0, a, b)

			## Divide the first [U64] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [U64].
			## ```roc
			## expect U64.div_ceil_by(7, 2) == 4
			##
			## expect U64.div_ceil_by(8, 2) == 4
			## ```
			div_ceil_by : U64, U64 -> U64
			div_ceil_by = |a, b|
				match U64.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
				}

			## Divide the first [U64] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect U64.div_ceil_try(7, 2) == Ok(4)
			##
			## expect U64.div_ceil_try(1, 0) == Err(DivByZero)
			## ```
			div_ceil_try : U64, U64 -> Try(U64, [DivByZero, ..])
			div_ceil_try = |a, b| unsigned_div_ceil_try(0, 1, a, b)

			## Divide the first [U64] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [U64.div_by].
			## ```roc
			## expect U64.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U64, U64 -> U64

			## Return the remainder of dividing the first [U64] by the second.
			## ```roc
			## expect U64.rem_by(7, 3) == 1
			## ```
			rem_by : U64, U64 -> U64

			## Return the modulus of the first [U64] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [U64.rem_by].
			## ```roc
			## expect U64.mod_by(7, 3) == 1
			## ```
			mod_by : U64, U64 -> U64

			## Return the absolute difference between two [U64] values.
			## ```roc
			## expect U64.abs_diff(2, 5) == 3
			##
			## expect U64.abs_diff(5, 2) == 3
			## ```
			abs_diff : U64, U64 -> U64

			## Shift the bits of a [U64] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 2^64).
			## ```roc
			## expect U64.shift_left_by(1, 3) == 8
			##
			## expect U64.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U64, U8 -> U64

			## Shift the bits of a [U64] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [U64.shift_right_zf_by].
			## ```roc
			## expect U64.shift_right_by(32, 2) == 8
			##
			## expect U64.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U64, U8 -> U64

			## Shift the bits of a [U64] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [U64.shift_right_by].
			## ```roc
			## expect U64.shift_right_zf_by(32, 2) == 8
			##
			## expect U64.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U64, U8 -> U64

			## Returns the bitwise AND of two [U64] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect U64.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : U64, U64 -> U64

			## Returns the bitwise OR of two [U64] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect U64.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : U64, U64 -> U64

			## Returns the bitwise XOR of two [U64] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect U64.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : U64, U64 -> U64

			## Returns the bitwise NOT of a [U64] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`.
			## ```roc
			## expect U64.bitwise_not(0) == 18446744073709551615
			## ```
			bitwise_not : U64 -> U64

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect U64.count_leading_zero_bits(1) == 63
			##
			## expect U64.count_leading_zero_bits(0) == 64
			## ```
			count_leading_zero_bits : U64 -> U8
			count_leading_zero_bits = |value| unsigned_count_leading_zero_bits(64, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect U64.count_trailing_zero_bits(8) == 3
			##
			## expect U64.count_trailing_zero_bits(0) == 64
			## ```
			count_trailing_zero_bits : U64 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(64, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect U64.count_one_bits(0b1011) == 3
			##
			## expect U64.count_one_bits(0) == 0
			## ```
			count_one_bits : U64 -> U8
			count_one_bits = |value| unsigned_count_one_bits(0, 2, value)

			## Iterator of integers beginning with this `U64` and ending with the other `U64`.
			## (Use [U64.until] instead to end with the other `U64` minus one.)
			## Returns an empty iterator if this `U64` is greater than the other.
			## ```roc
			## expect Iter.fold(U64.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(U64.to(3, 3), [], |acc, item| acc.append(item)) == [3]
			##
			## expect Iter.fold(U64.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : U64, U64 -> Iter(U64)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					match U64.add_try(end - start, 1) {
						Ok(len) => Known(len)
						Err(Overflow) => Unknown
					}
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match U64.add_try(start, 1) {
									Ok(next) => if next <= end {
										U64.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `U64` and ending with the other `U64` minus one.
			## (Use [U64.to] instead to end with the other `U64` exactly, instead of minus one.)
			## Returns an empty iterator if this `U64` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(U64.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(U64.until(3, 3), [], |acc, item| acc.append(item)) == []
			##
			## expect Iter.fold(U64.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : U64, U64 -> Iter(U64)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					Known(end - start)
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match U64.add_try(start, 1) {
									Ok(next) => if next < end {
										U64.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build a [U64] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U64] (`0` to `18446744073709551615`), or if any element is not a valid digit.
			## ```roc
			## expect U64.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U64, [OutOfRange])
			from_int_digits = |digits| u64_from_int_digits(digits)

			## Convert a numeric literal into a [U64]. This is the hook the
			## compiler uses when a literal is given type [U64]; most code should
			## parse user text with [U64.from_str] instead.
			from_numeral : Numeral -> Try(U64, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| u64_from_str(str))

			## Parse a [U64] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U64] (`0` to `18446744073709551615`).
			## ```roc
			## expect U64.from_str("42") == Ok(42)
			##
			## expect U64.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U64, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U64] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U64.to_i8_wrap(42) == 42
			##
			## expect U64.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U64 -> I8

			## Convert a [U64] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i8_try(42) == Ok(42)
			##
			## expect U64.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U64 -> Try(I8, [OutOfRange, ..])

			## Convert a [U64] to an [I16], wrapping on overflow. Values from `0` to
			## `32767` are preserved; larger values wrap by truncating to the low 16
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U64.to_i16_wrap(42) == 42
			##
			## expect U64.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U64 -> I16

			## Convert a [U64] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i16_try(42) == Ok(42)
			##
			## expect U64.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U64 -> Try(I16, [OutOfRange, ..])

			## Convert a [U64] to an [I32], wrapping on overflow. Values from `0` to
			## `2147483647` are preserved; larger values wrap by truncating to the
			## low 32 bits and reinterpreting them in two's complement.
			## ```roc
			## expect U64.to_i32_wrap(42) == 42
			##
			## expect U64.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : U64 -> I32

			## Convert a [U64] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `2147483647` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i32_try(42) == Ok(42)
			##
			## expect U64.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : U64 -> Try(I32, [OutOfRange, ..])

			## Convert a [U64] to an [I64], wrapping on overflow. Values from `0` to
			## `9223372036854775807` are preserved; values from `9223372036854775808`
			## to `18446744073709551615` wrap into the negative range
			## `-9223372036854775808` to `-1` (two's complement reinterpretation of
			## the bits).
			## ```roc
			## expect U64.to_i64_wrap(42) == 42
			##
			## expect U64.to_i64_wrap(10000000000000000000) == -8446744073709551616
			## ```
			to_i64_wrap : U64 -> I64

			## Convert a [U64] to an [I64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `9223372036854775807` succeed; larger
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i64_try(42) == Ok(42)
			##
			## expect U64.to_i64_try(10000000000000000000) == Err(OutOfRange)
			## ```
			to_i64_try : U64 -> Try(I64, [OutOfRange, ..])

			## Convert a [U64] to an [I128]. This widening conversion preserves
			## every [U64] value exactly.
			to_i128 : U64 -> I128

			# Conversions to unsigned integers

			## Convert a [U64] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U64.to_u8_wrap(42) == 42
			##
			## expect U64.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U64 -> U8

			## Convert a [U64] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_u8_try(42) == Ok(42)
			##
			## expect U64.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U64 -> Try(U8, [OutOfRange, ..])

			## Convert a [U64] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; larger values wrap by truncating to the low 16
			## bits.
			## ```roc
			## expect U64.to_u16_wrap(42) == 42
			##
			## expect U64.to_u16_wrap(70000) == 4464
			## ```
			to_u16_wrap : U64 -> U16

			## Convert a [U64] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_u16_try(42) == Ok(42)
			##
			## expect U64.to_u16_try(70000) == Err(OutOfRange)
			## ```
			to_u16_try : U64 -> Try(U16, [OutOfRange, ..])

			## Convert a [U64] to a [U32], wrapping on overflow. Values from `0` to
			## `4294967295` are preserved; larger values wrap by truncating to the
			## low 32 bits.
			## ```roc
			## expect U64.to_u32_wrap(42) == 42
			##
			## expect U64.to_u32_wrap(5000000000) == 705032704
			## ```
			to_u32_wrap : U64 -> U32

			## Convert a [U64] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_u32_try(42) == Ok(42)
			##
			## expect U64.to_u32_try(5000000000) == Err(OutOfRange)
			## ```
			to_u32_try : U64 -> Try(U32, [OutOfRange, ..])

			## Convert a [U64] to a [U128]. This widening conversion preserves
			## every [U64] value exactly.
			to_u128 : U64 -> U128

			# Conversions to floating point (all safe)
			## Convert a [U64] to an [F32]. This conversion may round because
			## [F32] has fewer integer precision bits than [U64].
			to_f32 : U64 -> F32

			## Convert a [U64] to an [F64]. This conversion may round because
			## [F64] has fewer integer precision bits than [U64].
			to_f64 : U64 -> F64

			## Convert a [U64] to a [Dec]. Every [U64] value is exactly
			## representable as a [Dec].
			to_dec : U64 -> Dec

			# Encode a U64 using a format that provides encode_u64
			encode : U64, fmt -> Try(encoded, err)
				where [fmt.encode_u64 : fmt, U64 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u64(self)
			}

			decode : src, fmt -> (Try(U64, err), src)
				where [fmt.decode_u64 : fmt, src -> (Try(U64, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u64(format, source)
			}
		}

		I64 :: [].{

			## Returns the default [I64] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I64.default() == 0
			## ```
			default : () -> I64
			default = || 0

			## The highest value representable by an [I64], which is
			## `9223372036854775807`.
			## ```roc
			## expect I64.highest == 9223372036854775807
			## ```
			highest : I64
			highest = 9223372036854775807

			## The lowest value representable by an [I64], which is
			## `-9223372036854775808`.
			## ```roc
			## expect I64.lowest == -9223372036854775808
			## ```
			lowest : I64
			lowest = -9223372036854775808

			## Convert an [I64] to its decimal string representation.
			## ```roc
			## expect I64.to_str(42) == "42"
			##
			## expect I64.to_str(-42) == "-42"
			## ```
			to_str : I64 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I64.is_zero(0)
			##
			## expect !I64.is_zero(7)
			## ```
			is_zero : I64 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I64.is_negative(-3)
			##
			## expect !I64.is_negative(0)
			## ```
			is_negative : I64 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I64.is_positive(3)
			##
			## expect !I64.is_positive(0)
			## ```
			is_positive : I64 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I64.is_eq(3, 3)
			##
			## expect !I64.is_eq(3, 4)
			## ```
			is_eq : I64, I64 -> Bool

			## Feed an [I64] into a [Hasher].
			to_hash : I64, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_i64(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I64.is_gt(5, 3)
			##
			## expect !I64.is_gt(3, 3)
			## ```
			is_gt : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I64.is_gte(3, 3)
			##
			## expect !I64.is_gte(2, 3)
			## ```
			is_gte : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I64.is_lt(3, 5)
			##
			## expect !I64.is_lt(3, 3)
			## ```
			is_lt : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I64.is_lte(3, 3)
			##
			## expect !I64.is_lte(5, 3)
			## ```
			is_lte : I64, I64 -> Bool

			## Compare two [I64] values and return their ordering.
			## ```roc
			## expect I64.compare(1, 2) == LT
			##
			## expect I64.compare(2, 2) == EQ
			##
			## expect I64.compare(3, 2) == GT
			## ```
			compare : I64, I64 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect I64.is_even(4)
			##
			## expect !I64.is_even(5)
			## ```
			is_even : I64 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect I64.is_odd(5)
			##
			## expect !I64.is_odd(4)
			## ```
			is_odd : I64 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect I64.is_multiple_of(12, 3)
			##
			## expect I64.is_multiple_of(0, 0)
			##
			## expect !I64.is_multiple_of(5, 0)
			##
			## expect I64.is_multiple_of(I64.lowest, -1)
			## ```
			is_multiple_of : I64, I64 -> Bool
			is_multiple_of = |value, divisor| signed_is_multiple_of(0, -1, value, divisor)

			## Returns the greater of two [I64] values.
			## ```roc
			## expect I64.max(5, 3) == 5
			##
			## expect I64.max(-3, -1) == -1
			## ```
			max : I64, I64 -> I64
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I64] values.
			## ```roc
			## expect I64.min(5, 3) == 3
			##
			## expect I64.min(-3, -1) == -3
			## ```
			min : I64, I64 -> I64
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I64]. Crashes on `-9223372036854775808`, since
			## `9223372036854775808` does not fit in an [I64].
			## ```roc
			## expect I64.negate(3) == -3
			##
			## expect I64.negate(-3) == 3
			## ```
			negate : I64 -> I64

			## Return the absolute value of an [I64]. Crashes on `-9223372036854775808`,
			## since `9223372036854775808` does not fit in an [I64].
			## ```roc
			## expect I64.abs(3) == 3
			##
			## expect I64.abs(-3) == 3
			## ```
			abs : I64 -> I64

			## Add two [I64] values.
			## ```roc
			## expect I64.plus(2, 3) == 5
			## ```
			plus : I64, I64 -> I64

			## Add two [I64] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in an [I64].
			add_try : I64, I64 -> Try(I64, [Overflow, ..])
			add_try = |a, b| signed_add_try(I64.lowest, I64.highest, 0, a, b)

			steps_between : I64, I64 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					Known(start.abs_diff(end))
				else
					Known(0)

			## Add two [I64] values, saturating at [I64.highest] or [I64.lowest] on overflow rather than wrapping around.
			## ```roc
			## expect I64.plus_saturated(I64.highest, 1) == I64.highest
			##
			## expect I64.plus_saturated(I64.lowest, -1) == I64.lowest
			##
			## expect I64.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : I64, I64 -> I64
			plus_saturated = |a, b|
				if b > 0 and a > highest - b
					highest
				else if b < 0 and a < lowest - b
					lowest
				else
					a + b

			## Subtract the second [I64] from the first.
			## ```roc
			## expect I64.minus(5, 3) == 2
			## ```
			minus : I64, I64 -> I64

			## Subtract the second [I64] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in an [I64].
			sub_try : I64, I64 -> Try(I64, [Overflow, ..])
			sub_try = |a, b| signed_sub_try(I64.lowest, I64.highest, 0, a, b)

			## Subtract the second [I64] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect I64.minus_saturated(I64.lowest, 1) == I64.lowest
			##
			## expect I64.minus_saturated(I64.highest, -1) == I64.highest
			##
			## expect I64.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : I64, I64 -> I64
			minus_saturated = |a, b| signed_minus_saturated(I64.lowest, I64.highest, 0, a, b)

			## Multiply two [I64] values.
			## ```roc
			## expect I64.times(4, 3) == 12
			## ```
			times : I64, I64 -> I64

			## Multiply two [I64] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in an [I64].
			mul_try : I64, I64 -> Try(I64, [Overflow, ..])
			mul_try = |a, b| signed_mul_try(I64.lowest, I64.highest, 0, -1, a, b)

			## Multiply two [I64] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect I64.times_saturated(I64.highest, 2) == I64.highest
			##
			## expect I64.times_saturated(I64.lowest, 2) == I64.lowest
			##
			## expect I64.times_saturated(4, 3) == 12
			## ```
			times_saturated : I64, I64 -> I64
			times_saturated = |a, b| signed_times_saturated(I64.lowest, I64.highest, 0, -1, a, b)

			## Raise the first [I64] value to the power of the second.
			## Crashes if the exact result does not fit in [I64].
			## ```roc
			## expect I64.pow(2, 3) == 8
			##
			## expect I64.pow(5, 0) == 1
			## ```
			pow : I64, I64 -> I64
			pow = |base, exponent|
				match I64.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
					Err(Underflow) => {
						crash "integer exponentiation underflowed"
					}
				}

			## Raise the first [I64] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect I64.pow_try(2, 3) == Ok(8)
			##
			## expect I64.pow_try(I64.highest, 2) == Err(Overflow)
			##
			## expect I64.pow_try(2, -1) == Err(Underflow)
			##
			## expect I64.pow_try(-1, -3) == Ok(-1)
			## ```
			pow_try : I64, I64 -> Try(I64, [Overflow, Underflow, ..])
			pow_try = |base, exponent| signed_pow_try(I64.lowest, I64.highest, 0, 1, 2, -1, base, exponent)

			## Divide the first [I64] by the second, discarding any remainder. Crashes if the second [I64] is zero.
			## ```roc
			## expect I64.div_by(10, 2) == 5
			##
			## expect I64.div_by(11, 2) == 5
			## ```
			div_by : I64, I64 -> I64

			## Divide the first [I64] by the second. Returns `Err(DivByZero)` if
			## the divisor is zero, or `Err(Overflow)` for `I64.lowest / -1`.
			div_try : I64, I64 -> Try(I64, [DivByZero, Overflow, ..])
			div_try = |a, b| signed_div_try(I64.lowest, 0, -1, a, b)

			## Divide the first [I64] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [I64].
			## ```roc
			## expect I64.div_ceil_by(7, 2) == 4
			##
			## expect I64.div_ceil_by(8, 2) == 4
			##
			## expect I64.div_ceil_by(-7, 2) == -3
			##
			## expect I64.div_ceil_by(-7, -2) == 4
			## ```
			div_ceil_by : I64, I64 -> I64
			div_ceil_by = |a, b|
				match I64.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
					Err(Overflow) => {
						crash "integer ceiling division overflowed"
					}
				}

			## Divide the first [I64] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect I64.div_ceil_try(7, 2) == Ok(4)
			##
			## expect I64.div_ceil_try(1, 0) == Err(DivByZero)
			##
			## expect I64.div_ceil_try(I64.lowest, -1) == Err(Overflow)
			## ```
			div_ceil_try : I64, I64 -> Try(I64, [DivByZero, Overflow, ..])
			div_ceil_try = |a, b| signed_div_ceil_try(I64.lowest, I64.highest, 0, 1, -1, a, b)

			## Divide the first [I64] by the second, truncating toward zero.
			## ```roc
			## expect I64.div_trunc_by(7, 2) == 3
			##
			## expect I64.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I64, I64 -> I64

			## Return the remainder of dividing the first [I64] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I64.rem_by(7, 3) == 1
			##
			## expect I64.rem_by(-7, 3) == -1
			## ```
			rem_by : I64, I64 -> I64

			## Return the modulus of the first [I64] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [I64.rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I64.mod_by(7, 3) == 1
			##
			## expect I64.mod_by(-7, 3) == 2
			## ```
			mod_by : I64, I64 -> I64

			## Return the absolute difference between two [I64] values as a [U64].
			## The result is a [U64] because the difference between two [I64] values
			## can be as large as `18446744073709551615`, which does not fit in an [I64].
			## ```roc
			## expect I64.abs_diff(2, 5) == 3
			##
			## expect I64.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I64, I64 -> U64

			## Shift the bits of an [I64] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I64.shift_left_by(1, 3) == 8
			##
			## expect I64.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I64, U8 -> I64

			## Shift the bits of an [I64] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I64.shift_right_by(32, 2) == 8
			##
			## expect I64.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I64, U8 -> I64

			## Shift the bits of an [I64] to the right by the given number of
			## positions.
			## ```roc
			## expect I64.shift_right_zf_by(32, 2) == 8
			##
			## expect I64.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I64, U8 -> I64

			## Returns the bitwise AND of two [I64] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect I64.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : I64, I64 -> I64

			## Returns the bitwise OR of two [I64] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect I64.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : I64, I64 -> I64

			## Returns the bitwise XOR of two [I64] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect I64.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : I64, I64 -> I64

			## Returns the bitwise NOT of an [I64] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`. For signed integers
			## this is equivalent to `-value - 1`.
			## ```roc
			## expect I64.bitwise_not(5) == -6
			## ```
			bitwise_not : I64 -> I64

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect I64.count_leading_zero_bits(-1) == 0
			##
			## expect I64.count_leading_zero_bits(0) == 64
			## ```
			count_leading_zero_bits : I64 -> U8
			count_leading_zero_bits = |value| signed_count_leading_zero_bits(64, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect I64.count_trailing_zero_bits(-8) == 3
			##
			## expect I64.count_trailing_zero_bits(0) == 64
			## ```
			count_trailing_zero_bits : I64 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(64, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect I64.count_one_bits(-1) == 64
			##
			## expect I64.count_one_bits(0) == 0
			## ```
			count_one_bits : I64 -> U8
			count_one_bits = |value| signed_count_one_bits(64, 0, 2, value)

			## Iterator of integers beginning with this `I64` and ending with the other `I64`.
			## (Use [I64.until] instead to end with the other `I64` minus one.)
			## Returns an empty iterator if this `I64` is greater than the other.
			## ```roc
			## expect Iter.fold(I64.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(I64.to(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0, 1]
			##
			## expect Iter.fold(I64.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : I64, I64 -> Iter(I64)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					match I64.sub_try(end, start) {
						Ok(diff) => match I64.add_try(diff, 1) {
							Ok(d1) => Known(I64.to_u64_wrap(d1))
							Err(Overflow) => Unknown
						}
						Err(Overflow) => Unknown
					}
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match I64.add_try(start, 1) {
									Ok(next) => if next <= end {
										I64.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `I64` and ending with the other `I64` minus one.
			## (Use [I64.to] instead to end with the other `I64` exactly, instead of minus one.)
			## Returns an empty iterator if this `I64` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(I64.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(I64.until(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0]
			##
			## expect Iter.fold(I64.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : I64, I64 -> Iter(I64)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					match I64.sub_try(end, start) {
						Ok(diff) => Known(I64.to_u64_wrap(diff))
						Err(Overflow) => Unknown
					}
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match I64.add_try(start, 1) {
									Ok(next) => if next < end {
										I64.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build an [I64] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I64] (`-9223372036854775808` to `9223372036854775807`), or if any
			## element is not a valid digit. The result is always non-negative; to
			## build a negative value, [I64.negate] the result.
			## ```roc
			## expect I64.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I64, [OutOfRange])
			from_int_digits = |digits| i64_from_int_digits(digits)

			## Convert a numeric literal into an [I64]. This is the hook the
			## compiler uses when a literal is given type [I64]; most code should
			## parse user text with [I64.from_str] instead.
			from_numeral : Numeral -> Try(I64, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| i64_from_str(str))

			## Parse an [I64] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I64] (`-9223372036854775808` to `9223372036854775807`).
			## ```roc
			## expect I64.from_str("42") == Ok(42)
			##
			## expect I64.from_str("-1") == Ok(-1)
			## ```
			from_str : Str -> Try(I64, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I64] to an [I8], wrapping on overflow. Values from `-128`
			## to `127` are preserved; other values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect I64.to_i8_wrap(42) == 42
			##
			## expect I64.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I64 -> I8

			## Convert an [I64] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_i8_try(42) == Ok(42)
			##
			## expect I64.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I64 -> Try(I8, [OutOfRange, ..])

			## Convert an [I64] to an [I16], wrapping on overflow. Values from
			## `-32768` to `32767` are preserved; other values wrap by truncating
			## to the low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I64.to_i16_wrap(42) == 42
			##
			## expect I64.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : I64 -> I16

			## Convert an [I64] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_i16_try(42) == Ok(42)
			##
			## expect I64.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : I64 -> Try(I16, [OutOfRange, ..])

			## Convert an [I64] to an [I32], wrapping on overflow. Values from
			## `-2147483648` to `2147483647` are preserved; other values wrap by
			## truncating to the low 32 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect I64.to_i32_wrap(42) == 42
			##
			## expect I64.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : I64 -> I32

			## Convert an [I64] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-2147483648` to `2147483647` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_i32_try(42) == Ok(42)
			##
			## expect I64.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : I64 -> Try(I32, [OutOfRange, ..])

			## Convert an [I64] to an [I128]. This widening conversion preserves
			## every [I64] value exactly.
			to_i128 : I64 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I64] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I64.to_u8_wrap(42) == 42
			##
			## expect I64.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I64 -> U8

			## Convert an [I64] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u8_try(42) == Ok(42)
			##
			## expect I64.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I64 -> Try(U8, [OutOfRange, ..])

			## Convert an [I64] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; other values wrap by truncating to the low 16
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I64.to_u16_wrap(42) == 42
			##
			## expect I64.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I64 -> U16

			## Convert an [I64] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u16_try(42) == Ok(42)
			##
			## expect I64.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I64 -> Try(U16, [OutOfRange, ..])

			## Convert an [I64] to a [U32], wrapping on overflow. Values from `0` to
			## `4294967295` are preserved; other values wrap by truncating to the
			## low 32 bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I64.to_u32_wrap(42) == 42
			##
			## expect I64.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I64 -> U32

			## Convert an [I64] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u32_try(42) == Ok(42)
			##
			## expect I64.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I64 -> Try(U32, [OutOfRange, ..])

			## Convert an [I64] to a [U64], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of the
			## [U64] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I64.to_u64_wrap(42) == 42
			##
			## expect I64.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I64 -> U64

			## Convert an [I64] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u64_try(42) == Ok(42)
			##
			## expect I64.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I64 -> Try(U64, [OutOfRange, ..])

			## Convert an [I64] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I64.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I64 -> U128

			## Convert an [I64] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u128_try(42) == Ok(42)
			##
			## expect I64.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I64 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			## Convert an [I64] to an [F32]. This conversion may round because
			## [F32] has fewer integer precision bits than [I64].
			to_f32 : I64 -> F32

			## Convert an [I64] to an [F64]. This conversion may round because
			## [F64] has fewer integer precision bits than [I64].
			to_f64 : I64 -> F64

			## Convert an [I64] to a [Dec]. Every [I64] value is exactly
			## representable as a [Dec].
			to_dec : I64 -> Dec

			# Encode an I64 using a format that provides encode_i64
			encode : I64, fmt -> Try(encoded, err)
				where [fmt.encode_i64 : fmt, I64 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i64(self)
			}

			decode : src, fmt -> (Try(I64, err), src)
				where [fmt.decode_i64 : fmt, src -> (Try(I64, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i64(format, source)
			}
		}

		U128 :: [].{

			## Returns the default [U128] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U128.default() == 0
			## ```
			default : () -> U128
			default = || 0

			## The highest value representable by a [U128], which is
			## `340282366920938463463374607431768211455`.
			## ```roc
			## expect U128.highest == 340282366920938463463374607431768211455
			## ```
			highest : U128
			highest = 340282366920938463463374607431768211455

			## The lowest value representable by a [U128], which is `0`.
			## ```roc
			## expect U128.lowest == 0
			## ```
			lowest : U128
			lowest = 0

			## Convert a [U128] to its decimal string representation.
			## ```roc
			## expect U128.to_str(42) == "42"
			## ```
			to_str : U128 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U128.is_zero(0)
			##
			## expect !U128.is_zero(7)
			## ```
			is_zero : U128 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U128.is_eq(3, 3)
			##
			## expect !U128.is_eq(3, 4)
			## ```
			is_eq : U128, U128 -> Bool

			## Feed a [U128] into a [Hasher].
			to_hash : U128, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_u128(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U128.is_gt(5, 3)
			##
			## expect !U128.is_gt(3, 3)
			## ```
			is_gt : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U128.is_gte(3, 3)
			##
			## expect !U128.is_gte(2, 3)
			## ```
			is_gte : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U128.is_lt(3, 5)
			##
			## expect !U128.is_lt(3, 3)
			## ```
			is_lt : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U128.is_lte(3, 3)
			##
			## expect !U128.is_lte(5, 3)
			## ```
			is_lte : U128, U128 -> Bool

			## Compare two [U128] values and return their ordering.
			## ```roc
			## expect U128.compare(1, 2) == LT
			##
			## expect U128.compare(2, 2) == EQ
			##
			## expect U128.compare(3, 2) == GT
			## ```
			compare : U128, U128 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect U128.is_even(4)
			##
			## expect !U128.is_even(5)
			## ```
			is_even : U128 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect U128.is_odd(5)
			##
			## expect !U128.is_odd(4)
			## ```
			is_odd : U128 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect U128.is_multiple_of(12, 3)
			##
			## expect U128.is_multiple_of(0, 0)
			##
			## expect !U128.is_multiple_of(5, 0)
			## ```
			is_multiple_of : U128, U128 -> Bool
			is_multiple_of = |value, divisor| unsigned_is_multiple_of(0, value, divisor)

			## Returns the greater of two [U128] values.
			## ```roc
			## expect U128.max(5, 3) == 5
			## ```
			max : U128, U128 -> U128
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U128] values.
			## ```roc
			## expect U128.min(5, 3) == 3
			## ```
			min : U128, U128 -> U128
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U128] values.
			## ```roc
			## expect U128.plus(2, 3) == 5
			## ```
			plus : U128, U128 -> U128

			## Add two [U128] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in a [U128].
			add_try : U128, U128 -> Try(U128, [Overflow, ..])
			add_try = |a, b| unsigned_add_try(U128.highest, a, b)

			steps_between : U128, U128 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					match start.abs_diff(end).to_u64_try() {
						Ok(n) => Known(n)
						Err(OutOfRange) => Unknown
					}
				else
					Known(0)

			## Add two [U128] values, saturating at [U128.highest] on overflow rather than wrapping around.
			## ```roc
			## expect U128.plus_saturated(U128.highest, 1) == U128.highest
			##
			## expect U128.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : U128, U128 -> U128
			plus_saturated = |a, b|
				if b > highest - a
					highest
				else
					a + b

			## Subtract the second [U128] from the first.
			## ```roc
			## expect U128.minus(5, 3) == 2
			## ```
			minus : U128, U128 -> U128

			## Subtract the second [U128] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in a [U128].
			sub_try : U128, U128 -> Try(U128, [Overflow, ..])
			sub_try = |a, b| unsigned_sub_try(a, b)

			## Subtract the second [U128] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect U128.minus_saturated(0, 1) == 0
			##
			## expect U128.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : U128, U128 -> U128
			minus_saturated = |a, b| unsigned_minus_saturated(0, a, b)

			## Multiply two [U128] values.
			## ```roc
			## expect U128.times(4, 3) == 12
			## ```
			times : U128, U128 -> U128

			## Multiply two [U128] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in a [U128].
			mul_try : U128, U128 -> Try(U128, [Overflow, ..])
			mul_try = |a, b| unsigned_mul_try(U128.highest, 0, a, b)

			## Multiply two [U128] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect U128.times_saturated(U128.highest, 2) == U128.highest
			##
			## expect U128.times_saturated(4, 3) == 12
			## ```
			times_saturated : U128, U128 -> U128
			times_saturated = |a, b| unsigned_times_saturated(U128.highest, 0, a, b)

			## Raise the first [U128] value to the power of the second.
			## Crashes if the exact result does not fit in [U128].
			## ```roc
			## expect U128.pow(2, 3) == 8
			##
			## expect U128.pow(5, 0) == 1
			## ```
			pow : U128, U128 -> U128
			pow = |base, exponent|
				match U128.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
				}

			## Raise the first [U128] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect U128.pow_try(2, 3) == Ok(8)
			##
			## expect U128.pow_try(U128.highest, 2) == Err(Overflow)
			## ```
			pow_try : U128, U128 -> Try(U128, [Overflow, ..])
			pow_try = |base, exponent| unsigned_pow_try(U128.highest, 0, 1, 2, base, exponent)

			## Divide the first [U128] by the second, discarding any remainder. Crashes if the second [U128] is zero.
			## ```roc
			## expect U128.div_by(10, 2) == 5
			##
			## expect U128.div_by(11, 2) == 5
			## ```
			div_by : U128, U128 -> U128

			## Divide the first [U128] by the second, returning `Err(DivByZero)`
			## instead of crashing if the divisor is zero.
			div_try : U128, U128 -> Try(U128, [DivByZero, ..])
			div_try = |a, b| unsigned_div_try(0, a, b)

			## Divide the first [U128] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [U128].
			## ```roc
			## expect U128.div_ceil_by(7, 2) == 4
			##
			## expect U128.div_ceil_by(8, 2) == 4
			## ```
			div_ceil_by : U128, U128 -> U128
			div_ceil_by = |a, b|
				match U128.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
				}

			## Divide the first [U128] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect U128.div_ceil_try(7, 2) == Ok(4)
			##
			## expect U128.div_ceil_try(1, 0) == Err(DivByZero)
			## ```
			div_ceil_try : U128, U128 -> Try(U128, [DivByZero, ..])
			div_ceil_try = |a, b| unsigned_div_ceil_try(0, 1, a, b)

			## Divide the first [U128] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [U128.div_by].
			## ```roc
			## expect U128.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U128, U128 -> U128

			## Return the remainder of dividing the first [U128] by the second.
			## ```roc
			## expect U128.rem_by(7, 3) == 1
			## ```
			rem_by : U128, U128 -> U128

			## Return the modulus of the first [U128] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [U128.rem_by].
			## ```roc
			## expect U128.mod_by(7, 3) == 1
			## ```
			mod_by : U128, U128 -> U128

			## Return the absolute difference between two [U128] values.
			## ```roc
			## expect U128.abs_diff(2, 5) == 3
			##
			## expect U128.abs_diff(5, 2) == 3
			## ```
			abs_diff : U128, U128 -> U128

			## Shift the bits of a [U128] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 2^128).
			## ```roc
			## expect U128.shift_left_by(1, 3) == 8
			##
			## expect U128.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U128, U8 -> U128

			## Shift the bits of a [U128] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [U128.shift_right_zf_by].
			## ```roc
			## expect U128.shift_right_by(32, 2) == 8
			##
			## expect U128.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U128, U8 -> U128

			## Shift the bits of a [U128] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [U128.shift_right_by].
			## ```roc
			## expect U128.shift_right_zf_by(32, 2) == 8
			##
			## expect U128.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U128, U8 -> U128

			## Returns the bitwise AND of two [U128] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect U128.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : U128, U128 -> U128

			## Returns the bitwise OR of two [U128] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect U128.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : U128, U128 -> U128

			## Returns the bitwise XOR of two [U128] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect U128.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : U128, U128 -> U128

			## Returns the bitwise NOT of a [U128] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`.
			## ```roc
			## expect U128.bitwise_not(0) == 340282366920938463463374607431768211455
			## ```
			bitwise_not : U128 -> U128

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect U128.count_leading_zero_bits(1) == 127
			##
			## expect U128.count_leading_zero_bits(0) == 128
			## ```
			count_leading_zero_bits : U128 -> U8
			count_leading_zero_bits = |value| unsigned_count_leading_zero_bits(128, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect U128.count_trailing_zero_bits(8) == 3
			##
			## expect U128.count_trailing_zero_bits(0) == 128
			## ```
			count_trailing_zero_bits : U128 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(128, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect U128.count_one_bits(0b1011) == 3
			##
			## expect U128.count_one_bits(0) == 0
			## ```
			count_one_bits : U128 -> U8
			count_one_bits = |value| unsigned_count_one_bits(0, 2, value)

			## Iterator of integers beginning with this `U128` and ending with the other `U128`.
			## (Use [U128.until] instead to end with the other `U128` minus one.)
			## Returns an empty iterator if this `U128` is greater than the other.
			## ```roc
			## expect Iter.fold(U128.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(U128.to(3, 3), [], |acc, item| acc.append(item)) == [3]
			##
			## expect Iter.fold(U128.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : U128, U128 -> Iter(U128)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					match U128.to_u64_try(end - start) {
						Ok(diff_u64) => match U64.add_try(diff_u64, 1) {
							Ok(len) => Known(len)
							Err(Overflow) => Unknown
						}
						Err(OutOfRange) => Unknown
					}
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match U128.add_try(start, 1) {
									Ok(next) => if next <= end {
										U128.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `U128` and ending with the other `U128` minus one.
			## (Use [U128.to] instead to end with the other `U128` exactly, instead of minus one.)
			## Returns an empty iterator if this `U128` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(U128.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(U128.until(3, 3), [], |acc, item| acc.append(item)) == []
			##
			## expect Iter.fold(U128.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : U128, U128 -> Iter(U128)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					match U128.to_u64_try(end - start) {
						Ok(len) => Known(len)
						Err(OutOfRange) => Unknown
					}
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match U128.add_try(start, 1) {
									Ok(next) => if next < end {
										U128.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build a [U128] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U128] (`0` to `340282366920938463463374607431768211455`), or if any
			## element is not a valid digit.
			## ```roc
			## expect U128.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U128, [OutOfRange])
			from_int_digits = |digits| u128_from_int_digits(digits)

			## Convert a numeric literal into a [U128]. This is the hook the
			## compiler uses when a literal is given type [U128]; most code should
			## parse user text with [U128.from_str] instead.
			from_numeral : Numeral -> Try(U128, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| u128_from_str(str))

			## Parse a [U128] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U128] (`0` to `340282366920938463463374607431768211455`).
			## ```roc
			## expect U128.from_str("42") == Ok(42)
			##
			## expect U128.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U128, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U128] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U128.to_i8_wrap(42) == 42
			##
			## expect U128.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U128 -> I8

			## Convert a [U128] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i8_try(42) == Ok(42)
			##
			## expect U128.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U128 -> Try(I8, [OutOfRange, ..])

			## Convert a [U128] to an [I16], wrapping on overflow. Values from `0`
			## to `32767` are preserved; larger values wrap by truncating to the
			## low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect U128.to_i16_wrap(42) == 42
			##
			## expect U128.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U128 -> I16

			## Convert a [U128] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i16_try(42) == Ok(42)
			##
			## expect U128.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U128 -> Try(I16, [OutOfRange, ..])

			## Convert a [U128] to an [I32], wrapping on overflow. Values from `0`
			## to `2147483647` are preserved; larger values wrap by truncating to
			## the low 32 bits and reinterpreting them in two's complement.
			## ```roc
			## expect U128.to_i32_wrap(42) == 42
			##
			## expect U128.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : U128 -> I32

			## Convert a [U128] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `2147483647` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i32_try(42) == Ok(42)
			##
			## expect U128.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : U128 -> Try(I32, [OutOfRange, ..])

			## Convert a [U128] to an [I64], wrapping on overflow. Values from `0`
			## to `9223372036854775807` are preserved; larger values wrap by
			## truncating to the low 64 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect U128.to_i64_wrap(42) == 42
			##
			## expect U128.to_i64_wrap(10000000000000000000) == -8446744073709551616
			## ```
			to_i64_wrap : U128 -> I64

			## Convert a [U128] to an [I64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `9223372036854775807` succeed; larger
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i64_try(42) == Ok(42)
			##
			## expect U128.to_i64_try(10000000000000000000) == Err(OutOfRange)
			## ```
			to_i64_try : U128 -> Try(I64, [OutOfRange, ..])

			## Convert a [U128] to an [I128], wrapping on overflow. Values from `0`
			## to `170141183460469231731687303715884105727` are preserved; larger
			## values wrap into the negative range (two's complement reinterpretation
			## of the bits).
			## ```roc
			## expect U128.to_i128_wrap(42) == 42
			##
			## expect U128.to_i128_wrap(200000000000000000000000000000000000000) == -140282366920938463463374607431768211456
			## ```
			to_i128_wrap : U128 -> I128

			## Convert a [U128] to an [I128], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `170141183460469231731687303715884105727`
			## succeed; larger values return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i128_try(42) == Ok(42)
			##
			## expect U128.to_i128_try(200000000000000000000000000000000000000) == Err(OutOfRange)
			## ```
			to_i128_try : U128 -> Try(I128, [OutOfRange, ..])

			# Conversions to unsigned integers

			## Convert a [U128] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U128.to_u8_wrap(42) == 42
			##
			## expect U128.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U128 -> U8

			## Convert a [U128] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u8_try(42) == Ok(42)
			##
			## expect U128.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U128 -> Try(U8, [OutOfRange, ..])

			## Convert a [U128] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; larger values wrap by truncating to the low 16
			## bits.
			## ```roc
			## expect U128.to_u16_wrap(42) == 42
			##
			## expect U128.to_u16_wrap(70000) == 4464
			## ```
			to_u16_wrap : U128 -> U16

			## Convert a [U128] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u16_try(42) == Ok(42)
			##
			## expect U128.to_u16_try(70000) == Err(OutOfRange)
			## ```
			to_u16_try : U128 -> Try(U16, [OutOfRange, ..])

			## Convert a [U128] to a [U32], wrapping on overflow. Values from `0` to
			## `4294967295` are preserved; larger values wrap by truncating to the
			## low 32 bits.
			## ```roc
			## expect U128.to_u32_wrap(42) == 42
			##
			## expect U128.to_u32_wrap(5000000000) == 705032704
			## ```
			to_u32_wrap : U128 -> U32

			## Convert a [U128] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u32_try(42) == Ok(42)
			##
			## expect U128.to_u32_try(5000000000) == Err(OutOfRange)
			## ```
			to_u32_try : U128 -> Try(U32, [OutOfRange, ..])

			## Convert a [U128] to a [U64], wrapping on overflow. Values from `0` to
			## `18446744073709551615` are preserved; larger values wrap by truncating
			## to the low 64 bits.
			## ```roc
			## expect U128.to_u64_wrap(42) == 42
			## ```
			to_u64_wrap : U128 -> U64

			## Convert a [U128] to a [U64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `18446744073709551615` succeed;
			## larger values return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u64_try(42) == Ok(42)
			## ```
			to_u64_try : U128 -> Try(U64, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			## Convert a [U128] to an [F32]. This conversion may round, and the
			## largest [U128] values may become `inf`.
			to_f32 : U128 -> F32

			## Convert a [U128] to an [F64]. This conversion may round because
			## [F64] has fewer integer precision bits than [U128].
			to_f64 : U128 -> F64

			# Conversion to Dec (can overflow)
			## Convert a [U128] to a [Dec], returning `Err(OutOfRange)` if the
			## integer value does not fit in [Dec]'s fixed-point range.
			to_dec_try : U128 -> Try(Dec, [OutOfRange])
			to_dec_try = |num| out_of_range_try(u128_to_dec_try_unsafe(num))

			# Encode a U128 using a format that provides encode_u128
			encode : U128, fmt -> Try(encoded, err)
				where [fmt.encode_u128 : fmt, U128 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u128(self)
			}

			decode : src, fmt -> (Try(U128, err), src)
				where [fmt.decode_u128 : fmt, src -> (Try(U128, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u128(format, source)
			}
		}

		I128 :: [].{

			## Returns the default [I128] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I128.default() == 0
			## ```
			default : () -> I128
			default = || 0

			## The highest value representable by an [I128], which is
			## `170141183460469231731687303715884105727`.
			## ```roc
			## expect I128.highest == 170141183460469231731687303715884105727
			## ```
			highest : I128
			highest = 170141183460469231731687303715884105727

			## The lowest value representable by an [I128], which is
			## `-170141183460469231731687303715884105728`.
			## ```roc
			## expect I128.lowest == -170141183460469231731687303715884105728
			## ```
			lowest : I128
			lowest = -170141183460469231731687303715884105728

			## Convert an [I128] to its decimal string representation.
			## ```roc
			## expect I128.to_str(42) == "42"
			##
			## expect I128.to_str(-42) == "-42"
			## ```
			to_str : I128 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I128.is_zero(0)
			##
			## expect !I128.is_zero(7)
			## ```
			is_zero : I128 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I128.is_negative(-3)
			##
			## expect !I128.is_negative(0)
			## ```
			is_negative : I128 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I128.is_positive(3)
			##
			## expect !I128.is_positive(0)
			## ```
			is_positive : I128 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I128.is_eq(3, 3)
			##
			## expect !I128.is_eq(3, 4)
			## ```
			is_eq : I128, I128 -> Bool

			## Feed an [I128] into a [Hasher].
			to_hash : I128, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_i128(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I128.is_gt(5, 3)
			##
			## expect !I128.is_gt(3, 3)
			## ```
			is_gt : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I128.is_gte(3, 3)
			##
			## expect !I128.is_gte(2, 3)
			## ```
			is_gte : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I128.is_lt(3, 5)
			##
			## expect !I128.is_lt(3, 3)
			## ```
			is_lt : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I128.is_lte(3, 3)
			##
			## expect !I128.is_lte(5, 3)
			## ```
			is_lte : I128, I128 -> Bool

			## Compare two [I128] values and return their ordering.
			## ```roc
			## expect I128.compare(1, 2) == LT
			##
			## expect I128.compare(2, 2) == EQ
			##
			## expect I128.compare(3, 2) == GT
			## ```
			compare : I128, I128 -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns `Bool.True` if the value is evenly divisible by `2`.
			## ```roc
			## expect I128.is_even(4)
			##
			## expect !I128.is_even(5)
			## ```
			is_even : I128 -> Bool
			is_even = |value| integer_is_even(0, 2, value)

			## Returns `Bool.True` if the value is not evenly divisible by `2`.
			## ```roc
			## expect I128.is_odd(5)
			##
			## expect !I128.is_odd(4)
			## ```
			is_odd : I128 -> Bool
			is_odd = |value| integer_is_odd(0, 2, value)

			## Returns `Bool.True` if the first value is a multiple of the second.
			## A zero divisor returns `Bool.True` only when the first value is also zero.
			## ```roc
			## expect I128.is_multiple_of(12, 3)
			##
			## expect I128.is_multiple_of(0, 0)
			##
			## expect !I128.is_multiple_of(5, 0)
			##
			## expect I128.is_multiple_of(I128.lowest, -1)
			## ```
			is_multiple_of : I128, I128 -> Bool
			is_multiple_of = |value, divisor| signed_is_multiple_of(0, -1, value, divisor)

			## Returns the greater of two [I128] values.
			## ```roc
			## expect I128.max(5, 3) == 5
			##
			## expect I128.max(-3, -1) == -1
			## ```
			max : I128, I128 -> I128
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I128] values.
			## ```roc
			## expect I128.min(5, 3) == 3
			##
			## expect I128.min(-3, -1) == -3
			## ```
			min : I128, I128 -> I128
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I128]. Crashes on `-170141183460469231731687303715884105728`,
			## since `170141183460469231731687303715884105728` does not fit in an [I128].
			## ```roc
			## expect I128.negate(3) == -3
			##
			## expect I128.negate(-3) == 3
			## ```
			negate : I128 -> I128

			## Return the absolute value of an [I128]. Crashes on
			## `-170141183460469231731687303715884105728`, since
			## `170141183460469231731687303715884105728` does not fit in an [I128].
			## ```roc
			## expect I128.abs(3) == 3
			##
			## expect I128.abs(-3) == 3
			## ```
			abs : I128 -> I128

			## Add two [I128] values.
			## ```roc
			## expect I128.plus(2, 3) == 5
			## ```
			plus : I128, I128 -> I128

			## Add two [I128] values, returning `Err(Overflow)` instead of wrapping
			## if the result does not fit in an [I128].
			add_try : I128, I128 -> Try(I128, [Overflow, ..])
			add_try = |a, b| signed_add_try(I128.lowest, I128.highest, 0, a, b)

			steps_between : I128, I128 -> [Known(U64), Unknown]
			steps_between = |start, end|
				if start < end
					match start.abs_diff(end).to_u64_try() {
						Ok(n) => Known(n)
						Err(OutOfRange) => Unknown
					}
				else
					Known(0)

			## Add two [I128] values, saturating at [I128.highest] or [I128.lowest] on overflow rather than wrapping around.
			## ```roc
			## expect I128.plus_saturated(I128.highest, 1) == I128.highest
			##
			## expect I128.plus_saturated(I128.lowest, -1) == I128.lowest
			##
			## expect I128.plus_saturated(2, 3) == 5
			## ```
			plus_saturated : I128, I128 -> I128
			plus_saturated = |a, b|
				if b > 0 and a > highest - b
					highest
				else if b < 0 and a < lowest - b
					lowest
				else
					a + b

			## Subtract the second [I128] from the first.
			## ```roc
			## expect I128.minus(5, 3) == 2
			## ```
			minus : I128, I128 -> I128

			## Subtract the second [I128] from the first, returning `Err(Overflow)`
			## instead of wrapping if the result does not fit in an [I128].
			sub_try : I128, I128 -> Try(I128, [Overflow, ..])
			sub_try = |a, b| signed_sub_try(I128.lowest, I128.highest, 0, a, b)

			## Subtract the second [I128] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect I128.minus_saturated(I128.lowest, 1) == I128.lowest
			##
			## expect I128.minus_saturated(I128.highest, -1) == I128.highest
			##
			## expect I128.minus_saturated(5, 3) == 2
			## ```
			minus_saturated : I128, I128 -> I128
			minus_saturated = |a, b| signed_minus_saturated(I128.lowest, I128.highest, 0, a, b)

			## Multiply two [I128] values.
			## ```roc
			## expect I128.times(4, 3) == 12
			## ```
			times : I128, I128 -> I128

			## Multiply two [I128] values, returning `Err(Overflow)` instead of
			## wrapping if the result does not fit in an [I128].
			mul_try : I128, I128 -> Try(I128, [Overflow, ..])
			mul_try = |a, b| signed_mul_try(I128.lowest, I128.highest, 0, -1, a, b)

			## Multiply two [I128] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect I128.times_saturated(I128.highest, 2) == I128.highest
			##
			## expect I128.times_saturated(I128.lowest, 2) == I128.lowest
			##
			## expect I128.times_saturated(4, 3) == 12
			## ```
			times_saturated : I128, I128 -> I128
			times_saturated = |a, b| signed_times_saturated(I128.lowest, I128.highest, 0, -1, a, b)

			## Raise the first [I128] value to the power of the second.
			## Crashes if the exact result does not fit in [I128].
			## ```roc
			## expect I128.pow(2, 3) == 8
			##
			## expect I128.pow(5, 0) == 1
			## ```
			pow : I128, I128 -> I128
			pow = |base, exponent|
				match I128.pow_try(base, exponent) {
					Ok(result) => result
					Err(Overflow) => {
						crash "integer exponentiation overflowed"
					}
					Err(Underflow) => {
						crash "integer exponentiation underflowed"
					}
				}

			## Raise the first [I128] value to the power of the second.
			## Returns an error instead of crashing when the exact result does not fit.
			## ```roc
			## expect I128.pow_try(2, 3) == Ok(8)
			##
			## expect I128.pow_try(I128.highest, 2) == Err(Overflow)
			##
			## expect I128.pow_try(2, -1) == Err(Underflow)
			##
			## expect I128.pow_try(-1, -3) == Ok(-1)
			## ```
			pow_try : I128, I128 -> Try(I128, [Overflow, Underflow, ..])
			pow_try = |base, exponent| signed_pow_try(I128.lowest, I128.highest, 0, 1, 2, -1, base, exponent)

			## Divide the first [I128] by the second, discarding any remainder. Crashes if the second [I128] is zero.
			## ```roc
			## expect I128.div_by(10, 2) == 5
			##
			## expect I128.div_by(11, 2) == 5
			## ```
			div_by : I128, I128 -> I128

			## Divide the first [I128] by the second. Returns `Err(DivByZero)` if
			## the divisor is zero, or `Err(Overflow)` for `I128.lowest / -1`.
			div_try : I128, I128 -> Try(I128, [DivByZero, Overflow, ..])
			div_try = |a, b| signed_div_try(I128.lowest, 0, -1, a, b)

			## Divide the first [I128] by the second, rounding the result toward positive infinity.
			## Crashes if the divisor is zero or the exact result does not fit in [I128].
			## ```roc
			## expect I128.div_ceil_by(7, 2) == 4
			##
			## expect I128.div_ceil_by(8, 2) == 4
			##
			## expect I128.div_ceil_by(-7, 2) == -3
			##
			## expect I128.div_ceil_by(-7, -2) == 4
			## ```
			div_ceil_by : I128, I128 -> I128
			div_ceil_by = |a, b|
				match I128.div_ceil_try(a, b) {
					Ok(result) => result
					Err(DivByZero) => {
						crash "integer ceiling division by zero"
					}
					Err(Overflow) => {
						crash "integer ceiling division overflowed"
					}
				}

			## Divide the first [I128] by the second, rounding the result toward positive infinity.
			## Returns an error instead of crashing when the divisor is zero or the exact result does not fit.
			## ```roc
			## expect I128.div_ceil_try(7, 2) == Ok(4)
			##
			## expect I128.div_ceil_try(1, 0) == Err(DivByZero)
			##
			## expect I128.div_ceil_try(I128.lowest, -1) == Err(Overflow)
			## ```
			div_ceil_try : I128, I128 -> Try(I128, [DivByZero, Overflow, ..])
			div_ceil_try = |a, b| signed_div_ceil_try(I128.lowest, I128.highest, 0, 1, -1, a, b)

			## Divide the first [I128] by the second, truncating toward zero.
			## ```roc
			## expect I128.div_trunc_by(7, 2) == 3
			##
			## expect I128.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I128, I128 -> I128

			## Return the remainder of dividing the first [I128] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I128.rem_by(7, 3) == 1
			##
			## expect I128.rem_by(-7, 3) == -1
			## ```
			rem_by : I128, I128 -> I128

			## Return the modulus of the first [I128] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [I128.rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I128.mod_by(7, 3) == 1
			##
			## expect I128.mod_by(-7, 3) == 2
			## ```
			mod_by : I128, I128 -> I128

			## Return the absolute difference between two [I128] values as a [U128].
			## The result is a [U128] because the difference between two [I128] values
			## can be as large as `340282366920938463463374607431768211455`, which does
			## not fit in an [I128].
			## ```roc
			## expect I128.abs_diff(2, 5) == 3
			##
			## expect I128.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I128, I128 -> U128

			## Shift the bits of an [I128] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I128.shift_left_by(1, 3) == 8
			##
			## expect I128.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I128, U8 -> I128

			## Shift the bits of an [I128] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I128.shift_right_by(32, 2) == 8
			##
			## expect I128.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I128, U8 -> I128

			## Shift the bits of an [I128] to the right by the given number of
			## positions.
			## ```roc
			## expect I128.shift_right_zf_by(32, 2) == 8
			##
			## expect I128.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I128, U8 -> I128

			## Returns the bitwise AND of two [I128] values. Each bit in the result is
			## `1` only when the corresponding bit is `1` in both inputs.
			## ```roc
			## expect I128.bitwise_and(0b1100, 0b1010) == 0b1000
			## ```
			bitwise_and : I128, I128 -> I128

			## Returns the bitwise OR of two [I128] values. Each bit in the result is
			## `1` when the corresponding bit is `1` in either input.
			## ```roc
			## expect I128.bitwise_or(0b1100, 0b1010) == 0b1110
			## ```
			bitwise_or : I128, I128 -> I128

			## Returns the bitwise XOR of two [I128] values. Each bit in the result is
			## `1` only when the corresponding bits of the inputs differ.
			## ```roc
			## expect I128.bitwise_xor(0b1100, 0b1010) == 0b0110
			## ```
			bitwise_xor : I128, I128 -> I128

			## Returns the bitwise NOT of an [I128] value, flipping every bit so that
			## each `0` becomes `1` and each `1` becomes `0`. For signed integers
			## this is equivalent to `-value - 1`.
			## ```roc
			## expect I128.bitwise_not(5) == -6
			## ```
			bitwise_not : I128 -> I128

			## Count the zero bits before the first one bit, starting at the most significant bit.
			## ```roc
			## expect I128.count_leading_zero_bits(-1) == 0
			##
			## expect I128.count_leading_zero_bits(0) == 128
			## ```
			count_leading_zero_bits : I128 -> U8
			count_leading_zero_bits = |value| signed_count_leading_zero_bits(128, 0, 2, value)

			## Count the zero bits after the last one bit, starting at the least significant bit.
			## ```roc
			## expect I128.count_trailing_zero_bits(-8) == 3
			##
			## expect I128.count_trailing_zero_bits(0) == 128
			## ```
			count_trailing_zero_bits : I128 -> U8
			count_trailing_zero_bits = |value| integer_count_trailing_zero_bits(128, 0, 2, value)

			## Count the one bits in the value.
			## ```roc
			## expect I128.count_one_bits(-1) == 128
			##
			## expect I128.count_one_bits(0) == 0
			## ```
			count_one_bits : I128 -> U8
			count_one_bits = |value| signed_count_one_bits(128, 0, 2, value)

			## Iterator of integers beginning with this `I128` and ending with the other `I128`.
			## (Use [I128.until] instead to end with the other `I128` minus one.)
			## Returns an empty iterator if this `I128` is greater than the other.
			## ```roc
			## expect Iter.fold(I128.to(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3, 4]
			##
			## expect Iter.fold(I128.to(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0, 1]
			##
			## expect Iter.fold(I128.to(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			to : I128, I128 -> Iter(I128)
			to = |start, end| {
				len_if_known: if start > end {
					Known(0)
				} else {
					match I128.sub_try(end, start) {
						Ok(diff) => match I128.to_u64_try(diff) {
							Ok(diff_u64) => match U64.add_try(diff_u64, 1) {
								Ok(len) => Known(len)
								Err(Overflow) => Unknown
							}
							Err(OutOfRange) => Unknown
						}
						Err(Overflow) => Unknown
					}
				},
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match I128.add_try(start, 1) {
									Ok(next) => if next <= end {
										I128.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of integers beginning with this `I128` and ending with the other `I128` minus one.
			## (Use [I128.to] instead to end with the other `I128` exactly, instead of minus one.)
			## Returns an empty iterator if this `I128` is greater than or equal to the other.
			## ```roc
			## expect Iter.fold(I128.until(1, 4), [], |acc, item| acc.append(item)) == [1, 2, 3]
			##
			## expect Iter.fold(I128.until(-2, 1), [], |acc, item| acc.append(item)) == [-2, -1, 0]
			##
			## expect Iter.fold(I128.until(5, 2), [], |acc, item| acc.append(item)) == []
			## ```
			until : I128, I128 -> Iter(I128)
			until = |start, end| {
				len_if_known: if start >= end {
					Known(0)
				} else {
					match I128.sub_try(end, start) {
						Ok(diff) => match I128.to_u64_try(diff) {
							Ok(len) => Known(len)
							Err(OutOfRange) => Unknown
						}
						Err(Overflow) => Unknown
					}
				},
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match I128.add_try(start, 1) {
									Ok(next) => if next < end {
										I128.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Build an [I128] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I128] (`-170141183460469231731687303715884105728` to
			## `170141183460469231731687303715884105727`), or if any element is not
			## a valid digit. The result is always non-negative; to build a negative
			## value, [I128.negate] the result.
			## ```roc
			## expect I128.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I128, [OutOfRange])
			from_int_digits = |digits| i128_from_int_digits(digits)

			## Convert a numeric literal into an [I128]. This is the hook the
			## compiler uses when a literal is given type [I128]; most code should
			## parse user text with [I128.from_str] instead.
			from_numeral : Numeral -> Try(I128, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| i128_from_str(str))

			## Parse an [I128] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I128] (`-170141183460469231731687303715884105728` to
			## `170141183460469231731687303715884105727`).
			## ```roc
			## expect I128.from_str("42") == Ok(42)
			##
			## expect I128.from_str("-1") == Ok(-1)
			## ```
			from_str : Str -> Try(I128, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I128] to an [I8], wrapping on overflow. Values from
			## `-128` to `127` are preserved; other values wrap by truncating to the
			## low 8 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I128.to_i8_wrap(42) == 42
			##
			## expect I128.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I128 -> I8

			## Convert an [I128] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i8_try(42) == Ok(42)
			##
			## expect I128.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I128 -> Try(I8, [OutOfRange, ..])

			## Convert an [I128] to an [I16], wrapping on overflow. Values from
			## `-32768` to `32767` are preserved; other values wrap by truncating
			## to the low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I128.to_i16_wrap(42) == 42
			##
			## expect I128.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : I128 -> I16

			## Convert an [I128] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i16_try(42) == Ok(42)
			##
			## expect I128.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : I128 -> Try(I16, [OutOfRange, ..])

			## Convert an [I128] to an [I32], wrapping on overflow. Values from
			## `-2147483648` to `2147483647` are preserved; other values wrap by
			## truncating to the low 32 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect I128.to_i32_wrap(42) == 42
			##
			## expect I128.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : I128 -> I32

			## Convert an [I128] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-2147483648` to `2147483647` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i32_try(42) == Ok(42)
			##
			## expect I128.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : I128 -> Try(I32, [OutOfRange, ..])

			## Convert an [I128] to an [I64], wrapping on overflow. Values from
			## `-9223372036854775808` to `9223372036854775807` are preserved; other
			## values wrap by truncating to the low 64 bits and reinterpreting them
			## in two's complement.
			## ```roc
			## expect I128.to_i64_wrap(42) == 42
			## ```
			to_i64_wrap : I128 -> I64

			## Convert an [I128] to an [I64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-9223372036854775808` to `9223372036854775807`
			## succeed; other values return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i64_try(42) == Ok(42)
			## ```
			to_i64_try : I128 -> Try(I64, [OutOfRange, ..])

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I128] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I128.to_u8_wrap(42) == 42
			##
			## expect I128.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I128 -> U8

			## Convert an [I128] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u8_try(42) == Ok(42)
			##
			## expect I128.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I128 -> Try(U8, [OutOfRange, ..])

			## Convert an [I128] to a [U16], wrapping on overflow. Values from `0`
			## to `65535` are preserved; other values wrap by truncating to the low
			## 16 bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I128.to_u16_wrap(42) == 42
			##
			## expect I128.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I128 -> U16

			## Convert an [I128] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u16_try(42) == Ok(42)
			##
			## expect I128.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I128 -> Try(U16, [OutOfRange, ..])

			## Convert an [I128] to a [U32], wrapping on overflow. Values from `0`
			## to `4294967295` are preserved; other values wrap by truncating to
			## the low 32 bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I128.to_u32_wrap(42) == 42
			##
			## expect I128.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I128 -> U32

			## Convert an [I128] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u32_try(42) == Ok(42)
			##
			## expect I128.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I128 -> Try(U32, [OutOfRange, ..])

			## Convert an [I128] to a [U64], wrapping on overflow. Values from `0`
			## to `18446744073709551615` are preserved; other values wrap by
			## truncating to the low 64 bits (two's complement reinterpretation of
			## those bits).
			## ```roc
			## expect I128.to_u64_wrap(42) == 42
			##
			## expect I128.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I128 -> U64

			## Convert an [I128] to a [U64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `18446744073709551615` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u64_try(42) == Ok(42)
			##
			## expect I128.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I128 -> Try(U64, [OutOfRange, ..])

			## Convert an [I128] to a [U128], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of
			## the [U128] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I128.to_u128_wrap(42) == 42
			##
			## expect I128.to_u128_wrap(-1) == 340282366920938463463374607431768211455
			## ```
			to_u128_wrap : I128 -> U128

			## Convert an [I128] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u128_try(42) == Ok(42)
			##
			## expect I128.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I128 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			## Convert an [I128] to an [F32]. This conversion may round because
			## [F32] has fewer integer precision bits than [I128].
			to_f32 : I128 -> F32

			## Convert an [I128] to an [F64]. This conversion may round because
			## [F64] has fewer integer precision bits than [I128].
			to_f64 : I128 -> F64

			## Convert an [I128] to a [Dec], returning `Err(OutOfRange)` if the
			## integer value does not fit in [Dec]'s fixed-point range.
			to_dec_try : I128 -> Try(Dec, [OutOfRange])
			to_dec_try = |num| out_of_range_try(i128_to_dec_try_unsafe(num))

			## Encode an I128 using a format that provides encode_i128
			encode : I128, fmt -> Try(encoded, err)
				where [fmt.encode_i128 : fmt, I128 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i128(self)
			}

			## Decode an I128 using a format that provides decode_i128
			decode : src, fmt -> (Try(I128, err), src)
				where [fmt.decode_i128 : fmt, src -> (Try(I128, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i128(format, source)
			}
		}

		Dec :: [].{

			## Returns the default [Dec] value, which is `0.0`. Functions like
			## [List.sum] use this as the starting value when adding up a list of
			## numbers.
			## ```roc
			## expect Dec.default() == 0.0
			## ```
			default : () -> Dec
			default = || 0.0

			## The highest value representable by a [Dec]. Dec values have 18
			## fixed fractional decimal places, so this is
			## `170141183460469231731.687303715884105727`.
			## ```roc
			## expect Dec.highest == 170141183460469231731.687303715884105727
			## ```
			highest : Dec
			highest = 170141183460469231731.687303715884105727

			## The lowest value representable by a [Dec]. Dec values have 18
			## fixed fractional decimal places, so this is
			## `-170141183460469231731.687303715884105728`.
			## ```roc
			## expect Dec.lowest == -170141183460469231731.687303715884105728
			## ```
			lowest : Dec
			lowest = -170141183460469231731.687303715884105728

			## Euler's number as a [Dec], truncated to 18 fractional decimal
			## places.
			e : Dec
			e = 2.718281828459045235

			## The circle constant pi as a [Dec], truncated to 18 fractional
			## decimal places.
			pi : Dec
			pi = 3.141592653589793238

			## The circle constant tau as a [Dec], truncated to 18 fractional
			## decimal places.
			tau : Dec
			tau = 6.283185307179586476

			## Convert a [Dec] to its decimal string representation.
			## ```roc
			## expect Dec.to_str(42.5) == "42.5"
			##
			## expect Dec.to_str(-42.5) == "-42.5"
			## ```
			to_str : Dec -> Str

			## Returns `Bool.True` if the value is `0.0`.
			## ```roc
			## expect Dec.is_zero(0.0)
			##
			## expect !Dec.is_zero(0.1)
			## ```
			is_zero : Dec -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0.0`.
			## ```roc
			## expect Dec.is_negative(-0.1)
			##
			## expect !Dec.is_negative(0.0)
			## ```
			is_negative : Dec -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0.0`.
			## ```roc
			## expect Dec.is_positive(0.1)
			##
			## expect !Dec.is_positive(0.0)
			## ```
			is_positive : Dec -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect Dec.is_eq(3.5, 3.5)
			##
			## expect !Dec.is_eq(3.5, 4.0)
			## ```
			is_eq : Dec, Dec -> Bool

			## Feed a [Dec] into a [Hasher].
			to_hash : Dec, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_dec(hasher, num)

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect Dec.is_gt(5.0, 3.0)
			##
			## expect !Dec.is_gt(3.0, 3.0)
			## ```
			is_gt : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect Dec.is_gte(3.0, 3.0)
			##
			## expect !Dec.is_gte(2.0, 3.0)
			## ```
			is_gte : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect Dec.is_lt(3.0, 5.0)
			##
			## expect !Dec.is_lt(3.0, 3.0)
			## ```
			is_lt : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect Dec.is_lte(3.0, 3.0)
			##
			## expect !Dec.is_lte(5.0, 3.0)
			## ```
			is_lte : Dec, Dec -> Bool

			## Compare two [Dec] values and return their ordering.
			## ```roc
			## expect Dec.compare(1.0, 2.0) == LT
			##
			## expect Dec.compare(2.0, 2.0) == EQ
			##
			## expect Dec.compare(3.0, 2.0) == GT
			## ```
			compare : Dec, Dec -> [LT, EQ, GT]
			compare = |a, b| numeric_compare(a, b)

			## Returns the greater of two [Dec] values.
			## ```roc
			## expect Dec.max(5, 3) == 5
			##
			## expect Dec.max(-3, -1) == -1
			## ```
			max : Dec, Dec -> Dec
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [Dec] values.
			## ```roc
			## expect Dec.min(5, 3) == 3
			##
			## expect Dec.min(-3, -1) == -3
			## ```
			min : Dec, Dec -> Dec
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate a [Dec].
			## ```roc
			## expect Dec.negate(3.5) == -3.5
			##
			## expect Dec.negate(-3.5) == 3.5
			## ```
			negate : Dec -> Dec

			## Return the absolute value of a [Dec].
			## ```roc
			## expect Dec.abs(3.5) == 3.5
			##
			## expect Dec.abs(-3.5) == 3.5
			## ```
			abs : Dec -> Dec

			## Add two [Dec] values.
			## ```roc
			## expect Dec.plus(1.5, 2.5) == 4.0
			## ```
			plus : Dec, Dec -> Dec

			## Add two [Dec] values, returning `Err(Overflow)` instead of wrapping
			## if the result is outside [Dec.lowest] through [Dec.highest].
			add_try : Dec, Dec -> Try(Dec, [Overflow, ..])
			add_try = |a, b| signed_add_try(Dec.lowest, Dec.highest, 0.0, a, b)

			## Conservative placeholder: always returns `Unknown`. Counting the steps
			## in a fractional `[start, end)` range advancing by `1` would require
			## `ceil(end - start)`; until that is implemented, `Unknown` is always a
			## correct (if imprecise) length hint.
			steps_between : Dec, Dec -> [Known(U64), Unknown]
			steps_between = |_start, _end| Unknown

			## Add two [Dec] values, saturating at [Dec.highest] or [Dec.lowest] on overflow rather than wrapping around.
			## ```roc
			## expect Dec.plus_saturated(Dec.highest, 1.0) == Dec.highest
			##
			## expect Dec.plus_saturated(Dec.lowest, -1.0) == Dec.lowest
			##
			## expect Dec.plus_saturated(1.5, 2.5) == 4.0
			## ```
			plus_saturated : Dec, Dec -> Dec
			plus_saturated = |a, b|
				if b > 0 and a > highest - b
					highest
				else if b < 0 and a < lowest - b
					lowest
				else
					a + b

			## Subtract the second [Dec] from the first.
			## ```roc
			## expect Dec.minus(5.0, 3.5) == 1.5
			## ```
			minus : Dec, Dec -> Dec

			## Subtract the second [Dec] from the first, returning
			## `Err(Overflow)` instead of wrapping if the result is outside
			## [Dec.lowest] through [Dec.highest].
			sub_try : Dec, Dec -> Try(Dec, [Overflow, ..])
			sub_try = |a, b| signed_sub_try(Dec.lowest, Dec.highest, 0.0, a, b)

			## Subtract the second [Dec] from the first, saturating at the nearest bound on overflow.
			## ```roc
			## expect Dec.minus_saturated(Dec.lowest, 1.0) == Dec.lowest
			##
			## expect Dec.minus_saturated(Dec.highest, -1.0) == Dec.highest
			##
			## expect Dec.minus_saturated(5.0, 3.5) == 1.5
			## ```
			minus_saturated : Dec, Dec -> Dec
			minus_saturated = |a, b| signed_minus_saturated(Dec.lowest, Dec.highest, 0.0, a, b)

			## Multiply two [Dec] values. The result is limited to [Dec]'s fixed
			## 18 fractional decimal places and crashes if it overflows.
			## ```roc
			## expect Dec.times(2.5, 4.0) == 10.0
			## ```
			times : Dec, Dec -> Dec

			## Multiply two [Dec] values, saturating at the nearest bound on overflow.
			## ```roc
			## expect Dec.times_saturated(Dec.highest, 2.0) == Dec.highest
			##
			## expect Dec.times_saturated(Dec.lowest, 2.0) == Dec.lowest
			##
			## expect Dec.times_saturated(2.5, 4.0) == 10.0
			## ```
			times_saturated : Dec, Dec -> Dec
			times_saturated = |a, b| signed_times_saturated(Dec.lowest, Dec.highest, 0.0, -1.0, a, b)

			## Raise a [Dec] to a [Dec] power. Results are limited to [Dec]'s
			## fixed 18 fractional decimal places. Fractional exponents require a
			## positive base; non-positive bases with fractional exponents crash.
			## Negative integer exponents divide by the positive power and crash
			## when that division is undefined, such as `0.0` to a negative power.
			pow : Dec, Dec -> Dec
			pow = |base, exponent| dec_pow_unsafe(base, exponent)

			## Return the square root of a [Dec]. Crashes if the input is negative.
			## The result is truncated to [Dec]'s fixed 18 fractional decimal
			## places.
			sqrt : Dec -> Dec
			sqrt = |self|
				match Dec.sqrt_try(self) {
					Ok(value) => value
					Err(SqrtOfNegative) => {
						crash "Dec.sqrt of negative number"
					}
				}

			## Return the square root of a [Dec], or `Err(SqrtOfNegative)` if the
			## input is negative. The result is truncated to [Dec]'s fixed 18
			## fractional decimal places.
			sqrt_try : Dec -> Try(Dec, [SqrtOfNegative, ..])
			sqrt_try = |self|
				if self < 0 {
					Err(SqrtOfNegative)
				} else {
					Ok(dec_sqrt_unsafe(self))
				}

			## Return the sine of a [Dec] angle in radians. This is a fixed-point
			## approximation limited to 18 fractional decimal places.
			sin : Dec -> Dec
			sin = |self| dec_sin_unsafe(self)

			## Return the cosine of a [Dec] angle in radians. This is a
			## fixed-point approximation limited to 18 fractional decimal places.
			cos : Dec -> Dec
			cos = |self| dec_cos_unsafe(self)

			## Return the tangent of a [Dec] angle in radians. This is computed as
			## sine divided by cosine, so it can crash if the fixed-point cosine
			## result is zero or the division overflows.
			tan : Dec -> Dec
			tan = |self| dec_tan_unsafe(self)

			## Return the arcsine of a [Dec] value in radians. Crashes if the
			## input is outside `-1.0` through `1.0`.
			asin : Dec -> Dec
			asin = |self| dec_asin_unsafe(self)

			## Return the arccosine of a [Dec] value in radians. Crashes if the
			## input is outside `-1.0` through `1.0`.
			acos : Dec -> Dec
			acos = |self| dec_acos_unsafe(self)

			## Return the arctangent of a [Dec] value in radians. This is a
			## fixed-point approximation limited to 18 fractional decimal places.
			atan : Dec -> Dec
			atan = |self| dec_atan_unsafe(self)

			## Divide the first [Dec] by the second. Crashes if the second [Dec]
			## is zero or the result overflows. Results with more than 18
			## fractional decimal places are truncated to [Dec] precision.
			## ```roc
			## expect Dec.div_by(10.0, 4.0) == 2.5
			## ```
			div_by : Dec, Dec -> Dec

			## Divide the first [Dec] by the second, truncating toward zero to
			## produce a whole-number [Dec] result.
			## ```roc
			## expect Dec.div_trunc_by(7.5, 2.0) == 3.0
			##
			## expect Dec.div_trunc_by(-7.5, 2.0) == -3.0
			## ```
			div_trunc_by : Dec, Dec -> Dec

			## Return the remainder of dividing the first [Dec] by the second.
			## The sign of the result matches the sign of the dividend.
			## ```roc
			## expect Dec.rem_by(7.5, 2.0) == 1.5
			##
			## expect Dec.rem_by(-7.5, 2.0) == -1.5
			## ```
			rem_by : Dec, Dec -> Dec

			## Return the absolute difference between two [Dec] values.
			## ```roc
			## expect Dec.abs_diff(2.5, 5.0) == 2.5
			##
			## expect Dec.abs_diff(-1.5, 5.0) == 6.5
			## ```
			abs_diff : Dec, Dec -> Dec

			## Round a [Dec] to the nearest [I8]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_i8(3.4) == 3
			## ```
			round_to_i8 : Dec -> I8
			round_to_i8 = |self| out_of_range_or_crash(Dec.to_i8_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [I16]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_i16(3.4) == 3
			## ```
			round_to_i16 : Dec -> I16
			round_to_i16 = |self| out_of_range_or_crash(Dec.to_i16_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [I32]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_i32(-3.6) == -4
			## ```
			round_to_i32 : Dec -> I32
			round_to_i32 = |self| out_of_range_or_crash(Dec.to_i32_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [I64]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_i64(7.2) == 7
			## ```
			round_to_i64 : Dec -> I64
			round_to_i64 = |self| out_of_range_or_crash(Dec.to_i64_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [I128]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_i128(7.2) == 7
			## ```
			round_to_i128 : Dec -> I128
			round_to_i128 = |self| out_of_range_or_crash(Dec.to_i128_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [U8]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_u8(3.4) == 3
			## ```
			round_to_u8 : Dec -> U8
			round_to_u8 = |self| out_of_range_or_crash(Dec.to_u8_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [U16]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_u16(3.4) == 3
			## ```
			round_to_u16 : Dec -> U16
			round_to_u16 = |self| out_of_range_or_crash(Dec.to_u16_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [U32]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_u32(7.2) == 7
			## ```
			round_to_u32 : Dec -> U32
			round_to_u32 = |self| out_of_range_or_crash(Dec.to_u32_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [U64]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_u64(7.2) == 7
			## ```
			round_to_u64 : Dec -> U64
			round_to_u64 = |self| out_of_range_or_crash(Dec.to_u64_try(dec_round_to_whole(self)))

			## Round a [Dec] to the nearest [U128]. Halfway values round away from zero. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.round_to_u128(7.2) == 7
			## ```
			round_to_u128 : Dec -> U128
			round_to_u128 = |self| out_of_range_or_crash(Dec.to_u128_try(dec_round_to_whole(self)))

			## Round a [Dec] down to an [I8]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_i8(-3.2) == -4
			## ```
			floor_to_i8 : Dec -> I8
			floor_to_i8 = |self| out_of_range_or_crash(Dec.to_i8_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to an [I16]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_i16(-3.2) == -4
			## ```
			floor_to_i16 : Dec -> I16
			floor_to_i16 = |self| out_of_range_or_crash(Dec.to_i16_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to an [I32]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_i32(3.8) == 3
			## ```
			floor_to_i32 : Dec -> I32
			floor_to_i32 = |self| out_of_range_or_crash(Dec.to_i32_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to an [I64]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_i64(3.8) == 3
			## ```
			floor_to_i64 : Dec -> I64
			floor_to_i64 = |self| out_of_range_or_crash(Dec.to_i64_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to an [I128]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_i128(3.8) == 3
			## ```
			floor_to_i128 : Dec -> I128
			floor_to_i128 = |self| out_of_range_or_crash(Dec.to_i128_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to a [U8]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_u8(3.8) == 3
			## ```
			floor_to_u8 : Dec -> U8
			floor_to_u8 = |self| out_of_range_or_crash(Dec.to_u8_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to a [U16]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_u16(3.8) == 3
			## ```
			floor_to_u16 : Dec -> U16
			floor_to_u16 = |self| out_of_range_or_crash(Dec.to_u16_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to a [U32]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_u32(3.8) == 3
			## ```
			floor_to_u32 : Dec -> U32
			floor_to_u32 = |self| out_of_range_or_crash(Dec.to_u32_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to a [U64]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_u64(3.8) == 3
			## ```
			floor_to_u64 : Dec -> U64
			floor_to_u64 = |self| out_of_range_or_crash(Dec.to_u64_try(dec_floor_to_whole(self)))

			## Round a [Dec] down to a [U128]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.floor_to_u128(3.8) == 3
			## ```
			floor_to_u128 : Dec -> U128
			floor_to_u128 = |self| out_of_range_or_crash(Dec.to_u128_try(dec_floor_to_whole(self)))

			## Round a [Dec] up to an [I8]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_i8(-3.2) == -3
			## ```
			ceiling_to_i8 : Dec -> I8
			ceiling_to_i8 = |self| out_of_range_or_crash(Dec.to_i8_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to an [I16]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_i16(-3.2) == -3
			## ```
			ceiling_to_i16 : Dec -> I16
			ceiling_to_i16 = |self| out_of_range_or_crash(Dec.to_i16_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to an [I32]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_i32(3.2) == 4
			## ```
			ceiling_to_i32 : Dec -> I32
			ceiling_to_i32 = |self| out_of_range_or_crash(Dec.to_i32_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to an [I64]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_i64(3.2) == 4
			## ```
			ceiling_to_i64 : Dec -> I64
			ceiling_to_i64 = |self| out_of_range_or_crash(Dec.to_i64_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to an [I128]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_i128(3.2) == 4
			## ```
			ceiling_to_i128 : Dec -> I128
			ceiling_to_i128 = |self| out_of_range_or_crash(Dec.to_i128_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to a [U8]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_u8(3.2) == 4
			## ```
			ceiling_to_u8 : Dec -> U8
			ceiling_to_u8 = |self| out_of_range_or_crash(Dec.to_u8_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to a [U16]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_u16(3.2) == 4
			## ```
			ceiling_to_u16 : Dec -> U16
			ceiling_to_u16 = |self| out_of_range_or_crash(Dec.to_u16_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to a [U32]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_u32(3.2) == 4
			## ```
			ceiling_to_u32 : Dec -> U32
			ceiling_to_u32 = |self| out_of_range_or_crash(Dec.to_u32_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to a [U64]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_u64(3.2) == 4
			## ```
			ceiling_to_u64 : Dec -> U64
			ceiling_to_u64 = |self| out_of_range_or_crash(Dec.to_u64_try(dec_ceiling_to_whole(self)))

			## Round a [Dec] up to a [U128]. Crashes if the rounded value is out of range.
			## ```roc
			## expect Dec.ceiling_to_u128(3.2) == 4
			## ```
			ceiling_to_u128 : Dec -> U128
			ceiling_to_u128 = |self| out_of_range_or_crash(Dec.to_u128_try(dec_ceiling_to_whole(self)))

			## Build a [Dec] from a list of base-10 digits, most significant
			## first. Each element of the list must be a digit in the range `0`
			## to `9`. Returns `Err(OutOfRange)` if the resulting value does not
			## fit in a [Dec], or if any element is not a valid digit. The result is always
			## non-negative; to build a negative value, [Dec.negate] the result.
			## ```roc
			## expect Dec.from_int_digits([1, 2, 3]) == Ok(123.0)
			## ```
			from_int_digits : List(U8) -> Try(Dec, [OutOfRange])
			from_int_digits = |digits| dec_from_int_digits(digits)

			## Build a [Dec] from a tuple of (integer digits, fractional digits),
			## each as a list of base-10 digits most significant first. Each
			## element of both lists must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [Dec], or if any element is not a valid digit. The result is
			## always non-negative; to build a negative value, [Dec.negate] the
			## result.
			## ```roc
			## expect Dec.from_dec_digits(([1, 2], [5])) == Ok(12.5)
			## ```
			from_dec_digits : (List(U8), List(U8)) -> Try(Dec, [OutOfRange])
			from_dec_digits = |digits| dec_from_dec_digits(digits)

			## Convert a numeric literal into a [Dec]. This is the hook the
			## compiler uses when a literal is given type [Dec]; most code should
			## parse user text with [Dec.from_str] instead.
			from_numeral : Numeral -> Try(Dec, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| dec_from_str(str))

			## Parse a [Dec] from a [Str]. Returns `Err(BadNumStr)` if the
			## string is not a valid decimal number, or if the parsed value does
			## not fit in a [Dec].
			## ```roc
			## expect Dec.from_str("42.5") == Ok(42.5)
			##
			## expect Dec.from_str("-1.25") == Ok(-1.25)
			##
			## expect Dec.from_str("not a number") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(Dec, [BadNumStr, ..])

			# Conversions to signed integers (all lossy - truncates fractional part)

			## Convert a [Dec] to an [I8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-128` to `127` are preserved; other values wrap by
			## truncating to the low 8 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect Dec.to_i8_wrap(42.7) == 42
			##
			## expect Dec.to_i8_wrap(200.0) == -56
			## ```
			to_i8_wrap : Dec -> I8

			## Convert a [Dec] to an [I8], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `-128` to `127` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_i8_try(42.7) == Ok(42)
			##
			## expect Dec.to_i8_try(200.0) == Err(OutOfRange)
			## ```
			to_i8_try : Dec -> Try(I8, [OutOfRange])
			to_i8_try = |num| out_of_range_try(dec_to_i8_try_unsafe(num))

			## Convert a [Dec] to an [I16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-32768` to `32767` are preserved; other values wrap
			## by truncating to the low 16 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect Dec.to_i16_wrap(42.5) == 42
			##
			## expect Dec.to_i16_wrap(40000.0) == -25536
			## ```
			to_i16_wrap : Dec -> I16

			## Convert a [Dec] to an [I16], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `-32768` to `32767` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_i16_try(42.5) == Ok(42)
			##
			## expect Dec.to_i16_try(40000.0) == Err(OutOfRange)
			## ```
			to_i16_try : Dec -> Try(I16, [OutOfRange])
			to_i16_try = |num| out_of_range_try(dec_to_i16_try_unsafe(num))

			## Convert a [Dec] to an [I32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-2147483648` to `2147483647` are preserved; other
			## values wrap by truncating to the low 32 bits and reinterpreting
			## them in two's complement.
			## ```roc
			## expect Dec.to_i32_wrap(42.5) == 42
			##
			## expect Dec.to_i32_wrap(3000000000.0) == -1294967296
			## ```
			to_i32_wrap : Dec -> I32

			## Convert a [Dec] to an [I32], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `-2147483648` to `2147483647`
			## succeed; other values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_i32_try(42.5) == Ok(42)
			##
			## expect Dec.to_i32_try(3000000000.0) == Err(OutOfRange)
			## ```
			to_i32_try : Dec -> Try(I32, [OutOfRange])
			to_i32_try = |num| out_of_range_try(dec_to_i32_try_unsafe(num))

			## Convert a [Dec] to an [I64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-9223372036854775808` to `9223372036854775807` are
			## preserved; other values wrap by truncating to the low 64 bits and
			## reinterpreting them in two's complement.
			## ```roc
			## expect Dec.to_i64_wrap(42.5) == 42
			## ```
			to_i64_wrap : Dec -> I64

			## Convert a [Dec] to an [I64], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero.
			## ```roc
			## expect Dec.to_i64_try(42.5) == Ok(42)
			## ```
			to_i64_try : Dec -> Try(I64, [OutOfRange])
			to_i64_try = |num| out_of_range_try(dec_to_i64_try_unsafe(num))

			## Convert a [Dec] to an [I128]. The fractional part is truncated
			## toward zero. The entire integer part of any [Dec] fits in an
			## [I128], so no wrapping occurs in practice.
			## ```roc
			## expect Dec.to_i128_wrap(42.5) == 42
			## ```
			to_i128_wrap : Dec -> I128

			## Convert a [Dec] to an [I128], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero.
			## ```roc
			## expect Dec.to_i128_try(42.5) == Ok(42)
			## ```
			to_i128_try : Dec -> Try(I128, [OutOfRange])
			to_i128_try = |num| out_of_range_try(dec_to_i128_try_unsafe(num))

			# Conversions to unsigned integers (all lossy - truncates fractional part)

			## Convert a [Dec] to a [U8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `255` are preserved; other values wrap by
			## truncating to the low 8 bits (two's complement reinterpretation
			## of those bits).
			## ```roc
			## expect Dec.to_u8_wrap(42.7) == 42
			##
			## expect Dec.to_u8_wrap(-1.0) == 255
			## ```
			to_u8_wrap : Dec -> U8

			## Convert a [Dec] to a [U8], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `0` to `255` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_u8_try(42.7) == Ok(42)
			##
			## expect Dec.to_u8_try(-1.0) == Err(OutOfRange)
			## ```
			to_u8_try : Dec -> Try(U8, [OutOfRange])
			to_u8_try = |num| out_of_range_try(dec_to_u8_try_unsafe(num))

			## Convert a [Dec] to a [U16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `65535` are preserved; other values wrap by
			## truncating to the low 16 bits (two's complement reinterpretation
			## of those bits).
			## ```roc
			## expect Dec.to_u16_wrap(42.5) == 42
			##
			## expect Dec.to_u16_wrap(-1.0) == 65535
			## ```
			to_u16_wrap : Dec -> U16

			## Convert a [Dec] to a [U16], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `0` to `65535` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_u16_try(42.5) == Ok(42)
			##
			## expect Dec.to_u16_try(-1.0) == Err(OutOfRange)
			## ```
			to_u16_try : Dec -> Try(U16, [OutOfRange])
			to_u16_try = |num| out_of_range_try(dec_to_u16_try_unsafe(num))

			## Convert a [Dec] to a [U32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `4294967295` are preserved; other values wrap
			## by truncating to the low 32 bits (two's complement
			## reinterpretation of those bits).
			## ```roc
			## expect Dec.to_u32_wrap(42.5) == 42
			##
			## expect Dec.to_u32_wrap(-1.0) == 4294967295
			## ```
			to_u32_wrap : Dec -> U32

			## Convert a [Dec] to a [U32], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `0` to `4294967295` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_u32_try(42.5) == Ok(42)
			##
			## expect Dec.to_u32_try(-1.0) == Err(OutOfRange)
			## ```
			to_u32_try : Dec -> Try(U32, [OutOfRange])
			to_u32_try = |num| out_of_range_try(dec_to_u32_try_unsafe(num))

			## Convert a [Dec] to a [U64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `18446744073709551615` are preserved; other
			## values wrap by truncating to the low 64 bits (two's complement
			## reinterpretation of those bits).
			## ```roc
			## expect Dec.to_u64_wrap(42.5) == 42
			##
			## expect Dec.to_u64_wrap(-1.0) == 18446744073709551615
			## ```
			to_u64_wrap : Dec -> U64

			## Convert a [Dec] to a [U64], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero.
			## ```roc
			## expect Dec.to_u64_try(42.5) == Ok(42)
			##
			## expect Dec.to_u64_try(-1.0) == Err(OutOfRange)
			## ```
			to_u64_try : Dec -> Try(U64, [OutOfRange])
			to_u64_try = |num| out_of_range_try(dec_to_u64_try_unsafe(num))

			## Convert a [Dec] to a [U128]. The fractional part is truncated
			## toward zero. Non-negative integer parts are preserved; negative
			## values wrap into the upper end of the [U128] range (two's
			## complement reinterpretation of the bits).
			## ```roc
			## expect Dec.to_u128_wrap(42.5) == 42
			## ```
			to_u128_wrap : Dec -> U128

			## Convert a [Dec] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. The fractional part is truncated toward zero.
			## ```roc
			## expect Dec.to_u128_try(42.5) == Ok(42)
			##
			## expect Dec.to_u128_try(-1.0) == Err(OutOfRange)
			## ```
			to_u128_try : Dec -> Try(U128, [OutOfRange])
			to_u128_try = |num| out_of_range_try(dec_to_u128_try_unsafe(num))

			# Conversions to floating point (lossy - Dec has more precision)

			## Convert a [Dec] to an [F32]. This conversion is lossy because
			## [Dec] has more decimal precision than [F32].
			to_f32_wrap : Dec -> F32

			## Convert a [Dec] to an [F32], returning `Err(OutOfRange)` if the
			## value does not fit in the finite [F32] range. All current [Dec]
			## values fit in that range, though precision may still be lost.
			to_f32_try : Dec -> Try(F32, [OutOfRange])
			to_f32_try = |num| out_of_range_try(dec_to_f32_try_unsafe(num))

			## Convert a [Dec] to an [F64]. This conversion is lossy because
			## [Dec] can have more decimal fractional precision than [F64] can
			## represent exactly.
			to_f64 : Dec -> F64

			## Iterator of decimals beginning with this `Dec` and ending with the
			## other `Dec`, stepping by `1.0`. (Use [Dec.until] instead to end with
			## the other `Dec` minus one.) Returns an empty iterator if this `Dec`
			## is greater than the other.
			## ```roc
			## expect Iter.fold(Dec.to(1.0, 4.0), [], |acc, item| acc.append(item)) == [1.0, 2.0, 3.0, 4.0]
			##
			## expect Iter.fold(Dec.to(-2.0, 1.0), [], |acc, item| acc.append(item)) == [-2.0, -1.0, 0.0, 1.0]
			##
			## expect Iter.fold(Dec.to(5.0, 2.0), [], |acc, item| acc.append(item)) == []
			## ```
			to : Dec, Dec -> Iter(Dec)
			to = |start, end| {
				len_if_known: Unknown,
				step: ||
					if start <= end {
						One(
							{
								item: start,
								rest: match Dec.add_try(start, 1.0) {
									Ok(next) => if next <= end {
										Dec.to(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Iterator of decimals beginning with this `Dec` and ending with the
			## other `Dec` minus one, stepping by `1.0`. (Use [Dec.to] instead to
			## end with the other `Dec` exactly, instead of minus one.) Returns
			## an empty iterator if this `Dec` is greater than or equal to the
			## other.
			## ```roc
			## expect Iter.fold(Dec.until(1.0, 4.0), [], |acc, item| acc.append(item)) == [1.0, 2.0, 3.0]
			##
			## expect Iter.fold(Dec.until(-2.0, 1.0), [], |acc, item| acc.append(item)) == [-2.0, -1.0, 0.0]
			##
			## expect Iter.fold(Dec.until(5.0, 2.0), [], |acc, item| acc.append(item)) == []
			## ```
			until : Dec, Dec -> Iter(Dec)
			until = |start, end| {
				len_if_known: Unknown,
				step: ||
					if start < end {
						One(
							{
								item: start,
								rest: match Dec.add_try(start, 1.0) {
									Ok(next) => if next < end {
										Dec.until(next, end)
									} else {
										range_done()
									}
									Err(Overflow) => range_done()
								},
							},
						)
					} else {
						Done
					},
			}

			## Encode a Dec using a format that provides encode_dec
			encode : Dec, fmt -> Try(encoded, err)
				where [fmt.encode_dec : fmt, Dec -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_dec(self)
			}

			## Decode a Dec using a format that provides decode_dec
			decode : src, fmt -> (Try(Dec, err), src)
				where [fmt.decode_dec : fmt, src -> (Try(Dec, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_dec(format, source)
			}
		}

		F32 :: [].{

			## Returns the default [F32] value, which is `0.0`. Functions like
			## [List.sum] use this as the starting value when adding up a list of
			## numbers.
			## ```roc
			## expect F32.is_zero(F32.default())
			## ```
			default : () -> F32
			default = || 0.0

			## The highest finite value representable by an [F32], which is
			## `3.40282347e38`.
			## ```roc
			## expect F32.to_bits(F32.highest) == 2139095039
			## ```
			highest : F32
			highest = 3.40282347e38

			## The lowest finite value representable by an [F32], which is
			## `-3.40282347e38`.
			## ```roc
			## expect F32.to_bits(F32.lowest) == 4286578687
			## ```
			lowest : F32
			lowest = -3.40282347e38

			## A quiet NaN [F32] value.
			## ```roc
			## expect F32.is_nan(F32.nan)
			## ```
			nan : F32
			nan = F32.from_bits(2143289344)

			## Positive infinity as an [F32].
			## ```roc
			## expect F32.is_infinite(F32.infinity)
			##
			## expect F32.infinity > F32.highest
			## ```
			infinity : F32
			infinity = F32.from_bits(2139095040)

			## Euler's number as an [F32].
			## ```roc
			## expect F32.to_bits(F32.e) == 1076754516
			## ```
			e : F32
			e = F32.from_bits(1076754516)

			## The circle constant pi as an [F32].
			## ```roc
			## expect F32.to_bits(F32.pi) == 1078530011
			## ```
			pi : F32
			pi = F32.from_bits(1078530011)

			## The circle constant tau as an [F32].
			## ```roc
			## expect F32.to_bits(F32.tau) == 1086918619
			## ```
			tau : F32
			tau = F32.from_bits(1086918619)

			## Convert an [F32] to its decimal string representation.
			## ```roc
			## expect F32.to_str(42.5) == "42.5"
			##
			## expect F32.to_str(-42.5) == "-42.5"
			## ```
			to_str : F32 -> Str

			## Return the raw IEEE 754 bit pattern of an [F32].
			## ```roc
			## expect F32.to_bits(F32.from_bits(1069547520)) == 1069547520
			##
			## expect F32.from_bits(F32.to_bits(1.5)).to_str() == "1.5"
			## ```
			to_bits : F32 -> U32

			## Build an [F32] from a raw IEEE 754 bit pattern.
			## ```roc
			## expect F32.from_bits(F32.to_bits(-0.0)).to_bits() == F32.to_bits(-0.0)
			##
			## expect F32.from_bits(0).to_bits() == 0
			## ```
			from_bits : U32 -> F32

			## Returns `Bool.True` if the value is `NaN`.
			## ```roc
			## expect F32.is_nan(F32.nan)
			##
			## expect !F32.is_nan(1.0)
			## ```
			is_nan : F32 -> Bool
			is_nan = |self| !F32.is_float_eq(self, self)

			## Returns `Bool.True` if the value is positive or negative infinity.
			## ```roc
			## expect F32.is_infinite(F32.infinity)
			##
			## expect F32.is_infinite(F32.negate(F32.infinity))
			##
			## expect !F32.is_infinite(1.0)
			## ```
			is_infinite : F32 -> Bool
			is_infinite = |self|
				if F32.is_float_eq(self, F32.infinity) {
					True
				} else {
					F32.is_float_eq(self, F32.negate(F32.infinity))
				}

			## Returns `Bool.True` if the value is neither `NaN` nor infinity.
			## ```roc
			## expect F32.is_finite(1.0)
			##
			## expect !F32.is_finite(F32.infinity)
			##
			## expect !F32.is_finite(F32.nan)
			## ```
			is_finite : F32 -> Bool
			is_finite = |self|
				if F32.is_nan(self) {
					False
				} else {
					!F32.is_infinite(self)
				}

			## Returns `Bool.True` if the value is `0.0`. Both positive and
			## negative zero return `Bool.True`.
			## ```roc
			## expect F32.is_zero(0.0)
			##
			## expect !F32.is_zero(0.5)
			## ```
			is_zero : F32 -> Bool
			is_zero = |self| if self >= 0 {
				self <= 0
			} else {
				False
			}

			## Returns `Bool.True` if two [F32] values compare equal using the
			## IEEE 754 `==` operation. `NaN` does not compare equal to itself.
			## ```roc
			## expect F32.is_float_eq(1.5, 1.5)
			##
			## expect !F32.is_float_eq(F32.nan, F32.nan)
			## ```
			is_float_eq : F32, F32 -> Bool

			## Feed an [F32] into a [Hasher].
			to_hash : F32, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_f32(hasher, num)

			## Returns `Bool.True` if the value is less than `0.0`.
			## ```roc
			## expect F32.is_negative(-0.5)
			##
			## expect !F32.is_negative(0.0)
			## ```
			is_negative : F32 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0.0`.
			## ```roc
			## expect F32.is_positive(0.5)
			##
			## expect !F32.is_positive(0.0)
			## ```
			is_positive : F32 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the first value is greater than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_gt(5.0, 3.0)
			##
			## expect !F32.is_gt(3.0, 3.0)
			## ```
			is_gt : F32, F32 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to
			## the second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_gte(3.0, 3.0)
			##
			## expect !F32.is_gte(2.0, 3.0)
			## ```
			is_gte : F32, F32 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_lt(3.0, 5.0)
			##
			## expect !F32.is_lt(3.0, 3.0)
			## ```
			is_lt : F32, F32 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the
			## second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_lte(3.0, 3.0)
			##
			## expect !F32.is_lte(5.0, 3.0)
			## ```
			is_lte : F32, F32 -> Bool

			## Returns the greater of two [F32] values.
			## ```roc
			## expect F32.is_float_eq(F32.max(5, 3), 5)
			##
			## expect F32.is_float_eq(F32.max(-3, -1), -1)
			## ```
			max : F32, F32 -> F32
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [F32] values.
			## ```roc
			## expect F32.is_float_eq(F32.min(5, 3), 3)
			##
			## expect F32.is_float_eq(F32.min(-3, -1), -3)
			## ```
			min : F32, F32 -> F32
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [F32]. Flips the sign bit, so negating `0.0` produces
			## `-0.0` and negating `NaN` produces `NaN`.
			## ```roc
			## expect F32.negate(3.5).to_str() == "-3.5"
			##
			## expect F32.negate(-3.5).to_str() == "3.5"
			## ```
			negate : F32 -> F32

			## Return the absolute value of an [F32]. The result of `abs(NaN)` is
			## `NaN`.
			## ```roc
			## expect F32.abs(3.5).to_str() == "3.5"
			##
			## expect F32.abs(-3.5).to_str() == "3.5"
			## ```
			abs : F32 -> F32

			## Return the square root of an [F32]. Crashes if the input is
			## negative. `NaN` and positive infinity follow IEEE 754 behavior:
			## `NaN` returns `NaN`, and positive infinity returns positive
			## infinity.
			## ```roc
			## expect F32.to_str(F32.sqrt(9.0)) == "3"
			##
			## expect F32.to_str(F32.sqrt(2.25)) == "1.5"
			## ```
			sqrt : F32 -> F32
			sqrt = |self|
				match F32.sqrt_try(self) {
					Ok(value) => value
					Err(SqrtOfNegative) => {
						crash "F32.sqrt of negative number"
					}
				}

			## Return the square root of an [F32], or `Err(SqrtOfNegative)` if
			## the input is negative. `NaN` and positive infinity return `Ok`
			## with the IEEE 754 result.
			## ```roc
			## expect F32.is_float_eq(F32.sqrt_try(9.0).ok_or(0.0), 3.0)
			##
			## expect F32.sqrt_try(-1.0) == Err(SqrtOfNegative)
			## ```
			sqrt_try : F32 -> Try(F32, [SqrtOfNegative, ..])
			sqrt_try = |self|
				if self < 0 {
					Err(SqrtOfNegative)
				} else {
					Ok(f32_sqrt_unsafe(self))
				}

			## Raise an [F32] to an [F32] power. The result follows IEEE 754
			## behavior and may be `inf`, `-inf`, or `NaN`, such as when a
			## negative base has a non-integer exponent.
			## ```roc
			## expect F32.pow(2.0, 3.0).to_str() == "8"
			##
			## expect F32.pow(9.0, 0.5).to_str() == "3"
			## ```
			pow : F32, F32 -> F32
			pow = |base, exponent| f32_pow_unsafe(base, exponent)

			## Return the sine of an [F32] angle in radians. `NaN` and infinite
			## inputs follow IEEE 754 behavior and return `NaN`.
			## ```roc
			## expect F32.sin(0.0).to_str() == "0"
			## ```
			sin : F32 -> F32
			sin = |self| f32_sin_unsafe(self)

			## Return the cosine of an [F32] angle in radians. `NaN` and infinite
			## inputs follow IEEE 754 behavior and return `NaN`.
			## ```roc
			## expect F32.cos(0.0).to_str() == "1"
			## ```
			cos : F32 -> F32
			cos = |self| f32_cos_unsafe(self)

			## Return the tangent of an [F32] angle in radians. The result follows
			## IEEE 754 behavior and may be `inf`, `-inf`, or `NaN`; `NaN` and
			## infinite inputs return `NaN`.
			## ```roc
			## expect F32.tan(0.0).to_str() == "0"
			## ```
			tan : F32 -> F32
			tan = |self| f32_tan_unsafe(self)

			## Return the arcsine of an [F32] value in radians. Inputs outside
			## `-1.0` through `1.0`, `NaN`, and infinities return `NaN`.
			## ```roc
			## expect F32.asin(0.0).to_str() == "0"
			## ```
			asin : F32 -> F32
			asin = |self| f32_asin_unsafe(self)

			## Return the arccosine of an [F32] value in radians. Inputs outside
			## `-1.0` through `1.0`, `NaN`, and infinities return `NaN`.
			## ```roc
			## expect F32.acos(1.0).to_str() == "0"
			## ```
			acos : F32 -> F32
			acos = |self| f32_acos_unsafe(self)

			## Return the arctangent of an [F32] value in radians. `NaN` returns
			## `NaN`; infinities follow IEEE 754 behavior.
			## ```roc
			## expect F32.atan(0.0).to_str() == "0"
			## ```
			atan : F32 -> F32
			atan = |self| f32_atan_unsafe(self)

			## Add two [F32] values. Addition is subject to IEEE 754 rounding; the
			## result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F32.plus(1.5, 2.5).to_str() == "4"
			## ```
			plus : F32, F32 -> F32

			## Subtract the second [F32] from the first. Subtraction is subject to
			## IEEE 754 rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F32.minus(5.0, 3.5).to_str() == "1.5"
			## ```
			minus : F32, F32 -> F32

			## Multiply two [F32] values. Multiplication is subject to IEEE 754
			## rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F32.times(2.5, 4.0).to_str() == "10"
			## ```
			times : F32, F32 -> F32

			## Divide the first [F32] by the second. Unlike integer division,
			## division by zero does not crash: it produces `inf`, `-inf`, or
			## `NaN` as specified by IEEE 754.
			## ```roc
			## expect F32.div_by(10.0, 4.0).to_str() == "2.5"
			## ```
			div_by : F32, F32 -> F32

			## Divide the first [F32] by the second, truncating toward zero to
			## produce a whole-number [F32] result.
			## ```roc
			## expect F32.div_trunc_by(7.5, 2.0).to_str() == "3"
			##
			## expect F32.div_trunc_by(-7.5, 2.0).to_str() == "-3"
			## ```
			div_trunc_by : F32, F32 -> F32

			## Return the remainder of dividing the first [F32] by the second.
			## The sign of the result matches the sign of the dividend.
			## ```roc
			## expect F32.rem_by(7.5, 2.0).to_str() == "1.5"
			##
			## expect F32.rem_by(-7.5, 2.0).to_str() == "-1.5"
			## ```
			rem_by : F32, F32 -> F32

			## Return the absolute difference between two [F32] values.
			## ```roc
			## expect F32.abs_diff(2.5, 5.0).to_str() == "2.5"
			##
			## expect F32.abs_diff(-1.5, 5.0).to_str() == "6.5"
			## ```
			abs_diff : F32, F32 -> F32

			## Round an [F32] to the nearest [I8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_i8(3.4) == 3
			## ```
			round_to_i8 : F32 -> I8
			round_to_i8 = |self| out_of_range_or_crash(F32.to_i8_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [I16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_i16(3.4) == 3
			## ```
			round_to_i16 : F32 -> I16
			round_to_i16 = |self| out_of_range_or_crash(F32.to_i16_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [I32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_i32(-3.6) == -4
			## ```
			round_to_i32 : F32 -> I32
			round_to_i32 = |self| out_of_range_or_crash(F32.to_i32_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [I64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_i64(7.2) == 7
			## ```
			round_to_i64 : F32 -> I64
			round_to_i64 = |self| out_of_range_or_crash(F32.to_i64_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [I128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_i128(7.2) == 7
			## ```
			round_to_i128 : F32 -> I128
			round_to_i128 = |self| out_of_range_or_crash(F32.to_i128_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [U8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_u8(3.4) == 3
			## ```
			round_to_u8 : F32 -> U8
			round_to_u8 = |self| out_of_range_or_crash(F32.to_u8_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [U16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_u16(3.4) == 3
			## ```
			round_to_u16 : F32 -> U16
			round_to_u16 = |self| out_of_range_or_crash(F32.to_u16_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [U32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_u32(7.2) == 7
			## ```
			round_to_u32 : F32 -> U32
			round_to_u32 = |self| out_of_range_or_crash(F32.to_u32_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [U64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_u64(7.2) == 7
			## ```
			round_to_u64 : F32 -> U64
			round_to_u64 = |self| out_of_range_or_crash(F32.to_u64_try(f32_round_to_whole(self)))

			## Round an [F32] to the nearest [U128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.round_to_u128(7.2) == 7
			## ```
			round_to_u128 : F32 -> U128
			round_to_u128 = |self| out_of_range_or_crash(F32.to_u128_try(f32_round_to_whole(self)))

			## Round an [F32] down to an [I8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_i8(-3.2) == -4
			## ```
			floor_to_i8 : F32 -> I8
			floor_to_i8 = |self| out_of_range_or_crash(F32.to_i8_try(f32_floor_unsafe(self)))

			## Round an [F32] down to an [I16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_i16(-3.2) == -4
			## ```
			floor_to_i16 : F32 -> I16
			floor_to_i16 = |self| out_of_range_or_crash(F32.to_i16_try(f32_floor_unsafe(self)))

			## Round an [F32] down to an [I32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_i32(3.8) == 3
			## ```
			floor_to_i32 : F32 -> I32
			floor_to_i32 = |self| out_of_range_or_crash(F32.to_i32_try(f32_floor_unsafe(self)))

			## Round an [F32] down to an [I64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_i64(3.8) == 3
			## ```
			floor_to_i64 : F32 -> I64
			floor_to_i64 = |self| out_of_range_or_crash(F32.to_i64_try(f32_floor_unsafe(self)))

			## Round an [F32] down to an [I128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_i128(3.8) == 3
			## ```
			floor_to_i128 : F32 -> I128
			floor_to_i128 = |self| out_of_range_or_crash(F32.to_i128_try(f32_floor_unsafe(self)))

			## Round an [F32] down to a [U8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_u8(3.8) == 3
			## ```
			floor_to_u8 : F32 -> U8
			floor_to_u8 = |self| out_of_range_or_crash(F32.to_u8_try(f32_floor_unsafe(self)))

			## Round an [F32] down to a [U16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_u16(3.8) == 3
			## ```
			floor_to_u16 : F32 -> U16
			floor_to_u16 = |self| out_of_range_or_crash(F32.to_u16_try(f32_floor_unsafe(self)))

			## Round an [F32] down to a [U32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_u32(3.8) == 3
			## ```
			floor_to_u32 : F32 -> U32
			floor_to_u32 = |self| out_of_range_or_crash(F32.to_u32_try(f32_floor_unsafe(self)))

			## Round an [F32] down to a [U64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_u64(3.8) == 3
			## ```
			floor_to_u64 : F32 -> U64
			floor_to_u64 = |self| out_of_range_or_crash(F32.to_u64_try(f32_floor_unsafe(self)))

			## Round an [F32] down to a [U128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.floor_to_u128(3.8) == 3
			## ```
			floor_to_u128 : F32 -> U128
			floor_to_u128 = |self| out_of_range_or_crash(F32.to_u128_try(f32_floor_unsafe(self)))

			## Round an [F32] up to an [I8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_i8(-3.2) == -3
			## ```
			ceiling_to_i8 : F32 -> I8
			ceiling_to_i8 = |self| out_of_range_or_crash(F32.to_i8_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to an [I16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_i16(-3.2) == -3
			## ```
			ceiling_to_i16 : F32 -> I16
			ceiling_to_i16 = |self| out_of_range_or_crash(F32.to_i16_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to an [I32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_i32(3.2) == 4
			## ```
			ceiling_to_i32 : F32 -> I32
			ceiling_to_i32 = |self| out_of_range_or_crash(F32.to_i32_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to an [I64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_i64(3.2) == 4
			## ```
			ceiling_to_i64 : F32 -> I64
			ceiling_to_i64 = |self| out_of_range_or_crash(F32.to_i64_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to an [I128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_i128(3.2) == 4
			## ```
			ceiling_to_i128 : F32 -> I128
			ceiling_to_i128 = |self| out_of_range_or_crash(F32.to_i128_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to a [U8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_u8(3.2) == 4
			## ```
			ceiling_to_u8 : F32 -> U8
			ceiling_to_u8 = |self| out_of_range_or_crash(F32.to_u8_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to a [U16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_u16(3.2) == 4
			## ```
			ceiling_to_u16 : F32 -> U16
			ceiling_to_u16 = |self| out_of_range_or_crash(F32.to_u16_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to a [U32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_u32(3.2) == 4
			## ```
			ceiling_to_u32 : F32 -> U32
			ceiling_to_u32 = |self| out_of_range_or_crash(F32.to_u32_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to a [U64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_u64(3.2) == 4
			## ```
			ceiling_to_u64 : F32 -> U64
			ceiling_to_u64 = |self| out_of_range_or_crash(F32.to_u64_try(f32_ceiling_unsafe(self)))

			## Round an [F32] up to a [U128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F32.ceiling_to_u128(3.2) == 4
			## ```
			ceiling_to_u128 : F32 -> U128
			ceiling_to_u128 = |self| out_of_range_or_crash(F32.to_u128_try(f32_ceiling_unsafe(self)))

			## Build an [F32] from a list of base-10 digits, most significant
			## first. Each element of the list must be a digit in the range `0`
			## to `9`. Returns `Err(OutOfRange)` if the resulting value does not
			## fit in an [F32], or if any element is not a valid digit. The
			## result is always non-negative; to build a negative value, [F32.negate]
			## the result.
			## ```roc
			## expect F32.to_str(F32.from_int_digits([1, 2, 3]).ok_or(0.0)) == "123"
			## ```
			from_int_digits : List(U8) -> Try(F32, [OutOfRange])
			from_int_digits = |digits| f32_from_int_digits(digits)

			## Build an [F32] from a tuple of (integer digits, fractional digits),
			## each as a list of base-10 digits most significant first. Each
			## element of both lists must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [F32], or if any element is not a valid digit. The result is
			## always non-negative; to build a negative value, [F32.negate] the
			## result.
			## ```roc
			## expect F32.to_str(F32.from_dec_digits(([1, 2], [5])).ok_or(0.0)) == "12.5"
			## ```
			from_dec_digits : (List(U8), List(U8)) -> Try(F32, [OutOfRange])
			from_dec_digits = |digits| f32_from_dec_digits(digits)

			## Convert a numeric literal into an [F32]. This is the hook the
			## compiler uses when a literal is given type [F32]; most code should
			## parse user text with [F32.from_str] instead.
			from_numeral : Numeral -> Try(F32, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| f32_from_str(str))

			## Parse an [F32] from a [Str]. Returns `Err(BadNumStr)` if the
			## string is not a valid decimal number, or if the parsed value does
			## not fit in an [F32].
			## ```roc
			## expect F32.to_str(F32.from_str("42.5").ok_or(0.0)) == "42.5"
			##
			## expect F32.to_str(F32.from_str("-1.25").ok_or(0.0)) == "-1.25"
			##
			## expect Try.is_err(F32.from_str("not a number"))
			## ```
			from_str : Str -> Try(F32, [BadNumStr, ..])

			# Conversions to signed integers (all lossy - truncation + range check)

			## Convert an [F32] to an [I8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-128` to `127` are preserved; other values wrap by
			## truncating to the low 8 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_i8_wrap(42.7) == 42
			## ```
			to_i8_wrap : F32 -> I8

			## Convert an [F32] to an [I8], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-128` to `127` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F32.to_i8_try(42.7) == Ok(42)
			##
			## expect F32.to_i8_try(200.0) == Err(OutOfRange)
			## ```
			to_i8_try : F32 -> Try(I8, [OutOfRange])
			to_i8_try = |num| out_of_range_try(f32_to_i8_try_unsafe(num))

			## Convert an [F32] to an [I16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-32768` to `32767` are preserved; other values wrap
			## by truncating to the low 16 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_i16_wrap(42.5) == 42
			## ```
			to_i16_wrap : F32 -> I16

			## Convert an [F32] to an [I16], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F32.to_i16_try(42.5) == Ok(42)
			##
			## expect F32.to_i16_try(40000.0) == Err(OutOfRange)
			## ```
			to_i16_try : F32 -> Try(I16, [OutOfRange])
			to_i16_try = |num| out_of_range_try(f32_to_i16_try_unsafe(num))

			## Convert an [F32] to an [I32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F32.to_i32_wrap(42.5) == 42
			## ```
			to_i32_wrap : F32 -> I32

			## Convert an [F32] to an [I32], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F32.to_i32_try(42.5) == Ok(42)
			## ```
			to_i32_try : F32 -> Try(I32, [OutOfRange])
			to_i32_try = |num| out_of_range_try(f32_to_i32_try_unsafe(num))

			## Convert an [F32] to an [I64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F32.to_i64_wrap(42.5) == 42
			## ```
			to_i64_wrap : F32 -> I64

			## Convert an [F32] to an [I64], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F32.to_i64_try(42.5) == Ok(42)
			## ```
			to_i64_try : F32 -> Try(I64, [OutOfRange])
			to_i64_try = |num| out_of_range_try(f32_to_i64_try_unsafe(num))

			## Convert an [F32] to an [I128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F32.to_i128_wrap(42.5) == 42
			## ```
			to_i128_wrap : F32 -> I128

			## Convert an [F32] to an [I128], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F32.to_i128_try(42.5) == Ok(42)
			## ```
			to_i128_try : F32 -> Try(I128, [OutOfRange])
			to_i128_try = |num| out_of_range_try(f32_to_i128_try_unsafe(num))

			# Conversions to unsigned integers (all lossy - truncation + range check)

			## Convert an [F32] to a [U8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u8_wrap(42.7) == 42
			## ```
			to_u8_wrap : F32 -> U8

			## Convert an [F32] to a [U8], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u8_try(42.7) == Ok(42)
			##
			## expect F32.to_u8_try(-1.0) == Err(OutOfRange)
			## ```
			to_u8_try : F32 -> Try(U8, [OutOfRange])
			to_u8_try = |num| out_of_range_try(f32_to_u8_try_unsafe(num))

			## Convert an [F32] to a [U16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u16_wrap(42.5) == 42
			## ```
			to_u16_wrap : F32 -> U16

			## Convert an [F32] to a [U16], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u16_try(42.5) == Ok(42)
			## ```
			to_u16_try : F32 -> Try(U16, [OutOfRange])
			to_u16_try = |num| out_of_range_try(f32_to_u16_try_unsafe(num))

			## Convert an [F32] to a [U32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u32_wrap(42.5) == 42
			## ```
			to_u32_wrap : F32 -> U32

			## Convert an [F32] to a [U32], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u32_try(42.5) == Ok(42)
			## ```
			to_u32_try : F32 -> Try(U32, [OutOfRange])
			to_u32_try = |num| out_of_range_try(f32_to_u32_try_unsafe(num))

			## Convert an [F32] to a [U64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u64_wrap(42.5) == 42
			## ```
			to_u64_wrap : F32 -> U64

			## Convert an [F32] to a [U64], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u64_try(42.5) == Ok(42)
			## ```
			to_u64_try : F32 -> Try(U64, [OutOfRange])
			to_u64_try = |num| out_of_range_try(f32_to_u64_try_unsafe(num))

			## Convert an [F32] to a [U128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u128_wrap(42.5) == 42
			## ```
			to_u128_wrap : F32 -> U128

			## Convert an [F32] to a [U128], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u128_try(42.5) == Ok(42)
			## ```
			to_u128_try : F32 -> Try(U128, [OutOfRange])
			to_u128_try = |num| out_of_range_try(f32_to_u128_try_unsafe(num))

			## Convert an [F32] to an [F64]. This is a safe widening conversion:
			## every [F32] value is exactly representable as an [F64], including
			## `NaN`, `inf`, and `-inf`.
			## ```roc
			## expect F64.to_str(F32.to_f64(1.5)) == "1.5"
			## ```
			to_f64 : F32 -> F64

			## Encode an F32 using a format that provides encode_f32
			encode : F32, fmt -> Try(encoded, err)
				where [fmt.encode_f32 : fmt, F32 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_f32(self)
			}

			## Decode an F32 using a format that provides decode_f32
			decode : src, fmt -> (Try(F32, err), src)
				where [fmt.decode_f32 : fmt, src -> (Try(F32, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_f32(format, source)
			}
		}

		F64 :: [].{

			## Returns the default [F64] value, which is `0.0`. Functions like
			## [List.sum] use this as the starting value when adding up a list of
			## numbers.
			## ```roc
			## expect F64.is_zero(F64.default())
			## ```
			default : () -> F64
			default = || 0.0

			## The highest finite value representable by an [F64], which is
			## `1.7976931348623157e308`.
			## ```roc
			## expect F64.to_bits(F64.highest) == 9218868437227405311
			## ```
			highest : F64
			highest = 1.7976931348623157e308

			## The lowest finite value representable by an [F64], which is
			## `-1.7976931348623157e308`.
			## ```roc
			## expect F64.to_bits(F64.lowest) == 18442240474082181119
			## ```
			lowest : F64
			lowest = -1.7976931348623157e308

			## A quiet NaN [F64] value.
			## ```roc
			## expect F64.is_nan(F64.nan)
			## ```
			nan : F64
			nan = F64.from_bits(9221120237041090560)

			## Positive infinity as an [F64].
			## ```roc
			## expect F64.is_infinite(F64.infinity)
			##
			## expect F64.infinity > F64.highest
			## ```
			infinity : F64
			infinity = F64.from_bits(9218868437227405312)

			## Euler's number as an [F64].
			## ```roc
			## expect F64.to_bits(F64.e) == 4613303445314885481
			## ```
			e : F64
			e = F64.from_bits(4613303445314885481)

			## The circle constant pi as an [F64].
			## ```roc
			## expect F64.to_bits(F64.pi) == 4614256656552045848
			## ```
			pi : F64
			pi = F64.from_bits(4614256656552045848)

			## The circle constant tau as an [F64].
			## ```roc
			## expect F64.to_bits(F64.tau) == 4618760256179416344
			## ```
			tau : F64
			tau = F64.from_bits(4618760256179416344)

			## Convert an [F64] to its decimal string representation.
			## ```roc
			## expect F64.to_str(42.5) == "42.5"
			##
			## expect F64.to_str(-42.5) == "-42.5"
			## ```
			to_str : F64 -> Str

			## Return the raw IEEE 754 bit pattern of an [F64].
			## ```roc
			## expect F64.to_bits(F64.from_bits(4609434218613702656)) == 4609434218613702656
			##
			## expect F64.from_bits(F64.to_bits(1.5)).to_str() == "1.5"
			## ```
			to_bits : F64 -> U64

			## Build an [F64] from a raw IEEE 754 bit pattern.
			## ```roc
			## expect F64.from_bits(F64.to_bits(-0.0)).to_bits() == F64.to_bits(-0.0)
			##
			## expect F64.from_bits(0).to_bits() == 0
			## ```
			from_bits : U64 -> F64

			## Returns `Bool.True` if the value is `NaN`.
			## ```roc
			## expect F64.is_nan(F64.nan)
			##
			## expect !F64.is_nan(1.0)
			## ```
			is_nan : F64 -> Bool
			is_nan = |self| !F64.is_float_eq(self, self)

			## Returns `Bool.True` if the value is positive or negative infinity.
			## ```roc
			## expect F64.is_infinite(F64.infinity)
			##
			## expect F64.is_infinite(F64.negate(F64.infinity))
			##
			## expect !F64.is_infinite(1.0)
			## ```
			is_infinite : F64 -> Bool
			is_infinite = |self|
				if F64.is_float_eq(self, F64.infinity) {
					True
				} else {
					F64.is_float_eq(self, F64.negate(F64.infinity))
				}

			## Returns `Bool.True` if the value is neither `NaN` nor infinity.
			## ```roc
			## expect F64.is_finite(1.0)
			##
			## expect !F64.is_finite(F64.infinity)
			##
			## expect !F64.is_finite(F64.nan)
			## ```
			is_finite : F64 -> Bool
			is_finite = |self|
				if F64.is_nan(self) {
					False
				} else {
					!F64.is_infinite(self)
				}

			## Returns `Bool.True` if the value is `0.0`. Both positive and
			## negative zero return `Bool.True`.
			## ```roc
			## expect F64.is_zero(0.0)
			##
			## expect !F64.is_zero(0.5)
			## ```
			is_zero : F64 -> Bool
			is_zero = |self| if self >= 0 {
				self <= 0
			} else {
				False
			}

			## Returns `Bool.True` if two [F64] values compare equal using the
			## IEEE 754 `==` operation. `NaN` does not compare equal to itself.
			## ```roc
			## expect F64.is_float_eq(1.5, 1.5)
			##
			## expect !F64.is_float_eq(F64.nan, F64.nan)
			## ```
			is_float_eq : F64, F64 -> Bool

			## Feed an [F64] into a [Hasher].
			to_hash : F64, Hasher -> Hasher
			to_hash = |num, hasher| Hasher.write_f64(hasher, num)

			## Returns `Bool.True` if the value is less than `0.0`.
			## ```roc
			## expect F64.is_negative(-0.5)
			##
			## expect !F64.is_negative(0.0)
			## ```
			is_negative : F64 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0.0`.
			## ```roc
			## expect F64.is_positive(0.5)
			##
			## expect !F64.is_positive(0.0)
			## ```
			is_positive : F64 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the first value is greater than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_gt(5.0, 3.0)
			##
			## expect !F64.is_gt(3.0, 3.0)
			## ```
			is_gt : F64, F64 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to
			## the second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_gte(3.0, 3.0)
			##
			## expect !F64.is_gte(2.0, 3.0)
			## ```
			is_gte : F64, F64 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_lt(3.0, 5.0)
			##
			## expect !F64.is_lt(3.0, 3.0)
			## ```
			is_lt : F64, F64 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the
			## second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_lte(3.0, 3.0)
			##
			## expect !F64.is_lte(5.0, 3.0)
			## ```
			is_lte : F64, F64 -> Bool

			## Returns the greater of two [F64] values.
			## ```roc
			## expect F64.is_float_eq(F64.max(5, 3), 5)
			##
			## expect F64.is_float_eq(F64.max(-3, -1), -1)
			## ```
			max : F64, F64 -> F64
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [F64] values.
			## ```roc
			## expect F64.is_float_eq(F64.min(5, 3), 3)
			##
			## expect F64.is_float_eq(F64.min(-3, -1), -3)
			## ```
			min : F64, F64 -> F64
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [F64]. Flips the sign bit, so negating `0.0` produces
			## `-0.0` and negating `NaN` produces `NaN`.
			## ```roc
			## expect F64.negate(3.5).to_str() == "-3.5"
			##
			## expect F64.negate(-3.5).to_str() == "3.5"
			## ```
			negate : F64 -> F64

			## Return the absolute value of an [F64]. The result of `abs(NaN)` is
			## `NaN`.
			## ```roc
			## expect F64.abs(3.5).to_str() == "3.5"
			##
			## expect F64.abs(-3.5).to_str() == "3.5"
			## ```
			abs : F64 -> F64

			## Return the square root of an [F64]. Crashes if the input is
			## negative. `NaN` and positive infinity follow IEEE 754 behavior:
			## `NaN` returns `NaN`, and positive infinity returns positive
			## infinity.
			## ```roc
			## expect F64.to_str(F64.sqrt(9.0)) == "3"
			##
			## expect F64.to_str(F64.sqrt(2.25)) == "1.5"
			## ```
			sqrt : F64 -> F64
			sqrt = |self|
				match F64.sqrt_try(self) {
					Ok(value) => value
					Err(SqrtOfNegative) => {
						crash "F64.sqrt of negative number"
					}
				}

			## Return the square root of an [F64], or `Err(SqrtOfNegative)` if
			## the input is negative. `NaN` and positive infinity return `Ok`
			## with the IEEE 754 result.
			## ```roc
			## expect F64.is_float_eq(F64.sqrt_try(9.0).ok_or(0.0), 3.0)
			##
			## expect F64.sqrt_try(-1.0) == Err(SqrtOfNegative)
			## ```
			sqrt_try : F64 -> Try(F64, [SqrtOfNegative, ..])
			sqrt_try = |self|
				if self < 0 {
					Err(SqrtOfNegative)
				} else {
					Ok(f64_sqrt_unsafe(self))
				}

			## Raise an [F64] to an [F64] power. The result follows IEEE 754
			## behavior and may be `inf`, `-inf`, or `NaN`, such as when a
			## negative base has a non-integer exponent.
			## ```roc
			## expect F64.pow(2.0, 3.0).to_str() == "8"
			##
			## expect F64.pow(9.0, 0.5).to_str() == "3"
			## ```
			pow : F64, F64 -> F64
			pow = |base, exponent| f64_pow_unsafe(base, exponent)

			## Return the sine of an [F64] angle in radians. `NaN` and infinite
			## inputs follow IEEE 754 behavior and return `NaN`.
			## ```roc
			## expect F64.sin(0.0).to_str() == "0"
			## ```
			sin : F64 -> F64
			sin = |self| f64_sin_unsafe(self)

			## Return the cosine of an [F64] angle in radians. `NaN` and infinite
			## inputs follow IEEE 754 behavior and return `NaN`.
			## ```roc
			## expect F64.cos(0.0).to_str() == "1"
			## ```
			cos : F64 -> F64
			cos = |self| f64_cos_unsafe(self)

			## Return the tangent of an [F64] angle in radians. The result follows
			## IEEE 754 behavior and may be `inf`, `-inf`, or `NaN`; `NaN` and
			## infinite inputs return `NaN`.
			## ```roc
			## expect F64.tan(0.0).to_str() == "0"
			## ```
			tan : F64 -> F64
			tan = |self| f64_tan_unsafe(self)

			## Return the arcsine of an [F64] value in radians. Inputs outside
			## `-1.0` through `1.0`, `NaN`, and infinities return `NaN`.
			## ```roc
			## expect F64.asin(0.0).to_str() == "0"
			## ```
			asin : F64 -> F64
			asin = |self| f64_asin_unsafe(self)

			## Return the arccosine of an [F64] value in radians. Inputs outside
			## `-1.0` through `1.0`, `NaN`, and infinities return `NaN`.
			## ```roc
			## expect F64.acos(1.0).to_str() == "0"
			## ```
			acos : F64 -> F64
			acos = |self| f64_acos_unsafe(self)

			## Return the arctangent of an [F64] value in radians. `NaN` returns
			## `NaN`; infinities follow IEEE 754 behavior.
			## ```roc
			## expect F64.atan(0.0).to_str() == "0"
			## ```
			atan : F64 -> F64
			atan = |self| f64_atan_unsafe(self)

			## Add two [F64] values. Addition is subject to IEEE 754 rounding; the
			## result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F64.plus(1.5, 2.5).to_str() == "4"
			## ```
			plus : F64, F64 -> F64

			## Subtract the second [F64] from the first. Subtraction is subject to
			## IEEE 754 rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F64.minus(5.0, 3.5).to_str() == "1.5"
			## ```
			minus : F64, F64 -> F64

			## Multiply two [F64] values. Multiplication is subject to IEEE 754
			## rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F64.times(2.5, 4.0).to_str() == "10"
			## ```
			times : F64, F64 -> F64

			## Divide the first [F64] by the second. Unlike integer division,
			## division by zero does not crash: it produces `inf`, `-inf`, or
			## `NaN` as specified by IEEE 754.
			## ```roc
			## expect F64.div_by(10.0, 4.0).to_str() == "2.5"
			## ```
			div_by : F64, F64 -> F64

			## Divide the first [F64] by the second, truncating toward zero to
			## produce a whole-number [F64] result.
			## ```roc
			## expect F64.div_trunc_by(7.5, 2.0).to_str() == "3"
			##
			## expect F64.div_trunc_by(-7.5, 2.0).to_str() == "-3"
			## ```
			div_trunc_by : F64, F64 -> F64

			## Return the remainder of dividing the first [F64] by the second.
			## The sign of the result matches the sign of the dividend.
			## ```roc
			## expect F64.rem_by(7.5, 2.0).to_str() == "1.5"
			##
			## expect F64.rem_by(-7.5, 2.0).to_str() == "-1.5"
			## ```
			rem_by : F64, F64 -> F64

			## Return the absolute difference between two [F64] values.
			## ```roc
			## expect F64.abs_diff(2.5, 5.0).to_str() == "2.5"
			##
			## expect F64.abs_diff(-1.5, 5.0).to_str() == "6.5"
			## ```
			abs_diff : F64, F64 -> F64

			## Round an [F64] to the nearest [I8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_i8(3.4) == 3
			## ```
			round_to_i8 : F64 -> I8
			round_to_i8 = |self| out_of_range_or_crash(F64.to_i8_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [I16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_i16(3.4) == 3
			## ```
			round_to_i16 : F64 -> I16
			round_to_i16 = |self| out_of_range_or_crash(F64.to_i16_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [I32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_i32(-3.6) == -4
			## ```
			round_to_i32 : F64 -> I32
			round_to_i32 = |self| out_of_range_or_crash(F64.to_i32_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [I64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_i64(7.2) == 7
			## ```
			round_to_i64 : F64 -> I64
			round_to_i64 = |self| out_of_range_or_crash(F64.to_i64_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [I128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_i128(7.2) == 7
			## ```
			round_to_i128 : F64 -> I128
			round_to_i128 = |self| out_of_range_or_crash(F64.to_i128_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [U8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_u8(3.4) == 3
			## ```
			round_to_u8 : F64 -> U8
			round_to_u8 = |self| out_of_range_or_crash(F64.to_u8_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [U16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_u16(3.4) == 3
			## ```
			round_to_u16 : F64 -> U16
			round_to_u16 = |self| out_of_range_or_crash(F64.to_u16_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [U32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_u32(7.2) == 7
			## ```
			round_to_u32 : F64 -> U32
			round_to_u32 = |self| out_of_range_or_crash(F64.to_u32_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [U64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_u64(7.2) == 7
			## ```
			round_to_u64 : F64 -> U64
			round_to_u64 = |self| out_of_range_or_crash(F64.to_u64_try(f64_round_to_whole(self)))

			## Round an [F64] to the nearest [U128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.round_to_u128(7.2) == 7
			## ```
			round_to_u128 : F64 -> U128
			round_to_u128 = |self| out_of_range_or_crash(F64.to_u128_try(f64_round_to_whole(self)))

			## Round an [F64] down to an [I8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_i8(-3.2) == -4
			## ```
			floor_to_i8 : F64 -> I8
			floor_to_i8 = |self| out_of_range_or_crash(F64.to_i8_try(f64_floor_unsafe(self)))

			## Round an [F64] down to an [I16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_i16(-3.2) == -4
			## ```
			floor_to_i16 : F64 -> I16
			floor_to_i16 = |self| out_of_range_or_crash(F64.to_i16_try(f64_floor_unsafe(self)))

			## Round an [F64] down to an [I32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_i32(3.8) == 3
			## ```
			floor_to_i32 : F64 -> I32
			floor_to_i32 = |self| out_of_range_or_crash(F64.to_i32_try(f64_floor_unsafe(self)))

			## Round an [F64] down to an [I64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_i64(3.8) == 3
			## ```
			floor_to_i64 : F64 -> I64
			floor_to_i64 = |self| out_of_range_or_crash(F64.to_i64_try(f64_floor_unsafe(self)))

			## Round an [F64] down to an [I128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_i128(3.8) == 3
			## ```
			floor_to_i128 : F64 -> I128
			floor_to_i128 = |self| out_of_range_or_crash(F64.to_i128_try(f64_floor_unsafe(self)))

			## Round an [F64] down to a [U8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_u8(3.8) == 3
			## ```
			floor_to_u8 : F64 -> U8
			floor_to_u8 = |self| out_of_range_or_crash(F64.to_u8_try(f64_floor_unsafe(self)))

			## Round an [F64] down to a [U16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_u16(3.8) == 3
			## ```
			floor_to_u16 : F64 -> U16
			floor_to_u16 = |self| out_of_range_or_crash(F64.to_u16_try(f64_floor_unsafe(self)))

			## Round an [F64] down to a [U32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_u32(3.8) == 3
			## ```
			floor_to_u32 : F64 -> U32
			floor_to_u32 = |self| out_of_range_or_crash(F64.to_u32_try(f64_floor_unsafe(self)))

			## Round an [F64] down to a [U64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_u64(3.8) == 3
			## ```
			floor_to_u64 : F64 -> U64
			floor_to_u64 = |self| out_of_range_or_crash(F64.to_u64_try(f64_floor_unsafe(self)))

			## Round an [F64] down to a [U128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.floor_to_u128(3.8) == 3
			## ```
			floor_to_u128 : F64 -> U128
			floor_to_u128 = |self| out_of_range_or_crash(F64.to_u128_try(f64_floor_unsafe(self)))

			## Round an [F64] up to an [I8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_i8(-3.2) == -3
			## ```
			ceiling_to_i8 : F64 -> I8
			ceiling_to_i8 = |self| out_of_range_or_crash(F64.to_i8_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to an [I16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_i16(-3.2) == -3
			## ```
			ceiling_to_i16 : F64 -> I16
			ceiling_to_i16 = |self| out_of_range_or_crash(F64.to_i16_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to an [I32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_i32(3.2) == 4
			## ```
			ceiling_to_i32 : F64 -> I32
			ceiling_to_i32 = |self| out_of_range_or_crash(F64.to_i32_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to an [I64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_i64(3.2) == 4
			## ```
			ceiling_to_i64 : F64 -> I64
			ceiling_to_i64 = |self| out_of_range_or_crash(F64.to_i64_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to an [I128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_i128(3.2) == 4
			## ```
			ceiling_to_i128 : F64 -> I128
			ceiling_to_i128 = |self| out_of_range_or_crash(F64.to_i128_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to a [U8]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_u8(3.2) == 4
			## ```
			ceiling_to_u8 : F64 -> U8
			ceiling_to_u8 = |self| out_of_range_or_crash(F64.to_u8_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to a [U16]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_u16(3.2) == 4
			## ```
			ceiling_to_u16 : F64 -> U16
			ceiling_to_u16 = |self| out_of_range_or_crash(F64.to_u16_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to a [U32]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_u32(3.2) == 4
			## ```
			ceiling_to_u32 : F64 -> U32
			ceiling_to_u32 = |self| out_of_range_or_crash(F64.to_u32_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to a [U64]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_u64(3.2) == 4
			## ```
			ceiling_to_u64 : F64 -> U64
			ceiling_to_u64 = |self| out_of_range_or_crash(F64.to_u64_try(f64_ceiling_unsafe(self)))

			## Round an [F64] up to a [U128]. Crashes if the rounded value is out of range, `NaN`, or infinite.
			## ```roc
			## expect F64.ceiling_to_u128(3.2) == 4
			## ```
			ceiling_to_u128 : F64 -> U128
			ceiling_to_u128 = |self| out_of_range_or_crash(F64.to_u128_try(f64_ceiling_unsafe(self)))

			## Build an [F64] from a list of base-10 digits, most significant
			## first. Each element of the list must be a digit in the range `0`
			## to `9`. Returns `Err(OutOfRange)` if the resulting value does not
			## fit in an [F64], or if any element is not a valid digit. The
			## result is always non-negative; to build a negative value, [F64.negate]
			## the result.
			## ```roc
			## expect F64.to_str(F64.from_int_digits([1, 2, 3]).ok_or(0.0)) == "123"
			## ```
			from_int_digits : List(U8) -> Try(F64, [OutOfRange])
			from_int_digits = |digits| f64_from_int_digits(digits)

			## Build an [F64] from a tuple of (integer digits, fractional digits),
			## each as a list of base-10 digits most significant first. Each
			## element of both lists must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [F64], or if any element is not a valid digit. The result is
			## always non-negative; to build a negative value, [F64.negate] the
			## result.
			## ```roc
			## expect F64.to_str(F64.from_dec_digits(([1, 2], [5])).ok_or(0.0)) == "12.5"
			## ```
			from_dec_digits : (List(U8), List(U8)) -> Try(F64, [OutOfRange])
			from_dec_digits = |digits| f64_from_dec_digits(digits)

			## Convert a numeric literal into an [F64]. This is the hook the
			## compiler uses when a literal is given type [F64]; most code should
			## parse user text with [F64.from_str] instead.
			from_numeral : Numeral -> Try(F64, [InvalidNumeral(Str)])
			from_numeral = |numeral| from_numeral_with(numeral, |str| f64_from_str(str))

			## Parse an [F64] from a [Str]. Returns `Err(BadNumStr)` if the
			## string is not a valid decimal number, or if the parsed value does
			## not fit in an [F64].
			## ```roc
			## expect F64.to_str(F64.from_str("42.5").ok_or(0.0)) == "42.5"
			##
			## expect F64.to_str(F64.from_str("-1.25").ok_or(0.0)) == "-1.25"
			##
			## expect Try.is_err(F64.from_str("not a number"))
			## ```
			from_str : Str -> Try(F64, [BadNumStr, ..])

			# Conversions to signed integers (all lossy - truncation + range check)

			## Convert an [F64] to an [I8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-128` to `127` are preserved; other values wrap by
			## truncating to the low 8 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_i8_wrap(42.7) == 42
			## ```
			to_i8_wrap : F64 -> I8

			## Convert an [F64] to an [I8], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-128` to `127` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F64.to_i8_try(42.7) == Ok(42)
			##
			## expect F64.to_i8_try(200.0) == Err(OutOfRange)
			## ```
			to_i8_try : F64 -> Try(I8, [OutOfRange])
			to_i8_try = |num| out_of_range_try(f64_to_i8_try_unsafe(num))

			## Convert an [F64] to an [I16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-32768` to `32767` are preserved; other values wrap
			## by truncating to the low 16 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_i16_wrap(42.5) == 42
			## ```
			to_i16_wrap : F64 -> I16

			## Convert an [F64] to an [I16], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F64.to_i16_try(42.5) == Ok(42)
			##
			## expect F64.to_i16_try(40000.0) == Err(OutOfRange)
			## ```
			to_i16_try : F64 -> Try(I16, [OutOfRange])
			to_i16_try = |num| out_of_range_try(f64_to_i16_try_unsafe(num))

			## Convert an [F64] to an [I32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F64.to_i32_wrap(42.5) == 42
			## ```
			to_i32_wrap : F64 -> I32

			## Convert an [F64] to an [I32], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F64.to_i32_try(42.5) == Ok(42)
			## ```
			to_i32_try : F64 -> Try(I32, [OutOfRange])
			to_i32_try = |num| out_of_range_try(f64_to_i32_try_unsafe(num))

			## Convert an [F64] to an [I64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F64.to_i64_wrap(42.5) == 42
			## ```
			to_i64_wrap : F64 -> I64

			## Convert an [F64] to an [I64], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F64.to_i64_try(42.5) == Ok(42)
			## ```
			to_i64_try : F64 -> Try(I64, [OutOfRange])
			to_i64_try = |num| out_of_range_try(f64_to_i64_try_unsafe(num))

			## Convert an [F64] to an [I128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F64.to_i128_wrap(42.5) == 42
			## ```
			to_i128_wrap : F64 -> I128

			## Convert an [F64] to an [I128], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F64.to_i128_try(42.5) == Ok(42)
			## ```
			to_i128_try : F64 -> Try(I128, [OutOfRange])
			to_i128_try = |num| out_of_range_try(f64_to_i128_try_unsafe(num))

			# Conversions to unsigned integers (all lossy - truncation + range check)

			## Convert an [F64] to a [U8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u8_wrap(42.7) == 42
			## ```
			to_u8_wrap : F64 -> U8

			## Convert an [F64] to a [U8], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u8_try(42.7) == Ok(42)
			##
			## expect F64.to_u8_try(-1.0) == Err(OutOfRange)
			## ```
			to_u8_try : F64 -> Try(U8, [OutOfRange])
			to_u8_try = |num| out_of_range_try(f64_to_u8_try_unsafe(num))

			## Convert an [F64] to a [U16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u16_wrap(42.5) == 42
			## ```
			to_u16_wrap : F64 -> U16

			## Convert an [F64] to a [U16], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u16_try(42.5) == Ok(42)
			## ```
			to_u16_try : F64 -> Try(U16, [OutOfRange])
			to_u16_try = |num| out_of_range_try(f64_to_u16_try_unsafe(num))

			## Convert an [F64] to a [U32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u32_wrap(42.5) == 42
			## ```
			to_u32_wrap : F64 -> U32

			## Convert an [F64] to a [U32], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u32_try(42.5) == Ok(42)
			## ```
			to_u32_try : F64 -> Try(U32, [OutOfRange])
			to_u32_try = |num| out_of_range_try(f64_to_u32_try_unsafe(num))

			## Convert an [F64] to a [U64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u64_wrap(42.5) == 42
			## ```
			to_u64_wrap : F64 -> U64

			## Convert an [F64] to a [U64], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u64_try(42.5) == Ok(42)
			## ```
			to_u64_try : F64 -> Try(U64, [OutOfRange])
			to_u64_try = |num| out_of_range_try(f64_to_u64_try_unsafe(num))

			## Convert an [F64] to a [U128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u128_wrap(42.5) == 42
			## ```
			to_u128_wrap : F64 -> U128

			## Convert an [F64] to a [U128], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u128_try(42.5) == Ok(42)
			## ```
			to_u128_try : F64 -> Try(U128, [OutOfRange])
			to_u128_try = |num| out_of_range_try(f64_to_u128_try_unsafe(num))

			## Convert an [F64] to an [F32], narrowing the value. [F64] has more
			## precision and a wider range than [F32], so this conversion may
			## lose precision, and values that exceed the [F32] range overflow
			## to `inf` or `-inf`. `NaN`, `inf`, and `-inf` are preserved.
			## ```roc
			## expect F32.to_str(F64.to_f32_wrap(1.5)) == "1.5"
			## ```
			to_f32_wrap : F64 -> F32

			## Convert an [F64] to an [F32], returning `Err(OutOfRange)` if the
			## value's magnitude exceeds the [F32] range (which would otherwise
			## overflow to `inf` or `-inf`), or if the value is `NaN`. Values
			## that fit in an [F32] succeed, though precision may still be lost
			## due to [F32]'s smaller mantissa.
			## ```roc
			## expect F32.to_str(F64.to_f32_try(1.5).ok_or(0.0)) == "1.5"
			## ```
			to_f32_try : F64 -> Try(F32, [OutOfRange])
			to_f32_try = |num| out_of_range_try(f64_to_f32_try_unsafe(num))

			## Encode an F64 using a format that provides encode_f64
			encode : F64, fmt -> Try(encoded, err)
				where [fmt.encode_f64 : fmt, F64 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_f64(self)
			}

			## Decode an F64 using a format that provides decode_f64
			decode : src, fmt -> (Try(F64, err), src)
				where [fmt.decode_f64 : fmt, src -> (Try(F64, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_f64(format, source)
			}
		}
	}

}

hasher_start : U64 -> Hasher
hasher_start = |seed| { state: seed }

hasher_finish : Hasher -> U64

dict_pseudo_seed : () -> U64

crypto_sha256_hash_bytes : List(U8) -> List(U8)

crypto_sha256_hasher_empty : () -> List(U8)

crypto_sha256_hasher_write : List(U8), List(U8) -> List(U8)

crypto_sha256_hasher_finish : List(U8) -> List(U8)

crypto_blake3_hash_bytes : List(U8) -> List(U8)

crypto_blake3_hasher_empty : () -> List(U8)

crypto_blake3_hasher_write : List(U8), List(U8) -> List(U8)

crypto_blake3_hasher_finish : List(U8) -> List(U8)

dict_seed : () -> U64
dict_seed = || dict_pseudo_seed()

dict_combine_entry_hashes : U64, U64 -> U64
dict_combine_entry_hashes = |a, b| U64.bitwise_xor(a, b)

dict_empty_bucket : Dict.DictBucket
dict_empty_bucket = { dist_and_fingerprint: 0, entry_index: 0 }

dict_dist_inc : U32
dict_dist_inc = U32.shift_left_by(1, 8)

dict_fingerprint_mask : U32
dict_fingerprint_mask = dict_dist_inc - 1

dict_initial_shifts : U8
dict_initial_shifts = 61

dict_min_shifts : U8
dict_min_shifts = 32

dict_max_bucket_count : U64
dict_max_bucket_count = 4294967296

dict_max_entry_count : U64
dict_max_entry_count = 4294967296

dict_bucket_count_for_shifts : U8 -> U64
dict_bucket_count_for_shifts = |shifts| U64.shift_left_by(1, 64 - shifts)

dict_max_entries_for_bucket_count : U64 -> U64
dict_max_entries_for_bucket_count = |bucket_count|
	if bucket_count == 0 {
		0
	} else if bucket_count == dict_max_bucket_count {
		dict_max_entry_count
	} else {
		(bucket_count * 4) / 5
	}

dict_shifts_for_capacity : U64 -> U8
dict_shifts_for_capacity = |requested| {
	if requested > dict_max_entry_count {
		crash "Dict capacity overflow"
	}

	var $shifts = dict_initial_shifts
	while $shifts > dict_min_shifts and dict_max_entries_for_bucket_count(dict_bucket_count_for_shifts($shifts)) < requested {
		$shifts = $shifts - 1
	}

	$shifts
}

dict_allocate_buckets_for_capacity : U64 -> Dict.DictMetadata
dict_allocate_buckets_for_capacity = |requested| {
	if requested == 0 {
		{ buckets: [], max_entries_before_grow: 0, shifts: dict_initial_shifts }
	} else {
		shifts = dict_shifts_for_capacity(requested)
		bucket_count = dict_bucket_count_for_shifts(shifts)
		{
			buckets: List.repeat(dict_empty_bucket, bucket_count),
			max_entries_before_grow: dict_max_entries_for_bucket_count(bucket_count),
			shifts,
		}
	}
}

dict_add_capacity : U64, U64 -> U64
dict_add_capacity = |a, b|
	if b > U64.highest - a {
		crash "Dict capacity overflow"
	} else {
		a + b
	}

dict_hash_key : k -> U64
	where [k.to_hash : k, Hasher -> Hasher]
dict_hash_key = |key| hasher_finish(key.to_hash(hasher_start(dict_seed())))

dict_entry_index_from_u64 : U64 -> U32
dict_entry_index_from_u64 = |index|
	if index > U32.to_u64(U32.highest) {
		crash "Dict entry index overflow"
	} else {
		U64.to_u32_wrap(index)
	}

dict_bucket_index_from_hash : U64, U8 -> U64
dict_bucket_index_from_hash = |hash, shifts| U64.shift_right_zf_by(hash, shifts)

dict_dist_and_fingerprint_from_hash : U64 -> U32
dict_dist_and_fingerprint_from_hash = |hash| {
	fingerprint = U32.bitwise_and(U64.to_u32_wrap(hash), dict_fingerprint_mask)
	U32.bitwise_or(fingerprint, dict_dist_inc)
}

dict_increment_dist : U32 -> U32
dict_increment_dist = |dist_and_fingerprint| dist_and_fingerprint + dict_dist_inc

dict_decrement_dist : U32 -> U32
dict_decrement_dist = |dist_and_fingerprint| dist_and_fingerprint - dict_dist_inc

dict_next_bucket_index : U64, U64 -> U64
dict_next_bucket_index = |bucket_index, bucket_count|
	if bucket_index + 1 == bucket_count {
		0
	} else {
		bucket_index + 1
	}

dict_ensure_capacity : Dict.DictData(k, v), U64 -> Dict.DictData(k, v)
	where [k.to_hash : k, Hasher -> Hasher]
dict_ensure_capacity = |data, desired| {
	if desired <= data.max_entries_before_grow {
		data
	} else {
		metadata = dict_allocate_buckets_for_capacity(desired)
		entries = List.reserve(data.entries, desired - List.len(data.entries))
		buckets = dict_fill_buckets_from_entries(metadata.buckets, entries, metadata.shifts)
		{ entries, buckets, max_entries_before_grow: metadata.max_entries_before_grow, shifts: metadata.shifts }
	}
}

dict_find : Dict.DictData(k, v), k -> [Found({ bucket_index : U64, entry_index : U64, value : v }), Missing({ bucket_index : U64, dist_and_fingerprint : U32 })]
	where [k.is_eq : k, k -> Bool, k.to_hash : k, Hasher -> Hasher]
dict_find = |data, key| {
	if List.is_empty(data.entries) {
		if List.is_empty(data.buckets) {
			Missing({ bucket_index: 0, dist_and_fingerprint: 0 })
		} else {
			hash = dict_hash_key(key)
			Missing(
				{
					bucket_index: dict_bucket_index_from_hash(hash, data.shifts),
					dist_and_fingerprint: dict_dist_and_fingerprint_from_hash(hash),
				},
			)
		}
	} else if List.is_empty(data.buckets) {
		crash "Dict invariant violated: entries without buckets"
	} else {
		hash = dict_hash_key(key)
		dict_find_from(
			data.buckets,
			data.entries,
			dict_bucket_index_from_hash(hash, data.shifts),
			dict_dist_and_fingerprint_from_hash(hash),
			key,
		)
	}
}

dict_find_from : List(Dict.DictBucket), List((k, v)), U64, U32, k -> [Found({ bucket_index : U64, entry_index : U64, value : v }), Missing({ bucket_index : U64, dist_and_fingerprint : U32 })]
	where [k.is_eq : k, k -> Bool]
dict_find_from = |buckets, entries, bucket_index, dist_and_fingerprint, key| {
	bucket = list_get_unsafe(buckets, bucket_index)
	if dist_and_fingerprint == bucket.dist_and_fingerprint {
		entry_index = U32.to_u64(bucket.entry_index)
		(found_key, found_value) = list_get_unsafe(entries, entry_index)
		if found_key == key {
			Found({ bucket_index, entry_index, value: found_value })
		} else {
			dict_find_from(buckets, entries, dict_next_bucket_index(bucket_index, List.len(buckets)), dict_increment_dist(dist_and_fingerprint), key)
		}
	} else if dist_and_fingerprint > bucket.dist_and_fingerprint {
		Missing({ bucket_index, dist_and_fingerprint })
	} else {
		dict_find_from(buckets, entries, dict_next_bucket_index(bucket_index, List.len(buckets)), dict_increment_dist(dist_and_fingerprint), key)
	}
}

dict_insert_new_data : Dict.DictData(k, v), U64, U32, k, v -> Dict.DictData(k, v)
dict_insert_new_data = |data, bucket_index, dist_and_fingerprint, key, value| {
	if List.is_empty(data.buckets) {
		crash "Dict invariant violated: insert into empty bucket table"
	}

	entry_index = List.len(data.entries)
	entry_index_u32 = dict_entry_index_from_u64(entry_index)
	entries = List.append(data.entries, (key, value))
	buckets = dict_place_and_shift_up(data.buckets, { dist_and_fingerprint, entry_index: entry_index_u32 }, bucket_index)
	{ entries, buckets, max_entries_before_grow: data.max_entries_before_grow, shifts: data.shifts }
}

dict_remove_bucket_data : Dict.DictData(k, v), U64 -> Dict.DictData(k, v)
	where [k.to_hash : k, Hasher -> Hasher]
dict_remove_bucket_data = |data, bucket_index| {
	removed_bucket = list_get_unsafe(data.buckets, bucket_index)
	removed_entry_index = removed_bucket.entry_index
	removed_entry_index_u64 = U32.to_u64(removed_entry_index)

	removed = dict_remove_bucket_from_probe(data.buckets, bucket_index)
	buckets_without_removed = list_set_unsafe(removed.buckets, removed.empty_bucket_index, dict_empty_bucket)

	last_entry_index = List.len(data.entries) - 1
	if removed_entry_index_u64 == last_entry_index {
		{
			entries: List.drop_last(data.entries, 1),
			buckets: buckets_without_removed,
			max_entries_before_grow: data.max_entries_before_grow,
			shifts: data.shifts,
		}
	} else {
		entries_swapped = list_swap_unsafe(data.entries, removed_entry_index_u64, last_entry_index)
		(moved_key, _) = list_get_unsafe(entries_swapped, removed_entry_index_u64)
		moved_hash = dict_hash_key(moved_key)
		moved_base_bucket_index = dict_bucket_index_from_hash(moved_hash, data.shifts)
		moved_entry_index = dict_entry_index_from_u64(last_entry_index)
		moved_bucket_index = dict_scan_for_entry_index(buckets_without_removed, moved_base_bucket_index, moved_entry_index)
		moved_bucket = list_get_unsafe(buckets_without_removed, moved_bucket_index)
		buckets = list_set_unsafe(
			buckets_without_removed,
			moved_bucket_index,
			{ dist_and_fingerprint: moved_bucket.dist_and_fingerprint, entry_index: removed_entry_index },
		)

		{
			entries: List.drop_last(entries_swapped, 1),
			buckets,
			max_entries_before_grow: data.max_entries_before_grow,
			shifts: data.shifts,
		}
	}
}

dict_remove_bucket_from_probe : List(Dict.DictBucket), U64 -> { buckets : List(Dict.DictBucket), empty_bucket_index : U64 }
dict_remove_bucket_from_probe = |buckets, bucket_index| {
	next_index = dict_next_bucket_index(bucket_index, List.len(buckets))
	next_bucket = list_get_unsafe(buckets, next_index)
	if next_bucket.dist_and_fingerprint >= dict_dist_inc + dict_dist_inc {
		shifted = {
			dist_and_fingerprint: dict_decrement_dist(next_bucket.dist_and_fingerprint),
			entry_index: next_bucket.entry_index,
		}
		dict_remove_bucket_from_probe(list_set_unsafe(buckets, bucket_index, shifted), next_index)
	} else {
		{ buckets, empty_bucket_index: bucket_index }
	}
}

dict_scan_for_entry_index : List(Dict.DictBucket), U64, U32 -> U64
dict_scan_for_entry_index = |buckets, bucket_index, entry_index| {
	bucket = list_get_unsafe(buckets, bucket_index)
	if bucket.dist_and_fingerprint == 0 {
		crash "Dict invariant violated: moved entry bucket not found"
	} else if bucket.entry_index == entry_index {
		bucket_index
	} else {
		dict_scan_for_entry_index(buckets, dict_next_bucket_index(bucket_index, List.len(buckets)), entry_index)
	}
}

dict_fill_buckets_from_entries : List(Dict.DictBucket), List((k, v)), U8 -> List(Dict.DictBucket)
	where [k.to_hash : k, Hasher -> Hasher]
dict_fill_buckets_from_entries = |buckets, entries, shifts| {
	var $buckets = buckets
	var $index = 0
	while $index < List.len(entries) {
		(key, _) = list_get_unsafe(entries, $index)
		(bucket_index, dist_and_fingerprint) = dict_next_while_less($buckets, key, shifts)
		$buckets = dict_place_and_shift_up($buckets, { dist_and_fingerprint, entry_index: dict_entry_index_from_u64($index) }, bucket_index)
		$index = $index + 1
	}
	$buckets
}

dict_next_while_less : List(Dict.DictBucket), k, U8 -> (U64, U32)
	where [k.to_hash : k, Hasher -> Hasher]
dict_next_while_less = |buckets, key, shifts| {
	hash = dict_hash_key(key)
	dict_next_while_less_from(buckets, dict_bucket_index_from_hash(hash, shifts), dict_dist_and_fingerprint_from_hash(hash))
}

dict_next_while_less_from : List(Dict.DictBucket), U64, U32 -> (U64, U32)
dict_next_while_less_from = |buckets, bucket_index, dist_and_fingerprint| {
	loaded = list_get_unsafe(buckets, bucket_index)
	if dist_and_fingerprint < loaded.dist_and_fingerprint {
		dict_next_while_less_from(buckets, dict_next_bucket_index(bucket_index, List.len(buckets)), dict_increment_dist(dist_and_fingerprint))
	} else {
		(bucket_index, dist_and_fingerprint)
	}
}

dict_place_and_shift_up : List(Dict.DictBucket), Dict.DictBucket, U64 -> List(Dict.DictBucket)
dict_place_and_shift_up = |buckets, bucket, bucket_index| {
	loaded = list_get_unsafe(buckets, bucket_index)
	if loaded.dist_and_fingerprint == 0 {
		list_set_unsafe(buckets, bucket_index, bucket)
	} else {
		next_bucket = {
			dist_and_fingerprint: dict_increment_dist(loaded.dist_and_fingerprint),
			entry_index: loaded.entry_index,
		}
		dict_place_and_shift_up(list_set_unsafe(buckets, bucket_index, bucket), next_bucket, dict_next_bucket_index(bucket_index, List.len(buckets)))
	}
}

u8_from_str : Str -> Try(U8, [BadNumStr])

i8_from_str : Str -> Try(I8, [BadNumStr])

u16_from_str : Str -> Try(U16, [BadNumStr])

i16_from_str : Str -> Try(I16, [BadNumStr])

u32_from_str : Str -> Try(U32, [BadNumStr])

i32_from_str : Str -> Try(I32, [BadNumStr])

u64_from_str : Str -> Try(U64, [BadNumStr])

i64_from_str : Str -> Try(I64, [BadNumStr])

u128_from_str : Str -> Try(U128, [BadNumStr])

i128_from_str : Str -> Try(I128, [BadNumStr])

dec_from_str : Str -> Try(Dec, [BadNumStr])

f32_from_str : Str -> Try(F32, [BadNumStr])

f64_from_str : Str -> Try(F64, [BadNumStr])

u8_from_int_digits : List(U8) -> Try(U8, [OutOfRange])
u8_from_int_digits = |digits| int_from_digits(digits, |str| u8_from_str(str))

i8_from_int_digits : List(U8) -> Try(I8, [OutOfRange])
i8_from_int_digits = |digits| int_from_digits(digits, |str| i8_from_str(str))

u16_from_int_digits : List(U8) -> Try(U16, [OutOfRange])
u16_from_int_digits = |digits| int_from_digits(digits, |str| u16_from_str(str))

i16_from_int_digits : List(U8) -> Try(I16, [OutOfRange])
i16_from_int_digits = |digits| int_from_digits(digits, |str| i16_from_str(str))

u32_from_int_digits : List(U8) -> Try(U32, [OutOfRange])
u32_from_int_digits = |digits| int_from_digits(digits, |str| u32_from_str(str))

i32_from_int_digits : List(U8) -> Try(I32, [OutOfRange])
i32_from_int_digits = |digits| int_from_digits(digits, |str| i32_from_str(str))

u64_from_int_digits : List(U8) -> Try(U64, [OutOfRange])
u64_from_int_digits = |digits| int_from_digits(digits, |str| u64_from_str(str))

i64_from_int_digits : List(U8) -> Try(I64, [OutOfRange])
i64_from_int_digits = |digits| int_from_digits(digits, |str| i64_from_str(str))

u128_from_int_digits : List(U8) -> Try(U128, [OutOfRange])
u128_from_int_digits = |digits| int_from_digits(digits, |str| u128_from_str(str))

i128_from_int_digits : List(U8) -> Try(I128, [OutOfRange])
i128_from_int_digits = |digits| int_from_digits(digits, |str| i128_from_str(str))

dec_from_int_digits : List(U8) -> Try(Dec, [OutOfRange])
dec_from_int_digits = |digits| int_from_digits(digits, |str| dec_from_str(str))

dec_from_dec_digits : (List(U8), List(U8)) -> Try(Dec, [OutOfRange])
dec_from_dec_digits = |digits| dec_from_digits(digits, |str| dec_from_str(str))

f32_from_int_digits : List(U8) -> Try(F32, [OutOfRange])
f32_from_int_digits = |digits| int_from_digits(digits, |str| f32_from_str(str))

f32_from_dec_digits : (List(U8), List(U8)) -> Try(F32, [OutOfRange])
f32_from_dec_digits = |digits| dec_from_digits(digits, |str| f32_from_str(str))

f64_from_int_digits : List(U8) -> Try(F64, [OutOfRange])
f64_from_int_digits = |digits| int_from_digits(digits, |str| f64_from_str(str))

f64_from_dec_digits : (List(U8), List(U8)) -> Try(F64, [OutOfRange])
f64_from_dec_digits = |digits| dec_from_digits(digits, |str| f64_from_str(str))

json_u8_to_str : U8 -> Str
json_u8_to_str = |value| U8.to_str(value)

json_i8_to_str : I8 -> Str
json_i8_to_str = |value| I8.to_str(value)

json_u16_to_str : U16 -> Str
json_u16_to_str = |value| U16.to_str(value)

json_i16_to_str : I16 -> Str
json_i16_to_str = |value| I16.to_str(value)

json_u32_to_str : U32 -> Str
json_u32_to_str = |value| U32.to_str(value)

json_i32_to_str : I32 -> Str
json_i32_to_str = |value| I32.to_str(value)

json_u64_to_str : U64 -> Str
json_u64_to_str = |value| U64.to_str(value)

json_i64_to_str : I64 -> Str
json_i64_to_str = |value| I64.to_str(value)

json_u128_to_str : U128 -> Str
json_u128_to_str = |value| U128.to_str(value)

json_i128_to_str : I128 -> Str
json_i128_to_str = |value| I128.to_str(value)

json_dec_to_str : Dec -> Str
json_dec_to_str = |value| Dec.to_str(value)

json_f32_to_str : F32 -> Str
json_f32_to_str = |value| F32.to_str(value)

json_f32_is_nan : F32 -> Bool
json_f32_is_nan = |value| F32.is_nan(value)

json_f32_is_infinite : F32 -> Bool
json_f32_is_infinite = |value| F32.is_infinite(value)

json_f32_is_negative : F32 -> Bool
json_f32_is_negative = |value| F32.is_negative(value)

json_f64_to_str : F64 -> Str
json_f64_to_str = |value| F64.to_str(value)

json_f64_is_nan : F64 -> Bool
json_f64_is_nan = |value| F64.is_nan(value)

json_f64_is_infinite : F64 -> Bool
json_f64_is_infinite = |value| F64.is_infinite(value)

json_f64_is_negative : F64 -> Bool
json_f64_is_negative = |value| F64.is_negative(value)

from_numeral_with : Num.Numeral, (Str -> Try(item, err)) -> Try(item, [InvalidNumeral(Str)])
from_numeral_with = |numeral, parse|
	match numeral_to_str(numeral) {
		Err(err) => Err(err)
		Ok(str) =>
			match parse(str) {
				Ok(num) => Ok(num)
				Err(_) => Err(InvalidNumeral("invalid numeric literal"))
			}
		}

numeral_to_str : Num.Numeral -> Try(Str, [InvalidNumeral(Str)])
numeral_to_str = |numeral|
	match numeral {
		Literal({ is_negative, digits_before_pt, digits_after_pt, digits_after_pt_count }) => {
			int_digits = base256_to_decimal_digits(digits_before_pt)
			int_bytes = decimal_digits_to_bytes(int_digits)

			frac_bytes = if digits_after_pt_count == 0 {
				u8_list_with_capacity(0)
			} else {
				frac_digits = base256_to_decimal_digits(digits_after_pt)
				frac_len = u8_list_len(frac_digits)
				padded_frac_digits = if frac_len < digits_after_pt_count {
					u8_concat(u8_repeat(0, digits_after_pt_count - frac_len), frac_digits)
				} else {
					frac_digits
				}

				u8_concat(u8_single(46), decimal_digits_to_bytes(padded_frac_digits))
			}

			sign_bytes = if is_negative {
				u8_single(45)
			} else {
				u8_list_with_capacity(0)
			}

			bytes = u8_concat(sign_bytes, u8_concat(int_bytes, frac_bytes))
			match bytes_to_str(bytes) {
				Ok(str) => Ok(str)
				Err(_) => Err(InvalidNumeral("invalid numeric literal"))
			}
		}
	}

base256_to_decimal_digits : List(U8) -> List(U8)
base256_to_decimal_digits = |base256_digits| {
	var $decimal_digits = u8_single(0)
	var $index = 0
	len = u8_list_len(base256_digits)

	while $index < len {
		$decimal_digits = decimal_digits_mul_add($decimal_digits, 256, U8.to_u16(u8_list_get_unsafe(base256_digits, $index)))
		$index = $index + 1
	}

	trim_leading_zero_digits($decimal_digits)
}

decimal_digits_mul_add : List(U8), U16, U16 -> List(U8)
decimal_digits_mul_add = |digits, multiplier, addend| {
	var $carry = addend
	var $reversed = u8_list_with_capacity(u8_list_len(digits) + 4)
	var $index = u8_list_len(digits)

	while $index > 0 {
		$index = $index - 1
		digit = u8_list_get_unsafe(digits, $index)
		total = U8.to_u16(digit) * multiplier + $carry
		$carry = total / 10
		$reversed = u8_append($reversed, U16.to_u8_wrap(total % 10))
	}

	u8_rev(decimal_append_carry_reversed($reversed, $carry))
}

decimal_append_carry_reversed : List(U8), U16 -> List(U8)
decimal_append_carry_reversed = |reversed, carry| {
	var $digits = reversed
	var $carry = carry

	while $carry > 0 {
		$digits = u8_append($digits, U16.to_u8_wrap($carry % 10))
		$carry = $carry / 10
	}

	$digits
}

trim_leading_zero_digits : List(U8) -> List(U8)
trim_leading_zero_digits = |digits| {
	len = u8_list_len(digits)
	var $index = 0

	while $index + 1 < len and u8_list_get_unsafe(digits, $index) == 0 {
		$index = $index + 1
	}

	u8_drop_first(digits, $index)
}

decimal_digits_to_bytes : List(U8) -> List(U8)
decimal_digits_to_bytes = |digits| {
	len = u8_list_len(digits)
	var $bytes = u8_list_with_capacity(len)
	var $index = 0

	while $index < len {
		$bytes = u8_append($bytes, u8_list_get_unsafe(digits, $index) + 48)
		$index = $index + 1
	}

	$bytes
}

u8_single : U8 -> List(U8)
u8_single = |byte| u8_append(u8_list_with_capacity(1), byte)

u8_repeat : U8, U64 -> List(U8)
u8_repeat = |byte, count| {
	var $list = u8_list_with_capacity(count)
	var $index = 0

	while $index < count {
		$list = u8_append($list, byte)
		$index = $index + 1
	}

	$list
}

u8_append : List(U8), U8 -> List(U8)
u8_append = |list, byte| u8_list_append_unsafe(u8_list_reserve(list, 1), byte)

u8_concat : List(U8), List(U8) -> List(U8)
u8_concat = |left, right| {
	len = u8_list_len(right)
	var $out = u8_list_reserve(left, len)
	var $index = 0

	while $index < len {
		$out = u8_list_append_unsafe($out, u8_list_get_unsafe(right, $index))
		$index = $index + 1
	}

	$out
}

u8_rev : List(U8) -> List(U8)
u8_rev = |list| {
	len = u8_list_len(list)
	var $out = u8_list_with_capacity(len)
	var $remaining = len

	while $remaining > 0 {
		$remaining = $remaining - 1
		$out = u8_append($out, u8_list_get_unsafe(list, $remaining))
	}

	$out
}

u8_drop_first : List(U8), U64 -> List(U8)
u8_drop_first = |list, count| {
	len = u8_list_len(list)
	var $out = u8_list_with_capacity(len - count)
	var $index = count

	while $index < len {
		$out = u8_append($out, u8_list_get_unsafe(list, $index))
		$index = $index + 1
	}

	$out
}

crypto_digest_byte_len : U64
crypto_digest_byte_len = 32

crypto_digest_hex_len : U64
crypto_digest_hex_len = 64

crypto_digest_from_bytes : List(U8) -> Try(List(U8), Crypto.DigestBytesErr)
crypto_digest_from_bytes = |bytes| {
	actual = u8_list_len(bytes)
	if actual == crypto_digest_byte_len {
		Ok(bytes)
	} else {
		Err(WrongLength({ expected: crypto_digest_byte_len, actual }))
	}
}

crypto_hex_digit : U8 -> U8
crypto_hex_digit = |nibble|
	if nibble < 10 {
		nibble + 48
	} else {
		nibble + 87
	}

crypto_digest_to_hex_bytes : List(U8) -> List(U8)
crypto_digest_to_hex_bytes = |bytes| {
	var $out = u8_list_with_capacity(crypto_digest_hex_len)
	var $index = 0

	while $index < crypto_digest_byte_len {
		byte = u8_list_get_unsafe(bytes, $index)
		$out = u8_append($out, crypto_hex_digit(U8.shift_right_zf_by(byte, 4)))
		$out = u8_append($out, crypto_hex_digit(U8.bitwise_and(byte, 15)))
		$index = $index + 1
	}

	$out
}

crypto_digest_to_hex : List(U8) -> Str
crypto_digest_to_hex = |bytes|
	match bytes_to_str(crypto_digest_to_hex_bytes(bytes)) {
		Ok(hex) => hex
		Err(_) => {
			crash "Crypto hex encoder invariant violated"
		}
	}

crypto_hex_nibble : U8, U64 -> Try(U8, Crypto.DigestHexErr)
crypto_hex_nibble = |byte, index|
	if byte >= 48 and byte <= 57 {
		Ok(byte - 48)
	} else if byte >= 65 and byte <= 70 {
		Ok(byte - 55)
	} else if byte >= 97 and byte <= 102 {
		Ok(byte - 87)
	} else {
		Err(InvalidHex({ index, byte }))
	}

crypto_digest_from_hex : Str -> Try(List(U8), Crypto.DigestHexErr)
crypto_digest_from_hex = |hex| {
	bytes = Str.to_utf8(hex)
	actual = u8_list_len(bytes)
	if actual != crypto_digest_hex_len {
		return Err(WrongLength({ expected: crypto_digest_hex_len, actual }))
	}

	var $out = u8_list_with_capacity(crypto_digest_byte_len)
	var $index = 0

	while $index < crypto_digest_hex_len {
		high = crypto_hex_nibble(u8_list_get_unsafe(bytes, $index), $index)?
		low = crypto_hex_nibble(u8_list_get_unsafe(bytes, $index + 1), $index + 1)?
		$out = u8_append($out, U8.bitwise_or(U8.shift_left_by(high, 4), low))
		$index = $index + 2
	}

	Ok($out)
}

u8_list_len : List(U8) -> U64

u8_list_with_capacity : U64 -> List(U8)

u8_list_get_unsafe : List(U8), U64 -> U8

u8_list_append_unsafe : List(U8), U8 -> List(U8)

u8_list_reserve : List(U8), U64 -> List(U8)

dec_sqrt_unsafe : Dec -> Dec

dec_pow_unsafe : Dec, Dec -> Dec

dec_sin_unsafe : Dec -> Dec

dec_cos_unsafe : Dec -> Dec

dec_tan_unsafe : Dec -> Dec

dec_asin_unsafe : Dec -> Dec

dec_acos_unsafe : Dec -> Dec

dec_atan_unsafe : Dec -> Dec

out_of_range_try : { success : U8, val_or_memory_garbage : item } -> Try(item, [OutOfRange])
out_of_range_try = |answer|
	if answer.success != 0 {
		Ok(answer.val_or_memory_garbage)
	} else {
		Err(OutOfRange)
	}

out_of_range_or_crash : Try(item, [OutOfRange]) -> item
out_of_range_or_crash = |answer|
	match answer {
		Ok(value) => value
		Err(OutOfRange) => {
			crash "number is out of range"
		}
	}

dec_round_to_whole : Dec -> Dec
dec_round_to_whole = |self| {
	truncated = Dec.div_trunc_by(self, 1.0)
	remainder = self - truncated
	if remainder >= 0.5 {
		truncated + 1.0
	} else if remainder <= -0.5 {
		truncated - 1.0
	} else {
		truncated
	}
}

dec_floor_to_whole : Dec -> Dec
dec_floor_to_whole = |self| {
	truncated = Dec.div_trunc_by(self, 1.0)
	if self < truncated {
		truncated - 1.0
	} else {
		truncated
	}
}

dec_ceiling_to_whole : Dec -> Dec
dec_ceiling_to_whole = |self| {
	truncated = Dec.div_trunc_by(self, 1.0)
	if self > truncated {
		truncated + 1.0
	} else {
		truncated
	}
}

f32_round_to_whole : F32 -> F32
f32_round_to_whole = |self| {
	if self >= 0 {
		f32_floor_unsafe(self + 0.5)
	} else {
		f32_ceiling_unsafe(self - 0.5)
	}
}

f64_round_to_whole : F64 -> F64
f64_round_to_whole = |self| {
	if self >= 0 {
		f64_floor_unsafe(self + 0.5)
	} else {
		f64_ceiling_unsafe(self - 0.5)
	}
}

digits_to_bytes : List(U8) -> Try(List(U8), [OutOfRange])
digits_to_bytes = |digits|
	List.fold(
		digits,
		Ok([]),
		|state, digit|
			match state {
				Err(OutOfRange) => Err(OutOfRange)
				Ok(bytes) =>
					if digit > 9 {
						Err(OutOfRange)
					} else {
						Ok(List.append(bytes, digit + 48))
					}
				},
	)

int_from_digits : List(U8), (Str -> Try(item, err)) -> Try(item, [OutOfRange])
int_from_digits = |digits, parse|
	match digits_to_str(digits) {
		Ok(str) =>
			match parse(str) {
				Ok(num) => Ok(num)
				Err(_) => Err(OutOfRange)
			}
		Err(OutOfRange) => Err(OutOfRange)
	}

dec_from_digits : (List(U8), List(U8)), (Str -> Try(item, err)) -> Try(item, [OutOfRange])
dec_from_digits = |digits, parse| {
	(int_digits, frac_digits) = digits

	match digits_to_bytes(int_digits) {
		Err(OutOfRange) => Err(OutOfRange)
		Ok(int_bytes) =>
			match digits_to_bytes(frac_digits) {
				Err(OutOfRange) => Err(OutOfRange)
				Ok(frac_bytes) => {
					bytes = List.concat(List.concat(int_bytes, [46]), frac_bytes)
					match bytes_to_str(bytes) {
						Ok(str) =>
							match parse(str) {
								Ok(num) => Ok(num)
								Err(_) => Err(OutOfRange)
							}
						Err(OutOfRange) => Err(OutOfRange)
					}
				}
			}
		}
}

digits_to_str : List(U8) -> Try(Str, [OutOfRange])
digits_to_str = |digits|
	match digits_to_bytes(digits) {
		Err(OutOfRange) => Err(OutOfRange)
		Ok(bytes) => bytes_to_str(bytes)
	}

bytes_to_str : List(U8) -> Try(Str, [OutOfRange])
bytes_to_str = |bytes|
	match Str.from_utf8(bytes) {
		Ok(str) => Ok(str)
		Err(_) => Err(OutOfRange)
	}

unsigned_add_try : item, item, item -> Try(item, [Overflow, ..])
	where [item.is_gt : item, item -> Bool, item.minus : item, item -> item, item.plus : item, item -> item]
unsigned_add_try = |highest, a, b|
	if a > highest - b {
		Err(Overflow)
	} else {
		Ok(a + b)
	}

unsigned_sub_try : item, item -> Try(item, [Overflow, ..])
	where [item.is_lt : item, item -> Bool, item.minus : item, item -> item]
unsigned_sub_try = |a, b|
	if a < b {
		Err(Overflow)
	} else {
		Ok(a - b)
	}

unsigned_mul_try : item, item, item, item -> Try(item, [Overflow, ..])
	where [item.is_eq : item, item -> Bool, item.is_gt : item, item -> Bool, item.div_by : item, item -> item, item.times : item, item -> item]
unsigned_mul_try = |highest, zero, a, b|
	if b == zero {
		Ok(zero)
	} else if a > highest / b {
		Err(Overflow)
	} else {
		Ok(a * b)
	}

unsigned_div_try : item, item, item -> Try(item, [DivByZero, ..])
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item]
unsigned_div_try = |zero, a, b|
	if b == zero {
		Err(DivByZero)
	} else {
		Ok(a / b)
	}

signed_add_try : item, item, item, item, item -> Try(item, [Overflow, ..])
	where [item.is_gt : item, item -> Bool, item.is_lt : item, item -> Bool, item.plus : item, item -> item, item.minus : item, item -> item]
signed_add_try = |lowest, highest, zero, a, b|
	if b > zero {
		if a > highest - b {
			Err(Overflow)
		} else {
			Ok(a + b)
		}
	} else if b < zero {
		if a < lowest - b {
			Err(Overflow)
		} else {
			Ok(a + b)
		}
	} else {
		Ok(a)
	}

signed_sub_try : item, item, item, item, item -> Try(item, [Overflow, ..])
	where [item.is_gt : item, item -> Bool, item.is_lt : item, item -> Bool, item.plus : item, item -> item, item.minus : item, item -> item]
signed_sub_try = |lowest, highest, zero, a, b|
	if b > zero {
		if a < lowest + b {
			Err(Overflow)
		} else {
			Ok(a - b)
		}
	} else if b < zero {
		if a > highest + b {
			Err(Overflow)
		} else {
			Ok(a - b)
		}
	} else {
		Ok(a)
	}

signed_mul_try : item, item, item, item, item, item -> Try(item, [Overflow, ..])
	where [
		item.is_gt : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.is_eq : item, item -> Bool,
		item.minus : item, item -> item,
		item.times : item, item -> item,
		item.div_trunc_by : item, item -> item,
	]
signed_mul_try = |lowest, highest, zero, neg_one, a, b|
	if a == zero {
		Ok(zero)
	} else if b == zero {
		Ok(zero)
	} else if a == neg_one {
		if b == lowest {
			Err(Overflow)
		} else {
			Ok(zero - b)
		}
	} else if b == neg_one {
		if a == lowest {
			Err(Overflow)
		} else {
			Ok(zero - a)
		}
	} else if a > zero {
		if b > zero {
			if a > highest.div_trunc_by(b) {
				Err(Overflow)
			} else {
				Ok(a * b)
			}
		} else if b < lowest.div_trunc_by(a) {
			Err(Overflow)
		} else {
			Ok(a * b)
		}
	} else if b > zero {
		if a < lowest.div_trunc_by(b) {
			Err(Overflow)
		} else {
			Ok(a * b)
		}
	} else if a < highest.div_trunc_by(b) {
		Err(Overflow)
	} else {
		Ok(a * b)
	}

signed_div_try : item, item, item, item, item -> Try(item, [DivByZero, Overflow, ..])
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item]
signed_div_try = |lowest, zero, neg_one, a, b|
	if b == zero {
		Err(DivByZero)
	} else if a == lowest {
		if b == neg_one {
			Err(Overflow)
		} else {
			Ok(a / b)
		}
	} else {
		Ok(a / b)
	}

unsigned_pow_try : item, item, item, item, item, item -> Try(item, [Overflow, ..])
	where [
		item.is_eq : item, item -> Bool,
		item.is_gt : item, item -> Bool,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
		item.times : item, item -> item,
	]
unsigned_pow_try = |highest, zero, one, two, base, exponent|
	unsigned_pow_try_step(highest, zero, one, two, one, base, exponent)

unsigned_pow_try_step : item, item, item, item, item, item, item -> Try(item, [Overflow, ..])
	where [
		item.is_eq : item, item -> Bool,
		item.is_gt : item, item -> Bool,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
		item.times : item, item -> item,
	]
unsigned_pow_try_step = |highest, zero, one, two, acc, base, exponent|
	if exponent == zero {
		Ok(acc)
	} else {
		next_acc = if exponent.rem_by(two) == zero {
			Ok(acc)
		} else {
			unsigned_mul_try(highest, zero, acc, base)
		}

		match next_acc {
			Err(Overflow) => Err(Overflow)
			Ok(updated_acc) => {
				next_exponent = exponent / two
				if next_exponent == zero {
					Ok(updated_acc)
				} else {
					match unsigned_mul_try(highest, zero, base, base) {
						Err(Overflow) => Err(Overflow)
						Ok(updated_base) => unsigned_pow_try_step(highest, zero, one, two, updated_acc, updated_base, next_exponent)
					}
				}
			}
		}
	}

signed_pow_try : item, item, item, item, item, item, item, item -> Try(item, [Overflow, Underflow, ..])
	where [
		item.is_eq : item, item -> Bool,
		item.is_gt : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.div_trunc_by : item, item -> item,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
		item.minus : item, item -> item,
		item.times : item, item -> item,
	]
signed_pow_try = |lowest, highest, zero, one, two, neg_one, base, exponent|
	if exponent < zero {
		if base == one {
			Ok(one)
		} else if base == neg_one {
			if exponent.rem_by(two) == zero {
				Ok(one)
			} else {
				Ok(neg_one)
			}
		} else {
			Err(Underflow)
		}
	} else {
		signed_pow_try_step(lowest, highest, zero, one, two, neg_one, one, base, exponent)
	}

signed_pow_try_step : item, item, item, item, item, item, item, item, item -> Try(item, [Overflow, Underflow, ..])
	where [
		item.is_eq : item, item -> Bool,
		item.is_gt : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.div_trunc_by : item, item -> item,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
		item.minus : item, item -> item,
		item.times : item, item -> item,
	]
signed_pow_try_step = |lowest, highest, zero, one, two, neg_one, acc, base, exponent|
	if exponent == zero {
		Ok(acc)
	} else {
		next_acc = if exponent.rem_by(two) == zero {
			Ok(acc)
		} else {
			match signed_mul_try(lowest, highest, zero, neg_one, acc, base) {
				Ok(result) => Ok(result)
				Err(Overflow) => Err(Overflow)
			}
		}

		match next_acc {
			Err(Overflow) => Err(Overflow)
			Err(Underflow) => Err(Underflow)
			Ok(updated_acc) => {
				next_exponent = exponent / two
				if next_exponent == zero {
					Ok(updated_acc)
				} else {
					match signed_mul_try(lowest, highest, zero, neg_one, base, base) {
						Err(Overflow) => Err(Overflow)
						Ok(updated_base) => signed_pow_try_step(lowest, highest, zero, one, two, neg_one, updated_acc, updated_base, next_exponent)
					}
				}
			}
		}
	}

unsigned_div_ceil_try : item, item, item, item -> Try(item, [DivByZero, ..])
	where [
		item.is_eq : item, item -> Bool,
		item.plus : item, item -> item,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
	]
unsigned_div_ceil_try = |zero, one, a, b|
	match unsigned_div_try(zero, a, b) {
		Err(DivByZero) => Err(DivByZero)
		Ok(quotient) =>
			if a.rem_by(b) == zero {
				Ok(quotient)
			} else {
				Ok(quotient + one)
			}
		}

signed_div_ceil_try : item, item, item, item, item, item, item -> Try(item, [DivByZero, Overflow, ..])
	where [
		item.is_eq : item, item -> Bool,
		item.is_gt : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.plus : item, item -> item,
		item.minus : item, item -> item,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
	]
signed_div_ceil_try = |lowest, highest, zero, one, neg_one, a, b|
	match signed_div_try(lowest, zero, neg_one, a, b) {
		Err(DivByZero) => Err(DivByZero)
		Err(Overflow) => Err(Overflow)
		Ok(quotient) =>
			if a.rem_by(b) == zero {
				Ok(quotient)
			} else if a > zero {
				if b > zero {
					match signed_add_try(lowest, highest, zero, quotient, one) {
						Ok(result) => Ok(result)
						Err(Overflow) => Err(Overflow)
					}
				} else {
					Ok(quotient)
				}
			} else if a < zero {
				if b < zero {
					match signed_add_try(lowest, highest, zero, quotient, one) {
						Ok(result) => Ok(result)
						Err(Overflow) => Err(Overflow)
					}
				} else {
					Ok(quotient)
				}
			} else {
				Ok(quotient)
			}
		}

unsigned_minus_saturated : item, item, item -> item
	where [item.is_lt : item, item -> Bool, item.minus : item, item -> item]
unsigned_minus_saturated = |zero, a, b|
	if a < b {
		zero
	} else {
		a - b
	}

signed_minus_saturated : item, item, item, item, item -> item
	where [item.is_gt : item, item -> Bool, item.is_lt : item, item -> Bool, item.plus : item, item -> item, item.minus : item, item -> item]
signed_minus_saturated = |lowest, highest, zero, a, b|
	match signed_sub_try(lowest, highest, zero, a, b) {
		Ok(result) => result
		Err(Overflow) =>
			if b > zero {
				lowest
			} else {
				highest
			}
		}

unsigned_times_saturated : item, item, item, item -> item
	where [item.is_eq : item, item -> Bool, item.is_gt : item, item -> Bool, item.div_by : item, item -> item, item.times : item, item -> item]
unsigned_times_saturated = |highest, zero, a, b|
	match unsigned_mul_try(highest, zero, a, b) {
		Ok(result) => result
		Err(Overflow) => highest
	}

signed_times_saturated : item, item, item, item, item, item -> item
	where [
		item.is_gt : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.is_eq : item, item -> Bool,
		item.minus : item, item -> item,
		item.times : item, item -> item,
		item.div_trunc_by : item, item -> item,
	]
signed_times_saturated = |lowest, highest, zero, neg_one, a, b|
	match signed_mul_try(lowest, highest, zero, neg_one, a, b) {
		Ok(result) => result
		Err(Overflow) =>
			if a < zero {
				if b > zero {
					lowest
				} else {
					highest
				}
			} else if b < zero {
				lowest
			} else {
				highest
			}
		}

unsigned_count_leading_zero_bits : U8, item, item, item -> U8
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item]
unsigned_count_leading_zero_bits = |width, zero, two, value| width - unsigned_bit_length(zero, two, value, 0)

unsigned_bit_length : item, item, item, U8 -> U8
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item]
unsigned_bit_length = |zero, two, value, count|
	if value == zero {
		count
	} else {
		unsigned_bit_length(zero, two, value / two, count + 1)
	}

signed_count_leading_zero_bits : U8, item, item, item -> U8
	where [
		item.is_eq : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.div_by : item, item -> item,
	]
signed_count_leading_zero_bits = |width, zero, two, value|
	if value < zero {
		0
	} else {
		unsigned_count_leading_zero_bits(width, zero, two, value)
	}

integer_count_trailing_zero_bits : U8, item, item, item -> U8
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item, item.rem_by : item, item -> item]
integer_count_trailing_zero_bits = |width, zero, two, value|
	if value == zero {
		width
	} else {
		integer_count_trailing_zero_bits_step(zero, two, value, 0)
	}

integer_count_trailing_zero_bits_step : item, item, item, U8 -> U8
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item, item.rem_by : item, item -> item]
integer_count_trailing_zero_bits_step = |zero, two, value, count|
	if value.rem_by(two) != zero {
		count
	} else {
		integer_count_trailing_zero_bits_step(zero, two, value / two, count + 1)
	}

unsigned_count_one_bits : item, item, item -> U8
	where [item.is_eq : item, item -> Bool, item.div_by : item, item -> item, item.rem_by : item, item -> item]
unsigned_count_one_bits = |zero, two, value|
	if value == zero {
		0
	} else {
		bit = if value.rem_by(two) == zero {
			0
		} else {
			1
		}
		bit + unsigned_count_one_bits(zero, two, value / two)
	}

signed_count_one_bits : U8, item, item, item -> U8
	where [
		item.is_eq : item, item -> Bool,
		item.is_lt : item, item -> Bool,
		item.div_by : item, item -> item,
		item.rem_by : item, item -> item,
		item.bitwise_not : item -> item,
	]
signed_count_one_bits = |width, zero, two, value|
	if value < zero {
		width - unsigned_count_one_bits(zero, two, value.bitwise_not())
	} else {
		unsigned_count_one_bits(zero, two, value)
	}

integer_is_even : item, item, item -> Bool
	where [item.is_eq : item, item -> Bool, item.rem_by : item, item -> item]
integer_is_even = |zero, two, value| value.rem_by(two) == zero

integer_is_odd : item, item, item -> Bool
	where [item.is_eq : item, item -> Bool, item.rem_by : item, item -> item]
integer_is_odd = |zero, two, value| value.rem_by(two) != zero

unsigned_is_multiple_of : item, item, item -> Bool
	where [item.is_eq : item, item -> Bool, item.rem_by : item, item -> item]
unsigned_is_multiple_of = |zero, value, divisor|
	if divisor == zero {
		value == zero
	} else {
		value.rem_by(divisor) == zero
	}

signed_is_multiple_of : item, item, item, item -> Bool
	where [item.is_eq : item, item -> Bool, item.rem_by : item, item -> item]
signed_is_multiple_of = |zero, neg_one, value, divisor|
	if divisor == zero {
		value == zero
	} else if divisor == neg_one {
		True
	} else {
		value.rem_by(divisor) == zero
	}

numeric_compare : item, item -> [LT, EQ, GT]

range_done : () -> Iter(item)
range_done = || {
	len_if_known: Known(0),
	step: || Done,
}

# Implemented by the compiler, does not perform bounds checks
list_get_unsafe : List(item), U64 -> item

# Implemented by the compiler, does not perform bounds checks
list_append_unsafe : List(item), item -> List(item)

# Implemented by the compiler, does not perform bounds checks
list_set_unsafe : List(item), U64, item -> List(item)

# Implemented by the compiler, does not perform bounds checks.
# Returns the new list paired with the value that was replaced.
list_replace_unsafe : List(item), U64, item -> { list : List(item), prev : item }

# Implemented by the compiler, does not perform bounds checks
list_swap_unsafe : List(item), U64, U64 -> List(item)

# Implemented by the compiler. Returns string slices into the source string.
str_find_first_raw : Str, Str -> { before : Str, found : Bool, after : Str }

# Implemented by the compiler. Returns a string slice after the prefix when the
# source starts with the prefix using ASCII-caseless comparison.
str_drop_prefix_caseless_ascii_raw : Str, Str -> { after : Str, found : Bool }

# Implemented by the compiler. Returns 1 (otherwise 0) when List.map may reuse
# the input list's allocation for its output: the input and output element
# layouts are interchangeable, and at runtime the list is uniquely owned and
# not a seamless slice. Lowered to a constant 0 when the layouts are not
# interchangeable, which lets lowering drop the in-place branch entirely.
# Note: success is U8 (0 = false, 1 = true) since Bool is not available at top level
list_map_can_reuse : List(input), (input -> output) -> U8

# Implemented by the compiler. Retypes a unique, non-slice list in place so
# List.map can overwrite its elements without allocating a new list. Must only
# be called on a list for which list_map_can_reuse returned 1.
list_map_cast_unsafe : List(input) -> List(output)

# Implemented by the compiler. Moves ownership of the element at the given
# index out of the list's buffer; the slot keeps stale bytes until
# list_map_write_unsafe stores its replacement. No bounds checks.
list_map_extract_unsafe : List(output), U64 -> input

# Implemented by the compiler. Stores an owned element into the slot at the
# given index, which must have been vacated by list_map_extract_unsafe.
# No bounds checks.
list_map_write_unsafe : List(output), U64, output -> List(output)

# Implemented by the compiler, ensures at least spare additional elements of capacity
list_reserve : List(item), U64 -> List(item)

# Implemented by the compiler, trims unused list capacity
list_release_excess_capacity : List(item) -> List(item)

# Unsafe conversion functions - these return simple records instead of Try types
# They are low-level operations that get replaced by the compiler
# Note: success is U8 (0 = false, 1 = true) since Bool is not available at top level
u128_to_dec_try_unsafe : U128 -> { success : U8, val_or_memory_garbage : Dec }

i128_to_dec_try_unsafe : I128 -> { success : U8, val_or_memory_garbage : Dec }

dec_to_i8_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : I8 }

dec_to_i16_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : I16 }

dec_to_i32_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : I32 }

dec_to_i64_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : I64 }

dec_to_i128_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : I128 }

dec_to_u8_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : U8 }

dec_to_u16_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : U16 }

dec_to_u32_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : U32 }

dec_to_u64_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : U64 }

dec_to_u128_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : U128 }

dec_to_f32_try_unsafe : Dec -> { success : U8, val_or_memory_garbage : F32 }

f32_to_i8_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : I8 }

f32_to_i16_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : I16 }

f32_to_i32_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : I32 }

f32_to_i64_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : I64 }

f32_to_i128_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : I128 }

f32_to_u8_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : U8 }

f32_to_u16_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : U16 }

f32_to_u32_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : U32 }

f32_to_u64_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : U64 }

f32_to_u128_try_unsafe : F32 -> { success : U8, val_or_memory_garbage : U128 }

f32_floor_unsafe : F32 -> F32

f32_ceiling_unsafe : F32 -> F32

f32_sqrt_unsafe : F32 -> F32

f32_pow_unsafe : F32, F32 -> F32

f32_sin_unsafe : F32 -> F32

f32_cos_unsafe : F32 -> F32

f32_tan_unsafe : F32 -> F32

f32_asin_unsafe : F32 -> F32

f32_acos_unsafe : F32 -> F32

f32_atan_unsafe : F32 -> F32

f64_to_i8_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : I8 }

f64_to_i16_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : I16 }

f64_to_i32_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : I32 }

f64_to_i64_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : I64 }

f64_to_i128_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : I128 }

f64_to_u8_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : U8 }

f64_to_u16_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : U16 }

f64_to_u32_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : U32 }

f64_to_u64_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : U64 }

f64_to_u128_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : U128 }

f64_to_f32_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : F32 }

f64_floor_unsafe : F64 -> F64

f64_ceiling_unsafe : F64 -> F64

f64_sqrt_unsafe : F64 -> F64

f64_pow_unsafe : F64, F64 -> F64

f64_sin_unsafe : F64 -> F64

f64_cos_unsafe : F64 -> F64

f64_tan_unsafe : F64 -> F64

f64_asin_unsafe : F64 -> F64

f64_acos_unsafe : F64 -> F64

f64_atan_unsafe : F64 -> F64
