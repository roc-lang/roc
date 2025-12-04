Builtin :: [].{
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
		concat : Str, Str -> Str
		contains : Str, Str -> Bool
		trim : Str -> Str
		trim_start : Str -> Str
		trim_end : Str -> Str
		caseless_ascii_equals : Str, Str -> Bool
		with_ascii_lowercased : Str -> Str
		with_ascii_uppercased : Str -> Str
		starts_with : Str, Str -> Bool
		ends_with : Str, Str -> Bool
		repeat : Str, U64 -> Str
		with_prefix : Str, Str -> Str
		drop_prefix : Str, Str -> Str
		drop_suffix : Str, Str -> Str
		count_utf8_bytes : Str -> U64
		with_capacity : U64 -> Str
		reserve : Str, U64 -> Str
		release_excess_capacity : Str -> Str
		to_utf8 : Str -> List(U8)
		from_utf8_lossy : List(U8) -> Str
		from_utf8 : List(U8) -> Try(Str, [BadUtf8({ problem : Str.Utf8Problem, index : U64 }), ..others])
		split_on : Str, Str -> List(Str)
		join_with : List(Str), Str -> Str

		is_eq : Str, Str -> Bool
	}

	List(_item) :: [ProvidedByCompiler].{
		len : List(_item) -> U64
		is_empty : List(_item) -> Bool
		concat : List(item), List(item) -> List(item)
		with_capacity : U64 -> List(item)
		sort_with : List(item), (item, item -> [LT, EQ, GT]) -> List(item)

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

		append : List(a), a -> List(a)

		first : List(item) -> Try(item, [ListWasEmpty, ..others])
		first = |list| if List.is_empty(list) {
			Try.Err(ListWasEmpty)
		} else {
			Try.Ok(list_get_unsafe(list, 0))
		}

		get : List(item), U64 -> Try(item, [OutOfBounds, ..others])
		get = |list, index| if index < List.len(list) {
			Try.Ok(list_get_unsafe(list, index))
		} else {
			Try.Err(OutOfBounds)
		}

		for_each! : List(item), (item => {}) => {}
		for_each! = |items, cb!| for item in items {
			cb!(item)
		}

		map : List(a), (a -> b) -> List(b)
		map = |list, transform|
		# Implement using fold + concat for now
		# TODO: Optimize with in-place update when list is unique and element sizes match
			List.fold(list, [], |acc, item| List.concat(acc, [transform(item)]))

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

		fold : List(item), state, (state, item -> state) -> state
		fold = |list, init, step| {
			var $state = init

			for item in list {
				$state = step($state, item)
			}

			$state
		}

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

		any : List(a), (a -> Bool) -> Bool
		any = |list, predicate| {
			for item in list {
				if predicate(item) {
					return True
				}
			}
			False
		}

		contains : List(a), a -> Bool where [a.is_eq : a, a -> Bool]
		contains = |list, elt| {
			List.any(list, |x| x == elt)
		}

		all : List(a), (a -> Bool) -> Bool
		all = |list, predicate| {
			for item in list {
				if Bool.not(predicate(item)) {
					return False
				}
			}
			True
		}

		last : List(item) -> Try(item, [ListWasEmpty, ..others])
		last = |list| if List.is_empty(list) {
			Try.Err(ListWasEmpty)
		} else {
			Try.Ok(list_get_unsafe(list, List.len(list) - 1))
		}

		single : item -> List(item)
		single = |x| [x]

		drop_at : List(a), U64 -> List(a)

		sublist : List(a), { start : U64, len : U64 } -> List(a)

		take_first : List(a), U64 -> List(a)
		take_first = |list, n| {
			List.sublist(list, { len: n, start: 0 })
		}

		take_last : List(a), U64 -> List(a)
		take_last = |list, n| {
			len = List.len(list)
			start = if (len <= n) 0 else len - n
			List.sublist(list, { start: start, len: len })
		}

		drop_first : List(a), U64 -> List(a)
		drop_first = |list, n| {
			len = List.len(list)
			List.sublist(list, { start: n, len: len })
		}

		drop_last : List(a), U64 -> List(a)
		drop_last = |list, n| {
			len = List.len(list)
			take_len = if (len <= n) 0 else len - n
			List.sublist(list, { start: 0, len: take_len })
		}
	}

	Bool := [False, True].{
		not : Bool -> Bool
		not = |bool| match bool {
			Bool.True => Bool.False
			Bool.False => Bool.True
		}

		is_eq : Bool, Bool -> Bool

		# encoder : Bool -> Encoder(fmt, [])
		# 	where [fmt implements EncoderFormatting]
		# encoder =

		# Encoder fmt := List U8, fmt -> List U8 where fmt implements EncoderFormatting
	}

	Box(item) :: [ProvidedByCompiler].{}

	Try(ok, err) := [Ok(ok), Err(err)].{
		is_ok : Try(_ok, _err) -> Bool
		is_ok = |try| match try {
			Ok(_) => True
			Err(_) => False
		}

		is_err : Try(_ok, _err) -> Bool
		is_err = |try| match try {
			Ok(_) => False
			Err(_) => True
		}

		ok_or : Try(ok, _err), ok -> ok
		ok_or = |try, fallback| match try {
			Ok(val) => val
			Err(_) => fallback
		}

		err_or : Try(_ok, err), err -> err
		err_or = |try, fallback| match try {
			Err(val) => val
			Ok(_) => fallback
		}

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
	}

	Dict :: [EmptyDict].{}

	Set(item) :: [].{
		is_eq : Set(item), Set(item) -> Bool
		is_eq = |_a, _b| Bool.False
	}

	Num :: {}.{
		Numeral :: [
			Self(
				{ # TODO get rid of the "Self" wrapper once we have nominal records"
					# True iff there was a minus sign in front of the literal
					is_negative : Bool,
					# Base-256 digits before and after the decimal point, with any underscores
					# and leading/trailing zeros removed from the source code.
					#
					# Example: If I write "0356.5170" in the source file, that will be:
					# - [1, 100] before the pt, because in base-256, 356 = (1 * 256^1) + (100 * 256^0)
					# - [2, 5] after the pt, because in base-256, 517 = (2 * 256^1) + (5 * 256^0)
					#
					# This design compactly represents the digits without wasting any memory
					# (because base-256 stores each digit using every single bit of the U8), and also
					# allows arbitrary digit length so that userspace custom number types can work with
					# arbitrarily long number literals as long as the number types can support them.
					digits_before_pt : List(U8),
					digits_after_pt : List(U8),
				},
			),
		].{
			is_negative : Numeral -> Bool
			is_negative = |self| match self {
				# TODO make this a nominal record once we have those
				Self({ is_negative: neg, digits_before_pt: _, digits_after_pt: _ }) => neg
			}
		}

		U8 :: [].{
			to_str : U8 -> Str
			is_zero : U8 -> Bool
			is_eq : U8, U8 -> Bool
			is_gt : U8, U8 -> Bool
			is_gte : U8, U8 -> Bool
			is_lt : U8, U8 -> Bool
			is_lte : U8, U8 -> Bool

			plus : U8, U8 -> U8
			minus : U8, U8 -> U8
			times : U8, U8 -> U8
			div_by : U8, U8 -> U8
			div_trunc_by : U8, U8 -> U8
			rem_by : U8, U8 -> U8
			mod_by : U8, U8 -> U8
			abs_diff : U8, U8 -> U8

			from_int_digits : List(U8) -> Try(U8, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(U8, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(U8, [BadNumStr, ..others])

			# # List of integers beginning with this `U8` and ending with the other `U8`.
			# # (Use [until] instead to end with the other `U8` minus one.)
			# # Returns an empty list if this `U8` is greater than the other.
			to : U8, U8 -> List(U8)
			to = |start, end| range_to(start, end)

			# # List of integers beginning with this `U8` and ending with the other `U8` minus one.
			# # (Use [to] instead to end with the other `U8` exactly, instead of minus one.)
			# # Returns an empty list if this `U8` is greater than or equal to the other.
			until : U8, U8 -> List(U8)
			until = |start, end| range_until(start, end)

			# Conversions to signed integers (I8 is lossy, others are safe)
			to_i8_wrap : U8 -> I8
			to_i8_try : U8 -> Try(I8, [OutOfRange, ..others])
			to_i16 : U8 -> I16
			to_i32 : U8 -> I32
			to_i64 : U8 -> I64
			to_i128 : U8 -> I128

			# Conversions to unsigned integers (all safe widening)
			to_u16 : U8 -> U16
			to_u32 : U8 -> U32
			to_u64 : U8 -> U64
			to_u128 : U8 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U8 -> F32
			to_f64 : U8 -> F64
			to_dec : U8 -> Dec
		}

		I8 :: [].{
			to_str : I8 -> Str
			is_zero : I8 -> Bool
			is_negative : I8 -> Bool
			is_positive : I8 -> Bool
			is_eq : I8, I8 -> Bool
			is_gt : I8, I8 -> Bool
			is_gte : I8, I8 -> Bool
			is_lt : I8, I8 -> Bool
			is_lte : I8, I8 -> Bool

			negate : I8 -> I8
			abs : I8 -> I8
			plus : I8, I8 -> I8
			minus : I8, I8 -> I8
			times : I8, I8 -> I8
			div_by : I8, I8 -> I8
			div_trunc_by : I8, I8 -> I8
			rem_by : I8, I8 -> I8
			mod_by : I8, I8 -> I8
			abs_diff : I8, I8 -> U8

			from_int_digits : List(U8) -> Try(I8, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(I8, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(I8, [BadNumStr, ..others])

			# Conversions to signed integers (all safe widening)
			to_i16 : I8 -> I16
			to_i32 : I8 -> I32
			to_i64 : I8 -> I64
			to_i128 : I8 -> I128

			# Conversions to unsigned integers (all lossy for negative values)
			to_u8_wrap : I8 -> U8
			to_u8_try : I8 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : I8 -> U16
			to_u16_try : I8 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : I8 -> U32
			to_u32_try : I8 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : I8 -> U64
			to_u64_try : I8 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : I8 -> U128
			to_u128_try : I8 -> Try(U128, [OutOfRange, ..others])

			# Conversions to floating point (all safe)
			to_f32 : I8 -> F32
			to_f64 : I8 -> F64
			to_dec : I8 -> Dec
		}

		U16 :: [].{
			to_str : U16 -> Str
			is_zero : U16 -> Bool
			is_eq : U16, U16 -> Bool
			is_gt : U16, U16 -> Bool
			is_gte : U16, U16 -> Bool
			is_lt : U16, U16 -> Bool
			is_lte : U16, U16 -> Bool

			plus : U16, U16 -> U16
			minus : U16, U16 -> U16
			times : U16, U16 -> U16
			div_by : U16, U16 -> U16
			div_trunc_by : U16, U16 -> U16
			rem_by : U16, U16 -> U16
			mod_by : U16, U16 -> U16
			abs_diff : U16, U16 -> U16

			from_int_digits : List(U8) -> Try(U16, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(U16, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(U16, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : U16 -> I8
			to_i8_try : U16 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : U16 -> I16
			to_i16_try : U16 -> Try(I16, [OutOfRange, ..others])
			to_i32 : U16 -> I32
			to_i64 : U16 -> I64
			to_i128 : U16 -> I128

			# Conversions to unsigned integers
			to_u8_wrap : U16 -> U8
			to_u8_try : U16 -> Try(U8, [OutOfRange, ..others])
			to_u32 : U16 -> U32
			to_u64 : U16 -> U64
			to_u128 : U16 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U16 -> F32
			to_f64 : U16 -> F64
			to_dec : U16 -> Dec
		}

		I16 :: [].{
			to_str : I16 -> Str
			is_zero : I16 -> Bool
			is_negative : I16 -> Bool
			is_positive : I16 -> Bool
			is_eq : I16, I16 -> Bool
			is_gt : I16, I16 -> Bool
			is_gte : I16, I16 -> Bool
			is_lt : I16, I16 -> Bool
			is_lte : I16, I16 -> Bool

			negate : I16 -> I16
			abs : I16 -> I16
			plus : I16, I16 -> I16
			minus : I16, I16 -> I16
			times : I16, I16 -> I16
			div_by : I16, I16 -> I16
			div_trunc_by : I16, I16 -> I16
			rem_by : I16, I16 -> I16
			mod_by : I16, I16 -> I16
			abs_diff : I16, I16 -> U16

			from_int_digits : List(U8) -> Try(I16, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(I16, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(I16, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : I16 -> I8
			to_i8_try : I16 -> Try(I8, [OutOfRange, ..others])
			to_i32 : I16 -> I32
			to_i64 : I16 -> I64
			to_i128 : I16 -> I128

			# Conversions to unsigned integers (all lossy for negative values)
			to_u8_wrap : I16 -> U8
			to_u8_try : I16 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : I16 -> U16
			to_u16_try : I16 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : I16 -> U32
			to_u32_try : I16 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : I16 -> U64
			to_u64_try : I16 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : I16 -> U128
			to_u128_try : I16 -> Try(U128, [OutOfRange, ..others])

			# Conversions to floating point (all safe)
			to_f32 : I16 -> F32
			to_f64 : I16 -> F64
			to_dec : I16 -> Dec
		}

		U32 :: [].{
			to_str : U32 -> Str
			is_zero : U32 -> Bool
			is_eq : U32, U32 -> Bool
			is_gt : U32, U32 -> Bool
			is_gte : U32, U32 -> Bool
			is_lt : U32, U32 -> Bool
			is_lte : U32, U32 -> Bool

			plus : U32, U32 -> U32
			minus : U32, U32 -> U32
			times : U32, U32 -> U32
			div_by : U32, U32 -> U32
			div_trunc_by : U32, U32 -> U32
			rem_by : U32, U32 -> U32
			mod_by : U32, U32 -> U32
			abs_diff : U32, U32 -> U32

			from_int_digits : List(U8) -> Try(U32, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(U32, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(U32, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : U32 -> I8
			to_i8_try : U32 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : U32 -> I16
			to_i16_try : U32 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : U32 -> I32
			to_i32_try : U32 -> Try(I32, [OutOfRange, ..others])
			to_i64 : U32 -> I64
			to_i128 : U32 -> I128

			# Conversions to unsigned integers
			to_u8_wrap : U32 -> U8
			to_u8_try : U32 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : U32 -> U16
			to_u16_try : U32 -> Try(U16, [OutOfRange, ..others])
			to_u64 : U32 -> U64
			to_u128 : U32 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U32 -> F32
			to_f64 : U32 -> F64
			to_dec : U32 -> Dec
		}

		I32 :: [].{
			to_str : I32 -> Str
			is_zero : I32 -> Bool
			is_negative : I32 -> Bool
			is_positive : I32 -> Bool
			is_eq : I32, I32 -> Bool
			is_gt : I32, I32 -> Bool
			is_gte : I32, I32 -> Bool
			is_lt : I32, I32 -> Bool
			is_lte : I32, I32 -> Bool

			negate : I32 -> I32
			abs : I32 -> I32
			plus : I32, I32 -> I32
			minus : I32, I32 -> I32
			times : I32, I32 -> I32
			div_by : I32, I32 -> I32
			div_trunc_by : I32, I32 -> I32
			rem_by : I32, I32 -> I32
			mod_by : I32, I32 -> I32
			abs_diff : I32, I32 -> U32

			from_int_digits : List(U8) -> Try(I32, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(I32, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(I32, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : I32 -> I8
			to_i8_try : I32 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : I32 -> I16
			to_i16_try : I32 -> Try(I16, [OutOfRange, ..others])
			to_i64 : I32 -> I64
			to_i128 : I32 -> I128

			# Conversions to unsigned integers (all lossy for negative values)
			to_u8_wrap : I32 -> U8
			to_u8_try : I32 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : I32 -> U16
			to_u16_try : I32 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : I32 -> U32
			to_u32_try : I32 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : I32 -> U64
			to_u64_try : I32 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : I32 -> U128
			to_u128_try : I32 -> Try(U128, [OutOfRange, ..others])

			# Conversions to floating point (all safe)
			to_f32 : I32 -> F32
			to_f64 : I32 -> F64
			to_dec : I32 -> Dec
		}

		U64 :: [].{
			to_str : U64 -> Str
			is_zero : U64 -> Bool
			is_eq : U64, U64 -> Bool
			is_gt : U64, U64 -> Bool
			is_gte : U64, U64 -> Bool
			is_lt : U64, U64 -> Bool
			is_lte : U64, U64 -> Bool

			plus : U64, U64 -> U64
			minus : U64, U64 -> U64
			times : U64, U64 -> U64
			div_by : U64, U64 -> U64
			div_trunc_by : U64, U64 -> U64
			rem_by : U64, U64 -> U64
			mod_by : U64, U64 -> U64
			abs_diff : U64, U64 -> U64

			from_int_digits : List(U8) -> Try(U64, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(U64, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(U64, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : U64 -> I8
			to_i8_try : U64 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : U64 -> I16
			to_i16_try : U64 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : U64 -> I32
			to_i32_try : U64 -> Try(I32, [OutOfRange, ..others])
			to_i64_wrap : U64 -> I64
			to_i64_try : U64 -> Try(I64, [OutOfRange, ..others])
			to_i128 : U64 -> I128

			# Conversions to unsigned integers
			to_u8_wrap : U64 -> U8
			to_u8_try : U64 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : U64 -> U16
			to_u16_try : U64 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : U64 -> U32
			to_u32_try : U64 -> Try(U32, [OutOfRange, ..others])
			to_u128 : U64 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U64 -> F32
			to_f64 : U64 -> F64
			to_dec : U64 -> Dec
		}

		I64 :: [].{
			to_str : I64 -> Str
			is_zero : I64 -> Bool
			is_negative : I64 -> Bool
			is_positive : I64 -> Bool
			is_eq : I64, I64 -> Bool
			is_gt : I64, I64 -> Bool
			is_gte : I64, I64 -> Bool
			is_lt : I64, I64 -> Bool
			is_lte : I64, I64 -> Bool

			negate : I64 -> I64
			abs : I64 -> I64
			plus : I64, I64 -> I64
			minus : I64, I64 -> I64
			times : I64, I64 -> I64
			div_by : I64, I64 -> I64
			div_trunc_by : I64, I64 -> I64
			rem_by : I64, I64 -> I64
			mod_by : I64, I64 -> I64
			abs_diff : I64, I64 -> U64

			from_int_digits : List(U8) -> Try(I64, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(I64, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(I64, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : I64 -> I8
			to_i8_try : I64 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : I64 -> I16
			to_i16_try : I64 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : I64 -> I32
			to_i32_try : I64 -> Try(I32, [OutOfRange, ..others])
			to_i128 : I64 -> I128

			# Conversions to unsigned integers (all lossy for negative values)
			to_u8_wrap : I64 -> U8
			to_u8_try : I64 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : I64 -> U16
			to_u16_try : I64 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : I64 -> U32
			to_u32_try : I64 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : I64 -> U64
			to_u64_try : I64 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : I64 -> U128
			to_u128_try : I64 -> Try(U128, [OutOfRange, ..others])

			# Conversions to floating point (all safe)
			to_f32 : I64 -> F32
			to_f64 : I64 -> F64
			to_dec : I64 -> Dec
		}

		U128 :: [].{
			to_str : U128 -> Str
			is_zero : U128 -> Bool
			is_eq : U128, U128 -> Bool
			is_gt : U128, U128 -> Bool
			is_gte : U128, U128 -> Bool
			is_lt : U128, U128 -> Bool
			is_lte : U128, U128 -> Bool

			plus : U128, U128 -> U128
			minus : U128, U128 -> U128
			times : U128, U128 -> U128
			div_by : U128, U128 -> U128
			div_trunc_by : U128, U128 -> U128
			rem_by : U128, U128 -> U128
			mod_by : U128, U128 -> U128
			abs_diff : U128, U128 -> U128

			from_int_digits : List(U8) -> Try(U128, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(U128, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(U128, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : U128 -> I8
			to_i8_try : U128 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : U128 -> I16
			to_i16_try : U128 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : U128 -> I32
			to_i32_try : U128 -> Try(I32, [OutOfRange, ..others])
			to_i64_wrap : U128 -> I64
			to_i64_try : U128 -> Try(I64, [OutOfRange, ..others])
			to_i128_wrap : U128 -> I128
			to_i128_try : U128 -> Try(I128, [OutOfRange, ..others])

			# Conversions to unsigned integers
			to_u8_wrap : U128 -> U8
			to_u8_try : U128 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : U128 -> U16
			to_u16_try : U128 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : U128 -> U32
			to_u32_try : U128 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : U128 -> U64
			to_u64_try : U128 -> Try(U64, [OutOfRange, ..others])

			# Conversions to floating point (all safe)
			to_f32 : U128 -> F32
			to_f64 : U128 -> F64

			# Conversion to Dec (can overflow)
			to_dec_try : U128 -> Try(Dec, [OutOfRange, ..others])
		}

		I128 :: [].{
			to_str : I128 -> Str
			is_zero : I128 -> Bool
			is_negative : I128 -> Bool
			is_positive : I128 -> Bool
			is_eq : I128, I128 -> Bool
			is_gt : I128, I128 -> Bool
			is_gte : I128, I128 -> Bool
			is_lt : I128, I128 -> Bool
			is_lte : I128, I128 -> Bool

			negate : I128 -> I128
			abs : I128 -> I128
			plus : I128, I128 -> I128
			minus : I128, I128 -> I128
			times : I128, I128 -> I128
			div_by : I128, I128 -> I128
			div_trunc_by : I128, I128 -> I128
			rem_by : I128, I128 -> I128
			mod_by : I128, I128 -> I128
			abs_diff : I128, I128 -> U128

			from_int_digits : List(U8) -> Try(I128, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(I128, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(I128, [BadNumStr, ..others])

			# Conversions to signed integers
			to_i8_wrap : I128 -> I8
			to_i8_try : I128 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : I128 -> I16
			to_i16_try : I128 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : I128 -> I32
			to_i32_try : I128 -> Try(I32, [OutOfRange, ..others])
			to_i64_wrap : I128 -> I64
			to_i64_try : I128 -> Try(I64, [OutOfRange, ..others])

			# Conversions to unsigned integers (all lossy for negative values)
			to_u8_wrap : I128 -> U8
			to_u8_try : I128 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : I128 -> U16
			to_u16_try : I128 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : I128 -> U32
			to_u32_try : I128 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : I128 -> U64
			to_u64_try : I128 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : I128 -> U128
			to_u128_try : I128 -> Try(U128, [OutOfRange, ..others])

			# Conversions to floating point (all safe)
			to_f32 : I128 -> F32
			to_f64 : I128 -> F64

			# Conversion to Dec (can overflow)
			to_dec_try : I128 -> Try(Dec, [OutOfRange, ..others])
		}

		Dec :: [].{
			to_str : Dec -> Str
			is_zero : Dec -> Bool
			is_negative : Dec -> Bool
			is_positive : Dec -> Bool
			is_eq : Dec, Dec -> Bool
			is_gt : Dec, Dec -> Bool
			is_gte : Dec, Dec -> Bool
			is_lt : Dec, Dec -> Bool
			is_lte : Dec, Dec -> Bool

			negate : Dec -> Dec
			abs : Dec -> Dec
			plus : Dec, Dec -> Dec
			minus : Dec, Dec -> Dec
			times : Dec, Dec -> Dec
			div_by : Dec, Dec -> Dec
			div_trunc_by : Dec, Dec -> Dec
			rem_by : Dec, Dec -> Dec
			abs_diff : Dec, Dec -> Dec

			from_int_digits : List(U8) -> Try(Dec, [OutOfRange, ..others])
			from_dec_digits : (List(U8), List(U8)) -> Try(Dec, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(Dec, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(Dec, [BadNumStr, ..others])

			# Conversions to signed integers (all lossy - truncates fractional part)
			to_i8_wrap : Dec -> I8
			to_i8_try : Dec -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : Dec -> I16
			to_i16_try : Dec -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : Dec -> I32
			to_i32_try : Dec -> Try(I32, [OutOfRange, ..others])
			to_i64_wrap : Dec -> I64
			to_i64_try : Dec -> Try(I64, [OutOfRange, ..others])
			to_i128_wrap : Dec -> I128
			to_i128_try : Dec -> Try(I128, [OutOfRange, ..others])

			# Conversions to unsigned integers (all lossy - truncates fractional part)
			to_u8_wrap : Dec -> U8
			to_u8_try : Dec -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : Dec -> U16
			to_u16_try : Dec -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : Dec -> U32
			to_u32_try : Dec -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : Dec -> U64
			to_u64_try : Dec -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : Dec -> U128
			to_u128_try : Dec -> Try(U128, [OutOfRange, ..others])

			# Conversions to floating point (lossy - Dec has more precision)
			to_f32_wrap : Dec -> F32
			to_f32_try : Dec -> Try(F32, [OutOfRange, ..others])
			to_f64 : Dec -> F64
		}

		F32 :: [].{
			to_str : F32 -> Str
			is_zero : F32 -> Bool
			is_negative : F32 -> Bool
			is_positive : F32 -> Bool
			is_gt : F32, F32 -> Bool
			is_gte : F32, F32 -> Bool
			is_lt : F32, F32 -> Bool
			is_lte : F32, F32 -> Bool

			negate : F32 -> F32
			abs : F32 -> F32
			plus : F32, F32 -> F32
			minus : F32, F32 -> F32
			times : F32, F32 -> F32
			div_by : F32, F32 -> F32
			div_trunc_by : F32, F32 -> F32
			rem_by : F32, F32 -> F32
			abs_diff : F32, F32 -> F32

			from_int_digits : List(U8) -> Try(F32, [OutOfRange, ..others])
			from_dec_digits : (List(U8), List(U8)) -> Try(F32, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(F32, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(F32, [BadNumStr, ..others])

			# Conversions to signed integers (all lossy - truncation + range check)
			to_i8_wrap : F32 -> I8
			to_i8_try : F32 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : F32 -> I16
			to_i16_try : F32 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : F32 -> I32
			to_i32_try : F32 -> Try(I32, [OutOfRange, ..others])
			to_i64_wrap : F32 -> I64
			to_i64_try : F32 -> Try(I64, [OutOfRange, ..others])
			to_i128_wrap : F32 -> I128
			to_i128_try : F32 -> Try(I128, [OutOfRange, ..others])

			# Conversions to unsigned integers (all lossy - truncation + range check)
			to_u8_wrap : F32 -> U8
			to_u8_try : F32 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : F32 -> U16
			to_u16_try : F32 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : F32 -> U32
			to_u32_try : F32 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : F32 -> U64
			to_u64_try : F32 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : F32 -> U128
			to_u128_try : F32 -> Try(U128, [OutOfRange, ..others])

			# Conversion to F64 (safe widening)
			to_f64 : F32 -> F64
		}

		F64 :: [].{
			to_str : F64 -> Str
			is_zero : F64 -> Bool
			is_negative : F64 -> Bool
			is_positive : F64 -> Bool
			is_gt : F64, F64 -> Bool
			is_gte : F64, F64 -> Bool
			is_lt : F64, F64 -> Bool
			is_lte : F64, F64 -> Bool

			negate : F64 -> F64
			abs : F64 -> F64
			plus : F64, F64 -> F64
			minus : F64, F64 -> F64
			times : F64, F64 -> F64
			div_by : F64, F64 -> F64
			div_trunc_by : F64, F64 -> F64
			rem_by : F64, F64 -> F64
			abs_diff : F64, F64 -> F64

			from_int_digits : List(U8) -> Try(F64, [OutOfRange, ..others])
			from_dec_digits : (List(U8), List(U8)) -> Try(F64, [OutOfRange, ..others])
			from_numeral : Numeral -> Try(F64, [InvalidNumeral(Str), ..others])
			from_str : Str -> Try(F64, [BadNumStr, ..others])

			# Conversions to signed integers (all lossy - truncation + range check)
			to_i8_wrap : F64 -> I8
			to_i8_try : F64 -> Try(I8, [OutOfRange, ..others])
			to_i16_wrap : F64 -> I16
			to_i16_try : F64 -> Try(I16, [OutOfRange, ..others])
			to_i32_wrap : F64 -> I32
			to_i32_try : F64 -> Try(I32, [OutOfRange, ..others])
			to_i64_wrap : F64 -> I64
			to_i64_try : F64 -> Try(I64, [OutOfRange, ..others])
			to_i128_wrap : F64 -> I128
			to_i128_try : F64 -> Try(I128, [OutOfRange, ..others])

			# Conversions to unsigned integers (all lossy - truncation + range check)
			to_u8_wrap : F64 -> U8
			to_u8_try : F64 -> Try(U8, [OutOfRange, ..others])
			to_u16_wrap : F64 -> U16
			to_u16_try : F64 -> Try(U16, [OutOfRange, ..others])
			to_u32_wrap : F64 -> U32
			to_u32_try : F64 -> Try(U32, [OutOfRange, ..others])
			to_u64_wrap : F64 -> U64
			to_u64_try : F64 -> Try(U64, [OutOfRange, ..others])
			to_u128_wrap : F64 -> U128
			to_u128_try : F64 -> Try(U128, [OutOfRange, ..others])

			# Conversion to F32 (lossy narrowing)
			to_f32_wrap : F64 -> F32

			to_f32_try : F64 -> Try(F32, [OutOfRange, ..others])
			to_f32_try = |num| {
				answer = f64_to_f32_try_unsafe(num)
				if answer.success != 0 {
					Ok(answer.val_or_memory_garbage)
				} else {
					Err(OutOfRange)
				}
			}
		}
	}
}

range_to = |var $current, end| {
	var $answer = [] # Not bothering with List.with_capacity because this will become an iterator once those exist.

	while $current <= end {
		$answer = $answer.append($current)
		$current = $current + 1
	}

	$answer
}

range_until = |var $current, end| {
	var $answer = [] # Not bothering with List.with_capacity because this will become an iterator once those exist.

	while $current < end {
		$answer = $answer.append($current)
		$current = $current + 1
	}

	$answer
}

# Implemented by the compiler, does not perform bounds checks
list_get_unsafe : List(item), U64 -> item

# Unsafe conversion functions - these return simple records instead of Try types
# They are low-level operations that get replaced by the compiler
# Note: success is U8 (0 = false, 1 = true) since Bool is not available at top level
f64_to_f32_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : F32 }
