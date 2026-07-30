# I128 and U128 to Dec conversion, both in range and out of range.

main! = |args| {
	# derived from argv so these don't fold; a folded conversion is precomputed by the
	# interpreter, whereas we want this tested on the other backends as well
	zero_signed = List.len(args).to_i128() - List.len(args).to_i128()
	zero_unsigned = List.len(args).to_u128() - List.len(args).to_u128()

	# the largest whole number Dec can hold
	if (170141183460469231731 + zero_signed).to_dec_try() != Ok(170141183460469231731.0) {
		crash "I128 within Dec's range should convert exactly"
	}

	if (170141183460469231732 + zero_signed).to_dec_try() != Err(OutOfRange) {
		crash "I128 beyond Dec's range should be rejected"
	}

	if (170141183460469231731 + zero_unsigned).to_dec_try() != Ok(170141183460469231731.0) {
		crash "U128 within Dec's range should convert exactly"
	}

	if (170141183460469231732 + zero_unsigned).to_dec_try() != Err(OutOfRange) {
		crash "U128 beyond Dec's range should be rejected"
	}

	Ok({})
}
