
Case :: [].{
	MappingProfile : [UnicodeDefault, Turkic]
	Limits : CaseLimits
	Error : CaseError
	Result : { text : Str }

	unicode_default : MappingProfile
	unicode_default = UnicodeDefault

	unlimited_limits : Limits
	unlimited_limits = { max_output_bytes: U64.highest }

	to_lower : Str, MappingProfile, Limits -> Try(Result, Error)
	to_lower = |source, profile, budget| {
		match run_case(source, profile, budget, Lower) {
			Err(error) => Err(error)
			Ok(_) => Ok({ text: "" })
		}
	}
}

CaseLimits : {
	max_output_bytes : U64,
}

CaseError : [
	CoordinateOverflow({ at : { byte_offset : U64, scalar_offset : U64 } }),
	InternalEncodingFault,
]

Mapping : [Identity(U32), One(U32), Sequence(List(U32))]

Accumulator : {
	bytes : List(U8),
	input_scalars : U64,
}

Fold : [Running(Accumulator), Failed(CaseError)]

run_case = |source, profile, limits, operation| {
	folded = fold_scalars(
		source,
		Running(empty_accumulator),
		|state, scalar, byte_start, byte_end, scalar_index| {
			match state {
				Failed(_) => state
				Running(accumulator) => apply_scalar(source, profile, limits, operation, Bool.False, accumulator, scalar, byte_start, byte_end, scalar_index)
			}
		},
	)
	finish(folded)
}

apply_scalar = |source, profile, limits, operation, _title_contextual, accumulator, scalar, byte_start, byte_end, scalar_index| {
	props = { simple_lower: 0 }
	if scalar_index != accumulator.input_scalars {
		Failed(CoordinateOverflow({ at: { byte_offset: byte_start, scalar_offset: accumulator.input_scalars } }))
	} else {
		mapping = select_case_mapping(source, profile, operation, scalar, byte_end, props)
		apply_scalar_with_mapping(limits, accumulator, scalar, byte_start, byte_end, mapping.value, Bool.False)
	}
}

select_case_mapping = |source, profile, operation, scalar, byte_end, props| {
	var $selected = { value: simple_mapping(operation, scalar, props), contextual: Bool.False }
	var $index = 0.U64
	while $index < scalar.to_u64() {
		if profile == Turkic and following_cased(source, byte_end) {
			$selected = { value: One(0x69), contextual: Bool.False }
		}
		$index = $index + 1
	}
	$selected
}

simple_mapping = |operation, scalar, props| {
	mapped = match operation {
		Lower => props.simple_lower
	}
	if mapped == 0 {
		Identity(scalar)
	} else {
		One(mapped)
	}
}

following_cased = |source, byte_start| {
	bytes = source.to_utf8()
	bytes.len() > byte_start
}

apply_scalar_with_mapping : CaseLimits, Accumulator, U32, U64, U64, Mapping, Bool -> Fold
apply_scalar_with_mapping = |limits, accumulator, scalar, _byte_start, _byte_end, _mapping, _contextual| {
	if accumulator.input_scalars >= limits.max_output_bytes {
		return Failed(InternalEncodingFault)
	}
	bytes = accumulator.bytes.append(scalar.to_u8_wrap())
	Running({
		..accumulator,
		bytes,
		input_scalars: accumulator.input_scalars + 1,
	})
}

empty_accumulator = {
	bytes: [],
	input_scalars: 0,
}

finish = |fold| match fold {
	Failed(error) => Err(error)
	Running(acc) => Ok(acc.bytes)
}

fold_scalars = |source, initial, step| {
	var $result = initial
	var $accumulator = 0.U32
	var $expected_width = 0.U8
	var $byte_offset = 0.U64
	var $scalar_index = 0.U64

	for byte in source.iter_utf8() {
		byte_end = $byte_offset + 1
		if $expected_width == 0 {
			if byte < 0x80 {
				$result = step($result, byte.to_u32(), $byte_offset, byte_end, $scalar_index)
				$scalar_index = $scalar_index + 1
			} else {
				$expected_width = 2
			}
		} else {
			$accumulator = $accumulator + byte.to_u32()
			$result = step($result, $accumulator, 0, byte_end, $scalar_index)
			$scalar_index = $scalar_index + 1
			$expected_width = 0
		}

		$byte_offset = byte_end
	}

	$result
}
