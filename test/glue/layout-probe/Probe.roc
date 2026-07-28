Probe := [].{
	VectorRecord := {
		before : U64,
		bytes : U8x16,
		words : I32x4,
		after : U32,
	}

	VectorQuad := {
		a : U8x16,
		b : I16x8,
		c : U32x4,
		d : I64x2,
	}

	VectorHva := {
		a : U8x16,
		b : U8x16,
		c : U8x16,
		d : U8x16,
	}

	VectorWrapper := { only : U8x16 }
	TransparentVector := [VectorOnly(U8x16)]
	NestedVectorHva := { wrapped : TransparentVector, raw : U8x16 }
	TransparentFloat := [FloatOnly(F64)]
	NestedFloatHfa := { wrapped : TransparentFloat, raw : F64 }
	IntegerPair := { first : I64, second : U64 }

	VectorTag := [Bytes(U8x16), Empty, Pair(U64, I16x8)]

	LayoutProbe := [
		Wide(
			{
				label : Str,
				a : Box(U64),
				b : Box(U64),
				c : Box(U64),
				d : Box(U64),
				e : Box(U64),
				f : Box(U64),
				g : Box(U64),
				h : Box(U64),
			},
		),
		# Mixed alignment classes (align-8, pointer, align-4, align-1): the
		# committed field order must be identical at 32- and 64-bit pointer
		# widths, with the pointer field sorting between the 8- and 4-byte
		# alignment bands.
		Aligned(
			{
				marker : U64,
				token : Box(U64),
				flag : U32,
				tiny : U8,
			},
		),
		Empty,
	]

	roundtrip! : LayoutProbe => LayoutProbe

	roundtrip_u8x16! : U8x16 => U8x16
	roundtrip_i8x16! : I8x16 => I8x16
	roundtrip_u16x8! : U16x8 => U16x8
	roundtrip_i16x8! : I16x8 => I16x8
	roundtrip_u32x4! : U32x4 => U32x4
	roundtrip_i32x4! : I32x4 => I32x4
	roundtrip_u64x2! : U64x2 => U64x2
	roundtrip_i64x2! : I64x2 => I64x2
	roundtrip_vector_record! : VectorRecord => VectorRecord
	roundtrip_vector_quad! : VectorQuad => VectorQuad
	roundtrip_vector_hva! : VectorHva => VectorHva
	roundtrip_vector_wrapper! : VectorWrapper => VectorWrapper
	roundtrip_vector_tuple! : (U64, U8x16, I16x8) => (U64, U8x16, I16x8)
	roundtrip_vector_tag! : VectorTag => VectorTag
	exhaust_registers! : I64, I64, I64, I64, I64, I64, F64, F64, F64, F64, F64, F64, F64, F64, U8x16 => U8x16
	spill_vector_hva! : U8x16, U8x16, U8x16, U8x16, U8x16, U8x16, U8x16, NestedVectorHva => NestedVectorHva
	spill_float_hfa! : F64, F64, F64, F64, F64, F64, F64, NestedFloatHfa => NestedFloatHfa
	spill_integer_pair! : I64, I64, I64, I64, I64, I64, I64, IntegerPair => IntegerPair
	align_i128! : I64, I128 => I128
	spill_i128! : I64, I64, I64, I64, I64, I128 => I128
	spill_dec! : I64, I64, I64, I64, I64, Dec => Dec
	compact_stack! : I64, I64, I64, I64, I64, I64, I64, I64, U8, U16, U32 => U64
}
