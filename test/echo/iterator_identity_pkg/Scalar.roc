import ByteRange

Scalar :: { code : U32 }.{
	LocatedScalar : {
		scalar : Scalar,
		byte_range : ByteRange,
		scalar_index : U64,
	}

	to_u32 : Scalar -> U32
	to_u32 = |{ code }| code

	iter : Str -> Iter(LocatedScalar)
	iter = |source| {
		bytes = source.to_utf8()
		next_located = |cursor| {
			match List.get(cursor.bytes, cursor.offset) {
				Err(_) => Err(NoMore)
				Ok(byte) => {
					match ByteRange.from_bounds(cursor.offset, cursor.offset + 1) {
						Err(_) => Err(NoMore)
						Ok(byte_range) => Ok((
							{ scalar: { code: byte.to_u32() }, byte_range, scalar_index: cursor.offset },
							{ bytes: cursor.bytes, offset: cursor.offset + 1 },
						))
					}
				}
			}
		}
		Iter.custom({ bytes, offset: 0 }, Unknown, next_located)
	}
}
