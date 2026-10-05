AppendLeBytesCapacity := {}

# Encoded capacity 10 used to admit an eight-byte store at offset one into
# a five-byte allocation. Checking capacity makes the bug deterministic.
expect {
	bytes = List.with_capacity(5).append(1.U8)
	result = (0x0807060504030201.U64).append_le_bytes_to(bytes, 8).ok_or([])
	result == [1, 1, 2, 3, 4, 5, 6, 7, 8] and result.capacity() >= result.len()
}

# Four bytes fit exactly, but the fast path needs room for its full word.
# Pin both sides of that boundary and preserve the encoded capacity on return.
expect {
	append_header = |capacity| {
		bytes = List.with_capacity(capacity).append(1.U8)
		result = (0x0807060504030201.U64).append_le_bytes_to(bytes, 4).ok_or([])
		result == [1, 1, 2, 3, 4] and result.capacity() == capacity
	}
	append_header(5) and append_header(8) and append_header(9) and append_header(13)
}
