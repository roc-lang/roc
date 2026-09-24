ByteRange :: { start : U64, end : U64 }.{
	from_bounds : U64, U64 -> Try(ByteRange, [EndBeforeStart])
	from_bounds = |start, end| if start <= end Ok({ start, end }) else Err(EndBeforeStart)

	start : ByteRange -> U64
	start = |range| range.start

	end : ByteRange -> U64
	end = |range| range.end
}
