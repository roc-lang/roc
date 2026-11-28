# The newly constructed record forces a carry in this imported tail group;
# the return edge must not forward a field view after releasing its owner.
Issue13356Loop :: [].{
	ping : a, U64, Str -> Str
	ping = |x, n, acc| if n == 0 acc else Issue13356Loop.pong({ payload: x, next: n - 1 }, Str.concat(acc, "i"))

	pong : { payload : a, next : U64 }, Str -> Str
	pong = |state, acc| if state.next == 0 acc else Issue13356Loop.ping(state.payload, state.next - 1, Str.concat(acc, "o"))
}
