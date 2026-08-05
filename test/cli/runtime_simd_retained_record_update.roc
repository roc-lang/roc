app [main!] { pf: platform "../fx-open/platform/main.roc" }

# A retained byte list and its SIMD value cross a nominal callback boundary.
# The record update must preserve fields that are not explicitly changed after
# the callback's open result row is finalized as State.
State : {
	bytes : List(U8),
	offset : U64,
	checksum : U64,
	committed_source : U64,
	committed_gap : U64,
}

Handler := (State, U64 => [Handled(State), Declined]).{
	dispatch! : Handler, State, U64 => [Handled(State), Declined]
	dispatch! = |Handler.(handle!), state, event| handle!(state, event)
}

make_handler : U64 -> Handler
make_handler = |amount| {
	Handler.(
		|state, event| {
			match U8x16.load(state.bytes, state.offset) {
				Err(_) => Declined
				Ok(vector) => Handled({
					..state,
					offset: state.offset + 16,
					checksum: state.checksum + vector.sum_lanes().to_u64() + amount + event,
				})
			}
		},
	)
}

main! = |args| {
	heap = Str.concat("x0123456789abcdef", "ghijklmnopqrstuv")
	bytes = (heap.drop_first_bytes(1) ?? "").to_utf8()
	holder = [bytes, bytes]
	salt = args.len().to_u64()
	initial : State
	initial = {
		bytes,
		offset: 0,
		checksum: salt,
		committed_source: 0xCAFE,
		committed_gap: 0xBEEF,
	}

	match Handler.dispatch!(make_handler(7), initial, 11) {
		Declined => Err(UnexpectedDecline)
		Handled(done) => {
			if done.offset == 16
				and done.checksum == 1140 + salt
					and done.bytes.len() == 32
						and (done.bytes.get(16) ?? 0) == 0x67
							and done.committed_source == 0xCAFE
								and done.committed_gap == 0xBEEF
									and holder.len() == 2
				{
					Ok({})
				} else {
					Err(Mismatch(done, holder.len()))
				}
		}
	}
}
