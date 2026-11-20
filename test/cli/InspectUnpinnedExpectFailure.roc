InspectUnpinnedExpectFailure := {}

# A failed `expect` reports the top-level values it mentions, including the
# ones whose types keep a part that nothing pins.
empty = []

pair = ([], Ok([]))

expect Str.inspect(empty) == "nope"

expect {
	shown = Str.inspect(pair)
	shown == "nope"
}
