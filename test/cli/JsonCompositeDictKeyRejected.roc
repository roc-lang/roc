JsonCompositeDictKeyRejected :: [].{}

## JSON object keys are strings, so JSON implements the key-string methods and
## not `encode_key_start`. A dict keyed by a record is rejected there, by the
## format, rather than by a rule every format has to share.
main : Str
main = {
	d : Dict({ x : U64, y : U64 }, Str)
	d = Dict.from_list([({ x: 1, y: 2 }, "a")])

	Json.to_str(d)
}
