# Widening a stored const's tag row nested inside a record field used to
# panic monotype postcheck the same way direct tag-union widening did.
#   postcheck invariant violated: instantiation widened a closed tag union
cfg : { mode : [Fast, Slow] }
cfg = { mode: Fast }

wide : { mode : [Fast, Slow, Turbo] }
wide = cfg

speed! : List(Str) => [Fast, Slow, Turbo]
speed! = |args| if List.len(args) > 90 Turbo else wide.mode

main! = |args| {
	match speed!(args) {
		Fast => Ok({})
		Slow => Err(Exit(1))
		Turbo => Err(Exit(2))
	}
}
