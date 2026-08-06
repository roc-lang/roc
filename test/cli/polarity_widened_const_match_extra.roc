# A local annotation widening a stored const's error row, scrutinized by a
# match with a branch for the extra tag, used to panic monotype postcheck.
#   postcheck invariant violated: instantiation widened a closed tag union
r : Try(U8, [OutOfRange])
r = Ok(1)

classify! : List(Str) => U8
classify! = |args| {
	v : Try(U8, [OutOfRange, Other])
	v = if List.len(args) > 90 Err(Other) else r
	match v {
		Ok(n) => n
		Err(OutOfRange) => 100
		Err(Other) => 200
	}
}

main! = |args| {
	if classify!(args) == 1 Ok({}) else Err(Exit(3))
}
