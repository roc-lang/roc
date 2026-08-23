# Inspecting values that came back from a generic function walks them through
# their runtime descriptors: record field names, list elements, and tag
# payloads must all render identically to inspecting the concrete value.
identity : a -> a
identity = |x| x

main! = |_args| {
	rec = identity({ label: "hi", nums: [1, 2] })
	echo!("${Str.inspect(rec)}\n")
	present : { label ?: Str, nums ?: List(U64) }
	present = { label: "bye", nums: [3, 4] }
	echo!("${Str.inspect(present)}\n")
	missing : { label ?: Str, nums ?: List(U64) }
	missing = {}
	echo!("${Str.inspect(missing)}\n")
	tag : Try(I64, Str)
	tag = identity(Ok(3))
	echo!("${Str.inspect(tag)}\n")
	Ok({})
}
