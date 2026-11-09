# Compile-time evaluation freezes both lists into static data: one holds a
# million pointers to a single heap string, the other two hundred thousand
# distinct heap strings. Freezing, merging identical nodes, and re-encoding
# the image for the runtime program must stay proportional to the image,
# so the build finishes well inside the perf guard.

copies : List(Str)
copies = List.repeat("a string too long to be stored inline", 1000000)

labels : List(Str)
labels = (0.U64..<200000).iter().map(|i| "a constant label number ${i.to_str()}").collect()

main! = |args| {
	index = args.len()
	echo!(copies.get(index) ?? "none")
	echo!(labels.get(index) ?? "none")
	Ok({})
}
